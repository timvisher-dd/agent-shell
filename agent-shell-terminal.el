;;; agent-shell-terminal.el --- Terminal support for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Terminal capability handling for agent-shell.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'acp)
(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'subr-x)
(require 'agent-shell-meta)

(declare-function agent-shell--build-command-for-execution "agent-shell")
(declare-function agent-shell--resolve-path "agent-shell")
(declare-function agent-shell-cwd "agent-shell")

(declare-function agent-shell--append-tool-call-output "agent-shell")

(defvar agent-shell--state)

(defun agent-shell--terminal-next-id (state)
  "Return a new terminal ID and increment STATE counter."
  (let* ((count (1+ (or (map-elt state :terminal-count) 0)))
         (terminal-id (format "term_%d" count)))
    (map-put! state :terminal-count count)
    terminal-id))

(defvar agent-shell--terminal-release-grace-seconds 120
  "Seconds to retain released terminals after their last request.")

(defun agent-shell--terminal-get (state terminal-id)
  "Return terminal entry for TERMINAL-ID in STATE."
  (map-nested-elt state `(:terminals ,terminal-id)))

(defun agent-shell--terminal-put (state terminal-id terminal)
  "Store TERMINAL entry in STATE under TERMINAL-ID."
  (let ((terminals (copy-alist (map-elt state :terminals))))
    (setf (map-elt terminals terminal-id) terminal)
    (map-put! state :terminals terminals)))

(defun agent-shell--terminal-remove (state terminal-id)
  "Remove TERMINAL-ID from STATE."
  (when-let ((terminal (agent-shell--terminal-get state terminal-id)))
    (when-let ((buffer (agent-shell--terminal-output-buffer terminal)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  (map-put! state :terminals
            (map-delete (map-elt state :terminals) terminal-id)))

(defun agent-shell--terminal-ensure-list (value)
  "Coerce VALUE to a list, converting vectors when needed."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t nil)))

(defun agent-shell--terminal-normalize-env (env)
  "Normalize ENV to a list of \"NAME=VALUE\" strings."
  (let ((entries (agent-shell--terminal-ensure-list env)))
    (mapcan (lambda (entry)
              (let ((name (agent-shell--meta-lookup entry 'name))
                    (value (agent-shell--meta-lookup entry 'value)))
                (when (stringp name)
                  (list (format "%s=%s" name (or value ""))))))
            entries)))

(defun agent-shell--terminal-command-list (command args)
  "Return command list for terminal/create from COMMAND and ARGS."
  (if (or args (file-executable-p command))
      (cons command args)
    (list shell-file-name (or shell-command-switch "-c") command)))

(defun agent-shell--terminal-make-output-buffer (terminal-id)
  "Create an output buffer for TERMINAL-ID."
  (let ((buffer (generate-new-buffer (format " *agent-shell-terminal-output %s*" terminal-id))))
    (with-current-buffer buffer
      (setq buffer-undo-list t))
    buffer))

(defun agent-shell--terminal-output-buffer (terminal)
  "Return live output buffer for TERMINAL."
  (let ((buffer (map-elt terminal :output-buffer)))
    (or (and (buffer-live-p buffer) buffer)
        (when-let* ((proc (map-elt terminal :process))
                    (proc-buffer (process-buffer proc)))
          (and (buffer-live-p proc-buffer)
               proc-buffer)))))


(defun agent-shell--tool-call-terminal-ids (content)
  "Return terminal IDs from tool call CONTENT."
  (let ((items (agent-shell--terminal-ensure-list content)))
    (mapcan (lambda (item)
              (let ((type (agent-shell--meta-lookup item 'type)))
                (when (and (stringp type) (string= type "terminal"))
                  (let ((terminal-id (agent-shell--meta-lookup item 'terminalId)))
                    (when (stringp terminal-id)
                      (list terminal-id))))))
            items)))

(defun agent-shell--terminal-stream-output (state tool-call-id output)
  "Stream terminal OUTPUT into tool call TOOL-CALL-ID in STATE."
  (when (and (stringp output) (not (string-empty-p output)))
    (agent-shell--append-tool-call-output state tool-call-id output)))

(defun agent-shell--terminal-link-tool-call (state terminal-id tool-call-id)
  "Associate TERMINAL-ID with TOOL-CALL-ID in STATE and stream existing output."
  (when (and (stringp terminal-id) (stringp tool-call-id))
    (when-let ((terminal (agent-shell--terminal-get state terminal-id)))
      (let ((tool-call-ids (map-elt terminal :tool-call-ids)))
        (unless (member tool-call-id tool-call-ids)
          (setf (map-elt terminal :tool-call-ids)
                (cons tool-call-id tool-call-ids))
          (agent-shell--terminal-put state terminal-id terminal)
          (when-let ((buffer (agent-shell--terminal-output-buffer terminal)))
            (with-current-buffer buffer
              (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                (agent-shell--terminal-stream-output state tool-call-id output)))))))))

(defun agent-shell--terminal-link-tool-call-content (state tool-call-id content)
  "Link TOOL-CALL-ID to any terminals referenced in CONTENT for STATE."
  (dolist (terminal-id (agent-shell--tool-call-terminal-ids content))
    (agent-shell--terminal-link-tool-call state terminal-id tool-call-id)))

(defun agent-shell--terminal-handle-output (state terminal-id output)
  "Handle OUTPUT from TERMINAL-ID in STATE by recording and streaming."
  (when (and (stringp output) (not (string-empty-p output)))
    (when-let ((terminal (agent-shell--terminal-get state terminal-id)))
      (let ((buffer (agent-shell--terminal-output-buffer terminal)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert output)))))
      (agent-shell--terminal-touch state terminal-id terminal)
      (let ((tool-call-ids (map-elt terminal :tool-call-ids)))
        (dolist (tool-call-id tool-call-ids)
          (agent-shell--terminal-stream-output state tool-call-id output))))))

(defun agent-shell--terminal-process-filter (state terminal-id)
  "Return a process filter streaming output for TERMINAL-ID in STATE."
  (lambda (_proc output)
    (agent-shell--terminal-handle-output state terminal-id output)))

;; Output truncation is computed on demand for terminal/output.
(defun agent-shell--terminal-output-slice (terminal)
  "Return (OUTPUT . TRUNCATED) for TERMINAL respecting outputByteLimit.

The ACP spec defines outputByteLimit in bytes, but output must remain
valid text.  We compute byte positions with `position-bytes` and select
the trailing slice using `byte-to-position` so we never split multibyte
characters.  When the limit is <= 0, we return empty output and mark it
truncated if any bytes were present."
  (let ((buffer (agent-shell--terminal-output-buffer terminal))
        (limit (map-elt terminal :output-byte-limit)))
    (if (not (buffer-live-p buffer))
        (cons "" nil)
      (with-current-buffer buffer
        (let* ((min (point-min))
               (max (point-max))
               (output nil)
               (truncated nil))
          (if (not (numberp limit))
              (setq output (buffer-substring-no-properties min max))
            (let* ((min-bytes (position-bytes min))
                   (max-bytes (position-bytes max))
                   (bytes (- max-bytes min-bytes)))
              (cond
               ((<= limit 0)
                (setq output ""
                      truncated (< 0 bytes)))
               ((<= bytes limit)
                (setq output (buffer-substring-no-properties min max)))
               (t
                (let* ((excess (- bytes limit))
                       (cutoff-byte (+ min-bytes excess))
                       (cutoff-pos (or (byte-to-position cutoff-byte) min)))
                  (setq output (buffer-substring-no-properties cutoff-pos max)
                        truncated t))))))
          (cons output truncated))))))

(defun agent-shell--terminal-signal-name (code)
  "Return a signal name for CODE or a numeric string fallback."
  (or (ignore-errors
        (with-temp-buffer
          (call-process "kill" nil t nil "-l" (number-to-string code))
          (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
      (number-to-string code)))

(defun agent-shell--terminal-exit-status (terminal)
  "Return exit status alist for TERMINAL entry."
  (when-let ((proc (map-elt terminal :process)))
    ;; For subprocesses created via `make-process :command`, the meaningful
    ;; terminal states are `exit` and `signal`. Network-only statuses
    ;; (e.g., `open`, `listen`, `connect`) do not apply here.
    (let* ((status (process-status proc))
           (code (process-exit-status proc)))
      (cond
       ((eq status 'exit)
        (list (cons 'exitCode code)))
       ((eq status 'signal)
        (list (cons 'signal (agent-shell--terminal-signal-name code))))
       (t nil)))))

(defun agent-shell--terminal-respond-waiters (state terminal-id)
  "Respond to any pending waiters for TERMINAL-ID in STATE."
  (when-let ((terminal (agent-shell--terminal-get state terminal-id)))
    (let ((waiters (map-elt terminal :waiters)))
      (when waiters
        (let ((result (agent-shell--terminal-exit-status terminal)))
          (dolist (request-id waiters)
            (acp-send-response
             :client (map-elt state :client)
             :response `((:request-id . ,request-id)
                         (:result . ,result)))))
        (setf (map-elt terminal :waiters) nil)
        (agent-shell--terminal-put state terminal-id terminal)))))


(defun agent-shell--terminal-touch (state terminal-id &optional terminal)
  "Record terminal access for TERMINAL-ID in STATE.
Refresh cleanup timer when released.  Use TERMINAL when already looked up."
  (when-let ((entry (or terminal (agent-shell--terminal-get state terminal-id))))
    (setf (map-elt entry :last-access) (float-time))
    (agent-shell--terminal-put state terminal-id entry)
    (when (map-elt entry :released)
      (agent-shell--terminal-schedule-cleanup state terminal-id entry))))

(defun agent-shell--terminal-finalize (state terminal-id)
  "Finalize TERMINAL-ID in STATE after exit when released."
  (when-let ((terminal (agent-shell--terminal-get state terminal-id)))
    (agent-shell--terminal-respond-waiters state terminal-id)
    (when (map-elt terminal :released)
      (agent-shell--terminal-schedule-cleanup state terminal-id))))

(defun agent-shell--terminal-schedule-cleanup (state terminal-id &optional terminal)
  "Schedule cleanup for TERMINAL-ID in STATE after inactivity.
Use TERMINAL when already looked up."
  (when-let* ((buffer (map-elt state :buffer))
              (entry (or terminal (agent-shell--terminal-get state terminal-id))))
    (when (map-elt entry :released)
      (when-let ((timer (map-elt entry :cleanup-timer)))
        (ignore-errors (cancel-timer timer)))
      (let* ((timeout (if (numberp agent-shell--terminal-release-grace-seconds)
                          agent-shell--terminal-release-grace-seconds
                        120))
             (timer (run-at-time
                     timeout nil
                     (lambda ()
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (when-let ((current (agent-shell--terminal-get agent-shell--state terminal-id)))
                             (let* ((last (or (map-elt current :last-access) 0))
                                    (elapsed (- (float-time) last)))
                               (when (and (map-elt current :released)
                                          (<= timeout elapsed))
                                 (agent-shell--terminal-remove agent-shell--state terminal-id))))))))))
        (setf (map-elt entry :cleanup-timer) timer)
        (agent-shell--terminal-put state terminal-id entry)))))

(cl-defun agent-shell--on-terminal-create-request (&key state request)
  "Handle terminal/create REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((params .params)
               (command (agent-shell--meta-lookup params 'command))
               (args (agent-shell--terminal-ensure-list
                      (agent-shell--meta-lookup params 'args)))
               (env (agent-shell--terminal-normalize-env
                     (agent-shell--meta-lookup params 'env)))
               (cwd (agent-shell--meta-lookup params 'cwd))
               (output-byte-limit (let ((limit (agent-shell--meta-lookup params 'outputByteLimit)))
                                    (when (numberp limit) limit))))
          (unless (stringp command)
            (error "Terminal/create missing command"))
          (let* ((terminal-id (agent-shell--terminal-next-id state))
                 (output-buffer (agent-shell--terminal-make-output-buffer terminal-id))
                 (default-directory (file-name-as-directory
                                     (expand-file-name
                                      (or (and cwd (agent-shell--resolve-path cwd))
                                          (agent-shell-cwd)))))
                 (command-list (agent-shell--build-command-for-execution
                                (agent-shell--terminal-command-list command args)))
                 (process-environment (if env
                                          (append env process-environment)
                                        process-environment))
                 (terminal `((:id . ,terminal-id)
                             (:process . nil)
                             (:output-buffer . ,output-buffer)
                             (:output-byte-limit . ,output-byte-limit)
                             (:tool-call-ids . nil)
                             (:waiters . nil)
                             (:released . nil)
                             (:cleanup-timer . nil)
                             (:last-access . ,(float-time))))
                 (proc (progn
                         (unless (file-directory-p default-directory)
                           (error "Terminal/create cwd not found: %s" default-directory))
                         (make-process
                          :name (format "agent-shell-terminal-%s" terminal-id)
                          :buffer output-buffer
                          :command command-list
                          :noquery t
                          :filter (agent-shell--terminal-process-filter state terminal-id)
                          :sentinel (lambda (proc _event)
                                      (let ((status (process-status proc)))
                                        (when (memq status '(exit signal))
                                          (when (agent-shell--terminal-get state terminal-id)
                                            (agent-shell--terminal-finalize state terminal-id)))))))))
            (setf (map-elt terminal :process) proc)
            (agent-shell--terminal-put state terminal-id terminal)
            (let ((status (process-status proc)))
              (when (memq status '(exit signal))
                (when (agent-shell--terminal-get state terminal-id)
                  (agent-shell--terminal-finalize state terminal-id))))
            (acp-send-response
             :client (map-elt state :client)
             :response `((:request-id . ,.id)
                         (:result . ((terminalId . ,terminal-id)))))))
      (quit
       (acp-send-response
        :client (map-elt state :client)
        :response `((:request-id . ,.id)
                    (:error . ,(acp-make-error
                                :code -32603
                                :message "Operation cancelled by user")))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response `((:request-id . ,.id)
                    (:error . ,(acp-make-error
                                :code -32603
                                :message (error-message-string err)))))))))

(cl-defun agent-shell--on-terminal-output-request (&key state request)
  "Handle terminal/output REQUEST with STATE."
  (let-alist request
    (let* ((terminal-id (agent-shell--meta-lookup .params 'terminalId))
           (terminal (and terminal-id (agent-shell--terminal-get state terminal-id))))
      (if terminal
          (let* ((slice (agent-shell--terminal-output-slice terminal))
                 (output (car slice))
                 (truncated (if (cdr slice) t :false))
                 (exit-status (agent-shell--terminal-exit-status terminal)))
            (agent-shell--terminal-touch state terminal-id terminal)
            (acp-send-response
             :client (map-elt state :client)
             :response `((:request-id . ,.id)
                         (:result . ((output . ,output)
                                     (truncated . ,truncated)
                                     ,@(when exit-status
                                         `((exitStatus . ,exit-status))))))))
        (acp-send-response
         :client (map-elt state :client)
         :response `((:request-id . ,.id)
                     (:error . ,(acp-make-error
                                 :code -32002
                                 :message "Terminal not found"))))))))

(cl-defun agent-shell--on-terminal-wait-for-exit-request (&key state request)
  "Handle terminal/wait_for_exit REQUEST with STATE."
  (let-alist request
    (let* ((terminal-id (agent-shell--meta-lookup .params 'terminalId))
           (terminal (and terminal-id (agent-shell--terminal-get state terminal-id)))
           (exit-status (and terminal (agent-shell--terminal-exit-status terminal))))
      (cond
       ((not terminal)
        (acp-send-response
         :client (map-elt state :client)
         :response `((:request-id . ,.id)
                     (:error . ,(acp-make-error
                                 :code -32002
                                 :message "Terminal not found")))))
       (exit-status
        (agent-shell--terminal-touch state terminal-id terminal)
        (acp-send-response
         :client (map-elt state :client)
         :response `((:request-id . ,.id)
                     (:result . ,exit-status))))
       (t
        (let ((waiters (map-elt terminal :waiters)))
          (setf (map-elt terminal :waiters) (cons .id waiters))
          (agent-shell--terminal-put state terminal-id terminal)
          (agent-shell--terminal-touch state terminal-id terminal)))))))

(cl-defun agent-shell--on-terminal-kill-request (&key state request)
  "Handle terminal/kill REQUEST with STATE."
  (let-alist request
    (let* ((terminal-id (agent-shell--meta-lookup .params 'terminalId))
           (terminal (and terminal-id (agent-shell--terminal-get state terminal-id))))
      (if terminal
          (progn
            (when-let ((proc (map-elt terminal :process)))
              (ignore-errors (kill-process proc)))
            (agent-shell--terminal-touch state terminal-id terminal)
            (acp-send-response
             :client (map-elt state :client)
             :response `((:request-id . ,.id)
                         (:result . nil))))
        (acp-send-response
         :client (map-elt state :client)
         :response `((:request-id . ,.id)
                     (:error . ,(acp-make-error
                                 :code -32002
                                 :message "Terminal not found"))))))))

(cl-defun agent-shell--on-terminal-release-request (&key state request)
  "Handle terminal/release REQUEST with STATE."
  (let-alist request
    (let* ((terminal-id (agent-shell--meta-lookup .params 'terminalId))
           (terminal (and terminal-id (agent-shell--terminal-get state terminal-id))))
      (if terminal
          (progn
            (setf (map-elt terminal :released) t)
            (agent-shell--terminal-put state terminal-id terminal)
            (when-let ((proc (map-elt terminal :process)))
              (ignore-errors (kill-process proc)))
            (when (agent-shell--terminal-exit-status terminal)
              (agent-shell--terminal-finalize state terminal-id))
            (acp-send-response
             :client (map-elt state :client)
             :response `((:request-id . ,.id)
                         (:result . nil))))
        (acp-send-response
         :client (map-elt state :client)
         :response `((:request-id . ,.id)
                     (:error . ,(acp-make-error
                                 :code -32002
                                 :message "Terminal not found"))))))))


(provide 'agent-shell-terminal)

;;; agent-shell-terminal.el ends here
