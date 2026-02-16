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
  "Return terminal entry for TERMINAL-ID."
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

(defun agent-shell--terminal-normalize-args (args)
  "Normalize ARGS to a list of strings."
  (cond
   ((vectorp args) (append args nil))
   ((listp args) args)
   (t nil)))

(defun agent-shell--terminal-normalize-env (env)
  "Normalize ENV to a list of \"NAME=VALUE\" strings."
  (let ((entries (cond
                  ((vectorp env) (append env nil))
                  ((listp env) env)
                  (t nil))))
    (delq nil
          (mapcar (lambda (entry)
                    (let ((name (agent-shell--meta-lookup entry 'name))
                          (value (agent-shell--meta-lookup entry 'value)))
                      (when (stringp name)
                        (format "%s=%s" name (or value "")))))
                  entries))))

(defun agent-shell--terminal-command-list (command args)
  "Return command list for terminal/create from COMMAND and ARGS."
  (let ((args (or args nil)))
    (cond
     ((and (stringp command)
           (or args (file-executable-p command)))
      (append (list command) args))
     ((stringp command)
      (list shell-file-name (or shell-command-switch "-c") command))
     (t
      (append (list command) args)))))

(defun agent-shell--terminal-make-output-buffer (terminal-id)
  "Create an output buffer for TERMINAL-ID."
  (let ((buffer (generate-new-buffer (format " *agent-shell-terminal-output %s*" terminal-id))))
    (with-current-buffer buffer
      (setq buffer-undo-list t))
    buffer))

(defun agent-shell--terminal-output-buffer (terminal)
  "Return live output buffer for TERMINAL."
  (let ((buffer (map-elt terminal :output-buffer)))
    (cond
     ((buffer-live-p buffer) buffer)
     ((when-let ((proc (map-elt terminal :process)))
        (let ((proc-buffer (process-buffer proc)))
          (when (buffer-live-p proc-buffer)
            proc-buffer)))))))

;; Output truncation is computed on demand for terminal/output.
(defun agent-shell--terminal-output-slice (terminal)
  "Return (OUTPUT . TRUNCATED) for TERMINAL respecting outputByteLimit.

The ACP spec defines outputByteLimit in bytes, but output must remain
valid text. We compute byte positions with `position-bytes` and select
the trailing slice using `byte-to-position` so we never split multibyte
characters. When the limit is <= 0, we return empty output and mark it
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
  "Respond to any pending waiters for TERMINAL-ID."
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
  "Record terminal access and refresh cleanup timer when released."
  (when-let ((entry (or terminal (agent-shell--terminal-get state terminal-id))))
    (setf (map-elt entry :last-access) (float-time))
    (agent-shell--terminal-put state terminal-id entry)
    (when (map-elt entry :released)
      (agent-shell--terminal-schedule-cleanup state terminal-id entry))))

(defun agent-shell--terminal-finalize (state terminal-id)
  "Finalize TERMINAL-ID after exit when released."
  (when-let ((terminal (agent-shell--terminal-get state terminal-id)))
    (agent-shell--terminal-respond-waiters state terminal-id)
    (when (map-elt terminal :released)
      (agent-shell--terminal-schedule-cleanup state terminal-id))))

(defun agent-shell--terminal-schedule-cleanup (state terminal-id &optional terminal)
  "Schedule cleanup for TERMINAL-ID after inactivity."
  (when-let* ((buffer (map-elt state :buffer))
              (entry (or terminal (agent-shell--terminal-get state terminal-id))))
    (when (map-elt entry :released)
      (when-let ((timer (map-elt entry :cleanup-timer)))
        (ignore-errors (cancel-timer timer)))
      (let ((timeout (if (numberp agent-shell--terminal-release-grace-seconds)
                         agent-shell--terminal-release-grace-seconds
                       120)))
        (let ((timer (run-at-time
                      timeout nil
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (when-let ((current (agent-shell--terminal-get agent-shell--state terminal-id)))
                              (let* ((last (or (map-elt current :last-access) 0))
                                     (elapsed (- (float-time) last)))
                                (when (and (map-elt current :released)
                                           (>= elapsed timeout))
                                  (agent-shell--terminal-remove agent-shell--state terminal-id))))))))))
          (setf (map-elt entry :cleanup-timer) timer)
          (agent-shell--terminal-put state terminal-id entry))))))

(cl-defun agent-shell--on-notification (&key state notification)
  "Handle incoming notification using SHELL, STATE, and NOTIFICATION."
  (let-alist notification
    (cond ((equal .method "session/update")
           (let ((update (map-elt (map-elt notification 'params) 'update)))
             (condition-case err
                 (cond
              ((equal (map-elt update 'sessionUpdate) "tool_call")
               (agent-shell--save-tool-call
                state
                (map-elt update 'toolCallId)
                (append (list (cons :title (cond
                                            ((and (string= (map-elt update 'title) "Skill")
                                                  (map-nested-elt update '(rawInput command)))
                                             (format "Skill: %s"
                                                     (map-nested-elt update '(rawInput command))))
                                            (t
                                             (map-elt update 'title))))
                              (cons :status (map-elt update 'status))
                              (cons :kind (map-elt update 'kind))
                              (cons :command (map-nested-elt update '(rawInput command)))
                              (cons :description (map-nested-elt update '(rawInput description)))
                              (cons :content (map-elt update 'content)))
                        (when-let ((diff (agent-shell--make-diff-info :tool-call update)))
                          (list (cons :diff diff)))))
               (agent-shell--emit-event
                :event 'tool-call-update
                :data (list (cons :tool-call-id (map-elt update 'toolCallId))
                            (cons :tool-call (map-nested-elt state (list :tool-calls (map-elt update 'toolCallId))))))
               (let ((tool-call-labels (agent-shell-make-tool-call-label
                                        state (map-elt update 'toolCallId))))
                 (agent-shell--update-fragment
                  :state state
                  :block-id (map-elt update 'toolCallId)
                  :label-left (map-elt tool-call-labels :status)
                  :label-right (map-elt tool-call-labels :title)
                  :expanded agent-shell-tool-use-expand-by-default)
                 ;; Display plan as markdown block if present
                 (when-let ((plan (map-nested-elt update '(rawInput plan))))
                   (agent-shell--update-fragment
                    :state state
                    :block-id (concat (map-elt update 'toolCallId) "-plan")
                    :label-left (propertize "Proposed plan" 'font-lock-face 'font-lock-doc-markup-face)
                    :body plan
                    :expanded t)))
               (map-put! state :last-entry-type "tool_call"))
              ((equal (map-elt update 'sessionUpdate) "agent_thought_chunk")
               (let-alist update
                 (let* ((request-id (map-elt state :request-count))
                        (thought-request-id (map-elt state :thought-request-id))
                        (reuse (and request-id (equal request-id thought-request-id)))
                        (block-id (map-elt state :thought-block-id))
                        (append nil))
                   (unless reuse
                     (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                     (setq block-id (format "%s-agent_thought_chunk"
                                            (map-elt state :chunked-group-count)))
                     (map-put! state :thought-request-id request-id)
                     (map-put! state :thought-block-id block-id))
                   (when reuse
                     (if block-id
                         (setq append t)
                       (setq block-id (format "%s-agent_thought_chunk"
                                              (map-elt state :chunked-group-count)))
                       (map-put! state :thought-block-id block-id)))
                 (agent-shell--update-fragment
                  :state state
                  :block-id block-id
                  :label-left  (concat
                                agent-shell-thought-process-icon
                                " "
                                (propertize "Thought process" 'font-lock-face font-lock-doc-markup-face))
                  :body .content.text
                  :append append
                  :expanded agent-shell-thought-process-expand-by-default)))
               (map-put! state :last-entry-type "agent_thought_chunk"))
              ((equal (map-elt update 'sessionUpdate) "agent_message_chunk")
               (unless (equal (map-elt state :last-entry-type) "agent_message_chunk")
                 (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                 (agent-shell--append-transcript
                  :text (format "## Agent (%s)\n\n" (format-time-string "%F %T"))
                  :file-path agent-shell--transcript-file))
               (let-alist update
                 (agent-shell--append-transcript
                  :text .content.text
                  :file-path agent-shell--transcript-file)
                 (agent-shell--update-fragment
                  :state state
                  :block-id (format "%s-agent_message_chunk"
                                    (map-elt state :chunked-group-count))
                  :body .content.text
                  :create-new (not (equal (map-elt state :last-entry-type)
                                          "agent_message_chunk"))
                  :append t
                  :navigation 'never))
               (map-put! state :last-entry-type "agent_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "user_message_chunk")
               (let ((new-prompt-p (not (equal (map-elt state :last-entry-type)
                                               "user_message_chunk"))))
                 (when new-prompt-p
                   (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                   (agent-shell--append-transcript
                    :text (format "## User (%s)\n\n" (format-time-string "%F %T"))
                    :file-path agent-shell--transcript-file))
                 (let-alist update
                   (agent-shell--append-transcript
                    :text (format "> %s\n" .content.text)
                    :file-path agent-shell--transcript-file)
                   (agent-shell--update-text
                    :state state
                    :block-id (format "%s-user_message_chunk"
                                      (map-elt state :chunked-group-count))
                    :text (if new-prompt-p
                              (concat (propertize
                                       (map-nested-elt
                                        state '(:agent-config :shell-prompt))
                                       'font-lock-face 'comint-highlight-prompt)
                                      (propertize .content.text
                                                  'font-lock-face 'comint-highlight-input))
                            (propertize .content.text
                                        'font-lock-face 'comint-highlight-input))
                    :create-new new-prompt-p
                    :append t)))
               (map-put! state :last-entry-type "user_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "plan")
               (let-alist update
                 (agent-shell--update-fragment
                  :state state
                  :block-id "plan"
                  :label-left (propertize "Plan" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-plan .entries)
                  :expanded t))
               (map-put! state :last-entry-type "plan"))
              ((equal (map-elt update 'sessionUpdate) "tool_call_update")
               (agent-shell--handle-tool-call-update-streaming state update)
               (map-put! state :last-entry-type "tool_call_update"))
              ((equal (map-elt update 'sessionUpdate) "available_commands_update")
               (let-alist update
                 (map-put! state :available-commands (map-elt update 'availableCommands))
                 (agent-shell--update-fragment
                  :state state
                  :namespace-id "bootstrapping"
                  :block-id "available_commands_update"
                  :label-left (propertize "Available commands" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-available-commands (map-elt update 'availableCommands))))
               (map-put! state :last-entry-type "available_commands_update"))
              ((equal (map-elt update 'sessionUpdate) "current_mode_update")
               (let ((updated-session (map-elt state :session))
                     (new-mode-id (map-elt update 'currentModeId)))
                 (map-put! updated-session :mode-id new-mode-id)
                 (map-put! state :session updated-session)
                 (message "Session mode: %s"
                          (agent-shell--resolve-session-mode-name
                           new-mode-id
                           (agent-shell--get-available-modes state)))
                 ;; Note: No need to set :last-entry-type as no text was inserted.
                 (agent-shell--update-header-and-mode-line)))
              ((equal (map-elt update 'sessionUpdate) "config_option_update")
               ;; Silently handle config option updates (e.g., from set_model/set_mode)
               ;; These are informational notifications that don't require user-visible output
               ;; Note: No need to set :last-entry-type as no text was inserted.
               nil)
              ((equal (map-elt update 'sessionUpdate) "usage_update")
               ;; Extract context window and cost information
               (agent-shell--update-usage-from-notification :state state :update update)
               ;; Update header to reflect new context usage indicator
               (agent-shell--update-header-and-mode-line)
               ;; Note: This is session-level state, no need to set :last-entry-type
               nil)
              (t
               (agent-shell--update-fragment
                :state state
                :block-id "Session Update - fallback"
                :body (format "%s" notification)
                :create-new t
                :navigation 'never)
               (map-put! state :last-entry-type nil)))
               (error
                (unless (eq (car err) 'quit)
                  (agent-shell--debug-log-notification-error state update err))
                (signal (car err) (cdr err))))))
          (t
           (agent-shell--update-fragment
            :state state
            :block-id "Notification - fallback"
            :body (format "Unhandled notification (%s) and include:

```json
%s
```"
                          (agent-shell-ui-add-action-to-text
                           "please file a feature request"
                           (lambda ()
                             (interactive)
                             (browse-url "https://github.com/xenodium/agent-shell/issues/new/choose"))
                           (lambda ()
                             (message "Press RET to open URL"))
                           'link)
                          (with-temp-buffer
                            (insert (json-serialize notification))
                            (json-pretty-print (point-min) (point-max))
                            (buffer-string)))
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--on-request (&key state request)
  "Handle incoming request using SHELL, STATE, and REQUEST."
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (agent-shell--save-tool-call
            state .params.toolCall.toolCallId
            (append (list (cons :title .params.toolCall.title)
                          (cons :status .params.toolCall.status)
                          (cons :kind .params.toolCall.kind)
                          (cons :permission-request-id .id))
                    (when-let ((diff (agent-shell--make-diff-info
                                      :tool-call .params.toolCall)))
                      (list (cons :diff diff)))))
           (agent-shell--update-fragment
            :state state
            ;; block-id must be the same as the one used
            ;; in agent-shell--delete-fragment param.
            :block-id (format "permission-%s" .params.toolCall.toolCallId)
            :body (with-current-buffer (map-elt state :buffer)
                    (agent-shell--make-tool-call-permission-text
                     :request request
                     :client (map-elt state :client)
                     :state state))
            :expanded t
            :navigation 'never)
           (agent-shell-jump-to-latest-permission-button-row)
           (when-let (((map-elt state :buffer))
                      (viewport-buffer (agent-shell-viewport--buffer
                                        :shell-buffer (map-elt state :buffer)
                                        :existing-only t)))
             (with-current-buffer viewport-buffer
               (agent-shell-jump-to-latest-permission-button-row)))
           (map-put! state :last-entry-type "session/request_permission"))
          ((equal .method "fs/read_text_file")
           (agent-shell--on-fs-read-text-file-request
            :state state
            :request request))
          ((equal .method "fs/write_text_file")
           (agent-shell--on-fs-write-text-file-request
            :state state
            :request request))
          ((equal .method "terminal/create")
           (agent-shell--on-terminal-create-request
            :state state
            :request request))
          ((equal .method "terminal/output")
           (agent-shell--on-terminal-output-request
            :state state
            :request request))
          ((equal .method "terminal/wait_for_exit")
           (agent-shell--on-terminal-wait-for-exit-request
            :state state
            :request request))
          ((equal .method "terminal/kill")
           (agent-shell--on-terminal-kill-request
            :state state
            :request request))
          ((equal .method "terminal/release")
           (agent-shell--on-terminal-release-request
            :state state
            :request request))
          (t
           (agent-shell--update-fragment
            :state state
            :block-id "Unhandled Incoming Request"
            :body (format "⚠ Unhandled incoming request: \"%s\"" .method)
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--on-terminal-create-request (&key state request)
  "Handle terminal/create REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((params .params)
               (command (agent-shell--meta-lookup params 'command))
               (args (agent-shell--terminal-normalize-args
                      (agent-shell--meta-lookup params 'args)))
               (env (agent-shell--terminal-normalize-env
                     (agent-shell--meta-lookup params 'env)))
               (cwd (agent-shell--meta-lookup params 'cwd))
               (output-byte-limit (let ((limit (agent-shell--meta-lookup params 'outputByteLimit)))
                                    (when (numberp limit) limit))))
          (unless (stringp command)
            (error "terminal/create missing command"))
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
                             (:waiters . nil)
                             (:released . nil)
                             (:cleanup-timer . nil)
                             (:last-access . ,(float-time))))
                 (proc (progn
                         (unless (file-directory-p default-directory)
                           (error "terminal/create cwd not found: %s" default-directory))
                         (make-process
                          :name (format "agent-shell-terminal-%s" terminal-id)
                          :buffer output-buffer
                          :command command-list
                          :noquery t
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
