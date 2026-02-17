;;; agent-shell-debug.el --- Debug helpers for agent-shell -*- lexical-binding: t; -*-

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
;; Debug helpers for agent-shell.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'acp)
(require 'comint)
(require 'map)
(require 'shell-maker)
(require 'subr-x)

(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-previous-permission-button "agent-shell")
(declare-function agent-shell-ui-backward-block "agent-shell-ui")

(defvar agent-shell--state)
(defvar agent-shell--transcript-file)
(defvar agent-shell--version)

;;; TODO These two new variables would be better as agent-shell-verbose.
;;; `nil` means off but you can set it to other levels. take inspiration
;;; from tramp-verbose.
;;; @/opt/homebrew/Cellar/emacs/30.2/share/emacs/30.2/lisp/net/tramp-message.el.gz:60-77
(defcustom agent-shell-logging-enabled t
  "Whether ACP logging/traffic capture is enabled for agent shells."
  :type 'boolean
  :group 'agent-shell
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq acp-logging-enabled value)))

(defcustom agent-shell-debug-logging-enabled t
  "Whether agent-shell debug logging is enabled."
  :type 'boolean
  :group 'agent-shell)

(defun agent-shell-toggle-logging ()
  "Toggle logging."
  (interactive)
  (setq agent-shell-logging-enabled (not acp-logging-enabled))
  (setq acp-logging-enabled agent-shell-logging-enabled)
  (message "Logging: %s" (if acp-logging-enabled "ON" "OFF")))

(defun agent-shell-reset-logs ()
  "Reset all log buffers."
  (interactive)
  (acp-reset-logs :client (map-elt (agent-shell--state) :client))
  (message "Logs reset"))

(defvar agent-shell--debug-log-buffer-name "*Agent Shell Debug*")

(defun agent-shell--debug-log (fmt &rest args)
  "Append a timestamped debug log line."
  (when agent-shell-debug-logging-enabled
    (with-current-buffer (get-buffer-create agent-shell--debug-log-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format-time-string "%FT%T%z "))
        (insert (apply #'format fmt args))
        (insert "\n")))))

(defun agent-shell--debug-buffer-read-only (buffer)
  "Return BUFFER read-only state or nil when BUFFER is not live."
  (when (buffer-live-p buffer)
    (buffer-local-value 'buffer-read-only buffer)))

(defun agent-shell--debug-window-info (buffer)
  "Collect window point/prompt state for BUFFER."
  (let ((info nil))
    (dolist (window (get-buffer-window-list buffer nil t))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (window-point window))
          (let ((at-prompt (ignore-errors (shell-maker-point-at-last-prompt-p))))
            (push (list :window window
                        :selected (eq window (selected-window))
                        :point (point)
                        :point-max (point-max)
                        :at-prompt at-prompt)
                  info)))))
    (nreverse info)))


(defun agent-shell--debug-window-info-string (buffer)
  "Format window info for BUFFER."
  (let ((info (agent-shell--debug-window-info buffer)))
    (if (null info)
        "no-windows"
      (mapconcat
       (lambda (entry)
         (format "{win=%s selected=%s point=%d point-max=%d at-prompt=%s}"
                 (plist-get entry :window)
                 (plist-get entry :selected)
                 (plist-get entry :point)
                 (plist-get entry :point-max)
                 (plist-get entry :at-prompt)))
       info
       " "))))

(defun agent-shell-debug-previous-item ()
  "Report navigation candidates for previous-item in an agent shell buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent shell buffer"))
  (let* ((current-pos (point))
         (prompt-pos (save-excursion
                       (when (comint-next-prompt -1)
                         (let ((pos (point)))
                           (when (< pos current-pos)
                             pos)))))
         (block-pos (save-excursion
                      (let ((pos (agent-shell-ui-backward-block)))
                        (when (and pos (< pos current-pos))
                          pos))))
         (button-pos (save-excursion
                       (let ((pos (agent-shell-previous-permission-button)))
                         (when (and pos (< pos current-pos))
                           pos))))
         (positions (delq nil (list prompt-pos block-pos button-pos)))
         (next-pos (when positions
                     (apply #'max positions))))
    (message "cur=%s prompt=%s block=%s button=%s next=%s field=%S ui=%S"
             current-pos
             prompt-pos
             block-pos
             button-pos
             next-pos
             (and next-pos (get-text-property next-pos 'field))
             (and next-pos (get-text-property next-pos 'agent-shell-ui-state)))))


(defun agent-shell--debug-log-notification-error (state update err)
  "Log notification ERR details with buffer/window context."
  (let* ((shell-buffer (map-elt state :buffer))
         (current-buffer (current-buffer))
         (session-update (map-elt update 'sessionUpdate))
         (tool-call-id (map-elt update 'toolCallId)))
    (agent-shell--debug-log
     "NOTIFICATION ERROR: %s update=%s toolCallId=%s current=%s ro=%s shell=%s ro=%s windows=%s"
     (error-message-string err)
     session-update
     tool-call-id
     (buffer-name current-buffer)
     (agent-shell--debug-buffer-read-only current-buffer)
     (when (buffer-live-p shell-buffer) (buffer-name shell-buffer))
     (agent-shell--debug-buffer-read-only shell-buffer)
     (when (buffer-live-p shell-buffer)
       (agent-shell--debug-window-info-string shell-buffer)))))

(defun agent-shell--debug-bundle-copy-to-clipboard (text)
  "Copy TEXT to the clipboard and kill ring when possible."
  (when (and text (not (string-empty-p text)))
    (kill-new text)
    (when (fboundp 'gui-set-selection)
      (gui-set-selection 'CLIPBOARD text))
    (when (executable-find "pbcopy")
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max) "pbcopy" nil nil nil)))
    text))

(defun agent-shell--debug-bundle-write-load-path (path)
  "Write `load-path' to PATH."
  (with-temp-file path
    (let ((print-circle t)
          (print-length nil)
          (print-level nil))
      (pp load-path (current-buffer)))))

(defun agent-shell--debug-bundle-write-libraries (path)
  "Write library locations for core packages to PATH."
  (with-temp-file path
    (dolist (lib '("agent-shell"
                   "agent-shell-openai"
                   "agent-shell-ui"
                   "shell-maker"
                   "acp"))
      (insert (format "%s: %s\n"
                      lib
                      (or (locate-library lib) "not-found"))))))

(defun agent-shell--debug-bundle-write-env (path)
  "Write relevant environment variables to PATH."
  (with-temp-file path
    (dolist (var '("TIMVISHER_AGENT_SHELL_CHECKOUT"
                   "TIMVISHER_SHELL_MAKER_CHECKOUT"
                   "TIMVISHER_ACP_CHECKOUT"
                   "TIMVISHER_CODEX_LAUNCHER_PATH"
                   "TIMVISHER_CODEX_LAUNCHER_SHA256"
                   "TIMVISHER_CODEX_PROMPT"
                   "TIMVISHER_CODEX_LOG_FILE"))
      (insert (format "%s=%s\n" var (or (getenv var) ""))))))

(defun agent-shell--debug-bundle-write-recent-keys (path)
  "Write recent key events to PATH."
  (with-temp-file path
    (let ((keys (recent-keys)))
      (insert (format-kbd-macro keys t))
      (insert "\n"))))

(defun agent-shell--debug-bundle-write-lossage (path)
  "Write lossage output to PATH."
  (save-window-excursion
    (condition-case err
        (view-lossage)
      (error
       (agent-shell--debug-log "lossage capture failed: %s" err)))
    (let ((lossage-buffer (get-buffer "*Lossage*")))
      (if lossage-buffer
          (agent-shell--debug-bundle-write-buffer lossage-buffer path)
        (agent-shell--debug-bundle-write-recent-keys path)))))

(defun agent-shell--debug-bundle-default-dir ()
  "Return default directory for debug bundles."
  (let* ((root (agent-shell-cwd))
         (dir (expand-file-name ".agent-shell/debug-bundles" root))
         (stamp (format-time-string "%Y%m%d-%H%M%S")))
    (expand-file-name (format "bundle-%s" stamp) dir)))

(defun agent-shell--debug-bundle-write-buffer (buffer path)
  "Write BUFFER contents to PATH when BUFFER exists."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (write-region (point-min) (point-max) path nil 'silent))))

(defun agent-shell--debug-bundle-write-traffic (client path)
  "Write ACP traffic for CLIENT to PATH."
  (when client
    (let ((buffer (acp-traffic-buffer :client client)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'acp-traffic-mode)
          (acp-traffic-mode))
        (let ((objects (acp-traffic--objects)))
          (with-temp-file path
            (let ((print-circle t))
              (pp objects (current-buffer)))))))))

(defun agent-shell--debug-bundle-write-metadata (path shell-buffer transcript project-root)
  "Write metadata for SHELL-BUFFER and TRANSCRIPT to PATH."
  (with-temp-file path
    (insert (format "timestamp: %s\n" (format-time-string "%F %T")))
    (insert (format "emacs-version: %s\n" (emacs-version)))
    (insert (format "agent-shell-version: %s\n" agent-shell--version))
    (insert (format "logging-enabled: %s\n" (if acp-logging-enabled "true" "false")))
    (insert (format "shell-buffer: %s\n" (buffer-name shell-buffer)))
    (insert (format "project-root: %s\n" project-root))
    (insert (format "transcript: %s\n" (or transcript "none")))
    (insert (format "launcher-path: %s\n" (or (getenv "TIMVISHER_CODEX_LAUNCHER_PATH") "none")))
    (insert (format "launcher-sha256: %s\n" (or (getenv "TIMVISHER_CODEX_LAUNCHER_SHA256") "none")))
    (insert (format "agent-shell-library: %s\n" (or (locate-library "agent-shell") "not-found")))
    (insert (format "acp-library: %s\n" (or (locate-library "acp") "not-found")))
    (insert (format "shell-maker-library: %s\n" (or (locate-library "shell-maker") "not-found")))
    (insert "files: load-path.el libraries.txt environment.txt agent-shell-debug.log messages.log lossage.log\n")))

(defun agent-shell-save-debug-bundle (directory)
  "Save ACP traffic/logs, transcript, *Messages*, and lossage to DIRECTORY."
  (interactive (list (read-directory-name
                      "Save debug bundle to: "
                      (agent-shell--debug-bundle-default-dir)
                      nil nil)))
  (let* ((shell-buffer (agent-shell--shell-buffer :no-create t :no-error t)))
    (unless shell-buffer
      (user-error "No agent shell buffer found"))
    (let* ((client (with-current-buffer shell-buffer
                     (map-elt agent-shell--state :client)))
           (transcript (with-current-buffer shell-buffer
                         agent-shell--transcript-file))
           (project-root (with-current-buffer shell-buffer
                           (agent-shell-cwd)))
           (dir (file-name-as-directory directory))
           (traffic-path (expand-file-name "traffic.traffic" dir))
           (log-path (expand-file-name "acp.log" dir))
           (debug-log-path (expand-file-name "agent-shell-debug.log" dir))
           (messages-path (expand-file-name "messages.log" dir))
           (lossage-path (expand-file-name "lossage.log" dir))
           (recent-keys-path (expand-file-name "recent-keys.txt" dir))
           (transcript-path (expand-file-name "transcript.md" dir))
           (load-path-path (expand-file-name "load-path.el" dir))
           (libraries-path (expand-file-name "libraries.txt" dir))
           (env-path (expand-file-name "environment.txt" dir))
           (meta-path (expand-file-name "metadata.txt" dir)))
      (make-directory dir t)
      (agent-shell--debug-bundle-write-traffic client traffic-path)
      (agent-shell--debug-bundle-write-buffer (acp-logs-buffer :client client) log-path)
      (agent-shell--debug-bundle-write-buffer (get-buffer agent-shell--debug-log-buffer-name)
                                              debug-log-path)
      (agent-shell--debug-bundle-write-buffer (get-buffer "*Messages*") messages-path)
      (agent-shell--debug-bundle-write-lossage lossage-path)
      (agent-shell--debug-bundle-write-recent-keys recent-keys-path)
      (agent-shell--debug-bundle-write-load-path load-path-path)
      (agent-shell--debug-bundle-write-libraries libraries-path)
      (agent-shell--debug-bundle-write-env env-path)
      (when (and transcript (file-readable-p transcript))
        (copy-file transcript transcript-path t))
      (agent-shell--debug-bundle-write-metadata meta-path shell-buffer transcript project-root)
      (agent-shell--debug-log "Saved debug bundle to %s" dir)
      (let ((copied (agent-shell--debug-bundle-copy-to-clipboard dir)))
        (message "Saved debug bundle to %s%s" dir (if copied " (copied to clipboard)" ""))))))


(provide 'agent-shell-debug)

;;; agent-shell-debug.el ends here
