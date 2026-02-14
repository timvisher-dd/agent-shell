;;; shell-command-output-repro.el --- Comint shell output baseline -*- lexical-binding: t; -*-

;; Run with (example):
;; SHELL_REPRO_COMMAND="bash -lc 'for x in {1..35000}; do printf \"line %d\\n\" \"$x\"; done'" \
;; emacs --batch -l scripts/shell-command-output-repro.el

(require 'package)
(require 'shell)
(require 'comint)

(defconst agent-shell-tests--shell-prompt "PROMPT> ")
(defconst agent-shell-tests--shell-prompt-regexp "^PROMPT> ")

(defvar agent-shell-tests--shell-done nil)
(defvar agent-shell-tests--shell-sent nil)
(defvar agent-shell-tests--shell-start-time nil)
(defvar agent-shell-tests--shell-command-marker nil)
(defvar agent-shell-tests--shell-waiting nil)
(defvar agent-shell-tests--shell-command nil)

(defun agent-shell-tests--shell-env-int (name default)
  "Read integer environment variable NAME or return DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun agent-shell-tests--shell-disable-user-init-hooks ()
  "Disable user init hooks that would load external env tooling."
  (when (fboundp 'timvisher-load-op-environment-variables)
    (fset 'timvisher-load-op-environment-variables
          (lambda () (message "Skipping timvisher-load-op-environment-variables"))))
  (setq after-load-alist (assq-delete-all 'agent-shell after-load-alist))
  (setq after-load-alist (assq-delete-all "agent-shell" after-load-alist)))

(defun agent-shell-tests--shell-prompt-visible-p (buf)
  "Return non-nil when the prompt is visible at the end of BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line 0)
        (looking-at agent-shell-tests--shell-prompt-regexp)))))

(defun agent-shell-tests--shell-finish (status)
  "Print STATUS and exit with success or failure."
  (let* ((buf (get-buffer "*shell*"))
         (lines (if buf
                    (with-current-buffer buf
                      (line-number-at-pos (point-max)))
                  0))
         (size (if buf
                   (with-current-buffer buf (buffer-size))
                 0))
         (elapsed (and agent-shell-tests--shell-start-time
                       (- (float-time) agent-shell-tests--shell-start-time))))
    (princ (format (concat "SHELL_COMMAND_OUTPUT_RESULT status=%s"
                           " lines=%d size=%d elapsed=%.2fs\n")
                   status lines size (or elapsed 0.0)))
    (setq agent-shell-tests--shell-done t)
    (kill-emacs 0)))

(defun agent-shell-tests--shell-output-filter (_output)
  "Detect prompt after command completes."
  (when (and agent-shell-tests--shell-waiting
             (buffer-live-p (current-buffer))
             (markerp agent-shell-tests--shell-command-marker))
    (save-excursion
      (goto-char agent-shell-tests--shell-command-marker)
      (when (re-search-forward agent-shell-tests--shell-prompt-regexp nil t)
        (setq agent-shell-tests--shell-waiting nil)
        (agent-shell-tests--shell-finish "completed")))))

(defun agent-shell-tests--shell-maybe-send (buf)
  "Send the command once BUF shows a prompt."
  (when (and (not agent-shell-tests--shell-sent)
             (agent-shell-tests--shell-prompt-visible-p buf))
    (setq agent-shell-tests--shell-sent t)
    (with-current-buffer buf
      (setq agent-shell-tests--shell-start-time (float-time))
      (setq agent-shell-tests--shell-command-marker (copy-marker (point-max)))
      (setq agent-shell-tests--shell-waiting t)
      (comint-send-string (get-buffer-process buf)
                          (concat agent-shell-tests--shell-command "\n")))))

(defun agent-shell-tests--shell-main ()
  "Run the shell.el output repro."
  (package-initialize)
  (agent-shell-tests--shell-disable-user-init-hooks)
  (let* ((command (or (getenv "SHELL_REPRO_COMMAND")
                      "bash -lc 'for x in {1..35000}; do printf \"line %d\\n\" \"$x\"; done'"))
         (timeout (max 1 (agent-shell-tests--shell-env-int "SHELL_REPRO_TIMEOUT" 600)))
         (shell-prog (or (getenv "SHELL_REPRO_SHELL") "bash"))
         (buffer-name "*shell*"))
    (setq agent-shell-tests--shell-command command)
    (let* ((explicit-shell-file-name shell-prog)
           (explicit-bash-args '("--noprofile" "--norc" "-i"))
           (process-environment
            (append (list (concat "PS1=" agent-shell-tests--shell-prompt)
                          "PROMPT_COMMAND="
                          "TERM=dumb")
                    process-environment)))
      (shell buffer-name))

    (let ((buf (get-buffer buffer-name)))
      (unless (buffer-live-p buf)
        (error "Failed to start shell buffer"))
      (with-current-buffer buf
        (setq-local comint-prompt-regexp agent-shell-tests--shell-prompt-regexp)
        (setq-local comint-use-prompt-regexp t)
        (add-hook 'comint-output-filter-functions
                  #'agent-shell-tests--shell-output-filter nil t))

      (run-at-time timeout nil
                   (lambda ()
                     (agent-shell-tests--shell-finish "timeout")))

      (while (not agent-shell-tests--shell-done)
        (agent-shell-tests--shell-maybe-send buf)
        (accept-process-output nil 0.1)))))

(when noninteractive
  (agent-shell-tests--shell-main))

(provide 'shell-command-output-repro)
;;; shell-command-output-repro.el ends here
