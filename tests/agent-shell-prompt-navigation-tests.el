;;; agent-shell-prompt-navigation-tests.el --- Prompt navigation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'shell-maker)

(defun agent-shell-prompt-navigation-test--make-shell ()
  "Create a minimal shell-maker buffer for prompt navigation tests."
  (let* ((config (make-shell-maker-config
                  :name "PromptNav"
                  :prompt "PromptNav> "
                  :prompt-regexp "PromptNav> "
                  :execute-command
                  (lambda (command shell)
                    (with-current-buffer (map-elt shell :buffer)
                      (shell-maker-write-output :config shell-maker--config
                                                :output (concat "OK " command "\n"))
                      (shell-maker-finish-output :config shell-maker--config :success t)))))
         (buffer (shell-maker-start config t nil t)))
    (with-current-buffer buffer
      (setq-local comint-use-prompt-regexp t))
    buffer))

(defmacro agent-shell-prompt-navigation-test--with-shell (&rest body)
  "Execute BODY with a temporary shell buffer as current."
  (declare (indent 0) (debug t))
  `(let ((buf (agent-shell-prompt-navigation-test--make-shell)))
     (unwind-protect
         (with-current-buffer buf
           ,@body)
       (when (buffer-live-p buf)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer buf))))))

(ert-deftest agent-shell-prompt-navigation-previous-prompt-skips-marker ()
  "Ensure `comint-previous-prompt' lands on a prompt line."
  (agent-shell-prompt-navigation-test--with-shell
    (goto-char (point-max))
    (insert "1")
    (shell-maker-submit)
    (goto-char (point-max))
    (comint-previous-prompt 1)
    (let ((line-start (line-beginning-position)))
      (should (save-excursion
                (goto-char line-start)
                (looking-at comint-prompt-regexp))))))

(provide 'agent-shell-prompt-navigation-tests)
;;; agent-shell-prompt-navigation-tests.el ends here
