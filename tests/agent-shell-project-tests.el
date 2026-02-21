;;; agent-shell-project-tests.el --- Tests for agent-shell-project -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-project)

;;; Code:

(ert-deftest agent-shell-project-root-function-default-test ()
  "Test `agent-shell-project-root' without overrides."
  (let ((temp-dir (make-temp-file "agent-shell-project-root-default-" t)))
    (unwind-protect
        (let ((agent-shell-project-root-function nil)
              (default-directory (file-name-as-directory temp-dir)))
          (let ((projectile-mode nil))
            (cl-letf (((symbol-function 'project-current) (lambda () nil)))
              (should (equal (agent-shell-project-root)
                             (file-name-as-directory temp-dir))))))
      (delete-directory temp-dir))))

(ert-deftest agent-shell-project-root-function-override-test ()
  "Test `agent-shell-project-root' with a function override."
  (let ((temp-dir (make-temp-file "agent-shell-project-root-function-" t)))
    (unwind-protect
        (let ((agent-shell-project-root-function (lambda ()
                                          (file-name-as-directory temp-dir)))
              (default-directory "/tmp/agent-shell-default/"))
          (let ((projectile-mode nil))
            (cl-letf (((symbol-function 'project-current)
                       (lambda () (error "project-current should not be called")))
                      ((symbol-function 'project-root)
                       (lambda (_) (error "project-root should not be called"))))
              (should (equal (agent-shell-project-root)
                             (file-name-as-directory temp-dir))))))
      (delete-directory temp-dir))))

(ert-deftest agent-shell-project-root-function-invalid-override-test ()
  "Test `agent-shell-project-root' with an invalid override."
  (let ((agent-shell-project-root-function 'default-directory)
        (default-directory "/tmp/agent-shell-default/"))
    (let ((projectile-mode nil))
      (should-error (agent-shell-project-root) :type 'error))))

(ert-deftest agent-shell-project-root-function-nil-override-test ()
  "Test `agent-shell-project-root' when override returns nil."
  (let ((agent-shell-project-root-function (lambda () nil))
        (default-directory "/tmp/agent-shell-default/"))
    (let ((projectile-mode nil))
      (should-error (agent-shell-project-root) :type 'error))))

(provide 'agent-shell-project-tests)
;;; agent-shell-project-tests.el ends here
