;;; agent-shell-project-tests.el --- Tests for agent-shell-project -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-project)

;;; Code:

(ert-deftest agent-shell-cwd-resolver-default-test ()
  "Test `agent-shell-cwd' without overrides."
  (let ((temp-dir (make-temp-file "agent-shell-cwd-default-" t)))
    (unwind-protect
        (let ((agent-shell-cwd-resolver nil)
              (default-directory (file-name-as-directory temp-dir)))
          (let ((projectile-mode nil))
            (cl-letf (((symbol-function 'project-current) (lambda () nil)))
              (should (equal (agent-shell-cwd)
                             (file-name-as-directory temp-dir))))))
      (delete-directory temp-dir))))

(ert-deftest agent-shell-cwd-resolver-variable-override-test ()
  "Test `agent-shell-cwd' with a variable symbol override."
  (let ((temp-dir (make-temp-file "agent-shell-cwd-variable-" t)))
    (unwind-protect
        (let ((agent-shell-cwd-resolver 'default-directory)
              (default-directory (file-name-as-directory temp-dir)))
          (let ((projectile-mode nil))
            (cl-letf (((symbol-function 'project-current)
                       (lambda () (error "project-current should not be called")))
                      ((symbol-function 'project-root)
                       (lambda (_) (error "project-root should not be called"))))
              (should (equal (agent-shell-cwd)
                             (file-name-as-directory temp-dir))))))
      (delete-directory temp-dir))))

(ert-deftest agent-shell-cwd-resolver-function-override-test ()
  "Test `agent-shell-cwd' with a function override."
  (let ((temp-dir (make-temp-file "agent-shell-cwd-function-" t)))
    (unwind-protect
        (let ((agent-shell-cwd-resolver (lambda ()
                                          (file-name-as-directory temp-dir)))
              (default-directory "/tmp/agent-shell-default/"))
          (let ((projectile-mode nil))
            (cl-letf (((symbol-function 'project-current)
                       (lambda () (error "project-current should not be called")))
                      ((symbol-function 'project-root)
                       (lambda (_) (error "project-root should not be called"))))
              (should (equal (agent-shell-cwd)
                             (file-name-as-directory temp-dir))))))
      (delete-directory temp-dir))))

(ert-deftest agent-shell-cwd-resolver-invalid-override-test ()
  "Test `agent-shell-cwd' with an invalid override."
  (let ((agent-shell-cwd-resolver (make-symbol "agent-shell-cwd-unbound"))
        (default-directory "/tmp/agent-shell-default/"))
    (let ((projectile-mode nil))
      (should-error (agent-shell-cwd) :type 'error))))

(ert-deftest agent-shell-cwd-resolver-nil-override-test ()
  "Test `agent-shell-cwd' when override returns nil."
  (let ((agent-shell-cwd-resolver (lambda () nil))
        (default-directory "/tmp/agent-shell-default/"))
    (let ((projectile-mode nil))
      (should-error (agent-shell-cwd) :type 'error))))

(provide 'agent-shell-project-tests)
;;; agent-shell-project-tests.el ends here
