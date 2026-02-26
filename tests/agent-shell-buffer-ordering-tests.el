;;; agent-shell-buffer-ordering-tests.el --- Tests for shell buffer ordering -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(defmacro agent-shell-buffer-ordering-tests--with-fake-shells (bindings &rest body)
  "Create temporary buffers in `agent-shell-mode', bind them, and run BODY.

BINDINGS is a list of (VAR PROJECT-DIR) pairs.  Each VAR is bound to a
buffer whose `major-mode' is `agent-shell-mode' and whose
`default-directory' is PROJECT-DIR.

All buffers are killed after BODY completes.  Viewport lookup is
stubbed out so only shell-mode buffers are considered."
  (declare (indent 1) (debug ((&rest (symbolp sexp)) body)))
  (let ((buffer-syms (mapcar #'car bindings)))
    `(let ,(mapcar (lambda (b) (list (car b) nil)) bindings)
       (unwind-protect
           (progn
             ,@(mapcar
                (lambda (b)
                  `(setq ,(car b)
                         (generate-new-buffer
                          ,(format " *test-%s*" (car b)))))
                bindings)
             ,@(mapcar
                (lambda (b)
                  `(with-current-buffer ,(car b)
                     (setq major-mode 'agent-shell-mode)
                     (setq default-directory ,(cadr b))))
                bindings)
             (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                        (lambda (_buf) nil))
                       ((symbol-function 'agent-shell-cwd)
                        (lambda ()
                          (expand-file-name default-directory))))
               ,@body))
         ,@(mapcar (lambda (sym) `(when (buffer-live-p ,sym)
                                    (kill-buffer ,sym)))
                   buffer-syms)))))

;; ---------------------------------------------------------------------------
;; Tests for (buffer-list) based ordering
;; ---------------------------------------------------------------------------

(ert-deftest agent-shell-buffers-reflects-buffer-list-order ()
  "Shells are returned in `(buffer-list)' order.

`agent-shell-buffers' iterates `(buffer-list)' and collects
`agent-shell-mode' buffers in the order it encounters them, so
the result should mirror `(buffer-list)' ordering."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    ;; Newly generated buffers go to the END of (buffer-list), so
    ;; iterating (buffer-list) encounters shell-a before shell-b.
    (should (equal (agent-shell-buffers)
                   (list shell-a shell-b)))))

(ert-deftest agent-shell-buffers-switch-to-buffer-promotes ()
  "`switch-to-buffer' promotes a shell to the front of `(buffer-list)'.

After `switch-to-buffer' to shell-b followed by switching away,
shell-b should appear before shell-a in `agent-shell-buffers'."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    (switch-to-buffer shell-b)
    (switch-to-buffer "*scratch*")
    (should (equal (agent-shell-buffers)
                   (list shell-b shell-a)))))

(ert-deftest agent-shell-buffers-select-window-promotes ()
  "`select-window' + `display-buffer' promotes a shell.

This is the code path used by `agent-shell--display-buffer'."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    (select-window (display-buffer shell-b))
    (switch-to-buffer "*scratch*")
    (should (equal (agent-shell-buffers)
                   (list shell-b shell-a)))))

(ert-deftest agent-shell-buffers-with-current-buffer-does-not-promote ()
  "`with-current-buffer' does NOT change `(buffer-list)' order.

`agent-shell--handle' dispatches commands via `with-current-buffer',
so sending commands to a shell does not promote it."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    (with-current-buffer shell-b
      (insert "simulated command"))
    (should (equal (agent-shell-buffers)
                   (list shell-a shell-b)))))

(ert-deftest agent-shell-buffers-bury-buffer-demotes ()
  "`bury-buffer' sends a shell to the end of `(buffer-list)'.

If a user leaves a shell via `quit-window' (which buries), the
shell drops to the back even if it was most recently used."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    ;; Promote shell-b to front
    (switch-to-buffer shell-b)
    (switch-to-buffer "*scratch*")
    ;; Verify shell-b is first
    (should (eq (seq-first (agent-shell-buffers)) shell-b))
    ;; Bury it
    (bury-buffer shell-b)
    ;; Now shell-a is first again
    (should (eq (seq-first (agent-shell-buffers)) shell-a))))

(ert-deftest agent-shell-buffers-no-display-buffer-stays-at-end ()
  "`generate-new-buffer' without display leaves shell at end.

Shells created via no-focus paths are never selected in a window,
so they stay at the end of `(buffer-list)' behind older shells."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    ;; Promote shell-a (simulates it being displayed at some point)
    (switch-to-buffer shell-a)
    (switch-to-buffer "*scratch*")
    ;; shell-b was never displayed, so shell-a stays ahead
    (should (eq (seq-first (agent-shell-buffers)) shell-a))))

(ert-deftest agent-shell-project-buffers-filters-by-project ()
  "`agent-shell-project-buffers' only returns shells matching the CWD."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project-a/")
       (shell-b "/tmp/project-b/")
       (shell-c "/tmp/project-a/"))
    (with-current-buffer shell-a
      (let ((project-buffers (agent-shell-project-buffers)))
        (should (= (length project-buffers) 2))
        (should (memq shell-a project-buffers))
        (should (memq shell-c project-buffers))
        (should-not (memq shell-b project-buffers))))))

(ert-deftest agent-shell-project-buffers-preserves-buffer-list-order ()
  "`agent-shell-project-buffers' preserves `(buffer-list)' order within a project."
  (agent-shell-buffer-ordering-tests--with-fake-shells
      ((shell-a "/tmp/project/")
       (shell-b "/tmp/project/"))
    ;; Promote shell-b
    (switch-to-buffer shell-b)
    (switch-to-buffer "*scratch*")
    (with-current-buffer shell-a
      (should (equal (agent-shell-project-buffers)
                     (list shell-b shell-a))))))

(provide 'agent-shell-buffer-ordering-tests)
;;; agent-shell-buffer-ordering-tests.el ends here
