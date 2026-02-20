;;; agent-shell-meta-tests.el --- Tests for agent-shell meta helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell-meta)

;;; Code:

(ert-deftest agent-shell--tool-call-meta-response-text-test ()
  "Extract toolResponse text from meta updates."
  (let ((update '((_meta . ((agent . ((toolResponse . ((content . "ok"))))))))))
    (should (equal (agent-shell--tool-call-meta-response-text update) "ok")))
  (let ((update '((_meta . ((toolResponse . [((type . "text") (text . "one"))
                                             ((type . "text") (text . "two"))]))))))
    (should (equal (agent-shell--tool-call-meta-response-text update)
                   "one\n\ntwo"))))

(provide 'agent-shell-meta-tests)
;;; agent-shell-meta-tests.el ends here
