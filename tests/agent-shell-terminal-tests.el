;;; agent-shell-terminal-tests.el --- Tests for agent-shell terminal capability -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell--terminal-normalize-env-test ()
  "Normalize terminal env entries to NAME=VALUE strings."
  (should (equal (agent-shell--terminal-normalize-env
                  [((name . "FOO") (value . "bar"))
                   ((name . "EMPTY"))])
                 '("FOO=bar" "EMPTY="))))

(ert-deftest agent-shell--terminal-output-streams-to-buffer-test ()
  "Stream terminal output referenced in tool call content into the shell buffer."
  (let* ((buffer (get-buffer-create " *agent-shell-terminal-output-stream*"))
         (terminal-id "term_1")
         (output-buffer nil)
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (setq output-buffer (agent-shell--terminal-make-output-buffer terminal-id))
    (agent-shell--terminal-put
     agent-shell--state
     terminal-id
     `((:id . ,terminal-id)
       (:process . nil)
       (:output-buffer . ,output-buffer)
       (:output-byte-limit . nil)
       (:tool-call-ids . nil)
       (:waiters . nil)
       (:released . nil)
       (:cleanup-timer . nil)
       (:last-access . ,(float-time))))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (lambda (&rest _args) nil)))
          (with-current-buffer buffer
            (agent-shell--on-notification
             :state agent-shell--state
             :notification `((method . "session/update")
                             (params . ((update . ((sessionUpdate . "tool_call")
                                                    (toolCallId . "call-1")
                                                    (status . "in_progress")
                                                    (title . "Terminal")
                                                    (kind . "tool")
                                                    (content . [((type . "terminal")
                                                                 (terminalId . ,terminal-id))]))))))))
          (agent-shell--terminal-handle-output
           agent-shell--state
           terminal-id
           "terminal chunk")
          (with-current-buffer buffer
            (should (string-match-p "terminal chunk" (buffer-string))))
          (with-current-buffer output-buffer
            (should (string-match-p "terminal chunk" (buffer-string)))))
      (when terminal-id
        (agent-shell--terminal-remove agent-shell--state terminal-id))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(ert-deftest agent-shell--terminal-create-output-test ()
  "Create a terminal and read truncated output."
  (let* ((buffer (get-buffer-create " *agent-shell-terminal-test*"))
         (temp-dir (make-temp-file "agent-shell-terminal" t))
         (responses nil)
         (terminal-id nil)
         (agent-shell--state (list (cons :buffer buffer)
                                   (cons :client 'test-client)
                                   (cons :terminals nil)
                                   (cons :terminal-count 0))))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'acp-send-response)
                   (lambda (&rest args)
                     (push args responses)))
                  ((symbol-function 'agent-shell-cwd)
                   (lambda () temp-dir)))
          (agent-shell--on-terminal-create-request
           :state agent-shell--state
           :request '((id . "req-1")
                      (params . ((command . "sh")
                                 (args . ["-c" "printf 'hello'"])
                                 (outputByteLimit . 2)))))
          (let* ((response (plist-get (car responses) :response))
                 (result (map-elt response :result)))
            (setq terminal-id (map-elt result 'terminalId))
            (should (stringp terminal-id)))
          (let* ((terminal (agent-shell--terminal-get agent-shell--state terminal-id))
                 (proc (map-elt terminal :process)))
            (while (and proc (process-live-p proc))
              (accept-process-output proc 0.1))
            (setq responses nil)
            (agent-shell--on-terminal-output-request
             :state agent-shell--state
             :request `((id . "req-2")
                        (params . ((terminalId . ,terminal-id)))))
            (let* ((out-response (plist-get (car responses) :response))
                   (out-result (map-elt out-response :result))
                   (output (map-elt out-result 'output)))
              (should (stringp output))
              (should (<= (length output) 2))
              (should (eq (map-elt out-result 'truncated) t))
              (should (equal (map-nested-elt out-result '(exitStatus exitCode)) 0)))))
      (when terminal-id
        (agent-shell--terminal-remove agent-shell--state terminal-id))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest agent-shell--terminal-wait-for-exit-test ()
  "Wait for a terminal process to exit and report status."
  (let* ((buffer (get-buffer-create " *agent-shell-terminal-wait*"))
         (temp-dir (make-temp-file "agent-shell-terminal" t))
         (responses nil)
         (terminal-id nil)
         (agent-shell--state (list (cons :buffer buffer)
                                   (cons :client 'test-client)
                                   (cons :terminals nil)
                                   (cons :terminal-count 0))))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'acp-send-response)
                   (lambda (&rest args)
                     (push args responses)))
                  ((symbol-function 'agent-shell-cwd)
                   (lambda () temp-dir)))
          (agent-shell--on-terminal-create-request
           :state agent-shell--state
           :request '((id . "req-1")
                      (params . ((command . "sh")
                                 (args . ["-c" "exit 0"])))))
          (let* ((response (plist-get (car responses) :response))
                 (result (map-elt response :result)))
            (setq terminal-id (map-elt result 'terminalId)))
          (let* ((terminal (agent-shell--terminal-get agent-shell--state terminal-id))
                 (proc (map-elt terminal :process)))
            (while (and proc (process-live-p proc))
              (accept-process-output proc 0.1)))
          (setq responses nil)
          (agent-shell--on-terminal-wait-for-exit-request
           :state agent-shell--state
           :request `((id . "req-2")
                      (params . ((terminalId . ,terminal-id)))))
          (let* ((wait-response (plist-get (car responses) :response))
                 (wait-result (map-elt wait-response :result)))
            (should (equal (map-elt wait-result 'exitCode) 0))))
      (when terminal-id
        (agent-shell--terminal-remove agent-shell--state terminal-id))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest agent-shell--terminal-release-test ()
  "Release a terminal and stop its process."
  (let* ((buffer (get-buffer-create " *agent-shell-terminal-release*"))
         (temp-dir (make-temp-file "agent-shell-terminal" t))
         (responses nil)
         (terminal-id nil)
         (agent-shell--state (list (cons :buffer buffer)
                                   (cons :client 'test-client)
                                   (cons :terminals nil)
                                   (cons :terminal-count 0))))
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'acp-send-response)
                   (lambda (&rest args)
                     (push args responses)))
                  ((symbol-function 'agent-shell-cwd)
                   (lambda () temp-dir)))
          (agent-shell--on-terminal-create-request
           :state agent-shell--state
           :request '((id . "req-1")
                      (params . ((command . "sh")
                                 (args . ["-c" "sleep 60"])))))
          (let* ((response (plist-get (car responses) :response))
                 (result (map-elt response :result)))
            (setq terminal-id (map-elt result 'terminalId)))
          (setq responses nil)
          (agent-shell--on-terminal-release-request
           :state agent-shell--state
           :request `((id . "req-2")
                      (params . ((terminalId . ,terminal-id)))))
          (let* ((release-response (plist-get (car responses) :response))
                 (terminal (agent-shell--terminal-get agent-shell--state terminal-id))
                 (proc (map-elt terminal :process)))
            (should (equal (map-elt release-response :result) nil))
            (should (map-elt terminal :released))
            (dotimes (_ 50)
              (when (and proc (process-live-p proc))
                (accept-process-output proc 0.1)))
            (should (or (not proc) (not (process-live-p proc))))))
      (when terminal-id
        (agent-shell--terminal-remove agent-shell--state terminal-id))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))

(provide 'agent-shell-terminal-tests)
;;; agent-shell-terminal-tests.el ends here
