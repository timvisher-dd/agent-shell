;;; agent-shell-terminal-tests.el --- Tests for agent-shell terminal capability -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'cl-lib)

;;; Code:

(defun agent-shell--test-make-terminal (terminal-id output-buffer &rest overrides)
  "Return a terminal entry for TERMINAL-ID and OUTPUT-BUFFER, applying OVERRIDES."
  (let ((terminal `((:id . ,terminal-id)
                    (:process . nil)
                    (:output-buffer . ,output-buffer)
                    (:output-byte-limit . nil)
                    (:tool-call-ids . nil)
                    (:waiters . nil)
                    (:released . nil)
                    (:cleanup-timer . nil)
                    (:last-access . ,(float-time)))))
    (dolist (override overrides)
      (setf (map-elt terminal (car override)) (cdr override)))
    terminal))

(defmacro agent-shell--with-terminal-test-fixture (buffer output-buffer terminal-id state &rest body)
  "Run BODY with a terminal test fixture.
Binds BUFFER, OUTPUT-BUFFER, TERMINAL-ID, and STATE, then cleans up."
  (declare (indent 4))
  `(let* ((,terminal-id ,(symbol-name (gensym "term_")))
          (,buffer (get-buffer-create (format " *agent-shell-terminal-test-%s*" ,terminal-id)))
          (,output-buffer nil)
          (,state (agent-shell--make-state :buffer ,buffer)))
     (map-put! ,state :client 'test-client)
     (map-put! ,state :request-count 1)
     (with-current-buffer ,buffer
       (erase-buffer)
       (agent-shell-mode)
       (setq-local agent-shell--state ,state))
     (setq ,output-buffer (agent-shell--terminal-make-output-buffer ,terminal-id))
     (agent-shell--terminal-put
      ,state ,terminal-id
      (agent-shell--test-make-terminal ,terminal-id ,output-buffer))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                    (lambda (&rest _args) nil)))
           ,@body)
       (when (agent-shell--terminal-get ,state ,terminal-id)
         (agent-shell--terminal-remove ,state ,terminal-id))
       (when (buffer-live-p ,buffer)
         (kill-buffer ,buffer))
       (when (buffer-live-p ,output-buffer)
         (kill-buffer ,output-buffer)))))

(ert-deftest agent-shell--terminal-normalize-env-test ()
  "Normalize terminal env entries to NAME=VALUE strings."
  (should (equal (agent-shell--terminal-normalize-env
                  [((name . "FOO") (value . "bar"))
                   ((name . "EMPTY"))])
                 '("FOO=bar" "EMPTY="))))

(ert-deftest agent-shell--terminal-output-streams-to-buffer-test ()
  "Stream terminal output referenced in tool call content into the shell buffer."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
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
      (should (string-match-p "terminal chunk" (buffer-string))))))

(ert-deftest agent-shell--terminal-output-does-not-accumulate-chunks-test ()
  "Terminal output does not accumulate in output chunks."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call")
                                              (toolCallId . "call-no-chunks")
                                              (status . "in_progress")
                                              (title . "Terminal")
                                              (kind . "tool")
                                              (content . [((type . "terminal")
                                                           (terminalId . ,terminal-id))]))))))))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "chunk-A")
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "chunk-B")
    (with-current-buffer buffer
      (let ((contents (buffer-string)))
        (should (string-match-p (regexp-quote "chunk-A") contents))
        (should (string-match-p (regexp-quote "chunk-B") contents))))
    (should-not (agent-shell--tool-call-output-text agent-shell--state
                                                    "call-no-chunks"))))

(ert-deftest agent-shell--terminal-final-update-ignores-agent-content-test ()
  "Final tool_call_update ignores agent content for terminal tool calls."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call")
                                              (toolCallId . "call-final-ignore")
                                              (status . "in_progress")
                                              (title . "Terminal")
                                              (kind . "tool")
                                              (content . [((type . "terminal")
                                                           (terminalId . ,terminal-id))]))))))))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "terminal output")
    (with-current-buffer buffer
      (should (string-match-p "terminal output" (buffer-string))))
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call_update")
                                              (toolCallId . "call-final-ignore")
                                              (status . "completed")
                                              (content . [((type . "content")
                                                           (content . ((text . "agent summary"))))]))))))))
    (with-current-buffer buffer
      (should (string-match-p "terminal output" (buffer-string)))
      (should-not (string-match-p "agent summary" (buffer-string))))))

(ert-deftest agent-shell--terminal-final-update-preserves-streamed-output-test ()
  "Final tool_call_update preserves streamed output for terminal tool calls."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call")
                                              (toolCallId . "call-final-preserve")
                                              (status . "in_progress")
                                              (title . "Terminal")
                                              (kind . "tool")
                                              (content . [((type . "terminal")
                                                           (terminalId . ,terminal-id))]))))))))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "hello ")
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "world")
    (let* ((before (with-current-buffer buffer (buffer-string)))
           (before-pos (string-match (regexp-quote "hello world") before))
           (before-tail (and before-pos (substring before before-pos))))
      (should before-pos)
      (agent-shell--handle-tool-call-update-streaming
       agent-shell--state
       `((toolCallId . "call-final-preserve")
         (status . "completed")))
      (let* ((after (with-current-buffer buffer (buffer-string)))
             (after-pos (string-match (regexp-quote "hello world") after))
             (after-tail (and after-pos (substring after after-pos))))
        (should after-pos)
        (should (string= before-tail after-tail))))))

(ert-deftest agent-shell--terminal-output-persists-after-release-test ()
  "Terminal output remains visible after release."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (let ((responses nil))
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push args responses))))
        (with-current-buffer buffer
          (agent-shell--on-notification
           :state agent-shell--state
           :notification `((method . "session/update")
                           (params . ((update . ((sessionUpdate . "tool_call")
                                                  (toolCallId . "call-release")
                                                  (status . "in_progress")
                                                  (title . "Terminal")
                                                  (kind . "tool")
                                                  (content . [((type . "terminal")
                                                               (terminalId . ,terminal-id))]))))))))
        (agent-shell--terminal-handle-output
         agent-shell--state
         terminal-id
         "release output")
        (with-current-buffer buffer
          (should (string-match-p "release output" (buffer-string))))
        (agent-shell--on-terminal-release-request
         :state agent-shell--state
         :request `((id . "req-release")
                    (params . ((terminalId . ,terminal-id)))))
        (with-current-buffer buffer
          (should (string-match-p "release output" (buffer-string))))
        (let ((terminal (agent-shell--terminal-get agent-shell--state terminal-id)))
          (should (eq (map-elt terminal :released) t)))))))

(ert-deftest agent-shell--terminal-output-accumulates-test ()
  "Terminal output chunks accumulate in shell and output buffers."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call")
                                              (toolCallId . "call-accumulate")
                                              (status . "in_progress")
                                              (title . "Terminal")
                                              (kind . "tool")
                                              (content . [((type . "terminal")
                                                           (terminalId . ,terminal-id))]))))))))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "chunk-A")
    (with-current-buffer buffer
      (should (string-match-p "chunk-A" (buffer-string))))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "chunk-B")
    (with-current-buffer buffer
      (should (string-match-p "chunk-A\\(?:.\\|\\n\\)*chunk-B" (buffer-string))))
    (with-current-buffer output-buffer
      (should (equal (buffer-string) "chunk-Achunk-B")))))

(ert-deftest agent-shell--terminal-has-terminal-flag-persists-test ()
  "Persist terminal flag across updates without terminal content."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call")
                                              (toolCallId . "call-has-terminal")
                                              (status . "in_progress")
                                              (title . "Terminal")
                                              (kind . "tool")
                                              (content . [((type . "terminal")
                                                           (terminalId . ,terminal-id))]))))))))
    (with-current-buffer buffer
      (should (map-nested-elt agent-shell--state '(:tool-calls "call-has-terminal" :has-terminal))))
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call_update")
                                              (toolCallId . "call-has-terminal")
                                              (status . "completed"))))))))
    (with-current-buffer buffer
      (should (map-nested-elt agent-shell--state '(:tool-calls "call-has-terminal" :has-terminal))))))


(ert-deftest agent-shell--terminal-output-continues-after-final-update-test ()
  "Terminal output keeps streaming after a final tool_call_update."
  (agent-shell--with-terminal-test-fixture buffer output-buffer terminal-id agent-shell--state
    (with-current-buffer buffer
      (agent-shell--on-notification
       :state agent-shell--state
       :notification `((method . "session/update")
                       (params . ((update . ((sessionUpdate . "tool_call")
                                              (toolCallId . "call-marker")
                                              (status . "in_progress")
                                              (title . "Terminal")
                                              (kind . "tool")
                                              (content . [((type . "terminal")
                                                           (terminalId . ,terminal-id))]))))))))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "first " )
    (agent-shell--handle-tool-call-update-streaming
     agent-shell--state
     `((toolCallId . "call-marker")
       (status . "completed")))
    (agent-shell--terminal-handle-output
     agent-shell--state
     terminal-id
     "second")
    (with-current-buffer buffer
      (should (string-match-p "first\(?:.\|\n\)*second" (buffer-string))))))

(ert-deftest agent-shell--terminal-release-cleanup-after-inactivity-test ()
  "Released terminals are cleaned up after inactivity."
  (let* ((buffer (get-buffer-create " *agent-shell-terminal-cleanup*"))
         (terminal-old "term_old")
         (terminal-new "term_new")
         (output-buffer-old nil)
         (output-buffer-new nil)
         (timer-old nil)
         (timer-new nil)
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--terminal-release-grace-seconds 120))
    (map-put! agent-shell--state :client 'test-client)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (progn
          (setq output-buffer-old (agent-shell--terminal-make-output-buffer terminal-old))
          (agent-shell--terminal-put
           agent-shell--state
           terminal-old
           (agent-shell--test-make-terminal
            terminal-old output-buffer-old (cons :released t)))
          (agent-shell--terminal-schedule-cleanup agent-shell--state terminal-old)
          (setq timer-old (map-elt (agent-shell--terminal-get agent-shell--state terminal-old)
                                   :cleanup-timer))
          (let ((terminal (agent-shell--terminal-get agent-shell--state terminal-old)))
            (setf (map-elt terminal :last-access) (- (float-time) 200))
            (agent-shell--terminal-put agent-shell--state terminal-old terminal))
          (should (timerp timer-old))
          (apply (timer--function timer-old) (timer--args timer-old))
          (should-not (agent-shell--terminal-get agent-shell--state terminal-old))

          (setq output-buffer-new (agent-shell--terminal-make-output-buffer terminal-new))
          (agent-shell--terminal-put
           agent-shell--state
           terminal-new
           (agent-shell--test-make-terminal
            terminal-new output-buffer-new (cons :released t)))
          (agent-shell--terminal-schedule-cleanup agent-shell--state terminal-new)
          (setq timer-new (map-elt (agent-shell--terminal-get agent-shell--state terminal-new)
                                   :cleanup-timer))
          (let ((terminal (agent-shell--terminal-get agent-shell--state terminal-new)))
            (setf (map-elt terminal :last-access) (float-time))
            (agent-shell--terminal-put agent-shell--state terminal-new terminal))
          (should (timerp timer-new))
          (apply (timer--function timer-new) (timer--args timer-new))
          (should (agent-shell--terminal-get agent-shell--state terminal-new)))
      (when (timerp timer-old)
        (cancel-timer timer-old))
      (when (timerp timer-new)
        (cancel-timer timer-new))
      (when (agent-shell--terminal-get agent-shell--state terminal-old)
        (agent-shell--terminal-remove agent-shell--state terminal-old))
      (when (agent-shell--terminal-get agent-shell--state terminal-new)
        (agent-shell--terminal-remove agent-shell--state terminal-new))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p output-buffer-old)
        (kill-buffer output-buffer-old))
      (when (buffer-live-p output-buffer-new)
        (kill-buffer output-buffer-new)))))

(ert-deftest agent-shell--terminal-create-output-test ()
  "Create a terminal and read truncated output."
  (let* ((buffer (get-buffer-create " *agent-shell-terminal-test*"))
         (temp-dir (make-temp-file "agent-shell-terminal" t))
         (responses nil)
         (terminal-id nil)
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
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
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
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
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
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
