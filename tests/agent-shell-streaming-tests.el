;;; agent-shell-streaming-tests.el --- Tests for streaming/dedup -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
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

(ert-deftest agent-shell--tool-call-normalize-output-trailing-newline-test ()
  "Normalized output should always end with a newline."
  (should (string-suffix-p "\n" (agent-shell--tool-call-normalize-output "hello")))
  (should (string-suffix-p "\n" (agent-shell--tool-call-normalize-output "hello\n")))
  (should (equal (agent-shell--tool-call-normalize-output "") ""))
  (should (equal (agent-shell--tool-call-normalize-output nil) nil)))

(ert-deftest agent-shell--tool-call-normalize-output-persisted-output-test ()
  "Persisted-output tags should be stripped and content fontified."
  (let ((result (agent-shell--tool-call-normalize-output
                 "<persisted-output>\nOutput saved to: /tmp/foo.txt\n\nPreview:\nline 0\n</persisted-output>")))
    ;; Tags stripped
    (should-not (string-match-p "<persisted-output>" result))
    (should-not (string-match-p "</persisted-output>" result))
    ;; Content preserved
    (should (string-match-p "Output saved to" result))
    (should (string-match-p "line 0" result))
    ;; Fontified as comment
    (should (eq (get-text-property 1 'font-lock-face result) 'font-lock-comment-face))))

(ert-deftest agent-shell--tool-call-update-writes-output-test ()
  "Tool call updates should write output to the shell buffer."
  (let* ((buffer (get-buffer-create " *agent-shell-tool-call-output*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (lambda (&rest _args) nil)))
          (with-current-buffer buffer
            (agent-shell--on-notification
             :state agent-shell--state
             :notification `((method . "session/update")
                             (params . ((update . ((sessionUpdate . "tool_call_update")
                                                   (toolCallId . "call-1")
                                                   (status . "completed")
                                                   (content . [((content . ((text . "stream chunk"))))]))))))))
          (with-current-buffer buffer
            (should (string-match-p "stream chunk" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--tool-call-meta-response-stdout-no-duplication-test ()
  "Meta toolResponse.stdout must not produce duplicate output.
Simplified replay without terminal notifications: sends tool_call
\(pending), tool_call_update with _meta stdout, then tool_call_update
\(completed).  A distinctive line must appear exactly once."
  (let* ((buffer (get-buffer-create " *agent-shell-dedup-test*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (tool-id "toolu_replay_dedup")
         (stdout-text "line 0\nline 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9"))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--make-diff-info)
                   (lambda (&rest _args) nil)))
          (with-current-buffer buffer
            ;; Notification 1: tool_call (pending)
            (agent-shell--on-notification
             :state agent-shell--state
             :notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call")
                                            (rawInput)
                                            (status . "pending")
                                            (title . "Bash")
                                            (kind . "execute")))))))
            ;; Notification 2: tool_call_update with toolResponse.stdout
            (agent-shell--on-notification
             :state agent-shell--state
             :notification `((method . "session/update")
                             (params . ((update
                                         . ((_meta (claudeCode (toolResponse (stdout . ,stdout-text)
                                                                             (stderr . "")
                                                                             (interrupted)
                                                                             (isImage)
                                                                             (noOutputExpected))
                                                               (toolName . "Bash")))
                                            (toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")))))))
            ;; Notification 3: tool_call_update completed
            (agent-shell--on-notification
             :state agent-shell--state
             :notification `((method . "session/update")
                             (params . ((update
                                         . ((toolCallId . ,tool-id)
                                            (sessionUpdate . "tool_call_update")
                                            (status . "completed")))))))))
          (with-current-buffer buffer
            (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                   (count-line5 (let ((c 0) (s 0))
                                  (while (string-match "line 5" buf-text s)
                                    (setq c (1+ c) s (match-end 0)))
                                  c)))
              ;; "line 9" must be present (output was rendered)
              (should (string-match-p "line 9" buf-text))
              ;; "line 5" must appear exactly once (no duplication)
              (should (= count-line5 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--initialize-request-includes-terminal-output-meta-test ()
  "Initialize request should include terminal_output meta capability.
Without this, agents like claude-agent-acp will not send
toolResponse.stdout streaming updates."
  (let* ((buffer (get-buffer-create " *agent-shell-init-request*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer)))
    (map-put! agent-shell--state :client 'test-client)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode)
      (setq-local agent-shell--state agent-shell--state))
    (unwind-protect
        (let ((captured-request nil))
          (cl-letf (((symbol-function 'acp-send-request)
                     (lambda (&rest args)
                       (setq captured-request (plist-get args :request)))))
            (agent-shell--initiate-handshake
             :shell-buffer buffer
             :on-initiated (lambda () nil)))
          (should (eq t (map-nested-elt captured-request
                                        '(:params clientCapabilities _meta terminal_output)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest agent-shell--codex-terminal-output-streams-without-duplication-test ()
"Codex-acp streams via terminal_output.data; output must not duplicate.
Replays the codex notification pattern: tool_call with terminal content,
incremental terminal_output.data chunks, then completed update."
(let* ((buffer (get-buffer-create " *agent-shell-codex-dedup*"))
(agent-shell--state (agent-shell--make-state :buffer buffer))
(tool-id "call_codex_test"))
(map-put! agent-shell--state :client 'test-client)
(map-put! agent-shell--state :request-count 1)
(with-current-buffer buffer
(erase-buffer)
(agent-shell-mode))
(unwind-protect
(cl-letf (((symbol-function 'agent-shell--make-diff-info)
(lambda (&rest _args) nil)))
(with-current-buffer buffer
;; Notification 1: tool_call (in_progress, terminal content)
(agent-shell--on-notification
:state agent-shell--state
:notification `((method . "session/update")
(params . ((update
. ((sessionUpdate . "tool_call")
(toolCallId . ,tool-id)
(title . "Run echo test")
(kind . "execute")
(status . "in_progress")
(content . [((type . "terminal")
(terminalId . ,tool-id))])
(_meta (terminal_info
(terminal_id . ,tool-id)))))))))
;; Notification 2: first terminal_output.data chunk
(agent-shell--on-notification
:state agent-shell--state
:notification `((method . "session/update")
(params . ((update
. ((sessionUpdate . "tool_call_update")
(toolCallId . ,tool-id)
(_meta (terminal_output
(terminal_id . ,tool-id)
(data . "alpha\n")))))))))
;; Notification 3: second terminal_output.data chunk
(agent-shell--on-notification
:state agent-shell--state
:notification `((method . "session/update")
(params . ((update
. ((sessionUpdate . "tool_call_update")
(toolCallId . ,tool-id)
(_meta (terminal_output
(terminal_id . ,tool-id)
(data . "bravo\n")))))))))
;; Notification 4: completed
(agent-shell--on-notification
:state agent-shell--state
:notification `((method . "session/update")
(params . ((update
. ((sessionUpdate . "tool_call_update")
(toolCallId . ,tool-id)
(status . "completed")
(_meta (terminal_exit
(terminal_id . ,tool-id)
(exit_code . 0)))))))))))
(with-current-buffer buffer
(let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
(count-alpha (let ((c 0) (s 0))
(while (string-match "alpha" buf-text s)
(setq c (1+ c) s (match-end 0)))
c)))
;; Both chunks rendered
(should (string-match-p "alpha" buf-text))
(should (string-match-p "bravo" buf-text))
;; No duplication
(should (= count-alpha 1))))
(when (buffer-live-p buffer)
  (kill-buffer buffer)))))


;;; Thought chunk dedup tests

(ert-deftest agent-shell--thought-chunk-delta-incremental-test ()
  "Incremental tokens with no prefix overlap pass through unchanged."
  (should (equal (agent-shell--thought-chunk-delta "AB" "CD") "CD"))
  (should (equal (agent-shell--thought-chunk-delta nil "hello") "hello"))
  (should (equal (agent-shell--thought-chunk-delta "" "hello") "hello")))

(ert-deftest agent-shell--thought-chunk-delta-cumulative-test ()
  "Cumulative re-delivery returns only the new tail."
  (should (equal (agent-shell--thought-chunk-delta "AB" "ABCD") "CD"))
  (should (equal (agent-shell--thought-chunk-delta "hello " "hello world") "world")))

(ert-deftest agent-shell--thought-chunk-delta-exact-duplicate-test ()
  "Exact duplicate returns empty string."
  (should (equal (agent-shell--thought-chunk-delta "ABCD" "ABCD") "")))

(ert-deftest agent-shell--thought-chunk-delta-suffix-test ()
  "Chunk already present as suffix of accumulated returns empty string.
This handles the case where leading whitespace tokens were streamed
incrementally but the re-delivery omits them."
  (should (equal (agent-shell--thought-chunk-delta "\n\nABCD" "ABCD") ""))
  (should (equal (agent-shell--thought-chunk-delta "\n\n**bold**" "**bold**") "")))

(ert-deftest agent-shell--thought-chunk-no-duplication-test ()
  "Thought chunks must not produce duplicate output in the buffer.
Replays the codex doubling pattern: incremental tokens followed by
a cumulative re-delivery of the complete thought text."
  (let* ((buffer (get-buffer-create " *agent-shell-thought-dedup*"))
         (agent-shell--state (agent-shell--make-state :buffer buffer))
         (agent-shell--transcript-file nil)
         (thought-text "**Checking beads**\n\nLooking for .beads directory."))
    (map-put! agent-shell--state :client 'test-client)
    (map-put! agent-shell--state :request-count 1)
    (with-current-buffer buffer
      (erase-buffer)
      (agent-shell-mode))
    (unwind-protect
        (with-current-buffer buffer
          ;; Send incremental tokens
          (dolist (token (list "\n\n" "**Checking" " beads**" "\n\n"
                               "Looking" " for" " .beads" " directory."))
            (agent-shell--on-notification
             :state agent-shell--state
             :notification `((method . "session/update")
                             (params . ((update
                                         . ((sessionUpdate . "agent_thought_chunk")
                                            (content (type . "text")
                                                     (text . ,token)))))))))
          ;; Cumulative re-delivery of the complete text
          (agent-shell--on-notification
           :state agent-shell--state
           :notification `((method . "session/update")
                           (params . ((update
                                       . ((sessionUpdate . "agent_thought_chunk")
                                          (content (type . "text")
                                                   (text . ,thought-text))))))))
          (let* ((buf-text (buffer-substring-no-properties (point-min) (point-max)))
                 (count (let ((c 0) (s 0))
                          (while (string-match "Checking beads" buf-text s)
                            (setq c (1+ c) s (match-end 0)))
                          c)))
            ;; Content must be present
            (should (string-match-p "Checking beads" buf-text))
            ;; Must appear exactly once (no duplication)
            (should (= count 1))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'agent-shell-streaming-tests)
;;; agent-shell-streaming-tests.el ends here
