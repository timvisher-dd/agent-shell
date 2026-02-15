;;; agent-shell-point-tests.el --- Point preservation tests -*- lexical-binding: t; -*-

;; These tests verify that point stays where the user put it during
;; async fragment updates and tool call output streaming.  When point
;; is at the end of buffer it should follow new output; when point is
;; somewhere in the middle it should stay put.

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-ui)
(require 'acp-fakes)

;;; Helpers

(defvar agent-shell-point-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun agent-shell-point-test--make-shell ()
  "Create a minimal fake agent-shell buffer for testing.
Returns the buffer."
  (let* ((messages (acp-traffic-read-file
                    (expand-file-name "gemini-wrong-output-grouping.traffic"
                                      agent-shell-point-test--dir)))
         (client (acp-fakes-make-client messages))
         (config (agent-shell-make-agent-config
                  :identifier 'point-test
                  :mode-line-name "PointTest"
                  :welcome-function (lambda (_config) "")
                  :buffer-name "PointTest"
                  :shell-prompt "PointTest> "
                  :shell-prompt-regexp "PointTest> "
                  :client-maker (lambda (&optional _buffer) client)))
         (buffer (agent-shell--start :config config :no-focus t :new-session t)))
    (with-current-buffer buffer
      (map-put! agent-shell--state :client client)
      (map-put! agent-shell--state :request-count 1))
    buffer))

(defmacro agent-shell-point-test--with-shell (&rest body)
  "Execute BODY with a temporary agent-shell buffer as current.
The buffer is killed after BODY completes."
  (declare (indent 0) (debug t))
  `(let ((buf (agent-shell-point-test--make-shell)))
     (unwind-protect
         (with-current-buffer buf
           ,@body)
       (when (buffer-live-p buf)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer buf))))))

;;; Tests: agent-shell--update-fragment point preservation

(ert-deftest agent-shell-point-at-eob-follows-new-fragment ()
  "When point is at end of buffer, it should follow after a new fragment."
  (agent-shell-point-test--with-shell
    (goto-char (point-max))
    (let ((old-max (point-max)))
      (agent-shell--update-fragment
       :state agent-shell--state
       :block-id "test-block"
       :label-left "Status"
       :body "Some tool output text here")
      (should (= (point) (point-max)))
      (should (< old-max (point-max))))))

(ert-deftest agent-shell-point-in-middle-stays-after-new-fragment ()
  "When point is in the middle of buffer, it should stay after a new fragment."
  (agent-shell-point-test--with-shell
    ;; First insert some content so there's a middle to be in
    (agent-shell--update-fragment
     :state agent-shell--state
     :block-id "setup-block"
     :label-left "Setup"
     :body "Initial content for the buffer")
    ;; Now place point somewhere in the middle
    (goto-char (point-min))
    (forward-line 1)
    (let ((saved-pos (point)))
      (agent-shell--update-fragment
       :state agent-shell--state
       :block-id "new-block"
       :label-left "New"
       :body "More content appended"
       :create-new t)
      ;; Point should be at same position (marker may shift if insert was before it)
      ;; but should NOT be at point-max
      (should-not (= (point) (point-max))))))

(ert-deftest agent-shell-point-at-eob-follows-fragment-update ()
  "When point is at eob, it should follow after an existing fragment is updated."
  (agent-shell-point-test--with-shell
    ;; Create initial fragment
    (agent-shell--update-fragment
     :state agent-shell--state
     :block-id "streaming-block"
     :label-left "Tool"
     :body "line 1")
    (goto-char (point-max))
    ;; Append to it
    (agent-shell--update-fragment
     :state agent-shell--state
     :block-id "streaming-block"
     :body "line 2"
     :append t)
    (should (= (point) (point-max)))))

(ert-deftest agent-shell-point-at-eob-follows-multiple-fragments ()
  "Point at eob should follow through a sequence of fragment updates."
  (agent-shell-point-test--with-shell
    (goto-char (point-max))
    (dotimes (i 5)
      (agent-shell--update-fragment
       :state agent-shell--state
       :block-id (format "block-%d" i)
       :label-left (format "Step %d" i)
       :body (format "Content for step %d" i)
       :create-new t)
      (should (= (point) (point-max))))))

;;; Tests: agent-shell--append-tool-call-output point preservation

(ert-deftest agent-shell-point-at-eob-follows-tool-output ()
  "When point is at eob, it should follow after tool call output is appended."
  (agent-shell-point-test--with-shell
    ;; Set up a tool call fragment first
    (agent-shell--update-fragment
     :state agent-shell--state
     :block-id "tool-1"
     :label-left "Running"
     :body "")
    (goto-char (point-max))
    ;; Append output
    (agent-shell--append-tool-call-output
     agent-shell--state "tool-1" "first chunk of output\n")
    (should (= (point) (point-max)))
    ;; Append more
    (agent-shell--append-tool-call-output
     agent-shell--state "tool-1" "second chunk of output\n")
    (should (= (point) (point-max)))))

(ert-deftest agent-shell-point-in-middle-stays-after-tool-output ()
  "When point is in the middle, it should stay after tool call output."
  (agent-shell-point-test--with-shell
    ;; Set up content and a tool call
    (agent-shell--update-fragment
     :state agent-shell--state
     :block-id "tool-2"
     :label-left "Running"
     :body "initial")
    ;; Place point at beginning
    (goto-char (point-min))
    (let ((saved-pos (point)))
      (agent-shell--append-tool-call-output
       agent-shell--state "tool-2" "output text\n")
      (should (= (point) saved-pos))
      (should-not (= (point) (point-max))))))

(provide 'agent-shell-point-tests)
;;; agent-shell-point-tests.el ends here
