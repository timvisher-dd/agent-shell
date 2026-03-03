;;; agent-shell-usage-tests.el --- Tests for usage tracking -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)

;; Load agent-shell-usage without pulling in the full agent-shell dependency tree.
;; Provide the declarations it needs.
(defvar agent-shell--state nil)
(defvar agent-shell-mode nil)
(require 'agent-shell-usage)

;;; Code:

(defun agent-shell-usage-tests--make-state (context-used context-size)
  "Create minimal usage state with CONTEXT-USED and CONTEXT-SIZE."
  (list (cons :usage
              (list (cons :total-tokens 0)
                    (cons :input-tokens 0)
                    (cons :output-tokens 0)
                    (cons :thought-tokens 0)
                    (cons :cached-read-tokens 0)
                    (cons :cached-write-tokens 0)
                    (cons :context-used context-used)
                    (cons :context-size context-size)
                    (cons :cost-amount 0.0)
                    (cons :cost-currency nil)))))

;; Stub the state accessor to return the buffer-local variable directly.
(defun agent-shell--state ()
  "Test stub: return the state variable without mode checks."
  agent-shell--state)

;; ============================================================
;; agent-shell--update-usage-from-notification
;; ============================================================

(ert-deftest agent-shell-usage--update-sets-used-and-size ()
  "Notification with used/size updates state."
  (let ((state (agent-shell-usage-tests--make-state 0 0)))
    (agent-shell--update-usage-from-notification
     :state state
     :acp-update '((used . 50000) (size . 200000)))
    (should (equal 50000 (map-elt (map-elt state :usage) :context-used)))
    (should (equal 200000 (map-elt (map-elt state :usage) :context-size)))))

(ert-deftest agent-shell-usage--compaction-resets-used ()
  "After compaction, a lower used value replaces the prior peak."
  (let ((state (agent-shell-usage-tests--make-state 0 0)))
    (agent-shell--update-usage-from-notification
     :state state
     :acp-update '((used . 965200) (size . 1000000)))
    (should (equal 965200 (map-elt (map-elt state :usage) :context-used)))
    ;; Compaction
    (agent-shell--update-usage-from-notification
     :state state
     :acp-update '((used . 24095) (size . 1000000)))
    (should (equal 24095 (map-elt (map-elt state :usage) :context-used)))
    (should (equal 1000000 (map-elt (map-elt state :usage) :context-size)))))

(ert-deftest agent-shell-usage--update-cost-fields ()
  "Cost amount and currency are extracted from the notification."
  (let ((state (agent-shell-usage-tests--make-state 0 0)))
    (agent-shell--update-usage-from-notification
     :state state
     :acp-update '((used . 10000)
                   (size . 200000)
                   (cost . ((amount . 0.42) (currency . "USD")))))
    (should (equal 0.42 (map-elt (map-elt state :usage) :cost-amount)))
    (should (equal "USD" (map-elt (map-elt state :usage) :cost-currency)))))

(ert-deftest agent-shell-usage--update-partial-fields ()
  "Notification with only used (no size) updates only that field."
  (let ((state (agent-shell-usage-tests--make-state 0 0)))
    (agent-shell--update-usage-from-notification
     :state state
     :acp-update '((used . 50000) (size . 200000)))
    (agent-shell--update-usage-from-notification
     :state state
     :acp-update '((used . 60000)))
    (should (equal 60000 (map-elt (map-elt state :usage) :context-used)))
    (should (equal 200000 (map-elt (map-elt state :usage) :context-size)))))

;; ============================================================
;; agent-shell--context-usage-indicator
;; ============================================================

(ert-deftest agent-shell-usage--indicator-low-usage-green ()
  "Low usage (< 60%) shows green."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 50000 200000)))
    (let ((indicator (agent-shell--context-usage-indicator)))
      (should indicator)
      (should (equal 'success (get-text-property 0 'face indicator))))))

(ert-deftest agent-shell-usage--indicator-medium-usage-warning ()
  "Medium usage (60-84%) shows warning."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 140000 200000)))
    (let ((indicator (agent-shell--context-usage-indicator)))
      (should indicator)
      (should (equal 'warning (get-text-property 0 'face indicator))))))

(ert-deftest agent-shell-usage--indicator-high-usage-error ()
  "High usage (>= 85%) shows error/red."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 180000 200000)))
    (let ((indicator (agent-shell--context-usage-indicator)))
      (should indicator)
      (should (equal 'error (get-text-property 0 'face indicator))))))

(ert-deftest agent-shell-usage--indicator-resets-after-compaction ()
  "Indicator reflects the lower usage after compaction."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 965200 1000000)))
    ;; Pre-compaction: red
    (should (equal 'error
                   (get-text-property 0 'face (agent-shell--context-usage-indicator))))
    ;; Compaction
    (agent-shell--update-usage-from-notification
     :state agent-shell--state
     :acp-update '((used . 24095) (size . 1000000)))
    ;; Post-compaction: green, smallest block
    (let ((indicator (agent-shell--context-usage-indicator)))
      (should (equal 'success (get-text-property 0 'face indicator)))
      (should (equal "▁" (substring-no-properties indicator))))))

(ert-deftest agent-shell-usage--indicator-block-characters-scale ()
  "Block characters scale with usage percentage."
  (let ((agent-shell-show-context-usage-indicator t))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 100000 1000000)))
      (should (equal "▁" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 300000 1000000)))
      (should (equal "▂" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 400000 1000000)))
      (should (equal "▃" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 550000 1000000)))
      (should (equal "▄" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 650000 1000000)))
      (should (equal "▅" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 800000 1000000)))
      (should (equal "▆" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 900000 1000000)))
      (should (equal "▇" (substring-no-properties (agent-shell--context-usage-indicator)))))
    (let ((agent-shell--state (agent-shell-usage-tests--make-state 1000000 1000000)))
      (should (equal "█" (substring-no-properties (agent-shell--context-usage-indicator)))))))

(ert-deftest agent-shell-usage--indicator-nil-when-disabled ()
  "Returns nil when the indicator is disabled."
  (let ((agent-shell-show-context-usage-indicator nil)
        (agent-shell--state (agent-shell-usage-tests--make-state 500000 1000000)))
    (should-not (agent-shell--context-usage-indicator))))

(ert-deftest agent-shell-usage--indicator-nil-when-no-data ()
  "Returns nil when context-size is 0."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 0 0)))
    (should-not (agent-shell--context-usage-indicator))))

(ert-deftest agent-shell-usage--indicator-nil-when-zero-usage ()
  "Returns nil when context-used is 0."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 0 1000000)))
    (should-not (agent-shell--context-usage-indicator))))

;; ============================================================
;; Full compaction replay from observed ACP traffic
;; ============================================================

(ert-deftest agent-shell-usage--compaction-replay ()
  "Replay observed traffic: linear fill -> compaction -> refill."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 0 0))
        (traffic '((48724 . 1000000)
                   (259218 . 1000000)
                   (494277 . 1000000)
                   (729572 . 1000000)
                   (870846 . 1000000)
                   (965200 . 1000000)   ; pre-compaction peak
                   (24095 . 1000000)    ; post-compaction drop
                   (74111 . 1000000)    ; refilling
                   (262548 . 1000000))))
    (dolist (pair traffic)
      (agent-shell--update-usage-from-notification
       :state agent-shell--state
       :acp-update (list (cons 'used (car pair))
                         (cons 'size (cdr pair)))))
    ;; Final state reflects last update
    (should (equal 262548 (map-elt (map-elt agent-shell--state :usage) :context-used)))
    (should (equal 1000000 (map-elt (map-elt agent-shell--state :usage) :context-size)))
    ;; Indicator: green, ▂ for 26.3%
    (let ((indicator (agent-shell--context-usage-indicator)))
      (should (equal 'success (get-text-property 0 'face indicator)))
      (should (equal "▂" (substring-no-properties indicator))))))

;; ============================================================
;; agent-shell--save-usage (PromptResponse tokens)
;; ============================================================

(ert-deftest agent-shell-usage--save-usage-token-counts ()
  "PromptResponse usage updates token counts."
  (let ((state (agent-shell-usage-tests--make-state 0 0)))
    (agent-shell--save-usage
     :state state
     :acp-usage '((totalTokens . 5000)
                  (inputTokens . 3000)
                  (outputTokens . 2000)
                  (thoughtTokens . 500)
                  (cachedReadTokens . 1000)
                  (cachedWriteTokens . 200)))
    (should (equal 5000 (map-elt (map-elt state :usage) :total-tokens)))
    (should (equal 3000 (map-elt (map-elt state :usage) :input-tokens)))
    (should (equal 2000 (map-elt (map-elt state :usage) :output-tokens)))
    (should (equal 500 (map-elt (map-elt state :usage) :thought-tokens)))
    (should (equal 1000 (map-elt (map-elt state :usage) :cached-read-tokens)))
    (should (equal 200 (map-elt (map-elt state :usage) :cached-write-tokens)))))

;; ============================================================
;; agent-shell--format-number-compact
;; ============================================================

(ert-deftest agent-shell-usage--format-number-compact ()
  "Number formatting uses k/m/b suffixes."
  (should (equal "42" (agent-shell--format-number-compact 42)))
  (should (equal "1k" (agent-shell--format-number-compact 1000)))
  (should (equal "24k" (agent-shell--format-number-compact 24095)))
  (should (equal "965k" (agent-shell--format-number-compact 965200)))
  (should (equal "1m" (agent-shell--format-number-compact 1000000)))
  (should (equal "2b" (agent-shell--format-number-compact 2000000000))))

(provide 'agent-shell-usage-tests)
;;; agent-shell-usage-tests.el ends here
