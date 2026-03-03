;;; agent-shell-usage-tests.el --- Tests for usage tracking -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(defmacro agent-shell-usage-tests--with-stub (&rest body)
  "Evaluate BODY with `agent-shell--state' stubbed to return the variable."
  (declare (indent 0) (debug body))
  `(cl-letf (((symbol-function 'agent-shell--state)
              (lambda () agent-shell--state)))
     ,@body))

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
  "Notification with only used (no size) preserves previously-stored size."
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
    (agent-shell-usage-tests--with-stub
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should indicator)
        (should (equal 'success (get-text-property 0 'face indicator)))))))

(ert-deftest agent-shell-usage--indicator-medium-usage-warning ()
  "Medium usage (60-84%) shows warning."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 140000 200000)))
    (agent-shell-usage-tests--with-stub
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should indicator)
        (should (equal 'warning (get-text-property 0 'face indicator)))))))

(ert-deftest agent-shell-usage--indicator-high-usage-error ()
  "High usage (>= 85%) shows error/red."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 180000 200000)))
    (agent-shell-usage-tests--with-stub
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should indicator)
        (should (equal 'error (get-text-property 0 'face indicator)))))))

(ert-deftest agent-shell-usage--indicator-full-usage ()
  "used == size shows full block with error face."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 200000 200000)))
    (agent-shell-usage-tests--with-stub
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should (equal "█" (substring-no-properties indicator)))
        (should (equal 'error (get-text-property 0 'face indicator)))))))

(ert-deftest agent-shell-usage--indicator-overflow-shows-question-mark ()
  "used > size shows ? with warning face, not a block character."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 419574 200000)))
    (agent-shell-usage-tests--with-stub
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should (equal "?" (substring-no-properties indicator)))
        (should (equal 'warning (get-text-property 0 'face indicator)))))))

(ert-deftest agent-shell-usage--indicator-resets-after-compaction ()
  "Indicator reflects the lower usage after compaction."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 965200 1000000)))
    (agent-shell-usage-tests--with-stub
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
        (should (equal "▁" (substring-no-properties indicator)))))))

(ert-deftest agent-shell-usage--indicator-block-characters-scale ()
  "Block characters scale with usage percentage."
  (let ((agent-shell-show-context-usage-indicator t))
    (agent-shell-usage-tests--with-stub
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
        (should (equal "█" (substring-no-properties (agent-shell--context-usage-indicator))))))))

(ert-deftest agent-shell-usage--indicator-nil-when-disabled ()
  "Return nil when the indicator is disabled."
  (let ((agent-shell-show-context-usage-indicator nil)
        (agent-shell--state (agent-shell-usage-tests--make-state 500000 1000000)))
    (agent-shell-usage-tests--with-stub
      (should-not (agent-shell--context-usage-indicator)))))

(ert-deftest agent-shell-usage--indicator-nil-when-no-data ()
  "Return nil when context-size is 0."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 0 0)))
    (agent-shell-usage-tests--with-stub
      (should-not (agent-shell--context-usage-indicator)))))

(ert-deftest agent-shell-usage--indicator-nil-when-zero-usage ()
  "Return nil when context-used is 0."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 0 1000000)))
    (agent-shell-usage-tests--with-stub
      (should-not (agent-shell--context-usage-indicator)))))

;; ============================================================
;; agent-shell--format-usage: overflow handling
;; ============================================================

(ert-deftest agent-shell-usage--format-usage-normal-percentage ()
  "Format shows percentage when used <= size."
  (let ((usage (map-elt (agent-shell-usage-tests--make-state 50000 200000) :usage)))
    (let ((formatted (agent-shell--format-usage usage)))
      (should (string-match-p "(25.0%)" formatted))
      (should-not (string-match-p "(\\?)" formatted)))))

(ert-deftest agent-shell-usage--format-usage-overflow-shows-unreliable ()
  "Format shows (?) instead of percentage when used > size."
  (let ((usage (map-elt (agent-shell-usage-tests--make-state 419574 200000) :usage)))
    (let ((formatted (agent-shell--format-usage usage)))
      (should (string-match-p "420k/200k" formatted))
      (should (string-match-p "(\\?)" formatted))
      (should-not (string-match-p "209" formatted)))))

(ert-deftest agent-shell-usage--format-usage-exact-full ()
  "Format shows 100.0% when used == size."
  (let ((usage (map-elt (agent-shell-usage-tests--make-state 200000 200000) :usage)))
    (let ((formatted (agent-shell--format-usage usage)))
      (should (string-match-p "(100.0%)" formatted))
      (should-not (string-match-p "(\\?)" formatted)))))

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
    (agent-shell-usage-tests--with-stub
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should (equal 'success (get-text-property 0 'face indicator)))
        (should (equal "▂" (substring-no-properties indicator)))))))

;; ============================================================
;; Regression: model-switch ACP traffic replay
;; ============================================================

;; This test replays real observed ACP traffic where a model switch from
;; Opus 1M to Sonnet 200k caused the server to report used > size.
;; The server takes Math.min across all models for `size`, so after the
;; switch size dropped from 1000000 to 200000 while used kept growing.
;; This is the regression test that would have caught this bug originally.
(ert-deftest agent-shell-usage--model-switch-overflow-replay ()
  "Replay model-switch traffic: size drops, used exceeds it, indicator shows ?."
  (let ((agent-shell-show-context-usage-indicator t)
        (agent-shell--state (agent-shell-usage-tests--make-state 0 0))
        ;; Real observed ACP traffic from Opus 1M -> Sonnet 200k switch
        (traffic '(;; On Opus 1M — normal
                   (32449 . 1000000)
                   ;; Switched to Sonnet — size drops to 200k
                   (60978 . 200000)
                   (122601 . 200000)
                   (209712 . 200000)
                   ;; used now exceeds size — server bug
                   (419574 . 200000))))
    (agent-shell-usage-tests--with-stub
      ;; First update: normal, on Opus 1M
      (agent-shell--update-usage-from-notification
       :state agent-shell--state
       :acp-update (list (cons 'used (caar traffic))
                         (cons 'size (cdar traffic))))
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should (equal "▁" (substring-no-properties indicator)))
        (should (equal 'success (get-text-property 0 'face indicator))))
      ;; Replay remaining updates
      (dolist (pair (cdr traffic))
        (agent-shell--update-usage-from-notification
         :state agent-shell--state
         :acp-update (list (cons 'used (car pair))
                           (cons 'size (cdr pair)))))
      ;; Final state: used=419574 > size=200000
      (should (equal 419574 (map-elt (map-elt agent-shell--state :usage) :context-used)))
      (should (equal 200000 (map-elt (map-elt agent-shell--state :usage) :context-size)))
      ;; Indicator: ? with warning face (not a block character)
      (let ((indicator (agent-shell--context-usage-indicator)))
        (should (equal "?" (substring-no-properties indicator)))
        (should (equal 'warning (get-text-property 0 'face indicator)))))))

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
