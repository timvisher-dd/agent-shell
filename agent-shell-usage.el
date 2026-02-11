;;; agent-shell-usage.el --- Session usage tracking utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;
;; Provides session usage tracking functionality for agent-shell.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)

(defvar agent-shell--state)
(defvar agent-shell-mode)
(declare-function agent-shell--state "agent-shell")

(defcustom agent-shell-show-usage-at-turn-end nil
  "Whether to display usage information when agent turn ends.

When non-nil, displays a formatted box showing token counts,
context window usage, and cost information after each agent response."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-show-context-usage-indicator t
  "Non-nil to show the context usage indicator in the header and mode line.

The indicator displays context window usage as a vertical bar character,
color-coded from green (low) to yellow (high) to red (critical).
Only appears when the ACP server provides usage information."
  :type 'boolean
  :group 'agent-shell)

(cl-defun agent-shell--save-usage (&key state acp-usage)
  "Update usage STATE from PromptResponse ACP-USAGE field.
Extracts cumulative token counts from the response."
  (let ((usage-state (map-elt state :usage)))
    (when-let ((total (map-elt acp-usage 'totalTokens)))
      (map-put! usage-state :total-tokens total))
    (when-let ((input (map-elt acp-usage 'inputTokens)))
      (map-put! usage-state :input-tokens input))
    (when-let ((output (map-elt acp-usage 'outputTokens)))
      (map-put! usage-state :output-tokens output))
    (when-let ((thought (map-elt acp-usage 'thoughtTokens)))
      (map-put! usage-state :thought-tokens thought))
    (when-let ((cached-read (map-elt acp-usage 'cachedReadTokens)))
      (map-put! usage-state :cached-read-tokens cached-read))
    (when-let ((cached-write (map-elt acp-usage 'cachedWriteTokens)))
      (map-put! usage-state :cached-write-tokens cached-write))
    (map-put! state :usage usage-state)))

(cl-defun agent-shell--update-usage-from-notification (&key state update)
  "Update usage STATE from session/update notification UPDATE.
Extracts context window and cost information from usage_update notification."
  (let ((usage-state (map-elt state :usage)))
    (when-let ((used (map-elt update 'used)))
      (map-put! usage-state :context-used used))
    (when-let ((size (map-elt update 'size)))
      (map-put! usage-state :context-size size))
    (when-let ((cost (map-elt update 'cost)))
      (when-let ((amount (map-elt cost 'amount)))
        (map-put! usage-state :cost-amount amount))
      (when-let ((currency (map-elt cost 'currency)))
        (map-put! usage-state :cost-currency currency)))
    (map-put! state :usage usage-state)))

(defun agent-shell--format-number-compact (num)
  "Format NUM compactly with k/m/b suffixes."
  (cond
   ((>= num 1000000000) (format "%.0fb" (/ num 1000000000.0)))
   ((>= num 1000000) (format "%.0fm" (/ num 1000000.0)))
   ((>= num 1000) (format "%.0fk" (/ num 1000.0)))
   (t (format "%d" num))))

(defun agent-shell--usage-has-data-p (usage)
  "Return non-nil if USAGE contains meaningful data.
Checks if any token counts or context size are non-zero."
  (or (> (or (map-elt usage :total-tokens) 0) 0)
      (> (or (map-elt usage :context-size) 0) 0)))

(defun agent-shell--format-usage-box (usage)
  "Format USAGE information as a box display.
Returns a string with box-drawing characters showing tokens, context, and cost."
  (let* ((content (agent-shell--format-usage usage))
         (content-width (length content))
         (title "Usage")
         ;; Top line: "╭─ Usage " + dashes + "╮"
         ;; Content line: "│ " + content + " │"
         ;; Bottom line: "╰" + dashes + "╯"
         ;; All lines should have the same total width
         (inner-width content-width)  ; width between the borders
         (top-line (concat "╭─ " title " " (make-string (- inner-width (length title) 1) ?─) "╮")))
    (concat top-line "\n"
            "│ " content " │\n"
            "╰" (make-string (+ inner-width 2) ?─) "╯")))

(defun agent-shell-show-usage ()
  "Display current session usage information in the minibuffer.
This is a stub UI function for testing usage tracking before implementing
a more polished presentation method."
  (interactive)
  (if (not (derived-mode-p 'agent-shell-mode))
      (message "Not in an agent-shell buffer")
    (message "%s" (agent-shell--format-usage (map-elt agent-shell--state :usage)))))

(defun agent-shell--format-usage (usage)
  "Format USAGE data as a display string.
USAGE should be an alist/plist with keys for token counts, context, and cost."
  (format "Context: %s/%s%s | Usage: %s total tokens (%s in, %s out%s%s%s) | Cost: %s%s"
          (if (> (or (map-elt usage :context-used) 0) 0)
              (agent-shell--format-number-compact (or (map-elt usage :context-used) 0))
            "0")
          (if (> (or (map-elt usage :context-size) 0) 0)
              (agent-shell--format-number-compact (or (map-elt usage :context-size) 0))
            "?")
          (if (and (map-elt usage :context-size)
                   (> (map-elt usage :context-size) 0))
              (format " (%.1f%%)" (* 100.0 (/ (float (or (map-elt usage :context-used) 0))
                                              (map-elt usage :context-size))))
            "")
          (if (> (or (map-elt usage :total-tokens) 0) 0)
              (agent-shell--format-number-compact (or (map-elt usage :total-tokens) 0))
            "0")
          (if (> (or (map-elt usage :input-tokens) 0) 0)
              (agent-shell--format-number-compact (or (map-elt usage :input-tokens) 0))
            "0")
          (if (> (or (map-elt usage :output-tokens) 0) 0)
              (agent-shell--format-number-compact (or (map-elt usage :output-tokens) 0))
            "0")
          (if (and (map-elt usage :thought-tokens) (> (map-elt usage :thought-tokens) 0))
              (format ", %s thought" (agent-shell--format-number-compact (map-elt usage :thought-tokens)))
            "")
          (if (and (map-elt usage :cached-read-tokens) (> (map-elt usage :cached-read-tokens) 0))
              (format ", %s cached-read" (agent-shell--format-number-compact (map-elt usage :cached-read-tokens)))
            "")
          (if (and (map-elt usage :cached-write-tokens) (> (map-elt usage :cached-write-tokens) 0))
              (format ", %s cached-write" (agent-shell--format-number-compact (map-elt usage :cached-write-tokens)))
            "")
          (if (map-elt usage :cost-currency)
              (format "%s " (map-elt usage :cost-currency))
            "$")
          (if (and (map-elt usage :cost-amount) (> (map-elt usage :cost-amount) 0))
              (format "%.2f" (map-elt usage :cost-amount))
            "0.00")))

(defun agent-shell--context-usage-indicator ()
  "Return a single character indicating context usage percentage.
Uses Unicode vertical block characters to show fill level.
Only returns an indicator if enabled and usage data is available."
  (when-let* ((agent-shell-show-context-usage-indicator)
              ((agent-shell--usage-has-data-p (map-elt (agent-shell--state) :usage)))
              (usage (map-elt (agent-shell--state) :usage))
              (context-used (map-elt usage :context-used))
              (context-size (map-elt usage :context-size))
              ((> context-size 0)))
    (let* ((percentage (/ (* 100.0 context-used) context-size))
           ;; Unicode vertical block characters from empty to full
           (indicator (cond
                       ((>= percentage 100) "█")  ; Full
                       ((>= percentage 87.5) "▇")
                       ((>= percentage 75) "▆")
                       ((>= percentage 62.5) "▅")
                       ((>= percentage 50) "▄")
                       ((>= percentage 37.5) "▃")
                       ((>= percentage 25) "▂")
                       ((> percentage 0) "▁")
                       (t nil)))  ; Return nil for no usage
           (face (cond
                  ((>= percentage 90) 'error)         ; Red for critical
                  ((>= percentage 75) 'warning)       ; Yellow/orange for warning
                  (t 'success))))                     ; Green for normal
      (when indicator
        (propertize indicator
                    'face face
                    'help-echo (agent-shell--format-usage usage))))))

(provide 'agent-shell-usage)
;;; agent-shell-usage.el ends here
