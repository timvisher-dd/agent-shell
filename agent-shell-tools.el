;;; agent-shell-tools.el --- Tool call helpers for agent-shell -*- lexical-binding: t; -*-

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
;; Tool call handling helpers for agent-shell.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-helpers)
(require 'agent-shell-terminal)
(require 'agent-shell-ui-helpers)
(require 'agent-shell-meta)

(defun agent-shell--tool-call-content-text (content)
  "Return concatenated text from tool call CONTENT items."
  (let* ((items (agent-shell--ensure-list content t))
         (parts (delq nil
                      (mapcar (lambda (item)
                                (let-alist item
                                  (when (and (stringp .content.text)
                                             (not (string-empty-p .content.text)))
                                    .content.text)))
                              items))))
    (when parts
      (mapconcat #'identity parts "\n\n"))))
(defun agent-shell--tool-call-normalize-output (text)
  "Normalize tool call output TEXT for streaming."
  (when (and text (stringp text))
    (let* ((lines (split-string text "
"))
           (filtered (seq-remove (lambda (line)
                                   (string-match-p "\`\s-*```" line))
                                 lines)))
      (string-join filtered "
"))))

(defun agent-shell--tool-call-update-overrides (state update &optional include-content include-diff)
  "Build tool call overrides for UPDATE in STATE.
INCLUDE-CONTENT and INCLUDE-DIFF control optional fields."
  (let ((diff (when include-diff
                (agent-shell--make-diff-info :tool-call update))))
    (append (list (cons :status (map-elt update 'status)))
            (when include-content
              (list (cons :content (map-elt update 'content))))
            ;; OpenCode reports bash as title in tool_call notification
            ;; without a command. tool_call_update notification may
            ;; now have the command so upgrade the title to command
            ;; as it's more useful.
            ;; See https://github.com/xenodium/agent-shell/issues/182
            (when-let* ((should-upgrade-title
                         (string= (map-nested-elt state
                                                  `(:tool-calls ,(map-elt update 'toolCallId) :title))
                                  "bash"))
                        (command (map-nested-elt update '(rawInput command))))
              (list (cons :title command)))
            (when diff
              (list (cons :diff diff))))))

(defun agent-shell--save-tool-call (state tool-call-id tool-call)
  "Store TOOL-CALL with TOOL-CALL-ID in STATE's :tool-calls alist."
  (let* ((tool-calls (map-elt state :tool-calls))
         (old-tool-call (map-elt tool-calls tool-call-id))
         (updated-tools (copy-alist tool-calls))
         (tool-call-overrides (seq-filter (lambda (pair)
                                            (cdr pair))
                                          tool-call)))
    (setf (map-elt updated-tools tool-call-id)
          (if old-tool-call
              (map-merge 'alist old-tool-call tool-call-overrides)
            tool-call-overrides))
    (map-put! state :tool-calls updated-tools)))

(defun agent-shell--mark-tool-calls-cancelled (state)
  "Mark in-flight tool-call entries in STATE as cancelled and update UI."
  (let ((tool-calls (map-elt state :tool-calls)))
    (when tool-calls
      (map-do
       (lambda (tool-call-id tool-call-data)
         (let ((status (map-elt tool-call-data :status)))
           (when (or (not status)
                     (member status '("pending" "in_progress")))
             (let ((output-text (or (agent-shell--tool-call-output-text state tool-call-id)
                                    (agent-shell--tool-call-content-text
                                     (map-elt tool-call-data :content)))))
               (agent-shell--handle-tool-call-update
                state
                `((toolCallId . ,tool-call-id)
                  (status . "cancelled"))
                output-text)
               (agent-shell--tool-call-clear-output state tool-call-id)))))
       tool-calls))))

(defun agent-shell--tool-call-final-p (status)
  "Return non-nil when STATUS represents a final tool call state."
  (and status (member status '("completed" "failed" "cancelled"))))

(defun agent-shell--handle-tool-call-update-streaming (state update)
  "Stream tool call UPDATE in STATE with minimal formatting."
  (let* ((tool-call-id (map-elt update 'toolCallId))
         (status (map-elt update 'status))
         (terminal-data (agent-shell--tool-call-terminal-output-data update))
         (meta-response (agent-shell--tool-call-meta-response-text update))
         (final (agent-shell--tool-call-final-p status))
         (has-terminal (map-nested-elt state `(:tool-calls ,tool-call-id :has-terminal))))
    (agent-shell--save-tool-call
     state
     tool-call-id
     (agent-shell--tool-call-update-overrides state update nil nil))
    (cond
     ((and terminal-data (stringp terminal-data))
      (let* ((chunk (agent-shell--tool-call-normalize-output terminal-data)))
        (when (and chunk (not (string-empty-p chunk)))
          (agent-shell--tool-call-append-output-chunk state tool-call-id chunk)
          (unless final
            (agent-shell--append-tool-call-output state tool-call-id chunk))))
      (when final
        (agent-shell--handle-tool-call-update
         state
         update
         (agent-shell--tool-call-output-text state tool-call-id))
        (unless has-terminal
          (agent-shell--tool-call-clear-output state tool-call-id))))
     ;; Non-final meta toolResponse: output in _meta.*.toolResponse
     ((and meta-response (not final))
      (let ((chunk (agent-shell--tool-call-normalize-output meta-response)))
        (when (and chunk (not (string-empty-p chunk)))
          (agent-shell--tool-call-append-output-chunk state tool-call-id chunk)
          (agent-shell--append-tool-call-output state tool-call-id chunk))))
     (final
      (agent-shell--handle-tool-call-update
       state
       update
       (unless has-terminal
         (or (agent-shell--tool-call-output-text state tool-call-id)
             (agent-shell--tool-call-content-text (map-elt update 'content)))))
      (unless has-terminal
        (agent-shell--tool-call-clear-output state tool-call-id))))))


(cl-defun agent-shell--append-transcript (&key text file-path)
  "Append TEXT to the transcript at FILE-PATH."
  (when (and file-path (agent-shell--ensure-transcript-file))
    (condition-case err
        (write-region text nil file-path t 'no-message)
      (error
       (message "Error writing to transcript: %S" err)))))

(defun agent-shell--extract-tool-parameters (raw-input)
  "Extract and format tool parameters from RAW-INPUT.
Returns a formatted string of key parameters, or nil if no relevant
parameters found.  Excludes `command', `description', and `plan' as
these are already shown separately in transcript entries.

For example, given RAW-INPUT:

  \\='((filePath . \"/home/user/project/file.el\")
    (offset . 10)
    (limit . 20)
    (command . \"grep -r foo\")
    (description . \"Search for foo\"))

returns:

  \"filePath: /home/user/project/file.el
  offset: 10
  limit: 20\""
  (when-let* ((raw-input)
              (excluded-keys '(command description plan))
              (params (seq-remove
                       (lambda (pair)
                         (let ((key (car pair))
                               (value (cdr pair)))
                           (or (memq key excluded-keys)
                               (null value)
                               (and (stringp value) (string-empty-p value)))))
                       raw-input)))
    (mapconcat (lambda (pair)
                 (format "%s: %s"
                         (symbol-name (car pair))
                         (cond
                          ((stringp (cdr pair)) (cdr pair))
                          ((numberp (cdr pair)) (number-to-string (cdr pair)))
                          ((eq (cdr pair) t) "true")
                          (t (prin1-to-string (cdr pair))))))
               params
               "\n")))

(cl-defun agent-shell--make-transcript-tool-call-entry (&key status title kind description command parameters output)
  "Create a formatted transcript entry for a tool call.

Includes STATUS, TITLE, KIND, DESCRIPTION, COMMAND, PARAMETERS, and OUTPUT."
  (concat
   (format "

### Tool Call [%s]: %s
"
           (or status "no status") (or title ""))
   (when kind
     (format "
**Tool:** %s" kind))
   (format "
**Timestamp:** %s" (format-time-string "%F %T"))
   (when description
     (format "
**Description:** %s" description))
   (when command
     (format "
**Command:** %s" command))
   (when parameters
     (format "
**Parameters:**
%s" parameters))
   "

"
   "```"
   "
"
   (string-trim
    (string-trim (string-trim output) "^```" "```$"))
   "
"
   "```"
   "
"))

(defun agent-shell--tool-call-build-body-data (state tool-call-id content output-text has-terminal)
  "Return alist with :body and :body-text for TOOL-CALL-ID.
STATE provides the stored tool call data, CONTENT is the update payload,
OUTPUT-TEXT overrides content-derived output, and HAS-TERMINAL controls
body omission for terminal-backed calls."
  (let* ((diff (map-nested-elt state `(:tool-calls ,tool-call-id :diff)))
         (content-text (cond
                        (output-text output-text)
                        (has-terminal "")
                        (t (or (agent-shell--tool-call-content-text content) ""))))
         (output (if (string-empty-p content-text)
                     ""
                   (concat "

" content-text "

")))
         (diff-text (agent-shell--format-diff-as-text diff))
         (body-text (if diff-text
                        (concat output
                                "

"
                                "╭─────────╮
"
                                "│ changes │
"
                                "╰─────────╯

"
                                diff-text)
                      output))
         (body (let ((trimmed (string-trim body-text)))
                 (if (and has-terminal (string-empty-p trimmed))
                     nil
                   trimmed))))
    (list (cons :body body)
          (cons :body-text body-text))))

(defun agent-shell--tool-call-finalize (state tool-call-id status terminal-content body-text)
  "Finalize TOOL-CALL-ID in STATE using STATUS, TERMINAL-CONTENT, and BODY-TEXT."
  (agent-shell--terminal-unlink-tool-call-content
   state tool-call-id terminal-content)
  (agent-shell--append-transcript
   :text (agent-shell--make-transcript-tool-call-entry
          :status status
          :title (map-nested-elt state `(:tool-calls ,tool-call-id :title))
          :kind (map-nested-elt state `(:tool-calls ,tool-call-id :kind))
          :description (map-nested-elt state `(:tool-calls ,tool-call-id :description))
          :command (map-nested-elt state `(:tool-calls ,tool-call-id :command))
          :parameters (agent-shell--extract-tool-parameters
                       (map-nested-elt state `(:tool-calls ,tool-call-id :raw-input)))
          :output body-text)
   :file-path agent-shell--transcript-file))

(defun agent-shell--tool-call-clear-permission (state tool-call-id status)
  "Clear permission dialog in STATE for TOOL-CALL-ID when STATUS is set."
  (when (and status
             (not (equal status "pending")))
    ;; block-id must be the same as the one used as
    ;; agent-shell--update-fragment param by "session/request_permission".
    (agent-shell--delete-fragment :state state :block-id (format "permission-%s" tool-call-id))))

(defun agent-shell--tool-call-update-ui (state tool-call-id body)
  "Update tool call fragment in STATE for TOOL-CALL-ID with BODY."
  (let ((tool-call-labels (agent-shell-make-tool-call-label
                           state tool-call-id)))
    (agent-shell--update-fragment
     :state state
     :block-id tool-call-id
     :label-left (map-elt tool-call-labels :status)
     :label-right (map-elt tool-call-labels :title)
     :body body
     :navigation 'always
     :expanded agent-shell-tool-use-expand-by-default)))

(defun agent-shell--handle-tool-call-update (state update &optional output-text)
  "Handle tool call UPDATE in STATE immediately.
OUTPUT-TEXT overrides content-derived output."
  (let-alist update
    (let* ((status (map-elt update 'status))
           (has-terminal (map-nested-elt state `(:tool-calls ,.toolCallId :has-terminal)))
           (terminal-content (or .content
                                 (map-nested-elt state `(:tool-calls ,.toolCallId :content))))
           (final (agent-shell--tool-call-final-p status)))
      ;; Update stored tool call data with new status and content
      (agent-shell--save-tool-call
       state
       .toolCallId
       (agent-shell--tool-call-update-overrides state update t t))
      (let* ((body-data (agent-shell--tool-call-build-body-data
                         state .toolCallId .content output-text has-terminal))
             (body (map-elt body-data :body))
             (body-text (map-elt body-data :body-text)))
        ;; Log tool call to transcript when completed or failed
        (when final
          (agent-shell--tool-call-finalize
           state .toolCallId status terminal-content body-text))
        ;; Hide permission after sending response.
        ;; Status and permission are no longer pending. User
        ;; likely selected one of: accepted/rejected/always.
        ;; Remove stale permission dialog.
        (agent-shell--tool-call-clear-permission state .toolCallId status)
        (agent-shell--tool-call-update-ui state .toolCallId body)))))

(provide 'agent-shell-tools)

;;; agent-shell-tools.el ends here
