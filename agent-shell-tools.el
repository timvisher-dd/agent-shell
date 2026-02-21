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
(require 'agent-shell-meta)
(require 'agent-shell-ui)

(declare-function agent-shell--make-diff-info "agent-shell")
(declare-function agent-shell--format-diff-as-text "agent-shell")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--make-transcript-tool-call-entry "agent-shell")
(declare-function agent-shell--delete-fragment "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell-make-tool-call-label "agent-shell")
(declare-function agent-shell--save-tool-call "agent-shell")
(declare-function agent-shell--terminal-unlink-tool-call-content "agent-shell-terminal")

(defvar agent-shell-tool-use-expand-by-default)
(defvar agent-shell--transcript-file)
(defvar agent-shell-ui--content-store)

(defun agent-shell--tool-call-command-to-string (command)
  "Normalize tool call COMMAND to a display string.

COMMAND, when present, may be a shell command string or an argv vector."
  (cond ((stringp command) command)
        ((vectorp command)
         (combine-and-quote-strings (append command nil)))
        ((null command) nil)
        (t (error "Unexpected tool-call command type: %S" (type-of command)))))

(defun agent-shell--tool-call-content-text (content)
  "Return concatenated text from tool call CONTENT items."
  (let* ((items (cond
                 ((vectorp content) (append content nil))
                 ((listp content) content)
                 (content (list content))
                 (t nil)))
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

(defun agent-shell--tool-call-append-output-chunk (state tool-call-id chunk)
  "Append CHUNK to tool call output buffer for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list)))
         (chunks (map-elt entry :output-chunks)))
    (setf (map-elt entry :output-chunks) (cons chunk chunks))
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--tool-call-output-marker (state tool-call-id)
  "Return output marker for TOOL-CALL-ID in STATE."
  (map-nested-elt state `(:tool-calls ,tool-call-id :output-marker)))

(defun agent-shell--tool-call-output-ui-state (state tool-call-id)
  "Return cached UI state for TOOL-CALL-ID in STATE."
  (map-nested-elt state `(:tool-calls ,tool-call-id :output-ui-state)))

(defun agent-shell--tool-call-set-output-marker (state tool-call-id marker)
  "Set output MARKER for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list))))
    (setf (map-elt entry :output-marker) marker)
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--tool-call-set-output-ui-state (state tool-call-id ui-state)
  "Set cached UI-STATE for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list))))
    (setf (map-elt entry :output-ui-state) ui-state)
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--tool-call-body-range-info (state tool-call-id)
  "Return tool call body range info for TOOL-CALL-ID in STATE."
  (when-let ((buffer (map-elt state :buffer)))
    (with-current-buffer buffer
      (let* ((qualified-id (format "%s-%s" (map-elt state :request-count) tool-call-id))
             (match (save-mark-and-excursion
                      (goto-char (point-max))
                      (text-property-search-backward
                       'agent-shell-ui-state nil
                       (lambda (_ state)
                         (equal (map-elt state :qualified-id) qualified-id))
                       t))))
        (when match
          (let* ((block-start (prop-match-beginning match))
                 (block-end (prop-match-end match))
                 (ui-state (get-text-property block-start 'agent-shell-ui-state))
                 (body-range (agent-shell-ui--nearest-range-matching-property
                              :property 'agent-shell-ui-section :value 'body
                              :from block-start :to block-end)))
            (list (cons :ui-state ui-state)
                  (cons :body-range body-range))))))))

(defun agent-shell--tool-call-ensure-output-marker (state tool-call-id)
  "Ensure an output marker exists for TOOL-CALL-ID in STATE."
  (let* ((buffer (map-elt state :buffer))
         (marker (agent-shell--tool-call-output-marker state tool-call-id)))
    (when (or (not (markerp marker))
              (not (eq (marker-buffer marker) buffer)))
      (setq marker nil))
    (unless marker
      (when-let ((info (agent-shell--tool-call-body-range-info state tool-call-id))
                 (body-range (map-elt info :body-range)))
        (setq marker (copy-marker (map-elt body-range :end) t))
        (agent-shell--tool-call-set-output-marker state tool-call-id marker)
        (agent-shell--tool-call-set-output-ui-state state tool-call-id (map-elt info :ui-state))))
    marker))

(defun agent-shell--tool-call-output-text (state tool-call-id)
  "Return aggregated output for TOOL-CALL-ID from STATE."
  (let ((chunks (map-nested-elt state `(:tool-calls ,tool-call-id :output-chunks))))
    (when (and chunks (listp chunks))
      (mapconcat #'identity (nreverse chunks) ""))))

(defun agent-shell--tool-call-clear-output (state tool-call-id)
  "Clear aggregated output for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (map-elt tool-calls tool-call-id)))
    (when entry
      (setf (map-elt entry :output-chunks) nil)
      (setf (map-elt entry :output-last) nil)
      (setf (map-elt entry :output-marker) nil)
      (setf (map-elt entry :output-ui-state) nil)
      (setf (map-elt tool-calls tool-call-id) entry)
      (map-put! state :tool-calls tool-calls))))

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

(defun agent-shell--append-tool-call-output (state tool-call-id text)
  "Append TEXT to tool call output body for TOOL-CALL-ID in STATE.
Avoid formatting when streaming chunks."
  (when (and text (not (string-empty-p text)))
    (with-current-buffer (map-elt state :buffer)
      (let* ((inhibit-read-only t)
             (buffer-undo-list t)
             (was-at-end (eobp))
             (saved-point (copy-marker (point) t))
             (marker (agent-shell--tool-call-ensure-output-marker state tool-call-id))
             (ui-state (agent-shell--tool-call-output-ui-state state tool-call-id)))
        (if (not marker)
            (progn
              (agent-shell--update-fragment
               :state state
               :block-id tool-call-id
               :body text
               :append t
               :navigation 'always)
              (agent-shell--tool-call-ensure-output-marker state tool-call-id))
          (goto-char marker)
          (let ((start (point)))
            (insert text)
            (let ((end (point))
                  (collapsed (and ui-state (map-elt ui-state :collapsed))))
              (set-marker marker end)
              (add-text-properties
               start end
               (list
                'read-only t
                'front-sticky '(read-only)
                'agent-shell-ui-state ui-state))
              (when collapsed
                (add-text-properties start end '(invisible t))))))
        (if was-at-end
            (goto-char (point-max))
          (goto-char saved-point))
        (set-marker saved-point nil)))))

(defun agent-shell--handle-tool-call-update-streaming (state update)
  "Stream tool call UPDATE in STATE with minimal formatting."
  (let* ((tool-call-id (map-elt update 'toolCallId))
         (status (map-elt update 'status))
         (terminal-data (agent-shell--tool-call-terminal-output-data update))
         (meta-response (agent-shell--tool-call-meta-response-text update))
         (final (member status '("completed" "failed" "cancelled"))))
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
        (agent-shell--tool-call-clear-output state tool-call-id)))
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
       (or (agent-shell--tool-call-output-text state tool-call-id)
           (agent-shell--tool-call-content-text (map-elt update 'content))))
      (agent-shell--tool-call-clear-output state tool-call-id)))))

(defun agent-shell--handle-tool-call-update (state update &optional output-text)
  "Handle tool call UPDATE in STATE immediately.
OUTPUT-TEXT overrides content-derived output."
  (let-alist update
    ;; Update stored tool call data with new status and content
    (agent-shell--save-tool-call
     state
     .toolCallId
     (agent-shell--tool-call-update-overrides state update t t))
    (let* ((diff (map-nested-elt state `(:tool-calls ,.toolCallId :diff)))
           (content-text (or output-text
                             (agent-shell--tool-call-content-text .content)
                             ""))
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

" diff-text)
                        output)))
      ;; Log tool call to transcript when completed or failed
      (when (and (map-elt update 'status)
                 (member (map-elt update 'status)
                         '("completed" "failed" "cancelled")))
        (agent-shell--append-transcript
         :text (agent-shell--make-transcript-tool-call-entry
                :status (map-elt update 'status)
                :title (map-nested-elt state `(:tool-calls ,.toolCallId :title))
                :kind (map-nested-elt state `(:tool-calls ,.toolCallId :kind))
                :description (map-nested-elt state `(:tool-calls ,.toolCallId :description))
                :command (map-nested-elt state `(:tool-calls ,.toolCallId :command))
                :output body-text)
         :file-path agent-shell--transcript-file))
      ;; Hide permission after sending response.
      ;; Status and permission are no longer pending. User
      ;; likely selected one of: accepted/rejected/always.
      ;; Remove stale permission dialog.
      (when (and (map-elt update 'status)
                 (not (equal (map-elt update 'status) "pending")))
        ;; block-id must be the same as the one used as
        ;; agent-shell--update-fragment param by "session/request_permission".
        (agent-shell--delete-fragment :state state :block-id (format "permission-%s" .toolCallId)))
      (let ((tool-call-labels (agent-shell-make-tool-call-label
                               state .toolCallId)))
        (agent-shell--update-fragment
         :state state
         :block-id .toolCallId
         :label-left (map-elt tool-call-labels :status)
         :label-right (map-elt tool-call-labels :title)
         :body (string-trim body-text)
         :navigation 'always
         :expanded agent-shell-tool-use-expand-by-default)))))

(provide 'agent-shell-tools)

;;; agent-shell-tools.el ends here
