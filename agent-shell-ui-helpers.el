;;; agent-shell-ui-helpers.el --- UI helpers for agent-shell -*- lexical-binding: t; -*-

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
;; UI helpers shared across agent-shell modules.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'acp)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'markdown-overlays)
(require 'shell-maker)

(require 'agent-shell-helpers)
(require 'agent-shell-diff)
(require 'agent-shell-project)
(require 'agent-shell-ui)
(require 'agent-shell-viewport)

(defcustom agent-shell-permission-icon "⚠"
  "Icon displayed when shell commands require permission to execute.

You may use \"􀇾\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-icon "💡"
  "Icon displayed during the AI's thought process.

You may use \"􁷘\" as an SF Symbol on macOS."
  :type 'string
  :group 'agent-shell)

(defcustom agent-shell-thought-process-expand-by-default nil
  "Whether thought process sections should be expanded by default.

When nil (the default), thought process sections are collapsed.
When non-nil, thought process sections are expanded."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-tool-use-expand-by-default nil
  "Whether tool use sections should be expanded by default.

When nil (the default), tool use sections are collapsed.
When non-nil, tool use sections are expanded."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-user-message-expand-by-default nil
  "Whether user message sections should be expanded by default.

When nil (the default), user message sections are collapsed.
When non-nil, user message sections are expanded."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-section-functions nil
  "Abnormal hook run after overlays are applied (experimental).
Called in `agent-shell--update-fragment' after all overlays
are applied.  Each function is called with a range alist containing:
  :block       - The block range with :start and :end positions
  :body        - The body range (if present)
  :label-left  - The left label range (if present)
  :label-right - The right label range (if present)
  :padding     - The padding range with :start and :end (if present)"
  :type 'hook
  :group 'agent-shell)

(defcustom agent-shell-highlight-blocks nil
  "Whether or not to highlight source blocks.

Highlighting source blocks is currently turned off by default
as we need a more efficient mechanism.

See https://github.com/xenodium/agent-shell/issues/119"
  :type 'boolean
  :group 'agent-shell)

(defvar agent-shell-ui--content-store)
(defvar agent-shell-mode-map)

(defun agent-shell--add-text-properties (string &rest properties)
  "Add text PROPERTIES to entire STRING and return the propertized string.
PROPERTIES should be a plist of property-value pairs."
  (let ((str (copy-sequence string))
        (len (length string)))
    (while properties
      (let ((prop (car properties))
            (value (cadr properties)))
        (if (memq prop '(face font-lock-face))
            ;; Merge face properties
            (let ((existing (get-text-property 0 prop str)))
              (put-text-property 0 len prop
                                 (if existing
                                     (list value existing)
                                   value)
                                 str))
          ;; Regular property replacement
          (put-text-property 0 len prop value str))
        (setq properties (cddr properties))))
    str))

(defun agent-shell--status-label (status)
  "Convert STATUS codes to user-visible labels."
  (let* ((config (pcase status
                   ("pending" '("pending" font-lock-comment-face))
                   ("in_progress" '("in progress" warning))
                   ("completed" '("completed" success))
                   ("failed" '("failed" error))
                   (_ '("unknown" warning))))
         (label (car config))
         (face (cadr config))
         ;; Wrap the label in [ and ] in TUI which cannot render the box border.
         (label-format (if (display-graphic-p) " %s " "[%s]")))
    (agent-shell--add-text-properties
     (propertize (format label-format label) 'font-lock-face 'default)
     'font-lock-face (list face '(:box t)))))

(defun agent-shell--shorten-paths (text &optional include-project)
  "Shorten file paths in TEXT relative to project root.

\"/path/to/project/file.txt\" -> \"file.txt\"

With INCLUDE-PROJECT

\"/path/to/project/file.txt\" -> \"project/file.txt\""
  (when text
    (let ((cwd (string-remove-suffix "/" (agent-shell-cwd))))
      (replace-regexp-in-string (concat (regexp-quote
                                         (if include-project
                                             (string-remove-suffix
                                              "/"
                                              (file-name-directory
                                               (directory-file-name cwd)))
                                           cwd)) "/")
                                ""
                                (or text "")))))

(defun agent-shell--ensure-transcript-file ()
  "Ensure the transcript file exists, creating it with header if needed.
Returns the file path, or nil if disabled."
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (when-let* ((filepath agent-shell--transcript-file)
              (dir (file-name-directory filepath)))
    (unless (file-exists-p filepath)
      (condition-case err
          (let ((agent-name (or (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
                                (map-nested-elt agent-shell--state '(:agent-config :buffer-name))
                                "Unknown Agent")))
            (make-directory dir t)
            (write-region
             (format "# Agent Shell Transcript

**Agent:** %s
**Started:** %s
**Working Directory:** %s

---

"
                     agent-name
                     (format-time-string "%F %T")
                     (agent-shell-cwd))
             nil filepath nil 'no-message)
            (message "Created %s"
                     (agent-shell--shorten-paths filepath t)))
        (error
         (message "Failed to initialize transcript: %S" err))))
    filepath))

(defun agent-shell-make-tool-call-label (state tool-call-id)
  "Create tool call label from STATE using TOOL-CALL-ID.

Returns propertized labels in :status and :title propertized."
  (when-let ((tool-call (map-nested-elt state `(:tool-calls ,tool-call-id))))
    `((:status . ,(let ((status (when (map-elt tool-call :status)
                                  (agent-shell--status-label (map-elt tool-call :status))))
                        (kind (when (map-elt tool-call :kind)
                                ;; Wrap the label in [ and ] in TUI which cannot render the box border.
                                (let* ((label-format (if (display-graphic-p) " %s " "[%s]")))
                                  (agent-shell--add-text-properties
                                   (propertize (format label-format (map-elt tool-call :kind))
                                               'font-lock-face 'default)
                                   'font-lock-face
                                   `(:box t))))))
                    (concat
                     (when status
                       status)
                     (when (and status kind)
                       " ")
                     (when kind
                       kind))))
      (:title . ,(let* ((title (when (map-elt tool-call :title)
                                 (agent-shell--shorten-paths (map-elt tool-call :title))))
                        (description (when (map-elt tool-call :description)
                                       (agent-shell--shorten-paths (map-elt tool-call :description)))))
                   (cond ((and title description
                               (not (equal (string-remove-prefix "`" (string-remove-suffix "`" (string-trim title)))
                                           (string-remove-prefix "`" (string-remove-suffix "`" (string-trim description))))))
                          (concat
                           (propertize title 'font-lock-face 'font-lock-doc-markup-face)
                           " "
                           (propertize description 'font-lock-face 'font-lock-doc-face)))
                         (title
                          (propertize title 'font-lock-face 'font-lock-doc-markup-face))
                         (description
                          (propertize description 'font-lock-face 'font-lock-doc-markup-face))))))))

(defun agent-shell--format-plan (entries)
  "Format plan ENTRIES for shell rendering."
  (agent-shell--align-alist
   :data entries
   :columns (list
             (lambda (entry)
               (agent-shell--status-label (map-elt entry 'status)))
             (lambda (entry)
               (map-elt entry 'content)))
   :joiner "\n"))

(defun agent-shell--format-available-commands (commands)
  "Format COMMANDS for shell rendering."
  (agent-shell--align-alist
   :data commands
   :columns (list
             (lambda (cmd)
               (propertize (concat "/" (map-elt cmd 'name))
                           'font-lock-face 'font-lock-function-name-face))
             (lambda (cmd)
               (when-let ((description (map-elt cmd 'description)))
                 (propertize description
                             'font-lock-face 'font-lock-comment-face))))
   :joiner "\n"))

(cl-defun agent-shell--delete-fragment (&key state block-id)
  "Delete fragment with STATE and BLOCK-ID."
  (when-let (((map-elt state :buffer))
             (viewport-buffer (agent-shell-viewport--buffer
                               :shell-buffer (map-elt state :buffer)
                               :existing-only t)))
    (with-current-buffer viewport-buffer
      (agent-shell-ui-delete-fragment :namespace-id (map-elt state :request-count) :block-id block-id :no-undo t)))
  (with-current-buffer (map-elt state :buffer)
    (unless (and (derived-mode-p 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (agent-shell-ui-delete-fragment :namespace-id (map-elt state :request-count) :block-id block-id :no-undo t)))

(cl-defun agent-shell--update-fragment (&key state namespace-id block-id label-left label-right
                                             body append create-new navigation expanded)
  "Update fragment in the shell buffer.

Creates or updates existing dialog using STATE's request count as namespace
unless NAMESPACE-ID (rarely needed).  Rely on count is possible.

BLOCK-ID uniquely identifies the block.

Dialog can have LABEL-LEFT, LABEL-RIGHT, and BODY.

Optional flags: APPEND text to existing content, CREATE-NEW block,
NAVIGATION for navigation style, EXPANDED to show block expanded
by default."
  (when-let (((map-elt state :buffer))
             (viewport-buffer (agent-shell-viewport--buffer
                               :shell-buffer (map-elt state :buffer)
                               :existing-only t))
             ((with-current-buffer viewport-buffer
                (derived-mode-p 'agent-shell-viewport-view-mode))))
    (with-current-buffer viewport-buffer
      (let ((inhibit-read-only t))
        ;; TODO: Investigate why save-restriction isn't enough
        ;; to save point. Saving (point) for now.
        (when-let* ((saved-point (point))
                    (range (agent-shell-ui-update-fragment
                            (agent-shell-ui-make-fragment-model
                             :namespace-id (or namespace-id
                                               (map-elt state :request-count))
                             :block-id block-id
                             :label-left label-left
                             :label-right label-right
                             :body body)
                            :navigation navigation
                            :append append
                            :create-new create-new
                            :expanded expanded
                            :no-undo t))
                    (padding-start (map-nested-elt range '(:padding :start)))
                    (padding-end (map-nested-elt range '(:padding :end)))
                    (block-start (map-nested-elt range '(:block :start)))
                    (block-end (map-nested-elt range '(:block :end))))
          ;; Apply markdown overlay to body.
          (save-restriction
            (when-let ((body-start (map-nested-elt range '(:body :start)))
                       (body-end (map-nested-elt range '(:body :end))))
              (narrow-to-region body-start body-end)
              (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                (markdown-overlays-put))))
          ;; Note: For now, we're skipping applying markdown overlays
          ;; on left labels as they currently carry propertized text
          ;; for statuses (ie. boxed).
          ;;
          ;; Apply markdown overlay to right label.
          (save-restriction
            (when-let ((label-right-start (map-nested-elt range '(:label-right :start)))
                       (label-right-end (map-nested-elt range '(:label-right :end))))
              (narrow-to-region label-right-start label-right-end)
              (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                (markdown-overlays-put))))
          (goto-char saved-point)))))
  (with-current-buffer (map-elt state :buffer)
    (unless (and (derived-mode-p 'agent-shell-mode)
                 (equal (current-buffer)
                        (map-elt state :buffer)))
      (error "Editing the wrong buffer: %s" (current-buffer)))
    (shell-maker-with-auto-scroll-edit
     (when-let* ((range (agent-shell-ui-update-fragment
                         (agent-shell-ui-make-fragment-model
                          :namespace-id (or namespace-id
                                            (map-elt state :request-count))
                          :block-id block-id
                          :label-left label-left
                          :label-right label-right
                          :body body)
                         :navigation navigation
                         :append append
                         :create-new create-new
                         :expanded expanded
                         :no-undo t))
                 (padding-start (map-nested-elt range '(:padding :start)))
                 (padding-end (map-nested-elt range '(:padding :end)))
                 (block-start (map-nested-elt range '(:block :start)))
                 (block-end (map-nested-elt range '(:block :end))))
       (save-restriction
         ;; TODO: Move this to shell-maker?
         (let ((inhibit-read-only t))
           ;; comint relies on field property to
           ;; derive `comint-next-prompt'.
           ;; Marking as field to avoid false positives in
           ;; `agent-shell-next-item' and `agent-shell-previous-item'.
           (add-text-properties (or padding-start block-start)
                                (or padding-end block-end) '(field output)))
         ;; Apply markdown overlay to body.
         (when-let ((body-start (map-nested-elt range '(:body :start)))
                    (body-end (map-nested-elt range '(:body :end))))
           (narrow-to-region body-start body-end)
           (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
             (markdown-overlays-put))
           (widen))
         ;;
         ;; Note: For now, we're skipping applying markdown overlays
         ;; on left labels as they currently carry propertized text
         ;; for statuses (ie. boxed).
         ;;
         ;; Apply markdown overlay to right label.
         (when-let ((label-right-start (map-nested-elt range '(:label-right :start)))
                    (label-right-end (map-nested-elt range '(:label-right :end))))
           (narrow-to-region label-right-start label-right-end)
           (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
             (markdown-overlays-put))
           (widen)))
       (run-hook-with-args 'agent-shell-section-functions range)))))

(cl-defun agent-shell--update-text (&key state namespace-id block-id text append create-new)
  "Update plain text entry in the shell buffer.

Uses STATE's request count as namespace unless NAMESPACE-ID is given.
BLOCK-ID uniquely identifies the entry.
TEXT is the string to insert or append.
APPEND and CREATE-NEW control update behavior."
  (let ((ns (or namespace-id (map-elt state :request-count))))
    (when-let (((map-elt state :buffer))
               (viewport-buffer (agent-shell-viewport--buffer
                                 :shell-buffer (map-elt state :buffer)
                                 :existing-only t))
               ((with-current-buffer viewport-buffer
                  (derived-mode-p 'agent-shell-viewport-view-mode))))
      (with-current-buffer viewport-buffer
        (let ((inhibit-read-only t))
          (agent-shell-ui-update-text
           :namespace-id ns
           :block-id block-id
           :text text
           :append append
           :create-new create-new
           :no-undo t))))
    (with-current-buffer (map-elt state :buffer)
      (shell-maker-with-auto-scroll-edit
       (when-let* ((range (agent-shell-ui-update-text
                           :namespace-id ns
                           :block-id block-id
                           :text text
                           :append append
                           :create-new create-new
                           :no-undo t))
                   (block-start (map-nested-elt range '(:block :start)))
                   (block-end (map-nested-elt range '(:block :end))))
         (let ((inhibit-read-only t))
           (add-text-properties block-start block-end '(field output))))))))

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

(defun agent-shell--tool-call-append-output-chunk (state tool-call-id chunk)
  "Append CHUNK to tool call output buffer for TOOL-CALL-ID in STATE."
  (let* ((tool-calls (map-elt state :tool-calls))
         (entry (or (map-elt tool-calls tool-call-id) (list)))
         (chunks (map-elt entry :output-chunks)))
    (setf (map-elt entry :output-chunks) (cons chunk chunks))
    (setf (map-elt tool-calls tool-call-id) entry)
    (map-put! state :tool-calls tool-calls)))

(defun agent-shell--append-tool-call-output (state tool-call-id text)
  "Append TEXT to TOOL-CALL-ID output body in STATE without formatting."
  (when (and text (not (string-empty-p text)))
    (with-current-buffer (map-elt state :buffer)
      (let* ((inhibit-read-only t)
             (buffer-undo-list t)
             (was-at-end (eobp))
             (saved-point (copy-marker (point) t))
             (marker (agent-shell--tool-call-ensure-output-marker state tool-call-id))
             (ui-state (agent-shell--tool-call-output-ui-state state tool-call-id))
             (store-output (lambda (state)
                             (when state
                               (let* ((qualified-id (map-elt state :qualified-id))
                                      (key (and qualified-id (concat qualified-id "-body"))))
                                 (when key
                                   (unless agent-shell-ui--content-store
                                     (setq agent-shell-ui--content-store (make-hash-table :test 'equal)))
                                   (puthash key
                                            (concat (or (gethash key agent-shell-ui--content-store) "") text)
                                            agent-shell-ui--content-store)))))))
        (if (not marker)
            (progn
              (agent-shell--update-fragment
               :state state
               :block-id tool-call-id
               :body text
               :append t
               :navigation 'always)
              (agent-shell--tool-call-ensure-output-marker state tool-call-id)
              (setq ui-state (agent-shell--tool-call-output-ui-state state tool-call-id))
              (funcall store-output ui-state))
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
              (funcall store-output ui-state)
              (when collapsed
                (add-text-properties start end '(invisible t))))))
        (if was-at-end
            (goto-char (point-max))
          (goto-char saved-point))
        (set-marker saved-point nil)))))

(cl-defun agent-shell--make-button (&key text help kind action keymap)
  "Make button with TEXT, HELP text, KIND, KEYMAP, and ACTION."
  ;; Use [ ] brackets in TUI which cannot render the box border.
  (let ((button (propertize
                 (if (display-graphic-p)
                     (format " %s " text)
                   (format "[ %s ]" text))
                 'font-lock-face '(:box t)
                 'help-echo help
                 'pointer 'hand
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1] action)
                           (define-key map (kbd "RET") action)
                           (define-key map [remap self-insert-command] 'ignore)
                           (when keymap
                             (set-keymap-parent map keymap))
                           map)
                 'button kind)))
    button))

(cl-defun agent-shell--make-tool-call-permission-text (&key request client state)
  "Create text to render permission dialog using REQUEST, CLIENT, and STATE.

For example:

   ╭─

       ⚠ Tool Permission ⚠

       Add more cowbell

       [ View (v) ] [ Allow (y) ] [ Reject (n) ] [ Always Allow (!) ]

   ╰─"
  (let* ((tool-call-id (map-nested-elt request '(params toolCall toolCallId)))
         (diff (map-nested-elt state `(:tool-calls ,tool-call-id :diff)))
         (actions (agent-shell--make-permission-actions (map-nested-elt request '(params options))))
         (shell-buffer (map-elt state :buffer))
         (keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (when-let ((char (map-elt action :char)))
                       (define-key map (kbd char)
                                   (lambda ()
                                     (interactive)
                                     (agent-shell--send-permission-response
                                      :client client
                                      :request-id (map-elt request 'id)
                                      :option-id (map-elt action :option-id)
                                      :state state
                                      :tool-call-id tool-call-id
                                      :message-text (map-elt action :option))
                                     (when (equal (map-elt action :kind) "reject_once")
                                       ;; No point in rejecting the change but letting
                                       ;; the agent continue (it doesn't know why you
                                       ;; have rejected the change).
                                       ;; May as well interrupt so you can course-correct.
                                       (with-current-buffer shell-buffer
                                         (when (fboundp 'agent-shell-interrupt)
                                           (funcall #'agent-shell-interrupt t))))))))
                   ;; Add diff keybinding if diff info is available
                   (when diff
                     (define-key map "v" (agent-shell--make-diff-viewing-function
                                          :diff diff
                                          :actions actions
                                          :client client
                                          :request-id (map-elt request 'id)
                                          :state state
                                          :tool-call-id tool-call-id)))
                   ;; Add interrupt keybinding
                   (define-key map (kbd "C-c C-c")
                               (lambda ()
                                 (interactive)
                                 (with-current-buffer shell-buffer
                                   (when (fboundp 'agent-shell-interrupt)
                                     (funcall #'agent-shell-interrupt t)))))
                   map))
         (title (let* ((title (map-nested-elt request '(params toolCall title)))
                       (command (agent-shell--tool-call-command-to-string
                                 (map-nested-elt request '(params toolCall rawInput command)))))
                  ;; Some agents don't include the command in the
                  ;; permission/tool call title, so it's hard to know
                  ;; what the permission is actually allowing.
                  ;; Display command if needed.
                  (if (and (stringp title)
                           (stringp command)
                           (not (string-empty-p command))
                           (string-match-p (regexp-quote command) title))
                      title
                    (or command title))))
         (diff-button (when diff
                        (agent-shell--make-permission-button
                         :text "View (v)"
                         :help "Press v to view diff"
                         :action (agent-shell--make-diff-viewing-function
                                  :diff diff
                                  :actions actions
                                  :client client
                                  :request-id (map-elt request 'id)
                                  :state state
                                  :tool-call-id tool-call-id)
                         :keymap keymap
                         :navigatable t
                         :char "v"
                         :option "view diff"))))
    (format "╭─

    %s %s %s%s

    %s%s

╰─"
            (propertize agent-shell-permission-icon
                        'font-lock-face 'warning)
            (propertize "Tool Permission" 'font-lock-face 'bold)
            (propertize agent-shell-permission-icon
                        'font-lock-face 'warning)
            (if title
                (propertize
                 (format "\n\n\n    %s" title)
                 'font-lock-face 'comint-highlight-input)
              "")
            (if diff-button
                (concat diff-button " ")
              "")
            (mapconcat (lambda (action)
                         (agent-shell--make-permission-button
                          :text (map-elt action :label)
                          :help (map-elt action :label)
                          :action (lambda ()
                                    (interactive)
                                    (agent-shell--send-permission-response
                                     :client client
                                     :request-id (map-elt request 'id)
                                     :option-id (map-elt action :option-id)
                                     :state state
                                     :tool-call-id tool-call-id
                                     :message-text (format "Selected: %s" (map-elt action :option)))
                                    (when (equal (map-elt action :kind) "reject_once")
                                      ;; No point in rejecting the change but letting
                                      ;; the agent continue (it doesn't know why you
                                      ;; have rejected the change).
                                      ;; May as well interrupt so you can course-correct.
                                      (with-current-buffer shell-buffer
                                        (when (fboundp 'agent-shell-interrupt)
                                          (funcall #'agent-shell-interrupt t)))))
                          :keymap keymap
                          :char (map-elt action :char)
                          :option (map-elt action :option)
                          :navigatable t))
                       actions
                       " "))))

(cl-defun agent-shell--send-permission-response (&key client request-id option-id cancelled state tool-call-id message-text)
  "Send a response to a permission request and clean up related dialog UI.

Choose OPTION-ID or CANCELLED (never both).

CLIENT: The ACP client used to send the response.
REQUEST-ID: The ID of the original permission request.
OPTION-ID: The ID of the selected permission option.
CANCELLED: Non-nil if the request was cancelled instead of selecting an option.
STATE: The buffer-local agent-shell session state.
TOOL-CALL-ID: The tool call identifier.
MESSAGE-TEXT: Optional message to display after sending the response."
  (acp-send-response
   :client client
   :response (acp-make-session-request-permission-response
              :request-id request-id
              :cancelled cancelled
              :option-id option-id))
  ;; Ensure in the shell buffer for state operations, as this
  ;; function may be invoked from a viewport buffer.
  (with-current-buffer (map-elt state :buffer)
    ;; Hide permission after sending response.
    ;; block-id must be the same as the one used as
    ;; agent-shell--update-fragment param by "session/request_permission".
    (agent-shell--delete-fragment :state state :block-id (format "permission-%s" tool-call-id))
    (map-put! state :tool-calls
              (map-delete (map-elt state :tool-calls) tool-call-id))
    (when (fboundp 'agent-shell--emit-event)
      (funcall #'agent-shell--emit-event
               :event 'permission-response
               :data (list (cons :request-id request-id)
                           (cons :tool-call-id tool-call-id)
                           (cons :option-id option-id)
                           (cons :cancelled cancelled))))
    (when message-text
      (message "%s" message-text))
    ;; Jump to any remaining permission buttons, or go to end of buffer.
    (or (agent-shell-jump-to-latest-permission-button-row)
        (goto-char (point-max)))
    (when-let (((map-elt state :buffer))
               (viewport-buffer (agent-shell-viewport--buffer
                                 :shell-buffer (map-elt state :buffer)
                                 :existing-only t)))
      (with-current-buffer viewport-buffer
        (or (agent-shell-jump-to-latest-permission-button-row)
            (goto-char (point-max)))))))

(cl-defun agent-shell--resolve-permission-choice-to-action (&key choice actions)
  "Resolve `agent-shell-diff' CHOICE to permission action from ACTIONS.

CHOICE can be \='accept or \='reject.
Returns the matching action or nil if no match found."
  (cond
   ((equal choice 'accept)
    (seq-find (lambda (action)
                (string= (map-elt action :kind) "allow_once"))
              actions))
   ((equal choice 'reject)
    (seq-find (lambda (action)
                (string= (map-elt action :kind) "reject_once"))
              actions))
   (t nil)))

(cl-defun agent-shell--make-diff-viewing-function (&key diff actions client request-id state tool-call-id)
  "Create a diffing handler for the ACP CLIENT's REQUEST-ID and TOOL-CALL-ID.

DIFF as per `agent-shell--make-diff-info'.
ACTIONS as per `agent-shell--make-permission-action'."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((shell-buffer (current-buffer)))
    (lambda ()
      (interactive)
      (agent-shell-diff
       :old (map-elt diff :old)
       :new (map-elt diff :new)
       :file (map-elt diff :file)
       :title (file-name-nondirectory (map-elt diff :file))
       :bindings (list (list :key "n"
                             :description "next hunk"
                             :command 'diff-hunk-next)
                       (list :key "p"
                             :description "previous hunk"
                             :command 'diff-hunk-prev)
                       (list :key "y"
                             :description "accept all"
                             :kind 'accept-all
                             :command (lambda ()
                                        (interactive)
                                        (let ((action (agent-shell--resolve-permission-choice-to-action
                                                       :choice 'accept
                                                       :actions actions))
                                              (agent-shell-on-exit nil))
                                          ;; Disable on-exit since killing
                                          ;; the buffer should not trigger
                                          ;; asking user if they want to
                                          ;; keep or reject changes.
                                          (kill-current-buffer)
                                          (with-current-buffer shell-buffer
                                            (agent-shell--send-permission-response
                                             :client client
                                             :request-id request-id
                                             :option-id (map-elt action :option-id)
                                             :state state
                                             :tool-call-id tool-call-id
                                             :message-text (map-elt action :option))))))
                       (list :key (key-description (where-is-internal 'agent-shell-interrupt agent-shell-mode-map t))
                             :description nil ;; hide from header-line-format
                             :kind 'reject-all
                             :command (lambda ()
                                        (interactive)
                                        (when (y-or-n-p "Interrupt?")
                                          (let ((agent-shell-on-exit nil))
                                            ;; Disable on-exit since killing
                                            ;; the buffer should not trigger
                                            ;; asking user if they want to
                                            ;; keep or reject changes.
                                            (kill-current-buffer))
                                          (with-current-buffer shell-buffer
                                            (when (fboundp 'agent-shell-interrupt)
                                              (funcall #'agent-shell-interrupt t))))))
                       (list :key "f"
                             :description (concat "open " (file-name-nondirectory (map-elt diff :file)))
                             :command (lambda ()
                                        (interactive)
                                        (find-file (map-elt diff :file))))
                       (list :key "q" :description "exit" :command 'kill-current-buffer))
       :on-exit (lambda ()
                  (if-let ((choice (condition-case nil
                                       (if (y-or-n-p "Accept changes?")
                                           'accept
                                         'reject)
                                     (quit 'ignore)))
                           (action (agent-shell--resolve-permission-choice-to-action
                                    :choice choice
                                    :actions actions)))
                      (progn
                        (agent-shell--send-permission-response
                         :client client
                         :request-id request-id
                         :option-id (map-elt action :option-id)
                         :state state
                         :tool-call-id tool-call-id
                         :message-text (map-elt action :option))
                        (when (eq choice 'reject)
                          ;; No point in rejecting the change but letting
                          ;; the agent continue (it doesn't know why you
                          ;; have rejected the change).
                          ;; May as well interrupt so you can course-correct.
                          (with-current-buffer shell-buffer
                            (when (fboundp 'agent-shell-interrupt)
                              (funcall #'agent-shell-interrupt t)))))
                    (message "Ignored")))))))

(cl-defun agent-shell--make-permission-button (&key text help action keymap navigatable char option)
  "Create a permission button with TEXT, HELP, ACTION, and KEYMAP.

For example:

  [ Allow (y) ]

When NAVIGATABLE is non-nil, make button character navigatable.
CHAR and OPTION are used for cursor sensor messages."
  (let ((button (agent-shell--make-button
                 :text text
                 :help help
                 :kind 'permission
                 :keymap keymap
                 :action action)))
    (when navigatable
      ;; Make the button character navigatable.
      ;;
      ;; For example, the "y" in:
      ;;
      ;; Graphical: " Allow (y) "
      ;;
      ;; Terminal: "[ Allow (y) ]"
      ;;
      ;; so adjust the offsets accordingly.
      (let ((trailing (if (display-graphic-p) 2 3)))
        (put-text-property (- (length button) (+ trailing 1))
                           (- (length button) trailing)
                           'agent-shell-permission-button t button)
        (put-text-property (- (length button) (+ trailing 1))
                           (- (length button) trailing)
                           'cursor-sensor-functions
                           (list (lambda (_window _old-pos sensor-action)
                                   (when (eq sensor-action 'entered)
                                     (if char
                                         (message "Press RET or %s to %s" char option)
                                       (message "Press RET to %s" option)))))
                           button)))
    button))

(defun agent-shell--make-permission-actions (acp-options)
  "Make actions from ACP-OPTIONS for shell rendering.

See `agent-shell--make-permission-action' for ACP-OPTION and return schema."
  (let (acp-seen-kinds)
    (seq-sort (lambda (a b)
                (< (length (map-elt a :label))
                   (length (map-elt b :label))))
              (delq nil (mapcar (lambda (acp-option)
                                  (let ((action (agent-shell--make-permission-action
                                                 :acp-option acp-option
                                                 :acp-seen-kinds acp-seen-kinds)))
                                    (push (map-elt acp-option 'kind) acp-seen-kinds)
                                    action))
                                acp-options)))))

(cl-defun agent-shell--make-permission-action (&key acp-option acp-seen-kinds)
  "Convert a single ACP-OPTION to an action alist.

ACP-OPTION should be a PermissionOption per ACP spec:

  https://agentclientprotocol.com/protocol/schema#permissionoption

An alist of the form:

  ((\='kind . \"allow_once\")
   (\='name . \"Allow\")
   (\='optionId . \"allow\"))

ACP-SEEN-KINDS is a list of kinds already processed.  If kind is in
ACP-SEEN-KINDS, omit the keybinding to avoid duplicates.

Returns an alist of the form:

  ((:label . \"Allow (y)\")
   (:option . \"Allow\")
   (:char . ?y)
   (:kind . \"allow_once\")
   (:option-id . ...))

Returns nil if the ACP-OPTION kind is not recognized."
  (let* ((char-map `(("allow_always" . "!")
                     ("allow_once" . "y")
                     ("reject_once" . ,(or (ignore-errors
                                             (key-description (where-is-internal 'agent-shell-interrupt
                                                                                 agent-shell-mode-map t)))
                                           "n"))))
         (kind (map-elt acp-option 'kind))
         (char (unless (member kind acp-seen-kinds)
                 (map-elt char-map kind)))
         (name (map-elt acp-option 'name)))
    (when (map-elt char-map kind)
      (map-into `((:label . ,(if char (format "%s (%s)" name char) name))
                  (:option . ,name)
                  (:char . ,char)
                  (:kind . ,kind)
                  (:option-id . ,(map-elt acp-option 'optionId)))
                'alist))))

(defun agent-shell-next-permission-button ()
  "Jump to the next button."
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let ((next-change (next-single-property-change
                                                  (point) 'agent-shell-permission-button)))
                           (goto-char next-change)))
                       (when-let ((next (text-property-search-forward
                                         'agent-shell-permission-button t t)))
                         (prop-match-beginning next)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-previous-permission-button ()
  "Jump to the previous button."
  (interactive)
  (when-let* ((found (save-mark-and-excursion
                       (when (get-text-property (point) 'agent-shell-permission-button)
                         (when-let ((prev-change (previous-single-property-change
                                                  (point) 'agent-shell-permission-button)))
                           (goto-char prev-change)))
                       (when-let ((prev (text-property-search-backward
                                         'agent-shell-permission-button t t)))
                         (prop-match-beginning prev)))))
    (deactivate-mark)
    (goto-char found)
    found))

(defun agent-shell-jump-to-latest-permission-button-row ()
  "Jump to the latest permission button row.

Returns non-nil if a permission button was found, nil otherwise."
  (interactive)
  (when-let ((found (save-mark-and-excursion
                      (goto-char (point-max))
                      (agent-shell-previous-permission-button))))
    (deactivate-mark)
    (goto-char found)
    (beginning-of-line)
    (agent-shell-next-permission-button)
    (when-let ((window (get-buffer-window (current-buffer))))
      (set-window-point window (point)))
    t))

(provide 'agent-shell-ui-helpers)

;;; agent-shell-ui-helpers.el ends here
