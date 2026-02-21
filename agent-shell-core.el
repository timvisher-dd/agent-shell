;;; agent-shell-core.el --- Core utilities for agent-shell -*- lexical-binding: t; -*-

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
;; Core utilities shared across agent-shell modules.
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

(defcustom agent-shell-path-resolver-function nil
  "Function for resolving remote paths on the local file-system, and vice versa.

Expects a function that takes the path as its single argument, and
returns the resolved path.  Set to nil to disable mapping."
  :type 'function
  :group 'agent-shell)

(defcustom agent-shell-container-command-runner nil
  "Command prefix for executing commands in a container.

When non-nil, both the agent command and shell commands will be
executed using this runner.  Can be a list of strings or a function
that takes a buffer and returns a list.

Example for static devcontainer:
  \\='(\"devcontainer\" \"exec\" \"--workspace-folder\" \".\")

Example for dynamic per-agent containers:
  (lambda (buffer)
    (let ((config (agent-shell-get-config buffer)))
      (pcase (map-elt config :identifier)
        (\\='claude-code \\='(\"docker\" \"exec\" \"claude-dev\" \"--\"))
        (\\='gemini-cli \\='(\"docker\" \"exec\" \"gemini-dev\" \"--\"))
        (_ \\='(\"devcontainer\" \"exec\" \".\")))))

Example for per-session containers:
  (lambda (buffer)
    (if (string-match \"project-a\" (buffer-name buffer))
        \\='(\"docker\" \"exec\" \"project-a-dev\" \"--\")
      \\='(\"docker\" \"exec\" \"project-b-dev\" \"--\")))"
  :type '(choice (repeat string) function)
  :group 'agent-shell)

(cl-defun agent-shell--make-state (&key agent-config buffer client-maker needs-authentication authenticate-request-maker heartbeat outgoing-request-decorator)
  "Construct shell agent state with AGENT-CONFIG and BUFFER.

Shell state is provider-dependent and needs CLIENT-MAKER, NEEDS-AUTHENTICATION,
HEARTBEAT, AUTHENTICATE-REQUEST-MAKER, and optionally
OUTGOING-REQUEST-DECORATOR (passed through to `acp-make-client')."
  (list (cons :agent-config agent-config)
        (cons :buffer buffer)
        (cons :client nil)
        (cons :client-maker client-maker)
        (cons :outgoing-request-decorator outgoing-request-decorator)
        (cons :heartbeat heartbeat)
        (cons :initialized nil)
        (cons :needs-authentication needs-authentication)
        (cons :authenticate-request-maker authenticate-request-maker)
        (cons :authenticated nil)
        (cons :set-model nil)
        (cons :set-session-mode nil)
        (cons :session (list (cons :id nil)
                             (cons :mode-id nil)
                             (cons :modes nil)))
        (cons :last-entry-type nil)
        (cons :chunked-group-count 0)
        (cons :request-count 0)
        (cons :tool-calls nil)
        (cons :terminals nil)
        (cons :terminal-count 0)
        (cons :available-commands nil)
        (cons :available-modes nil)
        (cons :supports-session-list nil)
        (cons :supports-session-load nil)
        (cons :supports-session-resume nil)
        (cons :prompt-capabilities nil)
        (cons :event-subscriptions nil)
        (cons :pending-requests nil)
        (cons :usage (list (cons :total-tokens 0)
                           (cons :input-tokens 0)
                           (cons :output-tokens 0)
                           (cons :thought-tokens 0)
                           (cons :cached-read-tokens 0)
                           (cons :cached-write-tokens 0)
                           (cons :context-used 0)
                           (cons :context-size 0)
                           (cons :cost-amount 0.0)
                           (cons :cost-currency nil)))))

(defvar-local agent-shell--state
    (agent-shell--make-state))

(defvar-local agent-shell--transcript-file nil
  "Path to the shell's transcript file.")

(cl-defun agent-shell--emit-event (&key event data)
  "Emit an EVENT to matching subscribers.
EVENT is a symbol identifying the event.
DATA is an optional alist of event-specific data."
  (let ((event-alist (list (cons :event event))))
    (when data
      (push (cons :data data) event-alist))
    (dolist (sub (map-elt agent-shell--state :event-subscriptions))
      (when (or (not (map-elt sub :event))
                (eq (map-elt sub :event) event))
        (with-current-buffer (map-elt agent-shell--state :buffer)
          (funcall (map-elt sub :on-event) event-alist))))))

(defun agent-shell--build-command-for-execution (command)
  "Build COMMAND for the configured execution environment.

COMMAND should be a list of command parts (executable and arguments).
Returns the adapted command if a container runner is configured,
otherwise returns COMMAND unchanged."
  (pcase agent-shell-container-command-runner
    ((pred functionp)
     (append (funcall agent-shell-container-command-runner
                      (current-buffer)) command))
    ((pred listp) (append agent-shell-container-command-runner command))
    (_ command)))

(cl-defun agent-shell--make-acp-client (&key command
                                             command-params
                                             environment-variables
                                             context-buffer)
  "Create an ACP client, optionally wrapping with container runner.

COMMAND, COMMAND-PARAMS, ENVIRONMENT-VARIABLES, and CONTEXT-BUFFER are
passed through to `acp-make-client'.

If `agent-shell-container-command-runner' is set, the command will be
wrapped with the runner prefix."
  (let* ((full-command (append (list command) command-params))
         (wrapped-command (agent-shell--build-command-for-execution full-command)))
    (acp-make-client :command (car wrapped-command)
                     :command-params (cdr wrapped-command)
                     :environment-variables environment-variables
                     :context-buffer context-buffer
                     :outgoing-request-decorator (when context-buffer
                                                   (map-elt (buffer-local-value 'agent-shell--state context-buffer)
                                                            :outgoing-request-decorator)))))

;;;###autoload
(cl-defun agent-shell-make-agent-config (&key identifier
                                              mode-line-name welcome-function
                                              buffer-name shell-prompt shell-prompt-regexp
                                              client-maker
                                              needs-authentication
                                              authenticate-request-maker
                                              default-model-id
                                              default-session-mode-id
                                              icon-name
                                              install-instructions)
  "Create an agent configuration alist.

Keyword arguments:
- IDENTIFIER: Symbol identifying agent type (e.g., \='claude-code)
- MODE-LINE-NAME: Name to display in the mode line
- WELCOME-FUNCTION: Function to call for welcome message
- BUFFER-NAME: Name of the agent buffer
- SHELL-PROMPT: The shell prompt string
- SHELL-PROMPT-REGEXP: Regexp to match the shell prompt
- CLIENT-MAKER: Function to create the client
- NEEDS-AUTHENTICATION: Non-nil authentication is required
- AUTHENTICATE-REQUEST-MAKER: Function to create authentication requests
- DEFAULT-MODEL-ID: Default model ID (function returning value).
- DEFAULT-SESSION-MODE-ID: Default session mode ID (function returning value).
- ICON-NAME: Name of the icon to use
- INSTALL-INSTRUCTIONS: Instructions to show when executable is not found

Returns an alist with all specified values."
  `((:identifier . ,identifier)
    (:mode-line-name . ,mode-line-name)
    (:welcome-function . ,welcome-function)                     ;; function
    (:buffer-name . ,buffer-name)
    (:shell-prompt . ,shell-prompt)
    (:shell-prompt-regexp . ,shell-prompt-regexp)
    (:client-maker . ,client-maker)                             ;; function
    (:needs-authentication . ,needs-authentication)
    (:authenticate-request-maker . ,authenticate-request-maker) ;; function
    (:default-model-id . ,default-model-id)                     ;; function
    (:default-session-mode-id . ,default-session-mode-id)       ;; function
    (:icon-name . ,icon-name)
    (:install-instructions . ,install-instructions)))

(defun agent-shell--resolve-path (path)
  "Resolve PATH using `agent-shell-path-resolver-function'."
  (funcall (or agent-shell-path-resolver-function #'identity) path))

(defun agent-shell--indent-string (n str)
  "Indent STR lines by N spaces."
  (mapconcat (lambda (line)
               (concat (make-string n ?\s) line))
             (split-string str "\n")
             "\n"))

(defun agent-shell--interpolate-gradient (colors progress)
  "Interpolate between gradient COLORS based on PROGRESS (0.0 to 1.0)."
  (let* ((segments (1- (length colors)))
         (segment-size (/ 1.0 segments))
         (segment (min (floor (/ progress segment-size)) (1- segments)))
         (local-progress (/ (- progress (* segment segment-size)) segment-size))
         (from-color (nth segment colors))
         (to-color (nth (1+ segment) colors)))
    (agent-shell--mix-colors from-color to-color local-progress)))

(defun agent-shell--mix-colors (color1 color2 ratio)
  "Mix two hex colors by RATIO (0.0 = COLOR1, 1.0 = COLOR2)."
  (let* ((r1 (string-to-number (substring color1 1 3) 16))
         (g1 (string-to-number (substring color1 3 5) 16))
         (b1 (string-to-number (substring color1 5 7) 16))
         (r2 (string-to-number (substring color2 1 3) 16))
         (g2 (string-to-number (substring color2 3 5) 16))
         (b2 (string-to-number (substring color2 5 7) 16))
         (r (round (+ (* r1 (- 1 ratio)) (* r2 ratio))))
         (g (round (+ (* g1 (- 1 ratio)) (* g2 ratio))))
         (b (round (+ (* b1 (- 1 ratio)) (* b2 ratio)))))
    (format "#%02x%02x%02x" r g b)))

(cl-defun agent-shell--align-alist (&key data columns (separator "  ") joiner)
  "Align COLUMNS from DATA.

DATA is a list of alists.  COLUMNS is a list of extractor functions,
where each extractor takes one alist and returns a string for that
column.  SEPARATOR is the string used to join columns (defaults to
two spaces).  JOINER, when provided, wraps the result with
`string-join' using JOINER as the separator.

Returns a list of strings with spaced-aligned columns, or a single
joined string if JOINER is provided."
  (let* ((rows (mapcar
                (lambda (item)
                  (mapcar (lambda (extractor) (funcall extractor item))
                          columns))
                data))
         (widths (seq-reduce
                  (lambda (acc row)
                    (seq-mapn #'max
                              acc
                              (mapcar (lambda (cell) (length (or cell ""))) row)))
                  rows
                  (make-list (length columns) 0)))
         (result (mapcar (lambda (row)
                           (string-trim-right
                            (string-join
                             (seq-mapn (lambda (cell width)
                                         (format (format "%%-%ds" width) (or cell "")))
                                       row
                                       widths)
                             separator)))
                         rows)))
    (if joiner
        (string-join result joiner)
      result)))

(cl-defun agent-shell--make-diff-info (&key tool-call)
  "Make diff information from TOOL-CALL.

TOOL-CALL is an ACP tool call object that may contain diff info in
either `content' (standard ACP format) or `rawInput' (eg.  Copilot).

Standard ACP format uses content with type "diff" containing
oldText/newText/path fields.

See https://agentclientprotocol.com/protocol/schema#toolcallcontent

Copilot sends old_str/new_str/path in rawInput instead.

See https://github.com/xenodium/agent-shell/issues/217

Returns in the form:

 `((:old . old-text)
   (:new . new-text)
   (:file . file-path))."
  (let ((content (map-elt tool-call 'content))
        (raw-input (map-elt tool-call 'rawInput)))
    (when-let* ((diff-item (cond
                            ;; Single diff object
                            ((and content (equal (map-elt content 'type) "diff"))
                             content)
                            ;; TODO: Is this needed?
                            ;; Isn't content always an alist?
                            ;; Vector/array content - find diff item
                            ((vectorp content)
                             (seq-find (lambda (item)
                                         (equal (map-elt item 'type) "diff"))
                                       content))
                            ;; TODO: Is this needed?
                            ;; Isn't content always an alist?
                            ;; List content - find diff item
                            ((and content (listp content))
                             (seq-find (lambda (item)
                                         (equal (map-elt item 'type) "diff"))
                                       content))
                            ;; Attempt to get from rawInput.
                            ((and raw-input (map-elt raw-input 'new_str))
                             `((oldText . ,(or (map-elt raw-input 'old_str) ""))
                               (newText . ,(map-elt raw-input 'new_str))
                               (path . ,(map-elt raw-input 'path))))
                            ;; Attempt diff from rawInput (eg. Copilot).
                            ((and raw-input (map-elt raw-input 'diff))
                             (let ((parsed (agent-shell--parse-unified-diff
                                            (map-elt raw-input 'diff))))
                               `((oldText . ,(car parsed))
                                 (newText . ,(cdr parsed))
                                 (path . ,(or (map-elt raw-input 'fileName)
                                              (map-elt raw-input 'path))))))))
                ;; oldText can be nil for Write tools creating new files, default to ""
                ;; TODO: Currently don't have a way to capture overwrites
                (old-text (or (map-elt diff-item 'oldText) ""))
                (new-text (map-elt diff-item 'newText))
                (file-path (map-elt diff-item 'path)))
      (append (list (cons :old old-text)
                    (cons :new new-text))
              (when file-path
                (list (cons :file file-path)))))))

;; Based on https://github.com/editor-code-assistant/eca-emacs/blob/298849d1aae3241bf8828b6558c6deb45d75a3c8/eca-diff.el#L22
(defun agent-shell--parse-unified-diff (diff-string)
  "Parse unified DIFF-STRING into old and new text.
Returns a cons cell (OLD-TEXT . NEW-TEXT)."
  (let (old-lines new-lines in-hunk)
    (dolist (line (split-string diff-string "\n"))
      (cond
       ((string-match "^@@.*@@" line)
        (setq in-hunk t))
       ((and in-hunk (string-prefix-p " " line))
        (push (substring line 1) old-lines)
        (push (substring line 1) new-lines))
       ((and in-hunk (string-prefix-p "-" line))
        (push (substring line 1) old-lines))
       ((and in-hunk (string-prefix-p "+" line))
        (push (substring line 1) new-lines))))
    (cons (string-join (nreverse old-lines) "\n")
          (string-join (nreverse new-lines) "\n"))))

(defun agent-shell--format-diff-as-text (diff)
  "Format DIFF info as text suitable for display in tool call body.

DIFF should be in the form returned by `agent-shell--make-diff-info':
  ((:old . old-text) (:new . new-text) (:file . file-path))"
  (when-let (diff
             (old-file (make-temp-file "old"))
             (new-file (make-temp-file "new")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert (map-elt diff :old)))
          (with-temp-file new-file (insert (map-elt diff :new)))
          (with-temp-buffer
            (call-process "diff" nil t nil "-U3" old-file new-file)
            ;; Remove file header lines with timestamps
            (goto-char (point-min))
            (when (looking-at "^---")
              (delete-region (point) (progn (forward-line 1) (point))))
            (when (looking-at "^\\+\\+\\+")
              (delete-region (point) (progn (forward-line 1) (point))))
            ;; Apply diff syntax highlighting
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line-start (point))
                    (line-end (line-end-position)))
                (cond
                 ;; Removed lines (start with -)
                 ((looking-at "^-")
                  (add-text-properties line-start line-end
                                       '(font-lock-face diff-removed)))
                 ;; Added lines (start with +)
                 ((looking-at "^\\+")
                  (add-text-properties line-start line-end
                                       '(font-lock-face diff-added)))
                 ;; Hunk headers (@@)
                 ((looking-at "^@@")
                  (add-text-properties line-start line-end
                                       '(font-lock-face diff-hunk-header))))
                (forward-line 1)))
            (buffer-string)))
      (delete-file old-file)
      (delete-file new-file))))

(defun agent-shell--get-available-modes (state)
  "Get available modes list, preferring session modes over agent modes.

STATE is the agent shell state.

Returns the modes list from session if available, otherwise from
the agent's available modes."
  (or (map-nested-elt state '(:session :modes))
      ;; Use agent-level availability as fallback.
      (map-nested-elt state '(:available-modes :modes))))

(defun agent-shell--resolve-session-mode-name (mode-id available-session-modes)
  "Get the name of the session mode with MODE-ID from AVAILABLE-SESSION-MODES.

AVAILABLE-SESSION-MODES is the list of mode objects from the ACP
session/new response.  Each mode has an `:id' and `:name' field.
We look up the mode by ID to get its display name.

See https://agentclientprotocol.com/protocol/session-modes for details."
  (when-let ((mode (seq-find (lambda (m)
                               (string= mode-id (map-elt m :id)))
                             available-session-modes)))
    (map-elt mode :name)))

(defun agent-shell--tool-call-command-to-string (command)
  "Normalize tool call COMMAND to a display string.

COMMAND, when present, may be a shell command string or an argv vector."
  (cond ((stringp command) command)
        ((vectorp command)
         (combine-and-quote-strings (append command nil)))
        ((null command) nil)
        (t (error "Unexpected tool-call command type: %S" (type-of command)))))

(provide 'agent-shell-core)

;;; agent-shell-core.el ends here
