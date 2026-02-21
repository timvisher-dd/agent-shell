;;; agent-shell.el --- Native agentic integrations for Claude Code, Gemini CLI, etc  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell
;; Version: 0.40.1
;; Package-Requires: ((emacs "29.1") (shell-maker "0.85.1") (acp "0.11.1"))

(defconst agent-shell--version "0.40.1")

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
;; `agent-shell' offers a native `comint' shell experience to
;; interact with any agent powered by ACP (Agent Client Protocol).
;;
;; `agent-shell' currently provides access to Claude Code, Cursor,
;; Gemini CLI, Goose, Codex, OpenCode, Qwen, and Auggie amongst other agents.
;;
;; This package depends on the `acp' package to provide the ACP layer
;; as per https://agentclientprotocol.com spec.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'acp)
(eval-when-compile
  (require 'cl-lib))
(require 'dired)
(require 'json)
(require 'map)
(require 'agent-shell-core)
(require 'agent-shell-ui-helpers)

;; Optional Flycheck APIs used for error context.
(declare-function flycheck-overlay-errors-at "flycheck" (pos))
(declare-function flycheck-error-pos "flycheck" (err))
(declare-function flycheck-error-end-line "flycheck" (err))
(declare-function flycheck-error-end-column "flycheck" (err))
(declare-function flycheck-error-level "flycheck" (err))
(declare-function flycheck-error-message "flycheck" (err))
(declare-function flycheck-error-line "flycheck" (err))
(declare-function flycheck-error-column "flycheck" (err))
(unless (require 'markdown-overlays nil 'noerror)
  (error "Please update 'shell-maker' to v0.85.1 or newer"))
(require 'agent-shell-anthropic)
(require 'agent-shell-auggie)
(require 'agent-shell-completion)
(require 'agent-shell-cursor)
(require 'agent-shell-diff)
(require 'agent-shell-droid)
(require 'agent-shell-github)
(require 'agent-shell-google)
(require 'agent-shell-goose)
(require 'agent-shell-heartbeat)
(require 'agent-shell-active-message)
(require 'agent-shell-mistral)
(require 'agent-shell-meta)
(require 'agent-shell-openai)
(require 'agent-shell-opencode)
(require 'agent-shell-pi)
(require 'agent-shell-project)
(require 'agent-shell-qwen)
(require 'agent-shell-terminal)
(require 'agent-shell-tools)
(require 'agent-shell-usage)
(require 'agent-shell-worktree)
(require 'agent-shell-ui)
(require 'agent-shell-viewport)
(require 'image)
(require 'markdown-overlays)
(require 'shell-maker)
(require 'svg nil :noerror)
(require 'transient)

(declare-function flycheck-overlay-errors-at "flycheck")
(declare-function flycheck-error-pos "flycheck")
(declare-function flycheck-error-end-line "flycheck")
(declare-function flycheck-error-end-column "flycheck")
(declare-function flycheck-error-level "flycheck")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-error-line "flycheck")
(declare-function flycheck-error-column "flycheck")

;; Declare as special so byte-compilation doesn't turn `let' bindings into
;; lexical bindings (which would not affect `auto-insert' behavior).
(defvar auto-insert)

(defcustom agent-shell-show-config-icons t
  "Whether to show icons in agent config selection."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-context-sources '(files region error line)
  "Sources to consider when determining \\<agent-shell-mode-map>\\[agent-shell] automatic context.

Each element can be:
- A symbol: `files', `region', `error', or `line'
- A function: Called with no arguments, should return context or nil

Sources are checked in order until one returns non-nil."
  :type '(repeat (choice (const :tag "Buffer files" files)
                         (const :tag "Selected region" region)
                         (const :tag "Error at point" error)
                         (const :tag "Current line" line)
                         (function :tag "Custom function")))
  :group 'agent-shell)

(defcustom agent-shell-text-file-capabilities t
  "Whether agents are initialized with read/write text file capabilities.

See `acp-make-initialize-request' for details."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-write-inhibit-minor-modes '(aggressive-indent-mode)
  "List of minor mode commands to inhibit during `fs/write_text_file' edits.

Each element is a minor mode command symbol, such as
`aggressive-indent-mode'.

Agent Shell disables any listed modes that are enabled in the target
buffer before applying `fs/write_text_file' edits, and then restores
them.

Modes whose variables are not buffer-local in the target buffer (for
example, globalized minor modes) are ignored."
  :type '(repeat symbol)
  :group 'agent-shell)

(defcustom agent-shell-display-action
  '(display-buffer-same-window)
  "Display action for agent shell buffers.
See `display-buffer' for the format of display actions."
  :type '(cons (repeat function) alist)
  :group 'agent-shell)

(defcustom agent-shell-prefer-viewport-interaction nil
  "Non-nil makes `agent-shell' prefer viewport interaction over shell interaction.

For example, `agent-shell-send*' will insert text into the viewport
buffer instead of the shell buffer.  If no viewport buffer exists, one
will be created."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-embed-file-size-limit 102400
  "Maximum file size in bytes for embedding with ContentBlock::Resource.
Files larger than this will use ContentBlock::ResourceLink instead.
Default is 100KB (102400 bytes)."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-header-style (if (display-graphic-p) 'graphical 'text)
  "Style for agent shell buffer headers.

Can be one of:

 \='graphical: Display header with icon and styled text.
 \='text: Display simple text-only header.
 nil: Display no header."
  :type '(choice (const :tag "Graphical" graphical)
                 (const :tag "Text only" text)
                 (const :tag "No header" nil))
  :group 'agent-shell)

(defcustom agent-shell-show-welcome-message t
  "Non-nil to show welcome message."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-show-busy-indicator t
  "Non-nil to show the busy indicator animation in the header and mode line."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-busy-indicator-frames 'wide
  "Frames for the busy indicator animation.
Can be a symbol selecting a predefined style, or a list of frame strings.
When providing custom frames, do not include leading spaces as padding
is added automatically."
  :type '(choice (const :tag "Wave (pulses up and down)" wave)
                 (const :tag "Dots Block (circular spin)" dots-block)
                 (const :tag "Dots Round (circular spin)" dots-round)
                 (const :tag "Wide (horizontal blocks)" wide)
                 (repeat :tag "Custom frames" string))
  :group 'agent-shell)

(defcustom agent-shell-screenshot-command
  (if (eq system-type 'darwin)
      '("/usr/sbin/screencapture" "-i")
    ;; ImageMagick is common on Linux and many other *nix systems.
    '("/usr/bin/import"))
  "The program to use for capturing screenshots.

Assume screenshot file path will be appended to this list."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-clipboard-image-handlers
  (list
   (list (cons :command "pngpaste")
         (cons :save (lambda (file-path)
                       (let ((exit-code (call-process "pngpaste" nil nil nil file-path)))
                         (unless (zerop exit-code)
                           (error "Command pngpaste failed with exit code %d" exit-code))))))
   (list (cons :command "xclip")
         (cons :save (lambda (file-path)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (let ((exit-code (call-process "xclip" nil t nil
                                                        "-selection" "clipboard"
                                                        "-t" "image/png" "-o")))
                           (unless (zerop exit-code)
                             (error "Command xclip failed with exit code %d" exit-code))
                           (write-region (point-min) (point-max) file-path nil 'silent)))))))
  "Handlers for saving clipboard images to a file.

Each handler is an alist with the following keys:

  :command  The executable name to look up via `executable-find'.
  :save     A function taking FILE-PATH that saves the clipboard
            image there, signaling an error on failure.

Handlers are tried in order.  The first whose :command is found
on the system is used."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'agent-shell)

(defcustom agent-shell-buffer-name-format 'default
  "Format to use when generating agent shell buffer names.

Each element can be:
- Default: For example \='Claude Code Agent @ My Project\='
- Kebab case: For example \='claude-code-agent @ my-project\='
- A function: Called with agent name and project name."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Kebab case" kebab-case)
                 (function :tag "Custom format"))
  :group 'agent-shell)

(defun agent-shell--make-default-agent-configs ()
  "Create a list of default agent configs.

This function aggregates agents from OpenAI, Anthropic, Google,
Goose, Cursor, Auggie, and others."
  (list (agent-shell-auggie-make-agent-config)
        (agent-shell-anthropic-make-claude-code-config)
        (agent-shell-openai-make-codex-config)
        (agent-shell-cursor-make-agent-config)
        (agent-shell-droid-make-agent-config)
        (agent-shell-github-make-copilot-config)
        (agent-shell-google-make-gemini-config)
        (agent-shell-goose-make-agent-config)
        (agent-shell-mistral-make-config)
        (agent-shell-opencode-make-agent-config)
        (agent-shell-pi-make-agent-config)
        (agent-shell-qwen-make-agent-config)))

(defcustom agent-shell-agent-configs
  (agent-shell--make-default-agent-configs)
  "The list of known agent configurations.

See `agent-shell-*-make-*-config' for details."
  :type '(repeat (alist :key-type symbol :value-type sexp))
  :group 'agent-shell)

(defcustom agent-shell-preferred-agent-config nil
  "Default agent to use for all new shells.

If this is set, `agent-shell' will unconditionally use this
agent and not prompt you to select one.

Can be set to a symbol identifier (e.g., `claude-code') or a full
configuration alist for backwards compatibility."
  :type '(choice (const :tag "None (prompt each time)" nil)
                 (const :tag "Auggie" auggie)
                 (const :tag "Claude Code" claude-code)
                 (const :tag "Codex" codex)
                 (const :tag "Copilot" copilot)
                 (const :tag "Cursor" cursor)
                 (const :tag "Droid" droid)
                 (const :tag "Gemini CLI" gemini-cli)
                 (const :tag "Goose" goose)
                 (const :tag "Mistral" le-chat)
                 (const :tag "OpenCode" opencode)
                 (const :tag "Pi" pi)
                 (const :tag "Qwen Code" qwen-code)
                 (symbol :tag "Custom identifier")
                 (alist :tag "Full configuration (legacy)"
                        :key-type symbol :value-type sexp))
  :group 'agent-shell)

(defcustom agent-shell-prefer-session-resume t
  "Prefer ACP session resume over session load when both are available.

When non-nil (and supported by agent), prefer ACP session resumes over loading."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-session-strategy 'new-deferred
  "How to handle sessions when starting a new shell.

Available values:

  `new-deferred': Start a new session, but defer initialization until the
                  first prompt is submitted.
  `new': Always start a new session.
  `latest': Always load/resume the latest session.
  `prompt': Always prompt to choose a session (or start a new one)."
  :type '(choice (const :tag "New session, deferred init" new-deferred)
                 (const :tag "Always start new session" new)
                 (const :tag "Load latest session" latest)
                 (const :tag "Prompt for session" prompt))
  :group 'agent-shell)

(defun agent-shell--resolve-preferred-config ()
  "Resolve `agent-shell-preferred-agent-config' to a full configuration.

If the value is a symbol, look it up in `agent-shell-agent-configs'.
If it's already an alist (legacy format), return it as-is.
Returns nil if no matching configuration is found."
  (cond
   ((null agent-shell-preferred-agent-config) nil)
   ((symbolp agent-shell-preferred-agent-config)
    (seq-find (lambda (config)
                (eq (map-elt config :identifier)
                    agent-shell-preferred-agent-config))
              agent-shell-agent-configs))
   ((listp agent-shell-preferred-agent-config)
    agent-shell-preferred-agent-config)))

(defcustom agent-shell-mcp-servers nil
  "List of MCP servers to initialize when creating a new session.

Each element should be an alist representing an MCP server configuration
following the ACP schema for McpServer as defined at:

https://agentclientprotocol.com/protocol/schema#mcpserver

The schema supports three transport variants:

1. Stdio Transport (universally supported):
   ((name . \"server-name\")
    (command . \"/path/to/executable\")
    (args . (\"arg1\" \"arg2\"))
    (env . (((name . \"ENV_VAR\") (value . \"value\")))))

2. HTTP Transport (requires mcpCapabilities.http):
   ((name . \"server-name\")
    (type . \"http\")
    (url . \"https://example.com/mcp\")
    (headers . (((name . \"Authorization\") (value . \"Bearer token\")))))

3. SSE Transport (requires mcpCapabilities.sse):
   ((name . \"server-name\")
    (type . \"sse\")
    (url . \"https://example.com/mcp\")
    (headers . (((name . \"Authorization\") (value . \"Bearer token\")))))

Example configuration with multiple servers:

  (setq agent-shell-mcp-servers
        \='(((name . \"notion\")
           (type . \"http\")
           (url . \"https://mcp.notion.com/mcp\")
           (headers . ()))
          ((name . \"filesystem\")
           (command . \"npx\")
           (args . (\"-y\"
                    \"@modelcontextprotocol/server-filesystem\" \"/tmp\"))
           (env . ()))))

Lambdas can be used anywhere in the configuration hierarchy for dynamic
evaluation at session startup time.  This is useful for values that
depend on runtime context like the current working directory
\(`agent-shell-cwd').  Note: only lambdas are evaluated, not named
functions, to avoid accidentally calling external symbols.

For example, using the `claude-code-ide' package (see its documentation
for more details), you can embed a lambda for the URL that registers
the session and returns the appropriate endpoint:

  (setq agent-shell-mcp-servers
        \='(((name . \"emacs\")
           (type . \"http\")
           (headers . ())
           (url . (lambda ()
                    (require \='claude-code-ide-mcp-server)
                    (let* ((project-dir (agent-shell-cwd))
                           (session-id (format \"agent-shell-%s-%s\"
                                         (file-name-nondirectory
                                           (directory-file-name project-dir))
                                         (format-time-string \"%Y%m%d-%H%M%S\"))))
                      (puthash session-id `(:project-dir ,project-dir)
                               claude-code-ide-mcp-server--sessions)
                      (format \"http://localhost:%d/mcp/%s\"
                              (claude-code-ide-mcp-server-ensure-server)
                              session-id)))))))"
  :type '(repeat (choice (alist :key-type symbol :value-type sexp) function))
  :group 'agent-shell)

(defvar agent-shell--shell-maker-config nil)

;;;###autoload
(defun agent-shell (&optional arg)
  "Start or reuse an existing agent shell.

`agent-shell' carries some DWIM (do what I mean) behaviour.

If in a project without a shell, offer to create one.

If already in a shell, invoke `agent-shell-toggle'.

If a region is active or point is on relevant context (ie.
`dired' files or image buffers), carry them over to the
shell input.

See `agent-shell-context-sources' on how to control DWIM
behaviour.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick an existing shell."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (agent-shell--dwim :switch-to-shell t))
   ((equal arg '(4))
    (agent-shell--dwim :new-shell t))
   (t
    (agent-shell--dwim))))

(cl-defun agent-shell--dwim (&key config new-shell switch-to-shell)
  "Start or reuse an agent shell with DWIM behavior.

CONFIG is the agent configuration to use.
NEW-SHELL when non-nil forces starting a new shell.
SWITCH-TO-SHELL when non-nil prompts to pick an existing shell.

NEW-SHELL and SWITCH-TO-SHELL are mutually exclusive.

This function respects `agent-shell-prefer-viewport-interaction' and
handles viewport mode detection, existing shell reuse, and project context."
  (when (and new-shell switch-to-shell)
    (error ":new-shell and :switch-to-shell are mutually exclusive"))
  (if agent-shell-prefer-viewport-interaction
      (if (and (not new-shell)
               (or (derived-mode-p 'agent-shell-viewport-view-mode)
                   (derived-mode-p 'agent-shell-viewport-edit-mode)))
          (agent-shell-toggle)
        (let ((shell-buffer
               (cond (switch-to-shell
                      (completing-read "Switch to shell: "
                                       (mapcar #'buffer-name (or (agent-shell-buffers)
                                                                 (user-error "No shells available")))
                                       nil t))
                     (new-shell
                      (agent-shell--start :config (or config
                                                      (agent-shell--resolve-preferred-config)
                                                      (agent-shell-select-config
                                                       :prompt "Start new agent: ")
                                                      (error "No agent config found"))
                                          :no-focus t
                                          :new-session t))
                     (t
                      (agent-shell--shell-buffer)))))
          (if (and new-shell
                   (eq agent-shell-session-strategy 'prompt))
              ;; Defer viewport display until session is selected.
              (agent-shell-subscribe-to
               :shell-buffer shell-buffer
               :event 'session-selected
               :on-event (lambda (_event)
                           (agent-shell-viewport--show-buffer
                            :shell-buffer shell-buffer)))
            (agent-shell-viewport--show-buffer
             :shell-buffer shell-buffer))))
    (cond (switch-to-shell
           (let* ((shell-buffer
                   (completing-read "Switch to shell: "
                                    (mapcar #'buffer-name (or (agent-shell-buffers)
                                                              (user-error "No shells available")))
                                    nil t))
                  (text (agent-shell--context :shell-buffer shell-buffer)))
             (agent-shell--display-buffer shell-buffer)
             (when text
               (agent-shell--insert-to-shell-buffer :text text
                                                    :shell-buffer shell-buffer))))
          (new-shell
           (agent-shell-start :config (or config
                                          (agent-shell--resolve-preferred-config)
                                          (agent-shell-select-config
                                           :prompt "Start new agent: ")
                                          (error "No agent config found"))))
          (t
           (if (derived-mode-p 'agent-shell-mode)
               (let* ((shell-buffer (agent-shell--shell-buffer :no-create t))
                      (text (agent-shell--context :shell-buffer shell-buffer)))
                 (agent-shell-toggle)
                 (when text
                   (agent-shell--insert-to-shell-buffer :text text
                                                        :shell-buffer shell-buffer)))
             (let* ((shell-buffer (agent-shell--shell-buffer))
                    (text (agent-shell--context :shell-buffer shell-buffer)))
               (agent-shell--display-buffer shell-buffer)
               (when text
                 (agent-shell--insert-to-shell-buffer :text text
                                                      :shell-buffer shell-buffer))))))))

;;;###autoload
(defun agent-shell-toggle ()
  "Toggle agent shell display."
  (interactive)
  (let ((shell-buffer (if agent-shell-prefer-viewport-interaction
                          (agent-shell-viewport--buffer)
                        (or (agent-shell--current-shell)
                            (seq-first (agent-shell-project-buffers))
                            (seq-first (agent-shell-buffers))))))
    (unless shell-buffer
      (user-error "No agent shell buffers available for current project"))
    (if-let ((window (get-buffer-window shell-buffer)))
        (if (and (> (count-windows) 1)
                 (not (bound-and-true-p transient--prefix)))
            (delete-window window)
          (switch-to-prev-buffer))
      (agent-shell--display-buffer shell-buffer))))

;;;###autoload
(defun agent-shell-new-shell ()
  "Start a new agent shell.

Always prompts for agent selection, even if existing shells are available."
  (interactive)
  (agent-shell '(4)))

;;;###autoload
(defun agent-shell-prompt-compose ()
  "Compose an `agent-shell' prompt in a dedicated buffer.

If currently visiting an `agent-shell', transfer latest input."
  (interactive)
  (if-let (((derived-mode-p 'agent-shell-mode))
           ((shell-maker-point-at-last-prompt-p))
           (input (agent-shell--input)))
      (progn
        ;; Clear shell prompt as it's now
        ;; transferred to the compose buffer.
        (comint-kill-input)
        (agent-shell-viewport--show-buffer :override input))
    (agent-shell-viewport--show-buffer)))

(cl-defun agent-shell-start (&key config outgoing-request-decorator)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format.

OUTGOING-REQUEST-DECORATOR is an optional function passed through to
`acp-make-client'.  See its docstring for details."
  (agent-shell--start :config config
                      :no-focus nil
                      :new-session t
                      :outgoing-request-decorator outgoing-request-decorator))

(cl-defun agent-shell--config-icon (&key config)
  "Create icon string for CONFIG if available and icons are enabled.
Returns an empty string if no icon should be displayed."
  (if-let* ((graphics-capable (display-graphic-p))
            (icon-filename (if (map-elt config :icon-name)
                               (agent-shell--fetch-agent-icon
                                (map-elt config :icon-name))
                             (agent-shell--make-agent-fallback-icon
                              (map-elt config :buffer-name) 100))))
      (with-temp-buffer
        (insert-image (create-image icon-filename nil nil
                                    :ascent 'center
                                    :height (frame-char-height)))
        (buffer-string))
    ""))

(cl-defun agent-shell-select-config (&key prompt)
  "Display PROMPT to select an agent config from `agent-shell-agent-configs'."
  (let* ((configs agent-shell-agent-configs)
         (choices (mapcar (lambda (config)
                            (let ((display-name (or (map-elt config :mode-line-name)
                                                    (map-elt config :buffer-name)
                                                    "Unknown Agent"))
                                  (icon (when agent-shell-show-config-icons
                                          (agent-shell--config-icon :config config))))
                              (cons (concat icon (when icon " ") display-name)
                                    config)))
                          configs))
         (selected-name (completing-read (or prompt "Select agent: ") choices nil t)))
    (map-elt choices selected-name)))

(defun agent-shell-buffers ()
  "Return all shell buffers ordered by recent access.
Includes shells accessed via viewport buffers, preserving visited order."
  (let (shell-buffers seen)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let ((shell-buffer
                    (cond ((derived-mode-p 'agent-shell-mode)
                           buffer)
                          ((or (derived-mode-p 'agent-shell-viewport-view-mode)
                               (derived-mode-p 'agent-shell-viewport-edit-mode))
                           (agent-shell-viewport--shell-buffer buffer)))))
          (unless (memq shell-buffer seen)
            (push shell-buffer seen)
            (push shell-buffer shell-buffers)))))
    (nreverse shell-buffers)))

(defun agent-shell-other-buffer ()
  "Switch to other associated buffer (viewport vs shell)."
  (interactive)
  (cond ((or (derived-mode-p 'agent-shell-viewport-view-mode)
             (derived-mode-p 'agent-shell-viewport-edit-mode))
         (switch-to-buffer (or (agent-shell--shell-buffer
                                :viewport-buffer (current-buffer)
                                :no-create t)
                               "No shell available")))
        ((derived-mode-p 'agent-shell-mode)
         (when-let ((viewport-buffer (or (agent-shell-viewport--buffer
                                          :shell-buffer (current-buffer))
                                         "Not in a shell viewport buffer")))
           (with-current-buffer viewport-buffer
             (when (derived-mode-p 'agent-shell-viewport-view-mode)
               (agent-shell-viewport-refresh)))
           (switch-to-buffer viewport-buffer)))
        (t
         (user-error "Not in an agent-shell buffer"))))

(defun agent-shell-version ()
  "Show `agent-shell' mode version."
  (interactive)
  (message "agent-shell v%s" agent-shell--version))

(defun agent-shell-interrupt (&optional force)
  "Interrupt in-progress request and reject all pending permissions.
When FORCE is non-nil, skip confirmation prompt."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond ((map-nested-elt (agent-shell--state) '(:session :id))
         (when (or force (y-or-n-p "Interrupt?"))
           ;; First cancel all pending permission requests
           (map-do
            (lambda (tool-call-id tool-call-data)
              (when (map-elt tool-call-data :permission-request-id)
                (agent-shell--send-permission-response
                 :client (map-elt (agent-shell--state) :client)
                 :request-id (map-elt tool-call-data :permission-request-id)
                 :cancelled t
                 :state (agent-shell--state)
                 :tool-call-id tool-call-id)))
            (map-elt (agent-shell--state) :tool-calls))
           ;; Then send the cancel notification
           (acp-send-notification
            :client (map-elt (agent-shell--state) :client)
            :notification (acp-make-session-cancel-notification
                           :session-id (map-nested-elt (agent-shell--state) '(:session :id))
                           :reason "User cancelled"))
           ;; Reflect cancellation in tool call UI.
           (agent-shell--mark-tool-calls-cancelled (agent-shell--state))))
        (t
         (agent-shell--shutdown)
         (call-interactively #'shell-maker-interrupt))))

(cl-defun agent-shell--make-shell-maker-config (&key prompt prompt-regexp)
  "Create `shell-maker' configuration with PROMPT and PROMPT-REGEXP."
  (make-shell-maker-config
   :name "agent"
   :prompt prompt
   :prompt-regexp prompt-regexp
   :execute-command
   (lambda (command shell)
     (agent-shell--handle
      :command command
      :shell-buffer (map-elt shell :buffer)))))

(defun agent-shell--filter-buffer-substring (start end &optional delete)
  "Return the buffer substring between START and END, after filtering.
Strip the text properties `line-prefix' and `wrap-prefix' from the
copied substring.  If DELETE is non-nil, delete the text between START
and END from the buffer."  (let ((text (if delete
                  (prog1 (buffer-substring start end)
                    (delete-region start end))
                (buffer-substring start end))))
    (remove-text-properties 0 (length text)
                            '(line-prefix nil wrap-prefix nil)
                            text)
    text))

(defun agent-shell--prompt-undo-enabled-p ()
  "Return non-nil when undo should be enabled for prompt entry."
  (and (derived-mode-p 'agent-shell-mode)
       (not (shell-maker-busy))))

(defun agent-shell--set-undo-enabled (enabled)
  "Enable undo recording for the current buffer when ENABLED is non-nil."
  (if enabled
      (when (eq buffer-undo-list t)
        (setq-local buffer-undo-list nil))
    (when (not (eq buffer-undo-list t))
      (setq-local buffer-undo-list t))))

(defun agent-shell--refresh-undo-state ()
  "Enable undo only while editing the active prompt."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell--set-undo-enabled (agent-shell--prompt-undo-enabled-p))))

(defun agent-shell--disable-undo-after-submit (&rest _args)
  "Disable undo after prompt submission."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell--set-undo-enabled nil)))

(advice-add 'shell-maker-submit :after #'agent-shell--disable-undo-after-submit)
(defvar-keymap agent-shell-mode-map
  :parent shell-maker-mode-map
  :doc "Keymap for `agent-shell-mode'."
  "TAB" #'agent-shell-next-item
  "<backtab>" #'agent-shell-previous-item
  "n" #'agent-shell-next-item
  "p" #'agent-shell-previous-item
  "C-<tab>" #'agent-shell-cycle-session-mode
  "C-c C-c" #'agent-shell-interrupt
  "C-c C-m" #'agent-shell-set-session-mode
  "C-c C-v" #'agent-shell-set-session-model
  "C-c C-o" #'agent-shell-other-buffer
  "<remap> <yank>" #'agent-shell-yank-dwim)

(shell-maker-define-major-mode (agent-shell--make-shell-maker-config) 'agent-shell-mode-map)

(cl-defun agent-shell--handle (&key command shell-buffer)
  "Handle SHELL-BUFFER COMMAND (and lazy initialize the ACP stack).

SHELL-BUFFER is the shell buffer.

Flow:

  Before a shell COMMAND can be sent as a prompt to the agent, a
  handful of ACP initialization steps must take place (some asynchronously).
  Once all initialization steps are cleared, only then the COMMAND
  can be sent to the agent as a prompt (thus recursive nature of this function).

  -> Initialize ACP client
      |-> Subscribe to ACP events
           |-> Initiate handshake (ie.  initialize RPC)
                |-> Authenticate (optional)
                     |-> Start prompt session
                          |-> Send COMMAND/prompt (finally!)"
  (with-current-buffer shell-buffer
    (unless (derived-mode-p 'agent-shell-mode)
      (error "Not in a shell"))
    (when (and command
               (not (eq agent-shell-session-strategy 'new-deferred))
               (not (map-nested-elt (agent-shell--state) '(:session :id))))
      (user-error "Session not ready... please wait"))
    (map-put! (agent-shell--state) :request-count
              ;; TODO: Make public in shell-maker.
              (shell-maker--current-request-id))
    (cond ((not (map-elt (agent-shell--state) :client))
           ;; Needs a client
           (agent-shell--emit-event :event 'init-started)
           (when (and agent-shell-show-busy-indicator
                      (not command))
             (agent-shell-heartbeat-start
              :heartbeat (map-elt agent-shell--state :heartbeat)))
           (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                        :shell-buffer shell-buffer
                                        :existing-only t)))
             (with-current-buffer viewport-buffer
               (agent-shell-viewport-view-mode)
               (agent-shell-viewport--initialize
                :prompt  command
                :response (agent-shell-viewport--response))))
           (when (agent-shell--initialize-client)
             (agent-shell--handle :command command :shell-buffer shell-buffer)))
          ;; Needs ACP subscriptions
          ((or (not (map-nested-elt (agent-shell--state) '(:client :request-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :notification-handlers)))
               (not (map-nested-elt (agent-shell--state) '(:client :error-handlers))))
           (when (agent-shell--initialize-subscriptions)
             (agent-shell--handle :command command :shell-buffer shell-buffer)))
          ;; Needs to send ACP initialize request
          ((not (map-elt (agent-shell--state) :initialized))
           (agent-shell--initiate-handshake
            :shell-buffer shell-buffer
            :on-initiated (lambda ()
                            (map-put! (agent-shell--state) :initialized t)
                            (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Needs to send ACP authenticate request (optional)
          ((and (map-elt (agent-shell--state) :needs-authentication)
                (not (map-elt (agent-shell--state) :authenticated)))
           (agent-shell--authenticate
            :shell-buffer shell-buffer
            :on-authenticated (lambda ()
                                (map-put! (agent-shell--state) :authenticated t)
                                (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Needs to send ACP new session request
          ((not (map-nested-elt (agent-shell--state) '(:session :id)))
           (agent-shell--initiate-session
            :shell-buffer shell-buffer
            :on-session-init (lambda ()
                               ;; Session is now initiated.
                               ;; Consider bootstrapping/handshake complete.
                               ;; Show shell prompt.
                               (unless command
                                 (agent-shell-heartbeat-stop
                                  :heartbeat (map-elt agent-shell--state :heartbeat))
                                 (when (seq-empty-p (map-elt (agent-shell--state) :available-commands))
                                   ;; Setting an "available commands" placeholder fragment before
                                   ;; displaying the prompt (shell-maker-finish-output).
                                   ;; This enables updating the placeholder even if the notification
                                   ;; arrives after bootstrapping prompt is displayed.
                                   (agent-shell--update-fragment
                                    :state (agent-shell--state)
                                    :namespace-id "bootstrapping"
                                    :block-id "available_commands_update"
                                    :label-left (propertize "Available /commands" 'font-lock-face 'font-lock-doc-markup-face)))
                                 (when (and (map-nested-elt (agent-shell--state) '(:agent-config :default-model-id))
                                            (funcall (map-nested-elt (agent-shell--state)
                                                                     '(:agent-config :default-model-id)))
                                            (not (map-elt (agent-shell--state) :set-model)))
                                   ;; Setting a "Setting model" placeholder fragment before
                                   ;; displaying the prompt (shell-maker-finish-output).
                                   ;; This enables updating the placeholder even if the response
                                   ;; arrives after bootstrapping prompt is displayed.
                                   (agent-shell--update-fragment
                                    :state (agent-shell--state)
                                    :namespace-id "bootstrapping"
                                    :block-id "set-model"
                                    :label-left (propertize "Setting model" 'font-lock-face 'font-lock-doc-markup-face)
                                    :body (format "Requesting %s..."
                                                  (funcall (map-nested-elt (agent-shell--state)
                                                                           '(:agent-config :default-model-id))))))
                                 (when (and (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id))
                                            (funcall (map-nested-elt (agent-shell--state)
                                                                     '(:agent-config :default-session-mode-id)))
                                            (not (map-elt (agent-shell--state) :set-session-mode)))
                                   ;; Setting a "Setting session mode" placeholder fragment before
                                   ;; displaying the prompt (shell-maker-finish-output).
                                   ;; This enables updating the placeholder even if the response
                                   ;; arrives after bootstrapping prompt is displayed.
                                   (agent-shell--update-fragment
                                    :state (agent-shell--state)
                                    :namespace-id "bootstrapping"
                                    :block-id "set-session-mode"
                                    :label-left (propertize "Setting session mode" 'font-lock-face 'font-lock-doc-markup-face)
                                    :body (format "Requesting %s..."
                                                  (funcall (map-nested-elt (agent-shell--state)
                                                                           '(:agent-config :default-session-mode-id))))))
                                 (shell-maker-finish-output :config shell-maker--config
                                                            :success nil)
                                 (agent-shell--emit-event :event 'prompt-ready))
                               (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Send ACP request to set default model (optional)
          ((and (map-nested-elt (agent-shell--state) '(:agent-config :default-model-id))
                (funcall (map-nested-elt (agent-shell--state)
                                         '(:agent-config :default-model-id)))
                (not (map-elt (agent-shell--state) :set-model)))
           (agent-shell--set-default-model
            :shell-buffer shell-buffer
            :model-id (funcall (map-nested-elt (agent-shell--state)
                                               '(:agent-config :default-model-id)))
            :on-model-changed (lambda ()
                                (map-put! (agent-shell--state) :set-model t)
                                (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Send ACP request to set default session mode (optional)
          ((and (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id))
                (funcall (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id)))
                (not (map-elt (agent-shell--state) :set-session-mode)))
           (agent-shell--set-default-session-mode
            :shell-buffer shell-buffer
            :mode-id (funcall (map-nested-elt (agent-shell--state) '(:agent-config :default-session-mode-id)))
            :on-mode-changed (lambda ()
                               (map-put! (agent-shell--state) :set-session-mode t)
                               (agent-shell--handle :command command :shell-buffer shell-buffer))))
          ;; Initialization complete
          (t
           (agent-shell--emit-event :event 'init-finished)
           ;; Send ACP prompt request
           (when (and command (not (string-empty-p (string-trim command))))
             (agent-shell--send-command :prompt command :shell-buffer shell-buffer))))))

(cl-defun agent-shell--on-error (&key state error)
  "Handle ERROR with SHELL an STATE."
  (let-alist error
    (agent-shell--update-fragment
     :state state
     :block-id "Error"
     :body (or .message "Some error ¯\\_ (ツ)_/¯")
     :create-new t
     :navigation 'never)))

(defun agent-shell-get-config (buffer)
  "Get the agent configuration for BUFFER.

Returns the agent configuration alist for the given buffer, or nil
if the buffer has no agent configuration.

This function is intended for use in `agent-shell-container-command-runner'
functions to access agent config properties like :identifier, :buffer-name, etc."
  (with-current-buffer buffer
    (map-elt agent-shell--state :agent-config)))

(cl-defun agent-shell--on-notification (&key state notification)
  "Handle incoming notification using SHELL, STATE, and NOTIFICATION."
  (let-alist notification
    (cond ((equal .method "session/update")
           (let ((update (map-elt (map-elt notification 'params) 'update)))
             (cond
              ((equal (map-elt update 'sessionUpdate) "tool_call")
               (let* ((content (map-elt update 'content))
                      (has-terminal (agent-shell--tool-call-terminal-ids content)))
                 (agent-shell--save-tool-call
                  state
                  (map-elt update 'toolCallId)
                  (append (list (cons :title (cond
                                              ((and (string= (map-elt update 'title) "Skill")
                                                    (map-nested-elt update '(rawInput command)))
                                               (format "Skill: %s"
                                                       (agent-shell--tool-call-command-to-string
                                                        (map-nested-elt update '(rawInput command)))))
                                              (t
                                               (map-elt update 'title))))
                                (cons :status (map-elt update 'status))
                                (cons :kind (map-elt update 'kind))
                                (cons :command (agent-shell--tool-call-command-to-string
                                                (map-nested-elt update '(rawInput command))))
                                (cons :description (map-nested-elt update '(rawInput description)))
                                (cons :content content)
                                (cons :raw-input (map-elt update 'rawInput))
                                (when has-terminal
                                  (cons :has-terminal t)))
                          (when-let ((diff (agent-shell--make-diff-info :tool-call update)))
                            (list (cons :diff diff)))))
                 (agent-shell--emit-event
                  :event 'tool-call-update
                  :data (list (cons :tool-call-id (map-elt update 'toolCallId))
                              (cons :tool-call (map-nested-elt state (list :tool-calls (map-elt update 'toolCallId))))))
                 (let ((tool-call-labels (agent-shell-make-tool-call-label
                                          state (map-elt update 'toolCallId))))
                   (agent-shell--update-fragment
                    :state state
                    :block-id (map-elt update 'toolCallId)
                    :label-left (map-elt tool-call-labels :status)
                    :label-right (map-elt tool-call-labels :title)
                    :expanded agent-shell-tool-use-expand-by-default)
                   ;; Display plan as markdown block if present
                   (when-let ((plan (map-nested-elt update '(rawInput plan))))
                     (agent-shell--update-fragment
                      :state state
                      :block-id (concat (map-elt update 'toolCallId) "-plan")
                      :label-left (propertize "Proposed plan" 'font-lock-face 'font-lock-doc-markup-face)
                      :body plan
                      :expanded t)))
                 (agent-shell--terminal-link-tool-call-content
                  state (map-elt update 'toolCallId) content)
                 (map-put! state :last-entry-type "tool_call")))
              ((equal (map-elt update 'sessionUpdate) "agent_thought_chunk")
               (let-alist update
                 ;; (message "agent_thought_chunk: last-type=%s, will-append=%s"
                 ;;          (map-elt state :last-entry-type)
                 ;;          (equal (map-elt state :last-entry-type) "agent_thought_chunk"))
                 (unless (equal (map-elt state :last-entry-type)
                                "agent_thought_chunk")
                   (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count))))
                 (agent-shell--update-fragment
                  :state state
                  :block-id (format "%s-agent_thought_chunk"
                                    (map-elt state :chunked-group-count))
                  :label-left  (concat
                                agent-shell-thought-process-icon
                                " "
                                (propertize "Thought process" 'font-lock-face font-lock-doc-markup-face))
                  :body .content.text
                  :append (equal (map-elt state :last-entry-type)
                                 "agent_thought_chunk")
                  :expanded agent-shell-thought-process-expand-by-default))
               (map-put! state :last-entry-type "agent_thought_chunk"))
              ((equal (map-elt update 'sessionUpdate) "agent_message_chunk")
               (unless (equal (map-elt state :last-entry-type) "agent_message_chunk")
                 (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                 (agent-shell--append-transcript
                  :text (format "## Agent (%s)\n\n" (format-time-string "%F %T"))
                  :file-path agent-shell--transcript-file))
               (let-alist update
                 (agent-shell--append-transcript
                  :text .content.text
                  :file-path agent-shell--transcript-file)
                 (agent-shell--update-fragment
                  :state state
                  :block-id (format "%s-agent_message_chunk"
                                    (map-elt state :chunked-group-count))
                  :body .content.text
                  :create-new (not (equal (map-elt state :last-entry-type)
                                          "agent_message_chunk"))
                  :append t
                  :navigation 'never))
               (map-put! state :last-entry-type "agent_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "user_message_chunk")
               (let ((new-prompt-p (not (equal (map-elt state :last-entry-type)
                                               "user_message_chunk"))))
                 (when new-prompt-p
                   (map-put! state :chunked-group-count (1+ (map-elt state :chunked-group-count)))
                   (agent-shell--append-transcript
                    :text (format "## User (%s)\n\n" (format-time-string "%F %T"))
                    :file-path agent-shell--transcript-file))
                 (let-alist update
                   (agent-shell--append-transcript
                    :text (format "> %s\n" .content.text)
                    :file-path agent-shell--transcript-file)
                   (agent-shell--update-text
                    :state state
                    :block-id (format "%s-user_message_chunk"
                                      (map-elt state :chunked-group-count))
                    :text (if new-prompt-p
                              (concat (propertize
                                       (map-nested-elt
                                        state '(:agent-config :shell-prompt))
                                       'font-lock-face 'comint-highlight-prompt)
                                      (propertize .content.text
                                                  'font-lock-face 'comint-highlight-input))
                            (propertize .content.text
                                        'font-lock-face 'comint-highlight-input))
                    :create-new new-prompt-p
                    :append t)))
               (map-put! state :last-entry-type "user_message_chunk"))
              ((equal (map-elt update 'sessionUpdate) "plan")
               (let-alist update
                 (agent-shell--update-fragment
                  :state state
                  :block-id "plan"
                  :label-left (propertize "Plan" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-plan .entries)
                  :expanded t))
               (map-put! state :last-entry-type "plan"))
              ((equal (map-elt update 'sessionUpdate) "tool_call_update")
               (agent-shell--handle-tool-call-update-streaming state update)
               (agent-shell--terminal-link-tool-call-content
                state (map-elt update 'toolCallId) (map-elt update 'content))
               (when (agent-shell--tool-call-terminal-ids (map-elt update 'content))
                 (agent-shell--save-tool-call
                  state
                  (map-elt update 'toolCallId)
                  (list (cons :has-terminal t))))
               (map-put! state :last-entry-type "tool_call_update"))
              ((equal (map-elt update 'sessionUpdate) "available_commands_update")
               (let-alist update
                 (map-put! state :available-commands (map-elt update 'availableCommands))
                 (agent-shell--update-fragment
                  :state state
                  :namespace-id "bootstrapping"
                  :block-id "available_commands_update"
                  :label-left (propertize "Available /commands" 'font-lock-face 'font-lock-doc-markup-face)
                  :body (agent-shell--format-available-commands (map-elt update 'availableCommands))))
               (map-put! state :last-entry-type "available_commands_update"))
              ((equal (map-elt update 'sessionUpdate) "current_mode_update")
               (let ((updated-session (map-elt state :session))
                     (new-mode-id (map-elt update 'currentModeId)))
                 (map-put! updated-session :mode-id new-mode-id)
                 (map-put! state :session updated-session)
                 (message "Session mode: %s"
                          (agent-shell--resolve-session-mode-name
                           new-mode-id
                           (agent-shell--get-available-modes state)))
                 ;; Note: No need to set :last-entry-type as no text was inserted.
                 (agent-shell--update-header-and-mode-line)))
              ((equal (map-elt update 'sessionUpdate) "config_option_update")
               ;; Silently handle config option updates (e.g., from set_model/set_mode)
               ;; These are informational notifications that don't require user-visible output
               ;; Note: No need to set :last-entry-type as no text was inserted.
               nil)
              ((equal (map-elt update 'sessionUpdate) "usage_update")
               ;; Extract context window and cost information
               (agent-shell--update-usage-from-notification :state state :update update)
               ;; Update header to reflect new context usage indicator
               (agent-shell--update-header-and-mode-line)
               ;; Note: This is session-level state, no need to set :last-entry-type
               nil)
              (t
               (agent-shell--update-fragment
                :state state
                :block-id "Session Update - fallback"
                :body (format "%s" notification)
                :create-new t
                :navigation 'never)
               (map-put! state :last-entry-type nil)))))
          (t
           (agent-shell--update-fragment
            :state state
            :block-id "Notification - fallback"
            :body (format "Unhandled notification (%s) and include:

```json
%s
```"
                          (agent-shell-ui-add-action-to-text
                           "please file a feature request"
                           (lambda ()
                             (interactive)
                             (browse-url "https://github.com/xenodium/agent-shell/issues/new/choose"))
                           (lambda ()
                             (message "Press RET to open URL"))
                           'link)
                          (with-temp-buffer
                            (insert (json-serialize notification))
                            (json-pretty-print (point-min) (point-max))
                            (buffer-string)))
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--on-request (&key state request)
  "Handle incoming request using SHELL, STATE, and REQUEST."
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (agent-shell--save-tool-call
            state .params.toolCall.toolCallId
            (append (list (cons :title .params.toolCall.title)
                          (cons :status .params.toolCall.status)
                          (cons :kind .params.toolCall.kind)
                          (cons :permission-request-id .id))
                    (when-let ((diff (agent-shell--make-diff-info
                                      :tool-call .params.toolCall)))
                      (list (cons :diff diff)))))
           (agent-shell--update-fragment
            :state state
            ;; block-id must be the same as the one used
            ;; in agent-shell--delete-fragment param.
            :block-id (format "permission-%s" .params.toolCall.toolCallId)
            :body (with-current-buffer (map-elt state :buffer)
                    (agent-shell--make-tool-call-permission-text
                     :request request
                     :client (map-elt state :client)
                     :state state))
            :expanded t
            :navigation 'never)
           (agent-shell-jump-to-latest-permission-button-row)
           (when-let (((map-elt state :buffer))
                      (viewport-buffer (agent-shell-viewport--buffer
                                        :shell-buffer (map-elt state :buffer)
                                        :existing-only t)))
             (with-current-buffer viewport-buffer
               (agent-shell-jump-to-latest-permission-button-row)))
           (map-put! state :last-entry-type "session/request_permission"))
          ((equal .method "fs/read_text_file")
           (agent-shell--on-fs-read-text-file-request
            :state state
            :request request))
          ((equal .method "fs/write_text_file")
           (agent-shell--on-fs-write-text-file-request
            :state state
            :request request))
          ((equal .method "terminal/create")
           (agent-shell--on-terminal-create-request
            :state state
            :request request))
          ((equal .method "terminal/output")
           (agent-shell--on-terminal-output-request
            :state state
            :request request))
          ((equal .method "terminal/wait_for_exit")
           (agent-shell--on-terminal-wait-for-exit-request
            :state state
            :request request))
          ((equal .method "terminal/kill")
           (agent-shell--on-terminal-kill-request
            :state state
            :request request))
          ((equal .method "terminal/release")
           (agent-shell--on-terminal-release-request
            :state state
            :request request))
          (t
           (agent-shell--update-fragment
            :state state
            :block-id "Unhandled Incoming Request"
            :body (format "⚠ Unhandled incoming request: \"%s\"" .method)
            :create-new t
            :navigation 'never)
           (map-put! state :last-entry-type nil)))))

(cl-defun agent-shell--extract-buffer-text (&key buffer line limit)
  "Extract text from BUFFER starting from LINE with optional LIMIT.
If the buffer's file has changed, prompt the user to reload it."
  (with-current-buffer buffer
    (when (and (buffer-file-name)
               (not (verify-visited-file-modtime))
               (y-or-n-p (format "%s has changed on file.  Reload? "
                                 (buffer-name))))
      (revert-buffer t nil nil))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (and line (> line 1))
          ;; Seems odd to use forward-line but
          ;; that's what `goto-line' recommends.
          (forward-line (1- line)))
        (let ((start (point)))
          (if limit
              ;; Seems odd to use forward-line but
              ;; that's what `goto-line' recommends.
              (forward-line limit)
            (goto-char (point-max)))
          (buffer-substring-no-properties start (point)))))))

(cl-defun agent-shell--on-fs-read-text-file-request (&key state request)
  "Handle fs/read_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (line (or .params.line 1))
               (limit .params.limit)
               (existing-buffer (find-buffer-visiting path))
               (content (if existing-buffer
                            ;; Read from open buffer (includes unsaved changes)
                            (agent-shell--extract-buffer-text :buffer existing-buffer :line line :limit limit)
                          ;; No open buffer, read from file
                          (with-temp-buffer
                            (insert-file-contents path)
                            (agent-shell--extract-buffer-text :buffer (current-buffer) :line line :limit limit)))))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-read-text-file-response
                      :request-id .id
                      :content content)))
      (quit
       ;; Handle C-g interrupts during file read prompts
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message "Operation cancelled by user"))))
      (file-missing
       ;; File doesn't exist - return RESOURCE_NOT_FOUND (-32002).
       ;; This allows agents to distinguish "file not found" from actual errors.
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32002
                           :message "Resource not found"
                           :data `((path . ,(nth 3 err)))))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-read-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(defun agent-shell--call-with-inhibited-minor-modes (modes thunk)
  "Call THUNK with MODES temporarily disabled in the current buffer.

Disable each mode in MODES that is enabled in the current buffer and has
a buffer-local mode variable.  Re-enable any modes disabled by this
function before returning."
  (let (disabled)
    (unwind-protect
        (progn
          (dolist (mode modes)
            (when (and (symbolp mode)
                       (fboundp mode)
                       (boundp mode)
                       (symbol-value mode)
                       (local-variable-p mode))
              (funcall mode -1)
              (push mode disabled)))
          (funcall thunk))
      (dolist (mode disabled)
        (funcall mode 1)))))

(cl-defun agent-shell--on-fs-write-text-file-request (&key state request)
  "Handle fs/write_text_file REQUEST with STATE."
  (let-alist request
    (condition-case err
        (let* ((path (agent-shell--resolve-path .params.path))
               (content .params.content)
               (dir (file-name-directory path))
               (buffer (or (find-buffer-visiting path)
                           ;; Prevent auto-insert-mode
                           ;; See issue #170
                           (let ((auto-insert nil))
                             (find-file-noselect path)))))
          (when (and dir (not (file-exists-p dir)))
            (make-directory dir t))
          (with-temp-buffer
            (insert content)
            (let ((content-buffer (current-buffer))
                  (inhibit-read-only t))
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  ;; Set a time-out to prevent locking up on large files
                  ;; https://github.com/xenodium/agent-shell/issues/168
                  (agent-shell--call-with-inhibited-minor-modes
                   agent-shell-write-inhibit-minor-modes
                   (lambda ()
                     (replace-buffer-contents content-buffer 1.0)))
                  (basic-save-buffer)))))
          (agent-shell--emit-event
           :event 'file-write
           :data (list (cons :path path)
                       (cons :content content)))
          (acp-send-response
           :client (map-elt state :client)
           :response (acp-make-fs-write-text-file-response
                      :request-id .id)))
      (quit
       ;; Handle C-g interrupts during file save prompts
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message "Operation cancelled by user"))))
      (error
       (acp-send-response
        :client (map-elt state :client)
        :response (acp-make-fs-write-text-file-response
                   :request-id .id
                   :error (acp-make-error
                           :code -32603
                           :message (error-message-string err))))))))

(defun agent-shell--get-devcontainer-workspace-path (cwd)
  "Return devcontainer workspaceFolder for CWD, or default value if none found.

See https://containers.dev for more information on devcontainers."
  (let ((devcontainer-config-file-name (expand-file-name ".devcontainer/devcontainer.json" cwd)))
    (condition-case _err
        (map-elt (json-read-file devcontainer-config-file-name) 'workspaceFolder
                 (concat "/workspaces/" (file-name-nondirectory (directory-file-name cwd)) "/"))
      (file-missing (error "Not found: %s" devcontainer-config-file-name))
      (permission-denied (error "Not readable: %s" devcontainer-config-file-name))
      (json-string-format (error "No valid JSON: %s" devcontainer-config-file-name)))))

(defun agent-shell--resolve-devcontainer-path (path)
  "Resolve PATH from a devcontainer in the local filesystem, and vice versa.

For example:

- /workspace/README.md => /home/xenodium/projects/kitchen-sink/README.md
- /home/xenodium/projects/kitchen-sink/README.md => /workspace/README.md"
  (let* ((cwd (agent-shell-cwd))
         (devcontainer-path (agent-shell--get-devcontainer-workspace-path cwd)))
    (if (string-prefix-p cwd path)
        (string-replace cwd devcontainer-path path)
      (if agent-shell-text-file-capabilities
          (if-let* ((is-dev-container (string-prefix-p devcontainer-path path))
                    (local-path (expand-file-name (string-replace devcontainer-path cwd path))))
              (or
               (and (file-in-directory-p local-path cwd) local-path)
               (error "Resolves to path outside of working directory: %s" path))
            (error "Unexpected path outside of workspace folder: %s" path))
        (error "Refuse to resolve to local filesystem with text file capabilities disabled: %s" path)))))

(defun agent-shell--stop-reason-description (stop-reason)
  "Return a human-readable text description for STOP-REASON.

https://agentclientprotocol.com/protocol/schema#param-stop-reason"
  (pcase stop-reason
    ("end_turn" "Finished")
    ("max_tokens" "Max token limit reached")
    ("max_turn_requests" "Exceeded request limit")
    ("refusal" "Refused")
    ("cancelled" "Cancelled")
    (_ (format "Stop for unknown reason: %s" stop-reason))))

(defun agent-shell--format-agent-capabilities (capabilities)
  "Format agent CAPABILITIES for shell rendering.

CAPABILITIES is as per ACP spec:

  https://agentclientprotocol.com/protocol/schema#agentcapabilities

Groups capabilities by category and displays them as comma-separated values.

Example output:

  prompt        image, and embedded context
  mcp           http, and sse"
  (let* ((case-fold-search nil)
         (categories (delq nil
                           (mapcar
                            (lambda (pair)
                              (let* ((key (if (symbolp (car pair))
                                              (symbol-name (car pair))
                                            (car pair)))
                                     (value (cdr pair))
                                     ;; "prompt Capabilities" -> "prompt"
                                     (group-name (replace-regexp-in-string
                                                  " Capabilities$" ""
                                                  ;; "promptCapabilities" -> "prompt Capabilities"
                                                  (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" key))))
                                (cond
                                 ;; Nested capability groups (promptCapabilities, mcpCapabilities)
                                 ((and (listp value)
                                       (not (vectorp value))
                                       (consp (car value)))
                                  (when-let ((enabled-items (delq nil (mapcar
                                                                       (lambda (cap-pair)
                                                                         ;; Match (key . t) and (key) forms.
                                                                         ;; eg. promptCapabilities uses (image . t)
                                                                         ;; but sessionCapabilities uses (fork).
                                                                         (when (or (eq (cdr cap-pair) t)
                                                                                   (null (cdr cap-pair)))
                                                                           (let* ((cap-key (car cap-pair))
                                                                                  (cap-name (if (symbolp cap-key)
                                                                                                (symbol-name cap-key)
                                                                                              cap-key)))
                                                                             (downcase
                                                                              (replace-regexp-in-string
                                                                               "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2"
                                                                               cap-name)))))
                                                                       value))))
                                    (cons (downcase group-name)
                                          (if (= (length enabled-items) 1)
                                              (car enabled-items)
                                            (concat (string-join (butlast enabled-items) ", ")
                                                    " and "
                                                    (car (last enabled-items)))))))
                                 ;; Top-level capabilities (loadSession)
                                 (t
                                  (cons (downcase group-name) nil)))))
                            capabilities))))
    (agent-shell--align-alist
     :data categories
     :columns (list
               (lambda (pair)
                 (propertize (car pair)
                             'font-lock-face 'font-lock-function-name-face))
               (lambda (pair)
                 (when (cdr pair)
                   (propertize (cdr pair)
                               'font-lock-face 'font-lock-comment-face))))
     :joiner "\n")))

(cl-defun agent-shell--make-error-handler (&key state shell-buffer)
  "Create ACP error handler with SHELL-BUFFER STATE."
  (lambda (error raw-message)
    (let-alist error
      (with-current-buffer (map-elt state :buffer)
        (agent-shell--update-fragment
         :state (agent-shell--state)
         :block-id (format "failed-%s-id:%s-code:%s"
                           (map-elt state :request-count)
                           (or .id "?")
                           (or .code "?"))
         :body (agent-shell--make-error-dialog-text
                :code .code
                :message .message
                :raw-message raw-message)
         :create-new t)))
    ;; TODO: Mark buffer command with shell failure.
    (with-current-buffer shell-buffer
      (shell-maker-finish-output :config shell-maker--config
                                 :success t))))

(cl-defun agent-shell--make-error-dialog-text (&key code message raw-message)
  "Create formatted error dialog text with CODE, MESSAGE, and RAW-MESSAGE."
  (format "╭─

  %s Error (%s) %s

  %s

  %s

╰─"
          (propertize "⚠" 'font-lock-face 'error)
          (or code "?")
          (propertize "⚠" 'font-lock-face 'error)
          (or message "¯\\_ (ツ)_/¯")
          (agent-shell--make-button
           :text "Details" :help "Details" :kind 'error
           :action (lambda ()
                     (interactive)
                     (agent-shell--view-as-error
                      (with-temp-buffer
                        (let ((print-circle t))
                          (pp raw-message (current-buffer))
                          (buffer-string))))))))

(defun agent-shell--view-as-error (text)
  "Display TEXT in a `read-only' error buffer."
  (let ((buf (get-buffer-create "*acp error*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (read-only-mode 1))
    (display-buffer buf)))

(defun agent-shell--clean-up ()
  "Clean up resources.

For example, shut down ACP client."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--shutdown)
  (when-let (((map-elt (agent-shell--state) :buffer))
             (viewport-buffer (agent-shell-viewport--buffer
                               :shell-buffer (map-elt (agent-shell--state) :buffer)
                               :existing-only t))
             (buffer-live-p viewport-buffer))
    (kill-buffer viewport-buffer)))

(defun agent-shell--shutdown ()
  "Shut down shell activity."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when (map-elt (agent-shell--state) :client)
    (acp-shutdown :client (map-elt (agent-shell--state) :client))
    (map-put! (agent-shell--state) :client nil)
    (map-put! (agent-shell--state) :initialized nil)
    (map-put! (agent-shell--state) :authenticated nil)
    (map-put! (agent-shell--state) :set-model nil)
    (map-put! (agent-shell--state) :set-session-mode nil))
  (agent-shell-heartbeat-stop
   :heartbeat (map-elt (agent-shell--state) :heartbeat)))

(cl-defun agent-shell--capture-screenshot (&key destination-dir)
  "Capture a screenshot and save it to DESTINATION-DIR.

Returns the full path to the captured screenshot file on success.
Signals an error on failure.

DESTINATION-DIR is required and must be provided."
  (unless destination-dir
    (error "Destination-dir is required"))
  (let* ((file-path (expand-file-name
                     (format "screenshot-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     destination-dir))
         (command (car agent-shell-screenshot-command))
         (args (append (cdr agent-shell-screenshot-command)
                       (list file-path))))
    (unless (file-directory-p destination-dir)
      (make-directory destination-dir t))
    (redisplay) ;; Give redisplay a chance before blocking call-process
    (let ((exit-code (apply #'call-process command nil nil nil args)))
      (cond
       ((not (zerop exit-code))
        (error "Screenshot command failed with exit code %d" exit-code))
       ((not (file-exists-p file-path))
        (error "Screenshot file was not created"))
       ((zerop (nth 7 (file-attributes file-path)))
        (error "Screenshot file is empty"))
       (t
        file-path)))))

(cl-defun agent-shell--save-clipboard-image (&key destination-dir no-error)
  "Save clipboard image to DESTINATION-DIR.
Returns the full path to the saved image file on success.
When NO-ERROR is non-nil, return nil instead of signaling errors.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details."
  (unless destination-dir
    (error "Destination-dir is required"))
  (let* ((file-path (expand-file-name
                     (format "clipboard-%s.png"
                             (format-time-string "%Y%m%d-%H%M%S"))
                     destination-dir))
         (handler (seq-find
                   (lambda (h)
                     (executable-find (map-elt h :command)))
                   agent-shell-clipboard-image-handlers)))
    (cond
     ((not handler)
      (unless no-error
        (error "No clipboard image utility found (tried: %s)"
               (mapconcat (lambda (h) (map-elt h :command))
                          agent-shell-clipboard-image-handlers ", "))))
     (t
      (unless (file-directory-p destination-dir)
        (make-directory destination-dir t))
      (condition-case err
          (funcall (map-elt handler :save) file-path)
        (error
         (unless no-error
           (signal (car err) (cdr err)))))
      (cond
       ((not (file-exists-p file-path))
        (unless no-error
          (error "Clipboard image file was not created")))
       ((zerop (nth 7 (file-attributes file-path)))
        (delete-file file-path)
        (unless no-error
          (error "No image found in clipboard")))
       (t
        file-path))))))

(defun agent-shell--format-buffer-name (agent-name project-name)
  "Format `agent-shell' buffer name using AGENT-NAME and PROJECT-NAME."
  (pcase agent-shell-buffer-name-format
        ((pred functionp)
         (funcall agent-shell-buffer-name-format agent-name project-name))
        ('kebab-case
         (format "%s-agent @ %s"
                 (downcase (replace-regexp-in-string " " "-" agent-name))
                 project-name))
        ('default
         (format "%s Agent @ %s"
                 agent-name
                 project-name))))

(cl-defun agent-shell--apply (&key function alist)
  "Apply keyword ALIST to FUNCTION.

ALIST should be a list of keyword-value pairs like (:foo 1 :bar 2).
FUNCTION should be a function accepting keyword arguments (&key ...)."
  (unless function
    (error "Missing required argument: :function"))
  (unless alist
    (error "Missing required argument: :alist"))
  (apply function
         (mapcan (lambda (pair)
                   (list (car pair) (cdr pair)))
                 alist)))

(cl-defun agent-shell--start (&key config no-focus new-session outgoing-request-decorator)
  "Programmatically start shell with CONFIG.

See `agent-shell-make-agent-config' for config format.

Set NO-FOCUS to start in background.
Set NEW-SESSION to start a separate new session.
OUTGOING-REQUEST-DECORATOR is passed through to `acp-make-client'."
  (unless (version<= "0.85.1" shell-maker-version)
    (error "Please update shell-maker to version 0.85.1 or newer"))
  (unless (version<= "0.11.1" acp-package-version)
    (error "Please update acp.el to version 0.11.1 or newer"))
  (when (boundp 'agent-shell--transcript-file-path-function)
    (user-error "'agent-shell--transcript-file-path-function is retired.

Please use 'agent-shell-transcript-file-path-function and unbind old
variable (see makunbound)"))
  (let* ((shell-maker-config (agent-shell--make-shell-maker-config
                              :prompt (map-elt config :shell-prompt)
                              :prompt-regexp (map-elt config :shell-prompt-regexp)))
         (agent-shell--shell-maker-config shell-maker-config)
         (default-directory (agent-shell-cwd))
         (shell-buffer
          (shell-maker-start agent-shell--shell-maker-config
                             t  ;; Always use no-focus, handle display below
                             nil ;; Defer showing welcome text
                             new-session
                             (agent-shell--format-buffer-name (map-elt config :buffer-name) (agent-shell--project-name))
                             (map-elt config :mode-line-name))))
    ;; While sending the first prompt request would already validate
    ;; finding the ACP agent executable, users have to wait until they
    ;; type a prompt and send it, only to find out that they are missing
    ;; the agent executable. This leaves them with an unsuable shell.
    ;; Better to check on shell creation and bail early (leaving no
    ;; shell behind).
    (with-current-buffer shell-buffer
      (unless (and (map-elt config :client-maker)
                   (funcall (map-elt config :client-maker) (current-buffer)))
        (kill-buffer shell-buffer)
        (error "No way to create a new client"))
      (let ((command (map-elt (funcall (map-elt config :client-maker) (current-buffer)) :command)))
        (unless (executable-find command)
          (kill-buffer shell-buffer)
          (error "%s" (agent-shell--make-missing-executable-error
                       :executable command
                       :install-instructions (map-elt config :install-instructions)))))
      ;; Initialize buffer-local state
      (setq-local agent-shell--state (agent-shell--make-state
                                      :buffer shell-buffer
                                      :heartbeat (agent-shell-heartbeat-make
                                                  :on-heartbeat
                                                  (lambda (_heartbeat _status)
                                                    (when (get-buffer-window shell-buffer)
                                                      (with-current-buffer shell-buffer
                                                        (agent-shell--update-header-and-mode-line)))
                                                    (when-let* ((using-viewports agent-shell-prefer-viewport-interaction)
                                                                (viewport-buffer (agent-shell-viewport--buffer
                                                                                  :shell-buffer shell-buffer
                                                                                  :existing-only t))
                                                                ((get-buffer-window viewport-buffer)))
                                                      (with-current-buffer viewport-buffer
                                                        (agent-shell-viewport--update-header)))))
                                      :client-maker (map-elt config :client-maker)
                                      :needs-authentication (map-elt config :needs-authentication)
                                      :authenticate-request-maker (map-elt config :authenticate-request-maker)
                                      :outgoing-request-decorator outgoing-request-decorator
                                      :agent-config config))
      ;; Initialize buffer-local shell-maker-config
      (setq-local agent-shell--shell-maker-config shell-maker-config)
      (setq-local filter-buffer-substring-function #'agent-shell--filter-buffer-substring)
      (agent-shell--update-header-and-mode-line)
      (add-hook 'kill-buffer-hook #'agent-shell--clean-up nil t)
      (agent-shell-ui-mode +1)
      (when agent-shell-file-completion-enabled
        (agent-shell-completion-mode +1))
      (agent-shell--setup-modeline)
      (setq-local agent-shell--transcript-file (agent-shell--transcript-file-path))
      ;; agent-shell does not support restoring sessions from transcript
      ;; via shell-maker. Unalias this functionality so it's not
      ;; misleading to users or appear via M-x.
      (fmakunbound 'agent-shell-restore-session-from-transcript)
      (when agent-shell--transcript-file
        ;; Prefer agent-shell--transcript-file over shell-maker's
        ;; transcript capabilities. Unalias to hide this in favor
        ;; of agent-shell's agent-shell--transcript-file usage.
        (fmakunbound 'agent-shell-save-session-transcript)
        (setq-local shell-maker-prompt-before-killing-buffer nil))
      ;; Show deferred welcome text,
      ;; but first wipe buffer content.
      (let ((inhibit-read-only t))
        (erase-buffer))
      (set-marker (process-mark (shell-maker--process)) (point-max))
      (when (and agent-shell-show-welcome-message
                 (map-elt config :welcome-function))
        (shell-maker-write-output
         :config shell-maker--config
         :output (funcall (map-elt config :welcome-function)
                          shell-maker--config)))
      (if (eq agent-shell-session-strategy 'new-deferred)
          ;; Show prompt now (unbootstrapped).
          (shell-maker-finish-output
           :config shell-maker--config
           :success nil)
        ;; Kick off ACP session bootstrapping.
        (agent-shell--handle :shell-buffer shell-buffer)))
    ;; Subscribe to session selection events (needed regardless of focus).
    (when (eq agent-shell-session-strategy 'prompt)
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer
       :event 'session-selection-cancelled
       :on-event (lambda (_event)
                   (kill-buffer shell-buffer)))
      (let ((active-message (agent-shell-active-message-show :text "Loading...")))
        (agent-shell-subscribe-to
         :shell-buffer shell-buffer
         :event 'session-prompt
         :on-event (lambda (_event)
                     (agent-shell-active-message-hide :active-message active-message)))
        (agent-shell-subscribe-to
         :shell-buffer shell-buffer
         :event 'session-selected
         :on-event (lambda (_event)
                     (agent-shell-active-message-hide :active-message active-message)))
        (agent-shell-subscribe-to
         :shell-buffer shell-buffer
         :event 'session-selection-cancelled
         :on-event (lambda (_event)
                     (agent-shell-active-message-hide :active-message active-message)))))
    ;; Display buffer if no-focus was nil, respecting agent-shell-display-action
    (unless no-focus
      (if (eq agent-shell-session-strategy 'prompt)
          ;; Defer display until user selects a session.
          ;; Why? The experience is janky to display a buffer
          ;; and soon after that prompt the user for input.
          ;; Better to prompt the user for input and then
          ;; display the buffer.
          (agent-shell-subscribe-to
           :shell-buffer shell-buffer
           :event 'session-selected
           :on-event (lambda (_event)
                       (agent-shell--display-buffer shell-buffer)))
        (agent-shell--display-buffer shell-buffer)))
    shell-buffer))

(defun agent-shell-toggle-logging ()
  "Toggle logging."
  (interactive)
  (setq acp-logging-enabled (not acp-logging-enabled))
  (message "Logging: %s" (if acp-logging-enabled "ON" "OFF")))

(defun agent-shell-reset-logs ()
  "Reset all log buffers."
  (interactive)
  (acp-reset-logs :client (map-elt (agent-shell--state) :client))
  (message "Logs reset"))

(defun agent-shell-next-item ()
  "Go to next item.

Could be a prompt or an expandable item.
If point is at the input prompt and a character key was pressed,
insert the character instead."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  ;; Check if at prompt and inserting a character
  ;; (Ignore special keys like TAB/Shift-TAB).
  (if (and (not (shell-maker-busy))
           (shell-maker-point-at-last-prompt-p)
           (integerp last-command-event)
           (> (length (this-command-keys-vector)) 0)
           ;; Ensure invoked using a key binding.
           (eq (key-binding (this-command-keys-vector)) this-command))
      ;; At prompt, insert character.
      (self-insert-command 1)
    ;; Otherwise navigate.
    (let* ((prompt-pos (save-mark-and-excursion
                         (when (comint-next-prompt 1)
                           (point))))
           (block-pos (save-mark-and-excursion
                        (agent-shell-ui-forward-block)))
           (button-pos (save-mark-and-excursion
                         (agent-shell-next-permission-button)))
           (next-pos (apply #'min (delq nil (list prompt-pos
                                                  block-pos
                                                  button-pos)))))
      (when next-pos
        (deactivate-mark)
        (goto-char next-pos)
        (when (eq next-pos prompt-pos)
          (comint-skip-prompt))))))

(defun agent-shell-previous-item ()
  "Go to previous item.

Could be a prompt or an expandable item.
If point is at the input prompt and a character key was pressed,
insert the character instead."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  ;; Check if at prompt and inserting a character
  ;; (Ignore special keys like TAB/Shift-TAB).
  (if (and (not (shell-maker-busy))
           (shell-maker-point-at-last-prompt-p)
           (integerp last-command-event)
           (> (length (this-command-keys-vector)) 0)
           ;; Ensure invoked using a key binding.
           (eq (key-binding (this-command-keys-vector)) this-command))
      ;; At prompt, insert character.
      (self-insert-command 1)
    ;; Otherwise navigate.
    (let* ((current-pos (point))
           (prompt-pos (save-mark-and-excursion
                         (when (comint-next-prompt (- 1))
                           (let ((pos (point)))
                             (when (< pos current-pos)
                               pos)))))
           (block-pos (save-mark-and-excursion
                        (let ((pos (agent-shell-ui-backward-block)))
                          (when (and pos (< pos current-pos))
                            pos))))
           (button-pos (save-mark-and-excursion
                         (let ((pos (agent-shell-previous-permission-button)))
                           (when (and pos (< pos current-pos))
                             pos))))
           (positions (delq nil (list prompt-pos
                                      block-pos
                                      button-pos)))
           (next-pos (when positions
                       (apply #'max positions))))
      (when next-pos
        (deactivate-mark)
        (goto-char next-pos)
        (when (eq next-pos prompt-pos)
          (comint-skip-prompt))))))

(cl-defun agent-shell-make-environment-variables (&rest vars &key inherit-env load-env &allow-other-keys)
  "Return VARS in the form expected by `process-environment'.

With `:INHERIT-ENV' t, also inherit system environment (as per `setenv')
With `:LOAD-ENV' PATH-OR-PATHS, load .env files from given path(s).

For example:

  (agent-shell-make-environment-variables
    \"PATH\" \"/usr/bin\"
    \"HOME\" \"/home/user\"
    :load-env \"~/.env\")

Returns:

   (\"PATH=/usr/bin\"
    \"HOME=/home/user\")."
  (unless (zerop (mod (length vars) 2))
    (error "`agent-shell-make-environment' must receive complete pairs"))
  (append (mapcan (lambda (pair)
                    (unless (keywordp (car pair))
                      (list (format "%s=%s" (car pair) (cadr pair)))))
                  (seq-partition vars 2))
          (when load-env
            (let ((paths (if (listp load-env) load-env (list load-env))))
              (mapcan (lambda (path)
                        (unless (file-exists-p path)
                          (error "File not found: %s" path))
                        (with-temp-buffer
                          (insert-file-contents path)
                          (let (result)
                            (dolist (line (mapcar #'string-trim (split-string (buffer-string) "\n" t)))
                              (unless (or (string-empty-p line)
                                          (string-prefix-p "#" line))
                                (if (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
                                    (push line result)
                                  (error "Malformed line in %s: %s" path line))))
                            (nreverse result))))
                      paths)))
          (when inherit-env
            process-environment)))

(defvar-local agent-shell--header-cache nil
  "Cache for graphical headers (no need for regenerating regularly).

A buffer-local hash table mapping cache keys to header strings.")

(cl-defun agent-shell--make-header-model (state &key qualifier bindings)
  "Create a header model alist from STATE, QUALIFIER, and BINDINGS.
The model contains all inputs needed to render the graphical header."
  (let* ((model-name (or (map-elt (seq-find (lambda (model)
                                              (string= (map-elt model :model-id)
                                                       (map-nested-elt state '(:session :model-id))))
                                            (map-nested-elt state '(:session :models)))
                                  :name)
                         (map-nested-elt state '(:session :model-id))))
         (mode-id (map-nested-elt state '(:session :mode-id)))
         (mode-name (when mode-id
                      (or (agent-shell--resolve-session-mode-name
                           mode-id
                           (agent-shell--get-available-modes state))
                          mode-id))))
    `((:buffer-name . ,(map-nested-elt state '(:agent-config :buffer-name)))
      (:icon-name . ,(map-nested-elt state '(:agent-config :icon-name)))
      (:model-id . ,(map-nested-elt state '(:session :model-id)))
      (:model-name . ,model-name)
      (:mode-id . ,mode-id)
      (:mode-name . ,mode-name)
      (:directory . ,default-directory)
      (:frame-width . ,(frame-pixel-width))
      (:font-height . ,(frame-char-height))
      (:font-size . ,(when-let* (((display-graphic-p))
                                 (font (face-attribute 'default :font))
                                 ((fontp font)))
                       (font-get font :size)))
      (:background-mode . ,(frame-parameter nil 'background-mode))
      (:context-indicator . ,(agent-shell--context-usage-indicator))
      (:busy-indicator-frame . ,(agent-shell--busy-indicator-frame))
      (:qualifier . ,qualifier)
      (:bindings . ,bindings))))

(defun agent-shell--header-cache-key (model)
  "Generate a cache key from header MODEL.
Joins all values from the model alist."
  (mapconcat (lambda (pair) (format "%s" (cdr pair)))
             model "|"))

(cl-defun agent-shell--make-header (state &key qualifier bindings)
  "Return header text for current STATE.

STATE should contain :agent-config with :icon-name, :buffer-name, and
:session with :mode-id and :modes for displaying the current session mode.

QUALIFIER: Any text to prefix BINDINGS row with.

BINDINGS is a list of alists defining key bindings to display, each with:
  :key         - Key string (e.g., \"n\")
  :description - Description to display (e.g., \"next hunk\")"
  (unless state
    (error "STATE is required"))
  (let* ((header-model (agent-shell--make-header-model state :qualifier qualifier :bindings bindings))
         (text-header (format " %s%s%s @ %s%s%s"
                              (propertize (concat (map-elt header-model :buffer-name) " Agent")
                                          'font-lock-face 'font-lock-variable-name-face)
                              (if (map-elt header-model :model-name)
                                  (concat " ➤ " (propertize (map-elt header-model :model-name) 'font-lock-face 'font-lock-negation-char-face))
                                "")
                              (if (map-elt header-model :mode-name)
                                  (concat " ➤ " (propertize (map-elt header-model :mode-name) 'font-lock-face 'font-lock-type-face))
                                "")
                              (propertize (string-remove-suffix "/" (abbreviate-file-name (map-elt header-model :directory)))
                                          'font-lock-face 'font-lock-string-face)
                              (if (map-elt header-model :context-indicator)
                                  (concat " " (map-elt header-model :context-indicator))
                                "")
                              (if (map-elt header-model :busy-indicator-frame)
                                  (map-elt header-model :busy-indicator-frame)
                                ""))))
    (pcase agent-shell-header-style
      ((or 'none (pred null)) nil)
      ('text text-header)
      ('graphical
       (if (display-graphic-p)
           ;; +------+
           ;; | icon | Top text line
           ;; |      | Bottom text line
           ;; +------+
           ;; [Qualifier] Bindings row (optional, last row)
           (let* ((cache-key (agent-shell--header-cache-key header-model))
                  (cached (progn
                            (unless agent-shell--header-cache
                              (setq agent-shell--header-cache (make-hash-table :test #'equal)))
                            (map-elt agent-shell--header-cache cache-key))))
             (or cached
                 (let* ((char-height (map-elt header-model :font-height))
                        (font-size (map-elt header-model :font-size))
                        (has-bindings (or bindings qualifier))
                        (image-height (* 3 char-height))
                        (image-width image-height)
                        (text-height char-height)
                        (top-padding-height (/ font-size 2))
                        (bottom-padding-height (if has-bindings (+ text-height top-padding-height) top-padding-height))
                        (row-spacing (if has-bindings font-size 0))
                        (total-height (+ image-height row-spacing top-padding-height bottom-padding-height))
                        ;; icon position
                        (icon-x 6)
                        (icon-y top-padding-height)
                        ;; text position right of the icon area
                        (icon-text-x (+ icon-x image-width 10))
                        (icon-text-y (+ icon-y char-height (/ (- char-height font-size) 2)))
                        ;; Bindings positioned below the icon area
                        (bindings-x icon-x)
                        (bindings-y (+ image-height font-size row-spacing))
                        (svg (svg-create (map-elt header-model :frame-width) total-height))
                        (icon-filename
                         (if (map-elt header-model :icon-name)
                             (agent-shell--fetch-agent-icon (map-elt header-model :icon-name))
                           (agent-shell--make-agent-fallback-icon (map-elt header-model :buffer-name) 100)))
                        (image-type (or (agent-shell--image-type-to-mime icon-filename)
                                        "image/png")))
                   ;; Icon
                   (when (and icon-filename image-type)
                     (svg-embed svg icon-filename
                                image-type nil
                                :x icon-x :y icon-y :width image-width :height image-height))
                   ;; Top text line
                   (svg--append svg (let ((text-node (dom-node 'text
                                                               `((x . ,icon-text-x)
                                                                 (y . ,icon-text-y)
                                                                 (font-size . ,font-size)))))
                                      ;; Agent name
                                      (dom-append-child text-node
                                                        (dom-node 'tspan
                                                                  `((fill . ,(face-attribute 'font-lock-variable-name-face :foreground)))
                                                                  (concat (map-elt header-model :buffer-name) " Agent")))
                                      ;; Model name (optional)
                                      (when (map-elt header-model :model-name)
                                        ;; Add separator arrow
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'default :foreground))
                                                                      (dx . "8"))
                                                                    "➤"))
                                        ;; Add model name
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'font-lock-negation-char-face :foreground))
                                                                      (dx . "8"))
                                                                    (map-elt header-model :model-name))))
                                      ;; Session mode (optional)
                                      (when (map-elt header-model :mode-id)
                                        ;; Add separator arrow
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'default :foreground))
                                                                      (dx . "8"))
                                                                    "➤"))
                                        ;; Add session mode text
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(or (face-attribute 'font-lock-type-face :foreground nil t)
                                                                                   "#6699cc"))
                                                                      (dx . "8"))
                                                                    (map-elt header-model :mode-name))))
                                      (when (map-elt header-model :context-indicator)
                                        (let* (;; Extract the face from the propertized string
                                               (face (get-text-property 0 'face (map-elt header-model :context-indicator)))
                                               ;; Get the foreground color from the face
                                               (color (if face
                                                          (face-attribute face :foreground nil t)
                                                        (face-attribute 'default :foreground))))
                                          (dom-append-child text-node
                                                            (dom-node 'tspan
                                                                      `((fill . ,color)
                                                                        (dx . "8"))
                                                                      (substring-no-properties (map-elt header-model :context-indicator))))))
                                      (when (map-elt header-model :busy-indicator-frame)
                                        (dom-append-child text-node
                                                          (dom-node 'tspan
                                                                    `((fill . ,(face-attribute 'default :foreground))
                                                                      (dx . "8"))
                                                                    (map-elt header-model :busy-indicator-frame))))
                                      text-node))
                   ;; Bottom text line
                   (svg-text svg (string-remove-suffix "/" (abbreviate-file-name (map-elt header-model :directory)))
                             :x icon-text-x :y (+ icon-text-y text-height (- char-height font-size))
                             :font-size font-size
                             :fill (face-attribute 'font-lock-string-face :foreground))
                   ;; Bindings row (last row if bindings or qualifier present)
                   (when (or bindings qualifier)
                     (svg--append svg (let ((text-node (dom-node 'text
                                                                 `((x . ,bindings-x)
                                                                   (y . ,bindings-y)
                                                                   (font-size . ,font-size))))
                                            (first t))
                                        ;; Add qualifier if present
                                        (when qualifier
                                          (dom-append-child text-node
                                                            (dom-node 'tspan
                                                                      `((fill . ,(face-attribute 'default :foreground)))
                                                                      qualifier))
                                          (setq first nil))
                                        (dolist (binding bindings)
                                          (when (map-elt binding :description)
                                            ;; Add key (XML-escape angle brackets)
                                            (dom-append-child text-node
                                                              (dom-node 'tspan
                                                                        `((fill . ,(face-attribute 'help-key-binding :foreground))
                                                                          ,@(unless first '((dx . "8"))))
                                                                        (replace-regexp-in-string
                                                                         "<" "&lt;"
                                                                         (replace-regexp-in-string
                                                                          ">" "&gt;"
                                                                          (map-elt binding :key)))))
                                            (setq first nil)
                                            ;; Add space and description
                                            (dom-append-child text-node
                                                              (dom-node 'tspan
                                                                        `((fill . ,(face-attribute 'default :foreground))
                                                                          (dx . "8"))
                                                                        (map-elt binding :description)))))
                                        text-node)))
                   (let ((result (format " %s" (with-temp-buffer
                                                 (svg-insert-image svg)
                                                 (buffer-string)))))
                     (map-put! agent-shell--header-cache cache-key result)
                     result))))
         text-header))
      (_ text-header))))

(defun agent-shell--image-type-to-mime (filename)
  "Convert image type from FILENAME to MIME type string.
Returns a MIME type like \"image/png\" or \"image/jpeg\"."
  (let* ((type (image-supported-file-p filename))
         (ext (downcase (or (file-name-extension filename) ""))))
    (cond
     (type
      (pcase type
        ('svg "image/svg+xml")
        (_ (format "image/%s" type))))
     ((member ext '("png" "gif" "webp"))
      (format "image/%s" ext))
     ((member ext '("jpg" "jpeg"))
      "image/jpeg")
     ((string= ext "svg")
      "image/svg+xml")
     (t nil))))

(defun agent-shell--update-header-and-mode-line ()
  "Update header and mode line based on `agent-shell-header-style'."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ((eq agent-shell-header-style 'graphical)
    (setq header-line-format (agent-shell--make-header (agent-shell--state))))
   ((memq agent-shell-header-style '(text none nil))
    (setq header-line-format (agent-shell--make-header (agent-shell--state)))
    (force-mode-line-update))))

(defun agent-shell--fetch-agent-icon (icon-name)
  "Download icon with ICON-NAME from GitHub, only if it exists, and save as binary.

Names can be found at https://github.com/lobehub/lobe-icons/tree/master/packages/static-png

Icon names starting with https:// are downloaded directly from that location."
  (when icon-name
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (is-url (string-prefix-p "https://" (downcase icon-name)))
           (url (if is-url
                    icon-name
                  (concat "https://raw.githubusercontent.com/lobehub/lobe-icons/refs/heads/master/packages/static-png/"
                          mode "/" icon-name)))
           (filename (if is-url
                         ;; For URLs, sanitize to create readable filename
                         ;; e.g., "https://opencode.ai/favicon.svg" -> "opencode.ai-favicon.svg"
                         (replace-regexp-in-string
                          "[/:]" "-"
                          (replace-regexp-in-string
                           "^https?://" ""
                           url))
                       ;; For lobe-icons names, use the original filename
                       (file-name-nondirectory url)))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((buffer (url-retrieve-synchronously url t t 5.0)))
          (when buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              (if (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                  (progn
                    (re-search-forward "\r?\n\r?\n")
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region (point) (point-max) cache-path)))
                (message "Icon fetch failed: %s" url)))
            (kill-buffer buffer))))
      (when (file-exists-p cache-path)
        cache-path))))

(defun agent-shell--make-agent-fallback-icon (icon-name width)
  "Create SVG icon with first character of ICON-NAME and WIDTH.
Return file path of the generated SVG."
  (when (and icon-name (not (string-empty-p icon-name)))
    (let* ((icon-text (char-to-string (string-to-char icon-name)))
           (mode (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light"))
           (filename (format "%s-%s.svg" icon-name width))
           (cache-dir (file-name-concat (temporary-file-directory) "agent-shell" mode))
           (cache-path (expand-file-name filename cache-dir))
           (font-size (* 0.7 width))
           (x (/ width 2))
           (y (/ width 2)))
      (unless (file-exists-p cache-path)
        (make-directory cache-dir t)
        (let ((svg (svg-create width width :stroke "white" :fill "black")))
          (svg-text svg icon-text
                    :x x :y y
                    :text-anchor "middle"
                    :dominant-baseline "central"
                    :font-weight "bold"
                    :font-size font-size
                    ;; :font-family "Monaco, Courier New, Courier, monospace"
                    :font-family (face-attribute 'default :family)
                    :fill (face-attribute 'default :foreground))
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (svg-print svg))
            (write-region (point-min) (point-max) cache-path))))
      cache-path)))

(defun agent-shell-view-traffic ()
  "View agent shell traffic buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((traffic-buffer (acp-traffic-buffer :client (map-elt (agent-shell--state) :client))))
    (when (with-current-buffer traffic-buffer
            (= (buffer-size) 0))
      (error "No traffic logs available.  Try M-x agent-shell-toggle-logging?"))
    (pop-to-buffer traffic-buffer)))

(defun agent-shell-view-acp-logs ()
  "View agent shell ACP logs buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((logs-buffer (acp-logs-buffer :client (map-elt (agent-shell--state) :client))))
    (when (with-current-buffer logs-buffer
            (= (buffer-size) 0))
      (error "No traffic logs available.  Try M-x agent-shell-toggle-logging?"))
    (pop-to-buffer logs-buffer)))

(cl-defun agent-shell--make-missing-executable-error (&key executable install-instructions)
  "Create error message for missing EXECUTABLE.
INSTALL-INSTRUCTIONS is optional installation guidance."
  (concat (format "Executable \"%s\" not found.  Do you need (add-to-list 'exec-path \"another/path/to/consider/\")?" executable)
          (when install-instructions
            (concat "  " install-instructions))))

(defun agent-shell--display-buffer (shell-buffer)
  "Toggle agent SHELL-BUFFER display."
  (interactive)
  (if (get-buffer-window shell-buffer)
      (select-window (get-buffer-window shell-buffer))
    (select-window (display-buffer shell-buffer agent-shell-display-action))))

(defun agent-shell--state ()
  "Get shell state or fail in an incompatible buffer."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Processed outside shell: %s" major-mode))
  (unless agent-shell--state
    (error "No shell state available"))
  agent-shell--state)

;;; Events

(defvar agent-shell--subscription-counter 0
  "Counter for generating unique subscription tokens.")

(cl-defun agent-shell-subscribe-to (&key shell-buffer event on-event)
  "Subscribe to events in SHELL-BUFFER.

ON-EVENT is a function called with an event alist containing:
  :event - A symbol identifying the event

When EVENT is non-nil, only events matching that symbol are dispatched.
When EVENT is nil, all events are dispatched.

Initialization events (emitted in order):
  `init-started'        - Initialization pipeline started
  `init-client'         - ACP client created
  `init-subscriptions'  - ACP event subscriptions registered
  `init-handshake'      - ACP initialize/handshake RPC completed
  `init-authenticate'   - ACP authentication completed (optional)
  `init-session'        - ACP session created
  `init-model'          - Default model set (optional)
  `init-session-mode'   - Default session mode set (optional)
  `session-list'        - Session list fetch initiated
  `session-prompt'      - About to prompt user for session selection
  `session-selected'    - Session chosen (new or existing)
    :data contains :session-id (nil when starting new)
  `session-selection-cancelled' - User cancelled session selection
  `init-finished'       - Initialization pipeline completed
  `prompt-ready'        - Shell prompt displayed and ready for input

Session events:
  `tool-call-update'    - Tool call started or updated
    :data contains :tool-call-id and :tool-call
  `file-write'          - File written via fs/write_text_file
    :data contains :path and :content
  `permission-response' - Permission response sent
    :data contains :request-id, :tool-call-id, :option-id, :cancelled

Returns a subscription token for use with `agent-shell-unsubscribe'.

Example usage:

  ;; Subscribe to all events
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :on-event (lambda (event)
               (message \"event: %s\" (map-elt event :event))))

  ;; Subscribe to file writes
  (agent-shell-subscribe-to
   :shell-buffer shell-buffer
   :event \\='file-write
   :on-event (lambda (event)
               (let ((data (map-elt event :data)))
                 (message \"wrote: %s\" (map-elt data :path)))))

  ;; Unsubscribe
  (let ((token (agent-shell-subscribe-to
                :shell-buffer shell-buffer
                :on-event #\\='my-handler)))
    (agent-shell-unsubscribe :subscription token))"
  (unless on-event
    (error "Missing required argument: :on-event"))
  (unless shell-buffer
    (error "Missing required argument: :shell-buffer"))
  (let ((token (cl-incf agent-shell--subscription-counter)))
    (with-current-buffer shell-buffer
      (let ((subscriptions (map-elt (agent-shell--state) :event-subscriptions)))
        (map-put! (agent-shell--state)
                  :event-subscriptions
                  (cons (list (cons :token token)
                              (cons :event event)
                              (cons :on-event on-event))
                        subscriptions))))
    token))

(cl-defun agent-shell-unsubscribe (&key subscription)
  "Remove event SUBSCRIPTION by token.

SUBSCRIPTION is a token returned by `agent-shell-subscribe-to'."
  (unless subscription
    (error "Missing required argument: :subscription"))
  (let ((subscriptions (map-elt (agent-shell--state) :event-subscriptions)))
    (map-put! (agent-shell--state)
              :event-subscriptions
              (seq-remove (lambda (sub)
                            (equal (map-elt sub :token) subscription))
                          subscriptions))))

;;; Initialization

(cl-defun agent-shell--initialize-client ()
  "Initialize ACP client."
  (agent-shell--update-fragment
   :state (agent-shell--state)
   :namespace-id "bootstrapping"
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "Creating client..."
   :create-new t)
  (if (map-elt (agent-shell--state) :client-maker)
      (progn
        (map-put! (agent-shell--state)
                  :client (funcall (map-elt agent-shell--state :client-maker)
                                   (map-elt agent-shell--state :buffer)))
        (agent-shell--emit-event :event 'init-client)
        t)
    (shell-maker-write-output :config shell-maker--config
                              :output "No :client-maker found")
    (shell-maker-finish-output :config shell-maker--config
                               :success nil)
    nil))

(cl-defun agent-shell--initialize-subscriptions ()
  "Initialize ACP client subscriptions."
  (agent-shell--update-fragment
   :state agent-shell--state
   :namespace-id "bootstrapping"
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "in_progress")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "\n\nSubscribing..."
   :append t)
  (if (map-elt agent-shell--state :client)
      (progn
        (agent-shell--subscribe-to-client-events :state agent-shell--state)
        (agent-shell--emit-event :event 'init-subscriptions)
        t)
    (shell-maker-write-output :config shell-maker--config
                              :output "No :client found")
    (shell-maker-finish-output :config shell-maker--config
                               :success nil)
    nil))

(cl-defun agent-shell--initiate-handshake (&key shell-buffer on-initiated)
  "Initiate ACP handshake with SHELL-BUFFER.

Must provide ON-INITIATED (lambda ())."
  (unless on-initiated
    (error "Missing required argument: :on-initiated"))
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-fragment
     :state agent-shell--state
     :namespace-id "bootstrapping"
     :block-id "starting"
     :body "\n\nInitializing..."
     :append t))
  (acp-send-request
   :client (map-elt agent-shell--state :client)
   :request (acp-make-initialize-request
             :protocol-version 1
             :client-info `((name . "agent-shell")
                            (title . "Emacs Agent Shell")
                            (version . ,agent-shell--version))
             :read-text-file-capability agent-shell-text-file-capabilities
             :write-text-file-capability agent-shell-text-file-capabilities
             :terminal-capability t)
   :on-success (lambda (response)
                 (with-current-buffer shell-buffer
                   (let ((acp-session-capabilities (or (map-elt response 'sessionCapabilities)
                                                       (map-nested-elt response '(agentCapabilities sessionCapabilities)))))
                     (map-put! agent-shell--state :supports-session-list
                               (and (listp acp-session-capabilities)
                                    (assq 'list acp-session-capabilities)
                                    t))
                     (map-put! agent-shell--state :supports-session-resume
                               (and (listp acp-session-capabilities)
                                    (assq 'resume acp-session-capabilities)
                                    t)))
                   ;; Save prompt capabilities from agent, converting to internal symbols
                   (when-let ((prompt-capabilities
                               (map-nested-elt response '(agentCapabilities promptCapabilities))))
                     (map-put! agent-shell--state :prompt-capabilities
                               (list (cons :image (map-elt prompt-capabilities 'image))
                                     (cons :embedded-context (map-elt prompt-capabilities 'embeddedContext)))))
                   ;; Save available modes from agent, converting to internal symbols
                   (when-let ((modes (map-elt response 'modes)))
                     (map-put! agent-shell--state :available-modes
                               (list (cons :current-mode-id (map-elt modes 'currentModeId))
                                     (cons :modes (mapcar (lambda (mode)
                                                            `((:id . ,(map-elt mode 'id))
                                                              (:name . ,(map-elt mode 'name))
                                                              (:description . ,(map-elt mode 'description))))
                                                          (map-elt modes 'availableModes))))))
                   (when-let ((agent-capabilities (map-elt response 'agentCapabilities)))
                     (map-put! agent-shell--state :supports-session-load
                               (eq (map-elt agent-capabilities 'loadSession) t))
                     (agent-shell--update-fragment
                      :state agent-shell--state
                      :namespace-id "bootstrapping"
                      :block-id "agent_capabilities"
                      :label-left (propertize "Agent capabilities" 'font-lock-face 'font-lock-doc-markup-face)
                      :body (agent-shell--format-agent-capabilities agent-capabilities)))
                   (agent-shell--emit-event :event 'init-handshake))
                 (funcall on-initiated))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell-buffer shell-buffer)))

(cl-defun agent-shell--authenticate (&key shell-buffer on-authenticated)
  "Initiate ACP authentication with SHELL-BUFFER.

Must provide ON-AUTHENTICATED (lambda ())."
  (with-current-buffer (map-elt agent-shell--state :buffer)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :namespace-id "bootstrapping"
     :block-id "starting"
     :body "\n\nAuthenticating..."
     :append t))
  (if (map-elt (agent-shell--state) :authenticate-request-maker)
      (acp-send-request
       :client (map-elt (agent-shell--state) :client)
       :request (funcall (map-elt agent-shell--state :authenticate-request-maker))
       :on-success (lambda (_response)
                     ;; TODO: More to be handled?
                     (with-current-buffer shell-buffer
                       (agent-shell--emit-event :event 'init-authenticate))
                     (funcall on-authenticated))
       :on-failure (agent-shell--make-error-handler
                    :state (agent-shell--state) :shell-buffer shell-buffer))
    (shell-maker-write-output :config shell-maker--config
                              :output "No :authenticate-request-maker")
    (shell-maker-finish-output :config shell-maker--config
                               :success nil)))

(cl-defun agent-shell--set-default-model (&key shell-buffer model-id on-model-changed)
  "Set default model to MODEL-ID in SHELL-BUFFER.
Call ON-MODEL-CHANGED on success."
  (when-let ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (with-current-buffer (map-elt agent-shell--state :buffer)
      (agent-shell--update-fragment
       :state (agent-shell--state)
       :namespace-id "bootstrapping"
       :block-id "set-model"
       :label-left (propertize "Setting model" 'font-lock-face 'font-lock-doc-markup-face)
       :body (format "Requesting %s..." model-id)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-model-request
               :session-id session-id
               :model-id model-id)
     :on-success (lambda (_response)
                   (agent-shell--update-fragment
                    :state (agent-shell--state)
                    :namespace-id "bootstrapping"
                    :block-id "set-model"
                    :body "\n\nDone"
                    :append t)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :model-id model-id)
                     (map-put! (agent-shell--state) :session updated-session))
                   (agent-shell--update-header-and-mode-line)
                   (agent-shell--emit-event :event 'init-model)
                   (when on-model-changed
                     (funcall on-model-changed)))
     :on-failure (agent-shell--make-error-handler
                  :state (agent-shell--state) :shell-buffer shell-buffer))))

(cl-defun agent-shell--set-default-session-mode (&key shell-buffer mode-id on-mode-changed)
  "Set default session mode to MODE-ID in SHELL-BUFFER.
Call ON-MODE-CHANGED on success."
  (when-let ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (with-current-buffer (map-elt agent-shell--state :buffer)
      (agent-shell--update-fragment
       :state (agent-shell--state)
       :namespace-id "bootstrapping"
       :block-id "set-session-mode"
       :label-left (propertize "Setting session mode" 'font-lock-face 'font-lock-doc-markup-face)
       :body (format "Requesting %s..." mode-id)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id session-id
               :mode-id mode-id)
     :on-success (lambda (_response)
                   (agent-shell--update-fragment
                    :state (agent-shell--state)
                    :namespace-id "bootstrapping"
                    :block-id "set-session-mode"
                    :body "\n\nDone"
                    :append t)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id mode-id)
                     (map-put! (agent-shell--state) :session updated-session))
                   (agent-shell--update-header-and-mode-line)
                   (agent-shell--emit-event :event 'init-session-mode)
                   (when on-mode-changed
                     (funcall on-mode-changed)))
     :on-failure (agent-shell--make-error-handler
                  :state (agent-shell--state) :shell-buffer shell-buffer))))

(cl-defun agent-shell--initiate-session (&key shell-buffer on-session-init)
  "Initiate ACP session creation with SHELL-BUFFER.

Must provide ON-SESSION-INIT (lambda ())."
  (unless on-session-init
    (error "Missing required argument: :on-session-init"))
  (with-current-buffer (map-elt (agent-shell--state) :buffer)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :namespace-id "bootstrapping"
     :block-id "starting"
     :body "\n\nCreating session..."
     :append t))
  (if (and (map-elt (agent-shell--state) :supports-session-list)
           (or (map-elt (agent-shell--state) :supports-session-load)
               (map-elt (agent-shell--state) :supports-session-resume))
           (not (memq agent-shell-session-strategy '(new-deferred new))))
      (agent-shell--initiate-session-list-and-load
       :shell-buffer shell-buffer
       :on-session-init on-session-init)
    (progn
      (agent-shell--emit-event :event 'session-selected)
      (agent-shell--initiate-new-session
       :shell-buffer shell-buffer
       :on-session-init on-session-init))))

(defun agent-shell--format-session-date (iso-timestamp)
  "Format ISO-TIMESTAMP as a human-friendly date string.

Returns \"Today, HH:MM\", \"Yesterday, HH:MM\", \"Mon DD, HH:MM\"
for the current year, or \"Mon DD, YYYY\" for other years."
  (condition-case nil
      (let* ((time (date-to-time iso-timestamp))
             (now (current-time))
             (decoded-now (decode-time now))
             (today-start (encode-time 0 0 0
                                       (decoded-time-day decoded-now)
                                       (decoded-time-month decoded-now)
                                       (decoded-time-year decoded-now)))
             (yesterday-start (time-subtract today-start (seconds-to-time (* 24 60 60))))
             (current-year (decoded-time-year (decode-time now)))
             (timestamp-year (decoded-time-year (decode-time time))))
        (cond
         ((not (time-less-p time today-start))
          (format-time-string "Today, %H:%M" time))
         ((not (time-less-p time yesterday-start))
          (format-time-string "Yesterday, %H:%M" time))
         ((= timestamp-year current-year)
          (format-time-string "%b %d, %H:%M" time))
         (t
          (format-time-string "%b %d, %Y" time))))
    (error iso-timestamp)))

(defun agent-shell--session-dir-name (acp-session)
  "Return directory name for ACP-SESSION."
  (file-name-nondirectory
   (directory-file-name (or (map-elt acp-session 'cwd) ""))))

(defun agent-shell--session-title (acp-session)
  "Return display title for ACP-SESSION, truncated to 50 chars."
  (let ((title (or (map-elt acp-session 'title) "Untitled")))
    (if (> (length title) 50)
        (concat (substring title 0 47) "...")
      title)))

(defun agent-shell--session-choice-label (acp-session max-dir-width max-title-width)
  "Return completion label for ACP-SESSION.
MAX-DIR-WIDTH is the column width for the directory name.
MAX-TITLE-WIDTH is the column width for the title."
  (let* ((dir-name (agent-shell--session-dir-name acp-session))
         (dir-padding (make-string (- (+ max-dir-width 1) (length dir-name)) ?\s))
         (dir-col (propertize (concat dir-name dir-padding) 'face 'font-lock-keyword-face))
         (title (agent-shell--session-title acp-session))
         (title-padding (make-string (- (+ max-title-width 1) (length title)) ?\s))
         (updated-at (or (map-elt acp-session 'updatedAt)
                         (map-elt acp-session 'createdAt)
                         "unknown-time"))
         (date-str (propertize (agent-shell--format-session-date updated-at)
                               'face 'font-lock-comment-face)))
    (concat dir-col title title-padding date-str)))

(defun agent-shell--prompt-select-session (acp-sessions)
  "Prompt to choose one from ACP-SESSIONS.

Return selected session alist, or nil to start a new session.
Falls back to latest session in batch mode (e.g. tests)."
  (when acp-sessions
    (if noninteractive
        (car acp-sessions)
    (let* ((max-dir-width (apply #'max (mapcar (lambda (s)
                                                (length (agent-shell--session-dir-name s)))
                                              acp-sessions)))
           (max-title-width (apply #'max (mapcar (lambda (s)
                                                   (length (agent-shell--session-title s)))
                                                 acp-sessions)))
           (new-session-choice "Start a new session")
           (choices (cons (cons new-session-choice nil)
                          (mapcar (lambda (acp-session)
                                    (cons (agent-shell--session-choice-label acp-session max-dir-width max-title-width)
                                          acp-session))
                                  acp-sessions)))
           (candidates (mapcar #'car choices))
           ;; Some completion frameworks yielded appended (nil) to each line
           ;; unless this-command was bound.
           ;;
           ;; For example:
           ;;
           ;; Let's build something                 Today, 16:25 (nil)
           ;; Let's optimize the rocket engine      Feb 12, 21:02 (nil)
           (this-command 'agent-shell))
      (agent-shell--emit-event :event 'session-prompt)
      (let ((selection (completing-read "Resume session: "
                                        candidates
                                        nil t nil nil
                                        new-session-choice)))
        (map-elt choices selection))))))

(cl-defun agent-shell--set-session-from-response (&key acp-response acp-session-id)
  "Set active session state from ACP-RESPONSE and ACP-SESSION-ID."
  (map-put! agent-shell--state
            :session (list (cons :id acp-session-id)
                           (cons :mode-id (map-nested-elt acp-response '(modes currentModeId)))
                           (cons :modes (mapcar (lambda (mode)
                                                  `((:id . ,(map-elt mode 'id))
                                                    (:name . ,(map-elt mode 'name))
                                                    (:description . ,(map-elt mode 'description))))
                                                (map-nested-elt acp-response '(modes availableModes))))
                           (cons :model-id (map-nested-elt acp-response '(models currentModelId)))
                           (cons :models (mapcar (lambda (model)
                                                   `((:model-id . ,(map-elt model 'modelId))
                                                     (:name . ,(map-elt model 'name))
                                                     (:description . ,(map-elt model 'description))))
                                                 (map-nested-elt acp-response '(models availableModels)))))))

(cl-defun agent-shell--finalize-session-init (&key on-session-init)
  "Finalize session initialization and invoke ON-SESSION-INIT."
  (agent-shell--update-fragment
   :state agent-shell--state
   :block-id "starting"
   :label-left (format "%s %s"
                       (agent-shell--status-label "completed")
                       (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
   :body "\n\nReady"
   :namespace-id "bootstrapping"
   :append t)
  (agent-shell--update-header-and-mode-line)
  (when (map-nested-elt agent-shell--state '(:session :models))
    (agent-shell--update-fragment
     :state agent-shell--state
     :namespace-id "bootstrapping"
     :block-id "available_models"
     :label-left (propertize "Available models" 'font-lock-face 'font-lock-doc-markup-face)
     :body (agent-shell--format-available-models
            (map-nested-elt agent-shell--state '(:session :models)))))
  (when (agent-shell--get-available-modes agent-shell--state)
    (agent-shell--update-fragment
     :state agent-shell--state
     :namespace-id "bootstrapping"
     :block-id "available_modes"
     :label-left (propertize "Available modes" 'font-lock-face 'font-lock-doc-markup-face)
     :body (agent-shell--format-available-modes
            (agent-shell--get-available-modes agent-shell--state))))
  (agent-shell--update-header-and-mode-line)
  (agent-shell--emit-event :event 'init-session)
  (funcall on-session-init))

(cl-defun agent-shell--initiate-new-session (&key shell-buffer on-session-init)
  "Initiate ACP session/new with SHELL-BUFFER and ON-SESSION-INIT."
  (acp-send-request
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-new-request
             :cwd (agent-shell--resolve-path (agent-shell-cwd))
             :mcp-servers (agent-shell--mcp-servers))
   :buffer (current-buffer)
   :on-success (lambda (response)
                 (map-put! agent-shell--state
                           :session (list (cons :id (map-elt response 'sessionId))
                                          (cons :mode-id (map-nested-elt response '(modes currentModeId)))
                                          (cons :modes (mapcar (lambda (mode)
                                                                 `((:id . ,(map-elt mode 'id))
                                                                   (:name . ,(map-elt mode 'name))
                                                                   (:description . ,(map-elt mode 'description))))
                                                               (map-nested-elt response '(modes availableModes))))
                                          (cons :model-id (map-nested-elt response '(models currentModelId)))
                                          (cons :models (mapcar (lambda (model)
                                                                  `((:model-id . ,(map-elt model 'modelId))
                                                                    (:name . ,(map-elt model 'name))
                                                                    (:description . ,(map-elt model 'description))))
                                                                (map-nested-elt response '(models availableModels))))))
                 (agent-shell--update-fragment
                  :state agent-shell--state
                  :block-id "starting"
                  :label-left (format "%s %s"
                                      (agent-shell--status-label "completed")
                                      (propertize "Starting agent" 'font-lock-face 'font-lock-doc-markup-face))
                  :body "\n\nReady"
                  :namespace-id "bootstrapping"
                  :append t)
                 (agent-shell--update-header-and-mode-line)
                 (when (map-nested-elt agent-shell--state '(:session :models))
                   (agent-shell--update-fragment
                    :state agent-shell--state
                    :namespace-id "bootstrapping"
                    :block-id "available_models"
                    :label-left (propertize "Available models" 'font-lock-face 'font-lock-doc-markup-face)
                    :body (agent-shell--format-available-models
                           (map-nested-elt agent-shell--state '(:session :models)))))
                 (when (agent-shell--get-available-modes agent-shell--state)
                   (agent-shell--update-fragment
                    :state agent-shell--state
                    :namespace-id "bootstrapping"
                    :block-id "available_modes"
                    :label-left (propertize "Available modes" 'font-lock-face 'font-lock-doc-markup-face)
                    :body (agent-shell--format-available-modes
                           (agent-shell--get-available-modes agent-shell--state))))
                 (agent-shell--update-header-and-mode-line)
                 (agent-shell--emit-event :event 'init-session)
                 (funcall on-session-init))
   :on-failure (agent-shell--make-error-handler
                :state agent-shell--state :shell-buffer shell-buffer)))

(cl-defun agent-shell--initiate-session-list-and-load (&key shell-buffer on-session-init)
  "Try loading latest existing session with SHELL-BUFFER and ON-SESSION-INIT."
  (with-current-buffer (map-elt (agent-shell--state) :buffer)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :namespace-id "bootstrapping"
     :block-id "starting"
     :body "\n\nLooking for existing sessions..."
     :append t))
  (agent-shell--emit-event :event 'session-list)
  (acp-send-request
   :client (map-elt (agent-shell--state) :client)
   :request (acp-make-session-list-request
             :cwd (agent-shell--resolve-path (agent-shell-cwd)))
   :buffer (current-buffer)
   :on-success (lambda (acp-response)
                 (let ((acp-sessions (append (or (map-elt acp-response 'sessions) '()) nil)))
                   (condition-case nil
                       (let* ((acp-session
                               (pcase agent-shell-session-strategy
                                 ('new-deferred nil)
                                 ('new nil)
                                 ('latest (car acp-sessions))
                                 ('prompt (agent-shell--prompt-select-session acp-sessions))
                                 (_ (message "Unknown session strategy '%s', starting a new session"
                                             agent-shell-session-strategy)
                                    nil)))
                              (acp-session-id (and acp-session
                                                   (map-elt acp-session 'sessionId))))
                         (agent-shell--emit-event
                          :event 'session-selected
                          :data (list (cons :session-id acp-session-id)))
                         (if acp-session-id
                             (progn
                               (agent-shell--update-fragment
                                :state (agent-shell--state)
                                :namespace-id "bootstrapping"
                                :block-id "starting"
                                :body (format "\n\nLoading session %s..." acp-session-id)
                                :append t)
                               (acp-send-request
                                :client (map-elt (agent-shell--state) :client)
                                :request (let ((cwd (agent-shell--resolve-path (agent-shell-cwd)))
                                               (mcp-servers (agent-shell--mcp-servers)))
                                           (let ((use-resume (if agent-shell-prefer-session-resume
                                                                  (map-elt (agent-shell--state) :supports-session-resume)
                                                                (not (map-elt (agent-shell--state) :supports-session-load)))))
                                             (if use-resume
                                                 (acp-make-session-resume-request
                                                  :session-id acp-session-id
                                                  :cwd cwd
                                                  :mcp-servers mcp-servers)
                                               (acp-make-session-load-request
                                                :session-id acp-session-id
                                                :cwd cwd
                                                :mcp-servers mcp-servers))))
                                :buffer (current-buffer)
                                :on-success (lambda (acp-load-response)
                                              (agent-shell--set-session-from-response
                                               :acp-response acp-load-response
                                               :acp-session-id acp-session-id)
                                              (agent-shell--update-fragment
                                               :state (agent-shell--state)
                                               :namespace-id "bootstrapping"
                                               :block-id "resumed_session"
                                               :label-left (format "%s %s"
                                                                   (agent-shell--status-label "completed")
                                                                   (propertize "Resuming session" 'font-lock-face 'font-lock-doc-markup-face))
                                               :expanded t
                                               :body (or (map-elt acp-session 'title) ""))
                                              (agent-shell--finalize-session-init :on-session-init on-session-init))
                                :on-failure (lambda (_error _raw-message)
                                              (agent-shell--update-fragment
                                               :state (agent-shell--state)
                                               :namespace-id "bootstrapping"
                                               :block-id "starting"
                                               :body "\n\nCould not load existing session. Creating a new one..."
                                               :append t)
                                              (agent-shell--initiate-new-session
                                               :shell-buffer shell-buffer
                                               :on-session-init on-session-init))))
                           (agent-shell--initiate-new-session
                            :shell-buffer shell-buffer
                            :on-session-init on-session-init)))
                     (quit
                      (agent-shell--emit-event :event 'session-selection-cancelled)))))
   :on-failure (lambda (_error _raw-message)
                 (agent-shell--initiate-new-session
                  :shell-buffer shell-buffer
                  :on-session-init on-session-init))))

(defun agent-shell--eval-dynamic-values (obj)
  "Recursively evaluate any lambda values in OBJ.
Named functions (symbols) are not evaluated to avoid accidentally
calling external symbols."
  (cond
   ((and (functionp obj) (not (symbolp obj))) (agent-shell--eval-dynamic-values (funcall obj)))
   ((consp obj)
    (cons (agent-shell--eval-dynamic-values (car obj))
          (agent-shell--eval-dynamic-values (cdr obj))))
   (t obj)))

(defun agent-shell--mcp-servers ()
  "Return normalized MCP servers configuration for JSON serialization.

Converts list-valued `args', `env', and `headers' fields to vectors
so they serialize properly to JSON arrays.  Returns a vector of
normalized server configs."
  (when agent-shell-mcp-servers
    (apply #'vector
           (mapcar (lambda (server)
                     (setq server (agent-shell--eval-dynamic-values server))
                     (let ((normalized (copy-alist server)))
                       (when (map-contains-key normalized 'args)
                         (let ((args (map-elt normalized 'args)))
                           (when (listp args)
                             (map-put! normalized 'args (apply #'vector args)))))
                       (when (map-contains-key normalized 'env)
                         (let ((env (map-elt normalized 'env)))
                           (when (listp env)
                             (map-put! normalized 'env (apply #'vector env)))))
                       (when (map-contains-key normalized 'headers)
                         (let ((headers (map-elt normalized 'headers)))
                           (when (listp headers)
                             (map-put! normalized 'headers (apply #'vector headers)))))
                       normalized))
                   agent-shell-mcp-servers))))

(cl-defun agent-shell--subscribe-to-client-events (&key state)
  "Subscribe SHELL and STATE to ACP events."
  (acp-subscribe-to-errors
   :client (map-elt state :client)
   :on-error (lambda (error)
               (agent-shell--update-fragment
                :state state
                :block-id (format "%s-notices"
                                  (map-elt state :request-count))
                :label-left (propertize "Notices" 'font-lock-face 'font-lock-doc-markup-face) ;;
                :body (or (map-elt error 'message)
                          (map-elt error 'data)
                          "Something is up ¯\\_ (ツ)_/¯")
                :append t)))
  (acp-subscribe-to-notifications
   :client (map-elt state :client)
   :on-notification (lambda (notification)
                      (agent-shell--on-notification :state state :notification notification)))
  (acp-subscribe-to-requests
   :client (map-elt state :client)
   :on-request (lambda (request)
                 (agent-shell--on-request :state state :request request))))

(defun agent-shell--parse-file-mentions (prompt)
  "Parse @ file mentions from PROMPT string.
Returns list of alists with :start, :end, and :path for each mention."
  (let ((mentions '())
        (pos 0))
    (while (string-match (rx (or line-start (not word))
                             "@"
                             (or (seq "\"" (group (+ (not "\""))) "\"")
                                 (group (+ (not space)))))
                         prompt pos)
      (push `((:start . ,(match-beginning 0))
              (:end . ,(match-end 0))
              (:path . ,(when-let ((path (or (match-string 1 prompt) (match-string 2 prompt))))
                          (substring-no-properties path))))
            mentions)
      (setq pos (match-end 0)))
    (nreverse mentions)))

(cl-defun agent-shell--build-content-blocks (prompt)
  "Build content blocks from the PROMPT."
  (let* ((supports-embedded-context (map-nested-elt agent-shell--state '(:prompt-capabilities :embedded-context)))
         (supports-image (map-nested-elt agent-shell--state '(:prompt-capabilities :image)))
         (mentions (agent-shell--parse-file-mentions prompt))
         (content-blocks '())
         (pos 0))
    (dolist (mention mentions)
      (let* ((start (map-elt mention :start))
             (end (map-elt mention :end))
             (relative-path (map-elt mention :path))
             (expanded-path (expand-file-name relative-path (agent-shell-cwd)))
             (resolved-path (agent-shell--resolve-path expanded-path)))
        ;; Add text before mention
        (when (> start pos)
          (push `((type . "text")
                  (text . ,(substring-no-properties prompt pos start)))
                content-blocks))

        ;; Try to embed or link file
        (condition-case nil
            (let ((file (and (file-readable-p expanded-path)
                             (agent-shell--read-file-content :file-path expanded-path))))
              (cond
               ;; File not readable - keep mention as text
               ((not file)
                (push `((type . "text")
                        (text . ,(substring-no-properties prompt start end)))
                      content-blocks))
               ;; Binary image and image capability supported
               ;; Use ContentBlock::Image
               ((and supports-image (map-elt file :base64-p)
                     (string-prefix-p "image/" (map-elt file :mime-type)))
                (push `((type . "image")
                        (data . ,(map-elt file :content))
                        (mimeType . ,(map-elt file :mime-type))
                        (uri . ,(concat "file://" resolved-path)))
                      content-blocks))
               ;; Text file, small enough, text file capabilities granted and embeddedContext supported
               ;; Use ContentBlock::Resource
               ((and agent-shell-text-file-capabilities supports-embedded-context (map-elt file :size)
                     (< (map-elt file :size) agent-shell-embed-file-size-limit))
                (push `((type . "resource")
                        (resource . ((uri . ,(concat "file://" resolved-path))
                                     (text . ,(map-elt file :content))
                                     (mimeType . ,(map-elt file :mime-type)))))
                      content-blocks))
               ;; File too large, no text file capabilities granted or embeddedContext not supported
               ;; Use resource link
               (t
                (push `((type . "resource_link")
                        (uri . ,(concat "file://" resolved-path))
                        (name . ,relative-path)
                        (mimeType . ,(map-elt file :mime-type))
                        (size . ,(map-elt file :size)))
                      content-blocks))))
          (error
           ;; On error, just keep the mention as text
           (push `((type . "text")
                   (text . ,(substring-no-properties prompt start end)))
                 content-blocks)))

        (setq pos end)))

    ;; Add remaining text
    (when (< pos (length prompt))
      (push `((type . "text")
              (text . ,(substring-no-properties prompt pos)))
            content-blocks))

    (nreverse content-blocks)))

(cl-defun agent-shell--read-file-content (&key file-path shallow)
  "Read FILE-PATH and return metadata and content as an alist.

When SHALLOW is non-nil, only metadata is returned without loading file content.

Returns an alist with:
  :size - file size in bytes
  :extension - file extension (lowercase)
  :mime-type - MIME type based on extension
  :base64-p - t if content is base64-encoded (binary image), nil otherwise
  :content - file content (omitted when SHALLOW is non-nil)"
  (let* ((ext (downcase (or (file-name-extension file-path) "")))
         (mime-type (or (agent-shell--image-type-to-mime file-path)
                        "text/plain"))
         ;; Only treat supported binary image formats as binary
         ;; SVG is XML/text and should not be base64-encoded
         ;; API only supports: image/png, image/jpeg, image/gif, image/webp
         (is-binary (member mime-type '("image/png" "image/jpeg" "image/gif" "image/webp")))
         (file-size (file-attribute-size (file-attributes file-path)))
         (content (unless shallow
                    (with-temp-buffer
                      (if is-binary
                          (progn
                            (insert-file-contents-literally file-path)
                            (base64-encode-string (buffer-string) t))
                        (insert-file-contents file-path)
                        (buffer-string))))))
    (append (list (cons :size file-size)
                  (cons :extension ext)
                  (cons :mime-type mime-type)
                  (cons :base64-p is-binary))
            (unless shallow
              (list (cons :content content))))))

(cl-defun agent-shell--load-image (&key file-path (max-width 200))
  "Load image from FILE-PATH and return the image object.

MAX-WIDTH specifies the maximum width in pixels for the image (default 200).
If FILE-PATH is not an image, returns nil."
  (when-let* (((display-graphic-p))
              (metadata (agent-shell--read-file-content :file-path file-path :shallow t))
              (mime-type (map-elt metadata :mime-type))
              ;; Check if it's an image type
              (is-image (string-prefix-p "image/" mime-type)))
    (create-image file-path nil nil :max-width max-width)))

(cl-defun agent-shell--collect-attached-files (content-blocks)
  "Collect attached resource uris from CONTENT-BLOCKS."
  (mapcan
   (lambda (content-block)
     (let ((type (map-elt content-block 'type)))
       (cond
        ((equal type "resource") (list (map-nested-elt content-block '(resource uri))))
        ((equal type "resource_link") (list (map-elt content-block 'uri)))
        ((equal type "image") (list (map-elt content-block 'uri)))
        (t nil))))
   content-blocks))

(cl-defun agent-shell--display-attached-files (uris)
  "Display the attached URIS in the buffer."
  (agent-shell--update-fragment
   :state agent-shell--state
   :block-id "attached-files"
   :label-left (format "%d file%s attached"
                       (length uris)
                       (if (= (length uris) 1) "" "s"))
   :body (mapconcat (lambda (f) (format "• %s" f))
                    (nreverse uris)
                    "\n")
   :create-new t))

(cl-defun agent-shell--send-command (&key prompt shell-buffer)
  "Send PROMPT to agent using SHELL-BUFFER."
  (let* ((content-blocks (condition-case nil
                             (agent-shell--build-content-blocks prompt)
                           (error `[((type . "text")
                                     (text . ,(substring-no-properties prompt)))])))
         (attached-files (agent-shell--collect-attached-files content-blocks)))
    (when attached-files
      (agent-shell--display-attached-files attached-files))
    (when agent-shell-show-busy-indicator
      (agent-shell-heartbeat-start
       :heartbeat (map-elt agent-shell--state :heartbeat)))

    (map-put! agent-shell--state :last-entry-type nil)

    (agent-shell--append-transcript
     :text (format "## User (%s)\n\n%s\n\n"
                   (format-time-string "%F %T")
                   prompt)
     :file-path agent-shell--transcript-file)

    (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                 :shell-buffer shell-buffer
                                 :existing-only t)))
      (with-current-buffer viewport-buffer
        (agent-shell-viewport-view-mode)
        (agent-shell-viewport--initialize
         :prompt  prompt)))

    (acp-send-request
     :client (map-elt agent-shell--state :client)
     :request (acp-make-session-prompt-request
               :session-id (map-nested-elt agent-shell--state '(:session :id))
               :prompt content-blocks)
     :buffer (current-buffer)
     :on-success (lambda (response)
                   (when (equal (map-elt (agent-shell--state) :last-entry-type) "agent_message_chunk")
                     (agent-shell--append-transcript
                      :text "\n\n"
                      :file-path agent-shell--transcript-file))
                   ;; Tool call details are no longer needed after
                   ;; a session prompt request is finished.
                   ;; Avoid accumulating them unnecessarily.
                   (map-put! (agent-shell--state) :tool-calls nil)
                   ;; Extract usage information from response
                   (when (map-elt response 'usage)
                     (agent-shell--save-usage :state (agent-shell--state) :acp-usage (map-elt response 'usage)))
                   (let ((success (equal (map-elt response 'stopReason)
                                         "end_turn")))
                     ;; Display usage box at end of turn if enabled and data available
                     (when (and success
                                agent-shell-show-usage-at-turn-end
                                (agent-shell--usage-has-data-p (map-elt (agent-shell--state) :usage)))
                       (agent-shell--update-fragment
                        :state (agent-shell--state)
                        :block-id (format "%s-usage" (map-elt (agent-shell--state) :request-count))
                        :label-left (propertize "Usage" 'font-lock-face 'font-lock-doc-markup-face)
                        :body (agent-shell--format-usage (map-elt (agent-shell--state) :usage) t)
                        :create-new t))
                     (unless success
                       (agent-shell--update-fragment
                        :state (agent-shell--state)
                        :block-id (format "%s-stop-reason"
                                          (map-elt (agent-shell--state) :request-count))
                        :body (agent-shell--stop-reason-description
                               (map-elt response 'stopReason))
                        :create-new t))
                     (agent-shell-heartbeat-stop
                      :heartbeat (map-elt agent-shell--state :heartbeat))
                     (unless success
                       (agent-shell--display-pending-requests))
                     (shell-maker-finish-output :config shell-maker--config
                                                :success t)
                     ;; Update viewport header (longer busy)
                     (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                                  :shell-buffer shell-buffer
                                                  :existing-only t)))
                       (with-current-buffer viewport-buffer
                         (agent-shell-viewport--update-header)))
                     (when success
                       (agent-shell--process-pending-request))))
     :on-failure (lambda (error raw-message)
                   ;; Display pending requests on failure.
                   (agent-shell--display-pending-requests)
                   (funcall (agent-shell--make-error-handler :state agent-shell--state :shell-buffer shell-buffer)
                            error raw-message)
                   (agent-shell-heartbeat-stop
                    :heartbeat (map-elt agent-shell--state :heartbeat))
                   ;; Update viewport header (longer busy)
                   (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                                :shell-buffer shell-buffer
                                                :existing-only t)))
                     (with-current-buffer viewport-buffer
                       (agent-shell-viewport--update-header)))))))

;;; Projects

(defun agent-shell-project-buffers ()
  "Return all shell buffers in the same project as current buffer."
  (let ((project-root (agent-shell-cwd)))
    (seq-filter (lambda (buffer)
                  (equal project-root
                         (with-current-buffer buffer
                           (agent-shell-cwd))))
                (agent-shell-buffers))))

(cl-defun agent-shell--shell-buffer (&key viewport-buffer no-error no-create)
  "Get an `agent-shell' buffer for the current project.

Resolution order:
1. If VIEWPORT-BUFFER is provided, derive shell buffer from its name.
2. If inside of a viewport buffer, derive shell buffer from its name.
3. If currently in an `agent-shell-mode' buffer, return it.
4. If there are shells in current project, return the first one found.
5. Otherwise, ask user to pick one.

When NO-CREATE is nil (default), prompt to create a new shell if none exists.
When NO-CREATE is non-nil, return existing shell or nil/error if none exists.
When NO-ERROR is non-nil, return nil instead of raising an error.

Returns a buffer object or nil."
  (let ((shell-buffer (or (agent-shell-viewport--shell-buffer
                           (or viewport-buffer (current-buffer)))
                          (if (derived-mode-p 'agent-shell-mode)
                              (current-buffer)
                            (seq-first (agent-shell-project-buffers))))))
    (if shell-buffer
        shell-buffer
      (if no-create
          (unless no-error
            (user-error "No agent shell buffers available for current project"))
        (if (y-or-n-p "No shells in project.  Start a new one? ")
            (get-buffer
             (agent-shell--start :config (or (agent-shell--resolve-preferred-config)
                                             (agent-shell-select-config
                                              :prompt "Start new agent: ")
                                             (error "No agent config found"))
                                 :no-focus t
                                 :new-session t))
          (if-let ((shell-buffers (agent-shell-buffers)))
              (get-buffer (completing-read "Choose a shell: "
                                           (mapcar #'buffer-name shell-buffers)
                                           nil t))
            (unless no-error
              (user-error "No agent shell buffers available"))))))))

(defun agent-shell--current-shell ()
  "Current shell for viewport or shell buffer."
  (cond ((derived-mode-p 'agent-shell-mode)
         (current-buffer))
        ((or (derived-mode-p 'agent-shell-viewport-view-mode)
             (derived-mode-p 'agent-shell-viewport-edit-mode))
         (seq-first (seq-filter (lambda (shell-buffer)
                                  (equal (agent-shell-viewport--buffer
                                          :shell-buffer shell-buffer
                                          :existing-only t)
                                         (current-buffer)))
                                (agent-shell-buffers))))))

(defun agent-shell--input ()
  "Return shell input (not yet submitted)."
  (when-let* ((shell-buffer (agent-shell--shell-buffer))
              (input (with-current-buffer shell-buffer
                       ;; Based on `comint-kill-input'
                       ;; to get latest input.
                       (buffer-substring
                        (or (marker-position comint-accum-marker)
                            (process-mark (get-buffer-process (current-buffer))))
                        (point-max)))))
    (unless (string-empty-p (string-trim input))
      input)))

;;; Shell

(defun agent-shell-insert-shell-command-output ()
  "Execute a shell command and insert output as a code block.

The command executes asynchronously.  When finished, the output is
inserted into the shell buffer prompt."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-viewport-view-mode)
              (derived-mode-p 'agent-shell-viewport-edit-mode)
              (derived-mode-p 'agent-shell-mode))
    (user-error "Not in an `agent-shell' buffer"))
  (let* ((command (read-string "insert command output: "))
         (shell-buffer (or (agent-shell--current-shell)
                           (user-error "No shell available")))
         (destination-buffer (progn
                               (when (with-current-buffer shell-buffer
                                       (shell-maker-busy))
                                 (user-error "Busy, try later"))
                               (if (or (derived-mode-p 'agent-shell-viewport-view-mode)
                                       (derived-mode-p 'agent-shell-viewport-edit-mode))
                                   (agent-shell-viewport--buffer
                                    :shell-buffer shell-buffer)
                                 shell-buffer)))
         (output-buffer (with-current-buffer (generate-new-buffer (format "*%s*" command))
                          (insert "$ " command "\n\n")
                          (setq-local buffer-read-only t)
                          (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "q") #'quit-window)
                            (use-local-map map))
                          (current-buffer)))
         (window-config (current-window-configuration))
         (proc (make-process
                :name command
                :buffer output-buffer
                :command (with-current-buffer shell-buffer
                           (agent-shell--build-command-for-execution
                            (list shell-file-name
                                  shell-command-switch
                                  ;; Merge stderr into stdout output
                                  ;; (all into output buffer)
                                  (format "%s 2>&1" command))))
                :connection-type 'pipe
                :filter
                (lambda (proc output)
                  (when (buffer-live-p (process-buffer proc))
                    (with-current-buffer (process-buffer proc)
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert output)))))
                :sentinel
                (lambda (process _event)
                  (when (memq (process-status process) '(exit signal))
                    (message "Done")
                    (set-window-configuration window-config)
                    (save-excursion
                      (goto-char (point-max))
                      (with-current-buffer destination-buffer
                        (insert "\n\n" (format "```shell
%s
```" (with-current-buffer output-buffer
       (buffer-string))))))
                    (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                      (markdown-overlays-put))
                    (when (buffer-live-p output-buffer)
                      (kill-buffer output-buffer)))))))
    (set-process-query-on-exit-flag proc nil)
    (run-at-time "0.2 sec" nil
                 (lambda ()
                   (unless (equal (process-status proc) 'exit)
                     (agent-shell--display-buffer output-buffer))))))

;;; Completion

(cl-defun agent-shell--get-files-context (&key files agent-cwd)
  "Process FILES into sendable text with image preview if applicable.

Uses AGENT-CWD to shorten file paths where necessary."
  (when files
    (mapconcat (lambda (file)
                 (when agent-cwd
                   (setq file (expand-file-name file agent-cwd)))
                 (if-let ((image-display (agent-shell--load-image
                                          :file-path file
                                          :max-width 200)))
                     ;; Propertize text to display the image
                     (agent-shell-ui-add-action-to-text
                      (propertize (concat "@" file)
                                  'display image-display
                                  'pointer 'hand)
                      (lambda ()
                        (interactive)
                        (find-file file))
                      (lambda ()
                        (message "Press RET to open"))
                      ;; No link face for image (no underline).
                      nil)
                   ;; Not an image, insert as normal text
                   (agent-shell-ui-add-action-to-text
                    (if (and agent-cwd (file-in-directory-p file agent-cwd))
                        ;; File within project, shorten path.
                        (propertize (concat "@" (file-relative-name file agent-cwd))
                                    'pointer 'hand)
                      (propertize (concat "@" file)
                                  'pointer 'hand))
                    (lambda ()
                      (interactive)
                      (find-file file))
                    (lambda ()
                      (message "Press RET to open"))
                    'link)))
               files
               "\n\n")))

(defun agent-shell-send-file (&optional prompt-for-file pick-shell)
  "Insert a file into `agent-shell'.

If visiting a file, send this file.

If invoked from shell, select a project file.

If invoked from `dired', use selection or region files.

With prefix argument PROMPT-FOR-FILE, always prompt for file selection.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive "P")
  (if (and (region-active-p)
           (buffer-file-name))
      (agent-shell-send-region)
    (let* ((in-shell (derived-mode-p 'agent-shell-mode))
           (files (if (or in-shell prompt-for-file)
                      (list (completing-read "Send file: " (agent-shell--project-files)))
                    (or (agent-shell--buffer-files)
                        (when (buffer-file-name)
                          (list (buffer-file-name)))
                        (list (completing-read "Send file: " (agent-shell--project-files)))
                        (user-error "No file to send"))))
           (shell-buffer (when pick-shell
                           (completing-read "Send file to shell: "
                                            (mapcar #'buffer-name (or (agent-shell-buffers)
                                                                      (user-error "No shells available")))
                                            nil t))))
      (agent-shell-insert :text (agent-shell--get-files-context :files files)
                          :shell-buffer shell-buffer))))

(defun agent-shell-send-file-to (&optional prompt-for-file)
  "Like `agent-shell-send-file' but prompt for which shell to use.

With prefix argument PROMPT-FOR-FILE, always prompt for file selection."
  (interactive "P")
  (agent-shell-send-file prompt-for-file t))

(cl-defun agent-shell--buffer-files (&key obvious)
  "Return buffer file(s) or `dired' selected file(s).

Buffer filename is OBVIOUS if its an image."
  (if (and obvious
           (buffer-file-name)
           (image-supported-file-p (buffer-file-name)))
      (list (buffer-file-name))
    (or
     (agent-shell--dired-paths-in-region)
     (dired-get-marked-files))))

(defun agent-shell--dired-paths-in-region ()
  "If `dired' buffer, return region files.  nil otherwise."
  (when (and (equal major-mode 'dired-mode)
             (use-region-p))
    (let ((start (region-beginning))
          (end (region-end))
          (paths))
      (save-excursion
        (save-restriction
          (goto-char start)
          (while (< (point) end)
            ;; Skip non-file lines.
            (while (and (< (point) end) (dired-between-files))
              (forward-line 1))
            (when (dired-get-filename nil t)
              (setq paths (append paths (list (dired-get-filename nil t)))))
            (forward-line 1))))
      paths)))

(defalias 'agent-shell-insert-file #'agent-shell-send-file)

(defalias 'agent-shell-send-current-file #'agent-shell-send-file)

(defun agent-shell-send-other-file ()
  "Prompt to send a file into `agent-shell'.

Always prompts for file selection, even if a current file is available."
  (interactive)
  (agent-shell-send-file t))

(defun agent-shell-send-screenshot (&optional pick-shell)
  "Capture a screenshot and insert it into `agent-shell'.

The screenshot is saved to .agent-shell/screenshots in the project root.
The captured screenshot file path is then inserted into the shell prompt.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive)
  (let* ((screenshots-dir (expand-file-name ".agent-shell/screenshots" (agent-shell-cwd)))
         (screenshot-path (agent-shell--capture-screenshot :destination-dir screenshots-dir))
         (shell-buffer (when pick-shell
                         (completing-read "Send screenshot to shell: "
                                          (mapcar #'buffer-name (or (agent-shell-buffers)
                                                                    (user-error "No shells available")))
                                          nil t))))
    (agent-shell-insert
     :text (agent-shell--get-files-context :files (list screenshot-path))
     :shell-buffer shell-buffer)))

(defun agent-shell-send-screenshot-to ()
  "Like `agent-shell-send-screenshot' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-screenshot t))

(defun agent-shell-send-clipboard-image (&optional pick-shell)
  "Paste clipboard image and insert it into `agent-shell'.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details.

The image is saved to .agent-shell/screenshots in the project root.
The saved image file path is then inserted into the shell prompt.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive)
  (let* ((screenshots-dir (expand-file-name ".agent-shell/screenshots" (agent-shell-cwd)))
         (image-path (agent-shell--save-clipboard-image :destination-dir screenshots-dir))
         (shell-buffer (when pick-shell
                         (completing-read "Send image to shell: "
                                          (mapcar #'buffer-name (or (agent-shell-buffers)
                                                                    (user-error "No shells available")))
                                          nil t))))
    (agent-shell-insert
     :text (agent-shell--get-files-context :files (list image-path))
     :shell-buffer shell-buffer)))

(defun agent-shell-send-clipboard-image-to ()
  "Like `agent-shell-send-clipboard-image' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-clipboard-image t))

(defun agent-shell-yank-dwim (&optional arg)
  "Yank or paste clipboard image into `agent-shell'.

If the clipboard contains an image, save it and insert as file context.
Otherwise, invoke `yank' with ARG as usual.

Needs external utilities.  See `agent-shell-clipboard-image-handlers'
for details."
  (interactive "*P")
  (let* ((screenshots-dir (expand-file-name ".agent-shell/screenshots" (agent-shell-cwd)))
         (image-path (agent-shell--save-clipboard-image :destination-dir screenshots-dir
                                                        :no-error t)))
    (if image-path
        (agent-shell-insert
         :text (agent-shell--get-files-context :files (list image-path))
         :shell-buffer (agent-shell--shell-buffer))
      (yank arg))))

;;; Region

(cl-defun agent-shell--insert-to-shell-buffer (&key shell-buffer text submit no-focus)
  "Insert TEXT into the agent shell buffer at `point-max'.

SHELL-BUFFER, when non-nil, specifies the target shell buffer.
Otherwise, uses `agent-shell--shell-buffer' to find one.

SUBMIT, when non-nil, submits the shell buffer after insertion.

NO-FOCUS, when non-nil, avoid focusing shell on insertion.

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))"
  (unless text
    (user-error "No text provided to insert"))
  (let* ((shell-buffer (or shell-buffer
                           (agent-shell--shell-buffer :no-create t))))
    (if (with-current-buffer shell-buffer
          (or (map-nested-elt agent-shell--state '(:session :id))
              (eq agent-shell-session-strategy 'new-deferred)))
        ;; Displaying before with-current-buffer below
        ;; ensures window is selected, thus window-point
        ;; is also updated after insertion.
        (let* ((inhibit-read-only t)
               (insert-start (if no-focus
                                 (with-current-buffer shell-buffer
                                   (point-max))
                               (agent-shell--display-buffer shell-buffer)
                               (point-max)))
               (insert-end nil))
          (with-current-buffer shell-buffer
            (when (shell-maker-busy)
              (user-error "Busy, try later"))
            (save-excursion
              (save-restriction
                (goto-char insert-start)
                (unless submit
                  (insert "\n\n"))
                (insert text)
                (setq insert-end (point))
                (narrow-to-region insert-start insert-end)
                (let ((markdown-overlays-highlight-blocks agent-shell-highlight-blocks))
                  (markdown-overlays-put))))
            (when submit
              (shell-maker-submit)))
          `((:buffer . ,shell-buffer)
            (:start . ,insert-start)
            (:end . ,insert-end)))
      (let ((token nil))
        (setq token
              (agent-shell-subscribe-to
               :shell-buffer shell-buffer
               :event 'prompt-ready
               :on-event (lambda (_event)
                           (agent-shell-unsubscribe :subscription token)
                           (agent-shell--insert-to-shell-buffer
                            :text text :submit submit
                            :no-focus no-focus :shell-buffer shell-buffer))))))))

(cl-defun agent-shell-insert (&key text submit no-focus shell-buffer)
  "Insert TEXT into the agent shell at `point-max'.

SUBMIT, when non-nil, submits the shell buffer after insertion.

NO-FOCUS, when non-nil, avoid focusing shell on insertion.

Use SHELL-BUFFER for insertion.

When `agent-shell-prefer-viewport-interaction' is non-nil, prefer inserting
into the viewport compose buffer instead of the shell buffer.  If no compose
buffer exists, one will be created.

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))

Uses optional SHELL-BUFFER to make paths relative to shell project."
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport--show-buffer :append text :submit submit
                                         :no-focus no-focus :shell-buffer shell-buffer)
    (agent-shell--insert-to-shell-buffer :text text :submit submit
                                         :no-focus no-focus :shell-buffer shell-buffer)))

(cl-defun agent-shell-send-region (&optional pick-shell)
  "Send region to last accessed shell buffer in project.

When PICK-SHELL is non-nil, prompt for which shell buffer to use."
  (interactive)
  (let ((shell-buffer (or (when pick-shell
                            (completing-read "Send region to shell: "
                                             (mapcar #'buffer-name (or (agent-shell-buffers)
                                                                       (user-error "No shells available")))
                                             nil t))
                          (agent-shell--shell-buffer))))
    (agent-shell-insert
     :text (agent-shell--get-region-context
            :deactivate t
            :agent-cwd (with-current-buffer shell-buffer
                         (agent-shell-cwd)))
     :shell-buffer shell-buffer)))

(defun agent-shell-send-region-to ()
  "Like `agent-shell-send-region' but prompt for which shell to use."
  (interactive)
  (agent-shell-send-region t))

(cl-defun agent-shell-send-dwim (&optional arg)
  "Send region or error at point to last accessed shell buffer in project.

With \\[universal-argument] prefix ARG, force start a new shell.

With \\[universal-argument] \\[universal-argument] prefix ARG, prompt to pick an existing shell."
  (interactive "P")
  (let ((shell-buffer
         (cond
          ((equal arg '(16))
           (agent-shell--dwim :switch-to-shell t)
           (agent-shell--shell-buffer))
          ((equal arg '(4))
           (agent-shell--dwim :new-shell t)
           (agent-shell--shell-buffer))
          (t
           (agent-shell--shell-buffer)))))
    (agent-shell-insert :text (agent-shell--context :shell-buffer shell-buffer)
                        :shell-buffer shell-buffer)))

(cl-defun agent-shell--get-region-context (&key deactivate no-error agent-cwd)
  "Get region as insertable text, ready for sending to agent.

When DEACTIVATE is non-nil, deactivate region.

When NO-ERROR is non-nil, return nil and continue without error.

Uses AGENT-CWD to shorten file paths where necessary."
  (let* ((region (or (agent-shell--get-region :deactivate deactivate)
                     (unless no-error
                       (user-error "No region selected"))))
         (processed-text (if (map-elt region :file)
                             (let ((file-link (agent-shell-ui-add-action-to-text
                                               (format "%s:%d-%d"
                                                       (if (and agent-cwd (file-in-directory-p (map-elt region :file) agent-cwd))
                                                           (file-relative-name (map-elt region :file) agent-cwd)
                                                         (map-elt region :file))
                                                       (map-elt region :line-start)
                                                       (map-elt region :line-end))
                                               (lambda ()
                                                 (interactive)
                                                 (if (and (map-elt region :file) (file-exists-p (map-elt region :file)))
                                                     (if-let ((window (when (get-file-buffer (map-elt region :file))
                                                                        (get-buffer-window (get-file-buffer (map-elt region :file))))))
                                                         (progn
                                                           (select-window window)
                                                           (goto-char (point-min))
                                                           (forward-line (1- (map-elt region :line-start)))
                                                           (beginning-of-line)
                                                           (push-mark (save-excursion
                                                                        (goto-char (point-min))
                                                                        (forward-line (1- (map-elt region :line-end)))
                                                                        (end-of-line)
                                                                        (point))
                                                                      t t))
                                                       (find-file (map-elt region :file))
                                                       (goto-char (point-min))
                                                       (forward-line (1- (map-elt region :line-start)))
                                                       (beginning-of-line)
                                                       (push-mark (save-excursion
                                                                    (goto-char (point-min))
                                                                    (forward-line (1- (map-elt region :line-end)))
                                                                    (end-of-line)
                                                                    (point))
                                                                  t t))
                                                   (message "File not found")))
                                               (lambda ()
                                                 (message "Press RET to open file"))
                                               'link))
                                   (numbered-preview
                                    (when-let ((buffer (get-file-buffer (map-elt region :file))))
                                      (let ((char-start (map-elt region :char-start))
                                            (char-end (map-elt region :char-end))
                                            (max-preview-lines 5))
                                        (if (equal (line-number-at-pos char-start)
                                                   (line-number-at-pos char-end))
                                            ;; Same line region? Avoid numbering.
                                            (buffer-substring char-start char-end)
                                          (agent-shell--get-numbered-region
                                           :buffer buffer
                                           :from char-start
                                           :to char-end
                                           :cap max-preview-lines))))))
                               (if numbered-preview
                                   (concat file-link "\n\n" numbered-preview)
                                 file-link))
                           (map-elt region :content))))
    processed-text))

(cl-defun agent-shell--get-numbered-region (&key buffer from to cap)
  "Get region from BUFFER between FROM and TO locations.

Expands to include entire lines.  Trims empty lines from beginning and end.

If CAP is non-nil, truncate at CAP."
  (with-current-buffer buffer
    (save-excursion
      (goto-char from)
      (let* ((start-line (line-number-at-pos from))
             (end-line (line-number-at-pos to))
             (lines '())
             (current-line start-line))
        (goto-char (point-min))
        (forward-line (1- start-line))
        (while (<= current-line end-line)
          (let ((line-content (buffer-substring
                               (line-beginning-position)
                               (line-end-position))))
            (push (format "   %d: %s" current-line line-content)
                  lines))
          (forward-line 1)
          (setq current-line (1+ current-line)))
        ;; Reverse the lines and trim empty lines from start and end
        (let ((reversed-lines (nreverse lines)))
          ;; Trim empty lines from the beginning
          (while (and reversed-lines
                      (string-match-p "^   [0-9]+:[[:space:]]*$" (car reversed-lines)))
            (setq reversed-lines (cdr reversed-lines)))
          ;; Trim empty lines from the end
          (setq reversed-lines (nreverse reversed-lines))
          (while (and reversed-lines
                      (string-match-p "^   [0-9]+:[[:space:]]*$" (car reversed-lines)))
            (setq reversed-lines (cdr reversed-lines)))
          ;; Reverse back to correct order and apply cap before final join
          (let ((final-lines (nreverse reversed-lines)))
            ;; Apply cap if specified
            (when (and cap (> (length final-lines) cap))
              (setq final-lines (append (seq-take final-lines cap) '("   ..."))))
            (string-join final-lines "\n")))))))

(cl-defun agent-shell--format-diagnostic (&key buffer beg end line col type text)
  "Format a diagnostic error with context.
BUFFER is the buffer containing the error.
BEG and END are the error region positions.
LINE and COL are the line and column numbers.
TYPE is the error type/level.
TEXT is the error message."
  (let* ((file (agent-shell--shorten-paths (buffer-file-name buffer) t))
         (code (when (and beg end)
                 (with-current-buffer buffer
                   (buffer-substring beg end))))
         (context-lines 3)
         (context (when beg
                    (with-current-buffer buffer
                      (save-excursion
                        (goto-char beg)
                        (let* ((start-line (max 1 (- line context-lines)))
                               (context-beg (progn
                                              (goto-char (point-min))
                                              (forward-line (1- start-line))
                                              (point)))
                               (context-end (progn
                                              (forward-line (+ context-lines context-lines 1))
                                              (point)))
                               (numbered-region (agent-shell--get-numbered-region
                                                 :buffer buffer
                                                 :from context-beg
                                                 :to context-end))
                               ;; Replace the line number prefix for the error line
                               (error-line-prefix (format "   %d:" line))
                               (highlight-prefix (format "-> %d:" line)))
                          (replace-regexp-in-string
                           (regexp-quote error-line-prefix)
                           highlight-prefix
                           numbered-region
                           nil 'literal)))))))
    (if (or (not code) (string-empty-p (string-trim code)))
        (format "%s:%d:%d: %s: %s"
                (or file (buffer-name buffer))
                line (or col 0) type text)
      (format "%s:%d:%d: %s: %s\n\n%s"
              (or file (buffer-name buffer))
              line (or col 0) type text context))))

(defun agent-shell--get-flymake-error-context ()
  "Get flymake error at point, ready for sending to agent."
  (when-let ((diagnostics (flymake-diagnostics (point))))
    (mapconcat
     (lambda (diagnostic)
       (let* ((buffer (flymake-diagnostic-buffer diagnostic))
              (beg (flymake-diagnostic-beg diagnostic))
              (end (flymake-diagnostic-end diagnostic))
              (type (flymake-diagnostic-type diagnostic))
              (text (flymake-diagnostic-text diagnostic))
              (line (with-current-buffer buffer
                      (line-number-at-pos beg)))
              (col (with-current-buffer buffer
                     (save-excursion
                       (goto-char beg)
                       (current-column)))))
         (agent-shell--format-diagnostic
          :buffer buffer
          :beg beg
          :end end
          :line line
          :col col
          :type type
          :text text)))
     diagnostics
     "\n\n")))

(defun agent-shell--get-flycheck-error-context ()
  "Get flycheck error at point, ready for sending to agent."
  (when-let (((bound-and-true-p flycheck-mode))
             ((fboundp 'flycheck-overlay-errors-at))
             (errors (flycheck-overlay-errors-at (point))))
    (mapconcat
     (lambda (err)
       (let* ((buffer (current-buffer))
              (beg (flycheck-error-pos err))
              (end (when beg
                     (save-excursion
                       (goto-char beg)
                       (if-let ((end-line (flycheck-error-end-line err))
                                (end-col (flycheck-error-end-column err)))
                           (progn
                             (forward-line (- end-line (line-number-at-pos)))
                             (move-to-column end-col)
                             (point))
                         beg))))
              (type (flycheck-error-level err))
              (text (flycheck-error-message err))
              (line (flycheck-error-line err))
              (col (flycheck-error-column err)))
         (agent-shell--format-diagnostic
          :buffer buffer
          :beg beg
          :end end
          :line line
          :col col
          :type type
          :text text)))
     errors
     "\n\n")))

(defun agent-shell--get-error-context ()
  "Get error at point from either flymake or flycheck, whichever is available.
Tries flymake first, then flycheck."
  (or (agent-shell--get-flymake-error-context)
      (agent-shell--get-flycheck-error-context)))

(cl-defun agent-shell--get-current-line-context (&key agent-cwd)
  "Get the current line as insertable text, ready for sending to agent.

Uses AGENT-CWD to shorten file paths where necessary."
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (goto-char start)
      (set-mark end)
      (activate-mark)
      (agent-shell--get-region-context :deactivate t :no-error t :agent-cwd agent-cwd))))

(cl-defun agent-shell--context (&key shell-buffer)
  "Return context (if available).  Nil otherwise.

Uses optional SHELL-BUFFER to make paths relative to shell project.

Context could be either a region or error at point or files.
The sources checked are controlled by `agent-shell-context-sources'."
  (unless (and (derived-mode-p 'agent-shell-mode)
               (not (region-active-p)))
    (let ((agent-cwd (when shell-buffer
                       (with-current-buffer shell-buffer
                         (agent-shell-cwd)))))
      (seq-some
       (lambda (source)
         (pcase source
           ('files (agent-shell--get-files-context
                    :files (agent-shell--buffer-files :obvious t)
                    :agent-cwd agent-cwd))
           ('region (agent-shell--get-region-context
                     :deactivate t :no-error t
                     :agent-cwd agent-cwd))
           ('error (agent-shell--get-error-context))
           ('line (agent-shell--get-current-line-context
                   :agent-cwd agent-cwd))
           ((pred functionp) (funcall source))))
       agent-shell-context-sources))))

(cl-defun agent-shell--get-region (&key deactivate)
  "Get the active region as an alist.

When DEACTIVATE is non-nil, deactivate region/selection.

Available values:

 :file :language :char-start :char-end :line-start :line-end and :content."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end))
          (content (buffer-substring-no-properties (region-beginning) (region-end)))
          (language (string-remove-suffix "-mode" (string-remove-suffix "-ts-mode" (symbol-name major-mode))))
          (file (buffer-file-name)))
      (when deactivate
        (deactivate-mark))
      `((:file . ,file)
        (:language . ,language)
        (:char-start . ,start)
        (:char-end . ,end)
        (:line-start . ,(save-excursion (goto-char start) (line-number-at-pos)))
        (:line-end . ,(save-excursion (goto-char end) (line-number-at-pos)))
        (:content . ,content)))))

(cl-defun agent-shell--get-decorated-region (&key deactivate)
  "Get the active region decorated with file path and Markdown code block.

When DEACTIVATE is non-nil, deactivate region/selection."
  (when-let ((region-data (agent-shell--get-region :deactivate deactivate)))
    (let ((file (map-elt region-data :file))
          (start (map-elt region-data :char-start))
          (end (map-elt region-data :char-end))
          (language (map-elt region-data :language))
          (content (map-elt region-data :content)))
      (concat (if file
                  (format "%s#C%d-C%d\n\n" file start end)
                "")
              "```"
              language
              "\n"
              content
              "\n"
              "```"))))

;;; Session modes

(defun agent-shell--busy-indicator-frame ()
  "Return busy frame string or nil if not busy."
  (when-let* ((agent-shell-show-busy-indicator)
              ((eq 'busy (map-nested-elt (agent-shell--state) '(:heartbeat :status))))
              (frames (pcase agent-shell-busy-indicator-frames
                        ('wave '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂"))
                        ('dots-block '("⣷" "⣯" "⣟" "⡿" "⢿" "⣻" "⣽" "⣾"))
                        ('dots-round '("⢎⡰" "⢎⡡" "⢎⡑" "⢎⠱" "⠎⡱" "⢊⡱" "⢌⡱" "⢆⡱"))
                        ('wide '("░   " "░░  " "░░░ " "░░░░" "░░░ " "░░  " "░   " "    "))
                        ((pred listp) agent-shell-busy-indicator-frames)
                        (_ '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" "▇" "▆" "▅" "▄" "▃" "▂")))))
    (concat " " (seq-elt frames (mod (map-nested-elt (agent-shell--state) '(:heartbeat :value))
                                     (length frames))))))

(defun agent-shell--mode-line-format ()
  "Return `agent-shell''s mode-line format.

Typically includes the container indicator, model, session mode and activity
or nil if unavailable.

For example: \" [C] [Sonnet] [Accept Edits] ░░░ \".
Shows \" [C]\" when running in a container."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              ((memq agent-shell-header-style '(text none nil))))
    (concat (when agent-shell-container-command-runner
              (propertize " [C]"
                          'face 'font-lock-constant-face
                          'help-echo "Running in container"))
            (when-let ((model-name (or (map-elt (seq-find (lambda (model)
                                                            (string= (map-elt model :model-id)
                                                                     (map-nested-elt (agent-shell--state) '(:session :model-id))))
                                                          (map-nested-elt (agent-shell--state) '(:session :models)))
                                                :name)
                                       (map-nested-elt (agent-shell--state) '(:session :model-id)))))
              (propertize (format " [%s]" model-name)
                          'face 'font-lock-variable-name-face
                          'help-echo (format "Model: %s" model-name)))
            (when-let ((mode-name (agent-shell--resolve-session-mode-name
                                   (map-nested-elt (agent-shell--state) '(:session :mode-id))
                                   (agent-shell--get-available-modes (agent-shell--state)))))
              (propertize (format " [%s]" mode-name)
                          'face 'font-lock-type-face
                          'help-echo (format "Session Mode: %s" mode-name)))
            (when-let ((indicator (agent-shell--context-usage-indicator)))
              (concat " " indicator))
            (agent-shell--busy-indicator-frame))))

(defun agent-shell--setup-modeline ()
  "Set up the modeline to display session mode.
Uses :eval so the mode updates automatically when state changes."
  (setq-local mode-line-misc-info
              (append mode-line-misc-info
                      '((:eval (agent-shell--mode-line-format))))))

(defun agent-shell-cycle-session-mode (&optional on-success)
  "Cycle through available session modes for the current `agent-shell' session.

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-modes (agent-shell--state))
    (user-error "No session modes available"))
  (let* ((mode-ids (mapcar (lambda (mode)
                             (map-elt mode :id))
                           (agent-shell--get-available-modes (agent-shell--state))))
         (mode-idx (or (seq-position mode-ids
                                     (map-nested-elt (agent-shell--state) '(:session :mode-id))
                                     #'string=) -1))
         (next-mode-idx (mod (1+ mode-idx) (length mode-ids)))
         (next-mode-id (nth next-mode-idx mode-ids)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id next-mode-id)
     :buffer (current-buffer)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id next-mode-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Session mode: %s"
                              (agent-shell--resolve-session-mode-name
                               next-mode-id
                               (agent-shell--get-available-modes (agent-shell--state)))))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change session mode: %s" error)))))

(defun agent-shell-set-session-mode (&optional on-success)
  "Set session mode (if any available).

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (agent-shell--get-available-modes (agent-shell--state))
    (user-error "No session modes available"))
  (let* ((current-mode-id (map-nested-elt (agent-shell--state) '(:session :mode-id)))
         (default-mode-name (and current-mode-id
                                 (agent-shell--resolve-session-mode-name
                                  current-mode-id
                                  (agent-shell--get-available-modes (agent-shell--state)))))
         (mode-choices (mapcar (lambda (mode)
                                 (cons (map-elt mode :name)
                                       (map-elt mode :id)))
                               (agent-shell--get-available-modes (agent-shell--state))))
         (selection (completing-read "Set session mode: "
                                     (mapcar #'car mode-choices)
                                     nil t nil nil default-mode-name))
         (selected-mode-id (cdr (seq-find (lambda (choice)
                                            (string= selection (car choice)))
                                          mode-choices))))
    (unless selected-mode-id
      (user-error "Unknown session mode: %s" selection))
    (when (and current-mode-id (string= selected-mode-id current-mode-id))
      (error "Session mode already %s" selection))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-mode-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :mode-id selected-mode-id)
     :buffer (current-buffer)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :mode-id selected-mode-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Session mode: %s"
                              (agent-shell--resolve-session-mode-name
                               selected-mode-id
                               (agent-shell--get-available-modes (agent-shell--state)))))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change session mode: %s" error)))))

(defun agent-shell-set-session-model (&optional on-success)
  "Set session model.

Optionally, get notified of completion with ON-SUCCESS function."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (map-nested-elt (agent-shell--state) '(:session :id))
    (user-error "No active session"))
  (unless (map-nested-elt (agent-shell--state) '(:session :models))
    (user-error "No session models available"))
  (let* ((current-model-id (map-nested-elt (agent-shell--state) '(:session :model-id)))
         (available-models (map-nested-elt (agent-shell--state) '(:session :models)))
         (default-model-name (and current-model-id
                                  (map-elt (seq-find (lambda (model)
                                                       (string= (map-elt model :model-id) current-model-id))
                                                     available-models)
                                           :name)))
         (model-choices (seq-mapn (lambda (title model)
                                    (cons title (map-elt model :model-id)))
                                  (agent-shell--align-alist
                                   :data available-models
                                   :columns (list
                                             (lambda (model)
                                               (map-elt model :name))
                                             (lambda (model)
                                               (format "(%s)" (map-elt model :model-id)))))
                                  available-models))
         (selection (completing-read "Set model: "
                                     (mapcar #'car model-choices)
                                     nil t nil nil
                                     (and default-model-name
                                          (car (seq-find (lambda (choice)
                                                           (string-prefix-p default-model-name (car choice)))
                                                         model-choices)))))
         (selected-model-id (cdr (seq-find (lambda (choice)
                                             (string= selection (car choice)))
                                           model-choices))))
    (unless selected-model-id
      (user-error "Unknown model: %s" selection))
    (when (and current-model-id (string= selected-model-id current-model-id))
      (error "Session model already %s" (map-elt (seq-find (lambda (model)
                                                             (string= (map-elt model :model-id) selected-model-id))
                                                           available-models)
                                                 :name)))
    (acp-send-request
     :client (map-elt (agent-shell--state) :client)
     :request (acp-make-session-set-model-request
               :session-id (map-nested-elt (agent-shell--state) '(:session :id))
               :model-id selected-model-id)
     :on-success (lambda (_response)
                   (let ((updated-session (map-elt (agent-shell--state) :session)))
                     (map-put! updated-session :model-id selected-model-id)
                     (map-put! (agent-shell--state) :session updated-session)
                     (message "Model: %s"
                              (map-elt (seq-find (lambda (model)
                                                   (string= (map-elt model :model-id) selected-model-id))
                                                 (map-nested-elt (agent-shell--state) '(:session :models)))
                                       :name)))
                   (agent-shell--update-header-and-mode-line)
                   (when on-success
                     (funcall on-success)))
     :on-failure (lambda (error _raw-message)
                   (message "Failed to change model: %s" error)))))

(defun agent-shell--format-available-modes (modes)
  "Format MODES for shell rendering.
If CURRENT-MODE-ID is provided, append \"(current)\" to the matching mode name."
  (agent-shell--align-alist
   :data modes
   :columns (list
             (lambda (mode)
               (when (map-elt mode :name)
                 (propertize (format "%s (%s)"
                                     (map-elt mode :name)
                                     (map-elt mode :id))
                             'font-lock-face 'font-lock-function-name-face)))
             (lambda (mode)
               (when (map-elt mode :description)
                 (propertize (map-elt mode :description)
                             'font-lock-face 'font-lock-comment-face))))
   :joiner "\n"))

(defun agent-shell--format-available-models (models)
  "Format MODELS for shell rendering.

Mark model using CURRENT-MODEL-ID."
  (agent-shell--align-alist
   :data models
   :columns (list
             (lambda (model)
               (concat
                (when (map-elt model :name)
                  (propertize (map-elt model :name)
                              'font-lock-face 'font-lock-function-name-face))
                (when (map-elt model :model-id)
                  (propertize (format " (%s)" (map-elt model :model-id))
                              'font-lock-face 'font-lock-function-name-face))))
             (lambda (model)
               (when (map-elt model :description)
                 (propertize (map-elt model :description)
                             'font-lock-face 'font-lock-comment-face))))
   :joiner "\n"))

;;; Transient

(transient-define-prefix agent-shell-help-menu ()
  "Transient menu for `agent-shell' commands."
  [["Navigation"
    ("<tab>" "Next item" agent-shell-next-item :transient t)
    ("<backtab>" "Previous item" agent-shell-previous-item :transient t)]
   ["Insert"
    ("!" "Shell command" agent-shell-insert-shell-command-output :transient t)
    ("@" "File" agent-shell-insert-file :transient t)
    ("d" "Dwim" agent-shell-send-dwim :transient t)
    ]]
  [["Session"
    ("m" "Cycle modes" agent-shell-cycle-session-mode :transient t)
    ("M" "Set mode" agent-shell-set-session-mode :transient t)
    ("v" "Set model" agent-shell-set-session-model :transient t)
    ("C" "Interrupt" agent-shell-interrupt :transient t)]
   ["Shell"
    ("b" "Toggle" agent-shell-toggle :transient t)
    ("N" "New shell" agent-shell-new-shell)]])

;;; Transcript

(defcustom agent-shell-transcript-file-path-function #'agent-shell--default-transcript-file-path
  "Function to generate the full transcript file path.
Called with no arguments, should return a string path or nil to disable.
When nil, transcript saving is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Custom function"))
  :group 'agent-shell)

(defun agent-shell--default-transcript-file-path ()
  "Generate a transcript file path in project root.

For example:

 project/.agent-shell/transcripts/."
  (let* ((dir (expand-file-name ".agent-shell/transcripts" (agent-shell-cwd)))
         (filename (format-time-string "%F-%H-%M-%S.md"))
         (filepath (expand-file-name filename dir)))
    filepath))

(defun agent-shell--transcript-file-path ()
  "Return the transcript file path, or nil if disabled."
  (when-let ((path-fn agent-shell-transcript-file-path-function))
    (condition-case err
        (funcall path-fn)
      (error
       (message "Failed to generate transcript path: %S" err)
       nil))))

(defun agent-shell-open-transcript ()
  "Open the transcript file for the current `agent-shell' buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in an agent-shell buffer"))
  (unless agent-shell--transcript-file
    (error "No transcript file available for this buffer"))
  (unless (file-exists-p agent-shell--transcript-file)
    (error "Transcript file does not exist: %s" agent-shell--transcript-file))
  (find-file agent-shell--transcript-file))

;;; Queueing

(cl-defun agent-shell--process-pending-request ()
  "Process the next pending request from the queue if available."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when-let ((pending (map-elt agent-shell--state :pending-requests))
             (next-request (car pending)))
    (map-put! agent-shell--state :pending-requests (cdr pending))
    (agent-shell--insert-to-shell-buffer
     :text next-request
     :submit t
     :no-focus t)))

(defun agent-shell--display-pending-requests ()
  "Display pending requests in the shell buffer if queue is not empty."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (unless (seq-empty-p (map-elt agent-shell--state :pending-requests))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-pending-requests"
                       (map-elt (agent-shell--state) :request-count))
     :body (format "Pending requests: %d

%s

Resume: M-x agent-shell-resume-pending-requests
Remove: M-x agent-shell-remove-pending-request
"
                   (seq-length (map-elt agent-shell--state :pending-requests))
                   (mapconcat
                    (lambda (idx-req)
                      (let* ((req (car idx-req))
                             (idx (cdr idx-req))
                             (first-line (car (split-string req "\n" t))))
                        (format "  %d: \"%s\""
                                (1+ idx)
                                (truncate-string-to-width first-line 80 nil nil "..."))))
                    (seq-map-indexed #'cons (map-elt agent-shell--state :pending-requests))
                    "\n"))
     :create-new t)))

(cl-defun agent-shell--enqueue-request (&key prompt)
  "Add PROMPT to the pending requests queue."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let ((pending (map-elt agent-shell--state :pending-requests)))
    (map-put! agent-shell--state :pending-requests
              (append pending (list prompt)))
    (message "Request queued (%d pending)" (length (map-elt agent-shell--state :pending-requests)))))

(defun agent-shell-queue-request (prompt)
  "Queue or immediately send a request depending on shell busy state.

Read PROMPT from the minibuffer.  If the shell is busy, add it to the pending
requests queue.  Otherwise, submit it immediately.  Queued requests will be
automatically sent when the current request completes."
  (interactive
   (progn
     (unless (derived-mode-p 'agent-shell-mode)
       (error "Not in a shell"))
     (list (read-string (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                            "Enqueue request: ")))))
  (if (shell-maker-busy)
      (agent-shell--enqueue-request :prompt prompt)
    (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)))

(defun agent-shell-resume-pending-requests ()
  "Resume processing pending requests in the queue."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (when (seq-empty-p (map-elt agent-shell--state :pending-requests))
    (user-error "No pending requests"))
  (if (shell-maker-busy)
      (message "Shell is busy, requests will auto-resume when ready")
    (agent-shell--process-pending-request)))

(defun agent-shell-remove-pending-request (&optional remove-index)
  "Remove all pending requests or a specific request by REMOVE-INDEX.

When called interactively with pending requests, prompt to either remove all
or select a specific request to remove."
  (interactive
   (let ((pending (map-elt agent-shell--state :pending-requests)))
     (unless (derived-mode-p 'agent-shell-mode)
       (error "Not in a shell"))
     (when (seq-empty-p pending)
       (user-error "No pending requests"))
     (let* ((choices (append
                      '(("Remove all" . remove-all))
                      (seq-map-indexed
                       (lambda (req idx)
                         (cons (format "%d: %s" (1+ idx)
                                       (truncate-string-to-width req 60 nil nil "..."))
                               idx))
                       pending)))
            (selection (cdr (assoc (completing-read "Remove: " choices nil t) choices))))
       (list (unless (eq selection 'remove-all) selection)))))
  (if remove-index
      (when-let* ((message "Remove? \"%s\"")
                  (confirmed (y-or-n-p (format message
                                               (nth remove-index
                                                    (map-elt agent-shell--state :pending-requests)))))
                  (pending (map-elt agent-shell--state :pending-requests))
                  (new-pending (append (seq-take pending remove-index)
                                       (seq-drop pending (1+ remove-index)))))
        (map-put! agent-shell--state :pending-requests new-pending)
        (message "Removed (%d remaining)"
                 (length new-pending)))
    (when (y-or-n-p (format "Remove %d pending requests?"
                            (length (map-elt agent-shell--state :pending-requests))))
      (map-put! agent-shell--state :pending-requests nil)
      (message "Removed all pending requests"))))

(provide 'agent-shell)

;;; agent-shell.el ends here
