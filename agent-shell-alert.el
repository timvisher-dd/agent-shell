;;; agent-shell-alert.el --- Desktop notifications via OSC and macOS native -*- lexical-binding: t; -*-

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
;; Send desktop notifications from Emacs.  In GUI mode on macOS, uses
;; native UNUserNotificationCenter via a dynamic module
;; (agent-shell-alert-mac.dylib).  In terminal mode, auto-detects the
;; host terminal emulator and sends the appropriate OSC escape
;; sequence: OSC 9 (iTerm2, Ghostty, WezTerm, foot, mintty, ConEmu),
;; OSC 99 (kitty), or OSC 777 (urxvt, VTE-based terminals),
;; with DCS passthrough for tmux (when allow-passthrough is enabled).
;; Falls back to osascript on macOS when the terminal is unknown or
;; tmux passthrough is not available.
;;
;; The JIT-compile-on-first-use pattern for the native dylib is
;; inspired by vterm's approach to vterm-module.so.  Terminal
;; detection and DCS wrapping are inspired by clipetty's approach.

;;; Code:

(require 'seq)

(declare-function agent-shell-alert-mac-notify "agent-shell-alert-mac")
(declare-function agent-shell-alert-mac-request-authorization "agent-shell-alert-mac")
(declare-function agent-shell-alert-mac-applescript-notify "agent-shell-alert-mac")

(defvar agent-shell-alert--source-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing agent-shell-alert source files.
Captured at load time so it remains correct after loading.")

(defvar agent-shell-alert--mac-authorized nil
  "Non-nil when native macOS notifications are authorized and working.")

(defvar agent-shell-alert--mac-module-tried nil
  "Non-nil after the first attempt to load the native module.
Prevents repeated compilation/load attempts on every notification.")

(defvar agent-shell-alert--osascript-warned nil
  "Non-nil after the osascript fallback warning has been shown.")

(defun agent-shell-alert--detect-terminal ()
  "Detect the host terminal emulator.

Inside tmux, TERM_PROGRAM is \"tmux\", so we query tmux's global
environment for the outer terminal.  Falls back to terminal-specific
environment variables that survive tmux session inheritance.

  ;; In iTerm2:
  (agent-shell-alert--detect-terminal)
  ;; => \"iTerm.app\"

  ;; In kitty inside tmux:
  (agent-shell-alert--detect-terminal)
  ;; => \"kitty\""
  (let ((tp (getenv "TERM_PROGRAM" (selected-frame))))
    (cond
     ((and tp (not (string= tp "tmux")))
      tp)
     ((string= tp "tmux")
      (when-let ((raw (ignore-errors
                        (string-trim
                         (shell-command-to-string
                          "tmux show-environment -g TERM_PROGRAM 2>/dev/null")))))
        (when (string-match "^TERM_PROGRAM=\\(.+\\)" raw)
          (let ((val (match-string 1 raw)))
            (unless (string= val "tmux")
              val)))))
     ((getenv "GHOSTTY_RESOURCES_DIR" (selected-frame))
      "ghostty")
     ((getenv "ITERM_SESSION_ID" (selected-frame))
      "iTerm.app")
     ((getenv "WEZTERM_EXECUTABLE" (selected-frame))
      "WezTerm")
     ((getenv "KITTY_PID" (selected-frame))
      "kitty")
     ((getenv "ConEmuPID" (selected-frame))
      "ConEmu")
     ((getenv "VTE_VERSION" (selected-frame))
      "vte")
     ((when-let ((term (getenv "TERM" (selected-frame))))
        (string-match-p "^rxvt" term))
      "urxvt")
     ((when-let ((term (getenv "TERM" (selected-frame))))
        (string-match-p "^foot" term))
      "foot")
     ((when-let ((term (getenv "TERM" (selected-frame))))
        (string-match-p "^mintty" term))
      "mintty"))))

(defun agent-shell-alert--osc-payload (title body)
  "Build the raw OSC notification payload for TITLE and BODY.

Selects the OSC protocol based on the detected terminal:
OSC 9 for iTerm2, Ghostty, WezTerm, foot, mintty, ConEmu;
OSC 99 for kitty; OSC 777 for urxvt and VTE-based terminals.
Returns nil if the terminal does not support OSC notifications.

  (agent-shell-alert--osc-payload \"Done\" \"Task finished\")
  ;; => \"\\e]9;Task finished\\e\\\\\"  (in iTerm2)

  (agent-shell-alert--osc-payload \"Done\" \"Task finished\")
  ;; => nil  (in Apple Terminal)"
  (let ((terminal (agent-shell-alert--detect-terminal)))
    (pcase terminal
      ("kitty"
       (format "\e]99;i=1:d=0;%s\e\\\e]99;i=1:p=body;%s\e\\" title body))
      ;; Extend these lists as users report supported terminals.
      ((or "urxvt" "vte")
       (format "\e]777;notify;%s;%s\e\\" title body))
      ((or "iTerm.app" "ghostty" "WezTerm" "foot" "mintty" "ConEmu")
       (format "\e]9;%s\e\\" body)))))

(defun agent-shell-alert--tmux-allow-passthrough-p ()
  "Return non-nil if tmux has allow-passthrough enabled.

  ;; With `set -g allow-passthrough on':
  (agent-shell-alert--tmux-allow-passthrough-p)
  ;; => t"
  (when-let ((out (ignore-errors
                    (string-trim
                     (shell-command-to-string
                      "tmux show-option -gv allow-passthrough 2>/dev/null")))))
    (string= out "on")))

(defun agent-shell-alert--tmux-passthrough (seq)
  "Wrap SEQ in tmux DCS passthrough if inside tmux.

Returns SEQ unchanged outside tmux.  Returns nil if inside tmux
but allow-passthrough is not enabled, signaling the caller to
fall back to osascript.

  ;; Inside tmux with passthrough enabled:
  (agent-shell-alert--tmux-passthrough \"\\e]9;hi\\e\\\\\")
  ;; => \"\\ePtmux;\\e\\e]9;hi\\e\\\\\\e\\\\\"

  ;; Outside tmux:
  (agent-shell-alert--tmux-passthrough \"\\e]9;hi\\e\\\\\")
  ;; => \"\\e]9;hi\\e\\\\\""
  (if (not (getenv "TMUX" (selected-frame)))
      seq
    (when (agent-shell-alert--tmux-allow-passthrough-p)
      (let ((escaped (replace-regexp-in-string "\e" "\e\e" seq t t)))
        (concat "\ePtmux;" escaped "\e\\")))))

(defun agent-shell-alert--osascript-notify (title body)
  "Send a macOS notification via osascript as a fallback.

TITLE and BODY are the notification title and message.

  (agent-shell-alert--osascript-notify \"agent-shell\" \"Done\")"
  (unless agent-shell-alert--osascript-warned
    (setq agent-shell-alert--osascript-warned t)
    (message "agent-shell-alert: using osascript for notifications.\
 For native terminal notifications:")
    (message "  - Use a terminal that supports OSC 9 \
(iTerm2, Ghostty, WezTerm) or OSC 99 (Kitty)")
    (when (getenv "TMUX" (selected-frame))
      (message "  - Enable tmux passthrough: \
set -g allow-passthrough on")))
  (call-process "osascript" nil 0 nil
                "-e"
                (format "tell application \"Emacs\" to \
display notification %S with title %S"
                        body title)))

(defun agent-shell-alert--mac-available-p ()
  "Return non-nil if native macOS notifications are authorized and working."
  (and (eq system-type 'darwin)
       (display-graphic-p)
       (fboundp 'agent-shell-alert-mac-notify)
       agent-shell-alert--mac-authorized))

(defun agent-shell-alert--source-directory ()
  "Return the directory containing agent-shell-alert source files."
  agent-shell-alert--source-dir)

(defun agent-shell-alert--module-path ()
  "Return the expected path of the compiled native module."
  (expand-file-name
   (concat "agent-shell-alert-mac" module-file-suffix)
   (agent-shell-alert--source-directory)))

(defun agent-shell-alert--compile-mac-module ()
  "Compile the macOS native notification module.
Returns non-nil on success."
  (let* ((source (expand-file-name "agent-shell-alert-mac.m"
                                   (agent-shell-alert--source-directory)))
         (output (agent-shell-alert--module-path))
         (emacs-dir (file-name-directory
                     (directory-file-name invocation-directory)))
         (include-dir
          (seq-find
           (lambda (d) (file-exists-p (expand-file-name "emacs-module.h" d)))
           (list (expand-file-name "include" emacs-dir)
                 (expand-file-name "Resources/include" emacs-dir)
                 (expand-file-name "../include" invocation-directory)))))
    (when (and (file-exists-p source) include-dir)
      (zerop
       (call-process "cc" nil nil nil
                     "-Wall" "-O2" "-fPIC"
                     "-shared" "-fobjc-arc"
                     (concat "-I" include-dir)
                     "-framework" "UserNotifications"
                     "-framework" "Foundation"
                     "-o" output source)))))

(defun agent-shell-alert--try-load-mac-module ()
  "Try to load the macOS native notification module, compiling if needed.
Returns non-nil on success."
  (setq agent-shell-alert--mac-module-tried t)
  (when (and (eq system-type 'darwin)
             (display-graphic-p)
             module-file-suffix
             (not (fboundp 'agent-shell-alert-mac-notify)))
    (unless (file-exists-p (agent-shell-alert--module-path))
      (ignore-errors (agent-shell-alert--compile-mac-module)))
    (ignore-errors
      (module-load (agent-shell-alert--module-path)))
    (if (fboundp 'agent-shell-alert-mac-notify)
        (condition-case err
            (when (agent-shell-alert-mac-request-authorization)
              (setq agent-shell-alert--mac-authorized t))
          (error
           (message "agent-shell-alert: native notifications unavailable \
(%s); falling back to osascript"
                    (error-message-string err))))
      (message "agent-shell-alert: native module unavailable; \
install Xcode command line tools (`xcode-select --install') \
then run M-x eval (agent-shell-alert--try-load-mac-module) RET \
to enable native macOS desktop notifications")))
  agent-shell-alert--mac-authorized)

(defun agent-shell-alert-notify (title body)
  "Send a desktop notification with TITLE and BODY.

In GUI Emacs on macOS, uses native notifications via
UNUserNotificationCenter.  In terminal Emacs, auto-detects the
terminal emulator and sends the appropriate OSC escape sequence,
with tmux DCS passthrough when available.  Falls back to
osascript on macOS when the terminal is unknown or tmux
passthrough is not enabled.

  (agent-shell-alert-notify \"agent-shell\" \"Turn complete\")"
  ;; Lazy-load: if the module hasn't been tried yet and we now have a
  ;; GUI frame (e.g. emacsclient connecting to a daemon), try loading.
  (when (and (not agent-shell-alert--mac-module-tried)
             (eq system-type 'darwin)
             (display-graphic-p))
    (agent-shell-alert--try-load-mac-module))
  (cond
   ((agent-shell-alert--mac-available-p)
    (condition-case nil
        (agent-shell-alert-mac-notify title body)
      (error
       (setq agent-shell-alert--mac-authorized nil)
       (agent-shell-alert-notify title body))))
   ((and (display-graphic-p)
         (eq system-type 'darwin)
         (fboundp 'agent-shell-alert-mac-applescript-notify))
    (condition-case nil
        (agent-shell-alert-mac-applescript-notify title body)
      (error
       (agent-shell-alert--osascript-notify title body))))
   ((not (display-graphic-p))
    (if-let ((payload (agent-shell-alert--osc-payload title body))
             (wrapped (agent-shell-alert--tmux-passthrough payload)))
        (send-string-to-terminal wrapped)
      (when (eq system-type 'darwin)
        (agent-shell-alert--osascript-notify title body))))
   ((eq system-type 'darwin)
    (agent-shell-alert--osascript-notify title body))))

(agent-shell-alert--try-load-mac-module)

(provide 'agent-shell-alert)

;;; agent-shell-alert.el ends here
