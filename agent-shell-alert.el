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
;; Send desktop notifications from Emacs.
;;
;; GUI Emacs on macOS:
;;
;;   Uses `ns-do-applescript' to run AppleScript's `display
;;   notification' from within the Emacs process.  Because the
;;   notification originates from Emacs itself, macOS attributes it to
;;   Emacs: the Emacs icon appears and clicking the notification
;;   activates Emacs.  No compilation, no dynamic module, no external
;;   dependencies.
;;
;;   We originally built a JIT-compiled Objective-C dynamic module
;;   (inspired by vterm's approach to vterm-module.so) that used
;;   UNUserNotificationCenter — Apple's modern notification API.  It
;;   worked perfectly on an adhoc-signed Emacs built from source, but
;;   fails with UNErrorDomain error 1 (UNErrorCodeNotificationsNotAllowed)
;;   on the Homebrew emacs-app cask build from emacsformacosx.com.
;;   Apple's documentation says no entitlement is needed for local
;;   notifications and the hardened runtime has no notification-related
;;   restrictions, so the root cause is unclear.  The investigation is
;;   tracked in x.notification-center-spiking.md and in beads issue
;;   agent-shell-4217.
;;
;;   `ns-do-applescript' turns out to give you essentially native
;;   notifications for free: Emacs-branded, no compilation step, works
;;   on every macOS Emacs build.  It uses the deprecated AppleScript
;;   notification bridge rather than UNUserNotificationCenter, but it
;;   works on current macOS versions and is the pragmatic choice until
;;   the UNUserNotificationCenter issue is resolved.
;;
;; Terminal Emacs:
;;
;;   Auto-detects the host terminal emulator and sends the appropriate
;;   OSC escape sequence: OSC 9 (iTerm2, Ghostty, WezTerm, foot,
;;   mintty, ConEmu), OSC 99 (kitty), or OSC 777 (urxvt, VTE-based
;;   terminals), with DCS passthrough for tmux (when
;;   allow-passthrough is enabled).
;;
;; Fallback:
;;
;;   Falls back to osascript on macOS when the terminal is unknown or
;;   tmux passthrough is not available.  On non-macOS platforms where
;;   the terminal is unrecognized, no OS-level notification is sent.
;;
;; Terminal detection and DCS wrapping are inspired by clipetty's
;; approach.

;;; Code:

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
                (format "display notification %S with title %S"
                        body title)))

(defun agent-shell-alert-notify (title body)
  "Send a desktop notification with TITLE and BODY.

In GUI Emacs on macOS, uses `ns-do-applescript' to run `display
notification' from within the Emacs process so the notification
is attributed to Emacs (Emacs icon, click activates Emacs).  In
terminal Emacs, auto-detects the terminal emulator and sends the
appropriate OSC escape sequence, with tmux DCS passthrough when
available.  Falls back to osascript on macOS when the terminal is
unknown or tmux passthrough is not enabled.

  (agent-shell-alert-notify \"agent-shell\" \"Turn complete\")"
  (cond
   ;; GUI Emacs on macOS: use ns-do-applescript for Emacs-branded
   ;; notifications (Emacs icon, click activates Emacs).
   ((and (eq system-type 'darwin)
         (display-graphic-p)
         (fboundp 'ns-do-applescript))
    (condition-case nil
        (ns-do-applescript
         (format "display notification %S with title %S" body title))
      (error
       (agent-shell-alert--osascript-notify title body))))
   ;; Terminal: try OSC escape sequences for terminal notifications.
   ((not (display-graphic-p))
    (if-let ((payload (agent-shell-alert--osc-payload title body))
             (wrapped (agent-shell-alert--tmux-passthrough payload)))
        (send-string-to-terminal wrapped)
      (when (eq system-type 'darwin)
        (agent-shell-alert--osascript-notify title body))))
   ;; GUI on macOS without ns-do-applescript (shouldn't happen), or
   ;; non-macOS GUI: fall back to osascript or just message.
   ((eq system-type 'darwin)
    (agent-shell-alert--osascript-notify title body))))

(provide 'agent-shell-alert)

;;; agent-shell-alert.el ends here
