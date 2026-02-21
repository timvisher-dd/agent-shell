;;; agent-shell-project.el --- Project file utilities for agent-shell. -*- lexical-binding: t; -*-

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

;;; Code:

(require 'project)
(require 'subr-x)

(defvar projectile-mode)

(defcustom agent-shell-project-root-function nil
  "Optional function override for `agent-shell-project-root'.

See `agent-shell-project-root' for details on how the root is used.

When non-nil, the value must be a function called with no arguments
that returns a directory satisfying `file-accessible-directory-p'.

Examples:
- Use the current buffer directory via a function:
  (setq agent-shell-project-root-function (lambda () default-directory))
- Use a custom function:
  (setq agent-shell-project-root-function (lambda () \"~/work/\"))"
  :type '(choice (const :tag "Default" nil)
                 (function :tag "Function (no args)"))
  :group 'agent-shell)

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-current-project-files "projectile")

(defun agent-shell--project-files ()
  "Get project files using projectile or project.el."
  (cond
   ((and (boundp 'projectile-mode)
         projectile-mode
         (projectile-project-p))
    (let ((root (file-name-as-directory (expand-file-name (projectile-project-root)))))
      (mapcar (lambda (f)
                (string-remove-prefix root f))
              (projectile-current-project-files))))
   ((fboundp 'project-current)
    (when-let* ((proj (project-current))
                (root (file-name-as-directory (expand-file-name (project-root proj)))))
      (mapcar (lambda (f)
                (string-remove-prefix root f))
              (project-files proj))))
   (t nil)))

(defun agent-shell-project-root ()
  "Return the project root for this shell.

This root anchors project-scoped behavior:

- Shell grouping: used to find or prompt for a shell when the current
  buffer's project changes.
- Path shortening and file mentions: file paths are shortened relative
  to this root, and file mentions expand against it.
- Project artifacts: screenshots and transcripts are stored under the
  `.agent-shell/' directory in this root.

If `agent-shell-project-root-function' is nil, use the project root when
available, falling back to `default-directory'.  When
`agent-shell-project-root-function' is non-nil, use that override."
  (let ((project-root (if agent-shell-project-root-function
                          (funcall agent-shell-project-root-function)
               (or (when (and (boundp 'projectile-mode)
                              projectile-mode
                              (fboundp 'projectile-project-root))
                     (projectile-project-root))
                   (when (fboundp 'project-root)
                     (when-let ((proj (project-current)))
                       (project-root proj)))
                   default-directory
                   (error "No project root available")))))
    (unless (file-accessible-directory-p project-root)
      (error "Project root is not an accessible directory: %S" project-root))
    (expand-file-name project-root)))

(defun agent-shell--project-name ()
  "Return the project name for this shell.

If in a project, use project name."
  (or (when-let (((boundp 'projectile-mode))
                 projectile-mode
                 ((fboundp 'projectile-project-name))
                 (root (projectile-project-root)))
        (projectile-project-name root))
      (when-let (((fboundp 'project-name))
                 (project (project-current)))
        (project-name project))
      (file-name-nondirectory
       (string-remove-suffix "/" default-directory))))

(provide 'agent-shell-project)

;;; agent-shell-project.el ends here
