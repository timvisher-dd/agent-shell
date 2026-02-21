;;; agent-shell-meta.el --- Meta helpers for agent-shell -*- lexical-binding: t; -*-

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
;; Meta helpers for agent-shell tool call handling.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'subr-x)

(defun agent-shell--meta-lookup (meta key)
  "Lookup KEY in META, handling symbol or string keys."
  (let ((value (map-elt meta key)))
    (if (and (null value) (symbolp key))
        (map-elt meta (symbol-name key))
      value)))

(defun agent-shell--tool-call-meta-response-text (update)
  "Return tool response text from UPDATE meta, if present.
Looks for a toolResponse entry inside any agent-specific _meta
namespace and extracts text from it.  Handles two common shapes:
an alist with a `content' string, or a vector of text items."
  (let* ((meta (or (map-elt update '_meta)
                   (map-elt update 'meta)))
         (response (and meta
                        (agent-shell--meta-find-tool-response meta))))
    (cond
     ;; Alist shape: (toolResponse (mode . "content") (content . "text..."))
     ((and (listp response)
           (not (vectorp response))
           (stringp (agent-shell--meta-lookup response 'content)))
      (agent-shell--meta-lookup response 'content))
     ;; Vector shape: (toolResponse . [((type . "text") (text . "text..."))])
     ((vectorp response)
      (let* ((items (append response nil))
             (parts (mapcan (lambda (item)
                              (let ((text (agent-shell--meta-lookup item 'text)))
                                (when (and (stringp text)
                                           (not (string-empty-p text)))
                                  (list text))))
                            items)))
        (when parts
          (mapconcat #'identity parts "\n\n")))))))

(defun agent-shell--meta-find-tool-response (meta)
  "Find a toolResponse value nested inside any namespace in META.
Agents may place toolResponse under an agent-specific key (e.g.
_meta.agentName.toolResponse).  Walk the top-level entries of META
looking for one that contains a toolResponse."
  (or (agent-shell--meta-lookup meta 'toolResponse)
      (cl-loop for entry in (if (listp meta) meta nil)
               for value = (when (and (consp entry) (listp (cdr entry)))
                             (agent-shell--meta-lookup (cdr entry) 'toolResponse))
               when value return value)))

(provide 'agent-shell-meta)

;;; agent-shell-meta.el ends here
