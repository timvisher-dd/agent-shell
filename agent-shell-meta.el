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

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'subr-x)

(defun agent-shell--meta-lookup (meta key)
  "Lookup KEY in META, handling symbol or string keys.

For example:

  (agent-shell--meta-lookup \\='((stdout . \"hello\")) \\='stdout)
    => \"hello\"

  (agent-shell--meta-lookup \\='((\"stdout\" . \"hello\")) \\='stdout)
    => \"hello\""
  (let ((value (map-elt meta key)))
    (when (and (null value) (symbolp key))
      (setq value (map-elt meta (symbol-name key))))
    value))

(defun agent-shell--meta-find-tool-response (meta)
  "Find a toolResponse value nested inside any namespace in META.
Agents may place toolResponse under an agent-specific key (e.g.
_meta.agentName.toolResponse).  Walk the top-level entries of META
looking for one that contains a toolResponse.

For example:

  (agent-shell--meta-find-tool-response
   \\='((claudeCode . ((toolResponse . ((stdout . \"hi\")))))))
    => ((stdout . \"hi\"))"
  (let ((found nil))
    (cond
     ((setq found (agent-shell--meta-lookup meta 'toolResponse))
      found)
     (t
      (cl-loop for entry in (when (listp meta) meta)
               for value = (cond
                            ((and (consp entry) (consp (cdr entry)))
                             (agent-shell--meta-lookup (cdr entry) 'toolResponse))
                            ((and (consp entry) (listp (cdr entry)))
                             (agent-shell--meta-lookup (cdr entry) 'toolResponse)))
               when value return value)))))

(defun agent-shell--tool-call-meta-response-text (update)
  "Return tool response text from UPDATE meta, if present.
Looks for a toolResponse entry inside any agent-specific _meta
namespace and extracts text from it.  Handles three common shapes:

An alist with a `stdout' string:

  \\='((toolCallId . \"id\")
    (_meta . ((claudeCode . ((toolResponse . ((stdout . \"output\"))))))))
    => \"output\"

An alist with a `content' string:

  \\='((_meta . ((agent . ((toolResponse . ((content . \"text\"))))))))
    => \"text\"

A vector of text items:

  \\='((_meta . ((toolResponse . [((type . \"text\") (text . \"one\"))
                                ((type . \"text\") (text . \"two\"))]))))
    => \"one\\n\\ntwo\""
  (when-let* ((meta (or (map-elt update '_meta)
                        (map-elt update 'meta)))
              (response (agent-shell--meta-find-tool-response meta)))
    (cond
     ((and (listp response)
           (not (vectorp response))
           (stringp (agent-shell--meta-lookup response 'stdout)))
      (agent-shell--meta-lookup response 'stdout))
     ((and (listp response)
           (not (vectorp response))
           (stringp (agent-shell--meta-lookup response 'content)))
      (agent-shell--meta-lookup response 'content))
     ((vectorp response)
      (let* ((items (append response nil))
             (parts (delq nil
                          (mapcar (lambda (item)
                                    (let ((text (agent-shell--meta-lookup item 'text)))
                                      (when (and (stringp text)
                                                 (not (string-empty-p text)))
                                        text)))
                                  items))))
        (when parts
          (mapconcat #'identity parts "\n\n")))))))

(provide 'agent-shell-meta)

;;; agent-shell-meta.el ends here
