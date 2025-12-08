;;; agent-shell-prompt-compose.el --- Agent shell prompt compose buffer  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alvaro Ramirez

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

;; Prompt compose buffers enable crafting more involved queries and
;; simplify both response navigation and follow-up queries.
;;
;; Support the work https://github.com/sponsors/xenodium

;;; Code:

(require 'cursor-sensor)
(require 'seq)
(require 'subr-x)
(require 'window)

(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-insert "agent-shell")

(defvar agent-shell-preferred-agent-config)

(defvar agent-shell-prompt-compose--experimental-compose nil)

(defun agent-shell-prompt-compose--show-buffer ()
  "Show a compose buffer for the agent shell."
  (when-let ((compose-buffer (agent-shell-prompt-compose--buffer))
             (shell-buffer (agent-shell-prompt-compose--shell-buffer)))
    (pop-to-buffer compose-buffer)
    ;; TODO: Do we need to get prompt and partial response,
    ;; in case compose buffer is created for the first time
    ;; on an ongoing/busy shell session?
    (if (agent-shell-prompt-compose--busy-p)
        (agent-shell-prompt-compose-view-mode)
      (agent-shell-prompt-compose-edit-mode)
      (agent-shell-prompt-compose--initialize))))

(defun agent-shell-prompt-compose-send ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (if agent-shell-prompt-compose--experimental-compose
      (agent-shell-prompt-compose-send-and-wait-for-response)
    (agent-shell-prompt-compose-send-and-kill)))

(defun agent-shell-prompt-compose-send-and-kill ()
  "Send the composed prompt to the agent shell and kill compose buffer."
  (interactive)
  (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
        (compose-buffer (current-buffer))
        (prompt (buffer-string)))
    (with-current-buffer shell-buffer
      (agent-shell-insert :text prompt
                          :submit t))
    (kill-buffer compose-buffer)
    (pop-to-buffer shell-buffer)))

(defun agent-shell-prompt-compose-send-and-wait-for-response ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (catch 'exit
    (unless (derived-mode-p 'agent-shell-prompt-compose-edit-mode)
      (user-error "Not in a shell compose buffer"))
    (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
          (compose-buffer (agent-shell-prompt-compose--buffer))
          (prompt (string-trim (buffer-string))))
      (when (agent-shell-prompt-compose--busy-p)
        (unless (y-or-n-p "Interrupt?")
          (throw 'exit nil))
        (with-current-buffer shell-buffer
          (agent-shell-interrupt t))
        (with-current-buffer compose-buffer
          (agent-shell-prompt-compose-view-mode)
          (agent-shell-prompt-compose--initialize
           :prompt prompt))
        (user-error "Aborted"))
      (when (string-empty-p (string-trim prompt))
        (agent-shell-prompt-compose--initialize)
        (user-error "Nothing to send"))
      (if (derived-mode-p 'agent-shell-prompt-compose-view-mode)
          (progn
            (agent-shell-prompt-compose-edit-mode)
            (agent-shell-prompt-compose--initialize))
        (let ((inhibit-read-only t))
          (markdown-overlays-put))
        (agent-shell-prompt-compose-view-mode)
        (agent-shell-prompt-compose--initialize :prompt prompt)
        ;; (setq view-exit-action 'kill-buffer) TODO
        (when (string-equal prompt "clear")
          (agent-shell-prompt-compose-edit-mode)
          (agent-shell-prompt-compose--initialize))
        (agent-shell-insert :text prompt
                            :submit t
                            :no-focus t)
        ;; TODO: Point should go to beginning of response after submission.
        (let ((inhibit-read-only t))
          (markdown-overlays-put))))))

(defun agent-shell-prompt-compose-interrupt ()
  "Interrupt active agent shell request."
  (interactive)
  (catch 'exit
    (let ((shell-buffer (agent-shell-prompt-compose--shell-buffer)))
      (unless (agent-shell-prompt-compose--busy-p)
        (user-error "No pending request"))
      (unless (y-or-n-p "Interrupt?")
        (throw 'exit nil))
      (with-current-buffer shell-buffer
        (agent-shell-interrupt t))
      (user-error "Aborted"))))

(cl-defun agent-shell-prompt-compose--initialize (&key prompt response)
  "Initialize compose buffer.

Optionally set its PROMPT and RESPONSE."
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (let ((inhibit-read-only t)
        (compose-buffer (current-buffer)))
    (erase-buffer)
    (when-let ((shell-buffer (agent-shell-prompt-compose--shell-buffer)))
      (with-current-buffer shell-buffer
        (unless (eq agent-shell-header-style 'graphical)
          ;; Insert read-only newline at the point-min
          ;; purely for display/layout purpose. This
          ;; is only needed for non-graphical header.
          (with-current-buffer compose-buffer
            (insert (propertize "\n"
                                'read-only t
                                'cursor-intangible t
                                'front-sticky '(read-only cursor-intangible)
                                'rear-nonsticky '(read-only cursor-intangible)))))))
    (when prompt
      (insert
       (if (derived-mode-p 'agent-shell-prompt-compose-view-mode)
           (propertize (concat prompt "\n\n")
                       'rear-nonsticky t
                       'agent-shell-prompt-compose-prompt t
                       'face 'font-lock-doc-face)
         prompt)))
    (when response
      (insert response))))

(defun agent-shell-prompt-compose--prompt ()
  "Return the buffer prompt."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-prompt-compose-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-prompt-compose-prompt)))
                (found (get-text-property start 'agent-shell-prompt-compose-prompt)))
      (string-trim
       (buffer-substring-no-properties
        start
        (or (next-single-property-change
             start 'agent-shell-prompt-compose-prompt)
            (point-max)))))))

(defun agent-shell-prompt-compose--response ()
  "Return the buffer response."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-prompt-compose-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-prompt-compose-prompt)))
                (found (get-text-property start 'agent-shell-prompt-compose-prompt))
                (end (next-single-property-change start 'agent-shell-prompt-compose-prompt)))
      (buffer-substring end (point-max)))))

(defun agent-shell-prompt-compose--prompt-start ()
  "Return the start position of the prompt, or nil if no prompt."
  (save-excursion
    (goto-char (point-min))
    (when-let ((start (if (get-text-property (point-min) 'agent-shell-prompt-compose-prompt)
                          (point-min)
                        (next-single-property-change (point-min) 'agent-shell-prompt-compose-prompt))))
      (when (get-text-property start 'agent-shell-prompt-compose-prompt)
        start))))

(defun agent-shell-prompt-compose--response-start ()
  "Return the start position of the response, or nil if no response."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start (if (get-text-property (point-min) 'agent-shell-prompt-compose-prompt)
                           (point-min)
                         (next-single-property-change (point-min) 'agent-shell-prompt-compose-prompt)))
                (found (get-text-property start 'agent-shell-prompt-compose-prompt))
                (end (next-single-property-change start 'agent-shell-prompt-compose-prompt)))
      (when (< end (point-max))
        end))))

(defun agent-shell-prompt-compose-cancel ()
  "Cancel prompt composition."
  (interactive)
  (when (or (string-empty-p (string-trim (buffer-string)))
            (y-or-n-p "Discard compose buffer? "))
    (kill-buffer (current-buffer))))

(defun agent-shell-prompt-compose-next-item ()
  "Go to next item."
  (interactive)
  (unless (derived-mode-p 'agent-shell-prompt-compose-view-mode)
    (error "Not in a compose buffer"))
  (let* ((current-pos (point))
         (prompt-start (agent-shell-prompt-compose--prompt-start))
         (response-start (agent-shell-prompt-compose--response-start))
         (block-pos (save-mark-and-excursion
                      (agent-shell-ui-forward-block)))
         (button-pos (save-mark-and-excursion
                       (agent-shell-next-permission-button)))
         ;; Filter positions to only those after current position
         (candidates (delq nil (list
                                (when (and prompt-start (> prompt-start current-pos))
                                  prompt-start)
                                (when (and response-start (> response-start current-pos))
                                  response-start)
                                block-pos
                                button-pos)))
         (next-pos (if candidates
                       (apply #'min candidates)
                     ;; Fall back to point-max if no candidates
                     (when (< current-pos (point-max))
                       (point-max)))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos))))

(defun agent-shell-prompt-compose-previous-item ()
  "Go to previous item."
  (interactive)
  (unless (derived-mode-p 'agent-shell-prompt-compose-view-mode)
    (error "Not in a compose buffer"))
  (let* ((current-pos (point))
         (prompt-start (agent-shell-prompt-compose--prompt-start))
         (response-start (agent-shell-prompt-compose--response-start))
         (block-pos (save-mark-and-excursion
                      (let ((pos (agent-shell-ui-backward-block)))
                        (when (and pos (< pos current-pos))
                          pos))))
         (button-pos (save-mark-and-excursion
                       (let ((pos (agent-shell-previous-permission-button)))
                         (when (and pos (< pos current-pos))
                           pos))))
         ;; Filter positions to only those before current position
         (candidates (delq nil (list
                                (when (and prompt-start (< prompt-start current-pos))
                                  prompt-start)
                                (when (and response-start (< response-start current-pos))
                                  response-start)
                                block-pos
                                button-pos)))
         (next-pos (when candidates
                     (apply #'max candidates))))
    (when next-pos
      (deactivate-mark)
      (goto-char next-pos))))

(cl-defun agent-shell-prompt-compose--buffer (&key shell-buffer existing-only)
  "Get the compose buffer associated with a SHELL-BUFFER.

With EXISTING-ONLY, only return existing buffers without creating."
  (when-let ((shell-buffer (or shell-buffer
                               (agent-shell-prompt-compose--shell-buffer))))
    (with-current-buffer shell-buffer
      (let* ((compose-buffer-name (concat (buffer-name shell-buffer)
                                          " [compose]"))
             (compose-buffer (get-buffer compose-buffer-name)))
        (if compose-buffer
            compose-buffer
          (if existing-only
              nil
            (get-buffer-create compose-buffer-name)))))))

(defun agent-shell-prompt-compose-reply ()
  "Reply as a follow-up and compose another query."
  (interactive)
  (when (agent-shell-prompt-compose--busy-p)
    (user-error "Busy, please wait"))
  (with-current-buffer (agent-shell-prompt-compose--shell-buffer)
    (goto-char (point-max)))
  (agent-shell-prompt-compose-edit-mode)
  (agent-shell-prompt-compose--initialize)
  (goto-char (point-min)))

(defun agent-shell-prompt-compose-previous-interaction ()
  "Show previous interaction (request / response)."
  (interactive)
  (agent-shell-prompt-compose-next-interaction t))

(defun agent-shell-prompt-compose-next-interaction (&optional backwards)
  "Show next interaction (request / response).

If BACKWARDS is non-nil, go to previous interaction."
  (interactive)
  (unless (derived-mode-p 'agent-shell-prompt-compose-view-mode)
    (error "Not in a compose buffer"))
  (when (agent-shell-prompt-compose--busy-p)
    (user-error "Busy... please wait"))
  (when-let ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
             (compose-buffer (agent-shell-prompt-compose--buffer))
             (next (with-current-buffer shell-buffer
                     (if backwards
                         (when (save-excursion
                                 (let ((orig-line (line-number-at-pos)))
                                   (comint-previous-prompt 1)
                                   (= orig-line (line-number-at-pos))))
                           (error "No previous page"))
                       (when (save-excursion
                               (let ((orig-line (point)))
                                 (comint-next-prompt 1)
                                 (= orig-line (point))))
                         (error "No next page")))
                     (shell-maker-next-command-and-response backwards))))
    (agent-shell-prompt-compose--initialize
     :prompt (car next) :response (cdr next))
    (goto-char (point-min))
    next))

(cl-defun agent-shell-prompt-compose--shell-buffer (&key no-error)
  "Get an `agent-shell' buffer (create one if needed).

With NO-ERROR, return nil instead of raising an error."
  (get-buffer
   (or
    (seq-first (agent-shell-project-buffers))
    (if (y-or-n-p "No shells in project.  Start a new one? ")
        (agent-shell--start :config (or agent-shell-preferred-agent-config
                                        (agent-shell-select-config
                                         :prompt "Start new agent: ")
                                        (error "No agent config found"))
                            :no-focus t
                            :new-session t)
      (unless no-error
        (error "No shell to compose on"))))))

(defun agent-shell-prompt-compose--busy-p ()
  "Return non-nil if the associated shell buffer is busy."
  (when-let ((shell-buffer (agent-shell-prompt-compose--shell-buffer :no-error t)))
    (with-current-buffer shell-buffer
      shell-maker--busy)))

(defun agent-shell-prompt-compose--update-header ()
  "Update header and mode line based on `agent-shell-header-style'.

Automatically determines qualifier and bindings based on current major mode."
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (let ((qualifier (cond
                    ((agent-shell-prompt-compose--busy-p)
                     "[busy]")
                    ((derived-mode-p 'agent-shell-prompt-compose-edit-mode)
                     "[edit]")
                    ((derived-mode-p 'agent-shell-prompt-compose-view-mode)
                     "[view]")))
        (bindings (cond
                   ((derived-mode-p 'agent-shell-prompt-compose-edit-mode)
                    (list
                     `((:key . ,(key-description (where-is-internal 'agent-shell-prompt-compose-send agent-shell-prompt-compose-edit-mode-map t)))
                       (:description . "send"))
                     `((:key . ,(key-description (where-is-internal 'agent-shell-prompt-compose-cancel agent-shell-prompt-compose-edit-mode-map t)))
                       (:description . "cancel"))))
                   ((derived-mode-p 'agent-shell-prompt-compose-view-mode)
                    (append
                     (list
                      `((:key . ,(key-description (where-is-internal 'agent-shell-prompt-compose-next-item agent-shell-prompt-compose-view-mode-map t)))
                        (:description . "next"))
                      `((:key . ,(key-description (where-is-internal 'agent-shell-prompt-compose-previous-item agent-shell-prompt-compose-view-mode-map t)))
                        (:description . "previous")))
                     (unless (agent-shell-prompt-compose--busy-p)
                       (list
                        `((:key . ,(key-description (where-is-internal 'agent-shell-prompt-compose-reply agent-shell-prompt-compose-view-mode-map t)))
                          (:description . "reply"))))
                     (when (agent-shell-prompt-compose--busy-p)
                       (list
                        `((:key . ,(key-description (where-is-internal 'agent-shell-prompt-compose-interrupt agent-shell-prompt-compose-view-mode-map t)))
                          (:description . "interrupt")))))))))
    (when-let* ((shell-buffer (agent-shell-prompt-compose--shell-buffer))
                (header (with-current-buffer shell-buffer
                          (cond
                           ((eq agent-shell-header-style 'graphical)
                            (agent-shell--make-header (agent-shell--state)
                                                      :qualifier qualifier
                                                      :bindings bindings))
                           ((memq agent-shell-header-style '(text none nil))
                            (agent-shell--make-header (agent-shell--state)
                                                      :qualifier qualifier
                                                      :bindings bindings))))))
      (setq-local header-line-format header))))

(defvar agent-shell-prompt-compose-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-send)
    (define-key map (kbd "C-c C-k") #'agent-shell-prompt-compose-cancel)
    map)
  "Keymap for `agent-shell-prompt-compose-edit-mode'.")

(define-derived-mode agent-shell-prompt-compose-edit-mode text-mode "Agent Compose"
  "Major mode for composing agent shell prompts.

\\{agent-shell-prompt-compose-edit-mode-map}"
  (cursor-intangible-mode +1)
  (setq buffer-read-only nil)
  (agent-shell-prompt-compose--update-header)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defvar agent-shell-prompt-compose-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-interrupt)
    (define-key map (kbd "<tab>") #'agent-shell-prompt-compose-next-item)
    (define-key map (kbd "<backtab>") #'agent-shell-prompt-compose-previous-item)
    (define-key map (kbd "f") #'agent-shell-prompt-compose-next-interaction)
    (define-key map (kbd "b") #'agent-shell-prompt-compose-previous-interaction)
    (define-key map (kbd "r") #'agent-shell-prompt-compose-reply)
    (define-key map (kbd "q") #'bury-buffer)
    map)
  "Keymap for `agent-shell-prompt-compose-view-mode'.")

(define-derived-mode agent-shell-prompt-compose-view-mode text-mode "Agent Compose"
  "Major mode for viewing agent shell prompts (read-only).

\\{agent-shell-prompt-compose-view-mode-map}"
  (cursor-intangible-mode +1)
  (agent-shell-ui-mode +1)
  (agent-shell-prompt-compose--update-header)
  (setq buffer-read-only t))

(provide 'agent-shell-prompt-compose)

;;; agent-shell-prompt-compose.el ends here
