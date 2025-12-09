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
(require 'flymake)
(require 'markdown-overlays)
(require 'shell-maker)

(eval-when-compile
  (require 'cl-lib))

(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--make-header "agent-shell")
(declare-function agent-shell--relevant-text "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-next-permission-button "agent-shell")
(declare-function agent-shell-previous-permission-button "agent-shell")
(declare-function agent-shell-project-buffers "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell-ui-backward-block "agent-shell")
(declare-function agent-shell-ui-forward-block "agent-shell")
(declare-function agent-shell-ui-mode "agent-shell")

(defvar agent-shell-header-style)
(defvar agent-shell-prefer-compose-buffer)
(defvar agent-shell-preferred-agent-config)

(cl-defun agent-shell-prompt-compose--show-buffer (&key text submit no-focus shell-buffer)
  "Show a compose buffer for the agent shell.

TEXT is inserted into the compose buffer.
SUBMIT, when non-nil, submits after insertion.
NO-FOCUS, when non-nil, avoids focusing the compose buffer.
SHELL-BUFFER, when non-nil, prefer this shell buffer.
NEW-SHELL, create a new shell (no history).

Returns an alist with insertion details or nil otherwise:

  ((:buffer . BUFFER)
   (:start . START)
   (:end . END))"
  (when submit
    (error "Not yet supported"))
  (when no-focus
    (error "Not yet supported"))
  (when shell-buffer
    ;; Momentarily set buffer to same window, so it's recent in stack.
    (let ((current (current-buffer)))
      (pop-to-buffer-same-window shell-buffer)
      (pop-to-buffer-same-window current)))
  (when-let ((shell-buffer (or shell-buffer (agent-shell--shell-buffer)))
             (compose-buffer (agent-shell-prompt-compose--buffer :shell-buffer shell-buffer))
             (text (or text (agent-shell--relevant-text) "")))
    (let ((insert-start nil)
          (insert-end nil))
      (agent-shell--display-buffer compose-buffer)
      ;; TODO: Do we need to get prompt and partial response,
      ;; in case compose buffer is created for the first time
      ;; on an ongoing/busy shell session?
      (if (agent-shell-prompt-compose--busy-p)
          (agent-shell-prompt-compose-view-mode)
        (if (derived-mode-p 'agent-shell-prompt-compose-edit-mode)
            (unless (string-empty-p text)
              (save-excursion
                (goto-char (point-max))
                (setq insert-start (point))
                (unless (string-empty-p text)
                  (insert "\n\n" text))
                (setq insert-end (point))))
          (agent-shell-prompt-compose-edit-mode)
          ;; Transitioned to edit mode. Start empty.
          (agent-shell-prompt-compose--initialize)
          (save-excursion
            (goto-char (point-max))
            (setq insert-start (point))
            (unless (string-empty-p text)
              (insert "\n\n" text))
            (setq insert-end (point)))))
      `((:buffer . ,compose-buffer)
        (:start . ,insert-start)
        (:end . ,insert-end)))))

(defun agent-shell-prompt-compose-send ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (if agent-shell-prefer-compose-buffer
      (agent-shell-prompt-compose-send-and-wait-for-response)
    (agent-shell-prompt-compose-send-and-kill)))

(defun agent-shell-prompt-compose-send-and-kill ()
  "Send the composed prompt to the agent shell and kill compose buffer."
  (interactive)
  (let ((shell-buffer (agent-shell--shell-buffer))
        (compose-buffer (current-buffer))
        (prompt (buffer-string)))
    (with-current-buffer shell-buffer
      (agent-shell--insert-to-shell-buffer
       :text prompt
       :submit t))
    (kill-buffer compose-buffer)
    (pop-to-buffer shell-buffer)))

(defun agent-shell-prompt-compose-send-and-wait-for-response ()
  "Send the composed prompt to the agent shell."
  (interactive)
  (catch 'exit
    (unless (derived-mode-p 'agent-shell-prompt-compose-edit-mode)
      (user-error "Not in a shell compose buffer"))
    (let ((shell-buffer (agent-shell--shell-buffer))
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
        (agent-shell--insert-to-shell-buffer
         :text prompt
         :submit t
         :no-focus t)
        ;; TODO: Point should go to beginning of response after submission.
        (let ((inhibit-read-only t))
          (markdown-overlays-put))))))

(defun agent-shell-prompt-compose-interrupt ()
  "Interrupt active agent shell request."
  (interactive)
  (catch 'exit
    (let ((shell-buffer (agent-shell--shell-buffer)))
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
    (when-let ((shell-buffer (agent-shell--shell-buffer)))
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
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (if (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
          (with-current-buffer (agent-shell--shell-buffer)
            (not (shell-maker-history))))
      (bury-buffer)
    ;; Edit mode
    (when (or (string-empty-p (string-trim (buffer-string)))
              (y-or-n-p "Discard compose buffer? "))
      (agent-shell-prompt-compose-view-last))))

(defun agent-shell-prompt-compose-view-last ()
  "Display the last request/response interaction."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (when-let ((shell-buffer (agent-shell--shell-buffer)))
    (with-current-buffer shell-buffer
      (goto-char comint-last-input-start)))
  (agent-shell-prompt-compose-view-mode)
  (agent-shell-prompt-compose-refresh))

(defun agent-shell-prompt-compose-refresh ()
  "Refresh compose buffer content with current item from shell."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (when-let ((shell-buffer (agent-shell--shell-buffer))
             (compose-buffer (agent-shell-prompt-compose--buffer))
             (current (with-current-buffer shell-buffer
                        (or (shell-maker--command-and-response-at-point)
                            (shell-maker-next-command-and-response t)))))
    (agent-shell-prompt-compose--initialize
     :prompt (car current)
     :response (cdr current))
    (goto-char (point-min))
    current))

(defun agent-shell-prompt-compose-next-item ()
  "Go to next item.

If at point-max, attempt to switch to next interaction."
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
                     ;; No more items, try point-max if not already there
                     (when (< current-pos (point-max))
                       (point-max)))))
    (if next-pos
        (progn
          (deactivate-mark)
          (goto-char next-pos))
      ;; At point-max with no more items, try next interaction
      (condition-case nil
          (agent-shell-prompt-compose-next-interaction)
        (error
         ;; At the end of all interactions, stay at point-max
         nil)))))

(defun agent-shell-prompt-compose-previous-item ()
  "Go to previous item.

If at the first item, attempt to switch to previous interaction."
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
    (if next-pos
        (progn
          (deactivate-mark)
          (goto-char next-pos))
      ;; No more items before current position, try previous interaction
      (condition-case nil
          ;; Switch to previous page and stop at point-max (call next-interaction directly)
          (agent-shell-prompt-compose-next-interaction :backwards t)
        (error
         ;; At the beginning of all interactions, stay at first item
         (when prompt-start
           (goto-char prompt-start)))))))

(cl-defun agent-shell-prompt-compose--buffer (&key shell-buffer existing-only)
  "Get the compose buffer associated with a SHELL-BUFFER.

With EXISTING-ONLY, only return existing buffers without creating."
  (when-let ((shell-buffer (or shell-buffer
                               (agent-shell--shell-buffer))))
    (with-current-buffer shell-buffer
      (let* ((compose-buffer-name (concat (buffer-name shell-buffer)
                                          " [compose]"))
             (compose-buffer (get-buffer compose-buffer-name)))
        (if compose-buffer
            compose-buffer
          (if existing-only
              nil
            (with-current-buffer (get-buffer-create compose-buffer-name)
              (agent-shell-prompt-compose-edit-mode)
              (current-buffer))))))))

(defun agent-shell-prompt-compose-reply ()
  "Reply as a follow-up and compose another query."
  (interactive)
  (when (agent-shell-prompt-compose--busy-p)
    (user-error "Busy, please wait"))
  (with-current-buffer (agent-shell--shell-buffer)
    (goto-char (point-max)))
  (agent-shell-prompt-compose-edit-mode)
  (agent-shell-prompt-compose--initialize)
  (goto-char (point-min)))

(defun agent-shell-prompt-compose-previous-interaction ()
  "Show previous interaction (request / response)."
  (interactive)
  (agent-shell-prompt-compose-next-interaction :backwards t :start-at-top t))

(cl-defun agent-shell-prompt-compose-next-interaction (&key backwards start-at-top)
  "Show next interaction (request / response).

If BACKWARDS is non-nil, go to previous interaction.
If START-AT-TOP is non-nil, position at point-min regardless of direction."
  (interactive)
  (unless (derived-mode-p 'agent-shell-prompt-compose-view-mode)
    (error "Not in a compose buffer"))
  (when (agent-shell-prompt-compose--busy-p)
    (user-error "Busy... please wait"))
  (when-let ((shell-buffer (agent-shell--shell-buffer))
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
    (goto-char (if start-at-top
                   (point-min)
                 (if backwards (point-max) (point-min))))
    (agent-shell-prompt-compose--update-header)
    next))

(defun agent-shell-prompt-compose-set-session-model ()
  "Set session model."
  (interactive)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (compose-buffer (agent-shell-prompt-compose--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-set-session-model
       (lambda ()
         (with-current-buffer compose-buffer
           (agent-shell-prompt-compose--update-header)))))))

(defun agent-shell-prompt-compose-set-session-mode ()
  "Set session mode."
  (interactive)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (compose-buffer (agent-shell-prompt-compose--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-set-session-mode
       (lambda ()
         (when compose-buffer
           (with-current-buffer compose-buffer
             (agent-shell-prompt-compose--update-header))))))))

(defun agent-shell-prompt-compose-cycle-session-mode ()
  "Cycle through available session modes."
  (interactive)
  (let* ((shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in an agent-shell buffer")))
         (compose-buffer (agent-shell-prompt-compose--buffer
                          :shell-buffer shell-buffer
                          :existing-only t)))
    (with-current-buffer shell-buffer
      (agent-shell-cycle-session-mode
       (lambda ()
         (when compose-buffer
           (with-current-buffer compose-buffer
             (agent-shell-prompt-compose--update-header))))))))

(defun agent-shell-prompt-compose--position ()
  "Return the position in history of the shell buffer."
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (let* ((shell-buffer (agent-shell--shell-buffer))
         (current (with-current-buffer shell-buffer
                    (shell-maker--command-and-response-at-point)))
         (history (with-current-buffer shell-buffer
                    (shell-maker-history)))
         (pos (seq-position history current)))
    (cond ((and current history pos)
           (cons (1+ pos) (length history)))
          (history
           (cons (1+ (length history))
                 (1+ (length history)))))))

(defun agent-shell-prompt-compose--busy-p ()
  "Return non-nil if the associated shell buffer is busy."
  (when-let ((shell-buffer (agent-shell--shell-buffer :no-error t)))
    (with-current-buffer shell-buffer
      shell-maker--busy)))

(defun agent-shell-prompt-compose--update-header ()
  "Update header and mode line based on `agent-shell-header-style'.

Automatically determines qualifier and bindings based on current major mode."
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (let* ((pos (or (agent-shell-prompt-compose--position)
                  (cons 1 1)))
         (pos-label (format "%d/%d" (car pos) (cdr pos)))
         (qualifier (cond
                     ((agent-shell-prompt-compose--busy-p)
                      (format "[%s][busy]" pos-label))
                     ((derived-mode-p 'agent-shell-prompt-compose-edit-mode)
                      (format "[%s][edit]" pos-label))
                     ((derived-mode-p 'agent-shell-prompt-compose-view-mode)
                      (format "[%s][view]" pos-label))))
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
    (when-let* ((shell-buffer (agent-shell--shell-buffer))
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

(defvar-local agent-shell-prompt-compose--clean-up t)

(defun agent-shell-prompt-compose--clean-up ()
  "Clean up resources.

For example, offer to kill associated shell session."
  (unless (or (derived-mode-p 'agent-shell-prompt-compose-view-mode)
              (derived-mode-p 'agent-shell-prompt-compose-edit-mode))
    (user-error "Not in a shell compose buffer"))
  (if (and agent-shell-prompt-compose--clean-up
           ;; Only offer to kill shell buffers when compose buffer
           ;; is explicitly being killed from a compose buffer.
           (eq (current-buffer)
               (window-buffer (selected-window))))
      ;; Temporarily disable cleaning up to avoid multiple clean-ups
      ;; triggered by shell buffers attempting to kill compose buffer.
      (let ((agent-shell-prompt-compose--clean-up nil))
        (when-let ((shell-buffers (seq-filter (lambda (shell-buffer)
                                                (equal (agent-shell-prompt-compose--buffer
                                                        :shell-buffer shell-buffer
                                                        :existing-only t)
                                                       (current-buffer)))
                                              (agent-shell-buffers)))
                   (_ (y-or-n-p "Kill shell session too?")))
          (mapc (lambda (shell-buffer)
                  (kill-buffer shell-buffer))
                shell-buffers)))))

(defvar agent-shell-prompt-compose-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-send)
    (define-key map (kbd "C-c C-k") #'agent-shell-prompt-compose-cancel)
    (define-key map (kbd "C-<tab>") #'agent-shell-prompt-compose-cycle-session-mode)
    (define-key map (kbd "C-c C-m") #'agent-shell-prompt-compose-set-session-mode)
    (define-key map (kbd "C-c C-v") #'agent-shell-prompt-compose-set-session-model)
    (define-key map (kbd "C-c C-o") #'agent-shell-other-buffer)
    map)
  "Keymap for `agent-shell-prompt-compose-edit-mode'.")

(define-derived-mode agent-shell-prompt-compose-edit-mode text-mode "Agent Compose"
  "Major mode for composing agent shell prompts.

\\{agent-shell-prompt-compose-edit-mode-map}"
  (cursor-intangible-mode +1)
  (setq buffer-read-only nil)
  (agent-shell-prompt-compose--update-header)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (add-hook 'kill-buffer-hook #'agent-shell-prompt-compose--clean-up nil t))

(defvar agent-shell-prompt-compose-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'agent-shell-prompt-compose-interrupt)
    (define-key map (kbd "<tab>") #'agent-shell-prompt-compose-next-item)
    (define-key map (kbd "<backtab>") #'agent-shell-prompt-compose-previous-item)
    (define-key map (kbd "f") #'agent-shell-prompt-compose-next-interaction)
    (define-key map (kbd "b") #'agent-shell-prompt-compose-previous-interaction)
    (define-key map (kbd "r") #'agent-shell-prompt-compose-reply)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "C-<tab>") #'agent-shell-prompt-compose-cycle-session-mode)
    (define-key map (kbd "v") #'agent-shell-prompt-compose-set-session-model)
    (define-key map (kbd "m") #'agent-shell-prompt-compose-set-session-mode)
    (define-key map (kbd "o") #'agent-shell-other-buffer)
    (define-key map (kbd "C-c C-o") #'agent-shell-other-buffer)
    map)
  "Keymap for `agent-shell-prompt-compose-view-mode'.")

(define-derived-mode agent-shell-prompt-compose-view-mode text-mode "Agent Compose"
  "Major mode for viewing agent shell prompts (read-only).

\\{agent-shell-prompt-compose-view-mode-map}"
  (cursor-intangible-mode +1)
  (agent-shell-ui-mode +1)
  (agent-shell-prompt-compose--update-header)
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook #'agent-shell-prompt-compose--clean-up nil t))

(provide 'agent-shell-prompt-compose)

;;; agent-shell-prompt-compose.el ends here
