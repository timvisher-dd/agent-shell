;;; agent-shell-focus-loss-repro.el --- Batch focus-loss repro -*- lexical-binding: t; -*-

;; Run with (example):
;; AGENT_SHELL_REPRO_TRAFFIC=/path/to/session.traffic \
;; emacs --batch -l scripts/agent-shell-focus-loss-repro.el

(require 'package)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defun agent-shell-focus--env (name default)
  "Return environment variable NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        value
      default)))

(defun agent-shell-focus--env-int (name default)
  "Return integer env NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun agent-shell-focus--dir-with-file (path filename)
  "Return PATH if it contains FILENAME, or its directory when PATH is the file."
  (cond
   ((and path (file-directory-p path)
         (file-exists-p (expand-file-name filename path)))
    (file-name-as-directory (expand-file-name path)))
   ((and path (file-regular-p path)
         (string= (file-name-nondirectory path) filename))
    (file-name-directory path))
   (t nil)))

(defun agent-shell-focus--resolve-dir (env-name filename &rest fallbacks)
  "Resolve ENV-NAME to a directory containing FILENAME, else try FALLBACKS."
  (or (agent-shell-focus--dir-with-file (getenv env-name) filename)
      (cl-loop for candidate in fallbacks
               for dir = (agent-shell-focus--dir-with-file candidate filename)
               when dir return dir)
      (error "Set %s to directory containing %s" env-name filename)))

(defun agent-shell-focus--maybe-add-path (path)
  "Add PATH to `load-path` when it exists."
  (when (and path (file-directory-p path))
    (add-to-list 'load-path path)
    t))

(defun agent-shell-focus--disable-user-init-hooks ()
  "Disable user init hooks that would load external env tooling."
  (when (fboundp 'timvisher-load-op-environment-variables)
    (fset 'timvisher-load-op-environment-variables
          (lambda () (message "Skipping timvisher-load-op-environment-variables"))))
  (setq after-load-alist (assq-delete-all 'agent-shell after-load-alist))
  (setq after-load-alist (assq-delete-all "agent-shell" after-load-alist)))

(defvar agent-shell-focus--done nil)
(defvar agent-shell-focus--sent nil)
(defvar agent-shell-focus--seen-busy nil)
(defvar agent-shell-focus--idle-since nil)
(defvar agent-shell-focus--focus-loss-count 0)
(defvar agent-shell-focus--focus-loss-events nil)
(defvar agent-shell-focus--traffic-file nil)

(defun agent-shell-focus--prompt-at-end-p (buf)
  "Return non-nil when BUF point is at the last prompt."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (ignore-errors (shell-maker-point-at-last-prompt-p)))))

(defun agent-shell-focus--record-focus-loss (buf where)
  "Record a prompt focus loss event for BUF in WHERE."
  (setq agent-shell-focus--focus-loss-count
        (1+ agent-shell-focus--focus-loss-count))
  (when (< (length agent-shell-focus--focus-loss-events) 8)
    (with-current-buffer buf
      (push (list :where where
                  :buffer (buffer-name)
                  :point (point)
                  :point-max (point-max))
            agent-shell-focus--focus-loss-events))))

(defun agent-shell-focus--check-focus (buf was-at-prompt where)
  "If BUF was at prompt and no longer is, record WHERE."
  (when (and was-at-prompt buf (buffer-live-p buf))
    (unless (agent-shell-focus--prompt-at-end-p buf)
      (agent-shell-focus--record-focus-loss buf where))))

(defun agent-shell-focus--advice-on-notification (orig &rest args)
  "Advice for `agent-shell--on-notification` to detect focus loss."
  (let* ((state (plist-get args :state))
         (buf (and state (map-elt state :buffer)))
         (was-at-prompt (agent-shell-focus--prompt-at-end-p buf)))
    (prog1 (apply orig args)
      (agent-shell-focus--check-focus buf was-at-prompt "on-notification"))))

(defun agent-shell-focus--advice-update-fragment (orig &rest args)
  "Advice for `agent-shell--update-fragment` to detect focus loss."
  (let* ((state (plist-get args :state))
         (buf (and state (map-elt state :buffer)))
         (was-at-prompt (agent-shell-focus--prompt-at-end-p buf)))
    (prog1 (apply orig args)
      (agent-shell-focus--check-focus buf was-at-prompt "update-fragment"))))

(defun agent-shell-focus--advice-append-tool-call-output (orig state tool-call-id text)
  "Advice for `agent-shell--append-tool-call-output` to detect focus loss."
  (let* ((buf (and state (map-elt state :buffer)))
         (was-at-prompt (agent-shell-focus--prompt-at-end-p buf)))
    (prog1 (funcall orig state tool-call-id text)
      (agent-shell-focus--check-focus buf was-at-prompt "append-tool-call-output"))))

(defun agent-shell-focus--install-focus-checks ()
  "Install prompt focus regression checks."
  (when (fboundp 'agent-shell--on-notification)
    (advice-add 'agent-shell--on-notification :around
                #'agent-shell-focus--advice-on-notification))
  (when (fboundp 'agent-shell--update-fragment)
    (advice-add 'agent-shell--update-fragment :around
                #'agent-shell-focus--advice-update-fragment))
  (when (fboundp 'agent-shell--append-tool-call-output)
    (advice-add 'agent-shell--append-tool-call-output :around
                #'agent-shell-focus--advice-append-tool-call-output)))

(defun agent-shell-focus--first-prompt-text (messages)
  "Return the first session/prompt text from MESSAGES."
  (when-let ((prompt (seq-find
                      (lambda (item)
                        (and (eq (map-elt item :direction) 'outgoing)
                             (equal (map-nested-elt item '(:object method)) "session/prompt")
                             (let ((text (map-nested-elt item '(:object params prompt 0 text))))
                               (and text (not (string-empty-p text))))))
                      messages)))
    (map-nested-elt prompt '(:object params prompt 0 text))))

(defun agent-shell-focus--start-fake-agent (messages)
  "Start a fake agent shell using traffic MESSAGES."
  (let* ((authenticate-message (acp-fakes--get-authenticate-request :messages messages))
         (authenticate-request (when authenticate-message
                                 (list (cons :method (map-nested-elt authenticate-message '(:object method)))
                                       (cons :params (map-nested-elt authenticate-message '(:object params))))))
         (client (agent-shell-focus--make-fake-client messages))
         (config (agent-shell-make-agent-config
                  :identifier 'focus
                  :mode-line-name "Focus"
                  :welcome-function (lambda (_config) "")
                  :buffer-name "Focus"
                  :shell-prompt "Focus> "
                  :shell-prompt-regexp "Focus> "
                  :client-maker (lambda (&optional _buffer) client)
                  :needs-authentication authenticate-request
                  :authenticate-request-maker (lambda ()
                                                authenticate-request)))
         (buffer (agent-shell--start :config config :no-focus t :new-session t)))
    (with-current-buffer buffer
      (map-put! agent-shell--state :client client))
    buffer))

(cl-defun agent-shell-focus--request-sender (&key client request on-success on-failure &allow-other-keys)
  "Handle fake requests while ignoring extra keys."
  (acp-fakes--request-sender :client client
                             :request request
                             :on-success on-success
                             :on-failure on-failure))

(defun agent-shell-focus--make-fake-client (messages)
  "Create a fake ACP client that tolerates extra request keys."
  (let ((client (acp-make-client
                 :command "cat"
                 :command-params nil
                 :environment-variables nil
                 :request-sender (cl-function
                                  (lambda (&rest args)
                                    (apply #'agent-shell-focus--request-sender args)))
                 :response-sender
                 (cl-function (lambda (&key _client response &allow-other-keys)
                                (acp-fakes--response-sender :response response)))
                 :request-resolver
                 (cl-function (lambda (&key client id &allow-other-keys)
                                (acp-fakes--request-resolver :client client :id id))))))
    (setf (map-elt client :message-queue) (copy-sequence messages))
    (setf (map-elt client :pending-requests) '())
    client))

(defun agent-shell-focus--ensure-point-at-end (buf)
  "Move point to end of BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (goto-char (point-max)))))

(defun agent-shell-focus--finish (status traffic-file)
  "Print STATUS and exit with success or failure."
  (let ((focus-loss agent-shell-focus--focus-loss-count)
        (ok (and (string= status "completed") (= agent-shell-focus--focus-loss-count 0))))
    (princ (format "AGENT_SHELL_FOCUS_LOSS_RESULT status=%s prompt_focus_loss=%d traffic=%s\n"
                   status focus-loss traffic-file))
    (when agent-shell-focus--focus-loss-events
      (dolist (event (nreverse agent-shell-focus--focus-loss-events))
        (princ (format
                "AGENT_SHELL_FOCUS_LOSS_EVENT where=%s buffer=%s point=%d point_max=%d\n"
                (plist-get event :where)
                (plist-get event :buffer)
                (plist-get event :point)
                (plist-get event :point-max)))))
    (setq agent-shell-focus--done t)
    (kill-emacs (if ok 0 1))))

(defun agent-shell-focus--tick (buf idle-seconds)
  "Poll for completion using BUF and IDLE-SECONDS."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (let ((busy (ignore-errors (shell-maker-busy))))
        (cond
         ((and agent-shell-focus--sent (not busy))
          (unless agent-shell-focus--idle-since
            (setq agent-shell-focus--idle-since (float-time)))
          (when (and agent-shell-focus--idle-since
                     (>= (- (float-time) agent-shell-focus--idle-since) idle-seconds))
            (agent-shell-focus--finish "completed" agent-shell-focus--traffic-file)))
         (busy
          (setq agent-shell-focus--idle-since nil)
          (setq agent-shell-focus--seen-busy t)))))))

(defun agent-shell-focus--main ()
  "Run focus loss repro against a traffic file."
  (package-initialize)
  (agent-shell-focus--disable-user-init-hooks)
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory (or load-file-name buffer-file-name)))))
         (agent-shell-dir (agent-shell-focus--resolve-dir
                           "AGENT_SHELL_REPRO_AGENT_DIR"
                           "agent-shell.el"
                           root
                           (expand-file-name "~/git.experiment.worktrees/xenodium/agent-shell/main")))
         (shell-maker-dir (agent-shell-focus--resolve-dir
                           "AGENT_SHELL_REPRO_SHELL_MAKER_DIR"
                           "shell-maker.el"
                           (expand-file-name "~/git.experiment.worktrees/xenodium/shell-maker/main")))
         (acp-dir (agent-shell-focus--resolve-dir
                   "AGENT_SHELL_REPRO_ACP_DIR"
                   "acp.el"
                   (expand-file-name "~/git.experiment.worktrees/xenodium/acp.el/main")))
         (traffic-default (expand-file-name "tests/gemini-wrong-output-grouping.traffic" root))
         (traffic-file (expand-file-name (agent-shell-focus--env "AGENT_SHELL_REPRO_TRAFFIC" traffic-default)))
         (timeout (max 1 (agent-shell-focus--env-int "AGENT_SHELL_REPRO_TIMEOUT" 20)))
         (poll (max 0.02 (agent-shell-focus--env-int "AGENT_SHELL_REPRO_POLL_INTERVAL" 0.05)))
         (idle-seconds (max 0.05 (agent-shell-focus--env-int "AGENT_SHELL_REPRO_IDLE_SECONDS" 1))))
    (unless (file-readable-p traffic-file)
      (error "Traffic file not readable: %s" traffic-file))

    (agent-shell-focus--maybe-add-path acp-dir)
    (agent-shell-focus--maybe-add-path shell-maker-dir)
    (agent-shell-focus--maybe-add-path agent-shell-dir)

    (load (expand-file-name "acp.el" acp-dir) nil t)
    (load (expand-file-name "acp-traffic.el" acp-dir) nil t)
    (load (expand-file-name "acp-fakes.el" acp-dir) nil t)
    (load (expand-file-name "shell-maker.el" shell-maker-dir) nil t)
    (load (expand-file-name "agent-shell-ui.el" agent-shell-dir) nil t)
    (load (expand-file-name "agent-shell.el" agent-shell-dir) nil t)

    ;; Avoid throttling so all updates land synchronously.
    (setq agent-shell-tool-call-update-throttle-seconds nil)

    (agent-shell-focus--install-focus-checks)

    (setq agent-shell-focus--traffic-file traffic-file)
    (let* ((messages (acp-traffic-read-file traffic-file))
           (prompt-text (agent-shell-focus--first-prompt-text messages))
           (buffer (agent-shell-focus--start-fake-agent messages)))
      (unless prompt-text
        (error "No session/prompt text found in traffic: %s" traffic-file))
      (agent-shell-focus--ensure-point-at-end buffer)
      (with-current-buffer buffer
        (agent-shell-insert :text prompt-text :submit t :no-focus t))
      (setq agent-shell-focus--sent t)

      (run-at-time timeout nil
                   (lambda ()
                     (agent-shell-focus--finish "timeout" traffic-file)))

      (run-at-time poll poll
                   (lambda ()
                     (agent-shell-focus--tick buffer idle-seconds)))

      (while (not agent-shell-focus--done)
        (accept-process-output nil poll)))))

(agent-shell-focus--main)

;;; agent-shell-focus-loss-repro.el ends here
