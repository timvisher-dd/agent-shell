;;; agent-shell-stress-repro.el --- Minimal ACP stress repro -*- lexical-binding: t; -*-

;; Run with (example):
;; OPENAI_API_KEY=... \
;; emacs --batch -l scripts/agent-shell-stress-repro.el

(require 'package)

(defun agent-shell-tests--env-int (name default)
  "Read integer environment variable NAME or return DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun agent-shell-tests--maybe-add-path (path)
  "Add PATH to `load-path` when it exists."
  (when (and path (file-directory-p path))
    (add-to-list 'load-path path)
    t))

(defun agent-shell-tests--first-existing-dir (&rest paths)
  "Return the first directory in PATHS that exists."
  (let ((found nil))
    (while (and paths (not found))
      (let ((candidate (car paths)))
        (when (and candidate (file-directory-p candidate))
          (setq found candidate)))
      (setq paths (cdr paths)))
    found))

(defun agent-shell-tests--disable-user-init-hooks ()
  "Disable user init hooks that would load external env tooling."
  (when (fboundp 'timvisher-load-op-environment-variables)
    (fset 'timvisher-load-op-environment-variables
          (lambda () (message "Skipping timvisher-load-op-environment-variables"))))
  (setq after-load-alist (assq-delete-all 'agent-shell after-load-alist))
  (setq after-load-alist (assq-delete-all "agent-shell" after-load-alist)))

(defvar agent-shell-tests--stress-done nil)
(defvar agent-shell-tests--stress-start-time nil)
(defvar agent-shell-tests--stress-warmup-start nil)
(defvar agent-shell-tests--stress-warmup-end nil)
(defvar agent-shell-tests--stress-measure-start nil)
(defvar agent-shell-tests--stress-measure-end nil)
(defvar agent-shell-tests--stress-warmup-sent nil)
(defvar agent-shell-tests--stress-warmup-seen-busy nil)
(defvar agent-shell-tests--stress-measure-sent nil)
(defvar agent-shell-tests--stress-measure-seen-busy nil)
(defvar agent-shell-tests--stress-stats nil)

(defun agent-shell-tests--stress-log (format-string &rest args)
  "Log a message when AGENT_SHELL_STRESS_DEBUG is set."
  (when (getenv "AGENT_SHELL_STRESS_DEBUG")
    (princ (apply #'format (concat format-string "\n") args))))

(defun agent-shell-tests--stress-stat-init ()
  "Initialize stress stats storage."
  (setq agent-shell-tests--stress-stats (make-hash-table :test 'equal)))

(defun agent-shell-tests--stress-stat-inc (key &optional delta)
  "Increment stats KEY by DELTA (default 1)."
  (let ((delta (or delta 1)))
    (puthash key (+ delta (or (gethash key agent-shell-tests--stress-stats) 0))
             agent-shell-tests--stress-stats)))

(defun agent-shell-tests--stress-stat-add (key delta)
  "Add DELTA to stats KEY."
  (agent-shell-tests--stress-stat-inc key delta))

(defun agent-shell-tests--stress-stat-max (key value)
  "Record max VALUE for KEY."
  (let ((current (gethash key agent-shell-tests--stress-stats)))
    (when (or (not current) (> value current))
      (puthash key value agent-shell-tests--stress-stats))))

(defun agent-shell-tests--stress-stat-get (key)
  "Get stats value for KEY."
  (gethash key agent-shell-tests--stress-stats))

(defun agent-shell-tests--stress-format-ms (start end)
  "Format elapsed time between START and END in milliseconds."
  (if (and start end)
      (format "%.1f" (* 1000.0 (- end start)))
    "none"))

(defun agent-shell-tests--stress-print-stats ()
  "Print collected stress stats."
  (when agent-shell-tests--stress-stats
    (let* ((notif-total (or (agent-shell-tests--stress-stat-get "notif.count") 0))
           (notif-time (or (agent-shell-tests--stress-stat-get "notif.time") 0.0))
           (update-frag-count (or (agent-shell-tests--stress-stat-get "update-fragment.count") 0))
           (update-frag-time (or (agent-shell-tests--stress-stat-get "update-fragment.time") 0.0))
           (ui-count (or (agent-shell-tests--stress-stat-get "ui-update.count") 0))
           (ui-time (or (agent-shell-tests--stress-stat-get "ui-update.time") 0.0))
           (md-count (or (agent-shell-tests--stress-stat-get "markdown.count") 0))
           (md-time (or (agent-shell-tests--stress-stat-get "markdown.time") 0.0))
           (parse-count (or (agent-shell-tests--stress-stat-get "json.count") 0))
           (parse-time (or (agent-shell-tests--stress-stat-get "json.time") 0.0))
           (body-total (or (agent-shell-tests--stress-stat-get "body.bytes") 0))
           (body-max (or (agent-shell-tests--stress-stat-get "body.max") 0)))
      (princ (format (concat "AGENT_SHELL_STRESS_STATS "
                             "notif_count=%d notif_time_ms=%.1f "
                             "update_fragment_count=%d update_fragment_time_ms=%.1f "
                             "ui_update_count=%d ui_update_time_ms=%.1f "
                             "markdown_count=%d markdown_time_ms=%.1f "
                             "json_count=%d json_time_ms=%.1f "
                             "body_bytes=%d body_max=%d\n")
                     notif-total (* 1000.0 notif-time)
                     update-frag-count (* 1000.0 update-frag-time)
                     ui-count (* 1000.0 ui-time)
                     md-count (* 1000.0 md-time)
                     parse-count (* 1000.0 parse-time)
                     body-total body-max))
      (maphash
       (lambda (key value)
         (when (string-prefix-p "notif.count." key)
           (let* ((suffix (string-remove-prefix "notif.count." key))
                  (time (or (agent-shell-tests--stress-stat-get
                             (concat "notif.time." suffix)) 0.0)))
             (princ (format "AGENT_SHELL_STRESS_STATS update=%s count=%d time_ms=%.1f\n"
                            suffix value (* 1000.0 time))))))
       agent-shell-tests--stress-stats))))

(defun agent-shell-tests--stress-wrap-timed (key fn)
  "Return wrapper that times FN and accumulates stats under KEY."
  (lambda (&rest args)
    (let ((start (float-time)))
      (prog1 (apply fn args)
        (agent-shell-tests--stress-stat-add key (- (float-time) start))
        (agent-shell-tests--stress-stat-inc (concat key ".count") 1)))))

(defun agent-shell-tests--stress-setup-instrumentation ()
  "Install timing instrumentation when AGENT_SHELL_STRESS_INSTRUMENT is set."
  (when (getenv "AGENT_SHELL_STRESS_INSTRUMENT")
    (agent-shell-tests--stress-stat-init)
    (when (fboundp 'agent-shell--on-notification)
      (advice-add
       'agent-shell--on-notification
       :around
       (lambda (orig &rest args)
         (let* ((notification (plist-get args :notification))
                (update (and notification (map-elt (map-elt notification 'params) 'update)))
                (update-type (or (map-elt update 'sessionUpdate)
                                 (map-elt update 'updateType)
                                 (map-elt update 'type)
                                 "unknown"))
                (start (float-time)))
           (agent-shell-tests--stress-stat-inc "notif.count" 1)
           (agent-shell-tests--stress-stat-inc (format "notif.count.%s" update-type) 1)
           (prog1 (apply orig args)
             (let ((dt (- (float-time) start)))
               (agent-shell-tests--stress-stat-add "notif.time" dt)
               (agent-shell-tests--stress-stat-add (format "notif.time.%s" update-type) dt)))))))
    (when (fboundp 'agent-shell--update-fragment)
      (advice-add
       'agent-shell--update-fragment
       :around
       (lambda (orig &rest args)
         (let* ((body (plist-get args :body))
                (body-len (and (stringp body) (length body)))
                (start (float-time)))
           (when body-len
             (agent-shell-tests--stress-stat-add "body.bytes" body-len)
             (agent-shell-tests--stress-stat-max "body.max" body-len))
           (agent-shell-tests--stress-stat-inc "update-fragment.count" 1)
           (prog1 (apply orig args)
             (agent-shell-tests--stress-stat-add "update-fragment.time"
                                                 (- (float-time) start)))))))
    (when (fboundp 'agent-shell-ui-update-fragment)
      (advice-add
       'agent-shell-ui-update-fragment
       :around
       (lambda (orig &rest args)
         (let ((start (float-time)))
           (agent-shell-tests--stress-stat-inc "ui-update.count" 1)
           (prog1 (apply orig args)
             (agent-shell-tests--stress-stat-add "ui-update.time"
                                                 (- (float-time) start)))))))
    (when (fboundp 'markdown-overlays-put)
      (advice-add
       'markdown-overlays-put
       :around
       (lambda (orig &rest args)
         (let ((start (float-time)))
           (agent-shell-tests--stress-stat-inc "markdown.count" 1)
           (prog1 (apply orig args)
             (agent-shell-tests--stress-stat-add "markdown.time"
                                                 (- (float-time) start)))))))
    (when (fboundp 'acp--parse-json)
      (advice-add
       'acp--parse-json
       :around
       (lambda (orig &rest args)
         (let ((start (float-time)))
           (agent-shell-tests--stress-stat-inc "json.count" 1)
           (prog1 (apply orig args)
             (agent-shell-tests--stress-stat-add "json.time"
                                                 (- (float-time) start)))))))
    (agent-shell-tests--stress-log "Instrumentation enabled.")))
(defun agent-shell-tests--stress-finish (status)
  "Print STATUS and exit with success or failure."
  (let ((total-ms (agent-shell-tests--stress-format-ms
                   agent-shell-tests--stress-start-time
                   (float-time)))
        (warmup-ms (agent-shell-tests--stress-format-ms
                    agent-shell-tests--stress-warmup-start
                    agent-shell-tests--stress-warmup-end))
        (measure-ms (agent-shell-tests--stress-format-ms
                     agent-shell-tests--stress-measure-start
                     agent-shell-tests--stress-measure-end)))
    (princ (format (concat "AGENT_SHELL_STRESS_RESULT status=%s"
                           " warmup_ms=%s measure_ms=%s total_ms=%s\n")
                   status warmup-ms measure-ms total-ms))
    (agent-shell-tests--stress-print-stats)
    (setq agent-shell-tests--stress-done t)
    (kill-emacs (if (string= status "completed") 0 1))))

(defun agent-shell-tests--stress-try-send (message)
  "Try to send MESSAGE; return non-nil on success."
  (condition-case err
      (progn
        (agent-shell-insert :text message :submit t :no-focus t)
        t)
    (error
     (agent-shell-tests--stress-log "Send failed: %s" err)
     nil)))

(defun agent-shell-tests--stress-tick (warmup-message measure-message)
  "Advance warmup/measure state machine."
  (when-let ((buf (agent-shell--shell-buffer :no-create t)))
    (with-current-buffer buf
      (let ((busy (ignore-errors (shell-maker-busy)))
            (now (float-time)))
        (when (and (not agent-shell-tests--stress-warmup-sent)
                   (not busy))
          (when (agent-shell-tests--stress-try-send warmup-message)
            (setq agent-shell-tests--stress-warmup-sent t)
            (setq agent-shell-tests--stress-warmup-start now)
            (agent-shell-tests--stress-log "Warmup sent.")))
        (when (and agent-shell-tests--stress-warmup-sent
                   (not agent-shell-tests--stress-warmup-seen-busy)
                   busy)
          (setq agent-shell-tests--stress-warmup-seen-busy t)
          (agent-shell-tests--stress-log "Warmup busy."))
        (when (and agent-shell-tests--stress-warmup-sent
                   agent-shell-tests--stress-warmup-seen-busy
                   (not agent-shell-tests--stress-warmup-end)
                   (not busy))
          (setq agent-shell-tests--stress-warmup-end now)
          (agent-shell-tests--stress-log "Warmup done."))
        (when (and agent-shell-tests--stress-warmup-end
                   (not agent-shell-tests--stress-measure-sent)
                   (not busy))
          (when (agent-shell-tests--stress-try-send measure-message)
            (setq agent-shell-tests--stress-measure-sent t)
            (setq agent-shell-tests--stress-measure-start now)
            (agent-shell-tests--stress-log "Measure sent.")))
        (when (and agent-shell-tests--stress-measure-sent
                   (not agent-shell-tests--stress-measure-seen-busy)
                   busy)
          (setq agent-shell-tests--stress-measure-seen-busy t)
          (agent-shell-tests--stress-log "Measure busy."))
        (when (and agent-shell-tests--stress-measure-sent
                   agent-shell-tests--stress-measure-seen-busy
                   (not agent-shell-tests--stress-measure-end)
                   (not busy))
          (setq agent-shell-tests--stress-measure-end now)
          (agent-shell-tests--stress-log "Measure done.")
          (agent-shell-tests--stress-finish "completed"))))))

(defun agent-shell-tests--stress-main ()
  "Run the minimal stress repro against the active agent shell."
  (package-initialize)
  (agent-shell-tests--disable-user-init-hooks)
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory (or load-file-name buffer-file-name)))))
         (acp-dev (or (getenv "ACP_DEV_DIR")
                      (agent-shell-tests--first-existing-dir
                       (expand-file-name "~/git.experiment.worktrees/xenodium/acp.el/main")
                       (expand-file-name "~/git.experiment.worktrees/timvisher-dd/acp.el/main"))))
         (shell-maker-dev (or (getenv "SHELL_MAKER_DEV_DIR")
                              (agent-shell-tests--first-existing-dir
                               (expand-file-name "~/git.experiment.worktrees/xenodium/shell-maker/main")
                               (expand-file-name "~/git.experiment.worktrees/timvisher-dd/shell-maker/main"))))
         (agent-shell-dev (or (getenv "AGENT_SHELL_DEV_DIR") root))
         (api-key (getenv "OPENAI_API_KEY"))
         (timeout (max 1 (agent-shell-tests--env-int "AGENT_SHELL_STRESS_TIMEOUT" 90)))
         (poll (max 0.02 (agent-shell-tests--env-int "AGENT_SHELL_STRESS_POLL_INTERVAL" 0.1)))
         (warmup-message (or (getenv "AGENT_SHELL_STRESS_WARMUP_MESSAGE") "hi"))
         (measure-file (or (getenv "AGENT_SHELL_STRESS_FILE")
                           (getenv "AGENT_SHELL_LARGE_OUTPUT_FILE")))
         (measure-message (or (getenv "AGENT_SHELL_STRESS_MESSAGE")
                              (getenv "AGENT_SHELL_LARGE_OUTPUT_MESSAGE")
                              (and measure-file (format "@%s" (expand-file-name measure-file)))
                              "!for x in {0..35000}; do printf 'line %d\\n' \"$x\"; done")))
    (unless (and api-key (not (string= api-key "")))
      (error "Set OPENAI_API_KEY in the environment"))

    (agent-shell-tests--maybe-add-path acp-dev)
    (agent-shell-tests--maybe-add-path shell-maker-dev)
    (agent-shell-tests--maybe-add-path agent-shell-dev)

    (when acp-dev
      (load (expand-file-name "acp.el" acp-dev) nil t))
    (when shell-maker-dev
      (load (expand-file-name "shell-maker.el" shell-maker-dev) nil t))
    (when agent-shell-dev
      (load (expand-file-name "agent-shell-ui.el" agent-shell-dev) nil t)
      (load (expand-file-name "agent-shell.el" agent-shell-dev) nil t)
      (load (expand-file-name "agent-shell-openai.el" agent-shell-dev) nil t))

    (setq agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :api-key api-key))

    (agent-shell-openai-start-codex)
    (agent-shell-tests--stress-setup-instrumentation)
    (setq agent-shell-tests--stress-start-time (float-time))

    (run-at-time timeout nil
                 (lambda ()
                   (agent-shell-tests--stress-finish "timeout")))

    (run-at-time poll poll
                 (lambda ()
                   (agent-shell-tests--stress-tick warmup-message measure-message)))

    (while (not agent-shell-tests--stress-done)
      (accept-process-output nil poll))))

(when noninteractive
  (agent-shell-tests--stress-main))

(provide 'agent-shell-stress-repro)
;;; agent-shell-stress-repro.el ends here
