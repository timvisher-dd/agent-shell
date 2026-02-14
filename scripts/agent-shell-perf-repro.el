;;; agent-shell-perf-repro.el --- Unified ACP perf repro -*- lexical-binding: t; -*-

;; Run with (example):
;; OPENAI_API_KEY=... \
;; AGENT_SHELL_REPRO_AGENT_DIR=/path/to/agent-shell \
;; AGENT_SHELL_REPRO_SHELL_MAKER_DIR=/path/to/shell-maker \
;; AGENT_SHELL_REPRO_ACP_DIR=/path/to/acp.el \
;; emacs --batch -l scripts/agent-shell-perf-repro.el

(require 'package)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defun agent-shell-perf--env (name default)
  "Return environment variable NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        value
      default)))

(defun agent-shell-perf--env-int (name default)
  "Return integer env NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun agent-shell-perf--env-flag (name)
  "Return non-nil when env NAME is a truthy value."
  (let ((value (agent-shell-perf--env name "")))
    (not (member (downcase value) '("" "0" "false" "nil")))))

(defun agent-shell-perf--dir-with-file (path filename)
  "Return PATH if it contains FILENAME, or its directory when PATH is the file."
  (cond
   ((and path (file-directory-p path)
         (file-exists-p (expand-file-name filename path)))
    (file-name-as-directory (expand-file-name path)))
   ((and path (file-regular-p path)
         (string= (file-name-nondirectory path) filename))
    (file-name-directory path))
   (t nil)))

(defun agent-shell-perf--resolve-dir (env-name filename &rest fallbacks)
  "Resolve ENV-NAME to a directory containing FILENAME, else try FALLBACKS."
  (or (agent-shell-perf--dir-with-file (getenv env-name) filename)
      (cl-loop for candidate in fallbacks
               for dir = (agent-shell-perf--dir-with-file candidate filename)
               when dir return dir)
      (error "Set %s to directory containing %s" env-name filename)))

(defun agent-shell-perf--maybe-add-path (path)
  "Add PATH to `load-path` when it exists."
  (when (and path (file-directory-p path))
    (add-to-list 'load-path path)
    t))

(defun agent-shell-perf--disable-user-init-hooks ()
  "Disable user init hooks that would load external env tooling."
  (when (fboundp 'timvisher-load-op-environment-variables)
    (fset 'timvisher-load-op-environment-variables
          (lambda () (message "Skipping timvisher-load-op-environment-variables"))))
  (setq after-load-alist (assq-delete-all 'agent-shell after-load-alist))
  (setq after-load-alist (assq-delete-all "agent-shell" after-load-alist)))

(defvar agent-shell-perf--done nil)
(defvar agent-shell-perf--start-time nil)
(defvar agent-shell-perf--warmup-start nil)
(defvar agent-shell-perf--warmup-end nil)
(defvar agent-shell-perf--measure-start nil)
(defvar agent-shell-perf--measure-end nil)
(defvar agent-shell-perf--warmup-sent nil)
(defvar agent-shell-perf--warmup-seen-busy nil)
(defvar agent-shell-perf--measure-sent nil)
(defvar agent-shell-perf--measure-seen-busy nil)
(defvar agent-shell-perf--focus-loss-count 0)
(defvar agent-shell-perf--focus-loss-events nil)
(defvar agent-shell-perf--profile-mode nil)
(defvar agent-shell-perf--profile-output nil)
(defvar agent-shell-perf--profile-root nil)
(defvar agent-shell-perf--profile-active nil)
(defvar agent-shell-perf--gc-enabled nil)
(defvar agent-shell-perf--gc-start nil)
(defvar agent-shell-perf--timing-enabled nil)
(defvar agent-shell-perf--timings nil)
(defvar agent-shell-perf--saw-terminal-output nil)
(defvar agent-shell-perf--require-terminal-output nil)

(defun agent-shell-perf--log (format-string &rest args)
  "Log a message when AGENT_SHELL_REPRO_DEBUG is set."
  (when (getenv "AGENT_SHELL_REPRO_DEBUG")
    (princ (apply #'format (concat format-string "\n") args))))

(defun agent-shell-perf--parse-profile-mode (value)
  "Parse profile VALUE into a profiler mode symbol."
  (let ((raw (downcase (or value ""))))
    (pcase raw
      ((or "" "0" "false" "nil") nil)
      ("cpu" 'cpu)
      ("mem" 'mem)
      ("cpu+mem" 'cpu+mem)
      (_ (agent-shell-perf--log "Unknown profile mode: %s" raw)
         nil))))

(defun agent-shell-perf--profile-path ()
  "Return the path for the profiler report."
  (when agent-shell-perf--profile-mode
    (let ((path (or agent-shell-perf--profile-output "")))
      (when (string-empty-p path)
        (let ((dir (expand-file-name ".agent-shell/transcripts"
                                     (or agent-shell-perf--profile-root
                                         default-directory))))
          (make-directory dir t)
          (setq path (expand-file-name
                      (format "agent-shell-perf-profile-%s.txt"
                              (format-time-string "%Y%m%d-%H%M%S"))
                      dir))))
      path)))

(defun agent-shell-perf--profile-path-with-suffix (base suffix)
  "Return BASE path with optional SUFFIX inserted before extension."
  (if (and suffix (not (string-empty-p suffix)))
      (let ((ext (file-name-extension base t)))
        (concat (file-name-sans-extension base) "-" suffix ext))
    base))

(defun agent-shell-perf--maybe-start-profile ()
  "Start the profiler when enabled."
  (when (and agent-shell-perf--profile-mode (fboundp 'profiler-start))
    (require 'profiler)
    (profiler-start agent-shell-perf--profile-mode)
    (setq agent-shell-perf--profile-active t)
    (agent-shell-perf--log "Profiler started: %s" agent-shell-perf--profile-mode)))

(defun agent-shell-perf--maybe-stop-profile ()
  "Stop the profiler and write report when active."
  (when (and agent-shell-perf--profile-active (fboundp 'profiler-stop))
    (require 'profiler)
    (profiler-stop)
    (let* ((want-cpu (memq agent-shell-perf--profile-mode '(cpu cpu+mem)))
           (want-mem (memq agent-shell-perf--profile-mode '(mem cpu+mem)))
           (multiple (and want-cpu want-mem))
           (base (agent-shell-perf--profile-path)))
      (when base
        (dolist (entry (delq nil
                             (list (when want-cpu
                                     (cons (profiler-cpu-profile)
                                           (when multiple "cpu")))
                                   (when want-mem
                                     (cons (profiler-memory-profile)
                                           (when multiple "mem"))))))
          (let ((profile (car entry))
                (suffix (cdr entry)))
            (when profile
              (let* ((buffer (profiler-report-setup-buffer profile))
                     (path (agent-shell-perf--profile-path-with-suffix base suffix)))
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (profiler-report-expand-entry t)
                  (write-region (point-min) (point-max) path))
                (kill-buffer buffer)
                (princ (format "AGENT_SHELL_PERF_PROFILE path=%s\n" path))))))))
    (setq agent-shell-perf--profile-active nil)))

(defun agent-shell-perf--gc-stamp ()
  "Return a GC stamp with counts and elapsed time."
  (list :gcs gcs-done
        :elapsed (when (boundp 'gc-elapsed) gc-elapsed)))

(defun agent-shell-perf--gc-mark-start ()
  "Capture GC start stamp when enabled."
  (when agent-shell-perf--gc-enabled
    (setq agent-shell-perf--gc-start (agent-shell-perf--gc-stamp))))

(defun agent-shell-perf--gc-report ()
  "Report GC delta when enabled and started."
  (when agent-shell-perf--gc-start
    (let* ((end (agent-shell-perf--gc-stamp))
           (gcs (let ((start (plist-get agent-shell-perf--gc-start :gcs)))
                  (and start (- (plist-get end :gcs) start))))
           (elapsed (let ((start (plist-get agent-shell-perf--gc-start :elapsed)))
                      (and start (plist-get end :elapsed)
                           (- (plist-get end :elapsed) start)))))
      (princ (format "AGENT_SHELL_PERF_GC_RESULT gcs=%s gc_elapsed=%s\n"
                     (if gcs (format "%d" gcs) "unknown")
                     (if elapsed (format "%.3f" elapsed) "unknown"))))))

(defun agent-shell-perf--timing-add (name elapsed)
  "Record ELAPSED seconds for NAME."
  (let* ((entry (gethash name agent-shell-perf--timings (cons 0 0.0)))
         (count (1+ (car entry)))
         (total (+ (cdr entry) elapsed)))
    (puthash name (cons count total) agent-shell-perf--timings)))

(defun agent-shell-perf--make-timing-advice (name)
  "Return advice wrapper for NAME."
  (lambda (orig &rest args)
    (let ((start (float-time)))
      (prog1 (apply orig args)
        (agent-shell-perf--timing-add name (- (float-time) start))))))

(defun agent-shell-perf--install-timing ()
  "Install timing advices when enabled."
  (when agent-shell-perf--timing-enabled
    (setq agent-shell-perf--timings (make-hash-table :test 'equal))
    (dolist (spec '((agent-shell--terminal-append-output . "terminal-append-output")
                    (agent-shell--terminal-truncate-output . "terminal-truncate-output")
                    (agent-shell--terminal-output-text . "terminal-output-text")
                    (agent-shell--append-tool-call-output . "append-tool-call-output")
                    (agent-shell-ui-append-to-fragment-body . "ui-append-fragment-body")))
      (when (fboundp (car spec))
        (advice-add (car spec) :around
                    (agent-shell-perf--make-timing-advice (cdr spec)))))))

(defun agent-shell-perf--timing-report ()
  "Report timing metrics when enabled."
  (when (and agent-shell-perf--timing-enabled agent-shell-perf--timings)
    (let ((names nil))
      (maphash (lambda (key _val) (push key names)) agent-shell-perf--timings)
      (dolist (name (sort names #'string<))
        (let* ((entry (gethash name agent-shell-perf--timings))
               (count (car entry))
               (total (cdr entry))
               (avg (if (> count 0) (/ total count) 0.0)))
          (princ (format
                  "AGENT_SHELL_PERF_TIMING name=%s count=%d total_ms=%.3f avg_ms=%.4f\n"
                  name count (* 1000.0 total) (* 1000.0 avg))))))))

(defun agent-shell-perf--format-ms (start end)
  "Format elapsed time between START and END in milliseconds."
  (if (and start end)
      (format "%.1f" (* 1000.0 (- end start)))
    "none"))

(defun agent-shell-perf--prompt-at-end-p (buf)
  "Return non-nil when BUF point is at the last prompt."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (ignore-errors (shell-maker-point-at-last-prompt-p)))))

(defun agent-shell-perf--record-focus-loss (buf where)
  "Record a prompt focus loss event for BUF in WHERE."
  (setq agent-shell-perf--focus-loss-count
        (1+ agent-shell-perf--focus-loss-count))
  (when (< (length agent-shell-perf--focus-loss-events) 5)
    (with-current-buffer buf
      (push (list :where where
                  :buffer (buffer-name)
                  :point (point)
                  :point-max (point-max))
            agent-shell-perf--focus-loss-events))))

(defun agent-shell-perf--check-focus (buf was-at-prompt where)
  "If BUF was at prompt and no longer is, record WHERE."
  (when (and was-at-prompt buf (buffer-live-p buf))
    (unless (agent-shell-perf--prompt-at-end-p buf)
      (agent-shell-perf--record-focus-loss buf where))))

(defun agent-shell-perf--advice-update-fragment (orig &rest args)
  "Advice for `agent-shell--update-fragment` to detect focus loss."
  (let* ((state (plist-get args :state))
         (buf (and state (map-elt state :buffer)))
         (was-at-prompt (agent-shell-perf--prompt-at-end-p buf)))
    (prog1 (apply orig args)
      (agent-shell-perf--check-focus buf was-at-prompt "update-fragment"))))

(defun agent-shell-perf--advice-append-tool-call-output (orig state tool-call-id text)
  "Advice for `agent-shell--append-tool-call-output` to detect focus loss."
  (let* ((buf (and state (map-elt state :buffer)))
         (was-at-prompt (agent-shell-perf--prompt-at-end-p buf)))
    (prog1 (funcall orig state tool-call-id text)
      (agent-shell-perf--check-focus buf was-at-prompt "append-tool-call-output"))))

(defun agent-shell-perf--install-focus-checks ()
  "Install prompt focus regression checks."
  (when (fboundp 'agent-shell--update-fragment)
    (advice-add 'agent-shell--update-fragment :around
                #'agent-shell-perf--advice-update-fragment))
  (when (fboundp 'agent-shell--append-tool-call-output)
    (advice-add 'agent-shell--append-tool-call-output :around
                #'agent-shell-perf--advice-append-tool-call-output)))

(defun agent-shell-perf--terminal-output-data (update)
  "Return terminal output data string from UPDATE meta, if present."
  (let* ((meta (or (map-elt update '_meta)
                   (map-elt update 'meta)))
         (terminal (and meta
                        (or (map-elt meta 'terminal_output)
                            (map-elt meta 'terminal-output)
                            (map-elt meta "terminal_output")
                            (map-elt meta "terminal-output"))))
         (data (and terminal
                    (or (map-elt terminal 'data)
                        (map-elt terminal "data")))))
    (when (stringp data)
      data)))

(defun agent-shell-perf--maybe-record-terminal-output (update)
  "Mark terminal output seen when UPDATE contains terminal output meta."
  (when (and (not agent-shell-perf--saw-terminal-output)
             (agent-shell-perf--terminal-output-data update))
    (setq agent-shell-perf--saw-terminal-output t)))

(defun agent-shell-perf--advice-handle-tool-call-update-streaming (orig state update)
  "Advice for `agent-shell--handle-tool-call-update-streaming`."
  (agent-shell-perf--maybe-record-terminal-output update)
  (funcall orig state update))

(defun agent-shell-perf--advice-handle-tool-call-update (orig state update &optional output-text)
  "Advice for `agent-shell--handle-tool-call-update`."
  (agent-shell-perf--maybe-record-terminal-output update)
  (funcall orig state update output-text))

(defun agent-shell-perf--advice-on-notification (orig &rest args)
  "Advice for `agent-shell--on-notification`."
  (let* ((notification (plist-get args :notification))
         (update (and notification
                      (map-elt notification 'params)
                      (map-elt (map-elt notification 'params) 'update))))
    (when update
      (agent-shell-perf--maybe-record-terminal-output update)))
  (apply orig args))

(defun agent-shell-perf--install-terminal-output-checks ()
  "Install terminal output detection for perf repro."
  (when (fboundp 'agent-shell--handle-tool-call-update-streaming)
    (advice-add 'agent-shell--handle-tool-call-update-streaming :around
                #'agent-shell-perf--advice-handle-tool-call-update-streaming))
  (when (fboundp 'agent-shell--handle-tool-call-update)
    (advice-add 'agent-shell--handle-tool-call-update :around
                #'agent-shell-perf--advice-handle-tool-call-update))
  (when (fboundp 'agent-shell--on-notification)
    (advice-add 'agent-shell--on-notification :around
                #'agent-shell-perf--advice-on-notification)))

(defun agent-shell-perf--ensure-point-at-end ()
  "Move point to end of the shell buffer if present."
  (when-let ((buf (agent-shell--shell-buffer :no-create t)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))))))

(defun agent-shell-perf--finish (status)
  "Print STATUS and exit with success or failure."
  (agent-shell-perf--maybe-stop-profile)
  (let* ((terminal-seen agent-shell-perf--saw-terminal-output)
         (require-terminal agent-shell-perf--require-terminal-output)
         (final-status (if (and require-terminal (not terminal-seen)
                                (string= status "completed"))
                           "missing_terminal_output"
                         status))
         (total-ms (agent-shell-perf--format-ms
                    agent-shell-perf--start-time
                    (float-time)))
         (warmup-ms (agent-shell-perf--format-ms
                     agent-shell-perf--warmup-start
                     agent-shell-perf--warmup-end))
         (measure-ms (agent-shell-perf--format-ms
                      agent-shell-perf--measure-start
                      agent-shell-perf--measure-end))
         (focus-loss agent-shell-perf--focus-loss-count)
         (ok (and (string= final-status "completed")
                  (= focus-loss 0)
                  (or (not require-terminal) terminal-seen))))
    (princ (format (concat "AGENT_SHELL_PERF_RESULT status=%s"
                           " warmup_ms=%s measure_ms=%s total_ms=%s"
                           " prompt_focus_loss=%d terminal_output_seen=%d\n")
                   final-status warmup-ms measure-ms total-ms focus-loss
                   (if terminal-seen 1 0)))
    (agent-shell-perf--gc-report)
    (agent-shell-perf--timing-report)
    (when agent-shell-perf--focus-loss-events
      (dolist (event (nreverse agent-shell-perf--focus-loss-events))
        (princ (format
                "AGENT_SHELL_PERF_FOCUS_LOSS where=%s buffer=%s point=%d point_max=%d\n"
                (plist-get event :where)
                (plist-get event :buffer)
                (plist-get event :point)
                (plist-get event :point-max)))))
    (setq agent-shell-perf--done t)
    (kill-emacs (if ok 0 1))))

(defun agent-shell-perf--try-send (message)
  "Try to send MESSAGE; return non-nil on success."
  (condition-case err
      (progn
        (agent-shell-perf--ensure-point-at-end)
        (agent-shell-insert :text message :submit t :no-focus t)
        t)
    (error
     (agent-shell-perf--log "Send failed: %s" err)
     nil)))

(defun agent-shell-perf--tick (warmup-message measure-message)
  "Advance warmup/measure state machine."
  (when-let ((buf (agent-shell--shell-buffer :no-create t)))
    (with-current-buffer buf
      (let ((busy (ignore-errors (shell-maker-busy)))
            (now (float-time)))
        (when (and (not agent-shell-perf--warmup-sent)
                   (not busy))
          (when (agent-shell-perf--try-send warmup-message)
            (setq agent-shell-perf--warmup-sent t)
            (setq agent-shell-perf--warmup-start now)
            (agent-shell-perf--log "Warmup sent.")))
        (when (and agent-shell-perf--warmup-sent
                   (not agent-shell-perf--warmup-seen-busy)
                   busy)
          (setq agent-shell-perf--warmup-seen-busy t)
          (agent-shell-perf--log "Warmup busy."))
        (when (and agent-shell-perf--warmup-sent
                   agent-shell-perf--warmup-seen-busy
                   (not agent-shell-perf--warmup-end)
                   (not busy))
          (setq agent-shell-perf--warmup-end now)
          (agent-shell-perf--log "Warmup done."))
        (when (and agent-shell-perf--warmup-end
                   (not agent-shell-perf--measure-sent)
                   (not busy))
          (when (agent-shell-perf--try-send measure-message)
            (setq agent-shell-perf--measure-sent t)
            (setq agent-shell-perf--measure-start now)
            (agent-shell-perf--gc-mark-start)
            (agent-shell-perf--maybe-start-profile)
            (agent-shell-perf--log "Measure sent.")))
        (when (and agent-shell-perf--measure-sent
                   (not agent-shell-perf--measure-seen-busy)
                   busy)
          (setq agent-shell-perf--measure-seen-busy t)
          (agent-shell-perf--log "Measure busy."))
        (when (and agent-shell-perf--measure-sent
                   agent-shell-perf--measure-seen-busy
                   (not agent-shell-perf--measure-end)
                   (not busy))
          (setq agent-shell-perf--measure-end now)
          (agent-shell-perf--log "Measure done.")
          (agent-shell-perf--finish "completed"))))))

(defun agent-shell-perf--main ()
  "Run the perf repro against the configured agent shell." 
  (package-initialize)
  (agent-shell-perf--disable-user-init-hooks)
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory (or load-file-name buffer-file-name)))))
         (agent-shell-dir (agent-shell-perf--resolve-dir
                           "AGENT_SHELL_REPRO_AGENT_DIR"
                           "agent-shell.el"
                           root
                           (expand-file-name "~/git.experiment.worktrees/xenodium/agent-shell/main")))
         (shell-maker-dir (agent-shell-perf--resolve-dir
                           "AGENT_SHELL_REPRO_SHELL_MAKER_DIR"
                           "shell-maker.el"
                           (expand-file-name "~/git.experiment.worktrees/xenodium/shell-maker/main")))
         (acp-dir (agent-shell-perf--resolve-dir
                   "AGENT_SHELL_REPRO_ACP_DIR"
                   "acp.el"
                   (expand-file-name "~/git.experiment.worktrees/xenodium/acp.el/main")))
         (cwd (agent-shell-perf--env "AGENT_SHELL_REPRO_CWD" default-directory))
         (api-key (getenv "OPENAI_API_KEY"))
         (timeout (max 1 (agent-shell-perf--env-int "AGENT_SHELL_REPRO_TIMEOUT" 90)))
         (poll (max 0.02 (agent-shell-perf--env-int "AGENT_SHELL_REPRO_POLL_INTERVAL" 0.1)))
         (warmup-message (agent-shell-perf--env "AGENT_SHELL_REPRO_WARMUP_MESSAGE" "hi"))
         (measure-message (agent-shell-perf--env
                           "AGENT_SHELL_REPRO_MESSAGE"
                           "!for x in {0..35000}; do printf 'line %d\\n' \"$x\"; done")))
    (unless (and api-key (not (string= api-key "")))
      (error "Set OPENAI_API_KEY in the environment"))
    (agent-shell-perf--log "Using agent-shell: %s" agent-shell-dir)
    (agent-shell-perf--log "Using shell-maker: %s" shell-maker-dir)
    (agent-shell-perf--log "Using acp.el: %s" acp-dir)
    (setq agent-shell-perf--profile-root root)
    (setq agent-shell-perf--profile-mode
          (agent-shell-perf--parse-profile-mode
           (agent-shell-perf--env "AGENT_SHELL_REPRO_PROFILE" "")))
    (let ((profile-output (agent-shell-perf--env "AGENT_SHELL_REPRO_PROFILE_OUTPUT" "")))
      (when (not (string-empty-p profile-output))
        (setq agent-shell-perf--profile-output
              (expand-file-name profile-output root))))
    (setq agent-shell-perf--gc-enabled
          (agent-shell-perf--env-flag "AGENT_SHELL_REPRO_GC"))
    (setq agent-shell-perf--timing-enabled
          (agent-shell-perf--env-flag "AGENT_SHELL_REPRO_TIMING"))
    (setq agent-shell-perf--require-terminal-output
          (agent-shell-perf--env-flag "AGENT_SHELL_REPRO_REQUIRE_TERMINAL_OUTPUT"))
    (setq agent-shell-perf--saw-terminal-output nil)
    (setq default-directory (file-name-as-directory (expand-file-name cwd)))

    (agent-shell-perf--maybe-add-path acp-dir)
    (agent-shell-perf--maybe-add-path shell-maker-dir)
    (agent-shell-perf--maybe-add-path agent-shell-dir)

    (load (expand-file-name "acp.el" acp-dir) nil t)
    (load (expand-file-name "shell-maker.el" shell-maker-dir) nil t)
    (load (expand-file-name "agent-shell-ui.el" agent-shell-dir) nil t)
    (load (expand-file-name "agent-shell.el" agent-shell-dir) nil t)
    (load (expand-file-name "agent-shell-openai.el" agent-shell-dir) nil t)

    (agent-shell-perf--install-focus-checks)
    (agent-shell-perf--install-timing)
    (agent-shell-perf--install-terminal-output-checks)

    (setq agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :api-key api-key))

    (agent-shell-openai-start-codex)
    (setq agent-shell-perf--start-time (float-time))

    (run-at-time timeout nil
                 (lambda ()
                   (agent-shell-perf--finish "timeout")))

    (run-at-time poll poll
                 (lambda ()
                   (agent-shell-perf--tick warmup-message measure-message)))

    (while (not agent-shell-perf--done)
      (accept-process-output nil poll))))

(when noninteractive
  (agent-shell-perf--main))

(provide 'agent-shell-perf-repro)
;;; agent-shell-perf-repro.el ends here
