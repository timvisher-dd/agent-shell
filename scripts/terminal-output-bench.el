;;; terminal-output-bench.el --- Microbench terminal output storage -*- lexical-binding: t; -*-

(require 'package)
(require 'subr-x)
(require 'map)

(defun agent-shell-terminal-bench--env (name default)
  "Return environment variable NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        value
      default)))

(defun agent-shell-terminal-bench--env-int (name default)
  "Return integer env NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun agent-shell-terminal-bench--env-flag (name)
  "Return non-nil when env NAME is a truthy value."
  (let ((value (agent-shell-terminal-bench--env name "")))
    (not (member (downcase value) '("" "0" "false" "nil")))))

(defun agent-shell-terminal-bench--dir-with-file (path filename)
  "Return PATH if it contains FILENAME, or its directory when PATH is the file."
  (cond
   ((and path (file-directory-p path)
         (file-exists-p (expand-file-name filename path)))
    (file-name-as-directory (expand-file-name path)))
   ((and path (file-regular-p path)
         (string= (file-name-nondirectory path) filename))
    (file-name-directory path))
   (t nil)))

(defun agent-shell-terminal-bench--resolve-dir (env-name filename &rest fallbacks)
  "Resolve ENV-NAME to a directory containing FILENAME, else try FALLBACKS."
  (or (agent-shell-terminal-bench--dir-with-file (getenv env-name) filename)
      (cl-loop for candidate in fallbacks
               for dir = (agent-shell-terminal-bench--dir-with-file candidate filename)
               when dir return dir)
      (error "Set %s to directory containing %s" env-name filename)))

(defun agent-shell-terminal-bench--maybe-add-path (path)
  "Add PATH to `load-path` when it exists."
  (when (and path (file-directory-p path))
    (add-to-list 'load-path path)
    t))

(defun agent-shell-terminal-bench--make-terminal (terminal-id limit)
  "Return terminal entry for TERMINAL-ID with LIMIT bytes."
  (let ((buffer (when (fboundp 'agent-shell--terminal-make-output-buffer)
                  (agent-shell--terminal-make-output-buffer terminal-id))))
    `((:id . ,terminal-id)
      (:process . nil)
      (:output . "")
      ,@(when buffer `((:output-buffer . ,buffer)))
      (:truncated . nil)
      (:output-byte-limit . ,limit)
      (:watchers . nil)
      (:waiters . nil)
      (:released . nil))))

(defun agent-shell-terminal-bench--output-text (terminal)
  "Return output text from TERMINAL regardless of storage mode."
  (cond
   ((fboundp 'agent-shell--terminal-output-text)
    (or (agent-shell--terminal-output-text terminal) ""))
   (t (or (map-elt terminal :output) ""))))

(defun agent-shell-terminal-bench--run ()
  "Run terminal output bench and report timing."
  (package-initialize)
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory (or load-file-name buffer-file-name)))))
         (agent-shell-dir (agent-shell-terminal-bench--resolve-dir
                           "AGENT_SHELL_TERMINAL_BENCH_AGENT_DIR"
                           "agent-shell.el"
                           root
                           (expand-file-name "~/git.experiment.worktrees/xenodium/agent-shell/main")))
         (chunk-size (max 1 (agent-shell-terminal-bench--env-int
                             "AGENT_SHELL_TERMINAL_BENCH_CHUNK_SIZE" 1024)))
         (chunks (max 1 (agent-shell-terminal-bench--env-int
                         "AGENT_SHELL_TERMINAL_BENCH_CHUNKS" 10000)))
         (limit (agent-shell-terminal-bench--env-int
                 "AGENT_SHELL_TERMINAL_BENCH_LIMIT" (* chunk-size 4000)))
         (read-output (agent-shell-terminal-bench--env-flag
                       "AGENT_SHELL_TERMINAL_BENCH_READ_OUTPUT"))
         (state (list (cons :terminals nil)))
         (terminal-id "term_1")
         (terminal nil)
         (chunk (make-string chunk-size ?a))
         (append-start nil)
         (append-elapsed nil)
         (read-start nil)
         (read-elapsed nil)
         (output-bytes 0)
         (truncated nil))
    (agent-shell-terminal-bench--maybe-add-path agent-shell-dir)
    (load (expand-file-name "agent-shell.el" agent-shell-dir) nil t)
    (setq terminal (agent-shell-terminal-bench--make-terminal terminal-id limit))
    (agent-shell--terminal-put state terminal-id terminal)
    (setq append-start (float-time))
    (dotimes (_ chunks)
      (agent-shell--terminal-append-output state terminal-id chunk))
    (setq append-elapsed (- (float-time) append-start))
    (let ((current (agent-shell--terminal-get state terminal-id)))
      (setq output-bytes (string-bytes (agent-shell-terminal-bench--output-text current)))
      (setq truncated (if (map-elt current :truncated) t :false))
      (when read-output
        (setq read-start (float-time))
        (agent-shell-terminal-bench--output-text current)
        (setq read-elapsed (- (float-time) read-start)))
      (when-let ((buffer (map-elt current :output-buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    (princ (format (concat "AGENT_SHELL_TERMINAL_BENCH_RESULT"
                           " chunk_size=%d chunks=%d limit=%d"
                           " append_ms=%.3f"
                           " read_ms=%s"
                           " output_bytes=%d truncated=%s\n")
                   chunk-size chunks limit
                   (* 1000.0 append-elapsed)
                   (if read-output (format "%.3f" (* 1000.0 read-elapsed)) "n/a")
                   output-bytes truncated))))

(when noninteractive
  (agent-shell-terminal-bench--run))

(provide 'terminal-output-bench)
;;; terminal-output-bench.el ends here
