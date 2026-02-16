;;; agent-shell-large-output-repro.el --- Live large-output repro -*- lexical-binding: t; -*-

;; Run with (example):
;; OPENAI_API_KEY=... \
;; AGENT_SHELL_LARGE_OUTPUT_FILE=/path/to/large.txt \
;; emacs --batch -l scripts/agent-shell-large-output-repro.el

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

(defun agent-shell-tests--file-line-count (path)
  "Return line count for PATH when readable."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (count-lines (point-min) (point-max)))))

(defun agent-shell-tests--disable-user-init-hooks ()
  "Disable user init hooks that would load external env tooling."
  (when (fboundp 'timvisher-load-op-environment-variables)
    (fset 'timvisher-load-op-environment-variables
          (lambda () (message "Skipping timvisher-load-op-environment-variables"))))
  (setq after-load-alist (assq-delete-all 'agent-shell after-load-alist))
  (setq after-load-alist (assq-delete-all "agent-shell" after-load-alist)))

(defvar agent-shell-tests--large-output-done nil)
(defvar agent-shell-tests--large-output-sent nil)
(defvar agent-shell-tests--large-output-seen-busy nil)
(defvar agent-shell-tests--large-output-max-lines nil)
(defvar agent-shell-tests--large-output-start-time nil)
(defvar agent-shell-tests--large-output-peak-lines 0)
(defvar agent-shell-tests--large-output-peak-size 0)
(defvar agent-shell-tests--large-output-merge-count 0)
(defvar agent-shell-tests--large-output-instrument
  (let ((value (getenv "AGENT_SHELL_LARGE_OUTPUT_INSTRUMENT")))
    (and value (not (string= value "")))))
(defvar agent-shell-tests--large-output-metrics (make-hash-table :test 'eq))
(defvar agent-shell-tests--large-output-first-tool-update-time nil)
(defvar agent-shell-tests--large-output-last-tool-update-time nil)
(defvar agent-shell-tests--large-output-init-request nil)
(defvar agent-shell-tests--large-output-init-json nil)
(defvar agent-shell-tests--large-output-init-capabilities nil)

(defun agent-shell-tests--large-output-log (format-string &rest args)
  "Log a message when AGENT_SHELL_LARGE_OUTPUT_DEBUG is set."
  (when (getenv "AGENT_SHELL_LARGE_OUTPUT_DEBUG")
    (princ (apply #'format (concat format-string "\n") args))))

(defun agent-shell-tests--metric-inc (key delta)
  "Increment metric KEY by DELTA."
  (let ((current (gethash key agent-shell-tests--large-output-metrics 0)))
    (puthash key (+ current delta) agent-shell-tests--large-output-metrics)))

(defun agent-shell-tests--metric-count (key)
  "Increment counter metric KEY."
  (agent-shell-tests--metric-inc key 1))

(defun agent-shell-tests--metric-format (key value)
  "Format metric KEY with VALUE."
  (let ((name (symbol-name key)))
    (if (string-suffix-p "-time" name)
        (format "%.2fms" (* 1000.0 value))
      (format "%s" value))))

(defun agent-shell-tests--large-output-report-metrics ()
  "Print instrumentation metrics."
  (when agent-shell-tests--large-output-instrument
    (princ "AGENT_SHELL_LARGE_OUTPUT_METRICS\n")
    (maphash
     (lambda (key value)
       (princ (format "  %s=%s\n"
                      (symbol-name key)
                      (agent-shell-tests--metric-format key value))))
     agent-shell-tests--large-output-metrics)
    (when agent-shell-tests--large-output-init-capabilities
      (princ (format "  init-client-capabilities=%s\n"
                     (prin1-to-string agent-shell-tests--large-output-init-capabilities))))
    (when agent-shell-tests--large-output-init-json
      (let ((json (replace-regexp-in-string "\n\\'" "" agent-shell-tests--large-output-init-json)))
        (princ (format "  init-request-json=%s\n" json))))
    (when agent-shell-tests--large-output-first-tool-update-time
      (princ (format "  tool_update_window_ms=%.2f\n"
                     (* 1000.0 (- (or agent-shell-tests--large-output-last-tool-update-time
                                      agent-shell-tests--large-output-first-tool-update-time)
                                  agent-shell-tests--large-output-first-tool-update-time)))))))

(defun agent-shell-tests--instrument-around (symbol key)
  "Instrument SYMBOL and accumulate time in KEY."
  (when (fboundp symbol)
    (advice-add
     symbol :around
     (lambda (orig &rest args)
       (let ((start (float-time)))
         (prog1 (apply orig args)
           (agent-shell-tests--metric-inc key (- (float-time) start))
           (agent-shell-tests--metric-count (intern (format "%s-count" key)))))))))

(defun agent-shell-tests--instrument-acp-parse (orig json)
  "Instrument JSON parsing with ORIG on JSON string."
  (let ((start (float-time)))
    (prog1 (funcall orig json)
      (agent-shell-tests--metric-inc 'acp-json-time (- (float-time) start))
      (agent-shell-tests--metric-inc 'acp-json-bytes (length json))
      (agent-shell-tests--metric-count 'acp-json-count))))

(defun agent-shell-tests--instrument-tool-update (orig state update &optional output-text)
  "Instrument tool_call_update handling for UPDATE."
  (let* ((start (float-time))
         (content (map-elt update 'content))
         (terminal-data (when (fboundp 'agent-shell--tool-call-terminal-output-data)
                          (agent-shell--tool-call-terminal-output-data update)))
         (chunk (or terminal-data
                    (and content (agent-shell--tool-call-content-text content)))))
    (when chunk
      (agent-shell-tests--metric-inc 'tool-update-bytes (length chunk))
      (agent-shell-tests--metric-inc 'tool-update-newlines (cl-count ?\n chunk)))
    (prog1 (funcall orig state update output-text)
      (agent-shell-tests--metric-inc 'tool-update-time (- (float-time) start))
      (agent-shell-tests--metric-count 'tool-update-count))))

(defun agent-shell-tests--instrument-append-output (orig state tool-call-id text)
  "Instrument appending tool call TEXT."
  (let ((start (float-time)))
    (when text
      (agent-shell-tests--metric-inc 'append-output-bytes (length text))
      (agent-shell-tests--metric-inc 'append-output-newlines (cl-count ?\n text)))
    (prog1 (funcall orig state tool-call-id text)
      (agent-shell-tests--metric-inc 'append-output-time (- (float-time) start))
      (agent-shell-tests--metric-count 'append-output-count))))

(defun agent-shell-tests--instrument-send-request (orig &rest args)
  "Instrument outgoing ACP requests."
  (let ((request (plist-get args :request)))
    (when (and agent-shell-tests--large-output-instrument request)
      (when (equal (map-elt request :method) "initialize")
        (setq agent-shell-tests--large-output-init-request request)
        (setq agent-shell-tests--large-output-init-json (acp--serialize-json request))
        (setq agent-shell-tests--large-output-init-capabilities
              (map-nested-elt request '(:params clientCapabilities))))))
  (apply orig args))

(defun agent-shell-tests--instrument-on-notification (orig &rest args)
  "Instrument notifications."
  (let ((start (float-time)))
    (prog1 (apply orig args)
      (agent-shell-tests--metric-inc 'notification-time (- (float-time) start))
      (agent-shell-tests--metric-count 'notification-count)
      (let ((notification (plist-get args :notification)))
        (when (and notification
                   (equal (map-elt notification 'method) "session/update"))
          (let* ((params (map-elt notification 'params))
                 (update (and params (map-elt params 'update)))
                 (update-type (and update (or (map-elt update 'sessionUpdate)
                                              (map-elt update 'type)
                                              (map-elt update 'updateType)))))
            (when update-type
              (agent-shell-tests--metric-count
               (intern (format "update-%s-count" update-type))))
            (when (equal update-type "tool_call_update")
              (let ((now (float-time)))
                (unless agent-shell-tests--large-output-first-tool-update-time
                  (setq agent-shell-tests--large-output-first-tool-update-time now))
                (setq agent-shell-tests--large-output-last-tool-update-time now)))))))))

(defun agent-shell-tests--large-output-merge-advice (orig old-content new-content)
  "Log merge behavior for tool call content when debugging."
  (when (and (getenv "AGENT_SHELL_LARGE_OUTPUT_DEBUG")
             (< agent-shell-tests--large-output-merge-count 5))
    (let* ((old-items (agent-shell--tool-call-content-items old-content))
           (new-items (agent-shell--tool-call-content-items new-content))
           (old-text (agent-shell--tool-call-content-joined-text old-content))
           (new-text (agent-shell--tool-call-content-joined-text new-content))
           (old-len (and (stringp old-text) (length old-text)))
           (new-len (and (stringp new-text) (length new-text)))
           (old-count (and old-items (length old-items)))
           (new-count (and new-items (length new-items)))
           (prefix (and (stringp old-text)
                        (stringp new-text)
                        (string-prefix-p old-text new-text)))
           (contains (and (stringp old-text)
                          (stringp new-text)
                          (not (string-empty-p old-text))
                          (string-search old-text new-text)))
           (has-cr (and (stringp new-text) (string-match-p "\r" new-text)))
           (has-bs (and (stringp new-text) (string-match-p "\b" new-text))))
      (setq agent-shell-tests--large-output-merge-count
            (1+ agent-shell-tests--large-output-merge-count))
      (agent-shell-tests--large-output-log
       "merge %d old=%s new=%s old-len=%s new-len=%s prefix=%s contains=%s cr=%s bs=%s"
       agent-shell-tests--large-output-merge-count
       (or old-count "nil") (or new-count "nil")
       (or old-len "nil") (or new-len "nil")
       prefix (or contains "nil") has-cr has-bs)))
  (funcall orig old-content new-content))

(defun agent-shell-tests--large-output-finish (status)
  "Print STATUS and exit with success or failure."
  (let* ((buf (agent-shell--shell-buffer :no-create t))
         (lines (if buf
                    (with-current-buffer buf
                      (line-number-at-pos (point-max)))
                  0))
         (size (if buf
                   (with-current-buffer buf (buffer-size))
                 0))
         (max-lines agent-shell-tests--large-output-max-lines)
         (failed (and max-lines (> lines max-lines)))
         (elapsed (and agent-shell-tests--large-output-start-time
                       (- (float-time) agent-shell-tests--large-output-start-time))))
    (princ (format (concat "AGENT_SHELL_LARGE_OUTPUT_RESULT status=%s"
                           " lines=%d size=%d max=%s"
                           " peak_lines=%d peak_size=%d"
                           " elapsed=%.2fs\n")
                   status lines size (or max-lines "none")
                   agent-shell-tests--large-output-peak-lines
                   agent-shell-tests--large-output-peak-size
                   (or elapsed 0.0)))
    (agent-shell-tests--large-output-report-metrics)
    (setq agent-shell-tests--large-output-done t)
    (kill-emacs (if failed 1 0))))

(defun agent-shell-tests--large-output-main ()
  "Run the large-output repro against the active agent shell."
  (package-initialize)
  (agent-shell-tests--disable-user-init-hooks)
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory (or load-file-name buffer-file-name)))))
         (acp-dev (getenv "ACP_DEV_DIR"))
         (shell-maker-dev (getenv "SHELL_MAKER_DEV_DIR"))
         (agent-shell-dev (or (getenv "AGENT_SHELL_DEV_DIR") root))
         (file (getenv "AGENT_SHELL_LARGE_OUTPUT_FILE"))
         (message-text (or (getenv "AGENT_SHELL_LARGE_OUTPUT_MESSAGE")
                           (and file (format "@%s" (expand-file-name file)))))
         (api-key (getenv "OPENAI_API_KEY"))
         (timeout (max 1 (agent-shell-tests--env-int "AGENT_SHELL_LARGE_OUTPUT_TIMEOUT" 45)))
         (send-after (max 0 (agent-shell-tests--env-int "AGENT_SHELL_LARGE_OUTPUT_SEND_AFTER" 5)))
         (file-lines (agent-shell-tests--file-line-count file))
         (max-lines (or (agent-shell-tests--env-int "AGENT_SHELL_LARGE_OUTPUT_MAX_LINES" nil)
                        (and file-lines (* 10 file-lines))
                        200000)))
    (setq agent-shell-tests--large-output-max-lines max-lines)
    (unless message-text
      (error "Set AGENT_SHELL_LARGE_OUTPUT_FILE or AGENT_SHELL_LARGE_OUTPUT_MESSAGE"))
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

    (when agent-shell-tests--large-output-instrument
      (require 'cl-lib)
      (advice-add 'acp--parse-json :around #'agent-shell-tests--instrument-acp-parse)
      (advice-add 'acp-send-request :around #'agent-shell-tests--instrument-send-request)
      (advice-add 'agent-shell--on-notification :around
                  #'agent-shell-tests--instrument-on-notification)
      (advice-add 'agent-shell--handle-tool-call-update :around
                  #'agent-shell-tests--instrument-tool-update)
      (advice-add 'agent-shell--handle-tool-call-update-streaming :around
                  #'agent-shell-tests--instrument-tool-update)
      (advice-add 'agent-shell--append-tool-call-output :around
                  #'agent-shell-tests--instrument-append-output)
      (agent-shell-tests--instrument-around 'agent-shell--update-fragment
                                            'update-fragment-time)
      (agent-shell-tests--instrument-around 'agent-shell-ui-update-fragment
                                            'ui-update-fragment-time)
      (agent-shell-tests--instrument-around 'agent-shell-ui-append-to-fragment-body
                                            'ui-append-fragment-time)
      (agent-shell-tests--instrument-around 'shell-maker--output-filter
                                            'shell-output-filter-time))

    (when (getenv "AGENT_SHELL_LARGE_OUTPUT_DEBUG")
      (advice-add 'agent-shell--merge-tool-call-content :around
                  #'agent-shell-tests--large-output-merge-advice))

    (setq agent-shell-openai-authentication
          (agent-shell-openai-make-authentication :api-key api-key))

    (agent-shell-openai-start-codex)
    (setq agent-shell-tests--large-output-start-time (float-time))

    (run-at-time timeout nil
                 (lambda ()
                   (agent-shell-tests--large-output-finish "timeout")))

    (run-at-time 0.2 0.2
                 (lambda ()
                   (when-let ((buf (agent-shell--shell-buffer :no-create t)))
                     (with-current-buffer buf
                       (let* ((busy (ignore-errors (shell-maker-busy)))
                              (now (float-time))
                              (elapsed (- now agent-shell-tests--large-output-start-time))
                              (lines (line-number-at-pos (point-max)))
                              (size (buffer-size)))
                         (when (> lines agent-shell-tests--large-output-peak-lines)
                           (setq agent-shell-tests--large-output-peak-lines lines))
                         (when (> size agent-shell-tests--large-output-peak-size)
                           (setq agent-shell-tests--large-output-peak-size size))
                         (cond
                          ((and (not agent-shell-tests--large-output-sent)
                                (or (not busy)
                                    (>= elapsed send-after)))
                           (condition-case err
                               (progn
                                 (setq agent-shell-tests--large-output-sent t)
                                 (agent-shell-tests--large-output-log
                                  "Sending message after %.2fs (busy=%s)" elapsed busy)
                                 (agent-shell-insert :text message-text :submit t))
                             (error
                              (setq agent-shell-tests--large-output-sent nil)
                              (agent-shell-tests--large-output-log
                               "Send failed (%s), will retry" err))))
                          ((and agent-shell-tests--large-output-sent busy)
                           (setq agent-shell-tests--large-output-seen-busy t))
                          ((and agent-shell-tests--large-output-sent
                                agent-shell-tests--large-output-seen-busy
                                (not busy))
                           (agent-shell-tests--large-output-finish "completed"))))))))

    (while (not agent-shell-tests--large-output-done)
      (accept-process-output nil 0.1))))

(when noninteractive
  (agent-shell-tests--large-output-main))

(provide 'agent-shell-large-output-repro)
;;; agent-shell-large-output-repro.el ends here
