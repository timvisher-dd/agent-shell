;;; acp-minimal-stress.el --- Minimal ACP stress client -*- lexical-binding: t; -*-

;; Run with (example):
;; OPENAI_API_KEY=... \
;; ACP_PROMPT_TEXT="!for x in {0..35000}; do printf 'line %d\\n' \"$x\"; done" \
;; emacs --batch -l scripts/acp-minimal-stress.el

(require 'package)
(require 'map)
(require 'subr-x)
(require 'cl-lib)

(defun acp-minimal--env (name default)
  "Return environment variable NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        value
      default)))

(defun acp-minimal--env-int (name default)
  "Return integer env NAME or DEFAULT."
  (let ((value (getenv name)))
    (if (and value (not (string= value "")))
        (string-to-number value)
      default)))

(defun acp-minimal--maybe-add-path (path)
  "Add PATH to `load-path` when it exists."
  (when (and path (file-directory-p path))
    (add-to-list 'load-path path)
    t))

(defun acp-minimal--count-newlines (text)
  "Count newline characters in TEXT."
  (if (and text (stringp text))
      (cl-count ?\n text)
    0))

(defvar acp-minimal--stats (make-hash-table :test 'eq))
(defvar acp-minimal--first-update nil)
(defvar acp-minimal--last-update nil)
(defvar acp-minimal--first-tool-update nil)
(defvar acp-minimal--last-tool-update nil)
(defvar acp-minimal--first-terminal-update nil)
(defvar acp-minimal--last-terminal-update nil)

(defun acp-minimal--metric-inc (key delta)
  "Increment metric KEY by DELTA."
  (puthash key (+ (gethash key acp-minimal--stats 0) delta)
           acp-minimal--stats))

(defun acp-minimal--metric-count (key)
  "Increment counter metric KEY."
  (acp-minimal--metric-inc key 1))

(defun acp-minimal--on-notification (notification)
  "Handle ACP NOTIFICATION updates."
  (let* ((now (float-time))
         (method (map-elt notification 'method)))
    (when (equal method "session/update")
      (setq acp-minimal--last-update now)
      (unless acp-minimal--first-update
        (setq acp-minimal--first-update now))
      (let* ((update (map-elt (map-elt notification 'params) 'update))
             (update-type (or (map-elt update 'sessionUpdate)
                              (map-elt update 'type)
                              (map-elt update 'updateType))))
        (when update-type
          (acp-minimal--metric-count (intern (format "update-%s-count" update-type))))
        (when (equal update-type "tool_call_update")
          (setq acp-minimal--last-tool-update now)
          (unless acp-minimal--first-tool-update
            (setq acp-minimal--first-tool-update now))
          (let* ((meta (or (map-elt update '_meta)
                           (map-elt update 'meta)))
                 (terminal (and meta (map-elt meta 'terminal_output)))
                 (data (and terminal (map-elt terminal 'data))))
            (when data
              (setq acp-minimal--last-terminal-update now)
              (unless acp-minimal--first-terminal-update
                (setq acp-minimal--first-terminal-update now))
              (acp-minimal--metric-inc 'terminal-bytes (length data))
              (acp-minimal--metric-inc 'terminal-newlines
                                       (acp-minimal--count-newlines data)))))
          (when-let ((content (map-elt update 'content)))
            (let ((text (with-temp-buffer
                          (dolist (item content)
                            (cond
                             ((and (listp item)
                                   (equal (map-elt item 'type) "text"))
                              (insert (map-elt item 'text)))
                             ((stringp item)
                              (insert item))))
                          (buffer-string))))
              (acp-minimal--metric-inc 'content-bytes (length text))
              (acp-minimal--metric-inc 'content-newlines
                                       (acp-minimal--count-newlines text)))))))

(defun acp-minimal--on-request (client request)
  "Auto-respond to ACP REQUEST for CLIENT."
  (let-alist request
    (cond
     ((equal .method "session/request_permission")
      (let* ((options (map-elt .params 'options))
             (option-id (and options (map-elt (car options) 'optionId))))
        (when option-id
          (acp-send-response
           :client client
           :response (acp-make-session-request-permission-response
                      :request-id .id
                      :option-id option-id)))))
     ((equal .method "fs/read_text_file")
      (let* ((path (map-elt .params 'path))
             (line (or (map-elt .params 'line) 1))
             (limit (map-elt .params 'limit))
             (content (with-temp-buffer
                        (insert-file-contents path)
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (if limit
                            (buffer-substring-no-properties
                             (point)
                             (progn (forward-line limit) (point)))
                          (buffer-substring-no-properties (point) (point-max))))))
        (acp-send-response
         :client client
         :response (acp-make-fs-read-text-file-response
                    :request-id .id
                    :content content))))
     (t
      (acp-send-response
       :client client
       :response `((:request-id . ,.id)
                   (:error . ((code . -32601)
                              (message . ,(format "Unhandled method: %s" .method)))))))))))

(defun acp-minimal--main ()
  "Run a minimal ACP prompt cycle and report timings."
  (package-initialize)
  (let* ((acp-dev (or (getenv "ACP_DEV_DIR")
                      (expand-file-name "~/git.experiment.worktrees/timvisher-dd/acp.el/tmp-2026-02-12T18-29-12+0000")))
         (bin (acp-minimal--env "CODEX_ACP_BIN" "codex-acp"))
         (cwd (acp-minimal--env "ACP_CWD" default-directory))
         (auth-method (acp-minimal--env "ACP_AUTH_METHOD" "openai-api-key"))
         (prompt-text (acp-minimal--env
                       "ACP_PROMPT_TEXT"
                       "!for x in {0..35000}; do printf 'line %d\\n' \"$x\"; done"))
         (client-info `((name . "acp-minimal")
                        (title . "ACP Minimal")
                        (version . "0.1")))
         (timeout (max 1 (acp-minimal--env-int "ACP_PROMPT_TIMEOUT" 600)))
         (cap-meta '((terminal_output . t)))
         (client (progn
                   (acp-minimal--maybe-add-path acp-dev)
                   (require 'acp)
                   (acp-make-client
                    :command bin
                    :environment-variables nil)))
         (start-time (float-time))
         (prompt-start nil)
         (prompt-end nil))

    (acp-subscribe-to-notifications
     :client client
     :on-notification #'acp-minimal--on-notification)
    (acp-subscribe-to-requests
     :client client
     :on-request (lambda (request) (acp-minimal--on-request client request)))

    (acp-send-request
     :client client
     :request (acp-make-initialize-request
               :protocol-version 1
               :client-info client-info
               :read-text-file-capability t
               :write-text-file-capability t
               :terminal-capability t
               :meta-capabilities cap-meta)
     :sync t)

    (unless (or (string= auth-method "none")
                (string= auth-method ""))
      (acp-send-request
       :client client
       :request (acp-make-authenticate-request :method-id auth-method)
       :sync t))

    (let* ((new-resp (acp-send-request
                      :client client
                      :request (acp-make-session-new-request :cwd cwd :mcp-servers [])
                      :sync t))
           (session-id (or (map-elt new-resp 'sessionId)
                           (map-elt new-resp "sessionId"))))
      (unless session-id
        (princ (format "ACP_MINIMAL_ERROR missing_session_id response=%S\n" new-resp))
        (kill-emacs 1))
      (setq prompt-start (float-time))
      (acp-send-request
       :client client
       :request (acp-make-session-prompt-request
                 :session-id session-id
                 :prompt `(((type . "text") (text . ,prompt-text))))
       :sync t)
      (setq prompt-end (float-time)))

    (let* ((total-ms (* 1000.0 (- (float-time) start-time)))
           (prompt-ms (* 1000.0 (- (or prompt-end (float-time))
                                   (or prompt-start start-time)))))
      (princ (format "ACP_MINIMAL_RESULT total_ms=%.2f prompt_ms=%.2f\n"
                     total-ms prompt-ms))
      (when acp-minimal--first-update
        (princ (format "ACP_MINIMAL_UPDATES first_update_ms=%.2f last_update_ms=%.2f\n"
                       (* 1000.0 (- acp-minimal--first-update prompt-start))
                       (* 1000.0 (- (or acp-minimal--last-update acp-minimal--first-update)
                                    prompt-start)))))
      (when acp-minimal--first-tool-update
        (princ (format "ACP_MINIMAL_TOOL_UPDATES first_ms=%.2f last_ms=%.2f\n"
                       (* 1000.0 (- acp-minimal--first-tool-update prompt-start))
                       (* 1000.0 (- (or acp-minimal--last-tool-update
                                        acp-minimal--first-tool-update)
                                    prompt-start)))))
      (when acp-minimal--first-terminal-update
        (princ (format "ACP_MINIMAL_TERMINAL_UPDATES first_ms=%.2f last_ms=%.2f\n"
                       (* 1000.0 (- acp-minimal--first-terminal-update prompt-start))
                       (* 1000.0 (- (or acp-minimal--last-terminal-update
                                        acp-minimal--first-terminal-update)
                                    prompt-start)))))
      (maphash
       (lambda (key value)
         (princ (format "ACP_MINIMAL_METRIC %s=%s\n" key value)))
       acp-minimal--stats)))

  (kill-emacs 0))

(when noninteractive
  (acp-minimal--main))

(provide 'acp-minimal-stress)
;;; acp-minimal-stress.el ends here
