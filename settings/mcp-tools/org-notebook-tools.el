;;; org-notebook-tools.el --- MCP tools for org notebook analysis
;;; Code:

;;; Dependency Detection Functions
(require 'jupyter)

(defun org-notebook-mcp--jupyter-available-p ()
  "Check if jupyter.el package is available and loaded."
  (and (fboundp 'jupyter-kernel-info)
       (fboundp 'jupyter-all-objects)
       (fboundp 'jupyter-repl-connected-p)))

(defun org-notebook-mcp--check-jupyter-dependencies ()
  "Check for Jupyter dependencies and provide helpful error message if missing."
  (unless (org-notebook-mcp--jupyter-available-p)
    (error "Jupyter integration not available.

Required: jupyter.el package must be installed and loaded.

To install jupyter.el:
1. Install via package manager:
   M-x package-install RET jupyter RET

2. Or install from source:
   git clone https://github.com/emacs-jupyter/jupyter
   Add to your init.el: (add-to-list 'load-path \"path/to/jupyter\")

3. Ensure Jupyter is installed on your system:
   pip install jupyter

4. Restart Emacs after installation

Alternative: You can also use EIN (Emacs IPython Notebook) but the current
implementation requires jupyter.el specifically.")))

(defun org-notebook-mcp--safe-jupyter-call (func-name &rest args)
  "Safely call a Jupyter function with dependency checking."
  (org-notebook-mcp--check-jupyter-dependencies)
  (apply (symbol-function func-name) args))

;;; Utility Functions

(defconst org-notebook-mcp--token-limit 16000
  "Approximate character limit equivalent to 4k tokens (4 chars per token).")

(defun org-notebook-mcp--extract-heading-content (file-path heading-title include-content include-code include-results include-subtree)
  "Extract content, code, and/or results from a specific org heading.
Returns the requested components based on flags, with length management for LLM consumption."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))

  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))

    ;; Find the heading
    (let ((heading-found nil)
          (heading-level nil)
          (content-start nil)
          (content-end nil))

      ;; Search for the heading
      (while (and (not heading-found)
                  (re-search-forward "^\\(\\*+\\)[ \t]+\\(.*\\)$" nil t))
        (let ((current-level (length (match-string 1)))
              (current-title (match-string 2)))
          (when (string-match-p (regexp-quote heading-title) current-title)
            (setq heading-found t
                  heading-level current-level
                  content-start (line-beginning-position 2)))))

      (unless heading-found
        (error "Heading not found: %s" heading-title))

      ;; Find the end of content
      (goto-char content-start)
      (if include-subtree
          ;; Include entire subtree - find next heading at same or higher level
          (progn
            (if (re-search-forward (format "^\\*\\{1,%d\\}[ \t]+" heading-level) nil t)
                (setq content-end (line-beginning-position))
              (setq content-end (point-max))))
        ;; Only direct content - find next heading at any level
        (if (re-search-forward "^\\*+[ \t]+" nil t)
            (setq content-end (line-beginning-position))
          (setq content-end (point-max))))

      ;; Extract the raw content
      (let ((raw-content (buffer-substring-no-properties content-start content-end)))
        (org-notebook-mcp--parse-heading-components raw-content include-content include-code include-results)))))

(defun org-notebook-mcp--parse-heading-components (raw-content include-content include-code include-results)
  "Parse raw content into components: content, code, and results."
  (let ((content-parts '())
        (code-parts '())
        (results-parts '())
        (lines (split-string raw-content "\n"))
        (in-code-block nil)
        (in-results nil)
        (current-code-block '())
        (current-results '()))

    (dolist (line lines)
      (cond
       ;; Start of code block
       ((string-match "^[ \t]*#\\+BEGIN_SRC" line)
        (setq in-code-block t)
        (when include-code
          (push line current-code-block)))

       ;; End of code block
       ((string-match "^[ \t]*#\\+END_SRC" line)
        (when include-code
          (push line current-code-block)
          (push (string-join (nreverse current-code-block) "\n") code-parts))
        (setq in-code-block nil
              current-code-block '()))

       ;; Start of results
       ((string-match "^[ \t]*#\\+RESULTS:" line)
        (setq in-results t)
        (when include-results
          (push line current-results)))

       ;; Inside code block
       (in-code-block
        (when include-code
          (push line current-code-block)))

       ;; Inside results or results continuation
       ((or in-results
            (and (not (string-match "^[ \t]*#\\+\\|^[ \t]*\\*" line))
                 (string-match "^[ \t]*:" line)))
        (when include-results
          (push line current-results))
        ;; Check if results section ends
        (when (and in-results
                   (not (string-match "^[ \t]*:" line))
                   (not (string-empty-p (string-trim line)))
                   (not (string-match "^[ \t]*#\\+" line)))
          (push (string-join (nreverse current-results) "\n") results-parts)
          (setq in-results nil
                current-results '())
          ;; This line is content, not results
          (when include-content
            (push line content-parts))))

       ;; Regular content
       (t
        (when include-content
          (push line content-parts))
        ;; End any ongoing results collection
        (when in-results
          (push (string-join (nreverse current-results) "\n") results-parts)
          (setq in-results nil
                current-results '())))))

    ;; Handle any remaining code or results
    (when (and include-code current-code-block)
      (push (string-join (nreverse current-code-block) "\n") code-parts))
    (when (and include-results current-results)
      (push (string-join (nreverse current-results) "\n") results-parts))

    ;; Combine and format results
    (let* ((content-text (when include-content
                          (string-join (nreverse content-parts) "\n")))
           (code-text (when include-code
                       (string-join (nreverse code-parts) "\n\n")))
           (results-text (when include-results
                          (string-join (nreverse results-parts) "\n\n")))
           (content-length (if content-text (length content-text) 0))
           (code-length (if code-text (length code-text) 0))
           (results-length (if results-text (length results-text) 0)))

      `((content . ,(if (and include-content (< content-length org-notebook-mcp--token-limit))
                       content-text
                     nil))
        (code . ,(if (and include-code (< code-length org-notebook-mcp--token-limit))
                    code-text
                   nil))
        (results . ,(if (and include-results (< results-length org-notebook-mcp--token-limit))
                       results-text
                     nil))
        (content_length . ,content-length)
        (code_length . ,code-length)
        (results_length . ,results-length)
        (content_truncated . ,(and include-content (>= content-length org-notebook-mcp--token-limit)))
        (code_truncated . ,(and include-code (>= code-length org-notebook-mcp--token-limit)))
        (results_truncated . ,(and include-results (>= results-length org-notebook-mcp--token-limit)))))))

(defun org-notebook-mcp--extract-analytical-review (file-path &optional heading-titles)
  "Extract headings, content, and results for analytical review.
Focuses on approach and outcomes, excluding code blocks for cleaner review.
Can extract from entire notebook or specific headings."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))

  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))

    (let ((review-sections '())
          (all-headings (not heading-titles))) ; If no specific headings, get all

      ;; Find all headings or specific ones
      (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.*\\)$" nil t)
        (let* ((heading-level (length (match-string 1)))
               (heading-title (match-string 2))
               (heading-start (line-beginning-position))
               (should-include (or all-headings
                                 (cl-some (lambda (target)
                                            (string-match-p (regexp-quote target) heading-title))
                                          heading-titles))))

          (when should-include
            ;; Find content boundaries for this heading
            (let ((content-start (line-beginning-position 2))
                  (content-end nil))

              ;; Find next heading at same or higher level
              (save-excursion
                (if (re-search-forward (format "^\\*\\{1,%d\\}[ \t]+" heading-level) nil t)
                    (setq content-end (line-beginning-position))
                  (setq content-end (point-max))))

              ;; Extract and parse content
              (let* ((raw-content (buffer-substring-no-properties content-start content-end))
                     (parsed (org-notebook-mcp--parse-analytical-content raw-content))
                     (content-text (cdr (assoc 'content parsed)))
                     (results-text (cdr (assoc 'results parsed)))
                     (content-length (if content-text (length content-text) 0))
                     (results-length (if results-text (length results-text) 0)))

                ;; Add to review sections
                (push `((heading . ,heading-title)
                       (level . ,heading-level)
                       (content . ,(if (< content-length org-notebook-mcp--token-limit)
                                      content-text
                                    nil))
                       (results . ,(if (< results-length org-notebook-mcp--token-limit)
                                     results-text
                                   nil))
                       (content_length . ,content-length)
                       (results_length . ,results-length)
                       (content_truncated . ,(>= content-length org-notebook-mcp--token-limit))
                       (results_truncated . ,(>= results-length org-notebook-mcp--token-limit)))
                      review-sections))))))

      ;; Return results
      `((file_path . ,file-path)
        (extraction_mode . ,(if all-headings "full_notebook" "selected_headings"))
        (requested_headings . ,(or heading-titles '()))
        (total_sections . ,(length review-sections))
        (sections . ,(nreverse review-sections))))))

(defun org-notebook-mcp--parse-analytical-content (raw-content)
  "Parse raw content for analytical review - content and results only, no code."
  (let ((content-parts '())
        (results-parts '())
        (lines (split-string raw-content "\n"))
        (in-code-block nil)
        (in-results nil)
        (current-results '()))

    (dolist (line lines)
      (cond
       ;; Start of code block - skip everything until end
       ((string-match "^[ \t]*#\\+BEGIN_SRC" line)
        (setq in-code-block t))

       ;; End of code block
       ((string-match "^[ \t]*#\\+END_SRC" line)
        (setq in-code-block nil))

       ;; Start of results
       ((string-match "^[ \t]*#\\+RESULTS:" line)
        (setq in-results t)
        (push line current-results))

       ;; Skip content inside code blocks
       (in-code-block
        nil) ; Do nothing, skip code content

       ;; Inside results or results continuation
       ((or in-results
            (and (not (string-match "^[ \t]*#\\+\\|^[ \t]*\\*" line))
                 (string-match "^[ \t]*:" line)))
        (push line current-results)
        ;; Check if results section ends
        (when (and in-results
                   (not (string-match "^[ \t]*:" line))
                   (not (string-empty-p (string-trim line)))
                   (not (string-match "^[ \t]*#\\+" line)))
          (push (string-join (nreverse current-results) "\n") results-parts)
          (setq in-results nil
                current-results '())
          ;; This line is content, not results
          (push line content-parts)))

       ;; Regular content (not code, not results)
       (t
        (push line content-parts)
        ;; End any ongoing results collection
        (when in-results
          (push (string-join (nreverse current-results) "\n") results-parts)
          (setq in-results nil
                current-results '())))))

    ;; Handle any remaining results
    (when current-results
      (push (string-join (nreverse current-results) "\n") results-parts))

    ;; Combine and format results
    (let* ((content-text (string-join (nreverse content-parts) "\n"))
           (results-text (string-join (nreverse results-parts) "\n\n")))

      `((content . ,(if (> (length content-text) 0) content-text nil))
        (results . ,(if (> (length results-text) 0) results-text nil))))))

(defun org-notebook-mcp--extract-functions-from-code-blocks (file-path)
  "Extract all functions from code blocks in an org file.
Returns information about functions including location and docstrings."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))

  (with-temp-buffer
    (insert-file-contents file-path)
    (org-mode)
    (goto-char (point-min))

    (let ((functions '())
          (block-count 0))

      ;; Find all source blocks
      (while (re-search-forward "^[ \t]*#\\+BEGIN_SRC\\s-+\\([^\s\n]+\\)" nil t)
        (let* ((lang (match-string 1))
               (block-start (match-beginning 0))
               (block-line (line-number-at-pos block-start))
               (content-start (progn (forward-line 1) (point)))
               (content-end (save-excursion
                             (if (re-search-forward "^[ \t]*#\\+END_SRC" nil t)
                                 (match-beginning 0)
                               (point-max))))
               (content (buffer-substring-no-properties content-start content-end)))

          (setq block-count (1+ block-count))

          ;; Extract functions from supported languages
          (when (member lang '("python" "jupyter-python" "elisp" "emacs-lisp" "javascript" "js" "typescript" "ts"))
            (let ((block-functions (org-notebook-mcp--parse-functions-in-content
                                   content lang block-line content-start)))
              (setq functions (append functions block-functions))))))

      `((file_path . ,file-path)
        (total_code_blocks . ,block-count)
        (total_functions . ,(length functions))
        (functions . ,functions)))))

(defun org-notebook-mcp--parse-functions-in-content (content language block-line content-start)
  "Parse functions from CONTENT in specified LANGUAGE."
  (let ((functions '())
        (lines (split-string content "\n"))
        (current-line block-line))

    (dolist (line lines)
      (setq current-line (1+ current-line))
      (let ((func-info (org-notebook-mcp--extract-function-from-line line language current-line content-start)))
        (when func-info
          (push func-info functions))))

    (nreverse functions)))

(defun org-notebook-mcp--extract-function-from-line (line language line-number buffer-pos)
  "Extract function information from a single LINE in LANGUAGE."
  (let ((patterns
         (cond
          ((member language '("python" "jupyter-python"))
           '(("^[ \t]*def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*(" . "def")))
          ((member language '("elisp" "emacs-lisp"))
           '(("^[ \t]*(defun[ \t]+\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" . "defun")
             ("^[ \t]*(defmacro[ \t]+\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" . "defmacro")
             ("^[ \t]*(defvar[ \t]+\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" . "defvar")
             ("^[ \t]*(defcustom[ \t]+\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" . "defcustom")))
          ((member language '("javascript" "js" "typescript" "ts"))
           '(("^[ \t]*function[ \t]+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" . "function")
             ("^[ \t]*const[ \t]+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)[ \t]*=[ \t]*(" . "const function")
             ("^[ \t]*\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)[ \t]*:[ \t]*function" . "method"))))))

    (dolist (pattern-pair patterns)
      (let ((pattern (car pattern-pair))
            (type (cdr pattern-pair)))
        (when (string-match pattern line)
          (let ((func-name (match-string 1 line))
                (docstring (org-notebook-mcp--extract-docstring-hint line)))
            (return `((name . ,func-name)
                     (type . ,type)
                     (language . ,language)
                     (line_number . ,line-number)
                     (buffer_position . ,buffer-pos)
                     (docstring_preview . ,(or docstring ""))
                     (source_line . ,(string-trim line))))))))))

(defun org-notebook-mcp--extract-docstring-hint (line)
  "Extract a hint of docstring from the function line or return nil."
  (cond
   ;; Python docstring patterns
   ((string-match "\"\"\"\\(.*?\\)\"\"\"" line)
    (match-string 1 line))
   ((string-match "'''\\(.*?\\)'''" line)
    (match-string 1 line))
   ;; Elisp docstring patterns
   ((string-match "\"\\([^\"]*\\)\"" line)
    (let ((potential-doc (match-string 1 line)))
      (when (> (length potential-doc) 10)
        potential-doc)))
   ;; JavaScript comment patterns
   ((string-match "//[ \t]*\\(.*\\)" line)
    (match-string 1 line))
   ((string-match "/\\*\\*?[ \t]*\\(.*?\\)[ \t]*\\*/" line)
    (match-string 1 line))
   (t nil)))

;;; Jupyter REPL Integration Functions

(defun org-notebook-mcp--list-jupyter-repl-clients ()
  "List all available Jupyter REPL clients with their kernel information.
Returns information about active REPL connections that can be used for code execution."
  (org-notebook-mcp--check-jupyter-dependencies)
  (let ((clients '()))
    (dolist (client (jupyter-all-objects 'jupyter--clients))
      (when (and (object-of-class-p client 'jupyter-repl-client)
                 (buffer-live-p (oref client buffer)))
        (with-current-buffer (oref client buffer)
          (when (jupyter-repl-connected-p)
            (let* ((kernel-info (jupyter-kernel-info client))
                   (lang-info (plist-get kernel-info :language_info))
                   (client-info
                    `((client_id . ,(format "%s" client))
                      (buffer_name . ,(buffer-name))
                      (kernel_name . ,(plist-get kernel-info :implementation))
                      (language . ,(plist-get lang-info :name))
                      (language_version . ,(plist-get lang-info :version))
                      (status . ,(if (jupyter-repl-connected-p) "connected" "disconnected"))
                      (execution_count . ,(jupyter-repl-cell-count)))))
              (push client-info clients))))))
    `((total_clients . ,(length clients))
      (clients . ,(nreverse clients)))))

(defun org-notebook-mcp--send-code-to-jupyter-repl (client_buffer_name code &optional timeout)
  "Send CODE to the Jupyter REPL client in CLIENT_BUFFER_NAME.
Returns execution results including output and any errors.
TIMEOUT specifies how long to wait for results (default 30 seconds)."
  (org-notebook-mcp--check-jupyter-dependencies)
  (let* ((timeout (or timeout 30))
         (buffer (get-buffer client_buffer_name))
         (client nil)
         (result nil))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (with-current-buffer buffer
      (unless (jupyter-repl-connected-p)
        (error "REPL client not connected: %s" client_buffer_name))

      (setq client jupyter-current-client))

    (unless client
      (error "No jupyter client found in buffer: %s" client_buffer_name))

    ;; Execute the code synchronously and capture results
    (let ((execution-result nil)
          (execution-error nil)
          (output-content "")
          (result-content "")
          (error-content ""))

      (jupyter-run-with-client client
        (let* ((req (jupyter-execute-request
                     :code code
                     :store-history t
                     :handlers '("execute_reply" "stream" "execute_result" "display_data" "error")))
               (msg-id (jupyter-message-id req)))

          ;; Set up message handlers
          (jupyter-add-hook 'jupyter-iopub-message-hook
                            (lambda (msg)
                              (when (equal (jupyter-message-parent-id msg) msg-id)
                                (pcase (jupyter-message-type msg)
                                  ("stream"
                                   (let ((stream-content (jupyter-message-content msg)))
                                     (setq output-content
                                           (concat output-content
                                                   (plist-get stream-content :text)))))
                                  ("execute_result"
                                   (let* ((result-data (jupyter-message-content msg))
                                          (data (plist-get result-data :data))
                                          (text (plist-get data :text/plain)))
                                     (when text
                                       (setq result-content
                                             (concat result-content text)))))
                                  ("display_data"
                                   (let* ((display-data (jupyter-message-content msg))
                                          (data (plist-get display-data :data))
                                          (text (plist-get data :text/plain)))
                                     (when text
                                       (setq result-content
                                             (concat result-content text)))))
                                  ("error"
                                   (let* ((error-data (jupyter-message-content msg))
                                          (ename (plist-get error-data :ename))
                                          (evalue (plist-get error-data :evalue))
                                          (traceback (plist-get error-data :traceback)))
                                     (setq execution-error t)
                                     (setq error-content
                                           (format "%s: %s\n%s"
                                                   ename evalue
                                                   (string-join traceback "\n"))))))))

          ;; Send the request
          (jupyter-send client req)

          ;; Wait for execution to complete
          (let ((start-time (current-time)))
            (while (and (not execution-result)
                        (not execution-error)
                        (< (float-time (time-subtract (current-time) start-time)) timeout))
              (accept-process-output nil 0.1)))

          (setq execution-result t)))

      ;; Return results
      `((client_buffer . ,client_buffer_name)
        (code . ,code)
        (success . ,(not execution-error))
        (output . ,output-content)
        (result . ,result-content)
        (error . ,(if execution-error error-content nil))
        (execution_time . ,(format-time-string "%Y-%m-%d %H:%M:%S")))))))

(defun org-notebook-mcp--get-jupyter-kernel-state (client_buffer_name)
  "Get the current execution state of a Jupyter kernel.
Returns detailed information about whether the kernel is busy, idle, or has pending requests.
This is essential for LLMs to know when to wait before sending more code."
  (let* ((buffer (get-buffer client_buffer_name)))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (org-notebook-mcp--check-jupyter-dependencies)
    (with-current-buffer buffer
      (let* ((client jupyter-current-client)
             (connected-p (jupyter-repl-connected-p))
             (busy-p nil)
             (pending-requests 0))

        ;; Check if kernel is busy by looking at REPL prompt state
        (when connected-p
          (save-excursion
            (goto-char (point-max))
            (setq busy-p (looking-back "In \\[\\*\\] " nil)))

          ;; Count pending requests if client has request queue
          (when (and client (slot-exists-p client 'requests))
            (setq pending-requests (length (oref client requests)))))

        `((buffer_name . ,client_buffer_name)
          (connected . ,(if connected-p t :json-false))
          (kernel_state . ,(cond
                           ((not connected-p) "disconnected")
                           (busy-p "busy")
                           ((> pending-requests 0) "pending")
                           (t "idle")))
          (busy . ,(if busy-p t :json-false))
          (pending_requests . ,pending-requests)
          (execution_count . ,(when connected-p (jupyter-repl-cell-count)))
          (last_check . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
          (recommendations . ,(cond
                              ((not connected-p) "Kernel is disconnected - reconnect before sending code")
                              (busy-p "Kernel is busy executing - wait before sending more code")
                              ((> pending-requests 0) "Kernel has pending requests - consider waiting")
                              (t "Kernel is ready for new code"))))))))

(defun org-notebook-mcp--get-jupyter-repl-status (client_buffer_name)
  "Get comprehensive status of a Jupyter REPL client.
Returns information about connection status, kernel info, and execution history."
  (let* ((buffer (get-buffer client_buffer_name)))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (org-notebook-mcp--check-jupyter-dependencies)
    (with-current-buffer buffer
      (let* ((client jupyter-current-client)
             (connected-p (jupyter-repl-connected-p))
             (kernel-info (when client (jupyter-kernel-info client)))
             (lang-info (when kernel-info (plist-get kernel-info :language_info))))

        `((buffer_name . ,client_buffer_name)
          (connected . ,(if connected-p t :json-false))
          (kernel_name . ,(when kernel-info (plist-get kernel-info :implementation)))
          (kernel_id . ,(when client (jupyter-session-id (oref client session))))
          (language . ,(when lang-info (plist-get lang-info :name)))
          (language_version . ,(when lang-info (plist-get lang-info :version)))
          (execution_count . ,(when connected-p (jupyter-repl-cell-count)))
          (buffer_size . ,(buffer-size))
          (last_activity . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
          (kernel_alive . ,(when client (jupyter-kernel-alive-p client))))))))

(defun org-notebook-mcp--send-code-async (client_buffer_name code)
  "Send code to Jupyter REPL asynchronously without waiting for results.
Returns immediately with request information. Use get-kernel-state to check completion.
This is better for long-running computations where LLMs need to poll for completion."
  (let* ((buffer (get-buffer client_buffer_name))
         (client nil))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (org-notebook-mcp--check-jupyter-dependencies)
    (with-current-buffer buffer
      (unless (jupyter-repl-connected-p)
        (error "REPL client not connected: %s" client_buffer_name))

      (setq client jupyter-current-client))

    (unless client
      (error "No jupyter client found in buffer: %s" client_buffer_name))

    ;; Note: jupyter-run-with-client is a macro, so we need to expand it directly with dependency checks
    (org-notebook-mcp--check-jupyter-dependencies)
    (jupyter-run-with-client client
      (let* ((req (jupyter-execute-request
                    :code code
                    :store-history t))
             (msg-id (jupyter-message-id req)))

        ;; Send the request asynchronously
        (jupyter-send client req)

        ;; Return request information
        `((client_buffer . ,client_buffer_name)
          (code . ,code)
          (request_id . ,msg-id)
          (status . "sent")
          (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
          (message . "Code sent asynchronously - use get-kernel-state to check completion"))))))

(defun org-notebook-mcp--get-last-output (client_buffer_name &optional lines)
  "Get the last output from a Jupyter REPL buffer.
Useful for retrieving results after async execution completes.
LINES specifies how many lines of output to retrieve (default 50)."
  (let* ((buffer (get-buffer client_buffer_name))
         (lines (or lines 50)))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (with-current-buffer buffer
      (let* ((output-content "")
             (line-count 0)
             (pos (point-max)))

        ;; Go backwards from end of buffer, collecting lines
        (save-excursion
          (goto-char pos)
          (while (and (> line-count (- lines))
                      (not (bobp)))
            (forward-line -1)
            (let ((line (thing-at-point 'line t)))
              (setq output-content (concat line output-content))
              (setq line-count (1- line-count)))))

        `((buffer_name . ,client_buffer_name)
          (lines_retrieved . ,(abs line-count))
          (max_lines . ,lines)
          (output . ,output-content)
          (buffer_size . ,(buffer-size))
          (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S")))))))

;;; MCP Tool Functions

(defun org-notebook-mcp--get-analytical-review (file_path &optional heading_titles)
  "Extract headings, content, and results for analytical review of org notebooks.
MCP Parameters:
  file_path - The absolute path to the org file to analyze
  heading_titles - Optional list of specific heading titles to extract (array of strings)"
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (when (string-empty-p file_path)
      (error "Empty file path provided"))
    (when (and heading_titles (not (listp heading_titles)))
      (error "heading_titles must be a list of strings"))

    (let ((result (org-notebook-mcp--extract-analytical-review file_path heading_titles)))
      result)))

(defun org-notebook-mcp--get-heading-content (file_path heading_title &optional include_content include_code include_results include_subtree)
  "Extract content, code, and/or results from a specific org heading.
MCP Parameters:
  file_path - The absolute path to the org file to analyze
  heading_title - The title of the heading to extract content from
  include_content - Whether to include text content (boolean, default t)
  include_code - Whether to include code blocks (boolean, default t)
  include_results - Whether to include results blocks (boolean, default t)
  include_subtree - Whether to include entire subtree or just direct content (boolean, default nil)"
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (when (string-empty-p file_path)
      (error "Empty file path provided"))
    (unless (stringp heading_title)
      (signal 'wrong-type-argument (list 'stringp heading_title)))
    (when (string-empty-p heading_title)
      (error "Empty heading title provided"))

    ;; Set defaults
    (setq include_content (if (null include_content) t include_content))
    (setq include_code (if (null include_code) t include_code))
    (setq include_results (if (null include_results) t include_results))
    (setq include_subtree (if (null include_subtree) nil include_subtree))

    (let ((result (org-notebook-mcp--extract-heading-content
                   file_path heading_title include_content include_code include_results include_subtree)))
      result)))

(defun org-notebook-mcp--get-functions-from-org-file (file_path)
  "Extract all functions from code blocks in an org file.
MCP Parameters:
  file_path - The absolute path to the org file to analyze"
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (when (string-empty-p file_path)
      (error "Empty file path provided"))

    (let ((result (org-notebook-mcp--extract-functions-from-code-blocks file_path)))
      result)))

(defun org-notebook-mcp--list-jupyter-repls ()
  "List all available Jupyter REPL clients.
MCP Parameters: None"
  (mcp-server-lib-with-error-handling
    (let ((result (org-notebook-mcp--list-jupyter-repl-clients)))
      result)))

(defun org-notebook-mcp--send-code-to-repl (client_buffer_name code &optional timeout)
  "Send code to a Jupyter REPL client and get results.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to send code to (string, required)
  code - Code to execute in the REPL (string, required)
  timeout - Maximum time to wait for execution in seconds (integer, optional, default 30)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))
    (unless (stringp code)
      (signal 'wrong-type-argument (list 'stringp code)))
    (when (string-empty-p code)
      (error "Empty code provided"))
    (when (and timeout (not (integerp timeout)))
      (signal 'wrong-type-argument (list 'integerp timeout)))

    (let ((result (org-notebook-mcp--send-code-to-jupyter-repl client_buffer_name code timeout)))
      result)))

(defun org-notebook-mcp--get-repl-status (client_buffer_name)
  "Get the status of a Jupyter REPL client.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))

    (let ((result (org-notebook-mcp--get-jupyter-repl-status client_buffer_name)))
      result)))

(defun org-notebook-mcp--get-kernel-state (client_buffer_name)
  "Get the execution state of a Jupyter kernel to check if it's busy.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))

    (let ((result (org-notebook-mcp--get-jupyter-kernel-state client_buffer_name)))
      result)))

(defun org-notebook-mcp--send-code-async (client_buffer_name code)
  "Send code to Jupyter REPL asynchronously for long-running computations.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to send code to (string, required)
  code - Code to execute in the REPL (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))
    (unless (stringp code)
      (signal 'wrong-type-argument (list 'stringp code)))
    (when (string-empty-p code)
      (error "Empty code provided"))

    (let ((result (org-notebook-mcp--send-code-async client_buffer_name code)))
      result)))

(defun org-notebook-mcp--get-last-output (client_buffer_name &optional lines)
  "Get the last output from a Jupyter REPL buffer.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to read from (string, required)
  lines - Number of lines to retrieve (integer, optional, default 50)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))
    (when (and lines (not (integerp lines)))
      (signal 'wrong-type-argument (list 'integerp lines)))

    (let ((result (org-notebook-mcp--get-last-output client_buffer_name lines)))
      result)))

(provide 'org-notebook-tools)
;;; org-notebook-tools.el ends here
