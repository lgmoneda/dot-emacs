;;; org-notebook-tools.el --- MCP tools for org notebook analysis
;;; Code:

;;; Dependency Detection Functions
(require 'jupyter)
(require 'cl-lib)
(require 'subr-x)

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

(defvar org-notebook-mcp--results-history (make-hash-table :test 'equal)
  "Cache of previous #+RESULTS text keyed by file path and heading title.")

(defun org-notebook-mcp--results-history-key (file-path heading-title)
  "Create a canonical key for FILE-PATH and HEADING-TITLE."
  (list (expand-file-name file-path) heading-title))

(defun org-notebook-mcp--normalize-results-text (text)
  "Normalize result TEXT for diffing.
Ensures consistent trailing newline and trims surrounding whitespace."
  (let* ((normalized (string-trim-right (or text ""))))
    (if (string-suffix-p "\n" normalized)
        normalized
      (concat normalized "\n"))))

(defun org-notebook-mcp--split-lines-preserve-empty (text)
  "Split TEXT into lines while preserving empty lines."
  (let ((lines '())
        (start 0)
        (len (length text)))
    (while (< start len)
      (let ((end (string-match "\n" text start)))
        (if end
            (progn
              (push (substring text start end) lines)
              (setq start (1+ end)))
          (push (substring text start) lines)
          (setq start len))))
    (nreverse lines)))

(defun org-notebook-mcp--compute-results-diff (previous current)
  "Return simple diff string between PREVIOUS and CURRENT result strings."
  (if (string= previous current)
      ""
    (let* ((prev-lines (org-notebook-mcp--split-lines-preserve-empty (string-trim-right previous)))
           (curr-lines (org-notebook-mcp--split-lines-preserve-empty (string-trim-right current)))
           (header '("--- previous" "+++ current"))
           (removed (mapcar (lambda (line) (concat "- " line)) prev-lines))
           (added (mapcar (lambda (line) (concat "+ " line)) curr-lines))
           (diff-lines (append header removed added)))
      (string-join diff-lines "\n"))))

(defun org-notebook-mcp--diff-heading-results (file-path heading-title)
  "Compute diff for #+RESULTS under HEADING-TITLE in FILE-PATH.
Returns alist with diff metadata and caches the current result snapshot."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))
  (let* ((content (org-notebook-mcp--extract-heading-content
                   file-path heading-title nil nil t nil))
         (raw-results (alist-get 'results content))
         (current (org-notebook-mcp--normalize-results-text raw-results))
         (key (org-notebook-mcp--results-history-key file-path heading-title))
         (previous (gethash key org-notebook-mcp--results-history))
         (has-previous (and previous t))
         (changed nil)
         (diff-string ""))
    (setq changed (and has-previous (not (string= previous current))))
    (when changed
      (setq diff-string (org-notebook-mcp--compute-results-diff previous current)))
    ;; Update cache with current snapshot
    (puthash key current org-notebook-mcp--results-history)
    (let* ((current-display (string-trim-right current))
           (previous-display (when has-previous (string-trim-right previous))))
      `((file_path . ,file-path)
        (heading_title . ,heading-title)
        (has_previous . ,(if has-previous t :json-false))
        (changed . ,(if changed t :json-false))
        (diff . ,(if (string-empty-p diff-string) nil diff-string))
        (previous_result . ,previous-display)
        (current_result . ,current-display)
        (history_cached . t)
        (note . ,(cond
                  ((not has-previous)
                   "No previous result cached; stored current snapshot for future comparisons.")
                  (changed
                   "Differences detected between previous and current results.")
                  (t
                   "No differences detected; cache refreshed with current result.")))))))

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

    (cl-block extract-function
      (dolist (pattern-pair patterns)
        (let ((pattern (car pattern-pair))
              (type (cdr pattern-pair)))
          (when (string-match pattern line)
            (let ((func-name (match-string 1 line))
                   (docstring (org-notebook-mcp--extract-docstring-hint line)))
              (cl-return-from extract-function `((name . ,func-name)
                       (type . ,type)
                       (language . ,language)
                       (line_number . ,line-number)
                       (buffer_position . ,buffer-pos)
                       (docstring_preview . ,(or docstring ""))
                       (source_line . ,(string-trim line)))))))))))

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

;;; Kernel-Direct Async Execution System

(defvar org-notebook-mcp--async-requests (make-hash-table :test 'equal)
  "Hash table to track async execution requests.
Keys are request IDs, values are request info alists.")

(defun org-notebook-mcp--stringify (value)
  "Convert VALUE to a string suitable for JSON serialization."
  (cond
   ((null value) nil)
    ((stringp value) value)
    ((symbolp value) (symbol-name value))
    ((numberp value) (format "%s" value))
    ((and (listp value) (cl-every #'stringp value))
     (mapconcat #'identity value ", "))
    ((listp value)
     (mapconcat (lambda (item) (org-notebook-mcp--stringify item)) value ", "))
    ((vectorp value)
     (org-notebook-mcp--stringify (append value nil)))
    (t (format "%s" value))))

(defun org-notebook-mcp--normalize-request-metadata (metadata)
  "Normalize METADATA into an alist of string keys and values."
  (cond
   ((null metadata) nil)
   ((hash-table-p metadata)
    (let (acc)
      (maphash (lambda (key val)
                 (push (cons (org-notebook-mcp--stringify key)
                             (org-notebook-mcp--stringify val))
                       acc))
               metadata)
      (nreverse acc)))
   ((and (listp metadata) (cl-every #'consp metadata))
    (mapcar (lambda (cell)
              (cons (org-notebook-mcp--stringify (car cell))
                    (org-notebook-mcp--stringify (cdr cell))))
            metadata))
   ((vectorp metadata)
    (cl-loop for idx from 0
             for item across metadata
             collect (cons (format "index-%d" idx)
                           (org-notebook-mcp--stringify item))))
   ((stringp metadata)
    `(("info" . ,metadata)))
   (t `(("info" . ,(org-notebook-mcp--stringify metadata))))))

(defun org-notebook-mcp--lookup-request (identifier)
  "Return async request info matching IDENTIFIER (request id or kernel message id)."
  (or (gethash identifier org-notebook-mcp--async-requests)
      (let (found)
        (maphash
         (lambda (_req-id req-info)
           (when (and (not found)
                      (string= identifier (or (cdr (assoc 'kernel_msg_id req-info)) "")))
             (setq found req-info)))
         org-notebook-mcp--async-requests)
        found)))

(defun org-notebook-mcp--collect-request-messages (req)
  "Collect output, result, error, and execution metadata from REQ."
  (let ((output-content "")
        (result-content "")
        (error-content nil)
        (execution-count nil)
        (messages (ignore-errors (jupyter-request-messages req))))
    (when messages
      (dolist (msg messages)
        (pcase (jupyter-message-type msg)
          ("stream"
           (let ((stream (jupyter-message-content msg)))
             (setq output-content
                   (concat output-content
                           (or (plist-get stream :text) "")))))
          ((or "execute_result" "display_data")
           (let* ((msg-content (jupyter-message-content msg))
                  (data (plist-get msg-content :data))
                  (text (plist-get data :text/plain)))
             (when text
               (setq result-content (concat result-content text))))
           (let ((content (plist-get (jupyter-message-content msg) :content)))
             (when (and (null execution-count) (plist-get content :execution_count))
               (setq execution-count (plist-get content :execution_count)))))
          ("execute_reply"
           (let ((content (plist-get (jupyter-message-content msg) :content)))
             (when (and content (plist-get content :execution_count))
               (setq execution-count (plist-get content :execution_count)))))
          ("error"
           (let* ((err (jupyter-message-content msg))
                  (ename (plist-get err :ename))
                  (evalue (plist-get err :evalue))
                  (traceback (plist-get err :traceback)))
             (setq error-content
                   (format "%s: %s\n%s"
                           ename evalue
                           (if traceback (string-join traceback "\n") ""))))))))
    `((output . ,output-content)
      (result . ,result-content)
      (error . ,error-content)
      (messages_seen . ,(if (listp messages) (length messages) 0))
      (execution_count . ,execution-count))))

(defun org-notebook-mcp--public-request-info (req-info)
  "Create a serializable snapshot from REQ-INFO."
  (let* ((status (cdr (assoc 'status req-info)))
         (start (cdr (assoc 'start_time req-info)))
         (completion (cdr (assoc 'completion_time req-info)))
         (metadata (cdr (assoc 'metadata req-info))))
    `((request_id . ,(cdr (assoc 'request_id req-info)))
      (kernel_msg_id . ,(cdr (assoc 'kernel_msg_id req-info)))
      (client_buffer . ,(cdr (assoc 'client_buffer req-info)))
      (status . ,status)
      (start_time . ,(and start (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time start))))
      (completion_time . ,(and completion (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time completion))))
      (age_seconds . ,(when start (- (float-time) start)))
      (metadata . ,metadata))))
(defun org-notebook-mcp--generate-request-id ()
  "Generate a unique request ID for async execution tracking."
  (format "req-%s-%d" (format-time-string "%Y%m%d-%H%M%S") (random 10000)))

(defun org-notebook-mcp--cleanup-completed-requests ()
  "Remove completed requests older than 1 hour to prevent memory leaks."
  (let ((cutoff-time (- (float-time) 3600))) ; 1 hour ago
    (maphash (lambda (req-id req-info)
               (let ((completion-time (cdr (assoc 'completion_time req-info))))
                 (when (and completion-time (< completion-time cutoff-time))
                   (remhash req-id org-notebook-mcp--async-requests))))
             org-notebook-mcp--async-requests)))

(defun org-notebook-mcp--send-kernel-code-async (client_buffer_name code &optional metadata)
  "Send code directly to kernel asynchronously with proper request tracking.
Returns immediately with request ID that can be used to check completion and get results."
  (let* ((buffer (get-buffer client_buffer_name))
         (client nil)
         (request-id (org-notebook-mcp--generate-request-id))
         (normalized-metadata (org-notebook-mcp--normalize-request-metadata metadata)))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (org-notebook-mcp--check-jupyter-dependencies)
    (with-current-buffer buffer
      (unless (jupyter-repl-connected-p)
        (error "REPL client not connected: %s" client_buffer_name))
      (setq client jupyter-current-client))

    (unless client
      (error "No jupyter client found in buffer: %s" client_buffer_name))

    ;; Clean up old completed requests
    (org-notebook-mcp--cleanup-completed-requests)

    ;; Send the request and store it for tracking
    (jupyter-run-with-client client
      (jupyter-mlet* ((req (jupyter-sent (jupyter-execute-request :code code))))
        (let* ((kernel-msg-id (ignore-errors (jupyter-request-id req)))
               (req-info `((request_id . ,request-id)
                         (client_buffer . ,client_buffer_name)
                         (kernel_msg_id . ,kernel-msg-id)
                         (code . ,code)
                         (status . "sent")
                         (request_object . ,req)
                         (client . ,client)
                         (start_time . ,(float-time))
                         (metadata . ,normalized-metadata)
                         (completion_time . nil)
                         (results . nil))))

          ;; Store request in tracking hash table
          (puthash request-id req-info org-notebook-mcp--async-requests)

          ;; Return request info immediately
          (jupyter-return `((request_id . ,request-id)
                           (client_buffer . ,client_buffer_name)
                           (kernel_msg_id . ,kernel-msg-id)
                           (code . ,code)
                           (metadata . ,normalized-metadata)
                           (status . "sent")
                           (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
                           (message . "Code sent to kernel - use check-async-execution to monitor completion")
                           (follow_up . "If this may take minutes, queue a bd reminder linked to this request so you remember to collect the results."))))))

    ;; Return the result from the monadic computation
    `((request_id . ,request-id)
      (client_buffer . ,client_buffer_name)
      (kernel_msg_id . ,(cdr (assoc 'kernel_msg_id (gethash request-id org-notebook-mcp--async-requests))))
      (code . ,code)
      (metadata . ,normalized-metadata)
      (status . "sent")
      (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
      (message . "Code sent to kernel - use check-async-execution to monitor completion")
      (follow_up . "If this may take minutes, queue a bd reminder linked to this request so you remember to collect the results."))))

(defun org-notebook-mcp--check-async-execution (request-identifier)
  "Check the status of an async execution request and return results if complete.
REQUEST-IDENTIFIER can be the MCP request identifier or the kernel-level message id."
  (let ((req-info (org-notebook-mcp--lookup-request request-identifier)))
    (unless req-info
      (error "Async request not found: %s" request-identifier))
    (let* ((request-id (cdr (assoc 'request_id req-info)))
           (kernel-msg-id (cdr (assoc 'kernel_msg_id req-info)))
           (req (cdr (assoc 'request_object req-info)))
           (client (cdr (assoc 'client req-info)))
           (client-buffer (cdr (assoc 'client_buffer req-info)))
           (code (cdr (assoc 'code req-info)))
           (metadata (cdr (assoc 'metadata req-info)))
           (start-time (cdr (assoc 'start_time req-info)))
           (cached-results (cdr (assoc 'results req-info))))
      (when cached-results
        (let ((with-meta (copy-alist cached-results)))
          (setf (alist-get 'metadata with-meta) metadata)
          (setf (alist-get 'kernel_msg_id with-meta) kernel-msg-id)
          (cl-return-from org-notebook-mcp--check-async-execution with-meta)))
      (unless (and client req)
        (error "Request %s no longer has a live client. Re-run the cell if needed." request-id))
      (jupyter-run-with-client client
        (if (jupyter-request-idle-p req)
            (let* ((capture (org-notebook-mcp--collect-request-messages req))
                   (error-content (cdr (assoc 'error capture)))
                   (success (if error-content :json-false t))
                   (completion-time (float-time))
                   (duration (if start-time (- completion-time start-time) 0.0))
                   (results
                    `((request_id . ,request-id)
                      (kernel_msg_id . ,kernel-msg-id)
                      (client_buffer . ,client-buffer)
                      (code . ,code)
                      (status . "completed")
                      (success . ,success)
                      (output . ,(cdr (assoc 'output capture)))
                      (result . ,(cdr (assoc 'result capture)))
                      (error . ,error-content)
                      (execution_count . ,(cdr (assoc 'execution_count capture)))
                      (messages_seen . ,(cdr (assoc 'messages_seen capture)))
                      (metadata . ,metadata)
                      (start_time . ,(and start-time (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time start-time))))
                      (completion_time . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
                      (execution_duration . ,(format "%.2f seconds" (max duration 0.0)))
                      (recommendations . ,(if success
                                              "Archive outputs or summarize in the org notebook before moving on."
                                            "Inspect the error, fix the cell, and rerun. Consider creating a bd follow-up if this blocks other work.")))))
              (let ((updated (copy-alist req-info)))
                (setf (alist-get 'results updated) results)
                (setf (alist-get 'completion_time updated) completion-time)
                (setf (alist-get 'status updated) "completed")
                (puthash request-id updated org-notebook-mcp--async-requests))
              (jupyter-return results))
          (let* ((capture (org-notebook-mcp--collect-request-messages req))
                 (elapsed (if start-time (- (float-time) start-time) 0.0)))
            (jupyter-return
             `((request_id . ,request-id)
               (kernel_msg_id . ,kernel-msg-id)
               (client_buffer . ,client-buffer)
               (code . ,code)
               (status . "pending")
               (output_preview . ,(cdr (assoc 'output capture)))
               (result_preview . ,(cdr (assoc 'result capture)))
               (messages_seen . ,(cdr (assoc 'messages_seen capture)))
               (metadata . ,metadata)
               (start_time . ,(and start-time (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time start-time))))
               (elapsed_time . ,(format "%.2f seconds" (max elapsed 0.0)))
               (recommendations . "Kernel still busy; poll again later or create a bd reminder if you need to follow up after stepping away.")))))))))

(defun org-notebook-mcp--list-async-requests ()
  "Return a summary of all tracked async execution requests."
  (let (snapshots)
    (maphash
     (lambda (_ req-info)
       (push (org-notebook-mcp--public-request-info req-info) snapshots))
     org-notebook-mcp--async-requests)
    (let* ((entries (nreverse snapshots))
           (pending (cl-count-if (lambda (info)
                                   (not (string= (cdr (assoc 'status info)) "completed")))
                                 entries)))
      `((total_requests . ,(length entries))
        (pending . ,pending)
        (completed . ,(- (length entries) pending))
        (requests . ,entries)))))

(defun org-notebook-mcp--get-kernel-state-direct (client_buffer_name)
  "Get kernel state directly without relying on buffer parsing.
Uses jupyter.el's internal request tracking to determine if kernel is busy."
  (let* ((buffer (get-buffer client_buffer_name)))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    (org-notebook-mcp--check-jupyter-dependencies)
    (with-current-buffer buffer
      (let* ((client jupyter-current-client)
             (connected-p (jupyter-repl-connected-p))
             (pending-requests 0)
             (has-pending-async 0))

        ;; Count requests in our async tracking system for this client
        (maphash (lambda (req-id req-info)
                   (when (and (string= client_buffer_name (cdr (assoc 'client_buffer req-info)))
                             (not (cdr (assoc 'results req-info))))
                     (setq has-pending-async (1+ has-pending-async))))
                 org-notebook-mcp--async-requests)

        ;; Count pending requests in client's request queue if available
        (when (and client (slot-exists-p client 'requests))
          (setq pending-requests (length (oref client requests))))

        (let ((kernel-state (cond
                            ((not connected-p) "disconnected")
                            ((> pending-requests 0) "busy")
                            ((> has-pending-async 0) "processing_async")
                            (t "idle"))))

          `((buffer_name . ,client_buffer_name)
            (connected . ,(if connected-p t :json-false))
            (kernel_state . ,kernel-state)
            (busy . ,(if (member kernel-state '("busy" "processing_async")) t :json-false))
            (pending_requests . ,pending-requests)
            (pending_async_requests . ,has-pending-async)
            (execution_count . ,(when connected-p (jupyter-repl-cell-count)))
            (last_check . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
            (recommendations . ,(cond
                                ((not connected-p) "Kernel is disconnected - reconnect before sending code")
                                ((string= kernel-state "busy") "Kernel is busy with synchronous execution")
                                ((string= kernel-state "processing_async") "Kernel is processing async requests - safe to send more")
                                (t "Kernel is ready for new code")))))))))

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
                    `((client_id . ,(format "client-%s" (buffer-name)))
                      (buffer_name . ,(buffer-name))
                      (kernel_name . ,(or (plist-get kernel-info :implementation) "unknown"))
                      (language . ,(or (plist-get lang-info :name) "unknown"))
                      (language_version . ,(or (plist-get lang-info :version) "unknown"))
                      (status . ,(if (jupyter-repl-connected-p) "connected" "disconnected"))
                      (execution_count . ,(or (jupyter-repl-cell-count) 0)))))
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
    (let ((execution-error nil)
          (output-content "")
          (result-content "")
          (error-content "")
          (req nil))

      (jupyter-run-with-client client
        (jupyter-mlet* ((completed-req (jupyter-idle (jupyter-execute-request :code code) timeout)))
          (setq req completed-req)

          ;; Extract results from completed request
          (let ((messages (oref req messages)))
            (dolist (msg messages)
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
                                 (if traceback (string-join traceback "\n") "")))))))

          ;; Return the result alist from within the monadic context
          (jupyter-return `((client_buffer . ,client_buffer_name)
                           (code . ,code)
                           (success . ,(not execution-error))
                           (output . ,output-content)
                           (result . ,result-content)
                           (error . ,(if execution-error error-content nil))
                           (execution_time . ,(format-time-string "%Y-%m-%d %H:%M:%S"))))))))))

(defun org-notebook-mcp--get-jupyter-kernel-state (client_buffer_name)
  "DEPRECATED: Use org-notebook-mcp--get-kernel-state-direct instead.
This function is kept for backward compatibility."
  (org-notebook-mcp--get-kernel-state-direct client_buffer_name))

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
          (kernel_id . ,(when client
                           (condition-case nil
                               (jupyter-session-id (oref client session))
                             (error
                               (condition-case nil
                                   (oref client kernel-id)
                                 (error "unknown"))))))
          (language . ,(when lang-info (plist-get lang-info :name)))
          (language_version . ,(when lang-info (plist-get lang-info :version)))
          (execution_count . ,(when connected-p (jupyter-repl-cell-count)))
          (buffer_size . ,(buffer-size))
          (last_activity . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
          (kernel_alive . ,(when client (jupyter-kernel-alive-p client))))))))

(defun org-notebook-mcp--send-code-async-legacy (client_buffer_name code &optional metadata)
  "DEPRECATED: Use org-notebook-mcp--send-kernel-code-async instead.
This function is kept for backward compatibility."
  (org-notebook-mcp--send-kernel-code-async client_buffer_name code metadata))

(defun org-notebook-mcp--get-last-output (client_buffer_name &optional lines)
  "Get the last output from a Jupyter REPL buffer.
Useful for retrieving results after async execution completes.
LINES specifies how many lines of output to retrieve (default 50)."
  (let* ((buffer (get-buffer client_buffer_name))
         (lines (or lines 50)))

    (unless buffer
      (error "REPL buffer not found: %s" client_buffer_name))

    ;; For now, return a safe response without reading buffer content
    ;; This prevents server crashes while we debug the buffer access issue
    `((buffer_name . ,client_buffer_name)
      (lines_retrieved . 0)
      (max_lines . ,lines)
      (output . "Buffer reading temporarily disabled to prevent server crashes")
      (buffer_size . ,(with-current-buffer buffer (buffer-size)))
      (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
      (note . "This function needs further debugging for safe buffer access"))))

;;; MCP Tool Functions

;; New kernel-direct async execution tools
(defun org-notebook-mcp--send-kernel-code-async-mcp (client_buffer_name code)
  "Send code directly to kernel asynchronously with proper request tracking.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to send code to (string, required)
  code - Code to execute in the kernel (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))
    (unless (stringp code)
      (signal 'wrong-type-argument (list 'stringp code)))
    (when (string-empty-p code)
      (error "Empty code provided"))

    (let ((result (org-notebook-mcp--send-kernel-code-async client_buffer_name code)))
      (json-encode result))))

(defun org-notebook-mcp--check-async-execution-mcp (request_id)
  "Check the status of an async execution request and return results if complete.
MCP Parameters:
  request_id - The request ID returned from send-kernel-code-async (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp request_id)
      (signal 'wrong-type-argument (list 'stringp request_id)))
    (when (string-empty-p request_id)
      (error "Empty request_id provided"))

    (let ((result (org-notebook-mcp--check-async-execution request_id)))
      (json-encode result))))

(defun org-notebook-mcp--get-kernel-state-direct-mcp (client_buffer_name)
  "Get kernel state directly without relying on buffer parsing.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))

    (let ((result (org-notebook-mcp--get-kernel-state-direct client_buffer_name)))
      (json-encode result))))

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
      (json-encode result))))

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
      (json-encode result))))

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
      (json-encode result))))

(defun org-notebook-mcp--get-kernel-state (client_buffer_name)
  "Get the execution state of a Jupyter kernel to check if it's busy.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))

    (let ((result (org-notebook-mcp--get-kernel-state-direct client_buffer_name)))
      (json-encode result))))

(defun org-notebook-mcp--send-code-async (client_buffer_name code &optional metadata)
  "Send code to Jupyter kernel asynchronously for long-running computations.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to send code to (string, required)
  code - Code to execute in the kernel (string, required)
  metadata - Optional context map for tracking (alist/hash/object)."
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))
    (when (string-empty-p client_buffer_name)
      (error "Empty client_buffer_name provided"))
    (unless (stringp code)
      (signal 'wrong-type-argument (list 'stringp code)))
    (when (string-empty-p code)
      (error "Empty code provided"))

    (let ((result (org-notebook-mcp--send-kernel-code-async client_buffer_name code metadata)))
      (json-encode result))))

(defun org-notebook-mcp--list-async-requests-mcp ()
  "List tracked async execution requests and their status."
  (mcp-server-lib-with-error-handling
    (json-encode (org-notebook-mcp--list-async-requests))))

(defun org-notebook-mcp--get-last-output (client_buffer_name &optional lines)
  "DEPRECATED: This function was replaced due to buffer reading crashes.
Use check-async-execution with request IDs from send-code-async instead.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to read from (string, required)
  lines - Number of lines to retrieve (integer, optional, default 50)"
  (mcp-server-lib-with-error-handling
    (unless (stringp client_buffer_name)
      (signal 'wrong-type-argument (list 'stringp client_buffer_name)))

    `((buffer_name . ,client_buffer_name)
      (status . "deprecated")
      (message . "This function is deprecated due to buffer reading crashes. Use the new async execution system instead.")
      (alternative . "Use send-code-async to get a request ID, then check-async-execution to get results")
      (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S")))))

(provide 'org-notebook-tools)
;;; org-notebook-tools.el ends here
