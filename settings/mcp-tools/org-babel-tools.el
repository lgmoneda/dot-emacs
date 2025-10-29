;;; org-babel-tools.el --- MCP tools for org-babel
;;; Code:

(require 'org)
(require 'ob)

;;; Utility Functions

(defconst org-babel-mcp--default-block-timeout 120
  "Maximum seconds to wait for async replacement when blocking.")

(defconst org-babel-mcp--default-poll-interval 0.75
  "Seconds between polls while waiting for async results.")

(defun org-babel-mcp--get-org-buffer (file-path)
  "Get or create buffer for FILE-PATH.
Returns the buffer object."
  (let ((buffer (find-file-noselect file-path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buffer))

(defun org-babel-mcp--get-block-result (buffer)
  "Get the result from the current position in BUFFER.
Assumes we're at a source block."
  (with-current-buffer buffer
    (save-excursion
      (when (re-search-forward "^[ \t]*#\\+RESULTS:" nil t)
        (forward-line 1)
        (let ((result-start (point)))
          (while (and (not (eobp))
                      (or (looking-at "^[ \t]*:")
                          (looking-at "^[ \t]*|")
                          (looking-at "^[ \t]*$")))
            (forward-line 1))
          (org-babel-mcp--normalize-result-string
           (buffer-substring-no-properties result-start (point))))))))

(defun org-babel-mcp--normalize-result-string (result)
  "Normalize RESULT text by removing Org result prefixes like ': '.
Preserves multi-line outputs while stripping leading colon markers."
  (let* ((text (string-trim-right (or result "")))
         (lines (split-string text "\n")))
    (string-join
     (mapcar (lambda (line)
               (let* ((trimmed (string-trim-right line))
                      (stripped (if (string-match "\\`:[ \t]*" trimmed)
                                    (substring trimmed (match-end 0))
                                  trimmed)))
                 (string-trim-right stripped)))
             lines)
     "\n")))

(defun org-babel-mcp--is-async-result-p (result)
  "Check if RESULT appears to be an async execution ID.
Returns t if the result looks like a Jupyter request ID or similar async identifier."
  (and (stringp result)
       (not (string-empty-p result))
       (or
        ;; Jupyter/EIN request IDs (common patterns)
        (string-match-p "^[a-f0-9-]\\{8,\\}$" result)
        (string-match-p "^request_[a-zA-Z0-9_-]+$" result)
        (string-match-p "^exec_[0-9]+$" result)
        ;; Other async patterns
        (string-match-p "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$" result)
        (string-match-p "^async_[a-zA-Z0-9_-]+$" result))))

(defun org-babel-mcp--wait-for-async-result (buffer initial-result max-wait-seconds &optional poll-interval)
  "Wait for async result to be replaced with actual output.
BUFFER is the org buffer, INITIAL-RESULT is the async ID, MAX-WAIT-SECONDS is timeout.
Returns plist with result details."
  (let ((start-time (current-time))
        (poll-interval (or poll-interval org-babel-mcp--default-poll-interval))
        (current-result initial-result)
        (iteration 0))

    (while (and (> max-wait-seconds 0)
                (< (float-time (time-subtract (current-time) start-time)) max-wait-seconds)
                (org-babel-mcp--is-async-result-p current-result))
      (sit-for poll-interval)
      (setq iteration (1+ iteration))
      (setq current-result (or (org-babel-mcp--get-block-result buffer) "")))

    (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
           (timed-out (and (> max-wait-seconds 0)
                           (>= elapsed max-wait-seconds)
                           (org-babel-mcp--is-async-result-p current-result)))
           (still-async (org-babel-mcp--is-async-result-p current-result)))

      `(:result ,current-result
        :initial-result ,initial-result
        :elapsed-time ,elapsed
        :timed-out ,timed-out
        :still-pending ,still-async
        :iterations ,iteration
        :retrieval-instructions
        ,(when still-async
           (format "Result still pending (ID: %s). To retrieve when ready, use get-org-heading-content or check-async-execution with request ID %s"
                   current-result current-result))))))

(defun org-babel-mcp--get-block-result-with-async-polling (buffer &optional max-wait-seconds poll-interval)
  "Get block result with optional async polling for better AI agent feedback.
BUFFER is the org buffer. MAX-WAIT-SECONDS defaults to 10.
Returns plist with comprehensive result information."
  (let ((initial-result (or (org-babel-mcp--get-block-result buffer) ""))
        (max-wait (or max-wait-seconds 10))
        (poll-interval (or poll-interval org-babel-mcp--default-poll-interval)))

    (if (org-babel-mcp--is-async-result-p initial-result)
        ;; Async result detected - poll for completion
        (let ((async-info (org-babel-mcp--wait-for-async-result buffer initial-result max-wait poll-interval)))
          `(:result ,(plist-get async-info :result)
            :initial-result ,(plist-get async-info :initial-result)
            :async-detected t
            :elapsed-time ,(plist-get async-info :elapsed-time)
            :timed-out ,(plist-get async-info :timed-out)
            :still-pending ,(plist-get async-info :still-pending)
            :iterations ,(plist-get async-info :iterations)
            :retrieval-instructions ,(plist-get async-info :retrieval-instructions)))
      ;; Immediate/sync result
      `(:result ,initial-result
        :initial-result ,initial-result
        :async-detected nil
        :elapsed-time 0
        :timed-out nil
        :still-pending nil))))

;;; MCP Tool Functions

(defun org-babel-mcp--execute-src-block (file_path &optional heading_title line_number wait-mode)
  "Execute a specific source block in an org file.
MCP Parameters:
  file_path - Path to the org file
  heading_title - Optional heading title (e.g., 'EDA') - finds first src block under that heading
  line_number - Optional line number - finds first src block at or after this line
  If both are provided, heading_title takes precedence
  If neither is provided, executes the first source block found
  WAIT-MODE controls async handling: nil or 'no-wait for immediate return,
  'block to wait until completion (up to timeout), or 'wait-or-id to wait a short
  period before returning the request id."
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (when (string-empty-p file_path)
      (error "Empty file path"))
    (unless (file-exists-p file_path)
      (error "File does not exist: %s" file_path))
    (unless (string-match-p "\\.org$" file_path)
      (error "File must have .org extension"))
    
    (let ((buffer (org-babel-mcp--get-org-buffer file_path))
          (block-info nil)
          (line-number nil)
          (result nil)
          (wait-mode (or wait-mode 'no-wait)))

      (unless (memq wait-mode '(no-wait block wait-or-id))
        (error "Unsupported wait_mode: %s" wait-mode))

      (let* ((wait-config (pcase wait-mode
                            ('no-wait
                             (list :max 0 :poll org-babel-mcp--default-poll-interval))
                            ('block
                             (list :max org-babel-mcp--default-block-timeout
                                   :poll org-babel-mcp--default-poll-interval))
                            ('wait-or-id
                             (list :max 3 :poll org-babel-mcp--default-poll-interval))))
             (max-wait (plist-get wait-config :max))
             (poll-interval (plist-get wait-config :poll)))

      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          
          (cond
           ;; Find by heading title (takes precedence)
           ((and heading_title (stringp heading_title) (not (string-empty-p heading_title)))
            (unless (re-search-forward
                     (format "^\\*+ .*%s.*$" (regexp-quote heading_title)) nil t)
              (error "Heading containing '%s' not found" heading_title))
            ;; Find first source block under this heading
            (let ((heading-level (org-current-level))
                  (found-block nil))
              (while (and (not found-block)
                         (re-search-forward "^[ \\t]*#\\+begin_src" nil t))
                (let ((current-pos (point)))
                  (save-excursion
                    (beginning-of-line)
                    (when (or (bobp)
                             (let ((prev-heading-level (save-excursion
                                                        (when (re-search-backward "^\\*+ " nil t)
                                                          (org-current-level)))))
                               (or (not prev-heading-level)
                                   (>= prev-heading-level heading-level))))
                      (setq found-block t)))))
              (unless found-block
                (error "No source block found under heading containing '%s'" heading_title))
              (beginning-of-line)))
           
           ;; Find by line number
           ((and line_number (numberp line_number))
            (goto-line line_number)
            (unless (re-search-forward "^[ \\t]*#\\+begin_src" nil t)
              (error "No source block found at or after line %d" line_number))
            (beginning-of-line))
           
           ;; Find first source block (default)
           (t
            (unless (re-search-forward "^[ \\t]*#\\+begin_src" nil t)
              (error "No source blocks found in file"))
            (beginning-of-line)))
          
          ;; Get block info
          (setq block-info (org-babel-get-src-block-info))
          (unless block-info
            (error "Could not get source block information"))
          
          (setq line-number (line-number-at-pos))
          
          ;; Execute the block
          (org-babel-execute-src-block)
          
          ;; Get the result with async polling
          (setq result (org-babel-mcp--get-block-result-with-async-polling buffer max-wait poll-interval))
          
          ;; Save the buffer
          (save-buffer)))

      ;; Return alist format with enhanced async info
      `((file_path . ,file_path)
        (block_name . ,(cond ((and heading_title (stringp heading_title) (not (string-empty-p heading_title)))
                              (format "heading-%s" heading_title))
                             ((and line_number (numberp line_number))
                              (format "line-%d" line_number))
                             (t "(unnamed)")))
        (language . ,(nth 0 block-info))
        (line_number . ,line-number)
        (result . ,(plist-get result :result))
        (async_detected . ,(plist-get result :async-detected))
        (wait_mode . ,(symbol-name wait-mode))
        (execution_time . ,(plist-get result :elapsed-time))
        (timed_out . ,(plist-get result :timed-out))
        (still_pending . ,(plist-get result :still-pending))
        (pending_request_id . ,(when (plist-get result :still-pending)
                                 (plist-get result :initial-result)))
        (retrieval_instructions . ,(plist-get result :retrieval-instructions))
        (file_modified . t))))))

(defun org-babel-mcp--execute-subtree (file_path heading_title)
  "Execute all source blocks within a specific org subtree.
MCP Parameters:
  file_path - Path to the org file
  heading_title - Title of the heading whose subtree should be executed"
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (when (string-empty-p file_path)
      (error "Empty file path"))
    (unless (file-exists-p file_path)
      (error "File does not exist: %s" file_path))
    (unless (string-match-p "\\.org$" file_path)
      (error "File must have .org extension"))
    (unless (stringp heading_title)
      (signal 'wrong-type-argument (list 'stringp heading_title)))
    (when (string-empty-p heading_title)
      (error "Empty heading title"))
    
    (let ((buffer (org-babel-mcp--get-org-buffer file_path))
          (executed-count 0)
          (failed-count 0)
          (execution-details '())
          (subtree-start nil)
          (subtree-end nil))
      
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            ;; Find the target heading
            (goto-char (point-min))
            (unless (re-search-forward
                     (format "^\\*+ .*%s.*$" (regexp-quote heading_title)) nil t)
              (error "Heading containing '%s' not found" heading_title))
            
            ;; Store heading position for reference
            (setq subtree-start (line-number-at-pos))
            
            ;; Narrow to this subtree
            (org-narrow-to-subtree)
            (setq subtree-end (line-number-at-pos (point-max)))
            
            ;; Count total blocks within the narrowed subtree
            (goto-char (point-min))
            (let ((total-blocks 0))
              (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
                (setq total-blocks (1+ total-blocks)))
              (when (= total-blocks 0)
                (error "No source blocks found in subtree '%s'" heading_title))
              
              ;; Execute all blocks within the narrowed subtree
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
                (beginning-of-line)
                (let* ((block-info (org-babel-get-src-block-info))
                       (language (when block-info (nth 0 block-info)))
                       (line-num (line-number-at-pos))
                       (success nil)
                       (error-msg nil)
                       (result nil))
                  
                  (condition-case err
                      (progn
                        (org-babel-execute-src-block)
                        (setq success t)
                        (setq executed-count (1+ executed-count))
                        (let ((result-info (org-babel-mcp--get-block-result-with-async-polling buffer 10)))
                          (setq result (plist-get result-info :result))
                          (when (plist-get result-info :still-pending)
                            (setq error-msg (plist-get result-info :retrieval-instructions)))))
                    (error
                     (setq success nil)
                     (setq failed-count (1+ failed-count))
                     (setq error-msg (error-message-string err))))
                  
                  (push `((line . ,line-num)
                          (language . ,(or language "unknown"))
                          (success . ,success)
                          (result . ,(or result ""))
                          (error . ,error-msg))
                        execution-details))
                
                ;; Move past the current block
                (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                  (forward-line 1))))
            
            ;; Widen back to full document
            (widen)))
        
        ;; Save the buffer to persist execution results
        (when (> executed-count 0)
          (save-buffer)))
      
      ;; Return alist format
      `((file_path . ,file_path)
        (heading_title . ,heading_title)
        (subtree_start_line . ,subtree-start)
        (subtree_end_line . ,subtree-end)
        (executed_blocks . ,executed-count)
        (failed_blocks . ,failed-count)
        (total_blocks . ,(+ executed-count failed-count))
        (execution_details . ,(nreverse execution-details))
        (file_saved . t)))))

(defun org-babel-mcp--execute-buffer (file_path)
  "Execute all source blocks in an org file buffer.
MCP Parameters:
  file_path - Path to the org file"
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (when (string-empty-p file_path)
      (error "Empty file path"))
    (unless (file-exists-p file_path)
      (error "File does not exist: %s" file_path))
    (unless (string-match-p "\\.org$" file_path)
      (error "File must have .org extension"))
    
    (let ((buffer (org-babel-mcp--get-org-buffer file_path))
          (executed-count 0)
          (failed-count 0)
          (execution-details '()))
      
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          
          ;; Count total blocks first
          (let ((total-blocks 0))
            (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
              (setq total-blocks (1+ total-blocks)))
            (when (= total-blocks 0)
              (error "No source blocks found in file"))
            
            ;; Execute all blocks
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
              (beginning-of-line)
              (let* ((block-info (org-babel-get-src-block-info))
                     (language (when block-info (nth 0 block-info)))
                     (line-num (line-number-at-pos))
                     (success nil)
                     (error-msg nil)
                     (result nil))
                
                (condition-case err
                    (progn
                      (org-babel-execute-src-block)
                      (setq success t)
                      (setq executed-count (1+ executed-count))
                      (let ((result-info (org-babel-mcp--get-block-result-with-async-polling buffer 10)))
                        (setq result (plist-get result-info :result))
                        (when (plist-get result-info :still-pending)
                          (setq error-msg (plist-get result-info :retrieval-instructions)))))
                  (error
                   (setq success nil)
                   (setq failed-count (1+ failed-count))
                   (setq error-msg (error-message-string err))))
                
                (push `((line . ,line-num)
                        (language . ,(or language "unknown"))
                        (success . ,success)
                        (result . ,(or result ""))
                        (error . ,error-msg))
                      execution-details))
              
              ;; Move past the current block
              (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                (forward-line 1))))
          
          ;; Save the buffer to persist execution results
          (when (> executed-count 0)
            (save-buffer))))
      
      ;; Check if we had any failures
      (when (> failed-count 0)
        (error "Executed buffer with %d successes and %d failures"
               executed-count failed-count))
      
      ;; Return alist format
      `((file_path . ,file_path)
        (executed_blocks . ,executed-count)
        (failed_blocks . ,failed-count)
        (total_blocks . ,(+ executed-count failed-count))
        (execution_details . ,(nreverse execution-details))
        (file_saved . t)))))

(provide 'org-babel-tools)
;;; org-babel-tools.el ends here
