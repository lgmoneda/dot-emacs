;;; org-babel-tools.el --- MCP tools for org-babel
;;; Code:

(require 'org)
(require 'ob)

;;; Utility Functions

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
          (string-trim (buffer-substring-no-properties result-start (point))))))))

;;; MCP Tool Functions

(defun org-babel-mcp--execute-src-block (file_path &optional heading_title line_number)
  "Execute a specific source block in an org file.
MCP Parameters:
  file_path - Path to the org file
  heading_title - Optional heading title (e.g., 'EDA') - finds first src block under that heading
  line_number - Optional line number - finds first src block at or after this line
  If both are provided, heading_title takes precedence
  If neither is provided, executes the first source block found"
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
          (result nil))
      
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
          
          ;; Get the result
          (setq result (or (org-babel-mcp--get-block-result buffer) ""))
          
          ;; Save the buffer
          (save-buffer)))
      
      ;; Return alist format
      `((file_path . ,file_path)
        (block_name . ,(cond ((and heading_title (stringp heading_title) (not (string-empty-p heading_title)))
                              (format "heading-%s" heading_title))
                             ((and line_number (numberp line_number))
                              (format "line-%d" line_number))
                             (t "(unnamed)")))
        (language . ,(nth 0 block-info))
        (line_number . ,line-number)
        (result . ,result)
        (file_modified . t)))))

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
                        (setq result (or (org-babel-mcp--get-block-result buffer) "")))
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
                      (setq result (or (org-babel-mcp--get-block-result buffer) "")))
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
