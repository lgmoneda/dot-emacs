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

(defun org-babel-mcp--execute-src-block (file_path &optional block_name)
  "Execute a specific source block in an org file.
MCP Parameters:
  file_path - Path to the org file
  block_name - Optional name of the block to execute"
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
          
          (if block_name
              ;; Find specific named block
              (progn
                (unless (re-search-forward
                         (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$"
                                 (regexp-quote block_name)) nil t)
                  (error "Block with name '%s' not found" block_name))
                ;; Move to the actual source block
                (unless (re-search-forward "^[ \t]*#\\+begin_src" nil t)
                  (error "No source block found after name declaration"))
                (beginning-of-line))
            ;; Find first source block
            (unless (re-search-forward "^[ \t]*#\\+begin_src" nil t)
              (error "No source blocks found in file"))
            (beginning-of-line))
          
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
        (block_name . ,(or block_name "(unnamed)"))
        (language . ,(nth 0 block-info))
        (line_number . ,line-number)
        (result . ,result)
        (file_modified . t)))))

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
