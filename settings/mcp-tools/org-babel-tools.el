;;; org-babel-tools.el
;;; Code:

(require 'org)
(require 'ob)

;;; Utility Functions

(defun org-babel-mcp--validate-file-path (file-path)
  "Validate that FILE-PATH is a non-empty string pointing to an existing org file.
Throws an error if validation fails."
  (unless (stringp file-path)
    (error "Invalid file path: must be a string"))
  (when (string-empty-p file-path)
    (error "Empty file path"))
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))
  (unless (string-match-p "\\.org$" file-path)
    (error "File must have .org extension")))

(defun org-babel-mcp--get-org-buffer (file-path)
  "Get or create buffer for FILE-PATH.
Returns the buffer object."
  (let ((buffer (find-file-noselect file-path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buffer))

(defun org-babel-mcp--format-success (message &optional details)
  "Format a success response as string.
MESSAGE is the success message.
DETAILS is an optional alist of additional details."
  (if details
      (format "%s\n\nDetails:\n%s" message
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    message))

(defun org-babel-mcp--format-error (message &optional details)
  "Format an error response as string.
MESSAGE is the error message.
DETAILS is an optional alist of additional details."
  (if details
      (format "Error: %s\n\nDetails:\n%s" message
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    (format "Error: %s" message)))

;;; MCP Tool Functions

(defun org-babel-mcp-execute-src-block (file-path &optional block-name)
  "Execute a specific source block in an org file.
FILE-PATH is the path to the org file.
BLOCK-NAME is the optional name of the block to execute. If not provided,
executes the block at point or the first block found."
  (org-babel-mcp--validate-file-path file-path)
  (let ((buffer (org-babel-mcp--get-org-buffer file-path))
        (block-info nil))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if block-name
            ;; Find specific named block
            (progn
              (unless (re-search-forward
                       (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$"
                               (regexp-quote block-name)) nil t)
                (error "Block with name '%s' not found" block-name))
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

        ;; Execute the block
        (condition-case err
            (progn
              (org-babel-execute-src-block)
              (save-buffer)

              ;; Get the actual result from the buffer
              (let ((actual-result ""))
                (save-excursion
                  (when (re-search-forward "^[ \t]*#\\+RESULTS:" nil t)
                    (forward-line 1)
                    (let ((result-start (point)))
                      (while (and (not (eobp))
                                  (or (looking-at "^[ \t]*:")
                                      (looking-at "^[ \t]*|")
                                      (looking-at "^[ \t]*$")))
                        (forward-line 1))
                      (setq actual-result (string-trim (buffer-substring-no-properties result-start (point)))))))

                (org-babel-mcp--format-success
                 (format "Executed source block%s"
                         (if block-name (format " '%s'" block-name) ""))
                 `(("file_path" . ,file-path)
                   ("block_name" . ,(or block-name "(unnamed)"))
                   ("language" . ,(nth 0 block-info))
                   ("line_number" . ,(line-number-at-pos))
                   ("result" . ,(or actual-result ""))
                   ("file_modified" . "true")))))
          (error
           (error "Error executing block: %s" (error-message-string err)))))))

(defun org-babel-mcp-execute-buffer (file-path)
  "Execute all source blocks in an org file buffer.
FILE-PATH is the path to the org file."
  (org-babel-mcp--validate-file-path file-path)
  (let ((buffer (org-babel-mcp--get-org-buffer file-path))
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
                   (success nil))
              (condition-case err
                  (progn
                    (org-babel-execute-src-block)
                    (setq success t)
                    (setq executed-count (1+ executed-count)))
                (error
                 (setq success nil)
                 (setq failed-count (1+ failed-count))))
              ;; Get actual result from buffer
              (let ((actual-result ""))
                (save-excursion
                  (when (re-search-forward "^[ \t]*#\\+RESULTS:" nil t)
                    (forward-line 1)
                    (let ((result-start (point)))
                      (while (and (not (eobp))
                                  (or (looking-at "^[ \t]*:")
                                      (looking-at "^[ \t]*|")
                                      (looking-at "^[ \t]*$")))
                        (forward-line 1))
                      (setq actual-result (string-trim (buffer-substring-no-properties result-start (point)))))))
                (push `((line . ,line-num)
                        (language . ,(or language "unknown"))
                        (success . ,(if success "true" "false"))
                        (result . ,(or actual-result "")))
                      execution-details)))
            ;; Move past the current block
            (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
              (forward-line 1)))
          ;; Save the buffer to persist execution results
          (when (> executed-count 0)
            (save-buffer))
          (let ((details-str (mapconcat
                             (lambda (detail)
                               (format "Line %d (%s): %s"
                                       (cdr (assoc 'line detail))
                                       (cdr (assoc 'language detail))
                                       (if (string= (cdr (assoc 'success detail)) "true")
                                           "Success"
                                         "Failed")))
                             (reverse execution-details) "\n")))
            (if (> failed-count 0)
                (error "Executed buffer with %d successes and %d failures\n\nDetails:\n%s"
                         executed-count failed-count details-str)
              (org-babel-mcp--format-success
               (format "Successfully executed all %d source blocks" executed-count)
               `(("File Path" . ,file-path)
                 ("Executed Blocks" . ,(number-to-string executed-count))
                 ("Total Blocks" . ,(number-to-string executed-count))
                 ("Execution Details" . ,details-str)
                 ("File Saved" . "true")))))))))))


(provide 'org-babel-tools)
;;; org-babel-tools.el ends here
