;;; org-tools.el --- MCP tools for org
;;; Code:

(require 'org)
(require 'org-element)

;;; Utility Functions

(defun org-mcp--count-subheadings (element level)
  "Count direct subheadings of ELEMENT at LEVEL."
  (let ((count 0)
        (contents (org-element-contents element)))
    (dolist (child contents)
      (when (and (eq (org-element-type child) 'headline)
                 (= (org-element-property :level child) (1+ level)))
        (setq count (1+ count))))
    count))

(defun org-mcp--get-heading-content-stats (element)
  "Get content statistics for a heading ELEMENT.
Returns a list: (content-length has-code-blocks-p)"
  (let ((content-length 0)
        (has-code-blocks nil))
    (org-element-map element '(paragraph src-block example-block)
      (lambda (elem)
        (pcase (org-element-type elem)
          ('paragraph
           (setq content-length 
                 (+ content-length 
                    (length (org-element-interpret-data elem)))))
          ((or 'src-block 'example-block)
           (setq has-code-blocks t)
           (setq content-length 
                 (+ content-length 
                    (length (org-element-interpret-data elem))))))))
    (list content-length has-code-blocks)))

(defun org-mcp--build-parent-path (element)
  "Build a breadcrumb path to ELEMENT."
  (let ((path '())
        (current element))
    (while (setq current (org-element-property :parent current))
      (when (eq (org-element-type current) 'headline)
        (let ((title (org-element-property :title current)))
          (push (if (listp title)
                    (org-element-interpret-data title)
                    (or title ""))
                path))))
    (mapconcat 'identity path " > ")))

;;; MCP Tool Functions

(defun org-mcp--get-org-file-headings (file_path &optional level parent_heading include_content_stats)
  "Get org headings from a file with navigation metadata.
MCP Parameters:
  file_path - Absolute path to the org file
  level - Optional level to filter (1, 2, 3, etc.)
  parent_heading - Optional parent heading to filter under
  include_content_stats - Whether to include content statistics (default t)"
  (mcp-server-lib-with-error-handling
    (unless (stringp file_path)
      (signal 'wrong-type-argument (list 'stringp file_path)))
    (unless (file-exists-p file_path)
      (error "File does not exist: %s" file_path))
    (unless (string-match-p "\\.org$" file_path)
      (error "File is not an org file: %s" file_path))

    (setq include_content_stats (if (null include_content_stats) t include_content_stats))
    
    (let ((headings '())
          (total-count 0))
      (with-temp-buffer
        (insert-file-contents file_path)
        (org-mode)
        (let* ((ast (condition-case err
                         (org-element-parse-buffer)
                       (error
                        (error "Failed to parse org file: %s" (error-message-string err)))))
               (all-headings (org-element-map ast 'headline 'identity)))
          
          (dolist (heading all-headings)
            (condition-case err
              (let* ((heading-level (org-element-property :level heading))
                     (heading-title (org-element-property :title heading))
                     (line-number (condition-case nil
                                    (line-number-at-pos 
                                     (org-element-property :begin heading))
                                    (error 1)))
                     (parent-path (condition-case nil
                                    (org-mcp--build-parent-path heading)
                                    (error "")))
                     (subheading-count (condition-case nil
                                         (org-mcp--count-subheadings heading heading-level)
                                         (error 0)))
                     (content-stats (when include_content_stats
                                     (condition-case nil
                                       (org-mcp--get-heading-content-stats heading)
                                       (error (list 0 nil)))))
                     (content-length (when content-stats (nth 0 content-stats)))
                     (has-code-blocks (when content-stats (nth 1 content-stats))))
              
              ;; Apply filters
              (when (and (or (null level) (= heading-level level))
                        (or (null parent_heading) 
                            (string-match-p (regexp-quote parent_heading) parent-path)))
                (let ((heading-data `((title . ,(if (listp heading-title)
                                                   (org-element-interpret-data heading-title)
                                                   (or heading-title "")))
                                     (level . ,heading-level)
                                     (line_number . ,line-number)
                                     (parent_path . ,(or parent-path ""))
                                     (subheading_count . ,subheading-count))))
                  (when include_content_stats
                    (push `(content_length . ,(or content-length 0)) heading-data)
                    (push `(has_code_blocks . ,(or has-code-blocks :json-false)) heading-data))
                  (push heading-data headings)
                  (setq total-count (1+ total-count)))))
              (error
                ;; Skip this heading if there's an error processing it
                (message "Warning: Skipped heading due to error: %s" (error-message-string err))))))
      
      ;; Return alist format expected by MCP
      `((file_path . ,file_path)
        (total_headings . ,total-count)
        (level_filter . ,(or level :json-null))
        (parent_filter . ,(or parent_heading :json-null))
        (include_content_stats . ,(if include_content_stats t :json-false))
        (headings . ,(nreverse headings)))))))

(provide 'org-tools)
;;; org-tools.el ends here
