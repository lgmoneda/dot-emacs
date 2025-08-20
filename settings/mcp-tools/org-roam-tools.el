;;; org-roam-tools.el --- MCP tools for org-roam
;;; Code:

(require 'org-roam)
(require 'org-roam-node)
(require 'org-roam-db)

;;; Utility Functions

(defun org-roam-mcp--get-node-content (node)
  "Extract the content of a NODE from its file.
Returns the content between start and end positions."
  (let ((file (org-roam-node-file node))
        (point (org-roam-node-point node)))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char point)
        (let ((start-line (line-number-at-pos point)))
          (if (= (org-roam-node-level node) 0)
              ;; For file-level nodes, get entire content
              (let ((end-line (line-number-at-pos (point-max))))
                (list (buffer-string) start-line end-line))
            ;; For heading nodes, get content until next heading at same or higher level
            (org-mode)
            (goto-char point)
            (let ((level (org-roam-node-level node))
                  (start (point))
                  (end (point-max)))
              (when (outline-next-heading)
                (while (and (not (eobp))
                           (> (org-outline-level) level))
                  (unless (outline-next-heading)
                    (goto-char (point-max))))
                (when (and (not (eobp))
                          (<= (org-outline-level) level))
                  (setq end (point))))
              (let ((content (buffer-substring-no-properties start end))
                    (end-line (line-number-at-pos end)))
                (list content start-line end-line)))))))))

;;; MCP Tool Functions

(defun org-roam-mcp--retrieve-node-by-id (roam_id)
  "Retrieve an org-roam node by its ID.
MCP Parameters:
  roam_id - The org-roam node ID to retrieve"
  (mcp-server-lib-with-error-handling
    (unless (stringp roam_id)
      (signal 'wrong-type-argument (list 'stringp roam_id)))
    (when (string-empty-p roam_id)
      (error "Empty ID provided"))
    
    (let ((node (org-roam-node-from-id roam_id)))
      (unless node
        (error "No node found with ID: %s" roam_id))
      
      (let* ((file (org-roam-node-file node))
             (content-info (org-roam-mcp--get-node-content node))
             (content (nth 0 content-info))
             (start-line (nth 1 content-info))
             (end-line (nth 2 content-info)))
        
        ;; Return alist format expected by MCP
        `((id . ,roam_id)
          (title . ,(or (org-roam-node-title node) ""))
          (file_path . ,(or file ""))
          (start_line . ,start-line)
          (end_line . ,end-line)
          (level . ,(or (org-roam-node-level node) 0))
          (tags . ,(org-roam-node-tags node))
          (aliases . ,(org-roam-node-aliases node))
          (content . ,(or content "")))))))

(defun org-roam-mcp--search-nodes-by-title (title &optional limit)
  "Search for org-roam nodes by title pattern.
MCP Parameters:
  title - The title pattern to search for
  limit - Optional maximum number of results (default 10)"
  (mcp-server-lib-with-error-handling
    (unless (stringp title)
      (signal 'wrong-type-argument (list 'stringp title)))
    (when (string-empty-p title)
      (error "Empty title provided"))
    
    (setq limit (or limit 10))
    
    (let* ((query [:select [id title file level tags]
                   :from nodes
                   :where (like title $r1)
                   :limit $s2])
           (results (org-roam-db-query query (format "%%%s%%" title) limit))
           (nodes '()))
      
      (dolist (result results)
        (let ((id (nth 0 result))
              (node-title (nth 1 result))
              (file (nth 2 result))
              (level (nth 3 result))
              (tags (nth 4 result)))
          (push `((id . ,id)
                  (title . ,(or node-title ""))
                  (file_path . ,(or file ""))
                  (level . ,(or level 0))
                  (tags . ,(if tags (split-string tags) '())))
                nodes)))
      
      ;; Return alist format
      `((search_term . ,title)
        (results_count . ,(length results))
        (nodes . ,(nreverse nodes))))))

(defun org-roam-mcp--get-backlinks (roam_id &optional limit)
  "Get nodes that link to a specific org-roam node.
MCP Parameters:
  roam_id - The node ID to find backlinks for
  limit - Optional maximum number of backlinks (default 10)"
  (mcp-server-lib-with-error-handling
    (unless (stringp roam_id)
      (signal 'wrong-type-argument (list 'stringp roam_id)))
    (when (string-empty-p roam_id)
      (error "Empty ID provided"))
    
    (setq limit (or limit 10))
    
    (let* ((query [:select [nodes:id nodes:title nodes:file links:pos]
                   :from links
                   :inner-join nodes :on (= links:source nodes:id)
                   :where (= links:dest $s1)
                   :limit $s2])
           (results (org-roam-db-query query roam_id limit))
           (backlinks '()))
      
      (dolist (result results)
        (let ((source-id (nth 0 result))
              (source-title (nth 1 result))
              (source-file (nth 2 result))
              (link-pos (nth 3 result)))
          (push `((source_id . ,source-id)
                  (source_title . ,(or source-title ""))
                  (source_file . ,(or source-file ""))
                  (link_position . ,(or link-pos 0)))
                backlinks)))
      
      ;; Return alist format
      `((target_id . ,roam_id)
        (backlinks_count . ,(length results))
        (backlinks . ,(nreverse backlinks))))))

(provide 'org-roam-tools)
;;; org-roam-tools.el ends here
