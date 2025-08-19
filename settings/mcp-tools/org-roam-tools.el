;;; org-roam-mcp.tools ---
;;; Code:

(require 'org-roam)
(require 'org-roam-node)
(require 'org-roam-db)

;;; Utility Functions

(defun org-roam-mcp--validate-id (roam-id)
  "Validate that ROAM-ID is a non-empty string.
Throws an error if validation fails."
  (unless (stringp roam-id)
    (error "Invalid ID: must be a string"))
  (when (string-empty-p roam-id)
    (error "Empty ID provided")))

(defun org-roam-mcp--format-success (message &optional details)
  "Format a success response as string.
MESSAGE is the success message.
DETAILS is an optional alist of additional details."
  (if details
      (format "%s\n\nDetails:\n%s" message
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    message))

(defun org-roam-mcp--format-error (message &optional details)
  "Format an error response as string.
MESSAGE is the error message.
DETAILS is an optional alist of additional details."
  (if details
      (format "Error: %s\n\nDetails:\n%s" message
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    (format "Error: %s" message)))

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

(defun org-roam-mcp-retrieve-node-by-id (roam-id)
  "Retrieve an org-roam node by its ID.
Returns file path, start line, end line, and content of the node."
  (org-roam-mcp--validate-id roam-id)

  (let ((node (org-roam-node-from-id roam-id)))
    (unless node
      (error
       (format "No node found with ID: %s" roam-id)))

    (let* ((file (org-roam-node-file node))
           (content-info (org-roam-mcp--get-node-content node))
           (content (nth 0 content-info))
           (start-line (nth 1 content-info))
           (end-line (nth 2 content-info)))

      (org-roam-mcp--format-success
       (format "Retrieved node: %s" (or (org-roam-node-title node) "Untitled"))
       `(("id" . ,roam-id)
         ("title" . ,(or (org-roam-node-title node) ""))
         ("file_path" . ,(or file ""))
         ("start_line" . ,(number-to-string start-line))
         ("end_line" . ,(number-to-string end-line))
         ("level" . ,(number-to-string (or (org-roam-node-level node) 0)))
         ("tags" . ,(mapconcat 'identity (org-roam-node-tags node) ", "))
         ("aliases" . ,(mapconcat 'identity (org-roam-node-aliases node) ", "))
         ("content" . ,(or content "")))))))

(defun org-roam-mcp-search-nodes-by-title (title &optional limit)
  "Search for org-roam nodes by TITLE.
Returns up to LIMIT matches (default 10)."
  (unless (stringp title)
    (error "Title must be a string"))
  (when (string-empty-p title)
    (error "Empty title provided"))

  (setq limit (or limit 10))

  (let* ((query [:select [id title file level tags]
                 :from nodes
                 :where (like title $r1)
                 :limit $s2])
         (results (org-roam-db-query query (format "%%%s%%" title) limit))
         (formatted-results '()))

    (dolist (result results)
      (let ((id (nth 0 result))
            (node-title (nth 1 result))
            (file (nth 2 result))
            (level (nth 3 result))
            (tags (nth 4 result)))
        (push `(("id" . ,id)
                ("title" . ,(or node-title ""))
                ("file_path" . ,(or file ""))
                ("level" . ,(number-to-string (or level 0)))
                ("tags" . ,(or tags "")))
              formatted-results)))

    (org-roam-mcp--format-success
     (format "Found %d nodes matching title '%s'" (length results) title)
     `(("search_term" . ,title)
       ("results_count" . ,(number-to-string (length results)))
       ("results" . ,(mapconcat
                     (lambda (result)
                       (format "ID: %s | Title: %s | File: %s"
                               (cdr (assoc "id" result))
                               (cdr (assoc "title" result))
                               (file-name-nondirectory (cdr (assoc "file_path" result)))))
                     (reverse formatted-results) "\n"))))))

(defun org-roam-mcp-get-backlinks (roam-id &optional limit)
  "Get nodes that link to the node with ROAM-ID.
Returns up to LIMIT backlinks (default 10)."
  (org-roam-mcp--validate-id roam-id)

  (setq limit (or limit 10))

  (let* ((query [:select [nodes:id nodes:title nodes:file links:pos]
                 :from links
                 :inner-join nodes :on (= links:source nodes:id)
                 :where (= links:dest $s1)
                 :limit $s2])
         (results (org-roam-db-query query roam-id limit))
         (formatted-results '()))

    (dolist (result results)
      (let ((source-id (nth 0 result))
            (source-title (nth 1 result))
            (source-file (nth 2 result))
            (link-pos (nth 3 result)))
        (push `(("source_id" . ,source-id)
                ("source_title" . ,(or source-title ""))
                ("source_file" . ,(or source-file ""))
                ("link_position" . ,(number-to-string (or link-pos 0))))
              formatted-results)))

    (org-roam-mcp--format-success
     (format "Found %d backlinks to node %s" (length results) roam-id)
     `(("target_id" . ,roam-id)
       ("backlinks_count" . ,(number-to-string (length results)))
       ("backlinks" . ,(mapconcat
                       (lambda (result)
                         (format "From: %s (%s) at position %s"
                                 (cdr (assoc "source_title" result))
                                 (file-name-nondirectory (cdr (assoc "source_file" result)))
                                 (cdr (assoc "link_position" result))))
                       (reverse formatted-results) "\n"))))))

(provide 'org-roam-tools)
;;; org-roam-tools.el ends here
