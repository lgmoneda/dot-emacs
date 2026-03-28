;;; org-roam-tools.el --- MCP tools for org-roam
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)

(defvar org-roam-mcp-semantic-search-api-base-url "http://localhost:8800/api/"
  "Base URL for the org-roam semantic search service.")

(defvar org-roam-mcp-semantic-search-timeout 15
  "Timeout in seconds for semantic search HTTP requests.")

(eval-when-compile
  (require 'org-roam nil 'noerror)
  (require 'org-roam-node nil 'noerror)
  (require 'org-roam-db nil 'noerror))

(defun org-roam-mcp--ensure-dependencies ()
  "Ensure org-roam libraries are available before executing a tool."
  (unless (featurep 'org-roam)
    (unless (require 'org-roam nil 'noerror)
      (mcp-server-lib-tool-throw "Org-roam not available. Install org-roam and load Emacs configuration.")))
  (unless (featurep 'org-roam-node)
    (unless (require 'org-roam-node nil 'noerror)
      (mcp-server-lib-tool-throw "org-roam-node library missing. Ensure org-roam is properly installed.")))
  (unless (featurep 'org-roam-db)
    (unless (require 'org-roam-db nil 'noerror)
      (mcp-server-lib-tool-throw "org-roam-db library missing. Ensure org-roam is properly installed."))))

;;; Utility Functions

(defun org-roam-mcp--get-node-content (node)
  "Extract the content of a NODE from its file.
Returns the content between start and end positions."
  (org-roam-mcp--ensure-dependencies)
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

(defun org-roam-mcp--semantic-search-request (query)
  "Send QUERY to the semantic search API and return the decoded response body."
  (let* ((encoded-query (url-hexify-string query))
         (request-url (concat org-roam-mcp-semantic-search-api-base-url encoded-query))
         (url-request-method "GET")
         (url-request-extra-headers '(("Content-Type" . "text/plain")))
         (buffer (url-retrieve-synchronously request-url t nil org-roam-mcp-semantic-search-timeout)))
    (unless buffer
      (error "Semantic search API unavailable at %s" org-roam-mcp-semantic-search-api-base-url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (let ((status-code (string-to-number (match-string 1))))
              (when (>= status-code 400)
                (search-forward-regexp "\n\n" nil t)
                (error "Semantic search API returned HTTP %d: %s"
                       status-code
                       (string-trim (buffer-substring-no-properties (point) (point-max)))))))
          (goto-char (point-min))
          (unless (search-forward-regexp "\n\n" nil t)
            (error "Malformed response from semantic search API"))
          (string-trim
           (decode-coding-string
            (buffer-substring-no-properties (point) (point-max))
            'utf-8)))
      (kill-buffer buffer))))

(defun org-roam-mcp--parse-semantic-search-results (response)
  "Parse semantic search RESPONSE into structured MCP-friendly entries."
  (let ((lines (split-string response "\n" t))
        (parsed-results '())
        (rank 0))
    (dolist (line lines)
      (let ((clean-line (string-trim line)))
        (unless (string-empty-p clean-line)
          (setq rank (1+ rank))
          (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" clean-line)
              (push `((rank . ,rank)
                      (id . ,(match-string 1 clean-line))
                      (title . ,(match-string 2 clean-line))
                      (text . ,clean-line))
                    parsed-results)
            (push `((rank . ,rank)
                    (text . ,clean-line))
                  parsed-results)))))
    (nreverse parsed-results)))

;;; MCP Tool Functions

(defun org-roam-mcp--retrieve-node-by-id (roam_id)
  "Retrieve an org-roam node by its ID.
MCP Parameters:
  roam_id - The org-roam node ID to retrieve"
  (mcp-server-lib-with-error-handling
    (org-roam-mcp--ensure-dependencies)
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
    (org-roam-mcp--ensure-dependencies)
    (require 'subr-x)
    (unless (stringp title)
      (signal 'wrong-type-argument (list 'stringp title)))
    (when (string-empty-p title)
      (error "Empty title provided"))

    (setq limit (or limit 10))
    (let* ((pattern (format "%%%s%%" title))
           ;; nodes: list of vectors [id title file level]
           (node-results
            (org-roam-db-query
             '[:select [id title file level]
               :from nodes
               :where (like title $s1)
               :limit $s2]
             pattern limit))
           (nodes '()))
      (dolist (row node-results)
        (let* ((id         (elt row 0))
               (node-title (elt row 1))
               (file       (elt row 2))
               (level      (elt row 3))
               ;; tags-results: list of vectors [tag]
               (tags-results
                (org-roam-db-query
                 '[:select tag :from tags :where (= node_id $s1)]
                 id))
               (tags (mapcar (lambda (r) (elt r 0)) tags-results)))
          (push `((id . ,id)
                  (title . ,(or node-title ""))
                  (file_path . ,(or file ""))
                  (level . ,(or level 0))
                  (tags . ,tags))
                nodes)))
      `((search_term . ,title)
        (results_count . ,(length node-results))
        (nodes . ,(nreverse nodes))))))

(defun org-roam-mcp--get-backlinks (roam_id &optional limit)
  "Get nodes that link to a specific org-roam node.
MCP Parameters:
  roam_id - The node ID to find backlinks for
  limit - Optional maximum number of backlinks (default 10)"
  (mcp-server-lib-with-error-handling
    (org-roam-mcp--ensure-dependencies)
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

(defun org-roam-mcp--semantic-search (query &optional limit)
  "Search org-roam notes semantically by QUERY using the external API.
MCP Parameters:
  query - Natural language query to send to the semantic search API
  limit - Optional maximum number of parsed entries to return (default 10)"
  (mcp-server-lib-with-error-handling
    (unless (stringp query)
      (signal 'wrong-type-argument (list 'stringp query)))
    (when (string-empty-p query)
      (error "Empty query provided"))
    (when (and limit (not (integerp limit)))
      (signal 'wrong-type-argument (list 'integerp limit)))

    (let* ((effective-limit (or limit 10))
           (_ (when (< effective-limit 1)
                (error "limit must be >= 1")))
           (raw-response (org-roam-mcp--semantic-search-request query))
           (parsed-results (org-roam-mcp--parse-semantic-search-results raw-response))
           (limited-results (if (> (length parsed-results) effective-limit)
                                (cl-subseq parsed-results 0 effective-limit)
                              parsed-results)))
      `((query . ,query)
        (api_base_url . ,org-roam-mcp-semantic-search-api-base-url)
        (limit . ,effective-limit)
        (results_count . ,(length limited-results))
        (total_results . ,(length parsed-results))
        (results . ,limited-results)
        (raw_response . ,raw-response)))))

(provide 'org-roam-tools)
;;; org-roam-tools.el ends here
