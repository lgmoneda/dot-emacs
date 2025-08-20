;;; claude-code-ide-settings.el --- Functions for the Claude Code IDE

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el" :branch "main")
  :bind ("C-x a" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  )

;; for eat terminal backend:
;; (use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

; Add MCP tools to load path
(load (expand-file-name "mcp-tools/org-roam-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-babel-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))

;;; Claude Code IDE MCP Tool Functions with Context

(defun org-roam-mcp-retrieve-node-by-id-with-context (roam-id)
  "Retrieve an org-roam node by its ID with session context."
  (claude-code-ide-mcp-server-with-session-context nil
    (org-roam-mcp--retrieve-node-by-id roam-id)))

(defun org-roam-mcp-search-nodes-by-title-with-context (title &optional limit)
  "Search for org-roam nodes by title with session context."
  (claude-code-ide-mcp-server-with-session-context nil
    (org-roam-mcp--search-nodes-by-title title limit)))

(defun org-roam-mcp-get-backlinks-with-context (roam-id &optional limit)
  "Get nodes that link to the node with ROAM-ID with session context."
  (claude-code-ide-mcp-server-with-session-context nil
    (org-roam-mcp--get-backlinks roam-id limit)))

;; Define and register the retrieve_node_by_id tool
(claude-code-ide-make-tool
 :function #'org-roam-mcp-retrieve-node-by-id-with-context
 :name "org-roam-retrieve-node-by-id"
 :description "Retrieve file path, start line, end line, and content of an org-roam node by its ID"
 :args '((:name "roam_id"
          :type string
          :description "The org-roam ID of the node to retrieve")))

;; Define and register the search_nodes_by_title tool
(claude-code-ide-make-tool
 :function #'org-roam-mcp-search-nodes-by-title-with-context
 :name "org-roam-search-nodes-by-title"
 :description "Search for org-roam nodes by title pattern"
 :args '((:name "title"
          :type string
          :description "Search pattern for node titles")
         (:name "limit"
          :type integer
          :description "Maximum number of results to return (default: 10)"
          :optional t)))

;; Define and register the get_backlinks tool
(claude-code-ide-make-tool
 :function #'org-roam-mcp-get-backlinks-with-context
 :name "org-roam-get-backlinks"
 :description "Find all nodes that link to a specific org-roam node"
 :args '((:name "roam_id"
          :type string
          :description "The org-roam ID of the target node")
         (:name "limit"
          :type integer
          :description "Maximum number of backlinks to return (default: 10)"
          :optional t)))

;;; Claude Code IDE MCP Tool Functions with Context

(defun org-babel-mcp-execute-src-block-with-context (file-path &optional block-name)
  "Execute a specific source block in an org file with session context."
  (if (fboundp 'claude-code-ide-mcp-server-with-session-context)
      (claude-code-ide-mcp-server-with-session-context nil
        (org-babel-mcp--execute-src-block file-path block-name))
    (org-babel-mcp--execute-src-block file-path block-name)))

(defun org-babel-mcp-execute-buffer-with-context (file-path)
  "Execute all source blocks in an org file buffer with session context."
  (if (fboundp 'claude-code-ide-mcp-server-with-session-context)
      (claude-code-ide-mcp-server-with-session-context nil
        (org-babel-mcp--execute-buffer file-path))
    (org-babel-mcp--execute-buffer file-path)))

;; Define and register the execute_src_block tool
(claude-code-ide-make-tool
 :function #'org-babel-mcp-execute-src-block-with-context
 :name "org-babel-execute-src-block"
 :description "Execute a specific Org Babel source block in an org file"
 :args '((:name "file_path"
          :type string
          :description "Path to the .org file")
         (:name "block_name"
          :type string
          :description "Optional name of the block to execute. If not provided, executes the first source block found"
          :optional t)))

;; Define and register the execute_buffer tool
(claude-code-ide-make-tool
 :function #'org-babel-mcp-execute-buffer-with-context
 :name "org-babel-execute-buffer"
 :description "Execute all Org Babel source blocks in an org file buffer"
 :args '((:name "file_path"
          :type string
          :description "Path to the .org file")))

;;; Enable Emacs Tools MCP Server
(claude-code-ide-emacs-tools-setup)

(provide 'claude-code-ide-settings)
;;; claude-code-ide-settings.el ends here
