;;; emacs-mcp-server-settings.el --- Functions to manage an MCP server exposing Emacs capabilities

(add-to-list 'load-path "~/.emacs.d/elpa/mcp-server-lib-20250728.457/")
(require 'mcp-server-lib)

;;; Org-roam MCP tools

(defun emacs-org-roam-mcp-enable ()
  "Enable the Org-roam MCP tools."
  (mcp-server-lib-register-tool
   #'org-roam-mcp-retrieve-node-by-id
   :id "org-roam-retrieve-node-by-id"
   :title "Retrieve Org-roam Node by ID"
   :description
   "Retrieve file path, start line, end line, and content of an org-roam node by its ID.

Parameters:
  roam_id - The org-roam ID of the node to retrieve (required)

Returns node information including:
- node ID and title
- file path and location (start/end line numbers)
- node level and hierarchical position
- associated tags and aliases
- complete node content text
- success/error status with detailed messages

The org-roam ID should be a valid UUID string that exists in the org-roam database.
Use this tool to fetch complete node details when you know the specific node ID."
   :read-only t)
  (mcp-server-lib-register-tool
   #'org-roam-mcp-search-nodes-by-title
   :id "org-roam-search-nodes-by-title"
   :title "Search Org-roam Nodes by Title"
   :description
   "Search for org-roam nodes by title pattern using SQL LIKE matching.

Parameters:
  title - Search pattern for node titles (required)
  limit - Maximum number of results to return (optional, default: 10)

Returns search results including:
- list of matching nodes with ID, title, file path
- node level and associated tags
- total count of results found
- formatted summary of all matches

The title parameter supports partial matching (e.g., 'project' will match 'My Project Notes').
Use this tool when you need to find nodes by their title content but don't know the exact ID."
   :read-only t)
  (mcp-server-lib-register-tool
   #'org-roam-mcp-get-backlinks
   :id "org-roam-get-backlinks"
   :title "Get Org-roam Node Backlinks"
   :description
   "Find all nodes that link to a specific org-roam node (reverse link lookup).

Parameters:
  roam_id - The org-roam ID of the target node (required)
  limit - Maximum number of backlinks to return (optional, default: 10)

Returns backlink information including:
- source node details (ID, title, file path)
- link position within source files
- total count of backlinks found
- formatted list of all linking nodes

Use this tool to understand node relationships and find all references to a specific node.
Essential for analyzing knowledge graph connections and content dependencies."
   :read-only t))

(defun emacs-org-roam-mcp-disable ()
  "Disable the Org-roam MCP tools."
  (mcp-server-lib-unregister-tool "org-roam-retrieve-node-by-id")
  (mcp-server-lib-unregister-tool "org-roam-search-nodes-by-title")
  (mcp-server-lib-unregister-tool "org-roam-get-backlinks"))

;;; Org-babel MCP tools

(defun emacs-org-babel-mcp-enable ()
  "Enable the Org-babel MCP tools."
  (mcp-server-lib-register-tool
   #'org-babel-mcp-execute-src-block
   :id "org-babel-execute-src-block"
   :title "Execute Org Babel Source Block"
   :description
   "Execute a specific Org Babel source block in an org file.

Parameters:
  file_path - Path to the .org file (required)
  block_name - Optional name of the block to execute. If not provided,
               executes the first source block found.

Returns execution results including:
- success status and execution confirmation
- output from the block execution (#+RESULTS content)
- error messages if any occur during execution
- block metadata (language, line number, file path)
- file modification status

The block name should match a #+NAME: declaration in the org file.
If no block name is provided, the first source block in the file will be executed.
The file is automatically saved after successful execution to persist results."
   :read-only nil)
  (mcp-server-lib-register-tool
   #'org-babel-mcp-execute-buffer
   :id "org-babel-execute-buffer"
   :title "Execute All Org Babel Blocks"
   :description
   "Execute all Org Babel source blocks in an org file buffer.

Parameters:
  file_path - Path to the .org file (required)

Returns execution results including:
- total number of blocks executed successfully
- count of failed executions (if any)
- detailed results for each individual block
- execution output and error details per block
- file save status after execution

All source blocks in the file will be executed sequentially in the order
they appear in the file. The results include comprehensive information about each
individual block execution, including line numbers, languages, and success status.
The file is automatically saved to persist all execution results."
   :read-only nil))

(defun emacs-org-babel-mcp-disable ()
  "Disable the Org-babel MCP tools."
  (mcp-server-lib-unregister-tool "org-babel-execute-src-block")
  (mcp-server-lib-unregister-tool "org-babel-execute-buffer"))

;; Start the server
(mcp-server-lib-start)

(provide 'emacs-mcp-server-settings)
;;; emacs-mcp-server-settings.el ends here
