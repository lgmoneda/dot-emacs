;;; emacs-mcp-server-settings.el --- MCP server for Emacs capabilities (org-roam, org-babel) -*- lexical-binding: t -*-

;; This file provides an MCP server that exposes Emacs org-roam and org-babel
;; capabilities to external clients like Claude Code.

;;; Code:

(add-to-list 'load-path "~/.emacs.d/elpa/mcp-server-lib-20250728.457/")
(require 'mcp-server-lib)
(require 'json)
(setq mcp-server-lib-log-io t)

(load (expand-file-name "mcp-tools/org-roam-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-babel-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))

;;; Utility Functions

(defun emacs-mcp--validate-string (value param-name)
  "Validate that VALUE is a non-empty string for PARAM-NAME.
Throws an error if validation fails."
  (unless (stringp value)
    (mcp-server-lib-tool-throw (format "Invalid %s: must be a string" param-name)))
  (when (string-empty-p value)
    (mcp-server-lib-tool-throw (format "Empty %s provided" param-name))))

(defun emacs-mcp--json-bool (value)
  "Convert Elisp boolean VALUE to JSON boolean representation."
  (if value t :json-false))


;;; Org-roam MCP Tool Functions

(defun emacs-mcp--org-roam-retrieve-node-by-id (roam_id)
  "Retrieve an org-roam node by its ID."
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string roam_id "roam_id")
    (let ((result (org-roam-mcp--retrieve-node-by-id roam_id)))
      (json-encode result))))

(defun emacs-mcp--org-roam-search-nodes-by-title (params)
  "Search for org-roam nodes by title pattern.

MCP Parameters:
  params - Alist containing title and optional limit"
  (mcp-server-lib-with-error-handling
    (let ((title (alist-get 'title params))
          (limit (alist-get 'limit params)))
      (emacs-mcp--validate-string title "title")
      (when (and limit (not (or (integerp limit) (null limit))))
        (mcp-server-lib-tool-throw "Invalid limit: must be an integer or null"))
      (let ((result (org-roam-mcp--search-nodes-by-title title limit)))
        (json-encode result)))))

(defun emacs-mcp--org-roam-get-backlinks (params)
  "Get nodes that link to a specific org-roam node.

MCP Parameters:
  params - Alist containing roam_id and optional limit"
  (mcp-server-lib-with-error-handling
    (let ((roam_id (alist-get 'roam_id params))
          (limit (alist-get 'limit params)))
      (emacs-mcp--validate-string roam_id "roam_id")
      (when (and limit (not (or (integerp limit) (null limit))))
        (mcp-server-lib-tool-throw "Invalid limit: must be an integer or null"))
      (let ((result (org-roam-mcp--get-backlinks roam_id limit)))
        (json-encode result)))))

;;; Org-roam MCP Registration

(defun emacs-org-roam-mcp-enable ()
  "Enable the Org-roam MCP tools."
  (mcp-server-lib-register-tool
   #'emacs-mcp--org-roam-retrieve-node-by-id
   :id "org-roam-retrieve-node-by-id"
   :description
   "Retrieve complete information about an org-roam node by its unique ID.
Returns comprehensive node data including content, location, and metadata.

This tool provides detailed information about a specific org-roam node when you
know its UUID. Essential for retrieving full node details including content text,
file location, hierarchical position, and associated metadata.

Parameters:
  roam_id - The org-roam UUID of the node to retrieve (string, required)
           Must be a valid UUID that exists in the org-roam database

Returns JSON object with:
  id - The node's UUID (string)
  title - Node title/heading (string)
  file_path - Absolute path to the file containing this node (string)
  start_line - Line number where node content begins (integer, 1-based)
  end_line - Line number where node content ends (integer, 1-based)
  level - Org heading level (0 for file-level nodes, 1+ for headings) (integer)
  tags - List of org-roam tags associated with this node (array of strings)
  aliases - List of alternative names/aliases for this node (array of strings)
  content - Complete text content of the node (string)

Use cases:
- Fetch complete node details when you have a specific ID
- Retrieve node content for analysis or processing
- Get file location and line numbers for editing
- Access node metadata (tags, aliases, hierarchical position)
- Extract content for summarization or reference

Error cases:
- Node ID not found in database
- Invalid or malformed UUID
- File access issues"
   :read-only t)

  (mcp-server-lib-register-tool
   #'emacs-mcp--org-roam-search-nodes-by-title
   :id "org-roam-search-nodes-by-title"
   :description
   "Search org-roam nodes by title using SQL LIKE pattern matching.
Supports partial matching and returns ranked results with metadata.

This tool helps discover org-roam nodes when you know part of the title but
not the exact UUID. Uses SQL LIKE matching for flexible search capabilities.

Parameters:
  title - Search pattern for node titles (string, required)
          Supports partial matching (e.g., 'project' matches 'My Project Notes')
  limit - Maximum number of results to return (integer, optional, default 10)
          Must be positive integer, reasonable limits recommended for performance

Returns JSON object with:
  search_term - The search pattern that was used (string)
  results_count - Total number of nodes found (integer)
  nodes - Array of matching nodes, each containing:
    id - Node UUID (string)
    title - Node title/heading (string)
    file_path - Absolute path to containing file (string)
    level - Org heading level (integer, 0 for file-level)
    tags - Associated org-roam tags (array of strings)

Search behavior:
- Case-insensitive matching
- Partial string matching (substring search)
- Results ordered by relevance/database order
- Empty titles handled gracefully

Use cases:
- Find nodes by partial title when UUID unknown
- Discover related nodes with similar titles
- Browse available nodes in knowledge base
- Search for specific topics or concepts
- Locate nodes for linking or reference

Error cases:
- Empty search term provided
- Invalid limit parameter
- Database connection issues"
   :read-only t)

  (mcp-server-lib-register-tool
   #'emacs-mcp--org-roam-get-backlinks
   :id "org-roam-get-backlinks"
   :description
   "Find all nodes that link TO a specific org-roam node (reverse link lookup).
Essential for understanding knowledge graph connections and node relationships.

This tool performs reverse link analysis to find which nodes reference a
specific target node. Critical for understanding information flow and
dependencies in your org-roam knowledge base.

Parameters:
  roam_id - UUID of the target node to find backlinks for (string, required)
           Must be a valid UUID that exists in the org-roam database
  limit - Maximum number of backlinks to return (integer, optional, default 10)
          Reasonable limits recommended for large knowledge bases

Returns JSON object with:
  target_id - The UUID of the node that was searched for (string)
  backlinks_count - Total number of nodes linking to the target (integer)
  backlinks - Array of linking nodes, each containing:
    source_id - UUID of the node that contains the link (string)
    source_title - Title of the linking node (string)
    source_file - Absolute path to file containing the linking node (string)
    link_position - Character position of the link in the source file (integer)

Analysis capabilities:
- Identify highly referenced nodes (knowledge hubs)
- Trace information dependencies and relationships
- Find nodes that might be affected by changes
- Discover unexpected connections in knowledge graph
- Support refactoring and content reorganization

Use cases:
- Understand which nodes depend on a specific concept
- Find all references before modifying a node
- Analyze knowledge graph structure and connectivity
- Identify orphaned vs highly-connected nodes
- Support content cleanup and reorganization
- Research information flow patterns

Error cases:
- Target node ID not found in database
- Invalid UUID format
- Database query failures
- File access issues for link resolution

Security: Read-only operation, safe for knowledge base analysis"
   :read-only t))

(defun emacs-org-roam-mcp-disable ()
  "Disable the Org-roam MCP tools."
  (mcp-server-lib-unregister-tool "org-roam-retrieve-node-by-id")
  (mcp-server-lib-unregister-tool "org-roam-search-nodes-by-title")
  (mcp-server-lib-unregister-tool "org-roam-get-backlinks"))


;;; Org-babel MCP Tool Functions

(defun emacs-mcp--org-babel-execute-src-block (params)
  "Execute a specific source block in an org file.

MCP Parameters:
  params - Alist containing file_path and optional block_name"
  (mcp-server-lib-with-error-handling
    (let ((file_path (alist-get 'file_path params))
          (block_name (alist-get 'block_name params)))
      (emacs-mcp--validate-string file_path "file_path")
      (when (and block_name (not (string-empty-p block_name)) (not (null block_name)))
        (emacs-mcp--validate-string block_name "block_name"))
      (let ((result (org-babel-mcp--execute-src-block file_path block_name)))
        (json-encode result)))))

(defun emacs-mcp--org-babel-execute-buffer (params)
  "Execute all source blocks in an org file buffer.

MCP Parameters:
  params - Alist containing file_path"
  (mcp-server-lib-with-error-handling
    (let ((file_path (alist-get 'file_path params)))
      (emacs-mcp--validate-string file_path "file_path")
      (let ((result (org-babel-mcp--execute-buffer file_path)))
        (json-encode result)))))

;;; Org-babel MCP Registration

(defun emacs-org-babel-mcp-enable ()
  "Enable the Org-babel MCP tools."
  (mcp-server-lib-register-tool
   #'emacs-mcp--org-babel-execute-src-block
   :id "execute_src_block"
   :description
   "Execute a specific named Org Babel source block in an org file.
Provides precise control over which block to execute with detailed results.

This tool allows targeted execution of individual source blocks within org files.
Essential for selective code execution, testing specific blocks, or running
particular analysis steps without executing entire documents.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file
  block_name - Name of the source block to execute (string, optional)
              Should match a #+NAME: declaration in the file
              If not provided, executes the first source block found

Returns JSON object with:
  file_path - The org file that was processed (string)
  block_name - Name of the executed block or \"(unnamed)\" (string)
  language - Programming language of the executed block (string)
  line_number - Line number where the block starts (integer, 1-based)
  result - Output/result from block execution (string)
  file_modified - Whether the file was saved with results (boolean, always true)

Execution behavior:
- Executes source block using org-babel
- Captures and formats output/results
- Automatically saves file to persist #+RESULTS
- Maintains org-mode formatting and structure
- Preserves existing file content and organization

Use cases:
- Execute specific analysis or calculation blocks
- Run targeted data processing steps
- Test individual code segments during development
- Generate specific outputs or visualizations
- Update particular results without full document execution

Security considerations:
- Code execution capability - use with trusted org files only
- File modification - automatically saves changes
- Results persistence - writes #+RESULTS sections

Error cases:
- File not found or not accessible
- File is not a valid .org file
- Named block not found in file
- Block execution errors (syntax, runtime issues)
- File write permission issues"
   :read-only nil)

  (mcp-server-lib-register-tool
   #'emacs-mcp--org-babel-execute-buffer
   :id "execute_buffer"
   :description
   "Execute ALL Org Babel source blocks in an org file sequentially.
Comprehensive batch execution with detailed per-block results and error handling.

This tool provides complete document execution by running every source block
in the file from top to bottom. Ideal for full document processing, complete
analysis workflows, or comprehensive report generation.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file

Returns JSON object with:
  file_path - The org file that was processed (string)
  executed_blocks - Number of blocks that executed successfully (integer)
  failed_blocks - Number of blocks that failed during execution (integer)
  total_blocks - Total number of source blocks found and processed (integer)
  execution_details - Array of per-block results, each containing:
    line - Line number where the block starts (integer, 1-based)
    language - Programming language of the block (string)
    success - Whether this block executed without errors (boolean)
    result - Output/result from successful execution (string, empty if failed)
    error - Error message if execution failed (string, null if successful)
  file_saved - Whether the file was saved with all results (boolean, always true)

Execution strategy:
- Processes blocks sequentially in document order
- Continues execution even if individual blocks fail
- Captures both successful results and error details
- Provides comprehensive execution report
- Automatically saves file to persist all #+RESULTS

Error handling:
- Individual block failures don't stop overall execution
- Detailed error reporting per block
- Overall success/failure summary
- Graceful handling of mixed success/failure scenarios

Use cases:
- Complete document processing and report generation
- Batch execution of analysis pipelines
- Full workflow execution from data to results
- Document validation and testing
- Automated report generation
- Comprehensive data processing workflows

Security considerations:
- Executes ALL code blocks in the document
- File modification - saves results automatically
- Use only with trusted org files
- Review document content before batch execution

Performance considerations:
- May take significant time for large documents
- Sequential execution (not parallel)
- Memory usage depends on block outputs
- Consider timeouts for long-running computations

Error cases:
- File not found or not accessible
- File is not a valid .org file
- No source blocks found in file
- File write permission issues
- Mixed execution results (some blocks fail)"
   :read-only nil))

(defun emacs-org-babel-mcp-disable ()
  "Disable the Org-babel MCP tools."
  (mcp-server-lib-unregister-tool "execute_src_block")
  (mcp-server-lib-unregister-tool "execute_buffer"))

;;; Server Management

;;;###autoload
(defun emacs-mcp-enable ()
  "Enable all Emacs MCP tools (org-roam and org-babel)."
  (emacs-org-roam-mcp-enable)
  (emacs-org-babel-mcp-enable))

;;;###autoload
(defun emacs-mcp-disable ()
  "Disable all Emacs MCP tools (org-roam and org-babel)."
  (emacs-org-roam-mcp-disable)
  (emacs-org-babel-mcp-disable))

;; Start the server with all tools enabled
(emacs-mcp-enable)
(mcp-server-lib-start)

(provide 'emacs-mcp-server-settings)
;;; emacs-mcp-server-settings.el ends here
