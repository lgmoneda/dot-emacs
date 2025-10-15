;;; emacs-mcp-server-settings.el --- MCP server for Emacs capabilities (org-roam, org-babel) -*- lexical-binding: t -*-

;; This file provides an MCP server that exposes Emacs org-roam and org-babel
;; capabilities to external clients like Claude Code.

;;; Code:

(add-to-list 'load-path "~/.emacs.d/elpa/mcp-server-lib-20250828.1124/")
(require 'mcp-server-lib)
(require 'json)
(setq mcp-server-lib-log-io t)

(load (expand-file-name "mcp-tools/org-roam-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-babel-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-agenda-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))

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
  "Retrieve an org-roam node by its ID.

MCP Parameters:
  roam_id - string with org roam node ID"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string roam_id "roam_id")
    (let ((result (org-roam-mcp--retrieve-node-by-id roam_id)))
      (json-encode result))))

(defun emacs-mcp--org-roam-search-nodes-by-title (title &optional limit)
  "Search for org-roam nodes by title pattern.
MCP Parameters:
  title - string with title pattern to search for
  limit - optional integer limiting number of results"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string title "title")
    (when (and limit (not (integerp limit)))
      (mcp-server-lib-tool-throw "Invalid limit: must be an integer or null"))
    (let ((result (org-roam-mcp--search-nodes-by-title title limit)))
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

(defun emacs-mcp--org-roam-get-backlinks (roam_id &optional limit)
  "Get nodes that link to a specific org-roam node.
MCP Parameters:
  roam_id - string with org roam node ID
  limit - optional integer limiting number of results"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string roam_id "roam_id")
    (when (and limit (not (integerp limit)))
      (mcp-server-lib-tool-throw "Invalid limit: must be an integer or null"))
    (let ((result (org-roam-mcp--get-backlinks roam_id limit)))
      (json-encode result))))

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

Parameter to call it:
params - Alist containing roam_id and optional limit

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
  async_detected - Whether async execution was detected (boolean)
  execution_time - Time spent waiting for async results in seconds (float)
  timed_out - Whether async polling timed out (boolean)
  still_pending - Whether result is still pending after timeout (boolean)
  retrieval_instructions - How to get pending results if still_pending=true (string)
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


;;; Org-agenda MCP Tool Functions

(defun emacs-mcp--add-todo-simple (params)
  "Add a TODO item to the default personal agenda location.
MCP Parameters:
  params - Alist containing content and optional scheduled"
  (mcp-server-lib-with-error-handling
    (let ((content (alist-get 'content params))
          (scheduled (alist-get 'scheduled params)))
      (emacs-mcp--validate-string content "content")
      (let ((result (org-agenda-mcp--add-todo-simple content scheduled)))
        (json-encode result)))))

(defun emacs-mcp--add-todo-with-refile (params)
  "Add a TODO item and optionally refile it to a specific location.
MCP Parameters:
  params - Alist containing content, optional refile_target, and optional scheduled"
  (mcp-server-lib-with-error-handling
    (let ((content (alist-get 'content params))
          (refile_target (alist-get 'refile_target params))
          (scheduled (alist-get 'scheduled params)))
      (emacs-mcp--validate-string content "content")
      (let ((result (org-agenda-mcp--add-todo-with-refile content refile_target scheduled)))
        (json-encode result)))))

(defun emacs-mcp--get-available-locations ()
  "Get list of available locations for refiling in the personal agenda.

MCP Parameters: None required"
  (mcp-server-lib-with-error-handling
    (let ((result (org-agenda-mcp--get-available-locations)))
      (json-encode result))))

;;; Org-agenda MCP Registration

(defun emacs-org-agenda-mcp-enable ()
  "Enable the Org-agenda MCP tools."
  (mcp-server-lib-register-tool
   #'emacs-mcp--add-todo-simple
   :id "add-todo-simple"
   :description
   "Add a TODO item to your personal agenda using org-capture.
Quickly capture TODO items to the default location (Life/Misc section) in your personal agenda.

This tool provides a simple way to add TODO items to your personal org-mode agenda
without needing to specify a location. Items are automatically placed in the
'Life/Misc' section using the 'd' org-capture template.

Parameters:
  content - The TODO item text/description (string, required)
           Should be a clear, actionable description of the task
           Do not include '* TODO' prefix - this is added automatically

Returns JSON object with:
  success - Whether the capture was successful (boolean)
  content - The captured content (string)
  location - Where the item was placed (string, always 'Life/Misc')
  file - Full path to the agenda file (string)
  template_used - Which org-capture template was used (string)

Behavior:
- Uses org-capture with the 'd' template (Personal task)
- Automatically saves the agenda file
- Items appear under Life > Misc section
- Includes timestamp and empty lines for readability

Use cases:
- Quick TODO capture during conversations
- Adding simple tasks without worrying about categorization
- Fast entry when specific location doesn't matter
- Default option when unsure where to place an item

Example content:
- 'Buy groceries for weekend'
- 'Call dentist to schedule appointment'
- 'Review quarterly budget spreadsheet'
- 'Research vacation destinations for summer'

Security: Safe operation, only adds content to personal agenda file"
   :read-only nil)

  (mcp-server-lib-register-tool
   #'emacs-mcp--add-todo-with-refile
   :id "add-todo-with-refile"
   :description
   "Add a TODO item to your personal agenda with optional refiling to specific location.
Advanced capture that allows placing TODO items in specific sections of your agenda.

This tool first captures the TODO item using org-capture, then optionally refiles
it to a specific location in your agenda hierarchy. Provides precise control over
where items are placed while leveraging your existing org-mode workflow.

Parameters:
  content - The TODO item text/description (string, required)
           Should be a clear, actionable description of the task
           Do not include '* TODO' prefix - this is added automatically
  refile_target - Target location path (string, optional)
                 Format: 'Life/Section Name' (e.g., 'Life/Financial Independence')
                 If omitted, item stays in default 'Life/Misc' location

Returns JSON object with:
  success - Whether the capture was successful (boolean)
  content - The captured content (string)
  location - Final location where item was placed (string)
  file - Full path to the agenda file (string)
  template_used - Which org-capture template was used (string)
  refiled - Whether the item was moved from default location (boolean)

Refiling behavior:
- Uses your org-refile-targets configuration (up to 3 levels deep)
- Searches for matching heading paths in your agenda structure
- Falls back to default location if target not found
- Provides error message with available targets if refile fails

Available refile targets include:
- Life/Goals of the Season
- Life/Family Goals
- Life/Maintenance
- Life/Financial Independence
- Life/House
- Life/Books
- Life/Misc (default)
- And other sections based on your current agenda structure

Use cases:
- Categorizing tasks by life area or project
- Placing financial tasks under 'Life/Financial Independence'
- Adding house-related tasks to 'Life/House'
- Organizing reading tasks under 'Life/Books'
- Strategic task placement for better agenda organization

Example usage:
- content: 'Review investment portfolio', refile_target: 'Life/Financial Independence'
- content: 'Fix kitchen faucet', refile_target: 'Life/House'
- content: 'Read Clean Code book', refile_target: 'Life/Books'

Error handling:
- Validates refile target exists before attempting to refile
- Provides list of available targets if specified target not found
- Graceful fallback to default location on refile failure

Security: Safe operation, only adds/moves content within personal agenda file"
   :read-only nil)

  (mcp-server-lib-register-tool
   #'emacs-mcp--get-available-locations
   :id "get-available-agenda-locations"
   :description
   "Get list of available locations for organizing tasks in your personal agenda.
Discover all available sections and subsections where TODO items can be placed.

This tool provides a comprehensive view of your agenda structure by querying
your org-refile-targets configuration. Essential for understanding where
tasks can be organized and for providing context when refiling items.

Parameters: None required

Returns JSON object with:
  file - Path to your personal agenda file (string)
  total_targets - Total number of available refile locations (integer)
  available_locations - Array of location objects, each containing:
    heading - Full heading path (string, e.g., 'Life/Financial Independence')
    file - File containing this heading (string)
    position - Character position in file (integer)

Location structure reflects your agenda hierarchy:
- Top-level: Life (main category)
- Second-level: Major life areas (Goals, Family, Financial, House, etc.)
- Third-level: Specific subcategories within each area

Common available locations:
- Life/Goals of the Season - Long-term objectives and projects
- Life/Family Goals - Family-related tasks and planning
- Life/Maintenance - Regular upkeep and maintenance tasks
- Life/Financial Independence - Money, investing, and financial planning
- Life/House - Home improvement, repairs, and household tasks
- Life/Books - Reading goals and book-related activities
- Life/Courses - Learning and educational content
- Life/Articles - Articles to read or write
- Life/Writing - Writing projects and creative work
- Life/Misc - General tasks that don't fit other categories

Use cases:
- Understanding your agenda structure before adding tasks
- Choosing appropriate location for new TODO items
- Exploring available categories for task organization
- Getting context for refile operations
- Auditing your current agenda organization

Integration:
- Works with your existing org-refile-targets configuration
- Respects your current agenda file structure
- Updates dynamically as you modify your agenda hierarchy
- Compatible with standard org-mode refile functionality

Security: Read-only operation, safe for exploring agenda structure"
   :read-only t))

(defun emacs-org-agenda-mcp-disable ()
  "Disable the Org-agenda MCP tools."
  (mcp-server-lib-unregister-tool "add-todo-simple")
  (mcp-server-lib-unregister-tool "add-todo-with-refile")
  (mcp-server-lib-unregister-tool "get-available-agenda-locations"))

;;; Server Management

;;;###autoload
(defun emacs-mcp-enable ()
  "Enable all Emacs MCP tools (org-roam, org-babel, and org-agenda)."
  (emacs-org-roam-mcp-enable)
  (emacs-org-babel-mcp-enable)
  (emacs-org-agenda-mcp-enable)
  )

;;;###autoload
(defun emacs-mcp-disable ()
  "Disable all Emacs MCP tools (org-roam, org-babel, and org-agenda)."
  (emacs-org-roam-mcp-disable)
  (emacs-org-babel-mcp-disable)
  (emacs-org-agenda-mcp-disable))

;; Start the server with all tools enabled
(emacs-mcp-enable)
(mcp-server-lib-start)

(provide 'emacs-mcp-server-settings)
;;; emacs-mcp-server-settings.el ends here
