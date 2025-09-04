;;; custom-emacs-mcp-server-settings.el --- MCP server for Emacs capabilities (org-roam, org-babel) -*- lexical-binding: t -*-

;; This file provides an MCP server that exposes Emacs org-roam and org-babel
;; capabilities to external clients like Claude Code.

;;; Code:

;; Force unload old version and load development version
(when (featurep 'mcp-server-lib)
  (unload-feature 'mcp-server-lib t))

;; Explicitly load the development version with full path to avoid ELPA conflicts
(load-file "/Users/luis.moneda/repos/mcp-server-lib.el/mcp-server-lib-metrics.el")
(load-file "/Users/luis.moneda/repos/mcp-server-lib.el/mcp-server-lib.el")
(load-file "/Users/luis.moneda/repos/mcp-server-lib.el/mcp-server-lib-commands.el")

;; Verify we have the new API
(unless (fboundp 'mcp-server-lib-register-tool)
  (error "mcp-server-lib-register-tool not found - development version not loaded correctly"))

;; Test the function signature to ensure we have the new version
(let ((func-def (symbol-function 'mcp-server-lib-register-tool)))
  (unless (eq (car-safe (help-function-arglist func-def)) '&rest)
    (error "Wrong version of mcp-server-lib-register-tool loaded - expected new API with &rest slots")))
(require 'json)
(setq mcp-server-lib-log-io t)

(load (expand-file-name "mcp-tools/org-roam-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-babel-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-agenda-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))
(load (expand-file-name "mcp-tools/org-notebook-tools.el" (file-name-directory (or load-file-name (buffer-file-name)))))

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


;;; Org-roam MCP Registration

(defun emacs-org-roam-mcp-enable ()
  "Enable the Org-roam MCP tools."
  (mcp-server-lib-register-tool
   :function #'emacs-mcp--org-roam-retrieve-node-by-id
   :name "org-roam-retrieve-node-by-id"
   :description "Retrieve complete information about an org-roam node by its unique ID.
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
   :args '((:name "roam_id"
            :type string
            :description "The org-roam UUID of the node to retrieve"))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--org-roam-search-nodes-by-title
   :name "org-roam-search-nodes-by-title"
   :description "Search org-roam nodes by title using SQL LIKE pattern matching.
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
   :args '((:name "title"
            :type string
            :description "Search pattern for node titles")
           (:name "limit"
            :type integer
            :description "Maximum number of results to return"
            :optional t))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--org-roam-get-backlinks
   :name "org-roam-get-backlinks"
   :description "Find all nodes that link TO a specific org-roam node (reverse link lookup).
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
   :args '((:name "roam_id"
            :type string
            :description "UUID of the target node to find backlinks for")
           (:name "limit"
            :type integer
            :description "Maximum number of backlinks to return"
            :optional t))
   :read-only t))

(defun emacs-org-roam-mcp-disable ()
  "Disable the Org-roam MCP tools."
  (mcp-server-lib-unregister-tool "org-roam-retrieve-node-by-id")
  (mcp-server-lib-unregister-tool "org-roam-search-nodes-by-title")
  (mcp-server-lib-unregister-tool "org-roam-get-backlinks"))


;;; Org-babel MCP Tool Functions

(defun emacs-mcp--org-babel-execute-src-block (file_path &optional heading_title line_number)
  "Execute a specific source block in an org file.
MCP Parameters:
  file_path - Path to the org file
  heading_title - Optional heading title (e.g., 'EDA') - finds first src block under that heading
  line_number - Optional line number - finds first src block at or after this line
  If both are provided, heading_title takes precedence
  If neither is provided, executes the first source block found"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (when (and heading_title
               (stringp heading_title)
               (string-empty-p heading_title))
      (mcp-server-lib-tool-throw "Empty heading_title provided"))
    (when (and heading_title (not (stringp heading_title)))
      (mcp-server-lib-tool-throw "Invalid heading_title: must be a string"))
    (when (and line_number (not (numberp line_number)))
      (mcp-server-lib-tool-throw "Invalid line_number: must be a number"))
    (let ((result (org-babel-mcp--execute-src-block file_path heading_title line_number)))
      (json-encode result))))

(defun emacs-mcp--org-babel-execute-subtree (file_path heading_title)
  "Execute all source blocks within a specific org subtree.
MCP Parameters:
  file_path - Path to the org file
  heading_title - Title of the heading whose subtree should be executed"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (emacs-mcp--validate-string heading_title "heading_title")
    (let ((result (org-babel-mcp--execute-subtree file_path heading_title)))
      (json-encode result))))

(defun emacs-mcp--org-babel-execute-buffer (file_path)
  "Execute all source blocks in an org file buffer.
MCP Parameters:
  file_path - Path to the org file"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (let ((result (org-babel-mcp--execute-buffer file_path)))
      (json-encode result))))

;;; Org-babel MCP Registration

(defun emacs-org-babel-mcp-enable ()
  "Enable the Org-babel MCP tools."
  (mcp-server-lib-register-tool
   :function #'emacs-mcp--org-babel-execute-src-block
   :name "execute_src_block"
   :description "Execute a specific Org Babel source block in an org file.
Provides precise control over which block to execute with detailed results.

This tool allows targeted execution of individual source blocks within org files.
Essential for selective code execution, testing specific blocks, or running
particular analysis steps without executing entire documents.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file
  heading_title - Optional heading title (e.g., 'EDA') - finds first src block under that heading
  line_number - Optional line number - finds first src block at or after this line
  If both are provided, heading_title takes precedence
  If neither is provided, executes the first source block found

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
- Referenced block not found (heading or line number)
- Block execution errors (syntax, runtime issues)
- File write permission issues"
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the .org file")
           (:name "heading_title"
            :type string
            :description "Optional heading title to find first src block under"
            :optional t)
           (:name "line_number"
            :type integer
            :description "Optional line number to find first src block at or after"
            :optional t))
   :read-only nil)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--org-babel-execute-buffer
   :name "execute_buffer"
   :description "Execute ALL Org Babel source blocks in an org file sequentially.
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
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the .org file"))
   :read-only nil)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--org-babel-execute-subtree
   :name "execute_subtree"
   :description "Execute all source blocks within a specific org subtree.
Provides targeted execution of all code blocks under a heading and its subheadings.

This tool uses org-mode's narrowing mechanism to safely execute all source blocks
within a specified subtree without line number conflicts. Essential for executing
logically related code sections like analysis chapters, methodology sections,
or experimental workflows.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file
  heading_title - Title of the heading whose subtree should be executed (string, required)
                 Uses substring matching to find the target heading

Returns JSON object with:
  file_path - The org file that was processed (string)
  heading_title - The heading title that was targeted (string)
  subtree_start_line - Line number where subtree begins (integer)
  subtree_end_line - Line number where subtree ends (integer)
  executed_blocks - Number of blocks executed successfully (integer)
  failed_blocks - Number of blocks that failed execution (integer)
  total_blocks - Total number of blocks found in subtree (integer)
  execution_details - Array of per-block results, each containing:
    line - Line number of the block within subtree (integer)
    language - Programming language of the block (string)
    success - Whether block executed without errors (boolean)
    result - Output/result from successful execution (string)
    error - Error message if execution failed (string or null)
  file_saved - Whether file was saved with results (boolean, always true)

Execution strategy:
- Finds target heading using substring matching
- Uses org-narrow-to-subtree to isolate the section
- Executes all blocks sequentially within narrowed region
- Widens back to full document immediately after execution
- Continues execution even if individual blocks fail
- Saves file to persist all #+RESULTS

Advantages over execute_buffer:
- Targeted execution of logical sections
- Efficient for large documents with multiple unrelated sections
- Respects org-mode hierarchical structure
- Includes all nested subheadings automatically
- No line number conflicts as results are added

Use cases:
- Execute specific analysis sections (\\\"Data Processing\\\", \\\"Results\\\")
- Run experimental methodology sections independently
- Process individual chapters or modules in large documents
- Execute related code blocks as logical units
- Iterative development of specific document sections

Best practices:
- Use descriptive heading titles for reliable matching
- Organize related code blocks under common headings
- Consider execution dependencies between blocks
- Review execution_details for failed blocks

Error cases:
- File not found or not accessible
- File is not a valid .org file
- Heading not found in document
- No source blocks found in subtree
- Individual block execution errors (captured in execution_details)
- File write permission issues

Security: Code execution capability - use with trusted org files only"
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the .org file")
           (:name "heading_title"
            :type string
            :description "Title of the heading whose subtree should be executed"))
   :read-only nil))

(defun emacs-org-babel-mcp-disable ()
  "Disable the Org-babel MCP tools."
  (mcp-server-lib-unregister-tool "execute_src_block")
  (mcp-server-lib-unregister-tool "execute_subtree")
  (mcp-server-lib-unregister-tool "execute_buffer"))


;;; Org-notebook MCP Tool Functions

(defun emacs-mcp--get-analytical-review (file_path &optional heading_titles)
  "Extract headings, content, and results for analytical review of org notebooks.
MCP Parameters:
  file_path - Absolute path to the org file to analyze
  heading_titles - Optional list of specific heading titles to extract"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (when heading_titles
      ;; Convert JSON array to Elisp list if needed
      (when (vectorp heading_titles)
        (setq heading_titles (append heading_titles nil)))
      (unless (listp heading_titles)
        (mcp-server-lib-tool-throw "heading_titles must be a list of strings"))
      (dolist (title heading_titles)
        (unless (stringp title)
          (mcp-server-lib-tool-throw "All heading_titles must be strings"))))
    (let ((result (org-notebook-mcp--get-analytical-review file_path heading_titles)))
      (json-encode result))))

(defun emacs-mcp--get-heading-content (file_path heading_title &optional include_content include_code include_results include_subtree)
  "Extract content, code, and/or results from a specific org heading.
MCP Parameters:
  file_path - Absolute path to the org file to analyze
  heading_title - The title of the heading to extract content from
  include_content - Whether to include text content (boolean, optional)
  include_code - Whether to include code blocks (boolean, optional)
  include_results - Whether to include results blocks (boolean, optional)
  include_subtree - Whether to include entire subtree (boolean, optional)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (emacs-mcp--validate-string heading_title "heading_title")
    (let ((result (org-notebook-mcp--get-heading-content
                   file_path heading_title include_content include_code include_results include_subtree)))
      (json-encode result))))

(defun emacs-mcp--get-functions-from-org-file (file_path)
  "Extract all functions from code blocks in an org file.
MCP Parameters:
  file_path - Absolute path to the org file to analyze"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (let ((result (org-notebook-mcp--get-functions-from-org-file file_path)))
      (json-encode result))))

;;; Jupyter REPL MCP Tool Functions

(defun emacs-mcp--list-jupyter-repls ()
  "List all available Jupyter REPL clients.
MCP Parameters: None"
  (mcp-server-lib-with-error-handling
    (let ((result (org-notebook-mcp--list-jupyter-repls)))
      (json-encode result))))

(defun emacs-mcp--send-code-to-jupyter-repl (client_buffer_name code &optional timeout)
  "Send code to a Jupyter REPL client and get execution results.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to send code to (string, required)
  code - Code to execute in the REPL (string, required)
  timeout - Maximum time to wait for execution in seconds (integer, optional)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string client_buffer_name "client_buffer_name")
    (emacs-mcp--validate-string code "code")
    (when (and timeout (not (integerp timeout)))
      (mcp-server-lib-tool-throw "Invalid timeout: must be an integer or null"))
    (let ((result (org-notebook-mcp--send-code-to-repl client_buffer_name code timeout)))
      (json-encode result))))

(defun emacs-mcp--get-jupyter-repl-status (client_buffer_name)
  "Get the status of a Jupyter REPL client.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string client_buffer_name "client_buffer_name")
    (let ((result (org-notebook-mcp--get-repl-status client_buffer_name)))
      (json-encode result))))

(defun emacs-mcp--get-jupyter-kernel-state (client_buffer_name)
  "Get the execution state of a Jupyter kernel to check if it's busy.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string client_buffer_name "client_buffer_name")
    (let ((result (org-notebook-mcp--get-kernel-state client_buffer_name)))
      (json-encode result))))

(defun emacs-mcp--send-code-to-jupyter-repl-async (client_buffer_name code)
  "Send code to Jupyter REPL asynchronously for long-running computations.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to send code to (string, required)
  code - Code to execute in the REPL (string, required)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string client_buffer_name "client_buffer_name")
    (emacs-mcp--validate-string code "code")
    (let ((result (org-notebook-mcp--send-code-async client_buffer_name code)))
      (json-encode result))))

(defun emacs-mcp--check-async-execution (request_id)
  "Check the status of an async execution request and return results if complete.
MCP Parameters:
  request_id - The request ID returned from send-code-to-jupyter-repl-async (string, required)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string request_id "request_id")
    (org-notebook-mcp--check-async-execution-mcp request_id)))

(defun emacs-mcp--get-jupyter-last-output (client_buffer_name &optional lines)
  "Get the last output from a Jupyter REPL buffer.
MCP Parameters:
  client_buffer_name - Name of the REPL buffer to read from (string, required)
  lines - Number of lines to retrieve (integer, optional)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string client_buffer_name "client_buffer_name")
    (when (and lines (not (integerp lines)))
      (mcp-server-lib-tool-throw "Invalid lines: must be an integer or null"))
    (let ((result (org-notebook-mcp--get-last-output client_buffer_name lines)))
      (json-encode result))))

;;; Org-tools MCP Tool Functions

(defun emacs-mcp--get-org-file-headings (file_path &optional level parent_heading include_content_stats)
  "Get org headings from a file with navigation metadata.
MCP Parameters:
  file_path - Absolute path to the org file (string, required)
  level - Optional level to filter (1, 2, 3, etc.)
  parent_heading - Optional parent heading to filter under
  include_content_stats - Whether to include content statistics (default t)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (when (and level (not (integerp level)))
      (mcp-server-lib-tool-throw "Invalid level: must be an integer or null"))
    (when (and parent_heading (not (stringp parent_heading)))
      (mcp-server-lib-tool-throw "Invalid parent_heading: must be a string or null"))
    (let ((result (org-mcp--get-org-file-headings file_path level parent_heading include_content_stats)))
      (json-encode result))))

;;; Org-agenda MCP Tool Functions

(defun emacs-mcp--add-todo-with-refile (title refile_target &optional priority content scheduled)
  "Add a TODO item with full org-mode capabilities and optional refiling.
MCP Parameters:
  title - The TODO item title/heading text (string, required)
  refile_target - Target heading path like 'Life/Financial Independence' (string, optional)
  priority - TODO priority level: 'A', 'B', or 'C' (string, optional)
  content - Body content under the heading supporting full org-mode syntax (string, optional)
  scheduled - Optional scheduled date in org format (string, optional)"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string title "title")
    (let ((result (org-agenda-mcp--add-todo-with-refile title refile_target priority content scheduled)))
      (json-encode result))))

(defun emacs-mcp--get-available-locations (&optional level parent)
  "Get list of available locations for refiling in the personal agenda.

MCP Parameters:
  level - Optional integer to filter by heading level (1=top-level, 2=second-level, etc.)
  parent - Optional string to filter by parent heading (e.g., 'Life' to show Life/* sections)"
  (mcp-server-lib-with-error-handling
    (when (and level (not (integerp level)))
      (mcp-server-lib-tool-throw "Invalid level: must be an integer or null"))
    (when (and parent (not (stringp parent)))
      (mcp-server-lib-tool-throw "Invalid parent: must be a string or null"))
    (let ((result (org-agenda-mcp--get-available-locations level parent)))
      (json-encode result))))

;;; Org-agenda MCP Registration

(defun emacs-org-agenda-mcp-enable ()
  "Enable the Org-agenda MCP tools."
    (mcp-server-lib-register-tool
   :function #'emacs-mcp--add-todo-with-refile
   :name "add-todo-with-refile"
   :description "Create advanced TODO items with full org-mode capabilities and hierarchical organization.
This powerful tool enables creation of complex, structured TODO items with priorities, scheduling,
nested sub-tasks, properties, and rich content - all properly organized in your agenda hierarchy.

CORE CAPABILITIES:
- **Hierarchical Structure**: Create main TODO with nested sub-tasks
- **Priority Management**: Set priority levels (A=urgent, B=important, C=someday)
- **Rich Content**: Support for all org-mode syntax in content body
- **Smart Organization**: Automatic placement in appropriate agenda sections
- **Scheduling**: Flexible date/time scheduling with org-mode formats

PARAMETERS:
  title - Main TODO heading text (string, required)
          Clean, actionable description - prefix '* TODO' added automatically
          Examples: 'Plan birthday party', 'Review quarterly finances'

  refile_target - Target agenda section (string, optional)
                  Format: 'Life/Section Name' - if omitted, goes to 'Life/Misc'
                  Available: Life/Family Goals, Life/House, Life/Financial Independence, etc.

  priority - Priority level (string, optional)
             Values: 'A' (urgent), 'B' (important), 'C' (someday)
             Displays as [#A], [#B], [#C] in org-mode

  content - Body content with full org-mode support (string, optional)
            **Nested TODOs**: Use '** TODO subtask' for hierarchical tasks
            **Properties**: ':PROPERTIES:\n:EFFORT: 2h\n:CONTEXT: work\n:END:'
            **Scheduling**: 'SCHEDULED: <2025-09-01 Mon>', 'DEADLINE: <2025-09-15>'
            **Lists**: '- [ ] checklist item', '1. numbered item'
            **Links**: '[[https://example.com][Description]]'
            **Code**: '#+BEGIN_SRC python\nprint(\"hello\")\n#+END_SRC'
            **Tables**: '| Column1 | Column2 |\n|---------|---------|'
            **Text markup**: '*bold*, /italic/, =code=, ~verbatim~'

  scheduled - Main TODO scheduling (string, optional)
              Formats: '<2025-09-01>', '<2025-09-01 Mon>', '<2025-09-01 10:00>'
              Can also be included in content body

EXAMPLE USAGES:

1. **Simple Task**:
   title: \"Buy groceries\"
   refile_target: \"Life/House\"
   priority: \"B\"

2. **Complex Project with Subtasks**:
   title: \"Plan birthday party\"
   refile_target: \"Life/Family Goals\"
   priority: \"A\"
   content: \"* TODO Book venue\n* TODO Send invitations\n* TODO Order cake\"
   scheduled: \"2025-09-01\"

3. **Work Task with Properties**:
   title: \"Quarterly review preparation\"
   refile_target: \"Life/Financial Independence\"
   priority: \"A\"
   content: \":PROPERTIES:\n:EFFORT: 4:00\n:CONTEXT: finance\n:END:\n\n* TODO Gather Q3 statements\n* TODO Update spreadsheet\n* TODO Schedule meeting\n\nFocus areas:\n- Investment performance\n- Budget variance analysis\"
   scheduled: \"2025-09-15\"

4. **Research Task with Links**:
   title: \"Research vacation destinations\"
   refile_target: \"Life/Family Goals\"
   priority: \"C\"
   content: \"* TODO Check [[https://booking.com][Booking.com]] prices\n* TODO Read travel blogs\n\nBudget: $3000\nDuration: 1 week\"

5. **Learning Project**:
   title: \"Master Python data analysis\"
   refile_target: \"Study/Courses\"
   priority: \"B\"
   content: \":PROPERTIES:\n:COURSE: Python for Data Science\n:DEADLINE: <2025-12-31>\n:END:\n\n* TODO Complete pandas tutorial\nSCHEDULED: <2025-09-10>\n* TODO Practice with real dataset\n* TODO Build portfolio project\n\n#+BEGIN_SRC python\n# Practice code snippets\nimport pandas as pd\n#+END_SRC\"

RETURNS:
  success - Operation success status (boolean)
  title - Main TODO title (string)
  priority - Applied priority level (string)
  content - Body content as processed (string)
  scheduled - Applied scheduling (string)
  location - Final placement location (string)
  file - Agenda file path (string)
  template_used - Capture template used (string)
  refiled - Whether item was moved from default location (boolean)

ORGANIZATIONAL WORKFLOW:
1. Item initially created in 'Life/Misc' default location
2. If refile_target specified, automatically moved to target section
3. Nested headings properly adjusted for hierarchy (** becomes ****)
4. All org-mode syntax preserved and functional

ERROR HANDLING:
- Validates all parameters before processing
- Provides helpful error messages with available targets
- Graceful fallback to default location if target not found
- Maintains data integrity throughout process

SECURITY: Safe operation - only modifies personal agenda file with validated input"
   :args '((:name "title"
            :type string
            :description "Main TODO heading text - clear, actionable description")
           (:name "refile_target"
            :type string
            :description "Target agenda section like 'Life/Financial Independence' - omit for default 'Life/Misc'"
            :optional t)
           (:name "priority"
            :type string
            :description "Priority level: 'A' (urgent), 'B' (important), 'C' (someday)"
            :optional t)
           (:name "content"
            :type string
            :description "Body content with full org-mode syntax: nested TODOs, properties, lists, links, etc."
            :optional t)
           (:name "scheduled"
            :type string
            :description "Scheduling date in org format: '<2025-09-01>' or '<2025-09-01 Mon 10:00>'"
            :optional t))
   :read-only nil)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-available-locations
   :name "get-available-agenda-locations"
   :description "Get list of available locations for organizing tasks in your personal agenda.
Discover available sections and subsections where TODO items can be placed with hierarchical filtering.

This tool provides a view of your agenda structure with optional filtering by level or parent
heading. Essential for understanding where tasks can be organized and for providing context
when refiling items. Now supports efficient hierarchical navigation to reduce token costs.

Parameters (all optional):
  level - Filter by heading level (integer, optional)
          1 = top-level headings only (Life, Study, Research, Projects, Emacs)
          2 = second-level headings only (Life/Misc, Study/Books, etc.)
          3 = third-level headings only
  parent - Filter by parent heading (string, optional)
           e.g., 'Life' shows only Life/* sections
           e.g., 'Study' shows only Study/* sections

Returns JSON object with:
  file - Path to your personal agenda file (string)
  total_targets - Total number of available refile locations after filtering (integer)
  level_filter - Applied level filter (integer or null)
  parent_filter - Applied parent filter (string or null)
  available_locations - Array of location objects, each containing:
    heading - Full heading path (string, e.g., 'Life/Financial Independence')
    file - File containing this heading (string)
    buffer_position - Character position in file (integer)
    line_number - Line number where heading is located (integer, 1-based)

Usage examples:
- No parameters: Returns all available locations (may be large)
- level=1: Returns only top-level sections (Life, Study, Research, Projects, Emacs)
- parent='Life': Returns all Life subsections (Life/Misc, Life/House, etc.)
- level=2, parent='Life': Returns only second-level Life sections

Hierarchical navigation workflow:
1. Call with level=1 to see top-level sections
2. Call with parent='Life' to explore Life subsections
3. Use specific paths in refile operations

Common available locations by category:
- Life/*: Personal life areas (Misc, House, Financial Independence, etc.)
- Study/*: Learning content (Books, Courses, Articles, Papers, Videos)
- Research/*: Academic work (Writing Papers, Research experiments)
- Projects/*: Personal projects (Blog, ML, Writing, Music, etc.)
- Emacs/*: Emacs configuration and improvements

Use cases:
- Efficient hierarchical browsing of agenda structure
- Choosing appropriate location for new TODO items
- Exploring available categories by parent section
- Reducing token costs with targeted filtering
- Getting context for refile operations

Integration:
- Works with your existing org-refile-targets configuration
- Respects your current agenda file structure
- Updates dynamically as you modify your agenda hierarchy
- Compatible with standard org-mode refile functionality

Security: Read-only operation, safe for exploring agenda structure"
   :args '((:name "level"
            :type integer
            :description "Filter by heading level (1=top-level, 2=second-level, etc.)"
            :optional t)
           (:name "parent"
            :type string
            :description "Filter by parent heading (e.g., 'Life' to show Life/* sections)"
            :optional t))
   :read-only t))

(defun emacs-org-agenda-mcp-disable ()
  "Disable the Org-agenda MCP tools."
  (mcp-server-lib-unregister-tool "add-todo-simple")
  (mcp-server-lib-unregister-tool "add-todo-with-refile")
  (mcp-server-lib-unregister-tool "get-available-agenda-locations"))

;;; Org-notebook MCP Registration

(defun emacs-org-notebook-mcp-enable ()
  "Enable the Org-notebook MCP tools."
  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-analytical-review
   :name "get-org-analytical-review"
   :description "Extract headings, content, and results for analytical review of org notebooks.
Designed specifically for LLM analytical review focusing on approaches and outcomes, excluding code implementation details.

This tool provides a clean, review-focused view of org notebooks by extracting heading titles, descriptive content,
and execution results while omitting code blocks. Perfect for understanding methodology, findings, and conclusions
without getting distracted by implementation details.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file
             Use get-org-file-headings to discover available headings first
  heading_titles - Specific headings to extract (array of strings, optional)
                  If omitted, extracts all headings in the notebook
                  Uses substring matching to find headings
                  Examples: [\"Introduction\", \"Results\", \"Conclusion\"]

Returns JSON object with:
  file_path - The org file that was analyzed (string)
  extraction_mode - \"full_notebook\" or \"selected_headings\" (string)
  requested_headings - List of requested heading titles (array of strings)
  total_sections - Number of sections extracted (integer)
  sections - Array of section objects, each containing:
    heading - The heading title (string)
    level - Org heading level (integer, 1-6)
    content - Descriptive text content, excluding code blocks (string or null if truncated)
    results - Execution results and outputs (string or null if truncated)
    content_length - Character count of content (integer)
    results_length - Character count of results (integer)
    content_truncated - Whether content exceeded token limit (boolean)
    results_truncated - Whether results exceeded token limit (boolean)

Focus Areas for Analytical Review:
- **Methodology**: Approach descriptions, experimental design, data processing steps
- **Findings**: Analysis outcomes, statistical results, visualizations outputs
- **Insights**: Interpretations, conclusions, recommendations
- **Context**: Background information, assumptions, limitations

Content Exclusions:
- Source code blocks (#+BEGIN_SRC...#+END_SRC)
- Implementation details and programming syntax
- Technical configuration and setup code

Token Management:
- Content exceeding ~4k tokens (16,000 characters) per section is truncated
- Length information provided even when content is truncated
- Truncation flags indicate when content was omitted for size

Use Cases:
- High-level review of analytical workflows and findings
- Understanding research methodology without implementation details
- Extracting key insights and conclusions from computational notebooks
- Generating executive summaries of data analysis projects
- Academic review of research approaches and results
- Quality assessment of analytical documentation

Integration:
- Use get-org-file-headings first to discover available sections
- Combine with get-org-heading-content for detailed code inspection when needed
- Perfect complement to function extraction tools for comprehensive review

Error cases:
- File not found or not accessible
- File is not a valid .org file
- Requested headings not found in the file
- Invalid parameter types

Security: Read-only operation, safe for file analysis"
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the .org file")
           (:name "heading_titles"
            :type array
            :description "Optional list of specific heading titles to extract (uses substring matching)"
            :optional t))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-heading-content
   :name "get-org-heading-content"
   :description "Extract content, code, and/or results from a specific org heading with configurable options.
Provides flexible access to different components of org headings with intelligent length management for LLM consumption.

This tool allows selective extraction of text content, code blocks, and execution results from org headings,
with options to include entire subtrees or just direct content. Features automatic truncation handling
for large content that exceeds LLM token limits.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file
  heading_title - Title of the heading to extract from (string, required)
                 Uses substring matching to find the heading
                 Use get-org-file-headings to discover available headings
  include_content - Whether to include text content (boolean, optional, default true)
                   Text content excludes code blocks and results
  include_code - Whether to include code blocks (boolean, optional, default true)
                Includes #+BEGIN_SRC...#+END_SRC blocks with their content
  include_results - Whether to include results blocks (boolean, optional, default true)
                   Includes #+RESULTS: blocks and their output
  include_subtree - Whether to include entire subtree (boolean, optional, default false)
                   true: includes all sub-headings and their content
                   false: only direct content under the heading

Returns JSON object with:
  content - Text content if requested and under token limit (string or null)
  code - Code blocks if requested and under token limit (string or null)
  results - Results blocks if requested and under token limit (string or null)
  content_length - Character count of content component (integer)
  code_length - Character count of code component (integer)
  results_length - Character count of results component (integer)
  content_truncated - Whether content was truncated due to size (boolean)
  code_truncated - Whether code was truncated due to size (boolean)
  results_truncated - Whether results were truncated due to size (boolean)

Token Management:
- Content exceeding ~4k tokens (16,000 characters) is truncated
- Returns length information even when content is truncated
- Allows selective inclusion of components to manage token usage
- Provides truncation flags to indicate when content was omitted

Component Definitions:
- Content: Regular text, lists, tables, formatting - excludes code and results
- Code: Source blocks (#+BEGIN_SRC...#+END_SRC) with all their content
- Results: Output blocks (#+RESULTS:) and their associated data/output

Use cases:
- Selective extraction of notebook components for analysis
- Token-aware content retrieval for LLM processing
- Focused exploration of specific heading content
- Code and results analysis with content separation
- Subtree extraction for comprehensive section analysis

Error cases:
- File not found or not accessible
- File is not a valid .org file
- Heading not found in the file
- Invalid parameter types

Security: Read-only operation, safe for file analysis"
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the .org file")
           (:name "heading_title"
            :type string
            :description "Title of the heading to extract content from")
           (:name "include_content"
            :type boolean
            :description "Whether to include text content (default true)"
            :optional t)
           (:name "include_code"
            :type boolean
            :description "Whether to include code blocks (default true)"
            :optional t)
           (:name "include_results"
            :type boolean
            :description "Whether to include results blocks (default true)"
            :optional t)
           (:name "include_subtree"
            :type boolean
            :description "Whether to include entire subtree (default false)"
            :optional t))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-functions-from-org-file
   :name "get-functions-from-org-file"
   :description "Extract all functions from code blocks in an org file with location and docstring information.
Analyzes all source blocks in an org file and identifies function definitions across multiple programming languages.

This tool provides comprehensive function discovery within org notebooks, essential for code analysis,
navigation, and understanding the structure of computational documents. Supports multiple programming
languages and extracts metadata about each function including location and documentation hints.

Parameters:
  file_path - Absolute path to the .org file (string, required)
             Must exist and be a valid org file with source blocks

Returns JSON object with:
  file_path - The org file that was analyzed (string)
  total_code_blocks - Total number of source blocks found (integer)
  total_functions - Total number of functions extracted (integer)
  functions - Array of function objects, each containing:
    name - Function name (string)
    type - Function type (def, defun, function, etc.) (string)
    language - Programming language of the source block (string)
    line_number - Line number where function appears (integer, 1-based)
    buffer_position - Character position in file (integer)
    docstring_preview - Beginning of docstring if found (string)
    source_line - The actual source line containing the function definition (string)

Supported languages:
- Python: def functions
- Emacs Lisp: defun, defmacro, defvar, defcustom
- JavaScript/TypeScript: function declarations, const functions, methods

Analysis capabilities:
- Detects function definitions using language-specific patterns
- Extracts inline docstring hints where available
- Provides precise location information for navigation
- Handles multiple programming languages in single document
- Processes all source blocks systematically

Use cases:
- Code navigation and exploration in org notebooks
- Function inventory and documentation analysis
- Understanding code structure in computational documents
- Supporting code refactoring and maintenance
- Generating function indices for large notebooks

Performance considerations:
- Parses entire file content for comprehensive analysis
- Regex-based pattern matching for function detection
- Memory usage scales with file size and function count

Error cases:
- File not found or not accessible
- File is not a valid .org file
- No source blocks found in file
- Malformed source block syntax

Security: Read-only operation, safe for file analysis"
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the .org file to analyze"))
   :read-only t)

  ;; Jupyter REPL tools
  (mcp-server-lib-register-tool
   :function #'emacs-mcp--list-jupyter-repls
   :name "list-jupyter-repls"
   :description "List all available Jupyter REPL clients with their kernel information.
Essential for discovering active REPL connections that can be used for code execution.

This tool scans all active Jupyter clients in Emacs and returns information about their
connection status, kernel type, language, and execution state. Critical for LLMs to
understand what computational environments are available for interaction.

Parameters: None

Returns JSON object with:
  total_clients - Number of available REPL clients (integer)
  clients - Array of client objects, each containing:
    client_id - Unique identifier for the client (string)
    buffer_name - Name of the REPL buffer (string) - use this for other operations
    kernel_name - Type of kernel (e.g., 'python3', 'julia-1.8') (string)
    language - Programming language (e.g., 'python', 'julia') (string)
    language_version - Version of the language (string)
    status - Connection status ('connected' or 'disconnected') (string)
    execution_count - Number of executed cells/commands (integer)

Use cases:
- Discover available computational environments before sending code
- Choose appropriate REPL for specific language/kernel requirements
- Monitor active Jupyter sessions and their states
- Provide users with list of available kernels for selection
- Integration with org-mode notebook workflows

Integration workflow:
1. Use this tool to discover available REPLs
2. Use send-code-to-jupyter-repl to execute code in chosen REPL
3. Use get-jupyter-kernel-state to monitor execution progress
4. Use get-jupyter-last-output to retrieve results

Security: Read-only operation, safe for system discovery"
   :args '()
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--send-code-to-jupyter-repl
   :name "send-code-to-jupyter-repl"
   :description "Send code to a Jupyter REPL client and get execution results synchronously.
Executes code in a running Jupyter kernel and waits for completion with timeout.

This tool provides synchronous code execution in Jupyter kernels with comprehensive
result capture including output, results, and errors. Ideal for quick computations
and interactive development workflows.

Parameters:
  client_buffer_name - Name of the REPL buffer (string, required)
                      Use list-jupyter-repls to discover available buffer names
  code - Code to execute in the kernel (string, required)
        Should be valid code in the kernel's language (Python, Julia, etc.)
  timeout - Maximum wait time in seconds (integer, optional, default 30)
           Increase for long-running computations

Returns JSON object with:
  client_buffer - REPL buffer where code was executed (string)
  code - The code that was executed (string)
  success - Whether execution completed without errors (boolean)
  output - Standard output from the execution (string)
  result - Return value or computed result (string)
  error - Error message if execution failed (string or null)
  execution_time - When the execution completed (timestamp string)

Execution behavior:
- Waits synchronously for completion up to timeout
- Captures stdout, stderr, return values, and display outputs
- Handles both successful execution and error cases
- Maintains kernel state and execution count
- Supports all Jupyter-compatible languages

Use cases:
- Quick calculations and data analysis
- Interactive code development and testing
- Automated notebook execution workflows
- Code validation and testing
- Educational and demonstration purposes

Best practices:
- Use shorter timeout for quick operations
- For long computations, consider send-code-to-jupyter-repl-async
- Check kernel state with get-jupyter-kernel-state before sending
- Handle both success and error cases in results

Error cases:
- REPL buffer not found or not connected
- Code syntax errors or runtime exceptions
- Timeout exceeded for long-running code
- Kernel unavailable or crashed

Security: Code execution capability - only use with trusted input"
   :args '((:name "client_buffer_name"
            :type string
            :description "Name of the REPL buffer to send code to")
           (:name "code"
            :type string
            :description "Code to execute in the REPL")
           (:name "timeout"
            :type integer
            :description "Maximum time to wait for execution in seconds (default 30)"
            :optional t))
   :read-only nil)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-jupyter-kernel-state
   :name "get-jupyter-kernel-state"
   :description "Get the current execution state of a Jupyter kernel to check if it's busy.
Essential for LLMs to know when kernels are ready for new code or still processing.

This tool provides real-time information about kernel execution state, helping LLMs
make intelligent decisions about when to send code and when to wait. Critical for
handling long-running computations and avoiding race conditions.

Parameters:
  client_buffer_name - Name of the REPL buffer to check (string, required)
                      Use list-jupyter-repls to discover available buffer names

Returns JSON object with:
  buffer_name - REPL buffer that was checked (string)
  connected - Whether kernel is connected (boolean)
  kernel_state - Current execution state (string):
                'idle' - Ready for new code
                'busy' - Currently executing code
                'pending' - Has queued requests
                'disconnected' - Not connected
  busy - Whether kernel is currently executing (boolean)
  pending_requests - Number of requests waiting in queue (integer)
  execution_count - Total number of executed commands (integer)
  last_check - Timestamp of this state check (string)
  recommendations - Suggested action based on current state (string)

State meanings:
- 'idle': Kernel ready, safe to send new code immediately
- 'busy': Kernel executing, wait before sending more code
- 'pending': Requests queued, consider waiting for completion
- 'disconnected': Kernel unavailable, cannot execute code

LLM workflow patterns:
1. Check kernel state before sending code
2. If busy/pending, wait and poll until idle
3. Send code when kernel is idle
4. For async operations, poll state to detect completion
5. Handle disconnected state by reconnecting or choosing different kernel

Use cases:
- Prevent sending code to busy kernels
- Implement polling loops for async execution monitoring
- Provide user feedback about computation progress
- Handle long-running analysis workflows gracefully
- Coordinate multiple code execution requests

Best practices:
- Always check state before sending code to busy kernels
- Implement exponential backoff when polling busy kernels
- Provide informative messages to users about wait reasons
- Consider kernel capacity when queuing multiple requests

Error cases:
- REPL buffer not found
- Kernel connection issues
- Invalid buffer name provided

Security: Read-only operation, safe for monitoring"
   :args '((:name "client_buffer_name"
            :type string
            :description "Name of the REPL buffer to check"))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--check-async-execution
   :name "check-async-execution"
   :description "Check the status of an async execution request and return results if complete.
Essential for monitoring completion of async code execution and retrieving results.

This tool checks the completion status of code submitted via send-code-to-jupyter-repl-async
and returns results when execution completes. Uses request tracking system for reliable
result retrieval without buffer reading issues.

Parameters:
  request_id - The request ID returned from send-code-to-jupyter-repl-async (string, required)
             Must be a valid request ID from the async tracking system

Returns JSON object with:
  request_id - The request ID that was checked (string)
  client_buffer - REPL buffer where code was executed (string)
  code - The original code that was executed (string)
  status - Execution status: 'pending' or 'completed' (string)

If status is 'pending':
  start_time - When execution started (timestamp string)
  elapsed_time - How long execution has been running (string)
  message - Status message for user (string)

If status is 'completed':
  success - Whether execution completed without errors (boolean)
  output - Standard output from execution (string)
  result - Return value or computed result (string)
  error - Error message if execution failed (string or null)
  start_time - When execution started (timestamp string)
  completion_time - When execution finished (timestamp string)
  execution_duration - Total execution time (string)

Async execution workflow:
1. Send code with send-code-to-jupyter-repl-async to get request_id
2. Poll this function with request_id until status becomes 'completed'
3. Process the results from the completed execution
4. Results are cached - subsequent calls with same request_id return cached results

Use cases:
- Monitor long-running model training or data processing
- Check completion of complex mathematical computations
- Handle async execution in workflows where LLM needs to continue other tasks
- Reliable result retrieval without buffer reading crashes

Error cases:
- Invalid or unknown request_id
- Request tracking system issues
- Kernel connection problems during execution

Security: Read-only operation for checking execution status"
   :args '((:name "request_id"
            :type string
            :description "The request ID returned from send-code-to-jupyter-repl-async"))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--send-code-to-jupyter-repl-async
   :name "send-code-to-jupyter-repl-async"
   :description "Send code to Jupyter REPL asynchronously without waiting for results.
Ideal for long-running computations where LLMs need to poll for completion status.

This tool sends code for execution and returns immediately with request information.
LLMs should then use check-async-execution to monitor progress and retrieve results when computation completes.

Parameters:
  client_buffer_name - Name of the REPL buffer (string, required)
                      Use list-jupyter-repls to discover available buffer names
  code - Code to execute in the kernel (string, required)
        Should be valid code in the kernel's language

Returns JSON object with:
  client_buffer - REPL buffer where code was sent (string)
  code - The code that was submitted (string)
  request_id - Unique identifier for this execution request (string)
  status - Request status ('sent') (string)
  timestamp - When the request was sent (string)
  message - Instructions for monitoring completion (string)

Async execution workflow for LLMs:
1. Send code using this function
2. Poll get-jupyter-kernel-state until kernel_state becomes 'idle'
3. Use get-jupyter-last-output to retrieve results
4. Handle any errors found in the output

Polling strategy:
- Start with 1-second intervals
- Increase to 2-3 seconds for longer operations
- Stop polling when kernel_state is 'idle'
- Set reasonable maximum poll time based on expected computation time

Use cases:
- Long data processing and analysis tasks
- Machine learning model training
- Large dataset operations
- Complex mathematical computations
- Any operation that might take more than 30 seconds

Advantages over synchronous execution:
- No timeout limitations
- LLM can provide progress updates to user
- Can handle very long computations gracefully
- Allows for better resource management

Best practices:
- Always implement polling loop after async send
- Provide user feedback during long computations
- Set reasonable maximum wait times
- Handle cases where computation fails or hangs

Error cases:
- REPL buffer not found or not connected
- Kernel unavailable
- Code submission failure

Security: Code execution capability - only use with trusted input"
   :args '((:name "client_buffer_name"
            :type string
            :description "Name of the REPL buffer to send code to")
           (:name "code"
            :type string
            :description "Code to execute in the REPL"))
   :read-only nil)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-jupyter-last-output
   :name "get-jupyter-last-output"
   :description "Get the last output from a Jupyter REPL buffer.
Essential for retrieving results after asynchronous code execution completes.

This tool extracts recent output from the REPL buffer, useful for getting results
after async execution or checking recent computation history. Provides flexible
control over how much output to retrieve.

Parameters:
  client_buffer_name - Name of the REPL buffer (string, required)
                      Use list-jupyter-repls to discover available buffer names
  lines - Number of lines to retrieve (integer, optional, default 50)
         Adjust based on expected output size

Returns JSON object with:
  buffer_name - REPL buffer that was read (string)
  lines_retrieved - Actual number of lines found (integer)
  max_lines - Maximum lines requested (integer)
  output - The retrieved output text (string)
  buffer_size - Total size of REPL buffer (integer)
  timestamp - When output was retrieved (string)

Output content:
- Includes execution results, printed output, error messages
- Shows prompts, execution counts, and formatted displays
- Preserves formatting and ANSI escape sequences
- May include rich output representations (plots, tables, etc.)

Use cases:
- Retrieve results after async code execution
- Get recent computation history
- Debug failed executions by examining error output
- Extract specific results from long output streams
- Monitor ongoing computation progress

Integration with async workflow:
1. Send code with send-code-to-jupyter-repl-async
2. Poll get-jupyter-kernel-state until execution completes
3. Use this function to retrieve the results
4. Parse output for specific results or error messages

Best practices:
- Request appropriate number of lines for expected output size
- Parse output to extract specific results when needed
- Handle cases where output might be empty
- Consider output size when processing large results

Parsing suggestions:
- Look for 'Out [n]:' patterns for execution results
- Check for error tracebacks and exception messages
- Extract specific data formats (JSON, CSV, etc.) from output
- Handle rich display outputs appropriately

Error cases:
- REPL buffer not found
- Empty or no recent output
- Invalid line count parameter

Security: Read-only operation, safe for output retrieval"
   :args '((:name "client_buffer_name"
            :type string
            :description "Name of the REPL buffer to read from")
           (:name "lines"
            :type integer
            :description "Number of lines to retrieve (default 50)"
            :optional t))
   :read-only t)

  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-jupyter-repl-status
   :name "get-jupyter-repl-status"
   :description "Get comprehensive status information about a Jupyter REPL client.
Provides detailed information about kernel connection, configuration, and current state.

This tool returns complete status information about a Jupyter REPL client including
kernel details, connection status, language information, and execution history.
Useful for understanding the computational environment and troubleshooting issues.

Panrameters:
  client_buffer_name - Name of the REPL buffer (string, required)
                      Use list-jupyter-repls to discover available buffer names

Returns JSON object with:
  buffer_name - REPL buffer name (string)
  connected - Whether kernel connection is active (boolean)
  kernel_name - Type/name of the kernel (string)
  kernel_id - Unique kernel identifier (string)
  language - Programming language (string)
  language_version - Version of the programming language (string)
  execution_count - Total number of executed commands (integer)
  buffer_size - Size of the REPL buffer in characters (integer)
  last_activity - Timestamp of last activity (string)
  kernel_alive - Whether kernel process is responsive (boolean)

Status information helps with:
- Verifying kernel connectivity before code execution
- Understanding available language features and versions
- Monitoring REPL session health and activity
- Troubleshooting execution issues
- Planning code execution strategies

Use cases:
- Pre-execution environment verification
- Debugging connection and execution issues
- Monitoring long-running kernel sessions
- Collecting system information for error reporting
- Understanding computational environment capabilities

Troubleshooting guide:
- connected=false: Kernel needs reconnection
- kernel_alive=false: Kernel process may have crashed
- High execution_count: Long-running session, may need restart
- Large buffer_size: Consider clearing buffer for performance

Best practices:
- Check status before starting complex computations
- Monitor kernel health during long-running operations
- Use information to provide helpful error messages
- Consider kernel capabilities when planning code execution

Error cases:
- REPL buffer not found
- Kernel connection issues
- Invalid buffer name

Security: Read-only operation, safe for status monitoring"
   :args '((:name "client_buffer_name"
            :type string
            :description "Name of the REPL buffer to check"))
   :read-only t))

(defun emacs-org-notebook-mcp-disable ()
  "Disable the Org-notebook MCP tools."
  (mcp-server-lib-unregister-tool "get-org-analytical-review")
  (mcp-server-lib-unregister-tool "get-org-heading-content")
  (mcp-server-lib-unregister-tool "get-functions-from-org-file")
  (mcp-server-lib-unregister-tool "list-jupyter-repls")
  (mcp-server-lib-unregister-tool "send-code-to-jupyter-repl")
  (mcp-server-lib-unregister-tool "get-jupyter-kernel-state")
  (mcp-server-lib-unregister-tool "send-code-to-jupyter-repl-async")
  (mcp-server-lib-unregister-tool "check-async-execution")
  (mcp-server-lib-unregister-tool "get-jupyter-last-output")
  (mcp-server-lib-unregister-tool "get-jupyter-repl-status"))

;;; Org-tools MCP Registration

(defun emacs-org-tools-mcp-enable ()
  "Enable the Org-tools MCP tools."
  (mcp-server-lib-register-tool
   :function #'emacs-mcp--get-org-file-headings
   :name "get-org-file-headings"
   :description "Get org headings from a file with navigation metadata for efficient file exploration.
Enables LLMs to navigate org files without loading full content, optimizing token usage.

This tool provides a hierarchical view of org file structure with rich metadata for each heading.
Essential for understanding file organization, locating specific sections, and enabling targeted
content access using line numbers. Supports filtering by level and parent heading for efficient
navigation of large files.

Parameters:
  file_path - Absolute path to the org file (string, required)
             Must exist and be a valid .org file
  level - Filter headings by specific level (integer, optional)
          1 = top-level headings, 2 = second-level, etc.
          If not specified, returns all headings
  parent_heading - Filter headings under a specific parent (string, optional)
                  e.g., 'Research' shows only headings under Research section
                  Supports partial matching for flexible navigation
  include_content_stats - Include content statistics (boolean, optional, default true)
                         When true, includes content_length and has_code_blocks metadata

Returns JSON object with:
  file_path - The org file that was analyzed (string)
  total_headings - Number of headings found after filtering (integer)
  level_filter - Applied level filter (integer or null)
  parent_filter - Applied parent filter (string or null)
  include_content_stats - Whether content statistics were included (boolean)
  headings - Array of heading objects, each containing:
    title - Heading text/title (string)
    level - Org heading level (integer, 1-6)
    line_number - Line number where heading starts (integer, 1-based)
    parent_path - Breadcrumb path to parent headings (string, e.g., 'Research > Methods')
    subheading_count - Number of direct child headings (integer)
    content_length - Character count of heading content (integer, if stats enabled)
    has_code_blocks - Whether heading contains source blocks (boolean, if stats enabled)

Navigation workflow:
1. Call without filters to get file overview
2. Use level=1 to see top-level structure
3. Use parent_heading to explore specific sections
4. Use line_number from results with Read tool for targeted content access

Use cases:
- Efficient org file exploration without token overhead
- Understanding document structure and organization
- Locating specific sections for targeted reading/editing
- Identifying content-rich sections (high content_length)
- Finding code-containing sections (has_code_blocks=true)
- Hierarchical navigation of large documents
- Supporting context-aware content access patterns

Performance benefits:
- Parses structure without loading full content
- Enables targeted Read tool usage with specific line numbers
- Reduces token consumption for large files
- Supports incremental exploration patterns

Content statistics (when enabled):
- content_length: Approximate character count of heading content
- has_code_blocks: Indicates presence of #+BEGIN_SRC blocks
- Helps prioritize which sections to examine in detail

Error cases:
- File not found or not accessible
- File is not a valid .org file
- Invalid level parameter (non-integer)
- Invalid parent_heading parameter (non-string)
- File parsing errors

Security: Read-only operation, safe for file analysis"
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the org file")
           (:name "level"
            :type integer
            :description "Filter headings by specific level (1=top-level, 2=second-level, etc.)"
            :optional t)
           (:name "parent_heading"
            :type string
            :description "Filter headings under a specific parent heading"
            :optional t)
           (:name "include_content_stats"
            :type boolean
            :description "Include content statistics (content_length, has_code_blocks)"
            :optional t))
   :read-only t))

(defun emacs-org-tools-mcp-disable ()
  "Disable the Org-tools MCP tools."
  (mcp-server-lib-unregister-tool "get-org-file-headings"))

;;; Server Management

;;;###autoload
(defun emacs-org-mode-mcp-enable ()
  "Enable all Emacs MCP tools (org-roam, org-babel, org-agenda, org-tools, and org-notebook)."
  (emacs-org-roam-mcp-enable)
  (emacs-org-babel-mcp-enable)
  (emacs-org-agenda-mcp-enable)
  (emacs-org-tools-mcp-enable)
  (emacs-org-notebook-mcp-enable)
  )

;;;###autoload
(defun emacs-org-mode-mcp-disable ()
  "Disable all Emacs MCP tools (org-roam, org-babel, org-agenda, org-tools, and org-notebook)."
  (emacs-org-roam-mcp-disable)
  (emacs-org-babel-mcp-disable)
  (emacs-org-agenda-mcp-disable)
  (emacs-org-tools-mcp-disable)
  (emacs-org-notebook-mcp-disable))

;; Start the server with all tools enabled
(emacs-org-mode-mcp-enable)
(mcp-server-lib-start)

(provide 'custom-emacs-mcp-server-settings)
;;; emacs-mcp-server-settings.el ends here
