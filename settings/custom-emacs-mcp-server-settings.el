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

(defun emacs-mcp--org-babel-execute-src-block (file_path &optional block_reference)
  "Execute a specific source block in an org file.
MCP Parameters:
  file_path - Path to the org file
  block_reference - Optional reference to the block to execute:
                   - String: Org heading title (e.g., 'EDA') - finds first src block under that heading
                   - Number: Line number where block starts
                   - If nil: Execute first source block found"
  (mcp-server-lib-with-error-handling
    (emacs-mcp--validate-string file_path "file_path")
    (when (and block_reference
               (not (null block_reference))
               (stringp block_reference)
               (string-empty-p block_reference))
      (mcp-server-lib-tool-throw "Empty block_reference provided"))
    (when (and block_reference (not (or (stringp block_reference) (numberp block_reference))))
      (mcp-server-lib-tool-throw "Invalid block_reference: must be a string (heading title) or number (line number)"))
    (let ((result (org-babel-mcp--execute-src-block file_path block_reference)))
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
  block_reference - Reference to the block to execute (string or number, optional)
                   - String: Org heading title (e.g., 'EDA') - finds first src block under that heading
                   - Number: Line number where block starts
                   - If not provided, executes the first source block found

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
           (:name "block_reference"
            :type string
            :description "Reference to the block: heading title (string) or line number (number)"
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
   :read-only nil))

(defun emacs-org-babel-mcp-disable ()
  "Disable the Org-babel MCP tools."
  (mcp-server-lib-unregister-tool "execute_src_block")
  (mcp-server-lib-unregister-tool "execute_buffer"))


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
  "Enable all Emacs MCP tools (org-roam, org-babel, org-agenda, and org-tools)."
  (emacs-org-roam-mcp-enable)
  (emacs-org-babel-mcp-enable)
  (emacs-org-agenda-mcp-enable)
  (emacs-org-tools-mcp-enable)
  )

;;;###autoload
(defun emacs-org-mode-mcp-disable ()
  "Disable all Emacs MCP tools (org-roam, org-babel, org-agenda, and org-tools)."
  (emacs-org-roam-mcp-disable)
  (emacs-org-babel-mcp-disable)
  (emacs-org-agenda-mcp-disable)
  (emacs-org-tools-mcp-disable))

;; Start the server with all tools enabled
(emacs-org-mode-mcp-enable)
(mcp-server-lib-start)

(provide 'custom-emacs-mcp-server-settings)
;;; emacs-mcp-server-settings.el ends here
