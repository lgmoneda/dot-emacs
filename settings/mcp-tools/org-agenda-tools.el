;;; org-agenda-tools.el --- MCP tools for org-agenda and org-capture

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-refile)

;;; Utility Functions

(defun org-agenda-mcp--parse-content-for-scheduling (content)
  "Parse CONTENT to extract main text and scheduling information.
Returns (main-content . scheduled-date) or (content . nil) if no scheduling found.

Note: Use proper org-mode syntax for scheduling, e.g.:
- SCHEDULED: <2025-08-25 Sun>
- SCHEDULED: <2025-08-25>
- You can also include DEADLINE:, TODO states, priorities [#A], etc."
  (if (string-match "\\(.*?\\)\\(?:\n\\|\\s-\\)+SCHEDULED:\\s-*\\(\\S-+\\)" content)
      (let ((main-text (string-trim (match-string 1 content)))
            (scheduled-part (match-string 2 content)))
        (cons main-text scheduled-part))
    (cons content nil)))

(defun org-agenda-mcp--validate-file-path (file-path)
  "Validate that FILE-PATH exists and is an org file."
  (unless (stringp file-path)
    (error "File path must be a string"))
  (when (string-empty-p file-path)
    (error "Empty file path provided"))
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))
  (unless (string-match-p "\\.org$" file-path)
    (error "File must have .org extension")))

(defun org-agenda-mcp--adjust-heading-levels (content base-level)
  "Adjust heading levels in CONTENT to be relative to BASE-LEVEL.
For example, if BASE-LEVEL is 4 and content has '* TODO task',
it becomes '***** TODO task' (base-level + 1)."
  (when (and content (not (string-empty-p content)))
    (let ((lines (split-string content "\n" t)))
      (mapconcat
       (lambda (line)
         (if (string-match "^\\(\\*+\\)\\s-*\\(.*\\)" line)
             (let* ((stars (match-string 1 line))
                    (rest (match-string 2 line))
                    (original-level (length stars))
                    (new-level (+ base-level original-level))
                    (new-stars (make-string new-level ?*)))
               (format "%s %s" new-stars rest))
           line))
       lines
       "\n"))))

(defun build-refile-targets (&optional max-level)
  "Build list of all possible refile targets.
If MAX-LEVEL is specified, limit to that level."
  (let ((org-refile-targets (if max-level
                               `((org-agenda-files :maxlevel . ,max-level))
                             '((org-agenda-files :maxlevel . 9))))
        (org-refile-use-outline-path 'file))
    (org-refile-get-targets)))

(defun find-refile-target (target-path targets &optional default-file)
  "Find a refile target matching TARGET-PATH from TARGETS.
TARGET-PATH can be:
- 'filename.org/Parent/Child'
- 'Parent/Child'
- 'Heading'

If no filename is present, assume DEFAULT-FILE (defaults to todo.org)."
  (let* ((normalized (replace-regexp-in-string "^/+" "" (string-trim (or target-path ""))))
         (components (split-string normalized "/"))
         ;; Check if the first part looks like a file
         (has-file (and components (string-match-p "\\.org\\'" (car components))))
         (default-file (or default-file
                           (if (and (boundp 'org-directory) org-directory)
                               (file-name-nondirectory
                                (expand-file-name "todo.org" org-directory))
                             "todo.org")))
         ;; drop filename if present
         (search-comps (if has-file (cdr components) components))
         (raw-path (mapconcat #'identity search-comps "/"))
         (cand-with-default (unless has-file
                              (concat default-file "/" raw-path)))
         (cand-raw (if has-file normalized raw-path))
         best)

    ;; First try with default file (todo.org/...)
    (when cand-with-default
      (catch 'found
        (dolist (cand targets)
          (let* ((outline-path (car cand))
                 (path-str (mapconcat #'identity
                                      (if (listp outline-path)
                                          outline-path
                                        (list outline-path))
                                      "/")))
            (when (string= cand-with-default path-str)
              (setq best cand)
              (throw 'found cand))))))

    ;; If not found, fall back to raw path
    (unless best
      (catch 'found
        (dolist (cand targets)
          (let* ((outline-path (car cand))
                 (path-str (mapconcat #'identity
                                      (if (listp outline-path)
                                          outline-path
                                        (list outline-path))
                                      "/")))
            (when (string= cand-raw path-str)
              (setq best cand)
              (throw 'found cand))))))

    best))

(defun org-agenda-mcp--add-todo-with-refile (title refile_target &optional priority content scheduled)
  "Add a TODO item using org-capture and optionally refile to a specific location.
Uses org-capture-string and org-refile for robust handling of any heading level.

MCP Parameters:
  title - The TODO item title/heading text (string, required)
          This becomes the heading text after '* TODO'
  refile_target - Target heading path like 'Life/Financial Independence' (string, optional)
                  If not provided or empty, item goes to default 'Life/Misc' location
  priority - TODO priority level (string, optional)
             Valid values: 'A', 'B', 'C' (will be formatted as [#A], [#B], [#C])
  content - Body content under the heading (string, optional)
            Can contain any valid org-mode syntax including:
            - PROPERTIES drawer: ':PROPERTIES:\\n:CUSTOM_ID: abc\\n:END:'
            - SCHEDULED/DEADLINE: 'SCHEDULED: <2025-08-25 Sun>'
            - Plain text content, lists, tables, code blocks
            - Tags, links, emphasis markup
            - Nested headings (will be adjusted to proper level)
            - Logbook entries, clocking data
            - Any other org-mode elements
  scheduled - Optional scheduled date in org format (string, optional)
              Examples: '<2025-08-25 Sun>', '2025-08-25', '<2025-08-25 Sun 10:00>'
              This will be added to the content if not already present"
  (mcp-server-lib-with-error-handling
    (unless (stringp title)
      (error "Title must be a string"))
    (when (string-empty-p title)
      (error "Empty title provided"))

    ;; Validate priority if provided
    (when (and priority (not (string-empty-p priority)))
      (unless (member (upcase priority) '("A" "B" "C"))
        (error "Priority must be A, B, or C")))

    ;; Use content directly - no parsing needed since we have separate parameters
    (let* ((body-content content)
           (final-scheduled scheduled)
           (target-file (expand-file-name "~/Dropbox/Agenda/todo.org"))
           (final-location "Life/Misc")
           (refiled nil))

      ;; Validate target file
      (org-agenda-mcp--validate-file-path target-file)

      ;; Build the capture text with title, priority, scheduling, and content
      (let* ((priority-str (when (and priority (not (string-empty-p priority)))
                            (format " [#%s]" (upcase priority))))
             (scheduled-str (when (and final-scheduled (not (string-empty-p final-scheduled)))
                             (format "SCHEDULED: %s"
                                    (if (string-match-p "^<.*>$" final-scheduled)
                                        final-scheduled
                                      (format "<%s>" final-scheduled)))))
             ;; Build the full TODO entry structure
             (todo-parts (list (concat title (or priority-str ""))))
             ;; Add scheduled line if present
             (todo-parts (if scheduled-str
                           (append todo-parts (list scheduled-str))
                         todo-parts))
             ;; Add content lines if present
             (todo-parts (if (and body-content (not (string-empty-p body-content)))
                           (append todo-parts (list body-content))
                         todo-parts))
             ;; Join everything with newlines
             (capture-text (string-join todo-parts "\n")))

        ;; Debug: show what we're capturing
        ;; Insert TODO item directly to avoid org-capture-string multi-line issues
        (with-current-buffer (find-file-noselect target-file)
          (save-excursion
            ;; Find the Life/Misc section (default location)
            (goto-char (point-min))
            (if (re-search-forward "^\\*\\*\\* Misc$" nil t)
                (progn
                  ;; Go to end of the Misc section
                  (org-end-of-subtree t t)
                  (unless (bolp) (insert "\n"))
                  ;; Insert the main TODO item with proper format
                  (insert (format "**** TODO%s %s\n"
                                 (or priority-str "")
                                 title))
                  ;; Add scheduled line if present
                  (when scheduled-str
                    (insert scheduled-str "\n"))
                  ;; Add body content with proper heading adjustment
                  (when (and body-content (not (string-empty-p body-content)))
                    (let ((adjusted-content (org-agenda-mcp--adjust-heading-levels body-content 4)))
                      (insert adjusted-content "\n"))))
              ;; If Misc section not found, add at end of file
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (format "**** TODO%s %s\n"
                             (or priority-str "")
                             title))
              (when scheduled-str
                (insert scheduled-str "\n"))
              (when (and body-content (not (string-empty-p body-content)))
                (let ((adjusted-content (org-agenda-mcp--adjust-heading-levels body-content 4)))
                  (insert adjusted-content "\n"))))
          (save-buffer)))

      ;; If a refile target was specified and it's not the default, refile the item
      (when (and refile_target
                 (not (string-empty-p refile_target))
                 (not (string= refile_target "Life/Misc")))

        ;; Get available refile targets using working function
        (let* ((targets (build-refile-targets))
               (target-match (find-refile-target refile_target targets)))

          (if target-match
              (progn
                ;; Find the just-captured TODO item and refile it
                (with-current-buffer (find-file-noselect target-file)
                  (save-excursion
                    ;; Go to the end of the file and work backwards to find our newly added item
                    (goto-char (point-max))
                    ;; Search for the TODO item, accounting for priority
                    (let ((search-pattern (if (and priority (not (string-empty-p priority)))
                                            (format "^\\*\\*\\*\\* TODO \\[#%s\\] %s" (upcase priority) (regexp-quote title))
                                          (format "^\\*\\*\\*\\* TODO %s" (regexp-quote title)))))
                      (when (re-search-backward search-pattern nil t)
                        ;; Refile using org-refile with the found target
                        (org-refile nil nil target-match)
                        (setq final-location refile_target)
                        (setq refiled t))))))

            ;; If refile target not found, provide helpful error
            (let ((available-targets (mapcar (lambda (target)
                                              (mapconcat #'identity
                                                        (if (listp (car target))
                                                            (car target)
                                                          (list (car target)))
                                                        "/"))
                                            targets)))
              (error "Refile target '%s' not found. Available targets: %s"
                     refile_target
                     (string-join available-targets ", "))))))

      ;; Return success information
      `((success . t)
        (title . ,title)
        (priority . ,priority)
        (content . ,body-content)
        (scheduled . ,final-scheduled)
        (location . ,final-location)
        (file . ,target-file)
        (template_used . "D")
        (refiled . ,refiled))))))

(defun filter-targets-by-level-and-parent (targets level parent)
  "Filter TARGETS by optional LEVEL and PARENT heading."
  (let ((filtered targets))
    ;; Filter by level if specified
    (when level
      (setq filtered (cl-remove-if-not
                      (lambda (target)
                        (let* ((outline-path (car target))
                               (path-list (if (listp outline-path) outline-path (list outline-path)))
                               ;; Safe approach: work directly with the list, don't use "/" as delimiter
                               ;; Remove filename (first element) and count remaining elements
                               (heading-components (if (and (listp path-list) (> (length path-list) 1))
                                                     (cdr path-list)  ; Remove filename, keep headings
                                                   (if (listp path-list) path-list (list path-list))))
                               (actual-level (length heading-components)))
                          (= actual-level level)))
                      filtered)))

    ;; Filter by parent if specified
    (when parent
      (setq filtered (cl-remove-if-not
                      (lambda (target)
                        (let* ((outline-path (car target))
                               (path-list (if (listp outline-path) outline-path (list outline-path)))
                               (path-str (mapconcat #'identity path-list "/")))
                          (string-match-p (regexp-quote parent) path-str)))
                      filtered)))

    filtered))

(defun filter-targets-by-parent (targets parent)
  "Filter TARGETS by PARENT heading."
  (cl-remove-if-not
   (lambda (target)
     (let* ((outline-path (car target))
            (path-list (if (listp outline-path) outline-path (list outline-path)))
            ;; Join path but handle escaped slashes correctly
            (path-str (mapconcat #'identity path-list "/")))
       (string-match-p (regexp-quote parent) path-str)))
   targets))

(defun format-targets-for-mcp (targets)
  "Format refile targets for MCP response."
  (mapcar (lambda (target)
            (let* ((outline-path (car target))
                   (file (nth 1 target))
                   (buffer-pos (nth 3 target))
                   (path-list (if (listp outline-path) outline-path (list outline-path)))
                   (heading-path (mapconcat #'identity
                                           (if (> (length path-list) 1)
                                               (cdr path-list)  ; Remove filename
                                             path-list)
                                           "/"))
                   ;; Calculate line number from buffer position
                   (line-number (when (and file buffer-pos)
                                  (with-current-buffer (find-file-noselect file)
                                    (save-excursion
                                      (goto-char buffer-pos)
                                      (line-number-at-pos))))))
              `((heading . ,heading-path)
                (file . ,file)
                (buffer_position . ,buffer-pos)
                (line_number . ,line-number))))
          targets))

(defun org-agenda-mcp--get-available-locations (&optional level parent)
  "Get list of available locations for refiling in the personal agenda.
Returns the available refile targets from todo.org, with hierarchical filtering.

Optional parameters:
  level - Limit to specific heading level (1 for top-level, 2 for second-level, etc.)
  parent - Show only children of this parent heading (e.g., 'Life' to show Life/* sections)"
  (mcp-server-lib-with-error-handling
    (let ((target-file (expand-file-name "~/Dropbox/Agenda/todo.org")))
      (org-agenda-mcp--validate-file-path target-file)
      (with-current-buffer (find-file-noselect target-file)
        (let* ((targets (build-refile-targets level))  ; Use org-refile's built-in level filtering
               (filtered-targets (if parent
                                   (filter-targets-by-parent targets parent)
                                 targets))
               (formatted-targets (format-targets-for-mcp filtered-targets)))
          `((file . ,target-file)
            (total_targets . ,(length filtered-targets))
            (level_filter . ,level)
            (parent_filter . ,parent)
            (available_locations . ,formatted-targets)))))))


(provide 'org-agenda-tools)
;;; org-agenda-tools.el ends here
