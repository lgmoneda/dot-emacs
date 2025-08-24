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

(defun org-agenda-mcp--get-refile-targets ()
  "Get available refile targets based on org-refile-targets configuration.
Returns a list of available headings for refiling."
  (let ((org-refile-cache nil)) ; Force refresh of refile cache
    (org-refile-get-targets)))

(defun org-agenda-mcp--format-refile-targets (targets)
  "Format refile TARGETS into a readable structure for LLM.
Each target is (heading . (file . position))."
  (mapcar (lambda (target)
            (let ((heading (car target))
                  (file-info (cdr target)))
              `((heading . ,heading)
                (file . ,(car file-info))
                (position . ,(cdr file-info)))))
          targets))

(defun org-agenda-mcp--find-refile-target (targets heading-path)
  "Find refile target in TARGETS that matches HEADING-PATH.
HEADING-PATH can be a string like 'Life/Misc' or 'Life/Financial Independence'."
  (let ((target-found nil))
    (dolist (target targets)
      (let ((heading (car target)))
        ;; Try exact match first, then partial match
        (when (or (string= heading-path heading)
                  (string-match-p (regexp-quote heading-path) heading))
          (setq target-found target))))
    target-found))

;;; MCP Tool Functions

(defun org-agenda-mcp--add-todo-simple (content &optional scheduled)
  "Add a TODO item to the default personal agenda location.
Directly adds to Life/Misc section in todo.org.

MCP Parameters:
  content - The TODO item content (string, required)
            Can include embedded org syntax like 'Task\\nSCHEDULED: <2025-08-25 Sun>'
            Support for DEADLINE:, priorities [#A], TODO states, etc.
  scheduled - Optional scheduled date in org format like '<2025-08-25 Sun>' or '2025-08-25'"
  (mcp-server-lib-with-error-handling
    (unless (stringp content)
      (error "Content must be a string"))
    (when (string-empty-p content)
      (error "Empty content provided"))

    ;; Parse content for embedded scheduling
    (let* ((parsed (org-agenda-mcp--parse-content-for-scheduling content))
           (main-content (car parsed))
           (embedded-scheduled (cdr parsed))
           (final-scheduled (or scheduled embedded-scheduled))
           (target-file (expand-file-name "~/Dropbox/Agenda/todo.org")))
      ;; Validate target file
      (org-agenda-mcp--validate-file-path target-file)

      ;; Add TODO item directly to Life/Misc section
      (with-current-buffer (find-file-noselect target-file)
        (save-excursion
          ;; Find Life section first, then Misc section
          (goto-char (point-min))
          (if (re-search-forward "^\\* Life" nil t)
              ;; Find Misc section within Life
              (if (re-search-forward "^\\*\\* Misc" nil t)
              (progn
                ;; Go to end of the Misc section
                (forward-line 1)
                ;; Find the end of this section (next heading at same or higher level)
                (let ((section-end (save-excursion
                                    (if (re-search-forward "^\\*\\{1,2\\} " nil t)
                                        (line-beginning-position)
                                      (point-max)))))
                  ;; Go to the end of the section content
                  (goto-char section-end)
                  (forward-line -1)
                  (end-of-line)
                  ;; Insert the new TODO item at proper hierarchy level (*** for level 3)
                  (let ((todo-text (format "\n*** TODO %s" main-content))
                        (schedule-text (when (and final-scheduled (not (string-empty-p final-scheduled)))
                                         (format "\nSCHEDULED: %s"
                                                (if (string-match-p "^<.*>$" final-scheduled)
                                                    final-scheduled
                                                  (format "<%s>" final-scheduled))))))
                    (insert (concat todo-text (or schedule-text "") "\n")))))
                (error "Could not find section 'Misc' under 'Life'"))
            (error "Could not find top-level section 'Life'")))

        ;; Save the file
        (save-buffer))

      ;; Return success information
      `((success . t)
        (content . ,content)
        (location . "Life/Misc")
        (file . ,target-file)
        (method . "direct insertion")))))

(defun org-agenda-mcp--add-todo-with-refile (content refile_target &optional scheduled)
  "Add a TODO item directly to a specific location in the agenda.
Finds the target section and adds the TODO there directly.

MCP Parameters:
  content - The TODO item content (string, required)
            Can include embedded org syntax like 'Task\\nSCHEDULED: <2025-08-25 Sun>'
            Support for DEADLINE:, priorities [#A], TODO states, etc.
  refile_target - Target heading path like 'Life/Financial Independence' (string, optional)
  scheduled - Optional scheduled date in org format like '<2025-08-25 Sun>' or '2025-08-25'"
  (mcp-server-lib-with-error-handling
    (unless (stringp content)
      (error "Content must be a string"))
    (when (string-empty-p content)
      (error "Empty content provided"))

    ;; Parse content for embedded scheduling
    (let* ((parsed (org-agenda-mcp--parse-content-for-scheduling content))
           (main-content (car parsed))
           (embedded-scheduled (cdr parsed))
           (final-scheduled (or scheduled embedded-scheduled))
           (target-file (expand-file-name "~/Dropbox/Agenda/todo.org"))
           (final-location "Life/Misc"))

      ;; Validate target file
      (org-agenda-mcp--validate-file-path target-file)

      ;; Determine target section
      (let ((target-section (if (and refile_target (not (string-empty-p refile_target)))
                                refile_target
                              "Life/Misc")))

        ;; Add TODO item directly to the target section
        (with-current-buffer (find-file-noselect target-file)
          (save-excursion
            ;; Parse target section (e.g., "Life/Financial Independence" -> find "Life" then "Financial Independence")
            (let* ((sections (split-string target-section "/"))
                   (found-section nil))

              (goto-char (point-min))

              ;; Handle different section formats
              (cond
               ;; Two-level path like "Life/Misc"
               ((= (length sections) 2)
                (let ((level1 (nth 0 sections))
                      (level2 (nth 1 sections)))
                  ;; Find level 1 section
                  (if (re-search-forward (format "^\\* %s" (regexp-quote level1)) nil t)
                      ;; Find level 2 section within level 1
                      (if (re-search-forward (format "^\\*\\* %s" (regexp-quote level2)) nil t)
                          (setq found-section t)
                        (error "Could not find section '%s' under '%s'" level2 level1))
                    (error "Could not find top-level section '%s'" level1))))

               ;; Single level like "Misc"
               (t
                (if (re-search-forward (format "^\\*\\* %s" (regexp-quote target-section)) nil t)
                    (setq found-section t)
                  (error "Could not find section '%s'" target-section))))

              ;; If we found the section, add the TODO
              (when found-section
                ;; Go to end of the section
                (forward-line 1)
                ;; Find the end of this section (next heading at same or higher level)
                (let ((section-end (save-excursion
                                    (if (re-search-forward "^\\*\\{1,2\\} " nil t)
                                        (line-beginning-position)
                                      (point-max)))))
                  ;; Go to the end of the section content
                  (goto-char section-end)
                  (forward-line -1)
                  (end-of-line)
                  ;; Insert the new TODO item at proper hierarchy level (*** for level 3)
                  (let ((todo-text (format "\n*** TODO %s" main-content))
                        (schedule-text (when (and final-scheduled (not (string-empty-p final-scheduled)))
                                         (format "\nSCHEDULED: %s"
                                                (if (string-match-p "^<.*>$" final-scheduled)
                                                    final-scheduled
                                                  (format "<%s>" final-scheduled))))))
                    (insert (concat todo-text (or schedule-text "") "\n")))
                  (setq final-location target-section)))))

          ;; Save the file
          (save-buffer)))

      ;; Return success information
      `((success . t)
        (content . ,content)
        (location . ,final-location)
        (file . ,target-file)
        (method . "direct insertion")
        (refiled . ,(not (string= final-location "Life/Misc")))))))

(defun org-agenda-mcp--get-available-locations ()
  "Get list of available locations for refiling in the personal agenda.
Returns the available refile targets from todo.org."
  (mcp-server-lib-with-error-handling
    (let ((target-file (expand-file-name "~/Dropbox/Agenda/todo.org")))
      (org-agenda-mcp--validate-file-path target-file)

      (with-current-buffer (find-file-noselect target-file)
        (let* ((targets (org-agenda-mcp--get-refile-targets))
               (formatted-targets (org-agenda-mcp--format-refile-targets targets)))

          `((file . ,target-file)
            (total_targets . ,(length targets))
            (available_locations . ,formatted-targets)))))))

(provide 'org-agenda-tools)
;;; org-agenda-tools.el ends here
