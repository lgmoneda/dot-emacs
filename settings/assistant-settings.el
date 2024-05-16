;;; assistant-settings.el --- Functions for my AI assistant

;; Assistant function to debug
(defun show-todo-diff-buffer ()
  "Show the diff of todo.org file."
  (interactive)
  (let* ((old-todo-file "/Users/luis.moneda/Dropbox/Agenda/todo.org") ; Replace with your actual path
         (new-todo-file (buffer-file-name))
         (temp-file (make-temp-file "todo_temp")))
    (write-region nil nil temp-file) ; Save current buffer to temporary file
    (with-current-buffer (get-buffer-create "*todo-diff*")
      (erase-buffer)
      (insert (shell-command-to-string (format "diff -u %s %s" old-todo-file temp-file)))
      (diff-mode)
      (pop-to-buffer (current-buffer)))
    (delete-file temp-file)))

(defun lgm/hierarchy-from-file-and-line (file line)
  "Return a formatted string representing the hierarchy of a heading at a given line in an org file."
  (with-temp-buffer
    ;; Load the org-file contents into the buffer.
    (insert-file-contents file)
    (org-mode) ; Ensure org-mode is activated for org-element functions to work.
    (goto-char (point-min))
    (forward-line (1- line)) ; Go to the desired line.

    (let ((elements (list))
          (current (org-element-at-point)))
      ;; Navigate upwards through the org document structure, collecting headings.
      (while current
        (let ((type (org-element-type current)))
          ;; Collect only headline names.
          (when (eq type 'headline)
            (let ((title (org-element-property :title current)))
              (push title elements)))
          ;; Set current to parent, or nil if at document top level to exit loop.
          (setq current (and (not (eq type 'org-data))
                             (org-element-property :parent current)))))
      ;; Format and return the collected headings as a string.
      (format "Heading hierarchy: %s" (string-join elements " > ")))))

(defun save-todo-diff ()
  "Save a summary of valid changes in the todo.org file to a changelog file, including the hierarchy."
  (interactive)
  (let* ((old-todo-file "/Users/luis.moneda/Dropbox/Agenda/todo.org")  ; Update with your path
         (new-todo-file (buffer-file-name))
         (changelog-file "/Users/luis.moneda/Dropbox/ai-assistant/todo_changelog.txt")
         (temp-file (make-temp-file "todo_temp"))
         (current-date-time (format-time-string "%Y-%m-%d %H:%M:%S"))
         diff-output)

    ;; Write the current buffer to the temp file to compare
    (write-region nil nil temp-file)
    ;; Get diff output as a string
    (setq diff-output (shell-command-to-string (format "diff -u %s %s" old-todo-file temp-file)))

    (with-temp-buffer
      (insert diff-output)
      (goto-char (point-min))

      ;; Search and skip until the actual content changes
      (while (re-search-forward "^@@" nil t)
        (let* ((line-text "")
               (line-info (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (change-type "")
               hierarchy
               (original-line 0)
               (new-line 0)
               (original-start 0)
               (new-start 0))

          ;; Parse line range information
          (when (string-match "^@@ -\\([0-9]+\\),?\\([0-9]*\\)? \\+\\([0-9]+\\),?\\([0-9]*\\)?" line-info)
            (setq original-start (string-to-number (match-string 1 line-info))
                  new-start (string-to-number (match-string 3 line-info))
                  original-line original-start
                  new-line new-start))

          ;; Skip the current line-info line itself
          (forward-line)

          ;; Iterate over each subsequent line to process each change
          (while (and (not (looking-at "^@@")) (not (eobp)))  ; Until the next change block or end of buffer
            (setq line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

            (cond
             ((string-prefix-p "+" line-text)
              (setq change-type "Addition")
              ;; Extract hierarchy before incrementing the line number
              (setq hierarchy (lgm/hierarchy-from-file-and-line temp-file new-line))
              (setq line-text (substring line-text 1))  ; Remove the leading +
              ;; Log the change before incrementing the line number for subsequent additions
              (let ((changelog-entry (format "\n %s | Addition | line %d: %s | %s\n"
                                             current-date-time
                                             new-line
                                             line-text
                                             hierarchy)))
                (append-to-file changelog-entry nil changelog-file))
              (cl-incf new-line))

             ((string-prefix-p "-" line-text)
              (setq change-type "Deletion")
              ;; Extract hierarchy before incrementing the line number
              (setq hierarchy (lgm/hierarchy-from-file-and-line old-todo-file original-line))
              (setq line-text (substring line-text 1))  ; Remove the leading -
              ;; Log the change before incrementing the line number for subsequent deletions
              (let ((changelog-entry (format "\n %s | Deletion | line %d: %s | %s\n"
                                             current-date-time
                                             original-line
                            line-text
                                             hierarchy)))
                (append-to-file changelog-entry nil changelog-file))
              (cl-incf original-line)))

            ;; Ensure both counters are incremented for unchanged lines
            (when (string-prefix-p " " line-text)
              (cl-incf new-line)
              (cl-incf original-line))

            (forward-line))))

      (delete-file temp-file))))

(add-hook 'before-save-hook
          (lambda ()
            (when (string= (buffer-file-name) "/Users/luis.moneda/Dropbox/Agenda/todo.org")
              (save-todo-diff))))

;; Dump agenda view in a text file
;; (defun export-org-agenda-to-file (file)
;;   "Export org agenda view to a text FILE."
;;   (interactive "FFile to export org agenda to: ")
;;   (with-temp-buffer
;;     (org-agenda nil "d")
;;     (org-agenda-redo)
;;     (write-region (point-min) (point-max) file)))

;; Dump agenda view in a text file without any visual side effects
(defun export-org-agenda-to-file (file)
  "Export org agenda view to a text FILE without showing the agenda buffer and without visual side effects."
  (interactive "FFile to export org agenda to: ")
  (let ((org-agenda-buffer (get-buffer-create " *temp-agenda*")))
    (save-window-excursion
      (with-current-buffer org-agenda-buffer
        (org-agenda nil "d")
        (org-agenda-redo)
        (write-region (point-min) (point-max) file)))
    ;; Kill the temporary agenda buffer to avoid clutter
    (kill-buffer org-agenda-buffer)))

(export-org-agenda-to-file "/Users/luis.moneda/Dropbox/ai-assistant/agenda.txt")

(add-hook 'after-save-hook
          (lambda ()
            (when (string= (buffer-file-name) "/Users/luis.moneda/Dropbox/Agenda/todo.org")
              (export-org-agenda-to-file "/Users/luis.moneda/Dropbox/ai-assistant/agenda.txt"))))

;; Knowledge info module
;; kb tracking
(defun start-kb-tracking ()
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (async-shell-command "fswatch --exclude '.git/' --exclude '.*\.#' --one-per-batch --latency 30 /Users/luis.moneda/Dropbox/Agenda/roam | xargs -n1 -I{} /Users/luis.moneda/repos/org-roam-ai/catalyst/dump_kb_changes.sh")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<3>"))))

(start-kb-tracking)

;; Refresh the agenda view dump

;; Emacs chat interface
(require 'shell-maker)

(defun call-catalyst-server (input-string)
  "Call Python server with INPUT-STRING and return the output string."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain"))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "http://localhost:8888/api/" input-string))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(defvar ai-catalyst-assistant-shell--config
  (make-shell-maker-config
   :name "Catalyst"
   :execute-command
   (lambda (command _history callback error-callback)
     (funcall callback
              (call-catalyst-server command)
              nil))
   :on-command-finished
   (lambda (command output)
     (chatgpt-shell--put-source-block-overlays))
   ))

(defun catalyst-shell ()
  "Start a Catalyst shell."
  (interactive)
  (shell-maker-start ai-catalyst-assistant-shell--config))

(defun start-catalyst-server ()
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (async-shell-command "python /Users/luis.moneda/repos/org-roam-ai/catalyst/catalyst.py")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<4>"))))

(start-catalyst-server)


(provide 'assistant-settings)
;;; assistant-settings.el ends here
