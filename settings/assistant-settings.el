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
              (save-todo-diff)
              (call-catalyst-server "<placeholder, passive mode>" "passive"))))

;; Dump agenda view in a text file
;; (defun export-org-agenda-to-file (file)
;;   "Export org agenda view to a text FILE."
;;   (interactive "FFile to export org agenda to: ")
;;   (with-temp-buffer
;;     (org-agenda nil "d")
;;     (org-agenda-redo)
;;     (write-region (point-min) (point-max) file)))

;; This function doesn't disrupt the agenda view on display
(defun export-org-agenda-to-file (file)
  "Export org agenda view to a text FILE without showing the agenda buffer and without visual side effects."
  (interactive "FFile to export org agenda to: ")
  (let* ((agenda-buffer (get-buffer "*Org Agenda*"))
         (org-agenda-buffer (get-buffer-create " *temp-agenda*"))
         (agenda-visible-window (when agenda-buffer (get-buffer-window agenda-buffer))))
    (if agenda-visible-window
        ;; If the agenda buffer is visible, use its current contents
        (with-current-buffer agenda-buffer
          (let ((inhibit-read-only t))
            (write-region (point-min) (point-max) file)))
      ;; Otherwise, create the agenda in a temporary buffer and export it
      (save-window-excursion
        (with-current-buffer org-agenda-buffer
          (let ((org-agenda-sticky nil))  ; Disable sticky agendas to ensure temporary agenda creation
            (org-agenda nil "d"))
          (write-region (point-min) (point-max) file)
          (kill-buffer org-agenda-buffer))))))

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

(defun call-catalyst-server (input-string &optional mode)
  "Call Python server with INPUT-STRING and optional MODE ('active' or 'passive'), defaulting to 'active' mode."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (base-url "http://localhost:8899/")
        (mode (or mode "active")))
    (if (not (string= mode "active"))
        ;; Asynchronous call for passive mode
        (url-retrieve
         (concat base-url mode "/api/" input-string)
         (lambda (status)
           (goto-char (point-min))
           (search-forward-regexp "\n\n")
		   ;; For debugging
           ;; (message "Passive response: %s" (buffer-substring (point) (point-max)))
		   ;; (buffer-substring (point) (point-max))
		   ))
      ;; Synchronous call for active mode
      (with-current-buffer
          (url-retrieve-synchronously (concat base-url mode "/api/" input-string))
        (goto-char (point-min))
        (search-forward-regexp "\n\n")
		;; (message "Active response: %s" (buffer-substring (point) (point-max)))
        (buffer-substring (point) (point-max))))))

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

;; Catalyst Proactive triggers

;; Send messages to the catalyst shell programatically
(defun send-command-directly-to-catalyst-shell (command mode)
  "Send COMMAND directly to the Catalyst shell and return its output."
  (let (output)
    ;; Call the execute-command method from the shell config
    (funcall (shell-maker-config-execute-command ai-catalyst-assistant-shell--config)
             command
             nil ;; Pass nil for history as it's not used here
             (lambda (result _error)
               (setq output result)) ;; Collect the output in a local variable
             (lambda (_error)
               (error "An error occurred while processing the command")))
    ;; Wait until `output` is set
    (while (null output)
      (sit-for 0.1))
    output))

(require 'cl-lib)

(defun call-catalyst-server (input-string &optional mode)
  "Call Python server with INPUT-STRING and optional MODE ('active' or 'passive'), defaulting to 'active' mode."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (base-url "http://localhost:8899/")
        (mode (or mode "active")))
    (if (not (string= mode "active"))
        ;; Asynchronous call for passive mode
        (url-retrieve
         (concat base-url mode "/api/" input-string)
         (lambda (status)
           (goto-char (point-min))
           (search-forward-regexp "\n\n")
           (let ((response (buffer-substring (point) (point-max))))
			 (while (null response)
			   (sit-for 0.1))
             ;; Process the response as needed
             ;; (message "Passive response: %s" response)
             response)))
      ;; Synchronous call for active mode
      (let ((response-buffer (url-retrieve-synchronously (concat base-url mode "/api/" input-string))))
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (search-forward-regexp "\n\n")
          (let ((response (buffer-substring (point) (point-max))))
            ;; Return the response as a string
            (kill-buffer response-buffer)
            response))))))

(defun print-catalyst-output-in-buffer (command mode buffer-name)
  "Send COMMAND directly to the Catalyst shell and print the output in a new buffer called 'proactive catalyst'."
  (interactive "sEnter command: ")
  (let ((buffer (get-buffer-create buffer-name)))
    (if (string= mode "active")
        ;; Synchronous call
        (let ((output (call-catalyst-server command mode)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-max))
              (insert (format "\n🤖 **%s**\n%s\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S")
                              output))))
          (display-buffer buffer))
      ;; Asynchronous call
      (url-retrieve
       (concat "http://localhost:8899/" mode "/api/" command)
       (lexical-let ((buffer buffer)) ;; Ensure buffer is accessible inside the callback
         (lambda (status)
           (let ((response-buffer (current-buffer))) ;; Save current buffer
             (goto-char (point-min))
             (search-forward-regexp "\n\n")
             (let ((output (buffer-substring (point) (point-max))))
               (with-current-buffer buffer
				 (markdown-mode)
                 (save-excursion
                   (goto-char (point-min))
				   (insert "-------------------------------------------------------")
                   (insert (format "\n🤖 **%s**\n%s\n\n"
                                   (format-time-string "%Y-%m-%d %H:%M:%S")
                                   output)))
				 (markdown-view-mode)
				 )
               ;; (display-buffer buffer)
               (set-mode-line-notification "🧠")
               (kill-buffer response-buffer)))))))))

;; To debug or to get used to a command without an automatic trigger
(defun catalyst/proactive-function-list ()
  "Prompt the user to select a function to execute from a predefined list."
  (interactive)
  (let* ((function-list '(proactive-catalyst/show-proactive-notifications
						  proactive-catalyst/morning-outline
                          proactive-catalyst/evening-close
						  proactive-catalyst/continuous-review
						  ))
         (function-name (completing-read "Select Catalyst function: " (mapcar 'symbol-name function-list))))
    (call-interactively (intern function-name))))

(global-set-key (kbd "C-c a") 'catalyst/proactive-function-list)


(defun proactive-catalyst/show-proactive-notifications ()
  (interactive)
  (display-buffer "*Proactive Catalyst*")
  )

(defun proactive-catalyst/morning-outline ()
  (interactive)
  (print-catalyst-output-in-buffer
   (concat
	"This an automated message from your user. It is the beginning of the day, "
	"I want you to look to my agenda and what I did recently. Focus on telling me how some tasks might be deviating from my year objectives. ")
   "proactive"
   "*Proactive Catalyst*"
   )
  )

(defun proactive-catalyst/evening-close ()
  (interactive)
  (print-catalyst-output-in-buffer
   (concat
	"This an automated message from your user. It is the end of the day."
	"I want you to look to what I have accomplished today and review it for quality issues.")
   "proactive"
   "*Proactive Catalyst*")
  )
(defun proactive-catalyst/continuous-review ()
  (interactive)
  (if (not (file-empty-p "~/Dropbox/ai-assistant/knowledge_base_changelog.txt"))
  (print-catalyst-output-in-buffer
   (concat
	"This an automated message from your user. It is a call for a routine review."
	"I want you to look to what is under 'The changelog of user's knowledge base/working space' and review it for quality.")
   "proactive"
   "*Proactive Catalyst*")
	  )
  )

(run-at-time "0 sec" 3600 #'proactive-catalyst/continuous-review)

(defun file-empty-p (file-path)
  "Check if the file at FILE-PATH is empty."
  (let ((attributes (file-attributes file-path)))
    (if attributes
        (zerop (file-attribute-size attributes))
      (error "File does not exist: %s" file-path))))

;; Timer triggers

;; (defun my-custom-command ()
;;   "Function to run custom command."
;;   (message "This is my custom command running."))

;; (defun my-check-time-window ()
;;   "Check if the current time is between 4am and 9am, and if it's the first start of the day."
;;   (let ((current-time (current-time))
;;         (file "~/.emacs.d/last-emacs-start-time"))
;;     (if (file-exists-p file)
;;         (let* ((last-time (with-temp-buffer
;;                             (insert-file-contents file)
;;                             (read (current-buffer))))
;;                (last-day (format-time-string "%Y-%m-%d" last-time))
;;                (current-day (format-time-string "%Y-%m-%d" current-time))
;;                (start-hour (string-to-number (format-time-string "%H" current-time))))
;;           (when (and (string= last-day current-day)
;;                      (>= start-hour 4)
;;                      (< start-hour 9)
;;                      (not (string= last-day current-day)))
;;             (my-custom-command))))
;;     ;; Save current time as last start time
;;     (with-temp-buffer
;;       (insert (format "%S" current-time))
;;       (write-file file))))

;; ;; Add to Emacs startup hook
;; (add-hook 'emacs-startup-hook 'my-check-time-window)

;; Notification functions for proactive calls
(defvar my-mode-line-notification "" "Custom notification for mode line.")

(defun set-mode-line-notification (msg)
  "Set a custom notification in the mode line to MSG."
  (setq my-mode-line-notification msg)
  (force-mode-line-update))

(defun clear-mode-line-notification ()
  "Clear the custom notification from the mode line."
  (setq my-mode-line-notification "")
  (force-mode-line-update))

;; Add the custom notification to the mode-line-format
(setq-default mode-line-format
              (list
               '(:eval (if (not (string= my-mode-line-notification ""))
                           (concat " " my-mode-line-notification " | ")
                         ""))
               mode-line-format))

(run-at-time "0 sec" 600 #'clear-mode-line-notification)
;; end of notification

(provide 'assistant-settings)
;;; assistant-settings.el ends here
