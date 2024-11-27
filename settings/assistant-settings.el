;;; assistant-settings.el --- Functions for my AI assistant

;; Assistant function to debug
(defun show-todo-diff-buffer ()
  "Show the diff of the todo.org file."
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

;; Trigger the function to save the diff when I save the todo.org file
;; Additionally, triggers the catalyst in the passive mode to read it and integrate as a memory
(add-hook 'before-save-hook
          (lambda ()
            (when (string= (buffer-file-name) "/Users/luis.moneda/Dropbox/Agenda/todo.org")
              (save-todo-diff)
              (call-catalyst-server "<placeholder, passive mode>" "passive"))))

;; Save the agenda view (built on top of todo.org) in a file
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

;; Run it once when starting Emacs
(export-org-agenda-to-file "/Users/luis.moneda/Dropbox/ai-assistant/agenda.txt")

;; Runs it everytime I save my todo.org
(add-hook 'after-save-hook
          (lambda ()
            (when (string= (buffer-file-name) "/Users/luis.moneda/Dropbox/Agenda/todo.org")
              (export-org-agenda-to-file "/Users/luis.moneda/Dropbox/ai-assistant/agenda.txt"))))

;; Knowledge info module
;; Tracks changes in my knowledge base files, which is the Org roam folder
(defun start-kb-tracking ()
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (async-shell-command "fswatch --exclude '.git/' --exclude '.*\.#' --one-per-batch --latency 30 /Users/luis.moneda/Dropbox/Agenda/roam | xargs -n1 -I{} /Users/luis.moneda/repos/org-roam-ai/catalyst/dump_kb_changes.sh")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<3>"))))

(start-kb-tracking)

;; Emacs chat interface
;; Uses the shell-maker from the chatgpt-shell project
;; https://github.com/xenodium/chatgpt-shell
(require 'shell-maker)

(defun call-catalyst-server (input-string &optional mode)
  "Call Python server with INPUT-STRING and optional MODE ('active', 'proactive, or 'passive'), defaulting to 'active' mode."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (base-url "http://localhost:8899/")
        (mode (or mode "active")))
    (if (not (string= mode "active"))
        ;; Asynchronous call for passive mode
        (decode-coding-string
		 (url-retrieve
         (concat base-url mode "/api/" input-string)
         (lambda (status)
           (goto-char (point-min))
           (search-forward-regexp "\n\n")
		   ;; For debugging
           ;; (message "Passive response: %s" (buffer-substring (point) (point-max)))
		   ;; (buffer-substring (point) (point-max))
		   ))
		   'utf-8
		 )
      ;; Synchronous call for active mode
      (with-current-buffer
          (decode-coding-string
		   (url-retrieve-synchronously (concat base-url mode "/api/" input-string))
		   'utf-8
		   )
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

;; The catalyst server
(defun start-catalyst-server ()
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (async-shell-command "python /Users/luis.moneda/repos/org-roam-ai/catalyst/catalyst.py")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<4>"))))

(start-catalyst-server)

;; Catalyst Proactive triggers

;; Send messages to the catalyst shell programatically
;; It isn't working. I need to review if it makes sense.
;; The current flow calls the server and shows the answer as a notification
;; in a new buffer.
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
             (decode-coding-string response 'utf-8))))
      ;; Synchronous call for active mode
      (let ((response-buffer (url-retrieve-synchronously (concat base-url mode "/api/" input-string))))
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (search-forward-regexp "\n\n")
          (let ((response (buffer-substring (point) (point-max))))
            ;; Return the response as a string
            (kill-buffer response-buffer)
            (decode-coding-string response 'utf-8)))))))

(defun print-catalyst-output-in-buffer (command mode buffer-name)
  "Send COMMAND to the Catalyst server and print the output in a new buffer called 'proactive catalyst'."
  (interactive "sEnter command: ")
  ;; (message "The command: ")
  ;; (message (url-encode-url (concat "http://localhost:8899/" mode "/api/" command)))
  (let ((buffer (get-buffer-create buffer-name)))
    (if (string= mode "active")
        ;; Synchronous call
        (let ((output (call-catalyst-server (url-hexify-string command) mode)))
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-max))
              (insert (format "\n🤖 **%s**\n%s\n"
                              (format-time-string "%Y-%m-%d %H:%M:%S")
                              (decode-coding-string output 'utf-8)))))
          (display-buffer buffer))
      ;; Asynchronous call
      (url-retrieve
       (concat "http://localhost:8899/" mode "/api/" (url-hexify-string command))
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
                                   (decode-coding-string output 'utf-8))))
				 (markdown-view-mode)
				 )
               ;; (display-buffer buffer)
               (set-mode-line-notification "🧠")
               (kill-buffer response-buffer)))))))))

;; To list of routines to call the Catalyst
;; I've added the proactive ones as a way to debug. I'm still refining concepts and language
;; Active/proactive/passive as a way to deliver a routine
(defun catalyst/proactive-function-list ()
  "Prompt the user to select a function to execute from a predefined list."
  (interactive)
  (let* ((function-list '(proactive-catalyst/show-proactive-notifications
			  catalyst-shell
			  proactive-catalyst/morning-outline
			  proactive-catalyst/evening-close
			  proactive-catalyst/continuous-review
			  active-catalyst/neuroscience-review
			  active-catalyst/ai-researcher-review
			  active-catalyst/evolutionary-biology-review
			  active-catalyst/analytic-philosophy-review
			  active-catalyst/greek-philosophy-review
			  catalyst-routine/zettelkasten-connection-suggestion
			  catalyst-routine/systems-thinking
			  proactive-catalyst/challenger-prompt
			  ))
         (function-name (completing-read "Select Catalyst function: " (mapcar 'symbol-name function-list))))
    (call-interactively (intern function-name))))

(global-set-key (kbd "C-c a") 'catalyst/proactive-function-list)

;; An easy way to jump to the notification when I see it in the mode line
(defun proactive-catalyst/show-proactive-notifications ()
  (interactive)
  (display-buffer "*Proactive Catalyst*")
  (clear-mode-line-notification)
  )

(defun proactive-catalyst/morning-outline ()
  "A routine to run proactively at the beginning of the day"
  (interactive)
  (print-catalyst-output-in-buffer
   (concat
	"This an automated message from your user. It is the beginning of the day, "
	"I want you to look at my recent agenda and what I did. Focus on telling me which tasks might deviate from my year objectives and why. Don't be shallow. Help me balance reading and creation (writing, coding), skewing me to creative work. Make me at most three provocative questions that don't sound generic about the tasks and the objectives.")
   "proactive"
   "*Proactive Catalyst*"
   )
  )

(defun proactive-catalyst/challenger-prompt ()
  "A routine to run proactively at the beginning of the day"
  (interactive)
  (print-catalyst-output-in-buffer
   (concat
	"This an automated message from your user. It is the beginning of the day, "
	"You are an AI assistant tasked with analyzing a person's recent agenda and activities in relation to their year objectives. Your goal is to identify potential deviations from these objectives, help balance reading and creative work, and formulate thought-provoking questions. Follow these steps carefully:

1. First, review the provided information which is in previous messages.

2. Analyze the recent agenda and activities, comparing them to the year objectives. Look for any misalignments or potential deviations. Consider both obvious and subtle discrepancies.

3. As you analyze, keep in mind the importance of balancing reading and creative work (writing, coding). Pay special attention to opportunities where the person could shift more towards creative work.

4. In your analysis, focus on tasks that might deviate from the year objectives. For each potential deviation:
   a. Identify the specific task or activity
   b. Explain why it might be deviating from the objectives
   c. Consider the potential impact of this deviation

5. Based on your analysis, formulate three provocative questions about the tasks and objectives. These questions should:
   a. Be specific to the person's situation, not generic
   b. Challenge assumptions or prompt deeper reflection
   c. Encourage thinking about alignment with objectives or work balance

6. Compile your findings and questions into a comprehensive response. Structure your response as follows:

<analysis>
[Provide your detailed analysis here, including identified deviations and explanations]
</analysis>

<balance_recommendation>
[Offer specific suggestions for improving the balance between reading and creative work, with an emphasis on increasing creative activities]
</balance_recommendation>

<provocative_questions>
1. [First provocative question]
2. [Second provocative question]
3. [Third provocative question]
</provocative_questions>

Remember to be thorough in your analysis, specific in your recommendations, and thought-provoking in your questions. Avoid generic statements and instead tailor your response to the unique situation presented in the agenda and objectives.")
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

(defun file-empty-p (file-path)
  "Check if the file at FILE-PATH is empty."
  (let ((attributes (file-attributes file-path)))
    (if attributes
        (zerop (file-attribute-size attributes))
      (error "File does not exist: %s" file-path))))

;; A proactive routine that runs from time to time, but
;; conditioned on having content to review.
(defun proactive-catalyst/continuous-review ()
  (interactive)
  (if (not (file-empty-p "~/Dropbox/ai-assistant/knowledge_base_changelog.txt"))
	  (progn
		;; (call-catalyst-server "" "new")
		(print-catalyst-output-in-buffer
   (concat
	"This an automated message from your user. It is a call for a routine review."
	"Look to the files changelogs inside the tag <knowledge-base-changelog> from the user's message to comment on the latest changes. Follow the rules:

\n Tell at the beginning of the answer it is the Continuous Review Report.
\n Review it by file in the form of: \n [File title](<file path>) \n {review content}.
\n The title should use markdown link syntax;
\n The {review content} should be SPECIFIC and cite the content of the changes, and provide examples on how to improve.
\n DONT review the formatting or the structure of the file.
\n Be very crictic.
\n The curter you are, the better.
\n Suggest connections with my knowledge base entries that were retrieved to you by similarity (also linking them with markdown syntax).
\n Don't suggest connections that ALREADY exist in the file you are reviewing.
\n Review ONLY the latest changes (NOT the ones coming from your memories or internal reflections the user doesn't see). Use this information only to enrich it.
\n No need to provide a summary at the end by saying something like 'Overall, ...'.
\n Split your suggestions by identifying them under different categories. Example: 'proof reading', 'originality', 'clarity', 'external references', etc
\n When it seems it is a technical note, push for clarity and completeness, and suggest technical references;
\n When it seems it is original work from the user, challenge if it is original by using your own knowledge and suggest references;
\n When it seems it is original and creative work, prompt the user for originality. Example: This is a common idea present in {paper, book, or article references}. Can you provide a unique perspective on this? For example, <aspect> is an unsolved problem in this area. Can you explore this further?
\n When it seems it is just a mundane note taking, don't push for originality, but try to suggest something useful;
\n When it seems it is about work (meetings), don't push for originality, but for clarity and completeness;
\n When it seems the user is just start to work on that knowledge base entry, refrain from providing suggestions, just acknowledge the beginning with a short phrase.")
   "proactive"
   "*Proactive Catalyst*"))
  (message "Catalyst: No changes to the knowledge base to review.")
	  )
  )

(run-at-time "0 sec" (* 240 60) #'proactive-catalyst/continuous-review)

;; Active/routines
(defun catalyst-on-region-or-buffer (prompt)
  "Process the selected region or entire buffer and send to print-catalyst-output-in-buffer."
  (interactive "sEnter the prompt: ")
  (let* ((text-content (if (use-region-p)
                           (buffer-substring (region-beginning) (region-end))
                         (buffer-substring-no-properties (point-min) (point-max))))
         (resulting-text (concat prompt text-content)))
    ;; (message resulting-text)
    (print-catalyst-output-in-buffer resulting-text "proactive" "*Proactive Catalyst*")
    ))

(defun active-catalyst/researcher-review (field)
  (interactive)
  (catalyst-on-region-or-buffer (concat
				 "You are an expericiend "
				 field
				 " researcher and you will "
				 "review the following text willing to provide insights from the "
				 field
				 " field that relate to it and justify every claim with a "
				 "scientific paper. \n Text to review: \n "
				 ))
  )

(defun active-catalyst/neuroscience-review ()
  (interactive)
  (active-catalyst/researcher-review "neuroscience")
  )

(defun active-catalyst/evolutionary-biology-review ()
  (interactive)
  (active-catalyst/researcher-review "evolutionary biology")
  )

(defun active-catalyst/analytic-philosophy-review ()
  (interactive)
  (active-catalyst/researcher-review "Analytic philosophy")
  )

(defun active-catalyst/greek-philosophy-review ()
  (interactive)
  (active-catalyst/researcher-review "Greek philosophy")
  )

(defun active-catalyst/ai-researcher-review ()
  (interactive)
  (active-catalyst/researcher-review "Artificial Intelligence (AI)")
  )

(defun catalyst-routine/zettelkasten-connection-suggestion ()
  (interactive)
  (catalyst-on-region-or-buffer (concat
				 "Your current task is to review the provided text and find "
				 "MEANINGFUL connections to Luis' Knowledge Base, "
				 "supporting im to find connections in the zettelkasten style."
				 "\n Text to review: \n "
				 ))
  )

(defun catalyst-routine/systems-thinking ()
  (interactive)
  (catalyst-on-region-or-buffer (concat
				 "You are an specialist in Systems Thinking. You use all the knowledge from  "
				 "Russel Ackoff, Donella Meadows, and other promenient figures of this field "
				 "to challenge and support ideas. Review the following text to challenge or "
				 "enforce it using concepts and examples from systemic thinking."
				 "\n Text to review: \n "
				 ))
  )

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

;; ;; To clear notification after reading
;; (defun check-and-trigger-on-buffer (my-buffer-name my-trigger-function)
;;   "Check buffer name and trigger function if it matches."
;;   (when (string-equal (buffer-name) my-buffer-name)
;;     (funcall my-trigger-function)))

;; (defun clear-proactive-catalyst-buffer-notification ()
;;   (interactive)
;;   (check-and-trigger-on-buffer "*Proactive Catalyst*" 'clear-mode-line-notification)
;;   )

;; (add-hook 'switch-to-buffer-hook 'clear-proactive-catalyst-buffer)

;; (defun displayed-buffers ()
;;   (interactive)
;;   "Return a list of buffers currently displayed in any window."
;;   (mapcar #'window-buffer (window-list)))

;; (message "Displayed buffers: %s" (mapconcat #'buffer-name (displayed-buffers) ", "))

;; end of notification

;; Notification buffer utils
;; This function enables org links using IDs using Markdown syntax.
;; I can force a refresh of the ids calling it with C-u C-c C-o. Or call
;; org-roam-update-org-id-location
(defun my/open-org-id-link-in-markdown (&optional refresh)
  "Open an Org file by ID from a Markdown link.
If REFRESH is non-nil, refresh the `org-id-locations` cache first."
  (interactive "P")
  (when refresh
    (message "Refreshing org-id-locations...")
    (org-id-update-id-locations
     (directory-files-recursively "/Users/luis.moneda/Library/CloudStorage/Dropbox/Agenda/roam/" "\\.org$"))
    (message "Refresh complete."))

  (let* ((link (thing-at-point 'line))
         (id-regexp "id:\\([A-Z0-9-]+\\)")
         (link-matches (string-match id-regexp link)))
    (if (not link-matches)
		(call-interactively #'markdown-follow-thing-at-point)
        ;; (message "No Markdown link ID found.")
      (let* ((org-id (match-string 1 link)))
        (message "Extracted ID: %s" org-id)
        (if (org-id-find org-id 'marker)
            (progn
              (message "Attempting to open Org entry with ID: %s" org-id)
              (org-id-open org-id 'marker))
          (message "No Org entry found with ID: %s" org-id))))))

;; Bind this function to C-c C-o in markdown-mode:
(define-key markdown-mode-map (kbd "C-c C-o") #'my/open-org-id-link-in-markdown)


(provide 'assistant-settings)
;;; assistant-settings.el ends here
