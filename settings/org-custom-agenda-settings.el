;;; org-custom-agenda-settings.el --- Settings for org custom org-agenda views

(require 'org)
(require 'org-agenda)

;; New states to to-do
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "DELEGATED(e)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)" "INACTIVE(i)" "FAIL(f)")))

(setq org-lowest-priority ?F)
(setq org-default-priority ?F)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-tags-exclude-from-inheritance '("epic"))
;; It didn't work as intended for tracking complex tasks and excluding the subtasks, but it is a cool feature to checkx
;; (setq org-enforce-todo-dependencies t)
;; (setq org-agenda-dim-blocked-tasks 'invisible)

;; Skip if a tag is in the direct parent to use in bulk tasks
(defun my-org-agenda-skip-if-parent-has-tag (tag)
  "Skip agenda entry if its direct parent is tagged with TAG."
  (save-excursion
    (let* ((element (org-element-at-point))
           (parent (org-element-property :parent element)))
      ;; Check if we are at a headline, if not go up until we find one
      (while (and parent (not (eq (org-element-type parent) 'headline)))
        (setq element parent
              parent (org-element-property :parent element)))
      ;; If parent is a headline, not a TODO, and has specific tag, return position to skip to
      (when (and (eq (org-element-type element) 'headline) parent
                 (member tag (org-element-property :tags parent)))
        (org-end-of-subtree t))))) ;; no change

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
	nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; From https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; Function to show scheduled only if missed
(defun org-agenda-skip-if-scheduled-today-or-later ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-seconds
            (time-to-seconds
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (- (time-to-seconds (current-time)) (* 3600 (+ 1 (nth 2 (decode-time (current-time))))))))
       (and scheduled-seconds
            (>= scheduled-seconds now)
            subtree-end))))

(defun my-org-agenda-skip-all-scheduled ()
  "Skip entries with any SCHEDULED timestamp."
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (when scheduled
      (save-excursion (org-end-of-subtree t)))))

(setq org-agenda-block-separator " ")

;; Identify with a symbol when a scheduled to-do has children
(with-eval-after-load 'org-agenda
  (defun my/org-has-children ()
    (if (save-excursion (org-goto-first-child)) "▶ " "   ")
  )
  (add-to-list 'org-agenda-prefix-format '(
     agenda  . "%i%-3:(my/org-has-children) %-12:c%?-12t% s "
  ))
  )

(defun scheduled-or-not (resp)
  (interactive)
  (if resp
    ;;'"OK"
    (concat "  " (number-to-string (org-time-stamp-to-now resp)) " day(s)")
    '"  Not Scheduled"
    )
  )

(setq org-agenda-custom-commands
      '(("d" "Personal agenda"
         (
		  ;; Deadlines
		  (tags-todo "+DEADLINE>=\"<-60d>\""
					 ((org-agenda-overriding-columns-format
					   "%25ITEM %DEADLINE %TAGS")
					  (org-agenda-overriding-header "Deadlines \n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					  (org-agenda-remove-tags t)
					  (org-agenda-sorting-strategy '(deadline-up))
					  (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"DEADLINE\")) ")
					  ))

		  ;; Week tasks and deadlines
		  (agenda ""
	  			  ((org-agenda-time-grid nil)
				   (org-agenda-remove-tags t)
	  			   (org-agenda-span 'week)
	  			   (org-deadline-warning-days 0)
	  			   (org-deadline-past-days 0)
				   (org-scheduled-past-days 0)
	  			   ;; (org-agenda-entry-types '(:deadline :scheduled))
	  			   (org-agenda-overriding-header "Week tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				   (org-agenda-scheduled-leaders '("" ""))
				   ;; (org-agenda-prefix-format "    %i %-12:c")
				   (org-agenda-prefix-format "    %i %-10:c%(my/org-has-children)")
	  			   ))

		  ;; High priority tasks
		  (tags "PRIORITY=\"A\"-cult+epic-delegated-family"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tech Epic\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-remove-tags t)
				 (org-agenda-todo-keyword-format "")
				 ))

		  ;; High priority tasks
		  (tags "PRIORITY=\"A\"+family+epic"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Family Epics\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-remove-tags t)
				 (org-agenda-todo-keyword-format "")
				 ))

	  	  ;; High priority tasks
		  (tags "PRIORITY=\"A\"+cult+epic"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Cult Journeys\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-remove-tags t)
				 (org-agenda-todo-keyword-format "")
				 ))

		  ;; Late tasks
		  (tags "+TODO=\"TODO\"+SCHEDULED<\"<today>\"-epic-goals-selfdevelopment"
				(
				 (org-agenda-overriding-header "Late tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) :%-8:c")
				 (org-agenda-sorting-strategy '(scheduled-up))
				 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
								(org-agenda-skip-if-scheduled-today-or-later)
								(org-agenda-skip-entry-if 'notscheduled)
								(my-org-agenda-skip-if-parent-has-tag "bulk")
								))
				 (org-agenda-remove-tags t)
				 (org-agenda-todo-keyword-format "")
				 )
				)

		  ;; All not scheduled things
		  (tags "-goals-selfdevelopment+TODO=\"TODO\"-epic-family"
	  			(
	  			 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
								;; (org-agenda-skip-if-scheduled-today-or-later)
												(my-org-agenda-skip-all-scheduled)
												(org-agenda-skip-if nil '(deadline))
												(my-org-agenda-skip-if-parent-has-tag "bulk")
								))
				 (org-agenda-overriding-header "Backlog\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
	  			 (org-agenda-remove-tags t)))

		  ;; Year Goals General
		  (tags "+goals-family+LEVEL=3+TODO=\"TODO\"|+goals-family+LEVEL=3+TODO=\"DONE\""
				((org-agenda-category-filter "-Nubank")
				 (org-agenda-prefix-format " ")
				 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
								(air-org-skip-subtree-if-priority ?A)
								(org-agenda-skip-if nil '(scheduled deadline))))
				 (org-agenda-overriding-header "Season Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-remove-tags t)))
		  ;;Family goals
		  (tags "+family-epic+LEVEL=3+TODO=\"TODO\"|+family+LEVEL=3+TODO=\"DONE\""
			        ((org-agenda-prefix-format " ")
				 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
								(air-org-skip-subtree-if-priority ?A)
								(org-agenda-skip-if nil '(scheduled deadline))))
				 (org-agenda-overriding-header "Season Family Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-remove-tags t)))

		  ;; Backlog projects
		  (tags "-PRIORITY=\"A\"+epic"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Backlog Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				 (org-agenda-remove-tags t)
				 (org-agenda-todo-keyword-format "")
				 ))
		  )
         ((org-agenda-compact-blocks nil))
		 )
	))

(add-to-list 'org-agenda-custom-commands
			 '("nu" "Nubank Agenda"
			   (
				;; Deadlines
				(tags-todo "+DEADLINE>=\"<-60d>\"|+perfcycle"
						   ((org-agenda-overriding-columns-format
							 "%25ITEM %DEADLINE %TAGS")
							(org-agenda-overriding-header "📅 Deadlines days\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
							(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
							(org-agenda-remove-tags t)
							(org-agenda-entry-types '(:deadline :scheduled))
							(org-agenda-sorting-strategy '(deadline-up))
							(org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"DEADLINE\")) ")
							))

				;; Week tasks and deadlines
				(agenda ""
	  					((org-agenda-time-grid nil)
	  					 (org-agenda-span 'week)
	  					 (org-deadline-warning-days 0)
	  					 (org-deadline-past-days 0)
						 (org-scheduled-past-days 0)
	  					 (org-agenda-entry-types '(:deadline :scheduled))
	  					 (org-agenda-overriding-header "⏳ Week tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
						 (org-agenda-scheduled-leaders '("" ""))
						 (org-agenda-prefix-format "    %i %-4e %-10:c%(my/org-has-children)")
						 (org-agenda-remove-tags t)
						 (org-agenda-skip-function '(air-org-skip-subtree-if-habit))
						 (org-agenda-skip-function '(my-org-agenda-skip-if-parent-has-tag "bulk"))
						 (tags-todo "batch")
						 (tags-todo "-batch")
						 ;; (org-agenda-skip-function #'add-clock-emoji-to-agenda)
	  					 ))

				;; High priority projects
				(tags "+epic-team+spinning"
					  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
					   (org-agenda-overriding-header "🌋 My Epics\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-remove-tags t)
					   (org-agenda-todo-keyword-format "")
					   ))

				(tags "+team+epic+spinning"
					  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
					   (org-agenda-overriding-header "📊 Team Epics\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-remove-tags t)
					   (org-agenda-todo-keyword-format "")
					   ))

				;; (tags "+epic+educ+PRIORITY=\"A\""
				;; 	  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				;; 	   (org-agenda-overriding-header "🧠 Education Epics\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				;; 	   (org-agenda-remove-tags t)
				;; 	   (org-agenda-todo-keyword-format "")
				;; 	   ))

				;; Delegated Tasks
				(tags "+team+directreports+TODO=\"TODO\"-epic|delegated+TODO=\"TODO\"-epic"
					  ((org-agenda-overriding-header "🤝 Delegated tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
					   (org-agenda-remove-tags t)
					   (org-agenda-remove-tags-when-in-prefix t)
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-todo-keyword-format "")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
													  ))
					   )
					  )

				;; High priority tasks
				(tags "-epic+PRIORITY=\"A\"-delegated"
					  (
					   (org-agenda-overriding-header "⚡ High-priority Tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-remove-tags t)
					   (org-agenda-todo-keyword-format "")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (org-agenda-skip-entry-if 'todo 'done)
									  ))
					   ))

				;; Batch tasks
				(tags "-epic+batch-delegated"
					  (
					   (org-agenda-overriding-header "🚚 Batch Tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-remove-tags t)
					   (org-agenda-todo-keyword-format "")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (org-agenda-skip-entry-if 'todo 'done)
									  ))
					   ))

				;; Blocked tasks
				(tags "-team+TODO=\"WAIT\"-PRIORITY=\"A\"-epic"
					  ((org-agenda-overriding-header "💀 Blocked & delayed tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
					   (org-agenda-remove-tags t)
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-todo-keyword-format "")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (my-org-agenda-skip-if-parent-has-tag "bulk")
													  ))
					   )
					  )

				;; Tasks in Projects
				(tags "+projects-delegated+TODO=\"TODO\"-PRIORITY=\"A\"-epic"
					  (
					   (org-agenda-overriding-header "🚢 Tasks in Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
					   (org-agenda-remove-tags t)
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-todo-keyword-format "")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (my-org-agenda-skip-if-parent-has-tag "bulk")
													  ))
					   )
					  )

				;; Tasks about Direct Reports
				(tags "+directreports-team+TODO=\"TODO\"-PRIORITY=\"A\"-epic"
					  ((org-agenda-overriding-header "👨‍👦‍👦 Tasks about direct reports\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
					   (org-agenda-remove-tags t)
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-todo-keyword-format "")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (my-org-agenda-skip-if-parent-has-tag "bulk")
													  ))
					   )
					  )

				;; NEXT Manager
				(tags "+manager-directreports+TODO=\"TODO\"-PRIORITY=\"A\"-epic-perfcycle-selfdevelopment-cyclegoals-delegated-batch"
					  (
					   (org-agenda-overriding-header "💼 Management\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
					   (org-agenda-remove-tags t)
					   (org-agenda-category-filter "")
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (my-org-agenda-skip-if-parent-has-tag "bulk")
													  ))
					   (org-agenda-todo-keyword-format "")
					   )
					  )

				;; NEXT Education
				(tags "+education+TODO=\"TODO\"-PRIORITY=\"A\"-epic-cyclegoals"
					  (
					   (org-agenda-overriding-header "📚 Education\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
					   (org-agenda-remove-tags t)
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (my-org-agenda-skip-if-parent-has-tag "bulk")
													  ))
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-todo-keyword-format "")
					   )
					  )

				;; Perf Cycle goals
				(tags "+cyclegoals+LEVEL=3+TODO=\"TODO\"|+cyclegoals+LEVEL=3+TODO=\"DONE\""
					  (;; (org-agenda-category-filter "-Nubank")
					   (org-agenda-prefix-format " ")
					   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
													  (air-org-skip-subtree-if-priority ?A)
													  (org-agenda-skip-if nil '(scheduled deadline))))
					   (org-agenda-overriding-header "🎯 Cycle Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-remove-tags t)

					   )
					  )

				;; All not scheduled things
				(tags "-cyclegoals-selfdevelopment+TODO=\"TODO\"-PRIORITY=\"A\"-manager-projects-education-directreports-spinning"
	  				  (
	  				   (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
									  (org-agenda-skip-if-scheduled-today-or-later)
									  (my-org-agenda-skip-if-parent-has-tag "bulk")
													  ))
					   (org-agenda-sorting-strategy '(priority-down))
					   (org-agenda-overriding-header "❄️ Backlog tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
	  				   (org-agenda-remove-tags t)
					   )
					  )

	  			;; Backlog projects
				(tags "+epic-spinning"
					  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
					   (org-agenda-overriding-header "📁 Backlog Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
					   (org-agenda-remove-tags t)
					   (org-agenda-todo-keyword-format "")
					   ))
				)
			   ((org-agenda-compact-blocks nil))
			   )
			 )

(add-to-list 'org-agenda-custom-commands
      '("b" "Agenda view debugger"
	 (
	  ;; All not scheduled things
	  		  (tags "-goals-selfdevelopment+TODO=\"TODO\"-epic-family"
	  			(
	  			 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
								;; (org-agenda-skip-if-scheduled-today-or-later)
								(my-org-agenda-skip-all-scheduled)
								(my-org-agenda-skip-if-parent-has-tag "bulk")
								))
				 (org-agenda-overriding-header "Backlog\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
	  			 (org-agenda-remove-tags t)))
	  )
	 )
      )

(add-to-list 'org-agenda-custom-commands
             '("i" "Debug backlog"
               ((tags "-goals-selfdevelopment+TODO=\"TODO\"-epic-family"
                      ((org-agenda-skip-function '(my-org-agenda-skip-all-scheduled))
                       (org-agenda-overriding-header "Backlog Debug\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                       (org-agenda-remove-tags t))))))


;; Only deadlines view
(add-to-list 'org-agenda-custom-commands
             '("z" "Deadlines"
               tags "+DEADLINE>=\"<today>\"&DEADLINE<=\"<+2m>\""
               ((org-agenda-overriding-columns-format
                 "%25ITEM %DEADLINE %TAGS")))
             )


;; Compact only day view
;; Custom agenda command: today's scheduled tasks, compact format
(add-to-list 'org-agenda-custom-commands
             '("l" "Compact today"
               agenda ""
               ((org-agenda-span 'day)
                (org-agenda-start-day "+0d")
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-scheduled-if-done t)
                (org-agenda-use-time-grid nil)
                (org-agenda-remove-tags t)
                (org-agenda-prefix-format '((agenda . " %s")))
                (org-agenda-scheduled-leaders '("" ""))
                (org-agenda-overriding-header "Today Agenda:"))))


;; Compact only day view
(add-to-list 'org-agenda-custom-commands
             '("o" "Compact today"
			   ((todo "TODO" (
							  (org-agenda-overriding-header "\n⚡ Do Today:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
							  (org-agenda-remove-tags t)
							  (org-agenda-prefix-format " %-2i %-15b")
							  (org-agenda-todo-keyword-format "")
							  ))
				(agenda "" (
							(org-agenda-start-day "+0d")
							(org-agenda-span 5)
							(org-agenda-overriding-header "⚡ Schedule:\n⎺⎺⎺⎺⎺⎺⎺⎺⎺")
							(org-agenda-repeating-timestamp-show-all nil)
							(org-agenda-remove-tags t)
							(org-agenda-prefix-format   "  %-3i  %-15b %t%s")
							(org-agenda-todo-keyword-format " ☐ ")
							(org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
							(org-agenda-scheduled-leaders '("" ""))
							(org-agenda-time-grid (quote ((daily today remove-match)
														  (0900 1200 1500 1800 2100)
														  "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))
							)))))
(provide 'org-custom-agenda-settings)
;;; org-custom-agendasettings.el ends here
