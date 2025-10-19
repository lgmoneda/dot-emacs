(require 'org)
(require 'org-super-agenda)
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

;; Org-super-agenda
(org-super-agenda-mode 1)

;; ✅ Each variable/value pair is balanced here.
(setq org-agenda-block-separator " ")
(setq org-super-agenda-header-separator "\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺\n")
(setq org-super-agenda-header-separator "\n")
;; to keep org sorting strategy in the org-super-agenda groups
(setq org-super-agenda-keep-order t)

;; Hide tags in agenda views
(setq org-agenda-remove-tags t)

(defun my/org-agenda-remove-category (item)
  "Remove category prefix, priority, and fix TODO keyword display."
  (if (string-match "^\\([[:space:]]*\\)\\([^[:space:]]+:\\)[[:space:]]*\\(\\[#[A-Z]\\][[:space:]]*\\)?\\(.\\)[[:space:]]*" item)
      (let* ((prefix-end (match-end 0))
             (first-char (match-string 4 item))
             (replacement "  ")
             (rest (substring item prefix-end))
             ;; Check if we need to remove the orphaned first letter
             (new-rest (if (and first-char
                               (string-match-p "^\\(ODO\\|ONE\\)" rest))
                          (concat first-char rest)
                        rest))
             (new-item (concat replacement new-rest)))
        ;; Copy all text properties
        (set-text-properties 0 (length replacement)
                            (text-properties-at prefix-end item)
                            new-item)
        new-item)
    item))

(defun my/org-agenda-remove-todo-keyword (item)
  "Remove TODO keyword from ITEM string in org-super-agenda display."
  (let* ((marker (org-find-text-property-in-string 'org-marker item))
         (todo (when (and marker (marker-buffer marker))
                 (org-with-point-at marker
                   (org-get-todo-state)))))
    (if (not todo)
        item
      ;; Look for the TODO keyword followed by colon or space
      (replace-regexp-in-string
       (concat "\\(" (regexp-quote todo) "\\)[[:space:]]+")
       "" item nil nil 1))))

(defun my/org-agenda-clean-item (item)
  "Remove both category and TODO keyword from ITEM."
  (let* ((marker (org-find-text-property-in-string 'org-marker item))
         (todo (when (and marker (marker-buffer marker))
                 (org-with-point-at marker
                   (org-get-todo-state))))
         ;; First remove category
         (cleaned (replace-regexp-in-string
                   "^[[:space:]]*[^[:space:]]+:[[:space:]]*"
                   "  "
                   item)))
    ;; Then remove TODO keyword if present
    (if todo
        (replace-regexp-in-string
         (concat "\\b" (regexp-quote todo) "\\b[[:space:]]*")
         ""
         cleaned)
      cleaned)))

(defun my/org-agenda-add-deadline-prefix (item)
  "Add deadline/scheduled prefix showing days until/since, preserving text properties."
  (let* ((marker (org-find-text-property-in-string 'org-marker item))
         (deadline (when (and marker (marker-buffer marker))
                     (org-with-point-at marker
                       (org-entry-get (point) "DEADLINE"))))
         (scheduled (when (and marker (marker-buffer marker))
                      (org-with-point-at marker
                        (org-entry-get (point) "SCHEDULED")))))
    (let* ((timestamp (or deadline scheduled))
           (prefix (if timestamp
                      (let* ((days (org-time-stamp-to-now timestamp))
                             (days-str (format "%d day(s)" days)))
                        (format "  %-16s " days-str))
                    "  Not Scheduled    "))
           (new-item (concat prefix item)))
      ;; Copy all text properties from the start of the original item to the prefix
      (set-text-properties 0 (length prefix)
                          (text-properties-at 0 item)
                          new-item)
      new-item)))

(defun lgm/org-skip-when-parent-scheduled ()
  "Skip entries whose parent has a SCHEDULED property."
  (save-restriction
    (widen)
    (let ((should-skip nil))
      (save-excursion
        (while (and (not should-skip) (org-up-heading-safe))
          (when (org-get-scheduled-time (point))
            (setq should-skip t))))
      (when should-skip
        (or (outline-next-heading) (point-max))))))

(setq org-agenda-custom-commands
      '(("d" "Personal Agenda"
         (
          ;; ───────────────────────────────
          ;; Deadlines
          (tags-todo "+DEADLINE>=\"<-60d>\""
                     ((org-agenda-overriding-header "Deadlines \n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      (org-agenda-sorting-strategy '(deadline-up))
                      (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"DEADLINE\")) ")
                      (org-super-agenda-groups nil)))

          ;; ───────────────────────────────
          ;; Week tasks (scheduled) - ungrouped
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
	  	   ))
          ;; ALL TODO items - grouped by org-super-agenda in ONE PASS
      ;; (tags "+goals|+epic"
	  (tags "LEVEL>=1+TODO=\"TODO\"|+TODO=\"WAIT\"|+TODO=\"DONE\""
		((org-agenda-overriding-header "")
		 (org-super-agenda-groups
		  '((:name "Tech Epic\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
			   :and (:priority "A"
					   :tag "epic"
           				   :todo "TODO"
					   :not (:tag "family")
					   :not (:tag "cult"))
		  	   :transformer my/org-agenda-remove-todo-keyword)

		  (:name "Family Epic\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
			  :and (:priority "A" :tag "family" :tag "epic" :todo "TODO")
			  :transformer my/org-agenda-remove-todo-keyword
			  :order 0)

		  (:name "Cult Journeys\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
			  :and (:priority "A" :tag "cult" :tag "epic" :todo "TODO")
			  :transformer my/org-agenda-remove-todo-keyword)

		  (:name "Late tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                 :and (:scheduled past
                                  :not (:todo "DONE")
                                  :not (:tag "epic"))
				 :transformer my/org-agenda-add-deadline-prefix)

		  (:name "Backlog\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                 :and (:scheduled nil
                                  :not (:todo "DONE")
                                  :not (:tag "epic")
								  :not (:deadline t)
								  :not (:tag "goals")
								  )
				 :transformer my/org-agenda-remove-todo-keyword)

		  ;; Season Goals
		  (:name "Season Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
			 :and (:tag "goals" :not (:tag "family") :todo ("TODO" "DONE"))
			 :transformer my/org-agenda-remove-category
			 )

		  (:name "Family goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
			 :and (:tag "family" :tag "goals" :todo ("TODO" "DONE"))
			 :transformer my/org-agenda-remove-category
			 )

		  ;; Backlog Projects (non-priority epics)
		  (:name "Backlog Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
			  :and (:tag "epic" :not (:priority "A") :todo ("TODO" "WAIT"))
			  :transformer my/org-agenda-remove-todo-keyword)

		  ;; Discard everything else
		  (:discard (:anything t))))))
)

         ;; Agenda options for this view
         ((org-agenda-compact-blocks nil)))))

(add-to-list 'org-agenda-custom-commands
      '("nu" "Nubank Agenda"
         (
          ;; 📅 Deadlines
          (tags-todo "+DEADLINE>=\"<-60d>\"|+perfcycle"
                     ((org-agenda-overriding-header "📅 Deadlines\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                      (org-agenda-entry-types '(:deadline :scheduled))
                      (org-agenda-sorting-strategy '(deadline-up))
                      (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"DEADLINE\")) ")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-remove-tags t)))
          ;; ⏳ Week Tasks
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
          ;; 🧩 Grouped section — tags view but grouped
          (tags "LEVEL>=1+TODO=\"TODO\"|+TODO=\"WAIT\""
		((org-agenda-overriding-header "")
		 (org-agenda-sorting-strategy '(priority-down))
		 (org-agenda-skip-function #'lgm/org-skip-when-parent-scheduled)
		 (org-super-agenda-groups
		  '(
                    (:name "🌋 My Epics\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:tag "epic"
				      :tag "spinning"
				      :not (:tag "team"))
                           :transformer my/org-agenda-remove-todo-keyword
                           :order 0)

                    (:name "📊 Team Epics\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:tag "team" :tag "epic" :tag "spinning" :todo "TODO")
                           :transformer my/org-agenda-remove-todo-keyword
                           :order 1)

		    ;; --- Delegated (team+directreports OR delegated), TODO, not epic ---
                    (:name "🤝 Delegated Tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:not (:todo "DONE") :tag "delegated" :not (:tag "epic"))
                           :order 2)

                    (:name "⚡ High Priority Tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:todo "TODO"
                                 :priority "A"
				 :scheduled nil
				 :not (:tag "epic" :tag "delegated")
				 )
                           :and (:todo "TODO"
                                 :priority "A"
				 :scheduled past
				 :not (:tag "epic" :tag "delegated")
				 )
			   :transformer my/org-agenda-remove-todo-keyword
			   :order 3)

                    ;; --- Blocked/WAIT (exclude epics/team) ---
                    (:name "💀 Blocked\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:todo "WAIT"
                                 :not (:tag "epic")
                                 :not (:tag "team")
				 :scheduled past
				 )
			   :and (:todo "WAIT"
                                 :not (:tag "epic")
                                 :not (:tag "team")
				 :scheduled nil
				 )
                           :order 4)

                    (:name "🚚 Batch Tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:tag "batch"
                                 :not (:tag "epic")
                                 :not (:tag "delegated")
				 :scheduled nil)
                           :and (:tag "batch"
                                 :not (:tag "epic")
                                 :not (:tag "delegated")
				 :scheduled past)
                           :order 5)

                    ;; --- Tasks in Projects — TODO, not A, UNSCHEDULED, not epic/delegated ---
                    (:name "🚢 Tasks in Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                           :and (:tag "projects"
                                      :todo "TODO"
                                      :not (:priority "A")
                                      :scheduled nil
                                      :not (:tag "epic")
                                      :not (:tag "delegated"))
                           :and (:tag "projects"
                                      :todo "TODO"
                                      :not (:priority "A")
                                      :scheduled past
                                      :not (:tag "epic")
                                      :not (:tag "delegated"))
			   :transformer my/org-agenda-add-deadline-prefix
                                    :order 6)

                             ;; --- Tasks in Management — TODO, not A, UNSCHEDULED, not epic/delegated ---
                             (:name "💼 Tasks in Management\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                                    :and (:todo "TODO"
                                          :tag "manager"
                                          :not (:priority "A")
                                          :scheduled nil
                                          :not (:tag "epic")
                                          :not (:tag "cyclegoals")
                                          :not (:tag "delegated"))
                                    :and (:todo "TODO"
                                          :tag "manager"
                                          :not (:priority "A")
                                          :scheduled past
                                          :not (:tag "epic")
                                          :not (:tag "cyclegoals")
                                          :not (:tag "delegated"))
				    :transformer my/org-agenda-add-deadline-prefix
                                    :order 7)

                             ;; --- Education ---
                             (:name "📚 Education\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
                                    :and (:todo "TODO"
                                          :tag "education"
                                          :scheduled nil
                                          :not (:tag "epic")
                                          :not (:tag "cyclegoals")
					  )
                                    :and (:todo "TODO"
                                          :tag "manager"
                                          :scheduled past
                                          :not (:tag "epic")
                                          :not (:tag "cyclegoals")
					  )

                                    :order 8)

			   ;; Season Goals
			     (:name "🎯 Season Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
				    :and (:tag "cyclegoals")
				    :transformer my/org-agenda-clean-item
				    :order 9
				    )

			     ;; --- Late (exclude epics/team) ---
			     (:name "💀 Late, untracked or backlog \n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺"
				    :and (:todo "TODO"
					  :scheduled past
					  )
				    :and (:todo "TODO"
					  :scheduled nil
					  )
				    :order 10)

		  ;; Discard everything else
		  (:discard (:anything t))
		  ))))
	  )
         ((org-agenda-compact-blocks nil))))

;; Use when iterating to make it faster
(benchmark-run (org-agenda nil "d"))

(provide 'org-custom-agenda-opt-settings)
;;; org-custom-agenda-opt-settings.el ends here
