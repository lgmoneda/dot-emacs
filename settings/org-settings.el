;;; org-settings.el --- Settings for org utilities

;Sunday, August 20, 2017
;============================
;==        Org-mode        ==
;============================
(require 'org)

;; Properties I want subtrees to inherite
(setq org-use-property-inheritance '("TIMELINE_FACE"))
(setq org-use-property-inheritance t)

;; Capture
(setq org-directory "~/Dropbox/Agenda/")
(setq org-default-notes-file (concat org-directory "capture.org"))
(global-set-key (kbd "C-c c") 'org-capture)

;; Indent tasks
;; Old star color: 626483
(setq org-startup-indented t)

;; Init hiding everything
(setq org-startup-folded t)

;; Always hide stars
(setq org-hide-leading-stars t)

;; Fix helm-org-heading style
(setq helm-org-headings-fontify t)

;; No blank lines between headers
(setq org-cycle-separator-lines 0)

;; Show deadlines 30 days before
(setq org-deadline-warning-days 60)

;; Consider everything under the tree to todo statistics
(setq org-hierarchical-todo-statistics nil)

;; Add close time when changing to DONE
(setq org-log-done 'time)

;; Configs to use org mode to track time in tasks

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

(defun lgm/clock-in-when-started ()
"Automatically clock in a task when status is changed to STARTED"
    (when (string= org-state "STARTED")
      (org-clock-in)))

(add-hook 'org-after-todo-state-change-hook 'lgm/clock-in-when-started)

;; Easy jump, clock in and clock out
(global-set-key (kbd "<f12>") 'org-clock-goto)
(global-set-key (kbd "C-<f12>") 'org-clock-in)
(global-set-key (kbd "M-<f12>") 'org-clock-out)

;; Wrap clock tags in logbook
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)

;; Change viewer apps C-c C-o
;; When not in MAC
(unless (string-equal system-type "darwin")
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "xdg-open %s")
        ("\\.pdf\\'" . "evince \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
        ("\\.pdf.xoj" . "xournal %s")))
      )

;;(add-hook 'org-trigger-hook 'lgm/clock-in-when-started)
;; From cashestocashes.com
;; Once you've included this, activate org-columns with C-c C-x C-c while on a top-level heading, which will allow you to view the time you've spent at the different levels (you can exit the view by pressing q)
;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16CLOSED")

;; New states to to-do
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)" "INACTIVE(i)" "FAIL(f)")))

(setq org-todo-keyword-faces
      '(
	;; ("TODO" . org-warning)
	("NEXT" . "pink")
	("STARTED" . "yellow")
	("WAIT" . "purple")
	("INACTIVE" . (:foreground "grey"))
        ("CANCELED" . (:foreground "blue" :weight bold))
        ("FAIL" . (:foreground "blue" :weight bold))))

;; No line number in org mode, please
;;(add-hook 'org-mode-hook (linum-mode 0))
;; Open org-agenda
;; (add-hook 'after-init-hook 'org-agenda-list)
(setq org-agenda-block-separator "-")

(org-defkey org-mode-map (kbd "C-S-s /") 'helm-org-agenda-files-headings)
;; (org-defkey org-mode-map (kbd "C-S-s /") 'counsel-org-agenda-headlines)

(defun org-tell-me-first-header ()
  (interactive)
  (save-excursion
    (outline-up-heading 3)
    (print (substring-no-properties (org-get-heading t t)))
  )
 )

;; TODO entry automatically change to done when all children are done (from orgmode.org)
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; https://github.com/gregsexton/ob-ipython
;; (use-package ob-ipython
;;   :ensure t
;;   :init
;;   ;; (setq ob-ipython-resources-dir (no-littering-expand-var-file-name "obipy-resources"))
;;   :config
;;   (require 'ob-ipython))

;; ;; to redefine images from evaluating code blocks
;; After executing code, it displays the image
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (python . t)
   (shell . t)
   (dot . t)
   (sql . nil)
   (sqlite . t)))

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/miniconda2/envs/ml3")

;Sunday, December 10, 2017
;============================
;==    Org beautifying     ==
;============================

;; set the fall-back font
;; this is critical for displaying various unicode symbols, such as those used in my init-org.el settings
;; http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;; Set font size
(set-face-attribute 'default nil :height 110)

;; Setting English Font
;; (set-face-attribute
;;   'default nil :stipple nil :height 130 :width 'normal :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal :foundry "outline" :family "DejaVu Sans Mono for Powerline")

;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; use org-bullets-mode for utf8 symbols as org bullets
(use-package org-bullets
	:ensure t)
;;(require 'org-bullets)
;; make available "org-bullet-face" such that I can control the font size individually
(setq org-bullets-face-name (quote org-bullet-face))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

;; org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola (my fall-back font) ⬎, ⤷, ⤵
(setq org-ellipsis " ▼")


(set-display-table-slot standard-display-table
                        'selective-display (string-to-vector " ◦◦◦ ")) ; or whatever you like


;; Agenda views / configs

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))
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

;; Org Journal
(use-package org-journal
  :ensure t
  :init (setq org-journal-dir "~/Dropbox/Agenda/Journal"))


;; Build progress bars
;; #+BEGIN: block-dashboard
;; #+END:
;; C-c C-c to build/update bars
;; (use-package org-dashboard
;;    :ensure t)

(setf org-journal-dir "~/Dropbox/Agenda/Journal")
(setf org-journal-file-format "my-journal.org")
(setf org-journal-date-prefix "* ")
(setf org-journal-time-prefix "** ")

;; Custom function to add today date - TIL
(defun lgm/org-add-today-as-entry ()
  (interactive)
  ;; empty file? Add a date timestamp
  (let* ((time (current-time)))
  (insert "\n")
  (insert org-journal-date-prefix
	  (format-time-string org-journal-date-format time)))
  )

;; Make Org Journal remember me about
;; writing my day thoughts like in Memento mode
(defcustom journal-file "~/Dropbox/Agenda/Journal/my-journal.org"
  "Customizable variable to specify any file, which will be used for Memento."
  :type 'string
  :group 'journal)

(defun lgm/org-journal-new-today-entry (prefix &optional event)
  "Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time heading. If a
prefix is given, don't add a new heading."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (current-time)))
    (lgm/org-journal-new-entry prefix time)))

(defun lgm/org-journal-new-entry (prefix &optional time)
  "Open today's journal file and start a new entry.
Giving the command a PREFIX arg will just open a today's file,
without adding an entry. If given a TIME, create an entry for the
time's day.

Whenever a journal entry is created the
`org-journal-after-entry-create-hook' hook is run"
  (interactive "P")
  (org-journal-dir-check-or-create)
  (let* ((entry-path (org-journal-get-entry-path time))
         (should-add-entry-p (not prefix)))

    ;; open journal file
    (unless (string= entry-path (buffer-file-name))
      (funcall org-journal-find-file entry-path))
    (org-journal-decrypt)
    (goto-char (point-max))
    (let ((unsaved (buffer-modified-p))
          (new-file-p (equal (point-max) 1)))

      ;; empty file? Add a date timestamp
      (insert "\n")
      (insert org-journal-date-prefix
              (format-time-string org-journal-date-format time))

      ;; add crypt tag if encryption is enabled and tag is not present
      (when org-journal-enable-encryption
        (goto-char (point-min))
        (unless (member org-crypt-tag-matcher (org-get-tags))
          (org-set-tags-to org-crypt-tag-matcher))
        (goto-char (point-max)))

      ;; move TODOs from previous day here
      (when (and new-file-p org-journal-carryover-items)
        (save-excursion (org-journal-carryover)))
      (print "dbuga")
      ;; insert the header of the entry
      (when should-add-entry-p
        (unless (eq (current-column) 0) (insert "\n"))
        (let ((timestamp (if (= (time-to-days (current-time)) (time-to-days time))
                             (format-time-string org-journal-time-format)
                           "")))
          (insert "\n" org-journal-time-prefix timestamp))
        (run-hooks 'org-journal-after-entry-create-hook))

      ;; switch to the outline, hide subtrees
      (org-journal-mode)
      (if (and org-journal-hide-entries-p (org-journal-time-entry-level))
          (outline-hide-sublevels
	   (- (org-journal-time-entry-level) 1))
	  (show-all)
	  )

      ;; open the recent entry when the prefix is given
      (when should-add-entry-p
        (show-entry))

      (set-buffer-modified-p unsaved)

      ;; Isolate it and use the write mode
      (delete-other-windows)
      (writeroom-mode)

      )))

(defun journal-get-modification-date ()
  "Returns the last modified date of the current memento file."
  (format-time-string "%Y-%m-%d"
                      (nth 5 (file-attributes journal-file))))

(defun journal-check-when-quit ()
  (interactive)
  (if (file-exists-p journal-file)
      ;; Check if there was a log written today. If this is not the case, then check if it's already tonight except the night.
      (if (and (string< (journal-get-modification-date) (format-time-string "%Y-%m-%d")) (or (string< (format-time-string "%k") " 6") (string< "20" (format-time-string "%k"))))
          ;; Invoke Memento if the user wants to proceed.
          (if (yes-or-no-p "Do you want to write your Journal?")
              (progn (call-interactively 'lgm/org-journal-new-today-entry)
		     (keyboard-quit)
		     )
	      ))
    ;; If the Memento file doesn't exist yet, create a file and proceed with creating a log.
    (write-region "" nil journal-file)
    (progn (call-interactively 'lgm/org-journal-new-today-entry))))

(add-to-list 'kill-emacs-hook 'journal-check-when-quit)

(defun lgm/copy-org-block ()
  "Place the mark in a org head, reveal its content and use
this command to copy it"
  (interactive)
  (next-line)
  (org-beginning-of-line)
  (set-mark-command nil)
  (org-next-visible-heading 1)
  (previous-line)
  (org-end-of-line)
  (kill-ring-save (mark) (point))
  (org-previous-visible-heading 1)
  (message "You're awesome!"))

(global-set-key (kbd "C-c b") (quote lgm/copy-org-block))

;; Keeps the agenda view nice
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)))

;; (use-package org-gcal
;;   :ensure t
;;   :init
;;   (setq org-gcal-notify-p nil)
;;   )

;; ;; Load gcalsync
;; (load "~/Dropbox/Projetos/Emacs/.gcalsync.el")

(use-package alert
  :commands (alert)
  )

(use-package org-alert
  :init
  :ensure t
  )

;; Start with my to-do
;; The org mode file is opened with
(find-file "~/Dropbox/Agenda/todo.org")
(switch-to-buffer "todo.org")
(setq org-agenda-window-setup 'other-window)
;; (org-agenda-list)

(add-to-list 'org-agenda-files  "~/Dropbox/Agenda/todo.org" "~/Dropbox/Agenda/finances.org")

(defvar org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("b" "Blank" entry (file org-default-notes-file)
	  "* %?\n%u")
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("D" "Daily Log" entry (file "~/org/daily-log.org")
	  "* %u %?\n*Summary*: \n\n*Problem*: \n\n*Insight*: \n\n*Tomorrow*: " :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%u" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))

(setq org-lowest-priority ?D)
(setq org-default-priority ?D)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; From https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-block-separator " ")
(setq org-agenda-custom-commands
      '(("d" "Daily agenda and NEXTs!"
         (
	  ;; Deadlines
	  (tags "+DEADLINE>=\"<-2m>\"&DEADLINE<=\"<+2m>\""
               ((org-agenda-overriding-columns-format
                 "%25ITEM %DEADLINE %TAGS")
				(org-agenda-overriding-header "Deadlines in the next 60 days\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
				(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				(org-agenda-remove-tags t)
				(org-agenda-entry-types '(:deadline :scheduled))
				(org-agenda-sorting-strategy '(deadline-up))
				(org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"DEADLINE\")) ")
				))

	  ;; Self-improvement top of mind
	  (tags "+selfdevelopment+TODO=\"TODO\""
		(
		 (org-agenda-overriding-header "Self-development hint\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format " ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; (agenda ""
	  ;; 	  ((org-agenda-time-grid nil)
	  ;; 	   (org-agenda-span 'day)
	  ;; 	   (org-deadline-warning-days 60)
	  ;; 	   (org-agenda-remove-tags t)
	  ;; 	   (org-agenda-entry-types '(:deadline))
	  ;; 	   (org-agenda-sorting-strategy '(deadline-up))
	  ;; 	   (org-agenda-format-date "")
	  ;; 	   (org-agenda-overriding-header "Deadlines in the next 60 days\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
	  ;; 	   ))

	  ;; Week tasks and deadlines
	  (agenda ""
	  	  ((org-agenda-time-grid nil)
		   (org-agenda-remove-tags t)
	  	   (org-agenda-span 'week)
	  	   (org-deadline-warning-days 0)
	  	   (org-deadline-past-days 0)
		   (org-scheduled-past-days 0)
	  	   (org-agenda-entry-types '(:deadline :scheduled))
	  	   ;; (org-agenda-sorting-strategy '(deadline-up))
	  	   (org-agenda-overriding-header "Week tasks\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		   (org-agenda-scheduled-leaders '("" ""))
		   (org-agenda-prefix-format "    %i %-12:c")
		   (org-agenda-skip-function '(air-org-skip-subtree-if-habit))
	  	   ))

	  ;; High priority tasks
	  (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-remove-tags t)
		 (org-agenda-todo-keyword-format "")
		 ))
	  ;; Today Agenda
          ;; (agenda ""
	  ;; 	  ((org-agenda-ndays 5)
	  ;; 	      (org-agenda-span 'day)
	  ;; 	      (org-agenda-time-grid nil)
	  ;; 	      (org-deadline-warning-days 0)
	  ;; 	      (org-agenda-skip-scheduled-delay-if-deadline t)
	  ;; 	      (org-agenda-todo-ignore-scheduled t)
	  ;; 	      (org-agenda-scheduled-leaders '("" ""))
	  ;; 	      (org-agenda-tags-todo-honor-ignore-options t)
	  ;; 	      (org-agenda-overriding-header "Today Agenda:")
	  ;; 	      )
	  ;; 	  )

	  ;; NEXT Nubank Tasks
        ;;   (tags-todo "+nubank+TODO=\"NEXT\""
	  	;; (
	  	;;  (org-agenda-category-filter-preset (quote ("+Nubank")))
	  	;;  (org-agenda-overriding-header "Next tasks at Nubank:")
	  	;;  (org-agenda-overriding-columns-format "%20ITEM %SCHEDULED")
	  	;;  (org-agenda-sorting-strategy '(category-keep))
	  	;;  (org-agenda-view-columns-initially t)
		;;  (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
	  	;;  )

	  	;; )

	  ;; NEXT Master
          (tags "+usp+TODO=\"NEXT\""
		(
		 (org-agenda-overriding-header "Next tasks in Master\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; NEXT Study
          (tags "+study+TODO=\"NEXT\""
		(
		 (org-agenda-overriding-header "Next tasks in Study\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; NEXT Projects
          (tags "+projects+TODO=\"NEXT\""
		(
		 (org-agenda-overriding-header "Next task in Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; NEXT Work
          (tags "+work+TODO=\"NEXT\"|udacity+TODO=\"NEXT\""
		(
		 (org-agenda-overriding-header "Next task in Work\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; NEXT Kaggle
          (tags "+kaggle+TODO=\"NEXT\""
		((org-agenda-overriding-header "Next tasks in Kaggle\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; NEXT Life
          (tags "+life-goals2020+TODO=\"NEXT\""
		((org-agenda-overriding-header "Next tasks in Life\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-prefix-format "%?-16 (scheduled-or-not (org-entry-get (point) \"SCHEDULED\")) ")
		 (org-agenda-remove-tags t)
		 (org-agenda-sorting-strategy '(scheduled-up))
		 (org-agenda-todo-keyword-format "")
		 )
		)

	  ;; Blocked Nubank
	  ;; (tags "+nubank+TODO=\"WAIT\""
	  ;; 	((org-agenda-skip-function 'my-skip-unless-waiting)
          ;;   (org-agenda-category-filter
	  ;;    (quote
	  ;;     ("+Nubank")))
	  ;;   (org-agenda-overriding-header "Blocked Nubank Tasks: "))

	  ;; 	)

	  ;; TODO Nubank
	;;   (tags "+nubank+LEVEL=3+TODO=\"TODO\""
	;; 	((org-agenda-skip-function 'my-skip-unless-waiting)
    ;;         (org-agenda-category-filter
	;; (quote
	;;  ("+Nubank")))
	;;     (org-agenda-overriding-header "Nubank Tasks: "))

	;; 	)

	  ;; Mid and low priority tasks
	  ;; (tags "+PRIORITY={B}"
          ;;       ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          ;;        (org-agenda-overriding-header "Mid or low-priority tasks:")))



	  ;; Long deadlines
	  ;; (agenda ""
	  ;; 	  ((org-agenda-time-grid nil)
	  ;; 	   (org-agenda-span 'day)
	  ;; 	   (org-deadline-warning-days 90)
	  ;; 	   (org-agenda-entry-types '(:deadline))
	  ;; 	   (org-agenda-sorting-strategy '(deadline-up))
	  ;; 	   (org-agenda-overriding-header "Deadlines in the next 90 days:")
	  ;; 	   ))


	  ;; Year Goals Milestones
	  ;; (tags "-nubank+goals2020+TODO=\"NEXT\""
	  ;; 	((org-agenda-category-filter "-Nubank")
	  ;; 	 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
	  ;; 					(air-org-skip-subtree-if-priority ?A)
	  ;; 					(org-agenda-skip-if nil '(scheduled deadline))))
	  ;;   (org-agenda-overriding-header "2020 Goals Milestones: ")
	  ;;   )
	  ;; 	)

	  ;; Backlog projects
	  (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Backlog Projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-remove-tags t)
		 (org-agenda-todo-keyword-format "")
		 ))

	  ;; Year Goals General
	  (tags "+goals2020+LEVEL=3+TODO=\"TODO\"|+goals2020+LEVEL=3+TODO=\"DONE\""
		((org-agenda-category-filter "-Nubank")
		 (org-agenda-prefix-format " ")
		 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
						(air-org-skip-subtree-if-priority ?A)
						(org-agenda-skip-if nil '(scheduled deadline))))
		 (org-agenda-overriding-header "2020 Goals\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-remove-tags t)

	    )
		)

	  ;; Medium-low priority tasks
	  (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Not-priority projects\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
		 (org-agenda-remove-tags t)
		 (org-agenda-todo-keyword-format "")
		 ))

	  ;; Next few days
          ;; (agenda "" ((org-agenda-ndays 2)
	  ;; 	      (org-agenda-span 3)
	  ;; 	      (org-agenda-start-day "+1d")
	  ;; 	      (org-agenda-scheduled-leaders '("" ""))
	  ;; 	  (org-agenda-overriding-header "Next few days:"))
	  ;; 	  )

	  ;; All next tasks
	  ;; (tags "-goals2020+TODO=\"NEXT\""
	  ;; 	(
	  ;; 	 (org-agenda-tags-todo-honor-ignore-options :scheduled)
	  ;; 	 (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
	  ;; 					(air-org-skip-subtree-if-priority ?A)
	  ;; 					(org-agenda-skip-if nil '(scheduled deadline))))
	  ;; 	 (org-agenda-overriding-header "Next tasks NOT SCHEDULED:")
	  ;; 	 (org-agenda-remove-tags t)
	  ;;   )
	  ;;  )
	  )
         ((org-agenda-compact-blocks nil))
	 )

    ;; 	("n" "Next Nubank tasks" todo "NEXT"
    ;; ((org-agenda-skip-function 'my-skip-unless-waiting)
    ;;         (org-agenda-category-filter-preset
    ;; 	(quote
    ;; 	 ("+Nubank")))
    ;; 	    (org-agenda-overriding-header "Nu Agenda: "))

    ;; )

	))

;; Enable the usage of two agenda views at the same time
(org-toggle-sticky-agenda)

;; Shortcut to display day activity
(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n"))

(defun scheduled-or-not (resp)
  (interactive)
  (if resp
    ;;'"OK"
    ;;(format-time-string "%Y-%m-%d" (org-time-string-to-time resp))
    (concat "In " (number-to-string (org-time-stamp-to-now resp)) " day(s)")
    '"Not Scheduled"
    )
)

;; Only deadlines view
(add-to-list 'org-agenda-custom-commands
             '("z" "Deadlines"
               tags "+DEADLINE>=\"<today>\"&DEADLINE<=\"<+2m>\""
               ((org-agenda-overriding-columns-format
                 "%25ITEM %DEADLINE %TAGS")))
             )

;; Compact only day view
(add-to-list 'org-agenda-custom-commands
             '("l" "Compact today"
	       agenda "" ((org-agenda-ndays 5)
			  (org-agenda-span 'day)
			  (org-deadline-warning-days 0)
			  (org-agenda-skip-scheduled-delay-if-deadline t)
			  (org-agenda-todo-ignore-scheduled t)
			  (org-agenda-scheduled-leaders '("" ""))
			  (org-agenda-tags-todo-honor-ignore-options t)
			  (org-agenda-overriding-header "Today Agenda:")
			  )
	       )
	     )

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
                       ))
          )
	       )
	     )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal notifier
;; requires 'brew install terminal-notifier'
;; stolen from erc-notifier


;; Change duration: defaults write com.apple.notificationcenterui bannerTime 25
(defun terminal-notifier-notify (title message)
  "Show a message with terminal-notifier-command."
  (start-process "terminal-notifier"
                 "terminal-notifier"
                 "terminal-notifier"
                 "-title" title
                 "-message" message
		 "-sound" "default"
                 "-sender" "org.gnu.Emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Warns every N minutes about all the deadlines and scheduled tasks
;; for the current day
;; (use-package org-alert
;; 	     :ensure t)

;; https://github.com/akhramov/org-wild-notifier.el

(use-package org-wild-notifier
	     :ensure t
	     :init ;; (org-wild-notifier-mode)
	     (setq org-wild-notifier-keyword-whitelist '("TODO" "NEXT"))
	     (setq org-wild-notifier-alert-time 5)
	     )

(defun remind-me-daily (fn time msg wavfile box)
  (when (and (boundp 'daily-reminder)
             (timerp daily-reminder))
    (cancel-timer daily-reminder))
  (let ((daily (* 60 60 24)))
    (setq daily-reminder
          (run-at-time time daily 'funcall fn msg wavfile box))))

(defun reminder-fn (msg wavfile box)
  (if wavfile
      (start-process-shell-command "lolsound" nil (concat "mplayer ~/.emacs.d/sounds/" wavfile))
      )
  (if box
      (terminal-notifier-notify "Emacs Notification" msg)
      (message msg)))

(remind-me-daily 'reminder-fn "10:30pm" "Bedtime!" nil t)

(defun my-terminal-notifier-notify (info)
  "Show a message with terminal-notifier-command."
  (start-process "terminal-notifier"
                 "terminal-notifier"
                 "terminal-notifier"
                 "-title" (plist-get info :title)
                 "-message" (plist-get info :message)
		 "-sound" "default"
                 "-sender" "org.gnu.Emacs"))

(alert-define-style 'style-name :title "My Style's title"
                    :notifier
		    'my-terminal-notifier-notify
                    ;; Removers are optional.  Their job is to remove
                    ;; the visual or auditory effect of the alert.
                    :remover
                    (lambda (info)
                      ;; It is the same property list that was passed to
                      ;; the notifier function.
                      ))

(setq alert-default-style 'style-name)

(defvar yt-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"https://www.youtube.com/embed/%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))


;; This should work :(
(add-hook 'org-finalize-agenda-hook
    (lambda ()
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "nu-agenda:" nil t)
          (add-text-properties (match-beginning 0) (point-at-eol)
             '(face (:foreground "green"))))
        (goto-char (point-min))
        (while (re-search-forward "work:" nil t)
          (add-text-properties (match-beginning 0) (point-at-eol)
			       '(face (:foreground "purple")))))))

;; Remember me to take a look at my agenda before quitting emacs,
;; so I check what I've done properly.
(defun agenda-check-when-quit ()
  (interactive)
  (if (yes-or-no-p "Would like to check your agenda before you go?")
	  (progn (call-interactively '(lambda() (interactive)(org-agenda 0 "l")))
		     (keyboard-quit)
		     )
	  ))

;; (add-to-list 'kill-emacs-hook 'agenda-check-when-quit)

;; (use-package calfw
;;   :ensure t)

;; (use-package calfw-org
;;   :ensure t)

(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))

(defun lgm/next-nu ()
  (interactive)
  (org-agenda nil "n")
  )

;; Look for it later
(use-package org-timeline
	     :ensure t)

;;(load-file "~/.emacs.d/elpa/org-timeline/org-timeline.el")
;;(add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)

(defun lgm/show-timeline ()
  (interactive)
  (org-timeline-insert-timeline))

(defun lgm/planner-mode ()
  (interactive)
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;; Always split stuff vertically
(setq split-height-threshold nil)

(defun lgm/write-til ()
  (interactive)
  (call-interactively '(lambda() (interactive)
			      (find-file "~/Dropbox/Agenda/til-2020.org")
			      (call-interactively 'lgm/org-add-today-as-entry)
			      )
		      )
  )

;; Ask me to write down my TIL if it's after 9 pm
(defun write-your-til ()
  (interactive)
  (if (or (string< (format-time-string "%k") " 6") (string< "20" (format-time-string "%k")))
	  (if (yes-or-no-p "Would like to write at your TIL?")
	      (progn (lgm/write-til)
		     (keyboard-quit)
				 )
		  )))

;; (add-to-list 'kill-emacs-hook 'write-your-til)

;; Open todo.org
;;(global-set-key (kbd "<f10>") (lambda() (interactive)(find-file "~/Dropbox/Agenda/todo.org")))
;; Open agenda
(global-set-key (kbd "C-<f10>") (lambda() (interactive)(org-agenda nil "d")(org-agenda-redo) (text-scale-decrease 1)))
(global-set-key (kbd "<f10>") (lambda() (interactive)(org-agenda nil "d")(org-agenda-redo)))
;; Initialize with agenda view
(add-hook 'after-init-hook (lambda () (org-agenda nil "d") (org-agenda-redo)))
;; Open day
(setq org-agenda-span 'day)
(global-set-key (kbd "C-c <f10>") (lambda() (interactive)(org-agenda 0 "a")))
(global-set-key (kbd "M-<f10>") (lambda() (interactive)(org-agenda 0 "l")))

;; Change buffer functionality
(org-defkey org-mode-map (kbd "C-<tab>") (lambda ()
												 (interactive)
												 (other-window -1)))

;; (customize-set-value
;;     'org-agenda-category-icon-alist
;;     `(
;;       ("Work" "~/.emacs.d/icons/money-bag.svg" nil nil :ascent center)
;;       ;; ("piano" ,(concat emoji-dir "1f3b9.png") nil nil :ascent center)
;;       ))
(defun lgm/python-org-code-block()
  (interactive)
  (open-line 2)
  (insert "#+begin_src python")
  (next-line)
  (insert "#+end_src")
  (previous-line)
  (end-of-line)
  (split-line)
  (next-line)
  (beginning-of-line)
   )

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/agenda/roam")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
			   ("C-c n b" . helm-bibtex)
               ("C-c n u" . org-roam-unlinked-references)
               ("C-c n g" . org-roam-graph-show))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))
			  (("C-c n s" . lgm/screenshot-to-org-link))
			  (("C-c n r" . ivy-bibtex))
			  (("C-c n c" . lgm/python-org-code-block))

	      )

	  :init
	  (setq org-roam-capture-templates
			'(("d" "default" plain (function org-roam--capture-get-point)
			   "%?"
			   :file-name "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
			   :head "#+title: ${title}
#+STARTUP: inlineimages latexpreview
#+roam_tags: "
			   :unnarrowed t)
;; 			  ("p" "paper" plain (function org-roam--capture-get-point)
;; 			   "%?"
;; 			   :file-name "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
;; 			   :head "#+title: ${title}
;; #+STARTUP: inlineimages latexpreview
;; #+roam_tags: paper
;; Authors:
;; Link: "
;; 			   :unnarrowed t)

;; 			  ("b" "book" plain (function org-roam--capture-get-point)
;; 			   "%?"
;; 			   :file-name "%(format-time-string \"%Y%m%d%H%M%S-${slug}\" (current-time) t)"
;; 			   :head "#+title: ${title}
;; #+STARTUP: inlineimages
;; #+roam_tags: book
;; Authors: "
;; 			   :unnarrowed t)
			  )))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; Function inspired by https://llazarek.com/2018/10/images-in-org-mode.html
;; check org-download for a more complete solution of it
(defun lgm/screenshot-to-org-link (&optional arg)
  (interactive)
  (unless (or arg
              (file-directory-p (concat (file-name-directory buffer-file-name) "/resources")))
    (make-directory (concat (file-name-directory buffer-file-name) "/resources")))
  (let* ((default-dest
           (format-time-string (concat (file-name-directory buffer-file-name) "/resources/screen_%Y%m%d_%H%M%S.jpg")))
         (dest (if arg
                   (helm-read-string "Save to: " default-dest)
                 default-dest)))
    (start-process "screencapture" nil "screencapture" "-i" dest)
    (read-char "Taking screenshot... Press any key when done.")
    (org-insert-link t (concat "file:" dest) "")
    (org-remove-inline-images)
    (org-display-inline-images)))

;; Company backend for org-roam links. Helps me to link ideas when useful.
;; (use-package company-org-roam
;;   :ensure t
;;   ;; You may want to pin in case the version from stable.melpa.org is not working
;;   ; :pin melpa
;;   :config
;;   (push 'company-org-roam company-backends))

;; (add-hook 'org-roam-mode-hook 'company-mode)

;; Increase latex preview font
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; Olivetti
;; Look & Feel for long-form writing

(use-package olivetti
  :ensure t)

;; Set the body text width
(setq olivetti-body-width 0.6)

;; Enable Olivetti for text-related mode such as Org Mode
(add-hook 'text-mode-hook 'olivetti-mode)

(add-hook 'org-mode-hook
	    (lambda ()
	      (make-local-variable 'company-backends)
	      (make-local-variable 'company-idle-delay)
	      (make-local-variable 'company-minimum-prefix-length)
	      (setq company-backends '(company-org-roam))
	      (setq company-idle-delay 0
		    company-minimum-prefix-length 3)))

;; Xunxo to disable in my todo file
(progn
  (switch-to-buffer "todo.org")
  (company-mode -1)
  )

;; Change background color of code blocks in org mode
(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 10))

;; Avoid org asking if I want to run code
(setq org-confirm-babel-evaluate nil)

;; org-roam-bib
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  ;; :init (setq org-ref-notes-function 'orb-notes-fn)
  ;; (setq bibtex-completion-find-note-functions 'orb-notes-fn)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${citekey}"
         :head "#+TITLE: ${title}, ${author-abbrev}\n#+ROAM_KEY: ${ref}\n#+Authors: ${author}\n#+STARTUP: inlineimages latexpreview\n#+roam_tags: bibliographical-notes " ; <--
         :unnarrowed t)))

(provide 'org-settings)
;;; org-settings.el ends here
