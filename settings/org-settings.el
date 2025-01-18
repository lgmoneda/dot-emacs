;;; org-settings.el --- Settings for org utilities

;Sunday, August 20, 2017
;============================
;==        Org-mode        ==
;============================

;; Properties I want subtrees to inherite
(setq org-use-property-inheritance '("TIMELINE_FACE"))
(setq org-use-property-inheritance t)

;; Capture
(setq org-directory "~/Dropbox/Agenda/")
(setq org-default-notes-file (concat org-directory "capture.org"))
(global-set-key (kbd "C-c c") 'org-capture)

;; Indent tasks
(setq org-startup-indented t)

;; Init hiding everything
(setq org-startup-folded t)

;; Always hide stars
(setq org-hide-leading-stars t)

;; Fix helm-org-heading style
(setq helm-org-headings-fontify t)

;; No blank lines between headers
(setq org-cycle-separator-lines 0)

;; Show deadlines 60 days before
(setq org-deadline-warning-days 60)

;; Consider everything under the tree for todo statistics
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

;; Add clocked-in task to the menubar
(defun my-clock-string ()
  (let ((clock-string (substring-no-properties (org-clock-get-clock-string))))
    (if (string-equal clock-string "")
        "Clock is not running"
      clock-string)))

;; It is needed to start the server
(server-start)

;; Call it using xbar, with a org-clock.ls.sh on xbar's folder
;; emacsclient --eval "(my-clock-string)" | sed 's/["()]//g'

;; Change viewer apps C-c C-o
;; When not in MAC
(unless (string-equal system-type "darwin")
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "xdg-open %s")
        ("\\.pdf\\'" . "evince \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
        ("\\.pdf.xoj" . "xournal %s")
	))
)

(setq org-file-apps
        (append org-file-apps
                '(
                  ("\\.mp3\\'" . (lambda (file) (emms-play-file file)))
                  ("\\.mp4\\'" . (lambda (file) (emms-play-file file))))))

;; From cashestocashes.com
;; Once you've included this, activate org-columns with C-c C-x C-c while on a top-level heading, which will allow you to view the time you've spent at the different levels (you can exit the view by pressing q)
;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16CLOSED")

;; New states to to-do
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "DELEGATED(e)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)" "INACTIVE(i)" "FAIL(f)")))

;; (setq org-todo-keyword-faces
;;       '(
;; 	("NEXT" . "pink")
;; 	("STARTED" . "yellow")
;; 	("WAIT" . "magenta")
;; 	("INACTIVE" . (:foreground "grey"))
;;         ("CANCELED" . (:foreground "blue" :weight bold))
;;         ("FAIL" . (:foreground "blue" :weight bold))))

;; TODO entry automatically change to done when all children are done (from orgmode.org)
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; org-async to execute code blocks async
(use-package ob-async
  :ensure t
  :config
  (require 'ob-async)
  (setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter"))
  )

;; (use-package jupyter
;;   :demand t
;;   :after (:all org python))

(use-package simple-httpd
  :ensure t)

(add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/jupyter-20241004.241/")
(autoload 'jupyter "jupyter" "" t)
;; (require 'jupyter)
(load "jupyter")

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t))

(pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")

;; Fix for 404 error
(defun gm/jupyter-api-request-xsrf-cookie-error-advice (func &rest args)
  (condition-case nil
      (apply func args)
    (jupyter-api-http-error nil)))
(advice-add 'jupyter-api-request-xsrf-cookie :around #'gm/jupyter-api-request-xsrf-cookie-error-advice)

;; Org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (python . t)
   (jupyter . t)
   (emacs-lisp . t)
   ;; Problems with orb capture
   (shell . t)
   (dot . t)
   (sql . nil)
   (sqlite . t)))

;; (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
;; 													 (:result . "both")
;; 													 (:pandoc . "t")
;;                                                      (:session . "py-edge")
;;                                                      (:kernel . "edge")))

;; Copied from scimax https://github.com/jkitchin/scimax
(setq org-babel-default-header-args:jupyter-python
      '((:async . "yes")
		(:results . "both")
		(:session . "py-edge")
		(:kernel . "edge")
		(:pandoc . "t")
		(:exports . "both")
		(:cache .   "no")
		(:noweb . "no")
		(:hlines . "no")
		(:tangle . "no")
		(:eval . "never-export")))

;; * Handling ansi codes

(defun scimax-jupyter-ansi ()
    (interactive)
  "Replaces ansi-codes in exceptions with colored text.
I thought emacs-jupyter did this automatically, but it may only
happen in the REPL. Without this, the tracebacks are very long
and basically unreadable.
We also add some font properties to click on goto-error.
This should only apply to jupyter-lang blocks."
  (when (string-match "^jupyter" (car (or (org-babel-get-src-block-info t) '(""))))
    (let* ((r (org-babel-where-is-src-block-result))
	   (result (when r
		     (save-excursion
		       (goto-char r)
		       (org-element-context)))))
      (when result
	(ansi-color-apply-on-region (org-element-property :begin result)
				    (org-element-property :end result))

	;; Let's fontify "# [goto error]" to it is clickable
	(save-excursion
	  (goto-char r)
	  (when (search-forward "# [goto error]" (org-element-property :end result) t)
	    (add-text-properties
	     (match-beginning 0) (match-end 0)
	     (list 'help-echo "Click to jump to error."
		   'mouse-face 'highlight
		   'local-map (let ((map (copy-keymap help-mode-map)))
				(define-key map [mouse-1] (lambda ()
							    (interactive)
							    (search-backward "#+BEGIN_SRC")
							    (scimax-jupyter-jump-to-error)))
				map))))))

      t)))


(add-to-list 'org-babel-after-execute-hook 'scimax-jupyter-ansi t)

;; ;; to redefine images from evaluating code blocks
;; After executing code, it displays the image
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Latex in org
(setq exec-path (append exec-path '("/Library/TeX/texbin/latex")))
(setq exec-path (append exec-path '("/usr/local/Cellar/imagemagick/7.1.0-55/bin/magick")))

;; ;; Add Tikz
(add-to-list 'org-latex-packages-alist
             '("" "tikz" t))

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(setq org-latex-create-formula-image-program 'imagemagick)
;; (add-to-list ' org-preview-latex-process-alist
;;              'imagemagick)

(use-package virtualenvwrapper
  :ensure t)

(setq python-shell-interpreter "python3")
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/Users/luis.moneda/miniconda3/envs/edge")


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
(set-face-attribute 'default nil :height 130)

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

;; make available "org-bullet-face" such that I can control the font size individually
(setq org-bullets-face-name (quote org-bullet-face))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

;; org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola (my fall-back font) ⬎, ⤷, ⤵
(setq org-ellipsis " ▼")

;; Prettify symbols
(add-hook 'org-mode-hook (lambda ()
						   (push '("#+TITLE: "        . "") prettify-symbols-alist)
						   (push '("#+title: "        . "") prettify-symbols-alist)
						   (push '("#+subtitle: "     . "") prettify-symbols-alist)
						   (push '("#+author: "       . "- ") prettify-symbols-alist)
						   (push '("#+AUTHOR: "       . "- ") prettify-symbols-alist)
						   (push '("#+Authors: "       . "- ") prettify-symbols-alist)
						   (push '(":properties:"     . "↩") prettify-symbols-alist)
						   (push '(":PROPERTIES:"     . "↩") prettify-symbols-alist)
						   (push '("#+begin_src"      . "λ") prettify-symbols-alist)
						   (push '("#+roam_tags:"      . "#") prettify-symbols-alist)
						   (push '("#+filetags:"      . "#") prettify-symbols-alist)
						   (push '("#+end_src"        . "src") prettify-symbols-alist)
						   (push '("#+results:"       . "»") prettify-symbols-alist)
						   (push '("#+STARTUP:"       . "»") prettify-symbols-alist)
						   (push '(":end:"            . "⋱") prettify-symbols-alist)
						   (push '(":results:"        . "»»»") prettify-symbols-alist)
						   (push '("#+name:"          . "-") prettify-symbols-alist)
						   (push '("#+begin_example"  . "~") prettify-symbols-alist)
						   (push '("#+begin_example"  . "~") prettify-symbols-alist)
						   (push '("#+end_example"    . "~") prettify-symbols-alist)
						   (push '("#+end_example"    . "~") prettify-symbols-alist)
						   (push '("#+begin_verbatim" . "") prettify-symbols-alist)
						   (push '("#+end_verbatim"   . "") prettify-symbols-alist)
						   (push '("#+begin_verse"    . "") prettify-symbols-alist)
						   (push '("#+end_verse"      . "") prettify-symbols-alist)
						   (push '("#+begin_quote"    . "𐄚") prettify-symbols-alist)
						   (push '("#+end_quote"      . "𐄚") prettify-symbols-alist)
						   (push '("#+tblfm:"         . "∫") prettify-symbols-alist)
						   (push '("[X]"              . (?\[ (Br . Bl) ?✓ (Br . Bl) ?\])) prettify-symbols-alist)
						   (push '("\\\\"             . "↩") prettify-symbols-alist)
						   (prettify-symbols-mode t)))

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

;; Journaling
(load "~/.emacs.d/elisp/my-org-journal/org-journal.el")
(setf org-journal-dir "~/Dropbox/Agenda/Journal")
(setf org-journal-file-format "my-journal.org")
(setf org-journal-date-prefix "* ")
(setf org-journal-time-prefix "** ")

;; Custom function to add today date - TIL
(defun lgm/org-add-today-as-entry ()
  "Insert the current date as a org header"
  (interactive)
  ;; empty file? Add a date timestamp
  (let* ((time (current-time)))
	(org-insert-heading-respect-content)
	(insert
	 (format-time-string org-journal-date-format time)))
  )


(defun lgm/org-add-date-as-entry ()
  "Insert the selected date as a org header"
  (interactive)
  (let* ((time (org-read-date t t)))
	(org-insert-heading-respect-content)
	(insert
	 (format-time-string org-journal-date-format time)))
  )

(defun lgm/org-add-this-week-as-entry ()
  "Insert the current week of the year as a org header"
  (interactive)
  (let* ((time (current-time)))
	(org-insert-heading-respect-content)
	(insert
	 (format-time-string "Week %U" time)))
  )

(defun lgm/org-add-new-meeting-notes ()
  "Insert org headers for meeting notes"
  (interactive)
  (let* ((time (current-time)))
	(org-insert-heading-respect-content)
	(save-excursion (insert
	 (format-time-string "[Meeting title], %R, %D" time))
	(org-insert-subheading 2)
	(insert "Pre-meeting")
	(org-insert-heading-respect-content)
	(insert "Notes")
	(org-insert-heading-respect-content)
	(insert "Action items")))
  )

(org-defkey org-mode-map (kbd "C-M-<return>") (lambda ()
						(interactive)
						(org-insert-subheading 2)))


;; Make Org Journal remember me about
;; writing my day thoughts like in Memento mode
(defcustom journal-file "~/Dropbox/Agenda/roam/20220619113235-my_journal.org"
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

(add-to-list 'org-capture-templates
             '("w" "Work task"  entry
               (file+headline "~/Dropbox/Agenda/nu.org" "Tasks")
               "* TODO %?"
	       :empty-lines 1))

(add-to-list 'org-capture-templates
             '("b" "Batch work task"  entry
               (file+headline "~/Dropbox/Agenda/nu.org" "Batch")
               "* TODO [#D] %?                                  :batch:
:PROPERTIES:
:Effort: 0:05
:END:
"
			   :empty-lines 1))

(add-to-list 'org-capture-templates
             '("e" "Work Epic"  entry
               (file "~/Dropbox/Agenda/nu.org")
               "* TODO %?                 :epic:spinning:
:PROPERTIES:
:SUMMARY:
:AUDACITY: Low
:SCOPE: Small
:CUSTOMER:
:GROWTH-COMMITMENTS:
:EVIDENCE:
:IMPACT-STATEMENT:
:IMPACT:   Low
:END:" :empty-lines 1))

(add-to-list 'org-capture-templates
             '("d" "Personal task"  entry
               (file "~/Dropbox/Agenda/todo.org" "Life" "Misc")
               "* TODO %?"
			   :empty-lines 1))

(add-to-list 'org-capture-templates
             '("a" "Org Roam daily" plain
               "%?"
               (function org-roam-dailies-capture-date)))

(add-to-list 'org-capture-templates
             '("m" "Fleeting memory" entry
			   (file "~/Dropbox/Agenda/roam/20220619110953-fleeting_memories.org")
               "* %t \n** %?"))

(add-to-list 'org-capture-templates
             '("n" "Fleeting notes" entry
			   (file "~/Dropbox/Agenda/roam/20200809213233-fleeting_notes.org")
			   "* %? \n%t "))

(add-to-list 'org-capture-templates
             '("u" "Nu Fleeting notes" entry
			   (file "~/Dropbox/Agenda/roam/20231001003359-nubank_fleeting_notes.org")
               "* %t %? \n "))

(add-to-list 'org-capture-templates
             '("j" "Journal" entry
			   (file "~/Dropbox/Agenda/roam/20220619113235-my_journal.org")
               "* %<%A, %D> \n** %?"))

;; I'm not using currently, but it is a way of forcing thinking on urgency and importance and use filters in the Agenda to treat them accordingly
;; (add-to-list 'org-capture-templates
;;              '("t" "Task with Properties" entry
;;                (file+headline "~/Dropbox/Agenda/nu.org" "Tasks")
;;                "* TODO %^{Task}\n:PROPERTIES:\n:IMPORTANCE:%^{Importance|low|high}\n:URGENCY:%^{Urgency|low|high}\n:END:\n%?"
;;                :empty-lines 1))

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

;; Identify with a symbol when a scheduled to-do has children
(with-eval-after-load 'org-agenda
  (defun my/org-has-children ()
    (if (save-excursion (org-goto-first-child)) "▶" "  ")
  )
  (add-to-list 'org-agenda-prefix-format '(
     agenda  . "%i%-3:(my/org-has-children) %-12:c%?-12t% s "
  ))
)


;; From https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun my-org-agenda-skip-all-scheduled ()
  "Skip entries with any SCHEDULED timestamp."
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (when scheduled
      (save-excursion (org-end-of-subtree t)))))

(setq org-agenda-block-separator " ")
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
				   (org-agenda-prefix-format "    %i %-12:c%(my/org-has-children)")
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
		  (tags "+TODO=\"TODO\"+SCHEDULED<\"<today>\"-epic-goals-selfdevelopment-family"
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
								(my-org-agenda-skip-if-parent-has-tag "bulk")
								))
				 (org-agenda-overriding-header "Backlog\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
	  			 (org-agenda-remove-tags t)))

		  ;; Year Goals General
		  (tags "+goals+LEVEL=3+TODO=\"TODO\"|+goals+LEVEL=3+TODO=\"DONE\""
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
							(org-agenda-overriding-header "📅 Deadlines in the next 60 days\n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
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
						 (org-agenda-prefix-format "    %i %-4e %-12:c%(my/org-has-children)")
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

;; Work settings
(add-to-list 'org-capture-templates
             '("e" "Work Epic"  entry
               (file "~/Dropbox/Agenda/nu.org")
               "* TODO %?                 :epic:spinning:
:PROPERTIES:
:SUMMARY:
:AUDACITY: Low
:SCOPE: Small
:CUSTOMER:
:GROWTH-COMMITMENTS:
:EVIDENCE:
:IMPACT-STATEMENT:
:IMPACT:   Low
:END:" :empty-lines 1))

;; Enable the usage of two agenda views at the same time
(org-toggle-sticky-agenda)

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

;; Shortcut to display day activity
(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "n"))

(defun scheduled-or-not (resp)
  (interactive)
  (if resp
    ;;'"OK"
    (concat "  In " (number-to-string (org-time-stamp-to-now resp)) " day(s)")
    '"  Not Scheduled"
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
							)))))

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

(defadvice org-agenda (around split-vertically activate)
  (let ((split-width-threshold 80))  ; or whatever width makes sense for you
    ad-do-it))

;; Always split stuff vertically
(setq split-height-threshold nil)

;; Avoid the todo selection to disrupt the buffers
(setq org-use-fast-todo-selection 'expert)

;; Start with my to-do
;; The org mode file is opened with
;; Make org agenda open in the right
(setq org-agenda-window-setup 'other-window)
(find-file "~/Dropbox/Agenda/todo.org")
(switch-to-buffer "todo.org")
(add-to-list 'org-agenda-files  "~/Dropbox/Agenda/todo.org")

;; Open agenda
(global-set-key (kbd "<f10>") (lambda() (interactive)(org-agenda nil "d")(org-agenda-redo)))
;; Work agenda
(global-set-key (kbd "<f7>") (lambda() (interactive)(org-agenda nil "nu")(org-agenda-redo)))
;; Initialize with agenda view
(add-hook 'after-init-hook (lambda () (org-agenda nil "d") (org-agenda-redo)))
;; Open day
(setq org-agenda-span 'day)
(global-set-key (kbd "C-c <f10>") (lambda() (interactive)(org-agenda 0 "a")))
(global-set-key (kbd "M-<f10>") (lambda() (interactive)(org-agenda 0 "l")))

;; Place the tags in a way I can user a larger font size
(setq org-tags-column -50)

;; Change buffer functionality
(org-defkey org-mode-map (kbd "C-<tab>") (lambda ()
					   (interactive)
					   (other-window 1)))

(setq org-code-block-header "jupyter-python")
(defun lgm/set-org-code-block-header()
  (interactive)
  (setq org-code-block-header
		(read-string "New header: "))
  )

(defun lgm/python-org-code-block()
  (interactive)
  (org-open-line 2)
  (insert (concat "#+begin_src " org-code-block-header))
  (next-line)
  (insert "#+end_src")
  (previous-line)
  (end-of-line)
  (split-line)
  (next-line)
  (beginning-of-line)
  (delete-line)
  (org-open-line 1)
  )

(defun lgm/double-dollar-sign()
  (interactive)
  (insert "$$$$")
  (backward-char 2)
  )

;; (add-to-list 'load-path "~/.emacs.d/elpa/deadgrep-20241012.1511/")
;; (require 'deadgrep)

(use-package deadgrep
  :ensure t
  :bind
  ("C-c n d" . deadgrep)
    )

(setq org-roam-v2-ack t)
(add-to-list 'load-path "~/.emacs.d/elpa/org-roam-20241007.1704/")
(require 'org-roam)

(use-package org-roam
  :init
  (setq org-roam-completion-everywhere t)
  (setq completion-ignore-case t)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (set (make-local-variable 'company-backends) '(company-capf))))
  (add-to-list 'company-backends 'company-capf)
  (setq org-roam-completion-ignore-case t)
  (setq org-roam-capture-templates
	'(("d" "default" plain
           (file "/Users/luis.moneda/Dropbox/Agenda/templates/default_org_roam.org")
           :target
           (file+head "%(format-time-string \"%Y%m%d%H%M%S-${slug}.org\" (current-time) t)" "#+title: ${title}
#+STARTUP: inlineimages latexpreview
#+filetags: ")
           :unnarrowed t)
	  ("r" "bibliography reference" plain
           (file "/Users/luis.moneda/Dropbox/Agenda/templates/bib_org_roam.org")
           :target
           (file+head "${citekey}.org" "#+TITLE: ${title}, ${author-abbrev}\n#+ROAM_KEY: ${citekey}\n#+Source: ${ref}\n#+Authors: ${author}\n#+STARTUP: inlineimages latexpreview\n#+filetags: :bibliographical_notes: \n")
	   :unnarrowed t)
	  )
	)
  :custom
  (org-roam-directory (file-truename "/Users/luis.moneda/Dropbox/Agenda/roam"))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ;; ("C-c n g" . org-roam-graph)
	 ("C-c n g" . counsel-org-goto)
	 ("C-c n b" . helm-bibtex)
	 ("C-c n s" . lgm/screenshot-to-org-link)
	 ("C-c n a" . org-roam-semantic-search-api)
	 ("C-c n y" . chunky-semantic-search)
	 ("C-c n q" . lgm/org-roam-ai-chat-to-notes)
	 ("C-c n p" . lgm/gpt-prompt)
	 ("C-c n c" . lgm/python-org-code-block)
	 ("C-c n e" . lgm/double-dollar-sign)
	 ("C-c k" . lgm/double-dollar-sign)
	 ("C-c n n" . org-id-get-create)
	 ("C-c n w" . my/drawio-create)
	 ("C-c n o" . my/drawio-edit)
	 )
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
	(concat "${title:105} "
		(propertize "${tags:40}" 'face 'org-tag))))


(add-to-list 'load-path "~/.emacs.d/elpa/org-roam-ui-20221105.1040/")
(require 'org-roam-ui)
(use-package org-roam-ui
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(require 'f)

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

;; Increase latex preview font
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2))

;; Olivetti
;; Look & Feel for long-form writing
(use-package olivetti
  :ensure t
  :config
  (setq-default olivetti-body-width 120))

;; Enable Olivetti for text-related mode such as Org Mode
(add-hook 'text-mode-hook 'olivetti-mode)

(add-hook 'org-mode-hook
	    (lambda ()
	      (make-local-variable 'company-backends)
	      (make-local-variable 'company-idle-delay)
	      (make-local-variable 'company-minimum-prefix-length)
	      (setq company-backends '(company-capf))
	      ;; (setq company-idle-delay 0.5
	      ;; 	    company-minimum-prefix-length 3)
	      )
	    )

;; Xunxo to disable in my todo file
(progn
  (switch-to-buffer "todo.org")
  (company-mode -1)
  )

;; Avoid org asking if I want to run code
(setq org-confirm-babel-evaluate nil)

;; Improve editing of code blocks inside org
;; src block indentation / editing / syntax highlighting
;; Source: https://github.com/syl20bnr/spacemacs/issues/13255
(setq org-src-fontify-natively t
      org-src-window-setup 'current-window ;; edit in current window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t ;; do not put two spaces on the left
      org-src-tab-acts-natively t)

;; org-roam-bib
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :init
  (setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file" "author" "author-abbrev")
      orb-process-file-keyword t
      orb-file-field-extensions '("pdf"))

  (setq orb-preformat-templates t)
  (setq orb-templates
	'(
	  ("r" "bibliography reference" plain
         (file "/Users/luis.moneda/Dropbox/Agenda/templates/bib_org_roam.org")
         :target
         (file+head "${citekey}.org" "#+TITLE: ${title}, ${author-abbrev}\n#+ROAM_KEY: ${ref}\n#+Authors: ${author}\n#+STARTUP: inlineimages latexpreview\n#+filetags: :bibliographical_notes: \n")
	 :unnarrowed t)
	  )
	)
  :config
  (require 'org-ref)
  )

(defun lgm/org-ref-notes-function (candidates)
  (let ((key (helm-marked-candidates)))
    (funcall 'orb-edit-note (car key))))

(helm-delete-action-from-source "Edit notes" helm-source-bibtex)
(helm-add-action-to-source "Edit notes" 'lgm/org-ref-notes-function helm-source-bibtex 7)

(use-package org-ql
  :ensure t)

;; Refile settings
;; from https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
;; If it's needed to define for different files: https://www.reddit.com/r/orgmode/comments/eonvo1/beginner_orgrefiletargets_for_each_file/
;; (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-targets '((nil :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun list-roam-files-with-tags ()
    "Return a list of files with associated tags
     I use this to denote files with tasks for org-agenda" ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (in tag $v1)] '(["nu-todo"])))))

(defun list-roam-files-with-tags-todo ()
    "Return a list of files with associated tags
     I use this to denote files with tasks for org-agenda" ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (in tag $v1)] '(["todo"])))))

(setq old-nu-roam-agenda-files '("~/Dropbox/Agenda/nu.org"
				 "~/Dropbox/Agenda/roam/20211123125642-nu_meeting_notes.org"))

;; I need to call it to bring org-roam mode
;; (org-roam-version)
(setq personal-dynamic-roam-agenda-files (list-roam-files-with-tags-todo))
(setq personal-roam-agenda-files (append '("~/Dropbox/Agenda/todo.org")
				   personal-dynamic-roam-agenda-files))

;; In order to quickly change from Personal to Nu context
;; I used to use the command C-c C-x < to set the agenda file
;; But now I want to have multiple files
;; Check in the future: https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(setq org-agenda-files personal-roam-agenda-files)
(setq org-agenda-files-personal-mode t)
(setq nu-dynamic-files-fetched nil)

(defun lgm/toggle-agenda-files ()
  "Toggles between Nu and personal files to build agenda"
  (interactive)
  (if org-agenda-files-personal-mode
      (progn (when (not nu-dynamic-files-fetched)
			   ;; I was getting an error from it
			   ;; (org-roam-version)
			   (require 'org-roam)
			   (setq nu-dynamic-roam-agenda-files (list-roam-files-with-tags))
			   (setq nu-roam-agenda-files (append '("~/Dropbox/Agenda/nu.org")
												  nu-dynamic-roam-agenda-files))
			   )
			 (setq org-agenda-files nu-roam-agenda-files)
			 (setq org-agenda-files-personal-mode nil)
			 (setq nu-dynamic-files-fetched t))

    (progn (setq org-agenda-files personal-roam-agenda-files)
	   (setq org-agenda-files-personal-mode t))
    )
  )

;; Take notes in text files with a highlight mark
;; (use-package org-remark
;;   :ensure t
;;   :init (org-remark-global-tracking-mode +1)
;;   )
;; (add-to-list 'load-path "~/repos/org-remark")
;; (require 'org-remark-global-tracking)

;; (autoload #'org-remark-mark "org-remark" nil t)
;; (autoload #'org-remark-mode "org-remark" nil t)
;; (define-key global-map (kbd "C-c n m") #'org-remark-mark)
;; (define-key global-map (kbd "C-c n k") #'org-remark-change)

;; The rest of keybidings are done only on loading `org-remark'
(with-eval-after-load 'org-remark
  (define-key org-remark-mode-map (kbd "C-c n o") #'org-remark-open)
  (define-key org-remark-mode-map (kbd "C-c n ]") #'org-remark-view-next)
  (define-key org-remark-mode-map (kbd "C-c n [") #'org-remark-view-prev)
  (define-key org-remark-mode-map (kbd "C-c n r") #'org-remark-remove))

(org-remark-mode)
(org-remark-create "red-line"
                   '(:underline (:color "magenta" :style wave))
                   '(CATEGORY "review" help-echo "Review this"))
(org-remark-create "yellow"
                   '(:underline "gold")
                   '(CATEGORY "important"))

;; Priority inheritance
;; source:
(defun my/org-inherited-priority (s)
  (cond

   ;; Priority cookie in this heading
   ((string-match org-priority-regexp s)
    (* 1000 (- org-priority-lowest
               (org-priority-to-value (match-string 2 s)))))

   ;; No priority cookie, but already at highest level
   ((not (org-up-heading-safe))
    (* 1000 (- org-priority-lowest org-priority-default)))

   ;; Look for the parent's priority
   (t
    (my/org-inherited-priority (org-get-heading)))))

(setq org-priority-get-priority-function #'my/org-inherited-priority)

(defun lgm/get-parent-priority ()
  (interactive)
  (save-excursion
    (org-up-heading-safe)
    (message "Heading in get parent f: %s"  (org-get-heading))
    (org-get-priority (org-get-heading))
    )
  )

(defun org-priority-integer-to-char (integer)
  "Convert priority INTEGER to a character priority."
  (- org-lowest-priority (/ integer 1000)))

;; When using org-get-priority in a entry without priority, org will jump to the parent
;; and return its priority. That's why I had to use regex
(defun lgm/is-heading-no-priority ()
  (interactive)
  (if (string-match org-priority-regexp (org-get-heading))
      nil
    t)
  )

(defun lgm/inherit-parent-priority-if-no-explicit ()
  (interactive)
  (when (lgm/is-heading-no-priority)
    (lgm/inherit-parent-priority)
    )
  )

(defun lgm/inherit-parent-priority ()
  "Set the priority to a specific org heading the cursor is over"
  (interactive)
    (org-priority (org-priority-integer-to-char (lgm/get-parent-priority))
    )
  )

(define-key org-mode-map (kbd "C-c m") 'lgm/inherit-parent-priority)

;; Automatically gets parent priority when using org-capture + org-refile
(add-hook 'org-after-refile-insert-hook #'lgm/inherit-parent-priority-if-no-explicit)

(use-package org-download
  :ensure t
  :init
  (setq org-download-image-dir "/Users/luis.moneda/Dropbox/Agenda/roam/resources")
  )

;; (add-to-list 'exec-path "/opt/homebrew/bin/")

;; The width to display images
;; If I want to set per image, this value needs to be nil
(setq org-image-actual-width 750)

(defun lgm/set-org-image-actual-width ()
  (interactive)
  (let ((width (read-from-minibuffer "Enter the width for images in org-mode: ")))
    (setq org-image-actual-width (string-to-number width))))

;;Inspired or copied from H4kman
(defun my/drawio-create (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let*((basename (if (not use-default-filename) (read-string (format "Filename [%s]: " "figure.svg") nil nil "figure.svg") nil))
        (dir (org-download--dir))
        (filepath (concat dir "/" (org-download-file-format-default basename)))
        (org-download-image-org-width 400))
    (make-directory dir t)
    ;; (when (not (file-exists-p filepath)) (copy-file "~/.emacs.d/resources/drawio_template.svg" filepath)) ; create empty svg file
	(when (not (file-exists-p filepath)) (copy-file "~/.emacs.d/resources/drawio_transparent_template.svg" filepath)) ; create empty svg file
    (start-process-shell-command "drawio" nil (format "exec /Applications/draw.io.app/Contents/MacOS/draw.io %s" filepath)) ; open svg file
    (org-download-insert-link basename filepath)
    )
  (org-toggle-inline-images)
  (org-toggle-inline-images)
  )

(defun my/drawio-edit ()
  (interactive)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (start-process-shell-command
       "drawio"
       "drawio"
       (format "exec /Applications/draw.io.app/Contents/MacOS/draw.io %s"
               (string-replace ".png" ".svg" (shell-quote-wildcard-pattern
					      (url-unhex-string (plist-get (cadr context) :path))))))))
  )

;; Some svg images are not displayed nicely on Emacs, converting to png solves it
(defun lgm/drawio-convert-to-png ()
  (interactive)
  (let*((context (org-element-context))
	(filepath (string-replace ".png" ".svg" (shell-quote-wildcard-pattern
						 (url-unhex-string (plist-get (cadr context) :path)))))
	(pngfilepath (string-replace ".svg" ".png" filepath)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (start-process-shell-command
       "drawio"
       "drawio"
	   ;; -t, --transparent for transparent png
       (format "exec /Applications/draw.io.app/Contents/MacOS/draw.io -x -f png --transparent -o %s %s" pngfilepath filepath))
      (kill-whole-line)
      (org-insert-link nil (concat "file:" pngfilepath) nil)
      (org-toggle-inline-images)
      (org-toggle-inline-images)
      )))

;; Functions related to effort in the tasks
;; Add total effort for the tasks in a day to enable a reality check
(require 'cl-lib)
(setq org-global-properties
      '(("Effort_ALL" .
         "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 2:30")))

(defun my/org-agenda-calculate-efforts (limit)
  "Sum the efforts of scheduled entries up to LIMIT in the
agenda buffer."
  (let (total)
    (save-excursion
     (while (< (point) limit)
       (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
         (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
       (forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
                (mapcar #'org-duration-to-minutes
                        (cl-remove-if-not 'identity total))))))

(defun my/org-agenda-insert-efforts ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
   (let (pos)
     (while (setq pos (text-property-any
                       (point) (point-max) 'org-agenda-date-header t))
       (goto-char pos)
       (end-of-line)
       (insert-and-inherit (concat " ("
                                   (my/org-agenda-calculate-efforts
                                    (next-single-property-change (point) 'day))
                                   ")"))
       (forward-line)))))

;; (add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-efforts)

;; Run only for work agenda
(defun my/org-agenda-finalize-for-specific-file ()
  "Customize agenda finalization for a specific org file."
  (when (member "~/Dropbox/Agenda/nu.org" org-agenda-files)
    (my/org-agenda-insert-efforts)))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-finalize-for-specific-file)

(defun lgm/prompt-for-effort-at-date (date)
  "Go through every TODO item scheduled for the specified DATE and prompt the user for the :effort: property if it is not defined.

If no date is specified, use today's date as the default."
  (interactive "P")
  (let ((input (org-read-date)))
    (if input
        (setq date (format-time-string "%Y-%m-%d" (date-to-time input)))
      (setq date (format-time-string "%Y-%m-%d" (current-time)))))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ TODO" nil t)
      (let ((scheduled-time (org-get-scheduled-time (point)))
            (effort (org-entry-get (point) "EFFORT")))
        (when (and scheduled-time
                   (equal date (format-time-string "%Y-%m-%d" scheduled-time))
                   (not effort))
	    (org-fold-reveal)
	    (recenter-top-bottom)
          (setq effort (read-string "Effort: "))
          (org-set-property "EFFORT" effort))))))

(defun lgm/prompt-for-effort-for-all-items ()
  "Go through every TODO item without the effort property and prompt for it"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ TODO" nil t)
      (let ((scheduled-time (org-get-scheduled-time (point)))
            (effort (org-entry-get (point) "EFFORT"))
	    (tags (org-get-tags))
	    )
        (when (and (not (or (member "epic" tags) (member "goals" tags) (member "delegated" tags)))
                   (not effort))
	    (org-fold-reveal)
	    (recenter-top-bottom)
	    (org-narrow-to-subtree)
          (setq effort (read-string "Effort: "))
          (org-set-property "EFFORT" effort)
	  (widen)
	  )))))

(defun minutes-to-hhmm (minutes)
  "Convert MINUTES to a string in the hh:mm format."
  (format "%02d:%02d" (/ minutes 60) (mod minutes 60)))

(defun get-scheduled-effort (date)
  "Get the sum of the effort of all scheduled TODO items for DATE in minutes."
  (let* ((scheduled-effort 0)
	(date-str (format-time-string "%Y-%m-%d" date)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-complex-heading-regexp nil t)
        (let* ((item-effort (org-entry-get (point) "Effort"))
               (item-effort (when item-effort (org-duration-to-minutes item-effort)))
               (scheduledp (org-get-scheduled-time (point))))
          (when (and scheduledp
		     (equal date-str (format-time-string "%Y-%m-%d" scheduledp))
		     )
            (setq scheduled-effort (+ scheduled-effort item-effort))))))
    scheduled-effort))

(defun lgm/schedule-todos ()
  "Schedule all possible unscheduled TODO items from the current org file
   such that their combined effort is below FOCUS-TIME and they are scheduled
   on DATE. Prioritize TODO items using org priority."
  (interactive)
  (let* ((focus-time (read-string "Focus time (hh:mm): "))
  	 (focus-time (org-duration-to-minutes focus-time))
  	 (date (org-read-date nil t))
         (scheduled-items 0)
         (scheduled-effort (get-scheduled-effort date))
	 )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-complex-heading-regexp nil t)
        (let* ((item-effort (org-entry-get (point) "Effort"))
               (item-effort (when item-effort (org-duration-to-minutes item-effort)))
               (item-priority (org-entry-get (point) "PRIORITY"))
               (item-priority (when item-priority (string-to-char item-priority)))
               (scheduledp (org-entry-get (point) "SCHEDULED")))
          (when (and (not scheduledp) item-effort (<= (+ scheduled-effort item-effort) focus-time))
            (org-schedule nil date)
            (setq scheduled-items (1+ scheduled-items))
            (setq scheduled-effort (+ scheduled-effort item-effort))))))
    (message "Scheduled %d TODO items with a total effort of %s on %s"
           scheduled-items (minutes-to-hhmm scheduled-effort)  (format-time-string "%d-%m-%Y" date))
    )
  )

(defun web-link-p (string)
  "Check if STRING is a web link."
  (not (null (string-match-p "\\`\\(https?\\|ftp\\|file\\):\\/\\/" string))))

(defun my-org-make-link ()
  "Turn selected text into an Org mode link by replacing the selected
text with a link that uses the selected text as the link description
and the clipboard contents as the link URL."
  (interactive)
  (let ((link-description (if (use-region-p)
                              (buffer-substring-no-properties (region-beginning) (region-end))
                            (read-string "Link description: ")))
        (clipboard-contents (current-kill 0)))
    (if (not (web-link-p clipboard-contents))
	(message "Not a valid link")
      (progn
	    (delete-region (region-beginning) (region-end))
	    (insert (concat "[[" clipboard-contents "][" link-description "]]")))
      )))

(define-key org-mode-map (kbd "s-v") 'my-org-make-link)

(defun my-exclude-org-roam-link (input-string)
  "Return a new string with org-roam links in the given INPUT-STRING transformed into plain text using the link description."
  (replace-regexp-in-string
   "\\[\\[id:\\([^][]+\\)\\]\\[\\([^][]+\\)\\]\\]"
   (lambda (match)
     (let ((link-path (match-string 1 match))
           (link-title (match-string 2 match)))
       (format "%s" link-title)))
   input-string))

(defun lgm/gpt-prompt ()
  "Summarize the selected text using the OpenAI API and display the
summary in a new temporary buffer."
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (let* ((selected-text (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
						  ""
                          ))
	 (filtered-text (my-exclude-org-roam-link selected-text))
	 (input-string (read-string "Enter a prompt: "))
		 (script-output (my-call-python-script "~/Dropbox/Projetos/Emacs/org-roam-ai/api_call.py" filtered-text input-string))
        )
	(with-output-to-temp-buffer "*OpenAI Output*"
	  (with-current-buffer "*OpenAI Output*"
        (insert script-output)))
	))

(setq open-ai-summary-prompt "\n\nTl;dr")

(defun my-openai-summarize ()
  "Summarize the selected text using the OpenAI API and display the
summary in a new temporary buffer."
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (let* ((selected-text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (read-string "Enter text to summarize: ")))
		 (filtered-text (my-exclude-org-roam-link selected-text))
		 (script-output (my-call-python-script "~/Dropbox/Projetos/Emacs/org-roam-ai/api_call.py" filtered-text open-ai-summary-prompt))
        )
	(with-output-to-temp-buffer "*OpenAI Summary*"
	  (with-current-buffer "*OpenAI Summary*"
            (insert script-output)))
	))

(defun my-openai-model-function ()
  (interactive)
  "Prompt the user to select an OpenAI model from a list of available models and set the `openai-model` variable to the selected model."
  (let ((available-models '("text-davinci-002" "text-curie-001" "text-ada-001")))
    (setq openai-model (completing-read "Select an OpenAI model: " available-models))))


(defun my-openai-summarize-text (text)
  "Call the OpenAI API and provide it with the given TEXT as input.
API-KEY is the OpenAI API key to use for the request."
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (my-call-python-script "~/Dropbox/Projetos/Emacs/org-roam-ai/api_call.py" text open-ai-summary-prompt)
)

(defun my-call-python-script (script-name text prompt)
  "Call the given Python SCRIPT-NAME with the given TEXT as input and
display the output in a new temporary buffer."
  (let ((script-output (shell-command-to-string
                        (format "python -W ignore %s \"%s\" \"%s\"" script-name text prompt))))
    script-output)
  )

(defun display-org-roam-nodes (node-ids)
  "Display the org-roam nodes with the given IDS in a temporary buffer.
   The buffer will show the node's title, the first 5 lines of content,
   and a link to visit the node."
  (let ((buf (get-buffer-create "*org-roam-node-display*")))
    (with-current-buffer buf
      (erase-buffer)
      (let ((nodes (org-roam-db-query
                    [:select [node-id]
                     :from nodes
                     :where (in node-id $node-ids)])))
        (dolist (node nodes)
          (let* ((node-id (plist-get node :node-id))
                 (title (org-roam-format-link (plist-get node :title)))
                 (first-lines (cl-subseq (split-string (plist-get node :content) "\n") 0 5))
                 (link (format "[[org-roam:node-id:%s][Visit node]]" node-id)))
            (insert (format "%s\n\n%s\n\n%s\n\n" title (mapconcat #'identity first-lines "\n") link)))))
      (display-buffer buf))))


;; org-roam-ai
(defun display-org-roam-nodes (node-ids)
  "Display the org-roam nodes with the given IDS in a temporary buffer.
   The buffer will show the node's title, the first 5 lines of content,
   and a link to visit the node."
  (let ((buf (get-buffer-create "*semantic-search-results*")))
    (with-current-buffer buf
      (erase-buffer)
	  (org-mode)
      (dolist (node-id node-ids)
        (let* ((node (org-roam-node-from-id node-id))
               (title (org-roam-node-title node))
			   (first-lines "")
               ;; (first-lines (cl-subseq (split-string (cdr node) "\n") 0 5))
               (link (format "[[id:%s][%s]]" node-id title)))
          (insert (format "- %s\n" link))))
      (insert "\n [[file:/Users/luis.moneda/Dropbox/Agenda/org-roam-ai/output.jpg]]")
	  (org-display-inline-images nil t)
      )
    (display-buffer buf)
    )
  )

;; Avoid error when node is nil (database fails)
(defun display-org-roam-nodes (node-ids)
  "Display the org-roam nodes with the given IDS in a temporary buffer.
   The buffer will show the node's title, the first 5 lines of content,
   and a link to visit the node."
  (let ((buf (get-buffer-create "*semantic-search-results*")))
    (with-current-buffer buf
      (erase-buffer)
	  (org-mode)
      (dolist (node-id node-ids)
        (let* ((node (org-roam-node-from-id node-id)))
          (when node
            (let* ((title (org-roam-node-title node))
                   (first-lines "")
                   (link (format "[[id:%s][%s]]" node-id title)))
              (insert (format "- %s\n" link))))))
      (insert "\n [[file:/Users/luis.moneda/Dropbox/Agenda/org-roam-ai/output.jpg]]")
      (org-display-inline-images nil t)
      (beginning-of-buffer)
      )
    (display-buffer buf)
    )
  )

(defun replace-quotes (string)
  "Replace quote characters in STRING with controlled version."
  (replace-regexp-in-string "\"" "\\\\\"" string))

(defun org-roam-ai-semantic-search ()
  "Call the given Python SCRIPT-NAME with the given TEXT as input and
display the output in a new temporary buffer."
    (interactive)
	(pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
	(let* ((text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (read-string "Enter search: ")))
		   (script-name "/Users/luis.moneda/Dropbox/Agenda/org-roam-ai/semantic_search.py")
		   (script-output (shell-command-to-string
				   (format "python -W ignore %s \"%s\"" script-name (replace-quotes text)))))
	  ;; (message script-output)
	  (display-org-roam-nodes (read script-output))
	  )
	)

;; (add-to-list 'load-path "~/.emacs.d/elpa/consult-org-roam-20240217.1442")
;; (require 'consult-org-roam)
(use-package consult-org-roam
   :ensure t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "C-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n h" . consult-org-roam-backlinks)
   ("C-c n j" . consult-org-roam-forward-links))

;; Org-similarity
;; (add-to-list 'load-path "~/repos/org-similarity")
;; (require 'org-similarity)

;; (use-package org-similarity
;;   :load-path  "~/repos/org-similarity")

;; Directory to scan for possibly similar documents.
;; org-roam users might want to change it to `org-roam-directory'.
(setq org-similarity-directory org-roam-directory)

;; The language passed to the Snowball stemmer in the `nltk' package.  The
;; following languages are supported: Arabic, Danish, Dutch, English, Finnish,
;; French, German, Hungarian, Italian, Norwegian, Portuguese, Romanian, Russian,
;; Spanish and Swedish.
(setq org-similarity-language "english")

;; How many similar entries to list at the end of the buffer.
(setq org-similarity-number-of-documents 10)

;; Whether to prepend the list entries with similarity scores.
(setq org-similarity-show-scores nil)

;; Whether the resulting list of similar documents will point to ID property or
;; filename. Default it nil.
;; However, I recommend setting it to `t' if you use `org-roam' v2.
(setq org-similarity-use-id-links t)

;; Scan for files inside `org-similarity-directory' recursively.
(setq org-similarity-recursive-search nil)

;; Effort functions for work
(defun my/org-agenda-calculate-total-efforts (limit)
  "Sum the efforts of scheduled entries up to LIMIT in the agenda buffer."
  (let (total)
    (save-excursion
      (while (< (point) limit)
	(when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
          (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
	(forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
                (mapcar #'org-duration-to-minutes
                        (cl-remove-if-not 'identity total))))))

(defun my/org-agenda-calculate-remaining-efforts (limit)
  "Sum the efforts of TODO entries up to LIMIT in the agenda buffer."
  (let (total)
    (save-excursion
      (while (< (point) limit)
	(when (and (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
		   (string= (org-get-at-bol 'todo-state) "TODO"))
	  (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
	(forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
		(mapcar #'org-duration-to-minutes
			(cl-remove-if-not 'identity total))))))

(defun my/org-agenda-calculate-state-efforts (limit state)
  "Sum the efforts of TODO entries up to LIMIT in the agenda buffer."
  (let (total)
    (save-excursion
      (while (< (point) limit)
	(when (and (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
		   (string= (org-get-at-bol 'todo-state) state))
	  (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
	(forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
		(mapcar #'org-duration-to-minutes
			(cl-remove-if-not 'identity total))))))

(defun my/org-agenda-calculate-remaining-high-priority (limit)
  "Sum the efforts of scheduled TODO entries with priority A up to LIMIT in the agenda buffer."
  (let (total)
    (save-excursion
      (while (< (point) limit)
	(when (and (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
		   (string= (org-get-at-bol 'todo-state) "TODO")
		   (string= (org-get-at-bol 'priority) "A"))
	  (push (org-entry-get (org-get-at-bol 'org-hd-marker) "Effort") total))
	(forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
		(mapcar #'org-duration-to-minutes
			(cl-remove-if-not 'identity total))))))

(defun my/org-agenda-insert-efforts ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
    (let (pos total-efforts remaining-efforts percentage)
      (while (setq pos (text-property-any
			(point) (point-max) 'org-agenda-date-header t))
	(goto-char pos)
	(end-of-line)
	(setq total-efforts (my/org-agenda-calculate-total-efforts (next-single-property-change (point) 'day)))
	(setq remaining-efforts (my/org-agenda-calculate-state-efforts (next-single-property-change (point) 'day) "TODO"))
	(setq blocked-efforts (my/org-agenda-calculate-state-efforts (next-single-property-change (point) 'day) "WAIT"))
	(setq blocked-percentage (/ (org-duration-to-minutes blocked-efforts) (org-duration-to-minutes total-efforts)))
	(setq percentage (- 1 (/ (org-duration-to-minutes remaining-efforts) (org-duration-to-minutes total-efforts))))
	(setq accomplished-percentage (- percentage blocked-percentage))
	(insert-and-inherit (concat " (" (number-to-string (floor (* accomplished-percentage 100))) "% done, " (number-to-string (floor (* blocked-percentage 100))) "% blocked, " total-efforts " total, " remaining-efforts " remaining)"))
	(forward-line)))))

;; There is a bug preventing the agenda view to finish
;; (add-hook 'org-agenda-finalize-hook 'my/org-agenda-insert-efforts)

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :init
  (require 'ox-org)
  :ensure t
  ;; Uncomment the below if 'ensure-system-packages` is installed
  ;;:ensure-system-package (gvgen . graphviz)
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

;; Imagemagick opening .eps
(setq imagemagick-enabled-types t)
(imagemagick-register-types)

;;org-transclude
;; Great to create new documents with different references to share with people
(use-package org-transclusion
  :ensure t)

;;Inline .eps
(add-to-list 'image-file-name-extensions "eps")

;; ChatGPT

(defun lgm/chatgpt-prompt ()
  "Calls the ChatGPT API"
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (let* ((selected-text (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
						  ""
                          ))
	 (filtered-text (my-exclude-org-roam-link selected-text))
	 (input-string (read-string "Enter a prompt: "))
		 (script-output (my-call-python-script "~/Dropbox/Agenda/org-roam-ai/chatgpt_api.py" filtered-text input-string))
        )
	(with-output-to-temp-buffer "*OpenAI Output*"
	  (with-current-buffer "*OpenAI Output*"
        (insert script-output)))
	))

(defun lgm/chatgpt-assistant-prompt ()
  "Calls the ChatGPT API"
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
  (let* ((selected-text (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
						  ""
                          ))
	 (filtered-text (my-exclude-org-roam-link selected-text))
	 (input-string (read-string "Enter a prompt: "))
		 (script-output (my-call-python-script "~/Dropbox/Agenda/org-roam-ai/chatgpt_api.py" filtered-text input-string))
        )
	(with-output-to-temp-buffer "*OpenAI Output*"
	  (with-current-buffer "*OpenAI Output*"
        (insert script-output)))
	))


(use-package org-tree-slide
  :ensure t
  :init
  ;; To start the presentation from the header the cursor is in
  (setq org-tree-slide-cursor-init nil)
  (add-hook 'org-tree-slide-mode-hook 'lgm/org-presentation-mode)
  )

(defun toggle-header-line-format ()
  "Toggle the value of `header-line-format' between nil and \" \""
  (interactive)
  (if header-line-format
      (setq header-line-format nil)
    (setq header-line-format " ")))

(defvar face-remapping-alist-config-1
  '((default (:height 1.5) variable-pitch)
    (header-line (:height 4.0) variable-pitch)
    (org-document-title (:height 1.75) org-document-title)
    (org-code (:height 1.55) org-code)
    (org-verbatim (:height 1.55) org-verbatim)
    (org-block (:height 1.25) org-block)
    (org-block-begin-line (:height 0.7) org-block)
    (org-level-1 (:height 2.0) org-block)
    (org-level-2 (:height 1.5) org-block)
    (org-level-3 (:height 1.5) org-block)
    (org-level-4 (:height 1.5) org-block)
	))

(defvar face-remapping-alist-config-2
  '((default variable-pitch default)))

(defun toggle-face-remapping ()
  (interactive)
  (if (equal face-remapping-alist face-remapping-alist-config-1)
      (setq-local face-remapping-alist face-remapping-alist-config-2)
    (setq-local face-remapping-alist face-remapping-alist-config-1)))

(defun lgm/org-presentation-mode ()
  (toggle-header-line-format)
  (toggle-face-remapping))

;; org-present
;; Inspired by https://systemcrafters.net/emacs-tips/presentations-with-org-present/
;; Install org-present if needed
(unless (package-installed-p 'org-present)
  (package-install 'org-present))

(use-package org-present
  :ensure t
  )

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children)

  ;; Unfold children
  (org-fold-show-subtree)
  )


(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)
				     (org-level-1 (:height 1.7) org-block)
				     (org-level-2 (:height 1.1) org-block)
				     (org-level-3 (:height 1.05) org-block)
				     (org-level-4 (:height 1.05) org-block)
									 ))
  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images))

(defun my/org-present-start ()
  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.3)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka" :weight 'medium :height (cdr face)))

  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (dolist (face '((org-level-1 . 1.0)
                (org-level-2 . 1.0)
                (org-level-3 . 1.0)
                (org-level-4 . 1.0)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "Iosevka" :weight 'normal :height (cdr face)))

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)
;; -- org present --

;; Hide markupt elements, like the * for bold
(setq org-hide-emphasis-markers t)

;; So I can use <q + TAB to create a quote block quickly, s for SRC
(require 'org-tempo)

;; To check the links under link words in org-mode
(defun show-org-link-target ()
  (interactive)
  (when (org-in-regexp org-link-bracket-re 1)
    (message "Target: %s" (org-link-unescape (match-string 1)))))

(global-set-key (kbd "C-c s") 'show-org-link-target)

;; It selects a header from the same org file I'm in and link it
;; It is useful when I want to refer other parts of the file, but they are not org roam nodes.
(defun org-insert-link-to-header ()
  "Insert a link to an existing header in the current Org file."
  (interactive)
  (let ((headers (org-element-map (org-element-parse-buffer) 'headline
                   (lambda (headline)
                     (cons
                      (org-element-property :raw-value headline)
                      (org-element-property :begin headline))))))
    (if headers
        (let* ((choice (completing-read "Choose a header: " (mapcar 'car headers)))
               (header (assoc choice headers)))
          (if header
              (let* ((begin (cdr header))
                     (link (format "[[*%s][%s]]" choice choice)))
                (goto-char (point))
                (insert link))
            (message "Header not found.")))
      (message "No headers found in this Org file."))))

(define-key org-mode-map (kbd "C-c n u") 'org-insert-link-to-header)

;; So I can quickly share org-roam with people without looking for the pdf file in the roam folder
(defun lgm/export-org-to-downloads-folder ()
  "Export the current Org file to PDF and save it in the downloads folder."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (pdf-file (concat (file-name-sans-extension org-file) ".pdf"))
         (downloads-folder "~/Downloads/")) ; Change this path to your desired folder

    (unless (and org-file (file-exists-p org-file))
      (error "Buffer is not visiting a file or file does not exist"))

    (org-latex-export-to-pdf) ; Export Org file to PDF

    ;; Move the exported PDF to the specified folder
    (if (file-exists-p pdf-file)
        (rename-file pdf-file (concat downloads-folder (file-name-nondirectory pdf-file)) t)
      (error "PDF file not found after export"))

    (message "Org file exported to PDF and saved in %s" downloads-folder)))

;; Makes it easy to share images I've saved in org files
(defun lgm/org-copy-linked-image-to-downloads ()
  "Copy the linked image at point in Org mode to ~/Downloads/."
  (interactive)
  (when (org-in-regexp org-link-any-re 1)
    (let* ((link (org-element-context))
           (type (org-element-type link))
           (path (org-element-property :path link))
           (dest-dir "~/Downloads/"))
      (when (and (eq type 'link)
                 (file-exists-p path))
        (let* ((file-name (lgm/file-name-from-path path))
               (new-path (expand-file-name file-name dest-dir)))
          (copy-file path new-path t)
          (message "Copied image to: %s" new-path))))))

(defun lgm/file-name-from-path (path)
  "Extract the file name from PATH."
  (let ((parts (split-string path "/")))
    (car (last parts))))

;; Enable using org mode link syntax inside other modes
;; (use-package org-link-minor-mode
;;   :load-path  "~/.emacs.d/elisp/org-link-minor-mode")

;; (add-hook 'markdown-mode-hook 'org-link-minor-mode)



;; accept broken links when exporting
(setq org-export-with-broken-links nil)

;; A way to fix all broken links from org-roam
;; I still need to see if I will find this issue from time to time
;; (org-id-update-id-locations (directory-files-recursively "~/Dropbox/Agenda/roam/" "\\.org$"))

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(provide 'org-settings)
;;; org-settings.el ends here
