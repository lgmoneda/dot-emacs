;;; editor-settings.el --- Settings for editing utilities

;; Save place
;; Start from the last place you were in a file the next time you visit it
(use-package saveplace
  :defer 3
  :init
  ;; Always resolve symlinks (avoids duplicate entries)
  (setq find-file-visit-truename t
        vc-follow-symlinks t)
  (save-place-mode 1)
  :custom
  (save-place-file (expand-file-name ".my-saved-places" user-emacs-directory))
  (save-place-ignore-files-regexp
   "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|elpa\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
  (save-place-forget-unreadable-files t)
  ;; :hook
  ;; ;; Make sure Org buffers also track their place
  ;; (org-mode . (lambda () (setq-local save-place t)))
  )


;; Optional: periodic save every 10 minutes (safe & async)
(run-with-idle-timer 600 600 #'save-place-kill-emacs-hook)

;; Garbage Collector teste
(setq gc-cons-threshold 20000000)

;; Adjusting Mouse sensitivity
(setq mouse-wheel-progressive-speed nil)

;; Let Elisp be less conservative
(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-list-eval-depth (* 15 max-lisp-eval-depth))

;; Move cursor after split
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Do not centralize cursor
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Better shell buffer behavior
(use-package shell-pop
 :ensure t
 :init (defalias 'sp 'shell-pop))

;; A package to jump to the last modification
(use-package goto-chg
  :ensure t)

;; Jump to last change
(global-set-key (kbd "M-[") 'goto-last-change)
(global-set-key (kbd "M-]") 'goto-last-change-reverse)

(use-package smartparens
  :ensure t
  :init
  (with-eval-after-load 'smartparens
  (sp-local-pair 'org-mode "'" "'" :actions '(rem)))

  )

;; Writeroom, a focus mode!
(use-package writeroom-mode
 :ensure t
 :init (setq buffer-face-mode-face '(:family "dejavu sans mono" :height 150))
 (setq writeroom-width 92))

;; Expand-region
(use-package expand-region
	     :ensure t)

;; When files have the same name, it informs enough part of their path
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; When we diminish a mode,
;; we are saying we want it to continue doing its work for us,
;; but we no longer want to be reminded of it.
(use-package diminish
    :ensure t
    :config
  (eval-after-load "hideshow" '(diminish 'hs-minor-mode))
  (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
  (eval-after-load "simple" '(diminish 'overwrite-mode))
  (eval-after-load "autorevert" '(diminish 'auto-revert-mode)))

;; Anzu shows total matchs for searchs
(use-package anzu
	     :ensure t
	     :config (global-anzu-mode))

;; Multiple cursors
;; First mark the word, then add more cursors
;; If you want to insert a new line in multiple cursors mode, use C-j
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-x , m" . mc/edit-lines)
   ("C-x , n" . mc/edit-ends-of-lines)))

;; Some initial confis
;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq  truncate-lines nil
       inhibit-startup-screen t
       initial-scratch-message ""
       fill-column 80)

;; Functions to copy words at point, from:
;; https://www.emacswiki.org/emacs/CopyWithoutSelection
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
	       (progn (comint-next-prompt 25535) (yank))
	     (progn (goto-char (mark)) (yank) )))))
    (if arg
	(if (= arg 1)
	    nil
	  (funcall pasteMe))
      (funcall pasteMe))
    ))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (kill-new (thing-at-point 'word))
  ;;(copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w") (quote copy-word))

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (message "You're awesome!")
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c l") (quote copy-line))

;; Trying to start using marker
;; C-<space> C-<space> to leave mark
;; C-u C-<space>
;; C-<space> again to previous marks
;; C-<space> C-s "char" to selec region between mark and I-search
;; ^ or use ace-jump!
;; C-x C-<space> global jump to last mark
(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-=") 'er/expand-region)
;; This is a faster option for something I do regularly in org
;; I often copy a word in a TODO entry to paste a link
(global-set-key (kbd "C-+") 'er/mark-word)

;; Creates a new line without breaking the current line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

(global-set-key (kbd "C-*")
    (lambda ()
      (interactive)
      (isearch-forward-symbol-at-point)))

;; Add blank line at the end of a file
(setq require-final-newline t)

;; Scroll while centering (for M-v and C-v)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)

;; Hide your password when prompted
(require 'comint)
(setq comint-password-prompt-regexp
		    (concat comint-password-prompt-regexp
			    "\\|^Password for .*:\\s *\\'"))

;; Defining switch tabs commands
(global-set-key [C-iso-lefttab]
    (lambda ()
      (interactive)
      (other-window -1)))

;; MacOS version
(global-set-key (kbd "C-S-<tab>")
    (lambda ()
      (interactive)
      (other-window -1)))

;; Defining switch tabs commands
(global-set-key [C-tab]
    (lambda ()
      (interactive)
      (other-window 1)))

;; Defining switch buffer command
(global-set-key (kbd "C-8")
    (lambda ()
      (interactive)
      (switch-to-prev-buffer)))

(global-set-key (kbd "C-9")
    (lambda ()
      (interactive)
      (switch-to-next-buffer)))

;; Defining switch buffer command
(global-set-key (kbd "C-7")
    (lambda ()
      (interactive)
      (bury-buffer)))

;; Tryng to save my hand
(global-set-key (kbd "C-0")
    (lambda ()
      (interactive)
      (other-frame 1)))

;; Smartparens dependency
(use-package dash
  :ensure t)

;; Pair parenthesis
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (with-eval-after-load 'smartparens
	(sp-local-pair 'org-mode "'" "'" :actions '(rem)))

  :config
  ;; Now i can express sadness in erc-mode
  (sp-local-pair 'erc-mode "(" nil :actions nil))

;; Smartparens keyibinding management
;; Sexp is a Symbolic Expression, something like (+ 40 2)

;; Big jump, jumps the outer ()
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; Enter the next at beginning ()
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)

;; Enter the last sexp at end
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)

;; Goes to the begining of the current sexp you're in
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)

;; Changes position under cursor element with the next
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

;; Replace highlighted text
(delete-selection-mode 1)

;; avy
(use-package avy
  :ensure t)

(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "M-c") 'avy-goto-char)
(global-set-key (kbd "C-,") 'avy-goto-char)
(global-set-key (kbd "C-?") 'avy-goto-line)

;; To be able to easily rescale the text on the fly
(use-package default-text-scale
	     :ensure t)

(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

;; Disable auto-save
(setq auto-save-default nil)
;; Disable backup
(setq backup-inhibited t)

;; Use super-save
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))
;; Save when emacs is idle
(setq super-save-auto-save-when-idle t)
;; Save silently
(setq super-save-silent t)

;; 100kb is enough to skip auto-saving
(defvar my-super-save-max-size (* 0.1 1024 1024)
  "Maximum buffer size (in bytes) for which `super-save' should trigger (default 2 MB).")

(defvar-local my-super-save-banned nil
  "Non-nil means this buffer is banned from `super-save' auto-saving this session.")

(defun my-super-save--eligible-p (buf)
  "Return non-nil if BUF should be saved by super-save."
  (with-current-buffer buf
    (and buffer-file-name
         (buffer-modified-p)
         (not my-super-save-banned)
         (< (buffer-size) my-super-save-max-size))))

(defun my-super-save-check-and-save ()
  "Check all eligible buffers and save them safely."
  (dolist (buf (buffer-list))
    (when (my-super-save--eligible-p buf)
      (with-current-buffer buf
        (super-save--command)))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (not my-super-save-banned)
                 (>= (buffer-size) my-super-save-max-size))
        (setq my-super-save-banned t)
        (message "[super-save] ⚠️ Skipping large file: %s (%.2f MB)"
                 (file-name-nondirectory buffer-file-name)
                 (/ (buffer-size) 1048576.0))))))

;; Store original function before overriding
(unless (fboundp 'super-save--command)
  (defalias 'super-save--command (symbol-function 'super-save-command)))

;; Override safely (no recursion)
(advice-add 'super-save-command :override #'my-super-save-check-and-save)

;; Disable mouse
(use-package disable-mouse
  :ensure t
  :init (global-disable-mouse-mode))

;; Saves clipboard to the kill ring in case
;; you kill something before you paste into Emacs
(setq save-interprogram-paste-before-kill t)

;; Set the minimum level of warning to pop up
(setq warning-minimum-level :error)

;; Define the shell I want to use
(setq explicit-shell-file-name "/bin/zsh")

;; Improve the candidates sorting
(use-package prescient
  :straight t
  :config
  (prescient-persist-mode 1))

;; Functions around revert to streamline working in org notebooks
;; assisted by AI coding tools like Claude Code
(setq auto-revert-verbose nil) ;; don’t spam minibuffer
(setq revert-without-query '(".*")) ;; accept reverts silently for all files

(provide 'editor-settings)
;;; editor-settings.el ends here
