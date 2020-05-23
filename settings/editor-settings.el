;;; editor-settings.el --- Settings for editing utilities

;; Save place
;; Start from the last place you were in a file the next time you visit it
;; It's a emacs24.5 or older way to do it
;; (require 'saveplace)
;; (setq-default save-place t)
;; For emacs 25+
(save-place-mode 1)
(setq save-place-file "~/.emacs.d/saveplace.log")

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

;; Save command history
(savehist-mode)

;; Better shell buffer behavior
(use-package shell-pop
 :ensure t
 :init (defalias 'sp 'shell-pop))

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

;; Auto-complete
(use-package auto-complete
  :ensure t)

;; Neotree
(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle))

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

;; Selection functions utilities
;; Stop using arrows for selection!
(defun mark-a-word-or-thing (arg)
   "..."
  (interactive "P")
  (cond ((or arg mark-active)
         (when (consp arg) (setq current-prefix-arg nil))
         (call-interactively 'mark-thing))
        (t
         (skip-syntax-backward "w_")
         (mark-thing 'word))))

(global-set-key (kbd "C-+") 'mark-a-word-or-thing)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Creates a new line without breaking the current line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Some functions from EmacsWiki
(add-to-list 'load-path "~/.emacs.d/elisp/misc/" t)

;; Jump to last change
(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)

(global-set-key (kbd "C-*")
    (lambda ()
      (interactive)
      (isearch-forward-symbol-at-point)))

;; Add blank line at the end of a file
(setq require-final-newline t)

;; Scroll while centering
(setq scroll-error-top-bottom 'true)
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
(global-set-key (kbd "C-3")
    (lambda ()
      (interactive)
      (switch-to-prev-buffer)))

(global-set-key (kbd "C-4")
    (lambda ()
      (interactive)
      (switch-to-next-buffer)))

;; Defining switch buffer command
(global-set-key (kbd "C-'")
    (lambda ()
      (interactive)
      (bury-buffer)))

;; Defining switch frames command
(global-set-key (kbd "C-2")
    (lambda ()
      (interactive)
      (other-frame 1)))

;; Tryng to save my hand
(global-set-key (kbd "C-0")
    (lambda ()
      (interactive)
      (other-frame 1)))

;; Pair parenthesis
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
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
;; (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
;; Changes position under cursor element with the next
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)


(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)


;; (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;; (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)


(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

;; Replace highlighted text
(delete-selection-mode 1)

;; avy
(use-package avy
  :ensure t)

(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "M-s") 'avy-goto-char)
(global-set-key (kbd "C-x a") 'avy-goto-char-timer)
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

(provide 'editor-settings)
;;; editor-settings.el ends here
