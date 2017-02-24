;; Load Theme
;; Use 'monokai when missing sublime
(setq custom-safe-themes t)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (load-theme 'monokai)))

;; Package Management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
     '("melpa" . "https://melpa.org/packages/"))
     ;; '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
     '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Helm
(use-package helm
  :ensure t)

;; Ido
(use-package ido
  :ensure t
  :init (ido-mode)
        (setq ido-enable-flex-matching t)
        (ido-everywhere t)
	(setq ido-file-extensions-order '(".py" ".org" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")))

(setq ido-use-faces nil)

;; Ido-vertical
(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode))

;; Ido ubiquitous
(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

;; Ido ubiquitous
(use-package flx-ido
  :ensure t
  :init (flx-ido-mode))

;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :bind (("C-c p s" . projectile-ag)
         ("C-c p g" . projectile-grep)))

;; Neotree
(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle))

;; Spotify
(use-package helm-spotify
  :ensure t)

;; Automatic enable anaconda-mode in all Python buffers
(add-hook 'python-mode-hook 'anaconda-mode)

;; Enable hide definitions functions
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key [f4] 'hs-toggle-hiding)

;; Company to display pop-ups 
(use-package company
  :ensure t)

(add-hook 'after-init-hook 'global-company-mode)

;; Company-Anaconda
(use-package company-anaconda
  :ensure t)

;; Pair parenthesis
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode))

;; Display time in the mode-line
(display-time-mode)

;; Autopair inhibits eldoc!
;; (use-package autopair
;;   :ensure t)

;;(autopair-global-mode)

;; Multiple cursors
;; First mark the word, then add more cursors
;; If you want to insert a new line in multiple cursors mode, use C-j
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-x , m" . mc/edit-lines)))

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; (eval-after-load "company"
;;   '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; Run python first time 
;; (defun run-python-once ()
;;   (remove-hook 'python-mode-hook 'run-python-once)
;;   (run-python (python-shell-parse-command)))

;; (add-hook 'python-mode-hook 'run-python-once)

;; Hiding menu and tool bar
(menu-bar-mode -99)
(tool-bar-mode -99)

;; Defining switch tabs commands
(global-set-key [C-iso-lefttab] 
    (lambda ()
      (interactive)
      (other-window -1)))

;Defining switch tabs commands
(global-set-key [C-tab] 
    (lambda ()
      (interactive)
      (other-window 1)))

;Defining switch buffer command
(global-set-key (kbd "C-1")
    (lambda ()
      (interactive)
      (switch-to-prev-buffer)))

;Defining switch buffer command
(global-set-key (kbd "C-'")
    (lambda ()
      (interactive)
      (bury-buffer)))

;Defining switch frames command
(global-set-key (kbd "C-2")
    (lambda ()
      (interactive)
      (other-frame 1)))

;Initialize in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'upcase-region 'disabled nil)

;Which-key minor mode
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :bind ([f8] . neotree-toggle))

;;Turn the system sound off
(setq ring-bell-function 'ignore)

;;Beacon minor mode
(use-package beacon
  :ensure t
  :diminish beacon-mode "Bacon"
  :init (beacon-mode 1)
        ;; For deeper-blue theme
        ;;(setq beacon-color "#00ff00")
        ;; For monokai theme
        (setq beacon-color "#A6E22E")
	(setq beacon-size 60)
	(setq beacon-blink-delay 0.5))

;; Emacs Ipython Notebook
(use-package ein
  :ensure t)

(require 'comint)
(setq comint-password-prompt-regexp
		    (concat comint-password-prompt-regexp
			    "\\|^Password for .*:\\s *\\'"))

;; Enable paren mode at start
(show-paren-mode 1)

;; Enable line numbers
(global-linum-mode 1)

;; avy
(use-package avy
  :ensure t)

(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-?") 'avy-goto-line)


;; custom package
;; load the custom helm-spotify-plus
(use-package multi
  :ensure t)

(load-file "~/repos/helm-spotify-plus/helm-spotify-plus.el")

;; Replace highlighted text
(delete-selection-mode 1)

;; Highlight matching tags
(load-file "~/.emacs.d/others/hl-tags-mode/hl-tags-mode.el")
(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode)))
(add-hook 'html-mode-hook (lambda () (hl-tags-mode)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode)))

;; enable eldoc in your programming modes
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Jedi
;; (setq jedi-mode t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; Disable auto-save
(setq auto-save-default nil)
;; Disable backup
(setq backup-inhibited t)


;; Python
(require 'python)
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
(define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
                               (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t))))))


;; (setq pop-up-frames nil)
;; (defun my-python-shell-run ()
;;   (interactive)
;;   (python-shell-send-buffer)
;;   (python-shell-switch-to-shell)
;;   )

;; Run python and pop-up its shell.
;; Kill process to solve the reload modules problem.
(defun my-python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
     (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
     (kill-process (get-buffer-process "*Python*"))
     ;; If you want to clean the buffer too.
     ;;(kill-buffer "*Python*")
     ;; Not so fast!
     (sleep-for 0.5)
     )
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  ;; Pop new window only if shell isn't visible
  ;; in any frame.
  (unless (get-buffer-window "*Python*" t) 
    (python-shell-switch-to-shell)
    )
 )

(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell)
  )

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-run)
     (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)
     ))


;; Trying to make TAB great again
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)
    ))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(dabbrev-expand nil))
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (dabbrev-expand nil)
	(indent-for-tab-command)))))

(defun my-smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(dabbrev-expand nil))
   (if mark-active
	(python-indent-shift-right (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (dabbrev-expand nil)
	(indent-for-tab-command)))))

(defun my-smart-backtab ()
  (interactive)
  (if mark-active
      (python-indent-shift-left (region-beginning)
				 (region-end))
    (if (looking-at "\\_>")
	(dabbrev-expand nil)
      (python-indent-dedent-line))))

;;(define-key python-mode-map (kbd "TAB") 'indent-or-complete)
(define-key python-mode-map (kbd "TAB") 'my-smart-tab)
(define-key python-mode-map (kbd "<backtab>") 'my-smart-backtab)

(defun fancy-tab (arg)
  (interactive "P")
  (setq this-command last-command)
  (if (or (eq this-command 'hippie-expand) (looking-at "\\_>"))
      (progn
	(setq this-command 'hippie-expand)
	(hippie-expand arg))
    (setq this-command 'indent-for-tab-command)
    (indent-for-tab-command arg)))


;; (define-key read-expression-map [(tab)] 'hippie-expand)
;; (global-set-key (kbd "TAB") 'fancy-tab)

;; (add-hook 'python-mode-hook
;;   (lambda () (setq indent-tabs-mode t)))

;; (setq company-idle-delay nil)

;; (setq-default tab-always-indent 'complete)

;;(define-key company-mode-map (kbd "TAB") 'company-complete-common)

;; (defun indent-or-complete ()
;;   "Complete if point is at end of line, and indent line."
;;   (interactive)
;;   (if (and (looking-at "$") (not (looking-back "^\\s-*")))
;;       (hippie-expand nil))
;;   (indent-for-tab-command)
;;   )

;; Adjusting Mouse sensitivity
(setq mouse-wheel-progressive-speed nil)

;; Trying to reproduce arrow keys
;; (define-key key-translation-map (kbd "C-l") (kbd "\C-b"))
;; (define-key key-translation-map (kbd "M-l") (kbd "M-b"))
;; (define-key key-translation-map (kbd "<C-dead-tilde>") (kbd "\C-f"))
;; (define-key key-translation-map (kbd "<C-dead-tilde>") (kbd "M-f"))
;; (define-key key-translation-map (kbd "C-ç") (kbd "\C-n"))

