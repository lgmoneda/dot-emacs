;; Load Theme
(add-hook 'emacs-startup-hook
  (lambda ()
    (load-theme 'deeper-blue)
    ))

;; Package Management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
     '("melpa" . "https://melpa.org/packages/"))
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

;; Company to display pop-ups 
(use-package company
  :ensure t)

(add-hook 'after-init-hook 'global-company-mode)

;; Company-Anaconda
(use-package company-anaconda
  :ensure t)

;; Pair parenthesis
(use-package autopair
  :ensure t)

(autopair-global-mode)

;; (eval-after-load "company"
;;   '(add-to-list 'company-backends 'company-anaconda))

(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; Run python first time 
(defun run-python-once ()
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python (python-shell-parse-command)))

(add-hook 'python-mode-hook 'run-python-once)

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
  :init (beacon-mode 1)
        (setq beacon-color "#00ff00")
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

(global-set-key (kbd "C-:") 'avy-goto-char-1)
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

;; Run python and pop-up its shell
(defun my-python-shell-run ()
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  )

(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  )

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-run)
     (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)
     ))
