(add-hook 'emacs-startup-hook
  (lambda ()
    (load-theme 'deeper-blue)
    ))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("elpy" . "<http://jorgenschaefer.github.io/packages/>")))



(add-to-list 'load-path "~/.emacs.d/elpa/neotree-20170110.321")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;Defining switch tabs commands
(global-set-key [C-iso-lefttab] 
    (lambda ()
      (interactive)
      (other-window -1)))

;Defining switch tabs commands
(global-set-key [C-tab] 
    (lambda ()
      (interactive)
      (other-window -1)))
;Initialize in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'upcase-region 'disabled nil)

;Which-key minor mode
(add-to-list 'load-path "~/.emacs.d/elpa/which-key-20161222.1221")
(require 'which-key)
(which-key-mode)

;Turn the system sound off
(setq ring-bell-function 'ignore)

(require 'cl)
(setq cfg-var: packages '(
		elpy
		flycheck
		company
))

(defun cfg:install-packages ()
	(let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
		(when pkgs
			(message "%s" "Emacs refresh packages database&#x2026;")
			(package-refresh-contents)
			(message "%s" " done.")
			(dolist (p cfg-var:packages)
				(package-install p)))))
(package-initialize)
(cfg:install-packages)


;;; Enabling Elpy
(elpy-enable)
(remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")
(setq elpy-rpc-backend "jedi")

(add-hook 'python-mode-hook
		  (lambda ()
			(setq-default indent-tabs-mode nil)
			(setq-default tab-width 4)
			(setq-default python-indent 4)))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
