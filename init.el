(setq user-emacs-directory (file-truename "~/.emacs.d/"))

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
      (other-window 1)))


;Defining switch frames command
(global-set-key (kbd "C-2")
    (lambda ()
      (interactive)
      (other-frame 1)))

;Defining switch buffer command
(global-set-key (kbd "C-1")
    (lambda ()
      (interactive)
      (bury-buffer)))

;Initialize in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'upcase-region 'disabled nil)

;Which-key minor mode
(add-to-list 'load-path "~/.emacs.d/elpa/which-key-20161222.1221")
(require 'which-key)
(which-key-mode)

;Turn the system sound off
(setq ring-bell-function 'ignore)

;Beacon minor mode
(add-to-list 'load-path "~/.emacs.d/elpa/beacon-20161004.756")
(require 'beacon)
(beacon-mode 1)
(setq beacon-color "#00ff00")
(setq beacon-size 60)
(setq beacon-blink-delay 0.5)

