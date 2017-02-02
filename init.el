;Load Theme
(add-hook 'emacs-startup-hook
  (lambda ()
    (load-theme 'deeper-blue)
    ))

;Package Management
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

(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle))


(use-package helm-spotify
  :ensure t)

;(global-set-key [f8] 'neotree-toggle)

;Hiding menu and tool bar
(menu-bar-mode -99)
(tool-bar-mode -99)

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

;Defining switch buffer command
(global-set-key (kbd "C-1")
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
;(add-to-list 'load-path "~/.emacs.d/elpa/which-key-20161222.1221")

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :bind ([f8] . neotree-toggle))


;; (require 'which-key)
;; (which-key-mode)

;Turn the system sound off
(setq ring-bell-function 'ignore)


;Beacon minor mode
;(add-to-list 'load-path "~/.emacs.d/elpa/beacon-20161004.756")
(require 'beacon)
(beacon-mode 1)
(setq beacon-color "#00ff00")
(setq beacon-size 60)
(setq beacon-blink-delay 0.5)

(use-package beacon
  :ensure t
  :init (beacon-mode 1)
        (setq beacon-color "#00ff00")
	(setq beacon-size 60)
	(setq beacon-blink-delay 0.5)
	(which-key-mode))



;Beacon minor mode
;(add-to-list 'load-path "~/.emacs.d/elpa/ein-20170131.844")
;(require 'ein)
(use-package ein
  :ensure t)


