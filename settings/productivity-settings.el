;;; productivity-settings.el --- Settings for packages that increase emacs productivity
;; Dash
(use-package dash
  :ensure t)

;; Helm
(use-package helm
  :ensure t)

;; Try
(use-package try
  :ensure t)

;; Smart Mode line
(use-package smart-mode-line
    :ensure t
    :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  ;; (setq sml/theme 'dark)
  (sml/setup))

;Monday, September 18, 2017
;============================
;==          Ivy           ==
;============================

(use-package ivy
	     :ensure t
	     :init
	     (ivy-mode 1)
	     (setq ivy-initial-inputs-alist nil)
	     ;; (setq ivy-use-virtual-buffers t)
	     ;; (setq enable-recursive-minibuffers t)

	     (setq ivy-re-builders-alist
		   '((ivy-switch-buffer . ivy--regex-plus)
		     (counsel-ag . ivy--regex-plus)
		     (t . ivy--regex-fuzzy)))

	     (setq ivy-use-virtual-buffers t)
	     (setq ivy-count-format "(%d/%d) ")

	     ;; Use C-j for immediate termination with the current value, and RET
	     ;; for continuing completion for that directory. This is the ido
	     ;; behaviour.
	     (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
	     (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
	     )

(use-package counsel
	     :ensure t
	     :init (counsel-mode)
	     (global-set-key (kbd "M-X") 'counsel-M-x)
	     ;; (global-set-key (kbd "C-s") 'swiper)
	     (global-set-key (kbd "M-x") 'counsel-M-x)
	     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
	     (global-set-key (kbd "<f1> f") 'counsel-describe-function)
	     (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
	     (global-set-key (kbd "<f1> l") 'counsel-find-library)
	     (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
	     (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

	     :bind
	     ;; Use ivy to search the kill-ring, but
	     ;; keep the previous behavior
	     (("M-y" . counsel-yank-pop)
		    :map ivy-minibuffer-map
		    ("M-y" . ivy-next-line-and-call))
	     )

(use-package counsel-projectile
	     :ensure t
	     :init
	     (counsel-projectile-mode))

;; imenu-anywhere
;; Changes the C-c C-j behavior
(use-package imenu-anywhere
  :ensure t
  :init
  ;; I want imenu, not new journal entry!
  (global-set-key (kbd "C-c C-j") 'imenu-anywhere)
  )

;; Ag (search)
(use-package ag
  :ensure t)

;; Smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :config
  ;; Using counsel now
  ;;(global-set-key (kbd "M-X") 'smex)
  ;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  ;;(global-set-key (kbd "M-x") 'execute-extended-command)
  )

;; Projectile

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  ;; (setq projectile-keymap-prefix (kbd "C-c p"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Smart Mode Line already displays project name
  ;; :config (setq projectile-mode-line'(:eval (format " P[%s]" (projectile-project-name))))
  :config (setq projectile-mode-line'(:eval (format "" (projectile-project-name))))
  :bind (("C-c p s" . projectile-ag)
         ("C-c p g" . projectile-grep)))


;; Which-key minor mode
(use-package which-key
  :ensure t
  :init (which-key-mode))

;;Turn the system sound off
(setq ring-bell-function 'ignore)

;; Add Hydra
(use-package hydra
  :ensure t)

;; custom package
;; load the custom helm-spotify-plus
(use-package multi
  :ensure t)

;; Highlight matching tags
(load-file "~/.emacs.d/others/hl-tags-mode/hl-tags-mode.el")
(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode)))
(add-hook 'html-mode-hook (lambda () (hl-tags-mode)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode)))

(defun counsel-projectile-ag-word (word &optional options)
  "Search the current project with ag.

OPTIONS, if non-nil, is a string containing additional options to
be passed to ag. It is read from the minibuffer if the function
is called with a prefix argument."
  (let* ((ignored (mapconcat (lambda (i)
                               (concat "--ignore "
                                       (shell-quote-argument i)
                                       " "))
                             (append (projectile-ignored-files-rel)
                                     (projectile-ignored-directories-rel))
                             ""))
         (options
          (if current-prefix-arg
              (read-string (projectile-prepend-project-name "ag options: ")
                           ignored
                           'counsel-projectile-ag-options-history)
            (concat ignored options))))
    (counsel-ag word
                (projectile-project-root)
                options
                (projectile-prepend-project-name "ag"))))

(defun lgm/look-for-word-in-project ()
  (interactive)
  (progn
   (er/mark-word)
   (counsel-projectile-ag-word (buffer-substring (region-beginning) (region-end)))
   )
  )

(defun lgm/look-for-selected-word-in-project ()
  (interactive)
  (progn
   (counsel-projectile-ag-word (buffer-substring (region-beginning) (region-end)))
   )
  )

(define-key projectile-command-map (kbd "s w") 'lgm/look-for-word-in-project)
(global-set-key (kbd "C-M-*") 'lgm/look-for-selected-word-in-project)

(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
;; for shell-mode
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(provide 'productivity-settings)
;;; productivity-settings.el ends here
