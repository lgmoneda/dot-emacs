;;; productivity-settings.el --- Settings for packages that increase emacs productivity
;; Dash
(use-package dash
  :ensure t)

;; Helm
(use-package helm
  :ensure t)

;; (add-to-list 'load-path "~/.emacs.d/elpa/helm-3.9.0")
;; (load "helm")

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

;; Motivated by org-roam-find-node
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

	     ;; Case insensitive
	     (setq ivy-case-fold-search-default t)
	     ;; Use ordeless
	     (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
	     (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
	     )

(add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/counsel-20240520.1323")
(load "counsel")
(autoload 'counsel "counsel" "" t)

(use-package counsel
	     :ensure t
	     :init (counsel-mode)
	     (global-set-key (kbd "M-X") 'counsel-M-x)
	     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
	     (global-set-key (kbd "<f1> f") 'counsel-describe-function)
	     (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
	     (global-set-key (kbd "<f1> l") 'counsel-find-library)
	     (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
	     (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
	     (global-set-key (kbd "C-c C-j") 'counsel-imenu)

	     :bind
	     ;; Use ivy to search the kill-ring, but
	     ;; keep the previous behavior
	     (("M-y" . counsel-yank-pop)
		    :map ivy-minibuffer-map
		    ("M-y" . ivy-next-line-and-call))
	     )

;; imenu-anywhere
;; Changes the C-c C-j behavior
(use-package imenu-anywhere
  :ensure t
  :init
  )

;; Ag (search)
(use-package ag
  :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  ;; (setq projectile-keymap-prefix (kbd "C-c p"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Smart Mode Line already displays project name
  ;; :config (setq projectile-mode-line'(:eval (format " P[%s]" (projectile-project-name))))
  :config (setq projectile-mode-line'(:eval (format "" (projectile-project-name))))
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  ;; (add-to-list 'projectile-globally-ignored-directories ".cache")
  :bind (("C-c p s" . projectile-ag)
         ("C-c p g" . projectile-grep)))

;; project.el
(require 'project)
;; This functions let me place a .project file in a folder and have it treated
;; like a project
(defun my/project-try-dot-project (dir)
  "Detect a `.project` marker and return an external project."
  (let ((root (locate-dominating-file dir ".project")))
    (when root
      (list 'external root))))

(cl-defmethod project-root ((project (head external)))
  (cadr project))

(add-hook 'project-find-functions #'my/project-try-dot-project)
;; (add-hook 'project-find-functions #'my/project-try-dot-project)

(use-package counsel-projectile
	     :ensure t
	     :init
	     (counsel-projectile-mode)
	     )

;; Which-key minor mode
(use-package which-key
  :ensure t
  :init
  (setq which-key-show-early-on-C-h t)
  (setq whickh-key-idle-delay 60000)
  (setq which-key-idle-secondary-delay 0.35)
  (which-key-mode)
  )

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

(defun lgm/look-for-def-in-project ()
  (interactive)
  (progn
   (er/mark-word)
   (counsel-projectile-ag-word " def ")
   )
  )

(defun lgm/look-for-selected-word-in-project ()
  (interactive)
  (progn
   (counsel-projectile-ag-word (buffer-substring (region-beginning) (region-end)))
   )
  )

(define-key projectile-command-map (kbd "s w") 'lgm/look-for-word-in-project)
(define-key projectile-command-map (kbd "s f") 'lgm/look-for-def-in-project)
(global-set-key (kbd "C-M-*") 'lgm/look-for-selected-word-in-project)

;; (require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
;; for shell-mode
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

;; Easily copy file path
(defun file-path-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(use-package engine-mode
  :ensure t
  :init (engine-mode t)
  (defengine google
    "https://www.google.com/search?q=%s"
    :keybinding "g"))

;;zygospore lets you revert C-x 1 (delete-other-window) by pressing C-x 1 again
(use-package zygospore
  :ensure t
  :init (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

(use-package quelpa-use-package
  :ensure t)

;; (use-package chatgpt
;;   :quelpa ((chatgpt :fetcher git :url "https://github.com/joshcho/ChatGPT.el.git") :upgrade t)
;;   :init
;;   (require 'python)
;;   (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
;;   (setq chatgpt-repo-path (expand-file-name "chatgpt/" quelpa-build-dir))
;;   ;; :bind ("C-c q" . chatgpt-query)
;;   )

;; Templates from work
(load-file "~/Dropbox/Projetos/Emacs/work-templates.el")

(provide 'productivity-settings)
;;; productivity-settings.el ends here
