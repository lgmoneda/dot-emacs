;;; aesthetics-settings.el --- Settings for the theme, custom colors and faces

(use-package rebecca-theme
  :ensure t)

(use-package mindre-theme
    :ensure t
    :custom
    (mindre-use-more-bold nil)
    (mindre-use-faded-lisp-parens t)
    )

;; Load Theme
(setq custom-safe-themes t)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (lgm/set-mindre-theme)
	    ))

(setq theme-background-color (frame-parameter nil 'background-color))

;; Display time in the mode-line
(setq display-time-format "%Hh%M ")
(setq display-time-default-load-average nil)

;; Show time in mode-line when using Emacs in fullscreen,
;; avoiding using it three days in a row without sleeping
(global-set-key (kbd "<f9>") (lambda()
				(interactive)
				(toggle-frame-fullscreen)
				;; In MacOS it takes a while to update frame params
				(sit-for 1)
				;; Now it works with multiple screens :)
				(if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'fullboth)
				;; (if (eq display-time-mode nil)
				    (display-time-mode 1)
				    (display-time-mode 0))
				))

;; Highligh current line!
(global-hl-line-mode +1)
(set-face-background 'hl-line "#292b2e")

;; Set cursor color
;; older: #ea51b2, #edac2c
(set-cursor-color "#e16527")

;; Initialize in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'upcase-region 'disabled nil)

;;Beacon minor mode
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init (beacon-mode 1)
        (setq beacon-color "#e16527")
        (setq beacon-size 25)
	(setq beacon-blink-delay 0.3)
	)

;; Mode line α
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . "")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (hs-minor-mode . "")
    (which-key-mode . "")
    (smartparens-mode . "")
    (counsel-mode . "")
    (ivy-mode . "")
    (abbrev-mode . "")
    (column-enforce-mode . "")
    (company-mode . "")
    (eldoc-mode . "")
    (Org-Indent . "")
    (visual-line-mode . "")
    (anzu-mode . "")
    (flyspell-mode . "")
    (color-identifiers-mode . "")
    (auto-revert-mode . "")
    (org-indent-mode . "")
    (disable-mouse-global-mode . "")
    ;; Major modes
    (fundamental-mode . "Fund")
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(use-package minions
  :ensure t
  :init
  (minions-mode 1))

(use-package mood-line
  :ensure t
  :init
  (mood-line-mode 1))

;; Make annoying yes or no less annoying
(defalias 'yes-or-no-p 'y-or-n-p)

;; Global visual line
(global-visual-line-mode t)
(diminish 'visual-line-mode)

;; Hiding menu, tool bar and scroll bar
(menu-bar-mode -99)
(tool-bar-mode -99)
(scroll-bar-mode -1)
;; (setq default-frame-alist '((undecorated . t)))

;; More thinner window divisions
(fringe-mode '(4 . 3))

;; Outside border to make it better in fullscreen mode
(add-to-list 'default-frame-alist '(internal-border-width . 2))

;; Enable paren mode at start
(show-paren-mode 1)
;;Highlights the whole expression
;; (setq show-paren-style 'expression)

;; Enable line numbers
;; (global-linum-mode 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "dark slate gray")
 '(company-quickhelp-color-foreground "wheat")
 '(conda-anaconda-home "/Users/luis.moneda/opt/miniconda3")
 '(display-time-mail-string "")
 '(ein:output-area-inlined-images t)
 '(ein:use-auto-complete t t)
 '(ein:use-auto-complete-superpack t)
 '(elfeed-goodies/entry-pane-position 'bottom)
 '(elfeed-goodies/show-mode-padding 40)
 '(global-hl-line-mode t)
 '(markdown-command "/usr/local/bin/pandoc")
 '(olivetti-body-width 105)
 '(org-roam-db-location "/Users/luis.moneda/Dropbox/agenda/roam/org-roam.db" nil nil "Customized with use-package org-roam")
 '(package-selected-packages
   '(hide-mode-line org-present org-tree-slide org-tree-slides magit magit-section khoj whisper chatgpt-shell org-transclusion biblio-core zmq helm-bibtext org-mind-map py-autopep8 pyimport orderless lsp-grammarly ivy-bibtex pdf-tools emacs-jupyter ob-async smudge mindre-theme chatgpt epc ctable concurrent quelpa-use-package quelpa org-download poet-theme exotica-theme company-posframe company-postframe ivy-clipmenu org-roam-ui flycheck-grammarly scala-mode wwg wwe smart-mode-line sublime-themes zygospore academic-phrases ob-ipython ein restclient-test helm-bibtex interleave org-roam-protocol langtool flycheck-vale flyspell-vale helm-org-ql deft org-helm-rifle idle-org-agenda ivy-posframe scihub gscholar-bibtex bibtex-completion org-roam-bibtex virtualenvwrapper olivetti company-org-roam google-translate org-roam-server emojify disable-mouse dracula-theme dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui company-tabnine engine-mode elfeed-goodies elfeed-org elfeed ess processing-mode elpy org-projectile-helm helm-org counsel-spotify helm-ag helm-rg clojure-mode-extra-font-locking pyvenv which-key org-gcal org-journal flymd restclient kaolin-themes rebecca-theme cherry-blossom-theme doom-modeline doom-themes helm-org-rifle org-wild-notifier cyberpunk-theme org-timeline fortune-cookie helm-spotify-plus paredit spacemacs-theme lsp-typescript sml-mode org-notify cider clj-refactor go-mode org-alert color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized sanityinc-color-theme power-line docker helm-tramp docker-tramp 0blayout counsel-projectile exec-path-from-shell default-text-scale slack ensime writeroom darkroom column-enforce-mode org-bullets latex-preview-pane scheme-complete quack org-dashboard electric-operator multi avy markdown-preview-mode beacon helm-company company-quickhelp company-flx company-anaconda neotree smex ag imenu-anywhere flx-ido ido-vertical-mode anzu thing-cmds rainbow-delimiters expand-region try base16-theme spinner monokai-theme hydra))
 '(paradox-github-token t)
 '(pdf-tools-handle-upgrades nil)
 '(show-paren-match ((t (:weight 'extra-bold))))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t)
 '(warning-suppress-types
   '((with-editor)
     (with-editor)
     (initialization)
     (initialization)
     (websocket)
     (ox-pandoc)
     (org-roam)
     (lsp-mode)
     (comp)
     (:warning))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(avy-lead-face ((t (:background "dark gray" :foreground "maroon3" :weight bold))))
;;  '(avy-lead-face-0 ((t (:background "dark gray" :foreground "maroon3" :weight bold))))
;;  '(avy-lead-face-1 ((t (:background "dark gray" :foreground "maroon4" :weight bold))))
;;  '(avy-lead-face-2 ((t (:background "dark gray" :foreground "maroon5" :weight bold))))
;;  '(company-tooltip-search ((t (:inherit highlight :background "steel blue"))))
;;  '(company-tooltip-search-selection ((t (:background "steel blue"))))
;;  '(ein:cell-input-area ((t (:background "black"))))
;;  '(font-latex-math-face ((t (:foreground "#f8834f"))))
;;  '(font-latex-script-char-face ((t (:foreground "dark gray"))))
;;  '(org-agenda-date-today ((t (:foreground "#f1fa8c" :underline nil :weight extra-bold))))
;;  '(org-block ((t (:extend t :background "black" :foreground "#6dfedf"))))
;;  '(org-link ((t (:background "DarkOrchid4" :foreground "SteelBlue2" :underline nil))))
;;  '(org-ref-cite-face ((t (:inherit org-link :foreground "#50fa7b"))))
;;  '(show-paren-match ((t (:background "#5C888B" :weight bold)))))

;; Change M-x position
(use-package ivy-posframe
  :ensure t
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
      '((left-fringe . 15)
        (right-fringe . 15)
	))
  (setq ivy-posframe-width 150
	ivy-posframe-height 11)
  (setq ivy-posframe-border-width 2)
  (ivy-posframe-mode 1)
  )

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init
  (all-the-icons-ivy-rich-mode 1)
  :config
  ;; Whether display the icons
  (setq all-the-icons-ivy-rich-icon t)
  ;; Whether display the colorful icons.
  ;; It respects `all-the-icons-color-icons'.
  (setq all-the-icons-ivy-rich-color-icon t)
  ;; The icon size
  (setq all-the-icons-ivy-rich-icon-size 1.0)
  ;; Whether support project root
  (setq all-the-icons-ivy-rich-project t)
  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  (setq inhibit-compacting-font-caches t)
  )

;; Poet theme change
;; https://explog.in/notes/poet.html
(use-package poet-theme
  :ensure t)

(defun lgm/set-rebecca-theme ()
  "Set the rebecca theme"
  (interactive)
  (load-theme 'rebecca)
  (set-face-attribute 'org-link t :background "DarkOrchid4" :foreground "SteelBlue2" :underline nil)
  (set-face-attribute 'avy-lead-face nil :background "dark gray" :foreground "maroon3" :weight 'bold)
  (set-face-attribute 'avy-lead-face-0 nil :background "dark gray" :foreground "maroon3" :weight 'bold)
  (set-face-attribute 'avy-lead-face-1 nil :background "dark gray" :foreground "maroon4" :weight 'bold)
  (set-face-attribute 'avy-lead-face-2 nil :background "dark gray" :foreground "maroon5" :weight 'bold)
  (set-face-attribute 'company-tooltip-search t :inherit 'highlight :background "steel blue")
  (set-face-attribute 'company-tooltip-search-selection t :background "steel blue")
  ;; (set-face-attribute 'ein:cell-input-area t :background "black")
  ;; (set-face-attribute 'font-latex-math-face t :foreground "#f8834f")
  ;; (set-face-attribute 'font-latex-script-char-face t :foreground "dark gray")
  (set-face-attribute 'org-block nil :extend t :background "black" :foreground "#6dfedf")
  (set-face-attribute 'org-link nil :background "purple4" :foreground "SteelBlue2" :underline nil)
  (set-face-attribute 'org-ref-cite-face nil :inherit 'org-link :foreground "#50fa7b")
  (set-face-attribute 'show-paren-match nil :background "#5C888B" :weight 'bold)
  (set-face-attribute 'org-agenda-date-today nil :foreground "#f1fa8c" :underline nil :weight 'extra-bold)

  (setq org-todo-keyword-faces
      '(
	("NEXT" . "pink")
	("STARTED" . "yellow")
	("WAIT" . "magenta")
	("INACTIVE" . (:foreground "grey"))
        ("CANCELED" . (:foreground "blue" :weight bold))
        ("FAIL" . (:foreground "blue" :weight bold))))
  ;; (custom-set-faces
  ;;  '(avy-lead-face ((t (:background "dark gray" :foreground "maroon3" :weight bold))))
  ;;  '(avy-lead-face-0 ((t (:background "dark gray" :foreground "maroon3" :weight bold))))
  ;;  '(avy-lead-face-1 ((t (:background "dark gray" :foreground "maroon4" :weight bold))))
  ;;  '(avy-lead-face-2 ((t (:background "dark gray" :foreground "maroon5" :weight bold))))
  ;;  '(company-tooltip-search ((t (:inherit highlight :background "steel blue"))))
  ;;  '(company-tooltip-search-selection ((t (:background "steel blue"))))
  ;;  '(ein:cell-input-area ((t (:background "black"))))
  ;;  '(font-latex-math-face ((t (:foreground "#f8834f"))))
  ;;  '(font-latex-script-char-face ((t (:foreground "dark gray"))))
  ;;  '(org-agenda-date-today ((t (:foreground "#f1fa8c" :underline nil :weight extra-bold))))
  ;;  '(org-block ((t (:extend t :background "black" :foreground "#6dfedf"))))
  ;;  '(org-link ((t (:background "DarkOrchid4" :foreground "SteelBlue2" :underline nil))))
  ;;  '(org-ref-cite-face ((t (:inherit org-link :foreground "#50fa7b"))))
  ;;  '(show-paren-match ((t (:background "#5C888B" :weight bold)))))
  )

(defun lgm/set-poet-theme ()
  "Set the poet light theme"
  (interactive)
  (load-theme 'poet)
  (global-hl-line-mode 0)
  (set-face-attribute 'org-hide nil :inherit 'unspecified :foreground "#e1d9c2" :background "#e1d9c2")
  (set-face-attribute 'org-indent nil :foreground "#e1d9c2" :background "#e1d9c2")
  (set-face-attribute 'org-link nil :foreground "cornflowerblue" :underline nil)
  (set-face-attribute 'org-agenda-date-today nil :box nil :background "#e1d9c2" :foreground "darkgreen" :underline nil :weight 'extra-bold)

  ;; (custom-set-faces
  ;;  '(org-hide ((t (:inherit unspecified :foreground "#e1d9c2" :background "#e1d9c2"))))
  ;;  '(org-indent ((t (:foreground "#e1d9c2" :background "#e1d9c2"))))
  ;;  '(org-agenda-date-today ((t (:box nil :background "#e1d9c2" :foreground "darkgreen" :underline nil :weight extra-bold))))
  ;;  '(org-link ((t (:foreground "cornflowerblue" :underline nil)))))
  )

(defun lgm/set-mindre-theme ()
  "Set the mindre theme"
  (interactive)
  (load-theme 'mindre)
  (global-hl-line-mode 0)
  (setq beacon-color "#000000")
  (set-face-attribute 'org-hide nil :inherit 'unspecified :foreground "#F5F5F5" :background "#F5F5F5")
  (set-face-attribute 'org-indent nil :foreground "#F5F5F5" :background "#F5F5F5")
  (set-face-attribute 'org-link nil :foreground "cornflowerblue" :underline nil)
  (set-face-attribute 'org-agenda-clocking nil :inherit 'unspecified)
  (setq org-todo-keyword-faces
		'(
		  ("TODO" . (:inherit (mindre-keyword mindre-strong) :weight medium))
		  ("STARTED" . "SeaGreen")
		  ("DELEGATED" . "VioletRed3")
		  ("WAIT" . "HotPink4")
		  ("INACTIVE" . (:foreground "grey"))
          ("CANCELED" . (:foreground "blue" :weight bold))
          ("FAIL" . (:foreground "blue" :weight bold))))
  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:font "Iosevka" :height 160))))
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(jupyter-repl-input-prompt ((t (:foreground "dark cyan"))))
 '(org-todo ((t (:inherit (mindre-keyword mindre-strong) :weight medium)))))

;; Change font size
;; Iosevka
(set-face-attribute 'default nil :height 160 :font "Iosevka")

;; Hide mode line
(use-package hide-mode-line
  :ensure t)

(provide 'aesthetics-settings)
;;; aesthetics-settings.el ends here
