;;; aesthetics-settings.el --- Settings for the theme, custom colors and faces

;; Hides the title bar
(setq default-frame-alist '((undecorated . t)))

(use-package rebecca-theme
  :ensure t)

(use-package mindre-theme
  ;; :ensure t
    :straight (:local-repo "/Users/luis.moneda/repos/mindre-theme/")
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
  :custom
  ;; 🌙 Smooth and subtle
  (beacon-color "#5c3e99")
  (beacon-blink-duration 0.15)         ;; quick fade
  (beacon-size 30)                     ;; smaller, subtler beam
  (beacon-blink-delay 0.2)
  (beacon-push-mark 1)
  (beacon-blink-when-point-moves-vertically 2)
  (beacon-blink-when-point-moves-horizontally nil)
  (beacon-blink-when-window-scrolls nil)
  (beacon-blink-when-buffer-changes t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-focused nil)      ;; no blink when switching apps
  :config
  (beacon-mode 1))

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
    (python-mode . "🐍")
    (python-ts-mode . "🐍")
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

;; Comment and find out if I really need it
;; (use-package minions
;;   :ensure t
;;   :init
;;   (minions-mode 1))

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
 '(conda-anaconda-home "/Users/luis.moneda/miniconda3")
 '(package-selected-packages
   '(straight org-sliced-images gptel ob-mermaid aider aidermacs langtool
			  org-modern nov shrface consult vertico citar-org-roam
			  citar-embark citar ox-bibtex ivy-bibtex ox-gfm
			  spacious-padding golden-ratio embark-consult embark
			  exec-path-from-shell smex ob-chatgpt-shell
			  quelpa-use-package copilot org-emms emms chatgpt-shell
			  pyimport electric-operator org-present org-tree-slide
			  org-transclusion org-mind-map consult-org-roam
			  org-download org-remark org-ql org-roam-bibtex olivetti
			  org-roam-ui org-roam deadgrep org-bullets
			  virtualenvwrapper simple-httpd column-enforce-mode conda
			  pyvenv pyenv-mode pyenv magit default-text-scale
			  zygospore writeroom-mode with-editor which-key try
			  smartparens smart-mode-line shell-pop restclient
			  rebecca-theme rainbow-delimiters poet-theme pdf-tools
			  org-ref orderless ob-async neotree multiple-cursors
			  multi mood-line minions mindre-theme
			  markdown-preview-mode lsp-grammarly latex-preview-pane
			  ivy-posframe imenu-anywhere hide-mode-line helm-bibtex
			  gscholar-bibtex goto-chg google-translate flymd
			  expand-region engine-mode disable-mouse diminish diff-hl
			  counsel-projectile company-posframe company-flx beacon
			  auto-complete anzu annotate all-the-icons-ivy-rich ag))
 '(package-vc-selected-packages
   '((agent-shell :url "https://github.com/xenodium/agent-shell")
	 (acp :url "https://github.com/xenodium/acp.el")
	 (claude-code :url
				  "https://github.com/stevemolitor/claude-code.el")))
 '(pdf-tools-handle-upgrades nil))

;; Vertico-posframe dependency
(use-package posframe
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

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
  )

;; Mindre's original main-bg color is F5F5F5
;; FAF9F5
(defun lgm/set-mindre-theme ()
  "Set the mindre theme"
  (interactive)
  (load-theme 'mindre)
  (global-hl-line-mode 0)
  ;; (setq beacon-color "#000000")
  (set-face-attribute 'org-hide nil :inherit 'unspecified :foreground "#f8f4ed" :background "#f8f4ed")
  (set-face-attribute 'org-indent nil :foreground "#f8f4ed" :background "#f8f4ed")
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
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(hl-tags-face ((t (:inherit highlight))))
 '(jupyter-repl-input-prompt ((t (:foreground "SeaGreen4"))))
 '(org-link ((nil (:foreground "cornflowerblue" :underline nil))))
 '(org-quote ((t (:inherit mindre-default :slant italic)))))

;; Change font size
;; Iosevka
(set-face-attribute 'default nil :height 160 :font "Iosevka")
;; (set-face-attribute 'default nil :height 180 :font "JetBrains Mono")

;; Hide mode line
(use-package hide-mode-line
  :ensure t)

;; Automatically resizes the window I'm focusing
;; (use-package golden-ratio
;;   :ensure t
;;   :init
;;   (golden-ratio-mode 1)
;;   )

;; I'm interested only in timestamps, todo keywords, and tables.
;; I disable the rest since I prefer my own customizations or other packages.
(use-package org-modern
  :ensure t
  :init
   (setq org-modern-star nil)
   (setq org-modern-block-name nil)
   (setq org-modern-keyword nil)
   (setq org-modern-todo-faces
      '(("TODO" :inherit (org-modern-todo mindre-keyword mindre-strong) :weight semi-bold)
        ("STARTED" :inherit org-modern-label :foreground "SeaGreen" :inverse-video t)
        ("DELEGATED" :inherit org-modern-label :foreground "VioletRed3" :inverse-video t)
        ("WAIT" :inherit org-modern-label :foreground "HotPink4" :inverse-video t)
        ("INACTIVE" :inherit org-modern-label :foreground "grey")
        ("CANCELED" :inherit org-modern-label :foreground "blue" :weight bold)
        ("FAIL" :inherit org-modern-label :foreground "blue" :weight bold)))

   (defface org-modern-bullet-face
    '((t :inherit default :height 1.2)) ; tweak as needed
    "Face for org-modern bullets.")

  (setq org-modern-star
        (mapcar (lambda (c)
                  (propertize c 'face 'org-modern-bullet-face))
                '("◉" "○" "✸" "✿" "✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥")))

   (add-hook 'org-mode-hook #'org-modern-mode)
   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
   ;; (global-org-modern-mode)
   )

(provide 'aesthetics-settings)
;;; aesthetics-settings.el ends here
