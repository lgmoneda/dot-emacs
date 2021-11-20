;;; aesthetics-settings.el --- Settings for the theme, custom colors and faces

(use-package rebecca-theme
  :ensure t)

(use-package dracula-theme
  :ensure t)

;; Load Theme
(setq custom-safe-themes t)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (load-theme 'dracula)
	    ))

(setq theme-background-color (frame-parameter nil 'background-color))

(use-package powerline
	     :ensure t)

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
;;(setq show-paren-style 'expression)

;; Enable line numbers
(global-linum-mode 0)

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
 '(markdown-command "/usr/local/bin/pandoc")
 '(olivetti-body-width 105)
 '(org-roam-db-location "/Users/luis.moneda/Dropbox/agenda/roam/org-roam.db" nil nil "Customized with use-package org-roam")
 '(org-roam-directory "/Users/luis.moneda/Dropbox/agenda/roam" nil nil "Customized with use-package org-roam")
 '(package-selected-packages
   '(annotate keytar lsp-grammarly flycheck-grammarly scala-mode all-the-icons-ivy-rich wwg wwe flymake-aspell flycheck-aspell smart-mode-line sublime-themes zygospore academic-phrases ob-ipython ein all-the-icons restclient-test helm-bibtex interleave org-roam-protocol langtool flycheck-vale flyspell-vale helm-org-ql deft org-ql org-helm-rifle org idle-org-agenda ivy-posframe scihub gscholar-bibtex bibtex-completion org-roam-bibtex virtualenvwrapper olivetti company-org-roam google-translate org-roam-server org-roam emojify writegood-mode disable-mouse dracula-theme dap-mode lsp-treemacs lsp-ivy helm-lsp lsp-ui lsp-mode company-tabnine rainbow-mode engine-mode elfeed-goodies elfeed-org elfeed ess processing-mode elpy pdf-tools ivy-bibtex org-ref org-projectile-helm helm-org counsel-spotify helm-ag helm-rg clojure-mode-extra-font-locking pyvenv conda which-key org-gcal org-journal git-timemachine magit flymd markdown-mode restclient company kaolin-themes rebecca-theme cherry-blossom-theme doom-modeline doom-themes helm-org-rifle org-wild-notifier py-autopep8 cyberpunk-theme org-timeline fortune-cookie helm-spotify-plus paredit spacemacs-theme lsp-typescript sml-mode org-notify cider clj-refactor clojure-mode go-mode org-alert color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized sanityinc-color-theme power-line docker helm-tramp docker-tramp 0blayout counsel-projectile counsel ivy exec-path-from-shell auctex default-text-scale slack ensime writeroom darkroom column-enforce-mode org-bullets latex-preview-pane scheme-complete quack org-dashboard pyimport electric-operator multi diff-hl avy markdown-preview-mode beacon helm-company company-quickhelp company-flx company-anaconda anaconda-mode neotree auto-complete smex ag imenu-anywhere flx-ido ido-vertical-mode anzu thing-cmds rainbow-delimiters expand-region try helm base16-theme spinner monokai-theme hydra))
 '(paradox-github-token t)
 '(pdf-tools-handle-upgrades nil)
 '(show-paren-match ((t (:weight 'extra-bold))))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t)
 '(warning-suppress-types '((lsp-mode) (comp) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:background "dark gray" :foreground "maroon3" :weight bold))))
 '(avy-lead-face-0 ((t (:background "dark gray" :foreground "maroon3" :weight bold))))
 '(avy-lead-face-1 ((t (:background "dark gray" :foreground "maroon4" :weight bold))))
 '(avy-lead-face-2 ((t (:background "dark gray" :foreground "maroon5" :weight bold))))
 '(company-tooltip-search ((t (:inherit highlight :background "steel blue"))))
 '(company-tooltip-search-selection ((t (:background "steel blue"))))
 '(cursor ((t (:background "#e16527"))))
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(ein:cell-input-area ((t (:background "black"))))
 '(font-latex-math-face ((t (:foreground "#f8834f"))))
 '(font-latex-script-char-face ((t (:foreground "dark gray"))))
 '(fringe ((t (:background nil))))
 '(lazy-highlight ((t (:foreground "white" :background "SteelBlue"))))
 '(mode-line ((t (:background "#663399" :foreground "#ae81ff" :inverse-video nil :box (:line-width 1 :color "#292b2e" :style released-button)))))
 '(org-agenda-date ((t (:foreground "#8be9fd" :underline nil :weight bold))))
 '(org-agenda-date-today ((t (:foreground "#f1fa8c" :underline nil :weight extra-bold))))
 '(org-agenda-done ((t (:foreground "#50fa7b"))))
 '(org-agenda-structure ((t (:foreground "#bd93f9" :weight ultra-bold :height 1.2))))
 '(org-block ((t (:background "black" :foreground "gray100"))))
 '(org-document-info ((t (:background "#292a44"))))
 '(org-document-info-keyword ((t (:background "#292a44"))))
 '(org-document-title ((t (:background "#292a44" :foreground "#6dfedf" :weight bold :height 1.44))))
 '(org-done ((t (:weight bold :box (:line-width 1 :color nil :style none) :foreground "#6dfedf" :background "#383a62"))))
 '(org-ellipsis ((t (:foreground "#969896" :underline nil))))
 '(org-link ((t (:foreground "#ff79c6" :underline t))))
 '(org-ref-cite-face ((t (:inherit org-link :foreground "#50fa7b"))))
 '(org-scheduled ((t (:foreground "chocolate1"))))
 '(org-scheduled-previously ((t (:foreground "#8eaee0"))))
 '(org-scheduled-today ((t (:foreground "chocolate1"))))
 '(org-tag ((t (:background "#292a44" :foreground "#ae81ff" :weight bold))))
 '(org-todo ((t (:weight bold :box (:line-width 1 :color nil :style none) :foreground "#8eaee0"))))
 '(org-upcoming-deadline ((t (:foreground "chocolate1"))))
 '(region ((t (:background "#f1fa8c" :foreground "#000000"))))
 '(show-paren-match ((t (:background "#5C888B" :weight bold)))))

;; Change M-x position
(use-package ivy-posframe
  :ensure t
  :init
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
      '((left-fringe . 30)
        (right-fringe . 30)
	))
  (setq ivy-posframe-width 120
		ivy-posframe-height 50)
  (setq ivy-posframe-border-width 4)

  (ivy-posframe-mode 1)
  )

(provide 'aesthetics-settings)
;;; aesthetics-settings.el ends here
