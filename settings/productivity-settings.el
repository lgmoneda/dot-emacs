;;; productivity-settings.el --- Settings for packages that increase emacs productivity

;; Try
(use-package try
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  ;; Matching style
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion))
     (command (styles orderless))
     (symbol (styles orderless))
     (variable (styles orderless))
     (buffer (styles orderless))))

  ;; Always ignore case — no “smart” switching
  (orderless-smart-case nil)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

;; But use plain/basic matching for Consult’s async results (ripgrep, grep, line, etc.)
(with-eval-after-load 'consult
  (add-to-list 'completion-category-overrides
               '(consult-location (styles . (basic)))))

;; Vertico replaces Ivy
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 12) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil                ;; comes bundled with Vertico
  :bind (:map vertico-map
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-prescient
  :straight t
  :after vertico
  :init
  (vertico-prescient-mode 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode 1)

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Centralize the vertico
(use-package vertico-posframe
  :ensure t
  :after vertico
  :custom
  ;; Frame behavior
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 2)
  (vertico-posframe-width 160)
  (vertico-posframe-min-width 60)
  (vertico-posframe-min-height 10)
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)
     (internal-border-width . 2)
     ))
  :config
  (vertico-posframe-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; imenu-anywhere
;; Changes the C-c C-j behavior
(use-package imenu-anywhere
  :ensure t
  :init
  )

;; This seems redundant, but my project identification
;; function doesn't work without it.
(require 'project)
;; project.el
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :init
  (setq project-mode-line '(:eval
                            (when-let ((pr (project-current)))
                              (format " [%s]" (project-name pr)))))
  (setq project-vc-merge-submodules t)
  (setq project-switch-commands 'project-find-file)
  (setq project-list-file (expand-file-name "projects" user-emacs-directory))
  :config
  ;; Add Consult integrations
  (define-key project-prefix-map (kbd "s") #'consult-ripgrep)
  (define-key project-prefix-map (kbd "b") #'consult-project-buffer)
  (define-key project-prefix-map (kbd "f") #'project-find-file)
  (define-key project-prefix-map (kbd "k") #'project-kill-buffers)
  )

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

(define-prefix-command 'my-M-s-map)
(global-set-key (kbd "M-s") 'my-M-s-map)
(global-set-key (kbd "M-s d") 'consult-dir)

;; Free it for Consult
(global-unset-key (kbd "M-."))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :custom
  (consult-preview-key "M-.")
  (consult-project-function
      (lambda (_)
        (or
         (when-let ((root (locate-dominating-file default-directory ".git")))
           root)
         (when-let ((project (project-current)))
           (car (project-roots project)))
         default-directory)))
  (consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=1000 \
--path-separator / --smart-case --no-heading --line-number")
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.1)
  (consult-async-input-throttle 0.2)


  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;; (consult-customize
  ;;  consult-theme :preview-key '(:debounce 0.2 any)
  ;;  consult-ripgrep consult-git-grep consult-grep consult-man
  ;;  consult-bookmark consult-recent-file consult-xref
  ;;  consult--source-bookmark consult--source-file-register
  ;;  consult--source-recent-file consult--source-project-recent-file
  ;;  :preview-key '(:debounce 0.4 any))
  (consult-customize consult-ripgrep :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package consult-project-extra
  :straight t
  :custom (consult-project-function #'consult-project-extra-project-fn) ;; Optional but recommended for a more consistent UI
  :config
  (define-key project-prefix-map (kbd "f") #'consult-project-extra-find)
  (define-key project-prefix-map (kbd "o") #'consult-project-extra-find-other-window)
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

(use-package multi
  :ensure t)

;; Highlight matching tags
(setq web-mode-enable-current-element-highlight t)
(use-package web-mode
  :ensure t
  :mode ("\\.\\(html\\|htm\\|xhtml\\|php\\|phtml\\|jsp\\|as[cp]x\\|erb\\|djhtml\\|liquid\\|vue\\|svelte\\|ejs\\)\\'" . web-mode)
  :init
  ;; General settings
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-indentation t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight nil
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-comment-style 2)
  :config
  ;; Optional: inherit comment style from the current language block
  (setq web-mode-comment-formats
        '(("javascript" . "//") ("css" . "/*") ("php" . "//") ("html" . "<!--")))
  )

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-r") #'consult-history)))

(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-r") #'consult-comint-history))

;; Easily copy file path
(defun lgm/file-path-to-clipboard ()
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

;;zygospore lets you revert C-x 1 (delete-other-window) by pressing C-x 1 again
(use-package zygospore
  :ensure t
  :init (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

(use-package quelpa-use-package
  :ensure t)

;; Templates from work
;; This is great and I should explore it in the future
;; (load-file "~/Dropbox/Projetos/Emacs/work-templates.el")

(provide 'productivity-settings)
;;; productivity-settings.el ends here
