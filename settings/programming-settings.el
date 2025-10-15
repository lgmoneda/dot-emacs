;;; programming-settings.el --- Settings for the programming in general

;; Enable eldoc in your programming modes
(use-package eldoc
  :ensure t
  :diminish
  :commands eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
	eldoc-echo-area-use-multiline-p nil)
  (eldoc-mode 1)
  :config
  (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

;; Enable hide definitions functions
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key [f4] 'hs-toggle-hhiding)

;; Test http rest webservices inside emacs
;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t)

;; From https://github.com/Automattic/harper/discussions/150
(use-package eglot
  :defer
  :hook
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  ;; (text-mode . eglot-ensure)
  :bind (:map
         eglot-mode-map
         ("C-c e r" . eglot-rename)
         ("C-c e a" . eglot-code-actions)
         ("C-c e o" . eglot-code-action-organize-imports)
         ("C-c e d" . eldoc)
         ("C-c e f" . eglot-format)
         ("C-c e =" . eglot-format))
  :config
  (add-to-list 'eglot-server-programs
               '((text-mode :language-id "plaintext") . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((english-prose-mode :language-id "plaintext") . ("harper-ls" "--stdio")))
  :custom
  (eglot-autoshutdown t) ;; default is to leave servers runing when last buffer exits
  (eglot-events-buffer-size 0) ;; disable events recording to speed
                               ;; things up, comment out to
                               ;; troubleshoot and look for the EGLOT
                               ;; buffer (eglot-stay-out-of
                               ;; '(yasnippet))
  (eglot-extend-to-xref nil) ;; cover files found through xref (M-.)
  )

;; Corfu — popup completion in buffer
;; corfu-insert-separator is M-SPC, use it to filter candidates
(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.5
        corfu-auto-prefix 2
        corfu-quit-no-match 'separator
        corfu-preselect 'first
        corfu-popupinfo-delay '(0.3 . 0.1))
  ;; Comment to make it dynamic
  ;; Make min and max equal to make it fixed
  (setq corfu-min-width 20    ; minimum popup width (in columns)
      corfu-max-width 200)   ; maximum popup width (fixed size)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (org-roam-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :config
  (corfu-popupinfo-mode)

    ;; Navigate popup docs
  (define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)
  (define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down)
  (define-key corfu-map (kbd "M-d") #'corfu-popupinfo-toggle)

  ;; ───── corfu-quick ─────
  ;; Avy-style quick selection
  (require 'corfu-quick)
  (define-key corfu-map (kbd "M-q") #'corfu-quick-complete) ; pick & exit
  (define-key corfu-map (kbd "C-s") #'corfu-quick-insert)  ; pick & stay
  :custom
  (corfu-scroll-margin 4)
  ;; Have Corfu wrap around when going up
  (corfu-cycle t)
  ;; Enable Corfu history mode to act like `prescient'
  (corfu-history-mode t)
  )


(defun my/corfu-org-roam-buffer-setup ()
  "Tame completion in Org-roam note buffers."
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-roam-file-p)
             (org-roam-file-p))              ;; ← true only for Roam files
    ;; timing: calmer in Roam notes
    (setq-local corfu-auto t)
    (setq-local corfu-auto-delay 0.7)
    (setq-local corfu-auto-prefix 4)
))

(add-hook 'org-mode-hook #'my/corfu-org-roam-buffer-setup)

;; Make completions case-insensitive
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-case-fold t) ; extra safety for new Emacs versions

;; Explicitly tell Orderless to fold case even when input starts uppercase
(setq orderless-smart-case nil)

(use-package cape
  :demand t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (setq-default completion-at-point-functions
                (append (default-value 'completion-at-point-functions)
                        (list #'cape-dabbrev #'cape-file #'cape-abbrev))))

;; ──────────────────────────────
;; Org-roam specific CAPF setup
;; ──────────────────────────────
(defun my/org-roam-capf-only ()
  "Restrict completion in Org-roam buffers to Org-roam node completion only."
  (setq-local completion-at-point-functions
              (list #'org-roam-complete-everywhere)))

(add-hook 'org-roam-mode-hook #'my/org-roam-capf-only)

(add-to-list 'treesit-extra-load-path "/Users/luis.moneda/.emacs.d/tree-sitter/")

;; treesit
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t) ;; or 't to auto-install
  (global-treesit-auto-mode))

(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
	       (elisp . ("https://github.com/Wilfred/tree-sitter-elisp" "v0.3.0"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
	     (elisp-mode . elisp-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
)

;; Try later
;; (use-package dap-mode
;;   :ensure t)
;; (dap-mode 1)
;; ;; The modes below are optional
;; (dap-ui-mode 1)
;; ;; enables mouse hover support
;; (dap-tooltip-mode 1)
;; ;; use tooltips for mouse hover
;; ;; if it is not enabled `dap-mode' will use the minibuffer.
;; (tooltip-mode 1)
;; ;; displays floating panel with debug buttons
;; ;; requies emacs 26+
;; (dap-ui-controls-mode 1)

(provide 'programming-settings)
;;; programming-settings.el ends here
