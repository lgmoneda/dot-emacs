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
(global-set-key [f4] 'hs-toggle-hiding)

(use-package company
  :ensure t
  :diminish
  :defer 4
  :init
  (progn
    (global-company-mode)
    )
  :config (progn
            (setq company-tooltip-limit 10
                  company-idle-delay 0.5
                  company-echo-delay 0.5
                  company-begin-commands '(self-insert-command  self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash )
                  company-transformers '(company-sort-by-occurrence)
                  company-selection-wrap-around t
                  company-minimum-prefix-length 6
                  company-dabbrev-downcase nil
		  company-require-match nil
		  company-tooltip-maximum-width 60
		  company-tooltip-minimum-width 60
                  )
            (bind-keys :map company-active-map
		       ("C-s" . company-filter-candidates)
                       ("C-n" . company-select-next)
                       ("C-p" . company-select-previous)
		       ("C-d" . company-quickhelp-manual-begin)
                       ("<tab>" . company--insert-candidate)
                       ("<escape>" . company-abort)
                       )
            )
  )

(use-package company-flx
  :ensure t)

(use-package company-posframe
  :ensure t
  :init
  (company-posframe-mode 1))

(with-eval-after-load 'company
  (company-flx-mode +1))

;; Not so useful, but eventually...
;; It wasn't loading properly
;; (use-package helm-company
;;   :ensure t
;;   :config (eval-after-load 'company
;;   '(progn
;;     (define-key company-active-map (kbd "C-:") 'helm-company))))

;; Test http rest webservices inside emacs
;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t)

;; From https://github.com/Automattic/harper/discussions/150
(use-package eglot
  :defer
  ;; :hook
  ;; (python-mode . eglot-ensure)
  ;; (text-mode . eglot-ensure)
  :bind (:map
         eglot-mode-map
         ("C-c e r" . eglot-rename)
         ("C-c e a" . eglot-code-actions)
         ("C-c e o" . eglot-code-action-organize-imports)
         ;; ("C-c e d" . eldoc)
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
  (eglot-extend-to-xref nil)) ;; cover files found through xref (M-.)

(provide 'programming-settings)
;;; programming-settings.el ends here
