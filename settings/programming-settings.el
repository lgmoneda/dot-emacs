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
  :init (progn
          (global-company-mode)
          (setq company-global-modes '(not python-mode cython-mode sage-mode ein:notebook-modes org-mode markdown-mode))
          )
  :config (progn
            (setq company-tooltip-limit 12
                  company-idle-delay 1.0
                  company-echo-delay 0.5
                  company-begin-commands '(self-insert-command  self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash )
                  company-transformers '(company-sort-by-occurrence)
                  company-selection-wrap-around t
                  company-minimum-prefix-length 2
                  company-dabbrev-downcase nil
		  company-require-match nil
                  )
;;	    (bind-keys :map company-mode-map
;;		       ("<tab>" . company-complete))
            (bind-keys :map company-active-map
		       ("C-s" . company-filter-candidates)
                       ("C-n" . company-select-next)
                       ("C-p" . company-select-previous)
		       ("C-d" . company-quickhelp-manual-begin)
                       ;;("C-d" . company-show-doc-buffer)
                       ("<tab>" . company--insert-candidate)
                       ("<escape>" . company-abort)
                       )
            )
  )

(use-package company-flx
  :ensure t)

(with-eval-after-load 'company
  (company-flx-mode +1))

;; Pop documentation help for Company
;; M-x customize-group <RET> company-quickhelp <RET>
(use-package company-quickhelp
  :ensure t
  ;; To see doc just press C-d in company candidate
  :init (company-quickhelp-mode 0)
  :config
  (eval-after-load 'company
  '(define-key company-active-map (kbd "C-d") #'company-quickhelp-manual-begin)))

;; Not so useful, but eventually...
(use-package helm-company
  :ensure t
  :config (eval-after-load 'company
  '(progn
     ;;(define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company))))

(provide 'programming-settings)
;;; programming-settings.el ends here
