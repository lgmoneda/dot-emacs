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
    ;; (setq company-global-modes '(not python-mode cython-mode sage-mode ein:notebook-modes markdown-mode processing-mode))
          (global-company-mode)
          )
  :config (progn
            (setq company-tooltip-limit 12
                  company-idle-delay 0
                  company-echo-delay 0.5
                  company-begin-commands '(self-insert-command  self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash )
                  company-transformers '(company-sort-by-occurrence)
                  company-selection-wrap-around t
                  company-minimum-prefix-length 3
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

;; Test http rest webservices inside emacs
;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t)

;; Source: https://github.com/MatthewZMD/.emacs.d/blob/master/README.md#company-tabnine
;; (use-package company-tabnine
;;   :defer 1
;;   :custom
;;   (company-tabnine-max-num-results 9)
;;   :bind
;;   (("M-q" . company-other-backend)
;;    ("C-c z" . company-tabnine))
;;   :hook
;;   (lsp-after-open . (lambda ()
;;                       (setq company-tabnine-max-num-results 3)
;;                       (add-to-list 'company-transformers 'company//sort-by-tabnine t)
;;                       (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))))
;;   (kill-emacs . company-tabnine-kill-process)
;;   :config
;;   ;; Enable TabNine on default
;;   (add-to-list 'company-backends #'company-tabnine)

;;   ;; Integrate company-tabnine with lsp-mode
;;   (defun company//sort-by-tabnine (candidates)
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-lsp
;;             candidates-tabnine)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-tabnine))
;;             (push candidate candidates-lsp)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-lsp (nreverse candidates-lsp))
;;         (setq candidates-tabnine (nreverse candidates-tabnine))
;;         (nconc (seq-take candidates-tabnine 3)
;;                (seq-take candidates-lsp 6))))))

(provide 'programming-settings)
;;; programming-settings.el ends here
