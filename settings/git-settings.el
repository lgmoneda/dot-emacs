;;; git-settings.el --- Settings for git utilities

(use-package with-editor
  :ensure t)

;; Auto revert buffers when change branches
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; Magit
(use-package magit
  :commands (magit-status magit-file-dispatch)
  :bind (("C-x g" . magit-status)))

;; Show differences between local and repo
(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'left)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  ;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)

  ;; defining the custom colors to the diff-hl

  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-delete ((t (:background "#ee6363")))))
  
  ;; Disable diff-hl in org-mode
  (defun disable-diff-hl-in-org-mode ()
    (diff-hl-mode -1))
  (add-hook 'org-mode-hook 'disable-diff-hl-in-org-mode)

  )

(provide 'git-settings)
;;; git-settings.el ends here
