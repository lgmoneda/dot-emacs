;;; git-settings.el --- Settings for git utilities

;; Magit
(use-package magit
  :quelpa ((magit :fetcher git :url "https://github.com/magit/magit") :upgrade t)
  ;; :ensure t
  :init
  ;; Auto revert buffers when change branches
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info t)
  )

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
   '(diff-hl-delete ((t (:background "#ee6363"))))))

(provide 'git-settings)
;;; git-settings.el ends here
