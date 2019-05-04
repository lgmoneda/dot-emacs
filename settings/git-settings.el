;;; git-settings.el --- Settings for git utilities

;; Magit
(use-package magit
  :ensure t
  :init
  ;; Auto revert buffers when change branches
  ;; Auto revert does not work with tramp,
  ;; check the workaround later
  ;; https://github.com/magit/magit/issues/1205
  ;; (global-auto-revert-mode 1)
  ;; (setq auto-revert-check-vc-info t)

  ;; (setq auto-revert-check-vc-info t)

    ;; (defun my-disable-auto-revert-vc-in-tramp ()
    ;;   (when (and buffer-file-name (file-remote-p buffer-file-name))
    ;;     (setq-local auto-revert-check-vc-info nil)))

    ;; (add-hook 'find-file-hook #'my-disable-auto-revert-vc-in-tramp)

  )

;; Git-timemachine
(use-package git-timemachine
	     :ensure t)

;; Show differences between local and repo
(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'left)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)

  ;; defining the custom colors to the diff-hl

  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-delete ((t (:background "#ee6363"))))))

(provide 'git-settings)
;;; git-settings.el ends here
