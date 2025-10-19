;;; claude-code-ide-settings.el --- Functions for the Claude Code IDE

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el" :branch "main")
  :bind ("C-x a" . claude-code-ide-menu)
  )

;; Load tools
(claude-code-ide-emacs-tools-setup)

;; for eat terminal backend:
(use-package eat
  :ensure t)
(setq eat-term-name "xterm-256color")

;; for vterm terminal backend:
(use-package vterm :ensure t)

(setq claude-code-ide-window-width 100)

(provide 'claude-code-ide-settings)
;;; claude-code-ide-settings.el ends here
