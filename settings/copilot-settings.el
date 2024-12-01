;;; copilot-settings.el --- Functions for AI assitance for coding
;; https://github.com/copilot-emacs/copilot.el?tab=readme-ov-file
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  )

(add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; https://github.com/chep/copilot-chat.el
(use-package copilot-chat
  :quelpa (copilot-chat :fetcher github
                   :repo "chep/copilot-chat.el"
                   :files ("*.el"))
  :after (request))

;;; copilot-settings.el ends here
