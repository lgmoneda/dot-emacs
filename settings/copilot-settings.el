;;; copilot-settings.el --- Functions for AI assitance for coding
;; https://github.com/copilot-emacs/copilot.el?tab=readme-ov-file
;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "copilot-emacs/copilot.el"
;;                    :branch "main"
;;                    :files ("*.el"))
;;   )

;; (add-hook 'prog-mode-hook 'copilot-mode)

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; ;; https://github.com/chep/copilot-chat.el
;; (use-package copilot-chat
;;   :quelpa (copilot-chat :fetcher github
;;                    :repo "chep/copilot-chat.el"
;;                    :files ("*.el"))
;;   :after (request))

;; (use-package aidermacs
;;   :ensure t
;;   :bind (("C-x a" . aidermacs-transient-menu))
;;   :config
;;   ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
;;   (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY"))
;;   (setenv "OPENROUTER_API_KEY" (getenv "OPENROUTER_API_KEY"))
;;   (setq aider-model "anthropic/claude-sonnet-4-20250514")
;;   :custom
;;   ; See the Configuration section below
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "openrouter/anthropic/claude-sonnet-4")
;;   )

;; I didn't try this one
;; (use-package emigo
;;   :straight (:host github :repo "MatthewZMD/emigo" :files (:defaults "*.py" "*.el"))
;;   :config
;;   (emigo-enable) ;; Starts the background process automatically
;;   :custom
;;   ;; Encourage using OpenRouter with Deepseek
;;   (emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
;;   (emigo-base-url "https://openrouter.ai/api/v1")
;;   (emigo-api-key (getenv "OPENROUTER_API_KEY")))

;; (org-babel-mcp-start-server)
;; (org-roam-mcp-start-server)
;; (mcp-server-lib-start)

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el"))
(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell"))
(setq agent-shell-openai-authentication
      (agent-shell-anthropic-make-authentication :login t))

;; With string
(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :api-key (getenv "OPENAI_API_KEY")))

(provide 'copilot-settings)
;;; copilot-settings.el ends here
