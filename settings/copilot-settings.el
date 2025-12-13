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

(use-package agent-shell
  :ensure t)

(setq agent-shell-file-completion-enabled t)
(setq agent-shell-show-welcome-message nil)

;; Graphic interacts poorly
;; (setq agent-shell-header-style 'text)

(add-hook 'agent-shell-mode-hook #'corfu-mode)

(setq agent-shell-openai-authentication
      (agent-shell-anthropic-make-authentication :login t))

(setq agent-shell-openai-codex-environment
      (agent-shell-make-environment-variables :inherit-env t))

;; With string
;; (setq agent-shell-openai-authentication
;;       (agent-shell-openai-make-authentication :api-key (getenv "OPENAI_API_KEY")))
(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :api-key (getenv "ANTHROPIC_API_KEY")))

;; (with-eval-after-load 'agent-shell
;;   ;; Codex (default profile from ~/.codex/config.toml)
;;   (add-to-list 'agent-shell-agent-configs
;;                (list
;;                 (cons :id "codex-default")
;;                 (cons :display-name "Codex (default)")
;;                 (cons :program "codex-acp")))
;;   ;; Codex (work profile)
;;   (add-to-list 'agent-shell-agent-configs
;;                (list
;;                 (cons :id "codex-work")
;;                 (cons :display-name "Codex (work)")
;;                 (cons :program "codex-acp")
;;                 ;; Some ACP adapters need a `--` before forward args; if your build
;;                 ;; doesn’t, try '("--profile" "work") instead.
;;                 (cons :args '("--" "--profile" "work")))))

;; Management for agent shell
(add-to-list 'load-path "~/repos/agent-shell-manager/")
;; (require 'agent-shell-manager)
(require 'agent-shell-manager)

(use-package agent-shell-manager
  :config
  (setq agent-shell-manager-display-buffer-alist
        '((display-buffer-in-side-window)
          (side . bottom)
          (window-height . 8)  ; 25% of frame height
          (slot . 0)
          (dedicated . t))))

;; (use-package agent-shell-sidebar
;;   :after agent-shell
;;   ;; :vc (:url "https://github.com/cmacrae/agent-shell-sidebar")
;;   :custom
;;   (agent-shell-sidebar-width "25%")
;;   (agent-shell-sidebar-minimum-width 80)
;;   (agent-shell-sidebar-maximum-width "50%")
;;   (agent-shell-sidebar-position 'right)
;;   (agent-shell-sidebar-locked t)
;;   (agent-shell-sidebar-default-config
;;    (agent-shell-anthropic-make-claude-code-config))
;;   ;; :bind
;;   ;; (("C-c a s" . agent-shell-sidebar-toggle)
;;   ;;  ("C-c a f" . agent-shell-sidebar-toggle-focus))
;;   )

(provide 'copilot-settings)
;;; copilot-settings.el ends here
