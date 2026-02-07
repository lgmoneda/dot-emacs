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
  :commands (agent-shell)
  :hook (agent-shell-mode . corfu-mode)
  :init
  (setq agent-shell-file-completion-enabled t
        agent-shell-show-welcome-message nil)
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (getenv "ANTHROPIC_API_KEY")))
  (setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))

  ;; Environment variables for Codex/OpenAI subprocesses
  (setq agent-shell-openai-codex-environment
        (agent-shell-make-environment-variables :inherit-env t))
  )

(defvar my/agent-shell-codex-profile 'personal
  "Current Codex profile for agent-shell. Either 'personal or 'work.")

(defun my/agent-shell--set-codex-command ()
  "Update `agent-shell-openai-codex-command` based on current profile."
  (setq agent-shell-openai-codex-command
        (list "codex-acp"
              "--profile"
              (symbol-name my/agent-shell-codex-profile))))

(defun lgm/agent-shell-toggle-codex-profile ()
  "Toggle Codex profile between personal and work for agent-shell."
  (interactive)
  (setq my/agent-shell-codex-profile
        (if (eq my/agent-shell-codex-profile 'personal)
            'work
          'personal))
  (my/agent-shell--set-codex-command)
  (message "Agent-shell Codex profile set to: %s"
           my/agent-shell-codex-profile))


;; Ensure claude-code-acp runs with env from ~/.nurc
;; This will make using work settings in all claude code sessions via agent-shell
(with-eval-after-load 'agent-shell
  (setopt agent-shell-anthropic-claude-command
          '("bash" "-lc" "source ~/.nurc >/dev/null 2>&1; exec claude-code-acp")))

;; Graphic interacts poorly
;; (setq agent-shell-header-style 'text)

;; With string
;; (setq agent-shell-openai-authentication
;;       (agent-shell-openai-make-authentication :api-key (getenv "OPENAI_API_KEY")))

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
(use-package agent-shell-manager
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager")
  :commands (agent-shell-manager-toggle)
  )

(with-eval-after-load 'agent-shell-manager
  ;; Let *your* display-buffer-alist decide placement/size.
  (setq agent-shell-manager-side nil)

  ;; Show the manager in a bottom side window, 18% of frame height.
  (add-to-list
   'display-buffer-alist
   '("\\*Agent-Shell Buffers\\*"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . 0.08)
     (preserve-size . (nil . t)))))

(defun my/agent-shell-project-root (dir)
  "Run agent-shell in DIR (project root directory)."
  (interactive "D")
  (let ((default-directory dir))
    (call-interactively #'agent-shell)))

(defun my/agent-shell-project-root (dir)
  "Open or switch to agent-shell for project at DIR."
  (interactive (list default-directory))
  (let* ((default-directory dir)
         ;; Look for existing agent-shell buffers in this project
         (existing-buffer 
          (cl-find-if
           (lambda (buf)
             (with-current-buffer buf
               (and (derived-mode-p 'agent-shell-mode)
                    (equal default-directory dir))))
           (buffer-list))))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (let ((default-directory dir))
        (call-interactively #'agent-shell)))))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "a") #'my/agent-shell-project-root))

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
