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

;; (org-babel-mcp-start-server)
;; (org-roam-mcp-start-server)
;; (mcp-server-lib-start)
;; (add-to-list 'load-path "/Users/luis.moneda/repos/agent-shell")
;; (require 'agent-shell)

;; (use-package agent-shell
;;   :load-path "/Users/luis.moneda/repos/agent-shell"
;;   :commands (agent-shell)
;;   :hook (agent-shell-mode . corfu-mode)
;;   :init
;;   (setq agent-shell-file-completion-enabled t
;;         agent-shell-show-welcome-message nil)
;;   :config
;;   (setq agent-shell-anthropic-authentication
;;         (agent-shell-anthropic-make-authentication
;;          :api-key (getenv "ANTHROPIC_API_KEY")))
;;   (setq agent-shell-openai-authentication
;;         (agent-shell-openai-make-authentication :login t))
;;   (setq agent-shell-openai-codex-environment
;;         (agent-shell-make-environment-variables :inherit-env t)))
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

;; Custom code to enable inline images in the agent-shell
;; (with-eval-after-load 'agent-shell
;;   (load-file "~/.emacs.d/settings/agent-shell-inline-images.el")
;;   (setq agent-shell-inline-image-fetch-remote t))


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

(with-eval-after-load 'agent-shell
  ;; Make sure the agent sees your normal Emacs environment (PATH, HOME, etc.)
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         :inherit-env t
         :load-env (expand-file-name "~/.nurc.env")))
  )

;; Create prefix keymap
(define-prefix-command 'my/agent-shell-map)

;; Bind prefix to C-c a
(global-set-key (kbd "C-c a") #'my/agent-shell-map)

;; Sub-bindings
(define-key my/agent-shell-map (kbd "f")
  #'agent-shell-send-current-file)

(define-key my/agent-shell-map (kbd "l")
	    #'agent-shell-send-dwim)

(define-key my/agent-shell-map (kbd "s")
			#'agent-shell-send-screenshot)

(define-key my/agent-shell-map (kbd "i")
	    #'agent-shell-send-clipboard-image)

(define-key my/agent-shell-map (kbd "r")
	    #'agent-shell-send-region)

(define-key my/agent-shell-map (kbd "o")
	    #'agent-shell-send-other-file)

(define-key my/agent-shell-map (kbd "c")
	    #'agent-shell-prompt-compose)

(define-key my/agent-shell-map (kbd "a")
	    #'agent-shell)

(defun my/agent-shell-setup-org-roam-links ()
  (require 'org) ;; for org-link face
  (font-lock-add-keywords
   nil
   '(;; Any org link: [[target][Description]] or [[target]]
     ;; Group 1: opening brackets + target + optional ][desc  (everything to hide before desc)
     ;; Group 2: description (or target when no desc)
     ;; Group 3: closing ]]
     ("\\(\\[\\[[^]]+\\]\\[\\)\\([^]]+\\)\\(\\]\\]\\)"
      (1 (prog1 nil
           (compose-region (match-beginning 1) (match-end 1) "")))
      (3 (prog1 nil
           (compose-region (match-beginning 3) (match-end 3) "")))
      (2 'org-link prepend)))
   'prepend)  ;; prepend so we run before other font-lock rules
  ;; make sure it applies right away
  (font-lock-flush))

(add-hook 'agent-shell-mode-hook #'my/agent-shell-setup-org-roam-links)

(defun my/org-roam-link-at-point ()
  "Return the org-roam ID if point is inside an [[id:UUID][Description]] link, else nil."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward
                     "\\[\\[id:\\([A-Za-z0-9-]+\\)\\]\\[[^]]+\\]\\]"
                     (line-end-position) t))
          (when (and (>= pos (match-beginning 0))
                     (<= pos (match-end 0)))
            (setq found (match-string 1))))
        found))))

(defun my/org-link-at-point ()
  "Return the target of any org link at point, else nil.
Handles [[id:UUID][desc]], [[target][desc]], and [[target]] forms."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (let ((found nil))
        (while (and (not found)
                    (re-search-forward
                     "\\[\\[\\([^]]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]"
                     (line-end-position) t))
          (when (and (>= pos (match-beginning 0))
                     (<= pos (match-end 0)))
            (setq found (match-string 1))))
        found))))

(defun my/agent-shell-setup-org-cite-face ()
  (require 'org)
  (font-lock-add-keywords
   nil
   `((,(rx "[cite:" (+ (not (any "]"))) "]")
      (0 ,(if (facep 'org-cite) 'org-cite 'org-link) t)))
   'append)
  (font-lock-flush))

(add-hook 'agent-shell-mode-hook #'my/agent-shell-setup-org-cite-face)

(with-eval-after-load 'agent-shell
  (define-key agent-shell-mode-map (kbd "C-c r") #'org-cite-insert))

(defun my/agent-shell-org-open-at-point ()
  "Org-like C-c C-o in agent-shell: delegate to `org-open-at-point`."
  (interactive)
  (require 'org)
  (require 'oc nil t)

  (let* ((offset (- (point) (line-beginning-position)))
         (line   (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))
    (with-temp-buffer
      (org-mode)

      ;; Insert a minimal org context plus the exact line text.
      (insert "* tmp\n\n" line "\n")

      ;; Move point to the same *character offset* within the copied line.
      (goto-char (point-min))
      (forward-line 2)
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (goto-char (min (+ bol offset) eol)))

      (condition-case nil
          (org-open-at-point)
        (error
         (when (fboundp 'agent-shell-other-buffer)
           (call-interactively #'agent-shell-other-buffer)))))))

(with-eval-after-load 'agent-shell
  (define-key agent-shell-mode-map (kbd "C-c C-o") #'my/agent-shell-org-open-at-point))

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

(defun my/agent-shell-manager-smart-toggle ()
  "If already in the Agent-Shell manager, toggle it.
Otherwise, jump to *Agent-Shell Buffers* (creating it if needed)."
  (interactive)
  (if (derived-mode-p 'agent-shell-manager-mode)
      (agent-shell-manager-toggle)
    (let ((buf (get-buffer "*Agent-Shell Buffers*")))
      (unless (buffer-live-p buf)
        (agent-shell-manager-toggle)
        (setq buf (get-buffer "*Agent-Shell Buffers*")))
      (when (buffer-live-p buf)
        ;; If it's already visible somewhere, just focus that window.
        (let ((win (get-buffer-window buf t)))
          (if (window-live-p win)
              (select-window win)
            (pop-to-buffer buf)))))))

(global-set-key (kbd "C-c m") #'my/agent-shell-manager-smart-toggle)


(with-eval-after-load 'agent-shell
  (when (boundp 'agent-shell-mode-map)
    (define-key agent-shell-mode-map (kbd "C-<tab>") #'my/window-next)))

;; project.el
(defun my/agent-shell-project-root (dir)
  "Open or switch to agent-shell for project at DIR in another window."
  (interactive (list default-directory))
  (let* ((default-directory dir)
         (existing-buffer
          (cl-find-if
           (lambda (buf)
             (with-current-buffer buf
               (and (derived-mode-p 'agent-shell-mode)
                    (equal default-directory dir))))
           (buffer-list))))
    (if existing-buffer
        (pop-to-buffer existing-buffer)   ;; <- new window
      (let ((default-directory dir))
        (call-interactively #'agent-shell)))))

;; embark
(defun my/project-agent-shell (dir)
  "Run agent-shell in DIR (project root directory)."
  (interactive "D")
  (let ((default-directory dir))
    (call-interactively #'agent-shell)))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "a") #'my/project-agent-shell)
  )


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
