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

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  ;; :load-path "/Users/luis.moneda/repos/agent-shell"
  :commands (agent-shell)
  :hook (agent-shell-mode . corfu-mode)
  :init
  (setq agent-shell-file-completion-enabled t
        agent-shell-anthropic-default-model-id "sonnet[1m]"
	agent-shell-anthropic-default-model-name "Sonnet"
	agent-shell-anthropic-default-session-mode-id "bypassPermissions"
        agent-shell-show-welcome-message nil)
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (getenv "ANTHROPIC_API_KEY")))
  (setq agent-shell-openai-authentication
	(agent-shell-openai-make-authentication :login t))
  ;; Only display codex and claude, the only agents I've been using
  (setq agent-shell-agent-configs
      (list (agent-shell-anthropic-make-claude-code-config)
            (agent-shell-openai-make-codex-config)))

  ;; Environment variables for Codex/OpenAI subprocesses
  (setq agent-shell-openai-codex-environment
        (agent-shell-make-environment-variables :inherit-env t))
  )

;; Aesthetic changes
(with-eval-after-load 'agent-shell
  ;; Use the text header so regular faces can style it.
  (setq agent-shell-header-style 'text)

  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :box nil
                      :weight 'medium
                      :height 1.20
                      :extend t)

  (when (facep 'agent-shell-buffer-name)
    (set-face-attribute 'agent-shell-buffer-name nil
                        :foreground "#5B3F8C"
                        :weight 'bold
                        :height 1.20))

  (when (facep 'agent-shell-model)
    (set-face-attribute 'agent-shell-model nil
                        :foreground "#245C73"
                        :height 1.20))

  (when (facep 'agent-shell-session-directory)
    (set-face-attribute 'agent-shell-session-directory nil
                        :foreground "#557A3E"
                        :height 1.20)))

;; Color user input
(with-eval-after-load 'agent-shell
  (dolist (face '(agent-shell-input comint-highlight-input))
    (when (facep face)
      (set-face-attribute face nil
                          :background "#DDECCB"
                          :foreground 'unspecified
                          :extend t))))

(defvar-local my/agent-shell-input-block-overlays nil
  "Overlays used to render agent-shell user messages as blocks.")

(defun my/agent-shell--clear-input-block-overlays (start end)
  "Remove user-message block overlays between START and END."
  (setq my/agent-shell-input-block-overlays
        (seq-remove
         (lambda (overlay)
           (if (and (overlay-buffer overlay)
                    (< (overlay-start overlay) end)
                    (> (overlay-end overlay) start))
               (progn
                 (delete-overlay overlay)
                 t)
             nil))
         my/agent-shell-input-block-overlays)))

(defun my/agent-shell--block-range-bound (range key)
  "Return KEY from agent-shell UI RANGE."
  (alist-get key range))

(defun my/agent-shell--make-input-block-overlay (start end)
  "Create an input block overlay between START and END."
  (let ((overlay (make-overlay start end nil t nil)))
    (overlay-put overlay 'face '(:background "#DDECCB" :extend t))
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'my/agent-shell-input-block t)
    (push overlay my/agent-shell-input-block-overlays)))

(defun my/agent-shell--render-input-block (result)
  "Render user-message RESULT from `agent-shell-ui-update-text' as a block."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              (padding (alist-get :padding result))
              (block (alist-get :block result))
              (start (my/agent-shell--block-range-bound padding :start))
              (block-start (my/agent-shell--block-range-bound block :start))
              (end (my/agent-shell--block-range-bound block :end))
              (state (get-text-property block-start 'agent-shell-ui-state))
              (qualified-id (map-elt state :qualified-id))
              ((string-suffix-p "-user_message_chunk" qualified-id)))
    (when (eq (char-after start) ?\n)
      (setq start (1+ start)))
    (my/agent-shell--clear-input-block-overlays start end)
    (my/agent-shell--make-input-block-overlay start end))
  result)

(defun my/agent-shell--render-live-input-block (orig-fun &rest args)
  "Render a submitted live prompt as a full-width input block."
  (if (not (derived-mode-p 'agent-shell-mode))
      (apply orig-fun args)
    (let ((start (save-excursion
                   (goto-char (shell-maker--prompt-begin-position))
                   (line-beginning-position))))
      (prog1 (apply orig-fun args)
        (when-let* ((end (and (boundp 'comint-last-input-end)
                              (markerp comint-last-input-end)
                              (marker-position comint-last-input-end)))
                    (end (save-excursion
                           (goto-char end)
                           (line-end-position)))
                    ((< start end)))
          (my/agent-shell--clear-input-block-overlays start end)
          (my/agent-shell--make-input-block-overlay start end))))))

(with-eval-after-load 'agent-shell-ui
  (unless (advice-member-p #'my/agent-shell--render-input-block
                           'agent-shell-ui-update-text)
    (advice-add 'agent-shell-ui-update-text
                :filter-return #'my/agent-shell--render-input-block)))

(with-eval-after-load 'shell-maker
  (unless (advice-member-p #'my/agent-shell--render-live-input-block
                           'shell-maker-submit)
    (advice-add 'shell-maker-submit
                :around #'my/agent-shell--render-live-input-block)))

;; to know mode ids, use inside an agent shell.
;; (mapcar (lambda (mode)
;;           (cons (map-elt mode :name)
;;                 (map-elt mode :id)))
;;         (agent-shell--get-available-modes (agent-shell--state)))

;; When I want to use a local version
;; (setq agent-shell-openai-codex-acp-command
;;       '("/Users/luis.moneda/repos/codex-acp/target/debug/codex-acp"))
(setq agent-shell-openai-default-model-id nil)
(setq agent-shell-openai-default-session-mode-id "agent-full-access")

(defvar my/agent-shell-codex-profile 'personal
  "Current Codex profile for agent-shell. Either 'personal or 'work.")

(defun my/agent-shell--set-codex-command ()
  "Update `agent-shell-openai-codex-command` based on current profile."
  (setq agent-shell-openai-codex-acp-command
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

(define-key my/agent-shell-map (kbd "w")
	    #'agent-shell-manager-set-annotation)

(define-key my/agent-shell-map (kbd "d")
	    #'agent-shell-send-dwim)

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

;; agent-shell-pet
(add-to-list 'load-path "/Users/luis.moneda/repos/agent-shell-pet")

(require 'agent-shell-pet)

(setq agent-shell-pet-renderer 'macos-native
      agent-shell-pet-speech-bubble-theme 'light
      agent-shell-pet-size 'small)

(setq agent-shell-pet-id "marcel-proust")

(setq agent-shell-pet-completion-sound-enabled t)

(add-hook 'agent-shell-pet-global-mode-enable-hook
          #'agent-shell-manager-toggle-ready-status-notifications)

(add-to-list 'load-path "/Users/luis.moneda/repos/agent-shell-manager")
(with-eval-after-load 'agent-shell
  (require 'agent-shell-manager))

(setq agent-shell-manager-ready-status-notification-sound t)
(setq agent-shell-manager-show-annotation-in-header t)
(setq agent-shell-manager-rename-buffers-with-annotation nil)
(setq agent-shell-context-sources nil)

(setq agent-shell-manager-visible-columns
      '(buffer status annotation model))

;; Management for agent shell
;; (use-package agent-shell-manager
;;   :vc (:url "https://github.com/jethrokuan/agent-shell-manager")
;;   :commands (agent-shell-manager-toggle)
;;   )

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
  (require 'agent-shell-manager)
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
            (with-current-buffer buf
              (if agent-shell-manager--column-widths
                  (agent-shell-manager-switch-to-side-window)
                (agent-shell-manager-switch-to-default-window)))))))))

(global-set-key (kbd "C-c m") #'my/agent-shell-manager-smart-toggle)


(with-eval-after-load 'agent-shell
  (when (boundp 'agent-shell-mode-map)
    (define-key agent-shell-mode-map (kbd "C-<tab>") #'my/window-next)))

;; project.el
(defun my/agent-shell--project-root-for (path)
  "Return the project root for PATH, falling back to PATH directory."
  (let* ((expanded-path (expand-file-name path))
         (target-directory
          (file-name-as-directory
           (if (file-directory-p expanded-path)
               expanded-path
             (file-name-directory expanded-path))))
         (default-directory target-directory))
    (if (fboundp 'agent-shell-cwd)
        (file-name-as-directory (expand-file-name (agent-shell-cwd)))
      target-directory)))

(defun my/agent-shell-project-root (path)
  "Open or switch to agent-shell for the project containing PATH."
  (interactive (list default-directory))
  (require 'agent-shell)
  (let* ((project-root (my/agent-shell--project-root-for path))
         (existing-buffer
          (cl-find-if
           (lambda (buf)
             (with-current-buffer buf
               (and (derived-mode-p 'agent-shell-mode)
                    (equal (file-name-as-directory
                            (expand-file-name default-directory))
                           project-root))))
           (buffer-list))))
    (if existing-buffer
        (pop-to-buffer existing-buffer)
      (let ((default-directory project-root))
        (if (fboundp 'agent-shell-new-shell)
            (call-interactively #'agent-shell-new-shell)
          (call-interactively #'agent-shell))))))

;; embark
(defun my/project-agent-shell (path)
  "Run or switch to an agent-shell for the project containing PATH."
  (interactive "D")
  (my/agent-shell-project-root path))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "a") #'my/project-agent-shell)
  )

(use-package agent-shell-dashboard
  :straight (:host github :repo "wandersoncferreira/agent-shell-dashboard")
  :after agent-shell
  :commands (agent-shell-dashboard))

(define-key my/agent-shell-map (kbd "b")
	    #'agent-shell-dashboard)

(use-package agent-shell-model-router
  :straight (:host github :repo "wandersoncferreira/agent-shell-model-router")
  :after agent-shell)

(require 'agent-shell-model-router)

;; Layer A: explicit rules. :model matches substrings of model display names.
(setq agent-shell-model-router-rules
      '((:name "research"  :model "Opus"
         :keywords ("research" "investigate" "analyze"))
        (:name "reply"     :model "Sonnet"
         :keywords ("reply" "send a message"))
        (:name "code"      :model "Opus"
         :keywords ("implement" "refactor"))
        (:name "trivial"   :model "Haiku"
         :keywords ("typo" "rename" "format"))))

;; Layer B: fallback by complexity.
(setq agent-shell-model-router-complexity-models
      '((high   . "Opus") (medium . "Sonnet") (low . "Haiku")))

(agent-shell-model-router-mode 1)

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

;;; ─── MCP auth helper ─────────────────────────────────────────────────────

(defconst lgm--mcp-needs-auth-cache
  (expand-file-name "~/.claude/mcp-needs-auth-cache.json")
  "Path to Claude Code's MCP needs-auth cache file.")

(defun lgm/auth-mcp-claude ()
  "Show MCP servers that need re-authentication.
Displays a buffer listing servers needing auth, with a button to open
a temporary terminal so you can run /mcp and complete the OAuth flow."
  (interactive)
  (let* ((json-object-type 'alist)
         (cache (if (file-exists-p lgm--mcp-needs-auth-cache)
                    (json-read-file lgm--mcp-needs-auth-cache)
                  nil))
         ;; 30-minute expiry window (matches Claude Code's internal constant)
         (expiry-ms (* 30 60 1000))
         (now-ms (* 1000 (float-time)))
         (stale (seq-filter
                 (lambda (entry)
                   (let ((ts (alist-get 'timestamp (cdr entry))))
                     (when ts (< (- now-ms ts) expiry-ms))))
                 cache))
         (buf (get-buffer-create "*MCP Auth Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# MCP Server Authentication Status\n\n")
        (if (null stale)
            (insert "All MCP servers appear authenticated (or none registered).\n")
          (insert (format "%d server(s) need re-authentication:\n\n" (length stale)))
          (dolist (entry stale)
            (insert (format "  • %s\n" (car entry))))
          (insert "\n")
          (insert "To re-authenticate, open a Claude Code terminal and run /mcp\n")
          (insert "then click the auth link for each server listed above.\n\n")
          (insert-button "Open claude-nu → /mcp (auth all at once)"
                         'action (lambda (_)
                                   (lgm--open-mcp-auth-terminal))
                         'follow-link t
                         'face 'link))
        (insert "\n\n")
        (insert "To clear the auth cache and force re-auth on next session start:\n")
        (insert-button "Clear auth cache"
                       'action (lambda (_)
                                 (when (yes-or-no-p "Clear MCP auth cache? ")
                                   (delete-file lgm--mcp-needs-auth-cache)
                                   (message "MCP auth cache cleared.")))
                       'follow-link t
                       'face 'link)
        (special-mode)))
    (display-buffer buf)))

(defun lgm--open-mcp-auth-terminal ()
  "Open a terminal buffer running claude-nu with /mcp pre-typed.
Waits for claude to start, then automatically sends /mcp so all
auth links appear immediately — click them in sequence in the browser."
  (let* ((buf-name "*claude-mcp-auth*")
         (existing (get-buffer buf-name)))
    (when existing (kill-buffer existing))
    (cond
     ((fboundp 'vterm)
      (vterm buf-name)
      (vterm-send-string "claude-nu\n")
      (run-with-timer 4 nil
                      (lambda (name)
                        (when-let ((buf (get-buffer name)))
                          (with-current-buffer buf
                            (vterm-send-string "/mcp\n"))))
                      buf-name))
     ((fboundp 'eat)
      (eat "claude-nu" t)
      (rename-buffer buf-name t)
      (run-with-timer 4 nil
                      (lambda (name)
                        (when-let ((buf (get-buffer name)))
                          (with-current-buffer buf
                            (eat-term-send-string eat-terminal "/mcp\n"))))
                      buf-name))
     (t
      (term "/bin/zsh")
      (rename-buffer buf-name t)
      (term-send-string (get-buffer-process (current-buffer)) "claude-nu\n")))
    (message "Opening claude-nu and running /mcp — click each auth link in the browser.")))

(provide 'copilot-settings)
;;; copilot-settings.el ends here
