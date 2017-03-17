;; Jedi
 ;; (use-package jedi
 ;;    :ensure t
 ;;    :config
 ;;    (setq jedi:server-command '("~/.emacs.d/elpa/jedi-core-20170121.610/jediepcserver.py"))
 ;;    (setq jedi:complete-on-dot t)
 ;;    ;;(setq jedi:tooltip-method '(eldoc-style))
 ;;    ;;(add-hook 'python-mode-hook 'jedi:setup)

 ;;    (add-hook 'python-mode-hook 'jedi:ac-setup)
 ;;    )


;; Run python first time 
;; (defun run-python-once ()
;;   (remove-hook 'python-mode-hook 'run-python-once)
;;   (run-python (python-shell-parse-command)))

;; (add-hook 'python-mode-hook 'run-python-once)

;; ;; Highlight current line
;; (global-hl-line-mode 1)
;; ;;(set-face-background 'hl-line "#3e4446")
;; (set-face-background 'hl-line "#000000")


;; (defun fancy-tab (arg)
;;   (interactive "P")
;;   (setq this-command last-command)
;;   (if (or (eq this-command 'hippie-expand) (looking-at "\\_>"))
;;       (progn
;; 	(setq this-command 'hippie-expand)
;; 	(hippie-expand arg))
;;     (setq this-command 'indent-for-tab-command)
;;     (indent-for-tab-command arg)))


;; Trying to reproduce arrow keys
;; (define-key key-translation-map (kbd "C-l") (kbd "\C-b"))
;; (define-key key-translation-map (kbd "M-l") (kbd "M-b"))
;; (define-key key-translation-map (kbd "<C-dead-tilde>") (kbd "\C-f"))
;; (define-key key-translation-map (kbd "<C-dead-tilde>") (kbd "M-f"))
;; (define-key key-translation-map (kbd "C-ç") (kbd "\C-n")


;; Beep when mention me
;; (add-hook 'erc-text-matched-hook 'erc-beep-on-match)
;; (setq erc-beep-match-types '(current-nick keyword))

;; Sound for private msg
;; (defun erc-my-privmsg-sound (proc parsed)
;;   (let* ((tgt (car (erc-response.command-args parsed)))
;; 	 (privp (erc-current-nick-p tgt)))
;;     (and
;;      privp
;;      (sound)
;;      nil))) ;We must return nil. See help for `erc-server-PRIVMSG-functions'

;; (add-hook 'erc-server-PRIVMSG-functions
;; 	  'erc-my-privmsg-sound)

;; (setq sound-default "~/.emacs.d/sounds/beep.wav")

;; (defun sound (&optional path)
;;   (start-process-shell-command
;;    "sound"
;;    nil
;;    (concat "mplayer -fcd " (or path sound-default))))


;; ;; Auto Complete
;; (use-package auto-complete
;;   :ensure t)

;; trocar por auto-complete later.
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (setq auto-complete-mode t)
;;   ;;(ac-config-default)
;;   (setq ac-use-fuzzy t)
;;   (setq ac-fuzzy-enable t)
;;   (setq ac-use-menu-map t)
;;   ;; ;; start completion but wait me to type 4 characters
;;   (setq ac-auto-start 2)
;;   (setq ac-delay 1)
  
;;   ;; ;; dont start the completion menu automatically
;;   (setq ac-auto-show-menu 1)

;;   ;; ;; do what I mean.
;;   ;; ;; a. TAB behave as completion (ac-complete) when only one candidate is left
;;   ;; ;; b. TAB behaves as completion (ac-complete) after you select candidate
;;   ;; ;; c. Disapears automatically when you complete a candidate
;;   (setq ac-dwim t)
  
;;   :config
;;   (use-package fuzzy
;;     :ensure t)
  
;;   (use-package pos-tip
;;     ;; show help beautifully
;;     ;; auto-complete-mode uses its native rendering for displaying quickhelp
;;     :ensure t
;;     :config
;;     ;;(ac-config-default)
;;     (setq ac-quick-help-delay 4))

;;   ;; start the completion manually
;;   (define-key ac-mode-map (kbd "C-<return>") 'auto-complete)

;;   ;; navigate inside the completion popup using C-n/C-p keys
;;   (define-key ac-menu-map "\C-n" 'ac-next)
;;   (define-key ac-menu-map "\C-p" 'ac-previous)

;;   ;; the isearch over the candidates in the popup menu is just an amazing feature.
;;   (define-key ac-menu-map "\C-s" 'ac-isearch)
;;   (define-key ac-mode-map (kbd "C-x /") 'ac-complete-filename)

;;   ;; finish completion by tab
;;   (define-key ac-completing-map "\t" 'ac-complete)
;;   (define-key ac-completing-map "\r" nil))


;; Bk's python
;; (use-package python
;;   :mode ("\\.py" . python-mode)
;;   :config
;;   (setq python-shell-interpreter-args "")
;;   (eval-after-load "python"
;;     '(progn
;;        (define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)))
;;   (use-package anaconda-mode
;;     :ensure t
;;     :diminish anaconda-mode
;;     :config)
;;     ;;(bk-python-hooks '(anaconda-mode python--add-debug-highlight)))
;;   (use-package jedi
;;     :ensure t
;;     :config
;;     (setq jedi:server-command '("~/.emacs.d/elpa/jedi-core-20170121.610/jediepcserver.py"))
;;     (setq jedi:complete-on-dot t)
;;     ;;I'm happy with Anaconda-Eldoc
;;     (setq jedi:tooltip-method '(pos-tip popup))
;;     (add-hook 'python-mode-hook 'jedi:setup))
;;     (define-key jedi-mode-map (kbd "<C-tab>") nil)
;;     )

;; (add-hook 'python-mode-hook
;; 	  '(lambda ()
;; 	     (local-set-key (kbd "M-?") 'jedi:show-doc)
;; 	     (local-set-key (kbd "M-.") 'jedi:goto-definition)
;; 	     (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)))


;; auto complete mode
;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode
;;   :init
;;   (setq ac-use-menu-map t)
;;   (setq ac-auto-start 3)
;;   (setq ac-fuzzy-enable t)
;;   (setq ac-use-fuzzy t)
;;   (setq ac-use-quick-help nil)
;;   :config
;;   (ac-config-default)
;;   (define-key ac-completing-map "\C-n" 'ac-next)
;;   (define-key ac-completing-map "\C-p" 'ac-previous)
;;   (define-key ac-completing-map "\C-s" 'ac-isearch)
;;   ;; show help menu beautifully
;;   (use-package pos-tip
;;     :ensure t)

;; auto complete mode
;; (use-package auto-complete
;;   :ensure t
;;   :diminish auto-complete-mode
;;   :init
  
;;   (setq ac-use-menu-map t)
;;   (setq ac-auto-start 4)
;;   (setq ac-use-fuzzy t)
;;   (setq ac-use-quick-help nil)
  
;;   :config
;;   (ac-config-default)
;;   (define-key ac-completing-map "\C-n" 'ac-next)
;;   (define-key ac-completing-map "\C-p" 'ac-previous)
;;   (define-key ac-completing-map (kbd "C-<return>") 'auto-complete))


;;; use 'complete when auto-complete is disabled
;; (setq-default indent-tabs-mode nil)
;; (setq tab-always-indent 'complete)
;; (add-to-list 'completion-styles 'initials t)


;; (use-package popup
;;   :ensure t)


