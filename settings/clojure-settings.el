;;; clojure-settings.el --- Settings for the programming language Clojure

;; (use-package clojure-mode
;;     :ensure t)
;; (use-package clj-refactor
;;     :ensure t)
;; (use-package cider
;;     :ensure t)

;; ;; start cider and clj-refactor when clojure-mode is enabled (by default, on .clj files)
;; (add-hook 'clojure-mode-hook (lambda ()
;; 			(cider-mode 1)
;; 			(clj-refactor-mode 1)
;; 			(cljr-add-keybindings-with-prefix "C-c C-o")
;; 			(setq clojure-align-forms-automatically t)))
;; (add-hook 'clojure-mode-hook 'paredit-mode)
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(use-package clojure-mode-extra-font-locking
	     :ensure t)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

(use-package cider
  :ensure t
  :after clojure
  :mode (("\\.edn$\\'" . clojure-mode)
	 ("\\.boot$\\'" . clojure-mode)
	 ("\\.cljs.*$\\'" . clojure-mode)
	 ("\\.lein-env\\'" . clojure-mode))
  :init
  (setq cider-repl-pop-to-buffer-on-connect t
	cider-show-error-buffer t
	cider-auto-select-error-buffer t
	cider-repl-display-help-banner nil
	cider-repl-history-file "~/.emacs.d/cider-history.log"
	cider-repl-wrap-history t)
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode))

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

(provide 'clojure-settings)
;;; clojure-settings.el ends here
