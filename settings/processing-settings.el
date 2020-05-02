;;; processing-settings.el --- Settings for programming language Processing

(use-package processing-mode
  :ensure t
  :config
  (define-key processing-mode-map (kbd "C-c C-c") 'processing-sketch-run))

(add-to-list 'load-path "~/.emacs.d/elpa/processing-mode-20171022.2302")
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))

(setq processing-location "/usr/local/bin/processing-java")
(setq processing-application-dir "/Applications/Processing.app")
(setq processing-sketchbook-dir "~/Documents/Processing")

(setq processing-output-dir "~/Downloads")

(defun processing-mode-init ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-dictionary ac-source-yasnippet))
  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary (append processing-functions
                                   processing-builtins
                                   processing-constants)))

(add-to-list 'ac-modes 'processing-mode)
(add-hook 'processing-mode-hook 'processing-mode-init)

(use-package rainbow-mode
  :ensure t
  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'processing-mode)
  (add-to-list 'rainbow-html-rgb-colors-font-lock-keywords
			   '("color(\s*\\([0-9]\\{1,3\\}\\(?:\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\\.[0-9]\\)?\\(?:\s*%\\)?\\)\s*)"
				 (0 (rainbow-colorize-rgb))))
  )

(provide 'processing-settings)
;;; processing-settings.el ends here
