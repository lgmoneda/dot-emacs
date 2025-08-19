;;; os-settings.el --- Settings for editing utilities

(cond
    ((string-equal system-type "gnu/linux")
     (progn
      (+ 1 1)
    ))
    ((string-equal system-type "darwin")
        (progn
	 (add-to-list 'exec-path "/usr/local/bin" "/Library/TeX/texbin/pdflatex")
	 ;;add hookup shell
	 (add-hook 'shell-mode-hook (lambda ()
				      (setenv "PATH" (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\""))
				      (setq exec-path (append (parse-colon-path (getenv "PATH")) (list exec-directory)))

				      ))
	 ;; I define it differently in another place, take a look when needed
	 (setq markdown-command "/usr/local/bin/pandoc")
	 (setq ispell-program-name "/opt/homebrew/bin/aspell")
	 (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
	 )
	)
)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  (exec-path-from-shell-copy-env "ANTHROPIC_API_KEY")
  (exec-path-from-shell-copy-env "GOOGLE_API_KEY")
  (exec-path-from-shell-copy-env "OPENROUTER_API_KEY")
  (exec-path-from-shell-copy-env "BRAVESEARCH_API_KEY")
  )

(setenv "PATH" (concat (getenv "PATH") ":/Users/luis.moneda/.npm-global/bin"))
(setq exec-path (append exec-path '("/Users/luis.moneda/.npm-global/bin")))

(provide 'os-settings)
;;; os-settings.el ends here
