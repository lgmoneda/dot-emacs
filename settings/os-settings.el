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
	 (setq markdown-command "/usr/local/bin/pandoc")
	 (setq ispell-program-name "/usr/local/bin/aspell")
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

(provide 'os-settings)
;;; os-settings.el ends here
