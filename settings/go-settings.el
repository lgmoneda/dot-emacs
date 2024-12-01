;;; go-settings.el --- Settings for the programming language Go

(use-package go-mode
     :ensure t
     :preface

     (defun bk/set-go-compiler ()
	(if (not (string-match "go" compile-command))
	    (set (make-local-variable 'compile-command)
		 "go build -v && go test -v && go run"))
	(local-set-key (kbd "M-p") 'compile))

     :init

     (when (string-equal system-type "darwin")
	(add-to-list 'exec-path "/usr/local/go/bin/go")
	(setenv "GOPATH" "/usr/local/go/bin/go"))

     (setq gofmt-command "goimports")
     :bind (:map go-mode-map
		  ("C-c C-r" . go-remove-unused-imports)
		  ("C-c i" . go-goto-imports)
		  ("M-." . godef-jump)
		  ("M-*" . pop-tag-mark))
     :config
     (add-hook 'before-save-hook 'gofmt-before-save)
     (add-hook 'go-mode-hook 'bk/set-go-compiler))

(provide 'go-settings)
;;; go-settings.el ends here
