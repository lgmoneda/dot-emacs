;;; elisp-settings.el --- Settings for the programming language Python

;; Fast jump to elisp function
;; In Debian derived distros, the package emacs24 or similar
;; does not include the source elisp code, you need it to
;; jump to the definition (.el or .el.gz files). To download it
;; install emacs24-el package.
(defun lgm/describe-func ()
  (interactive)
  (describe-function (function-called-at-point)))

(defun lgm/jump-to-elisp-func-def ()
  (interactive)
  (find-function (function-called-at-point))
  )

(global-set-key (kbd "C-h C-j") 'lgm/jump-to-elisp-func-def)
(global-set-key (kbd "C-h C-f") 'lgm/describe-func)

;; Rainbow delimiters in Elisp mode
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(provide 'elisp-settings)
;;; elisp-settings.el ends here
