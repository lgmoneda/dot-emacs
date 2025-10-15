;;; elisp-settings.el --- Settings for the programming language Python

;; Rainbow delimiters in Elisp mode
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(provide 'elisp-settings)
;;; elisp-settings.el ends here
