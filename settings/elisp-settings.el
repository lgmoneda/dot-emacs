;;; elisp-settings.el --- Settings for the programming language Python  -*- lexical-binding: t; -*-

;; Rainbow delimiters in Elisp mode
(use-package rainbow-delimiters
  :straight t
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(provide 'elisp-settings)
;;; elisp-settings.el ends here
