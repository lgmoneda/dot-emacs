;;; r-settings.el --- Settings for the programming language R

(setq inferior-R-program-name "/Library/Frameworks/R.framework/Resources/R")

(use-package ess
	     :ensure t
	     :init
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  ;; (require 'ess-eldoc)
  )

(provide 'r-settings)
;;; r-settings.el ends here
