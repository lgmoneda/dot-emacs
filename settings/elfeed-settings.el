;;; elfeed-settings.el --- Settings for feeds

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :init (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/Agenda/elfeed.org")))

(use-package elfeed-goodies
  :ensure t
  :init (elfeed-goodies/setup)
  (custom-set-variables
   '(elfeed-goodies/entry-pane-position (quote bottom))))

(provide 'elfeed-settings)
;;; elfeed-settings.el ends here
