;;; nu-settings.el --- Settings for specific Nu things

;; Node connection
(load "~/Dropbox/Projetos/Emacs/.nu-node.el")
(setenv "DATABRICKS_TOKEN" (getenv "DATABRICKS_TOKEN"))
(load-file "/Users/luis.moneda/Dropbox/ai-docs/databricks-emacs/dbxnb.el")


(provide 'nu-settings)
;;; nu-settings.el ends here
