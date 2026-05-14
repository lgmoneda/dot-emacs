;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Emacs runs package activation before init.el unless this is set here.
;; Some Emacs 32 package autoloads need cl-lib macros before activation.
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Avoid redisplay churn before the user configuration has selected fonts/theme.
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(defun lgm/ignore-startup-echo-area-message (&rest _args)
  "Disable the startup echo area message on Emacs 32 macOS builds."
  nil)

(advice-add 'display-startup-echo-area-message
            :override #'lgm/ignore-startup-echo-area-message)

;;; early-init.el ends here
