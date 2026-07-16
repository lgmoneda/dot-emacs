;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Emacs runs package activation before init.el unless this is set here.
;; Some Emacs 32 package autoloads need cl-lib macros before activation.
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Avoid redisplay churn before the user configuration has selected fonts/theme.
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; emacs-jupyter currently trips native compilation in the monadic execution
;; path on this setup (`state' becomes void in `jupyter-bind').
(defconst lgm/native-comp-jupyter-deny-list
  '("/\\(?:ob-\\)?jupyter.*\\.el\\'"))

(setq native-comp-jit-compilation-deny-list
      (append lgm/native-comp-jupyter-deny-list
              (bound-and-true-p native-comp-jit-compilation-deny-list))
      native-comp-deferred-compilation-deny-list
      (append lgm/native-comp-jupyter-deny-list
              (bound-and-true-p native-comp-deferred-compilation-deny-list)))

(defun lgm/ignore-startup-echo-area-message (&rest _args)
  "Disable the startup echo area message on Emacs 32 macOS builds."
  nil)

(advice-add 'display-startup-echo-area-message
            :override #'lgm/ignore-startup-echo-area-message)

;;; early-init.el ends here
