;;; scala-settings.el --- Settings for the programming language Scala

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(add-hook 'scala-mode-hook (lambda () (setq tab-width 4)))

(cond ( (string-equal system-type "darwin")
    (progn (setq sbt:program-name "/usr/local/bin/sbt")
    (use-package ensime
      :ensure t
      :init
    (add-hook 'scala-mode-hook 'ensime-mode-hook)
      (setq
      ensime-sbt-command "/usr/local/bin/sbt"
      sbt:program-name "/usr/local/bin/sbt")
      )
    (setq ensime-exec-path "/usr/local/bin/sbt")
    )

    )

      ((string-equal system-type "gnu/linux")
	(setq sbt:program-name "/usr/bin/sbt")
	(use-package ensime
	  :ensure t
	  :init
	(add-hook 'scala-mode-hook 'ensime-mode-hook)
	  (setq
	  ensime-sbt-command "/usr/bin/sbt"
	  sbt:program-name "/usr/bin/sbt")
	)
	(setq ensime-exec-path "/usr/bin/sbt")
       )
      )

(defun scala-run ()
    (interactive)
   (ensime-sbt-action "run")
   (ensime-sbt-action "~compile")
(let ((c (current-buffer)))
    (switch-to-buffer-other-window
   (get-buffer-create (ensime-sbt-build-buffer-name)))
 (switch-to-buffer-other-window c)))
 (setq exec-path
(append exec-path (list ensime-exec-path))) ;;REPLACE THIS with the directory of your scalac executable!

(setq ensime-startup-snapshot-notification nil)
(setq ensime-eldoc-hints 'all)

(defun scalafmt-file ()
  (interactive)
  (let ((str (concat "scalafmt -i -f " buffer-file-name)))
    (message str)
    (shell-command-to-string str))
  (message "scalafmt done"))

(provide 'scala-settings)
;;; scala-settings.el ends here
