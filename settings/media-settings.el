;;; media-settings.el --- Settings for media (audio, video)

;; (use-package smudge
;;    :ensure t)

;; (load-file "~/Dropbox/Projetos/Emacs/smudge.el")
;; (setq smudge-transport 'connect)
;; (define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
;; (global-smudge-remote-mode)

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players))

(use-package org-emms
  :ensure t)

(provide 'media-settings)
;;; media-settings.el ends here
