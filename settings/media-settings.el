;;; media-settings.el --- Settings for media (audio, video)

(use-package helm-spotify-plus
   :ensure t)

;; Helm-spotify-plus key binds
(global-set-key (kbd "C-c C-s") 'helm-spotify-plus)
(defhydra hydra-spotify (global-map "C-c s")
  "helm-spotify-plus"
 ("s" helm-spotify-plus)
 ("f" helm-spotify-plus-next)
 ("b" helm-spotify-plus-previous)
 ("p" helm-spotify-plus-play)
 ("g" helm-spotify-plus-pause))

(provide 'media-settings)
;;; media-settings.el ends here
