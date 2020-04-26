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
   '(elfeed-goodies/entry-pane-position (quote bottom))
   '(elfeed-goodies/show-mode-padding 40)
   ))

(defun ambrevar/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))


(define-key elfeed-search-mode-map "v" #'ambrevar/elfeed-play-with-mpv)

(add-hook 'elfeed-show-mode-hook
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (setq-local truncate-lines nil)
                (setq-local shr-width 85)
                (set-buffer-modified-p nil))
              (setq-local left-margin-width 15)
              (setq-local right-margin-width 15)
              ))

(provide 'elfeed-settings)
;;; elfeed-settings.el ends here
