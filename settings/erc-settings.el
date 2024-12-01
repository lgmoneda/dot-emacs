;;; org-settings.el --- Settings for org utilities

;;Tuesday, August 15, 2017
;;============================
;;==          ERC           ==
;;============================
;; (add-to-list 'load-path "~/.emacs.d/elisp/erc-extras" t)

;; ;; Trying to display nicely
;; (erc-spelling-mode 1)
;; (add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))
;; (make-variable-buffer-local 'erc-fill-column)
;;  (add-hook 'window-configuration-change-hook
;; 	   '(lambda ()
;; 	      (save-excursion
;; 	        (walk-windows
;; 		 (lambda (w)
;; 		   (let ((buffer (window-buffer w)))
;; 		     (set-buffer buffer)
;; 		     (when (eq major-mode 'erc-mode)
;; 		       (setq erc-fill-column (- (window-width w) 2)))))))))


;; (when (assoc "en0" (network-interface-list))
;;   (erc :server "irc.freenodenet" :port 6667 :nick "lgmoneda"))


;; (progn
;;      (require 'erc)
;;      (require 'erc-track)
;;      (erc-track-mode +1)

;;      ;; keywords to track
;;      (setq erc-keywords '("bayes" "reinforcement" "unsupervised" "bayesian"))

;;      ;; Ignore Server Buffer
;;      (setq erc-track-exclude-server-buffer t)

;;      ;; show only when my nickname is mentioned in any channel
;;      (setq erc-current-nick-highlight-type 'nick)
;;      (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
;;      (setq erc-track-use-faces t)
;;      (setq erc-track-faces-priority-list
;;            '(erc-current-nick-face
;;              erc-keyword-face
;;              erc-direct-msg-face))
;;      (setq erc-track-priority-faces-only 'all))

;; (load "~/.ercfile")
;; (require 'erc-services)
;; (erc-services-mode 1)
;; (setq erc-prompt-for-nickserv-password nil)
;; (setq erc-nickserv-passwords
;;       `((freenode (("lgmoneda" . ,lgmonedanick)))))

;; ;; Prevents Erc buffers flashing at start
;; (erc-autojoin-mode t)
;; (setq erc-autojoin-timing :ident)
;; (setq erc-autojoin-delay 40)
;; (setq erc-join-buffer 'bury)
;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#emacs" "#sptk" "##machinelearning"
;; 	 "#scikit-learn" "#tensorflow")))

;; (erc-autojoin-after-ident "irc.freenode.net" "lgmoneda")

;; (add-hook 'erc-nickserv-identified-hook 'erc-autojoin-after-ident)

;; ;; Log in a buffer when people talk to me
;; (setq erc-log-matches-flag t)
;; (setq erc-log-matches-types-alist
;;           '((keyword . "### Keywords Log ###")
;;             (current-nick . "### Me Log ###")))

;; ;; Prevent the new created buffer from pvt to be brought visible
;; (setq erc-auto-query 'bury)

;; ;; Both defadvices to track pvts using channels mode-line face
;; (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
;;   (if (erc-query-buffer-p)
;;       (setq ad-return-value (intern "erc-current-nick-face"))
;;     ad-do-it))

;; (defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
;;   (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
;;   ad-do-it
;;   (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))

;; (require 'erc-nicklist)
;; ;; bk's toggle nicklist!
;; (defun bk/nicklist-toggle ()
;;   "Function to toggle the nicklist in ERC mode."
;;   (interactive)
;;   (let ((nicklist-buffer-name (format " *%s-nicklist*" (buffer-name))))
;;     (if (get-buffer nicklist-buffer-name)
;;         (kill-buffer nicklist-buffer-name)
;;       (erc-nicklist))))

;; (define-key erc-mode-map (kbd "<f7>") 'bk/nicklist-toggle)

;; Smarter beep
;; Remember to apt-get install mplayer!
;; todo: no beep when buffer is visible
(add-hook 'erc-text-matched-hook 'erc-sound-if-not-server)
(defun erc-sound-if-not-server (match-type nickuserhost msg)
  (unless (or
	   (string-match "Serv" nickuserhost)
	   (string-match nickuserhost (erc-current-nick))
	   (string-match "Server" nickuserhost))
	(when (string= match-type "current-nick")
	(start-process-shell-command "lolsound" nil "mplayer ~/.emacs.d/sounds/icq-message.wav"))
	;;(setq mode-line-end-spaces
        ;; to-do use message-truncate-lines or messages-buffer-max-lines

       	(message
	      (format "[%s|<%s:%s> %s]"
	      	      (format-time-string "%Hh%M" (date-to-time (current-time-string)))
	      	      (subseq nickuserhost 0 (string-match "!" nickuserhost))
		      (or (erc-default-target) "")
		      (subseq msg 0 (- (length msg) 1))
		      ;; (if (eq (string-match (erc-current-nick) msg) 0)
		      ;; 	  (subseq msg (+ 1 (length (erc-current-nick))) 40)
		      ;; 	  msg
		      ;; 	  )
		      )
	      ;; Show msg for 20s
	       (run-with-timer 20 nil
                  (lambda ()
                    (message nil)))
	      )))

(provide 'erc-settings)
;;; erc-settings.el ends here
