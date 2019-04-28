;;; email-settings.el --- Settings for sending e-mail inside emacs

;; E-mail config
(setq user-mail-address "lg.moneda@gmail.com")
(setq user-full-name "Luis Moneda")
(setq
send-mail-function 'smtpmail-send-it
message-send-mail-function 'smtpmail-send-it
user-mail-address "lg.moneda@gmail.com"
smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
smtpmail-auth-credentials (expand-file-name "~/.authinfo")
smtpmail-default-smtp-server "smtp.gmail.com"
smtpmail-smtp-server "smtp.gmail.com"
smtpmail-smtp-service 587
smtpmail-debug-info t
starttls-extra-arguments nil
starttls-gnutls-program "/usr/bin/gnutls-cli"
starttls-extra-arguments nil
starttls-use-gnutls t
message-signature "Luis Moneda
http://lgmoneda.github.io/"
)

(provide 'email-settings)
;;; email-settings.el ends here
