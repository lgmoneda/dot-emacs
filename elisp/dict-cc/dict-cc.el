;; Wrapping https://github.com/rbaron/dict.cc.py

;; The default language map to be used
;; for fast consult in the most common case.
(setq dict-cc-default-map "en pt")


;; Base query function
(defun dict-cc-query (word dict-cc-map)
  (let ((inhibit-read-only t))
    (switch-to-buffer-other-window "*dict.cc*")
    ;; Or maybe people enjoy the log
    (erase-buffer)
    (call-process-shell-command (format "dict.cc.py %s %s" dict-cc-map word) nil t)
    (special-mode)
    ))

;; Prompt user for a word to be translated
;; using the default map
(defun dict-cc-default-word ()
  (interactive)
  (let ((word (read-input (format "Word to translate[%s]: " dict-cc-default-map))))
    (dict-cc-query word dict-cc-default-map)
    ))

;; Prompt user for both map and word
(defun dict-cc-word ()
  (interactive)
  (let ((map-to-use (read-input (format "From-to languages [eg. %s]: " dict-cc-default-map)))
	(word (read-input  "Word to translate: ")))
        (dict-cc-query word map-to-use)
    )
  )

;; Translate word under cursor using default-map
(defun dict-cc-default-at-point ()
  (interactive)
  (let ((word (word-at-point)))
    (dict-cc-query word dict-cc-default-map)
    ))


;; Bindings
;; Using only dict-cc-default-at-point with C-x t
;; is a good idea too.
(global-set-key (kbd "C-c t t")
    (lambda ()
      (interactive)
      (dict-cc-default-at-point)))

(global-set-key (kbd "C-c t w")
    (lambda ()
      (interactive)
      (dict-cc-default-word)))

(global-set-key (kbd "C-c t r")
    (lambda ()
      (interactive)
      (dict-cc-word)))


;; Version for very special people

(defun dict-cc-query-async (word dict-cc-map)
  (with-output-to-temp-buffer "*dict.cc*"
    (shell-command
     (format "dict.cc.py %s %s &" dict-cc-map word)
     "*dict.cc*")
    (sleep-for 2)
    (pop-to-buffer "*dict.cc*")
    (special-mode)
  ))


(defun dict-cc-at-point-async ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (dict-cc-query-async word dict-cc-default-map)
    )
  )


(defun dict-cc-default-word-async ()
  (interactive)
  (let ((word (read-input (format "Word to translate[%s]: " dict-cc-default-map))))
    (dict-cc-query-async word dict-cc-default-map)
    ))

(defun dict-cc-word-async ()
  (interactive)
  (let ((map-to-use (read-input (format "From-to languages [eg. %s]: " dict-cc-default-map)))
	(word (read-input  "Word to translate: ")))
        (dict-cc-query-async word map-to-use)
    )
  )


(global-set-key (kbd "C-x t")
    (lambda ()
      (interactive)
      (dict-cc-at-point-async)))

(global-set-key (kbd "C-c t w")
    (lambda ()
      (interactive)
      (dict-cc-default-word-async)))

(global-set-key (kbd "C-c t r")
    (lambda ()
      (interactive)
      (dict-cc-word-async)))


(provide 'dict-cc)
