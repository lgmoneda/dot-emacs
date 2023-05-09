;; gpt-settings.el --- Settings for using LLM inside Emacs

;; Custom with roam notes
(define-minor-mode chat-minor-mode
  "A minor mode to send chat messages to API."
  :init-value nil
  :lighter " Chat"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'send-message-to-api)
            map))

(defun lgm/org-roam-ai-chat-to-notes ()
  "Create a temporary buffer in org mode for chatting with an API."
  (interactive)
  (let ((buf (get-buffer-create "*Q&A Org-roam*")))
    (switch-to-buffer buf)
    (org-mode)
	(chat-minor-mode)
    (erase-buffer)
    (insert "#+TITLE: Q&A with org-roam\n\n")
    (goto-char (point-max))
    (insert "Type your message here:\n> ")
    (goto-char (point-max))
	))

(defun send-message-to-api ()
  "Send the current message to the API and display the response."
  (interactive)
  (let* ((message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (response (api-send-message message)))
    (goto-char (point-max))
    (insert "\n\n")
    (setq result (split-string-at-substring response "SOURCES:"))
    (insert-string-simulating-typing (nth 0 result))
    (if (nth 1 result)
		(insert (decode-coding-string (nth 1 result) 'utf-8)))
        ;; (insert (nth 1 result)))
    (goto-char (point-max))
    (insert "\n\n> ")))


(defun api-send-message (message)
  "Send MESSAGE to the API and return the response."
  ;; Your API code here
  (setq response (my-call-python-script "~/Dropbox/Agenda/org-roam-ai/chat_roam.py" "" message))
  ;; (message response)
  (concat ">>> " response))

(defun api-send-message (message)
  "Send MESSAGE to the API and return the response."
  ;; Your API code here
  (setq response (my-call-python-script "~/Dropbox/Agenda/org-roam-ai/chat_roam.py" "" message))
  ;; (message response)
  (concat ">>> " response))

(defun call-python-server (request-str)
  "Call the Python server with REQUEST-STR and return the response."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data request-str)
        (url-show-status nil)
        (url-show-trace nil))
    (with-current-buffer
        (url-retrieve-synchronously "http://localhost:8800/")
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(defun call-python-server (input-string &optional api-function search-mode)
  "Call Python server with INPUT-STRING and return the output string."
  (let ((api-function (or api-function "chat"))
		(url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain"))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "http://localhost:8800/api/" api-function "/" search-mode "/" input-string))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(defun api-send-message (input-string)
  "Send MESSAGE to the API and return the response."
  ;; Your API code here
  (setq api-input-string (let ((start-pos (string-match "> " input-string))
      (end-pos (length input-string)))
  (substring input-string start-pos end-pos)))
  (message api-input-string)
  (setq response (call-python-server api-input-string))
  ;; (message response)
  (concat ">>> " response))

(defun insert-string-simulating-typing (string)
  "Inserts STRING into the current buffer simulating typing."
  (interactive "sEnter string to insert: ")
  (let ((delay 0.03)) ; adjust this delay as desired
    (dolist (char (append string nil))
      (insert char)
      (sit-for delay))))

(defun split-string-at-substring (full-str sub-str)
  "Split FULL-STR at SUB-STR and return a list of two strings."
  (if (string-match sub-str full-str)
      (let ((parts (split-string full-str sub-str)))
        (list (car parts) (concat sub-str (mapconcat 'identity (cdr parts) sub-str))))
    (list full-str nil)))

(setq org-roam-ai-search-mode "roam")
(defun toggle-org-roam-ai-search-mode ()
  "Toggle the value of `org-roam-ai-search-mode' between 'roam' and 'open'."
  (interactive)
  (setq org-roam-ai-search-mode
        (if (string= org-roam-ai-search-mode "roam")
            "open"
          "roam"))
  (message "org-roam-ai-search-mode is now %s" org-roam-ai-search-mode))


(defun org-roam-ai-semantic-search-api ()
  "Call the given Python SCRIPT-NAME with the given TEXT as input and
display the output in a new temporary buffer."
    (interactive)
	;; (pyvenv-activate "/Users/luis.moneda/opt/miniconda3/envs/edge")
	(let* ((text (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "Enter search: ")))
		   (buf (get-buffer-create "*org-roam-node-display*"))
		   (script-output (call-python-server text "search" org-roam-ai-search-mode)))
	  ;; (message script-output)
	  ;; (display-org-roam-nodes (read script-output))
	(with-current-buffer buf
      (erase-buffer)
	  (org-mode)
      (insert (format "%s" script-output))
	  (org-shifttab)
	  ;; (insert "\n [[file:/Users/luis.moneda/Dropbox/Agenda/org-roam-ai/output.jpg]]")
	  ;; (org-display-inline-images nil t)
      (display-buffer buf))
	  )
	)

;; ChatGPT Shell
(use-package chatgpt-shell
  :quelpa ((chatgpt-shell :fetcher git :url "https://github.com/xenodium/chatgpt-shell") :upgrade t)
  :init
  (setq chatgpt-repo-path (expand-file-name "chatgpt-shell/" quelpa-build-dir))
  (setq chatgpt-shell-chatgpt-streaming t)
  (setq chatgpt-shell-openai-key
      (plist-get (car (auth-source-search :host "openai.com"))
                 :secret))
  :bind
  ("C-c q" . chatgpt-shell)
  ("C-c d" . dall-e-shell))

;; To use it in org mode, chatgpt-shell language
(require 'ob-chatgpt-shell)
(require 'ob-dall-e-shell)

;; Whisper
;; (use-package whisper
;;   :quelpa ((whisper :fetcher git :url "https://github.com/natrys/whisper.el") :upgrade t)
;;   :init
;;   (setq whisper-repo-path (expand-file-name "whisper/" quelpa-build-dir))
;;   :config
;;   (setq whisper-install-directory "/tmp/"
;;         whisper-model "base"
;;         whisper-language "en"
;;         whisper-translate nil)
;;   (setq whisper--ffmpeg-input-device ":0")
;;   (setq whisper--ffmpeg-input-format "avfoundation")

;;   :bind
;;   ("C-c n r" . whisper-run))

;; Blog functions
(defun call-roam-search-python-server (input-string)
  "Call Python server with INPUT-STRING and return the output string."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain"))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "http://localhost:8800/api/" input-string))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(prefer-coding-system 'utf-8)

(defun org-roam-semantic-search-api ()
  "Call the given Python SCRIPT-NAME with the given TEXT as input and
display the output in a new temporary buffer."
    (interactive)
	(let* ((text (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "Enter search: ")))
		   (buf (get-buffer-create "*org-roam-similar-nodes*"))
		   (api-output (call-roam-search-python-server text)))
	  (with-current-buffer buf
		(erase-buffer)
		(set-buffer-file-coding-system 'utf-8)
		(org-mode)
		(insert (decode-coding-string api-output 'utf-8))
		;; (insert (format "%s" api-output))
		(org-shifttab)
		;; (insert "\n [[file:/Users/luis.moneda/Dropbox/Agenda/org-roam-ai/output.jpg]]")
		;; (org-display-inline-images nil t)
		(display-buffer buf))
	  )
	)

(provide 'gpt-settings)
;;; gpt-settings.el ends here
