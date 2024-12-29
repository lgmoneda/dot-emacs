;; gpt-settings.el --- Settings for using LLM inside Emacs

;; Custom with roam notes
(define-minor-mode chat-minor-mode
  "A minor mode to send chat messages to API."
  :init-value nil
  :lighter " Chat"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "RET") 'exchange-with-chat-api)
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
	;; (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/edge")
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

;; (use-package ob-chatgpt-shell
;;   :ensure t)
;; (use-package ob-dall-e-shell
;;   :ensure t)

;; ChatGPT Shell
(use-package chatgpt-shell
  ;; :quelpa ((chatgpt-shell :fetcher git :url "https://github.com/xenodium/chatgpt-shell") :upgrade t)
  :ensure t
  :init
  ;; (setq chatgpt-repo-path (expand-file-name "chatgpt-shell/" quelpa-build-dir))
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  (setq chatgpt-shell-chatgpt-streaming t)
  ;; (setq chatgpt-shell-openai-key
  ;;     (plist-get (car (auth-source-search :host "openai.com"))
  ;;                :secret))
  (setq chatgpt-shell-model-version "gpt-4o")
  (require 'chatgpt-shell)
  ;; (require 'ob-chatgpt-shell)
  ;; (ob-chatgpt-shell-setup)
  ;; (require 'ob-dall-e-shell)
  :bind
  ("C-c q" . chatgpt-shell)
  ("C-c d" . dall-e-shell))

;; Whisper
;; (use-package whisper
;;   :quelpa ((whisper :fetcher git :url "https://github.com/natrys/whisper.el") :upgrade t)
;;   :init
;;   (setq whisper-repo-path (expand-file-name "whisper/" quelpa-build-dir))
;;   :config
;;   (setq whisper-install-directory "~/.emacs.d/.cache/"
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
		(display-buffer buf)
		)
	  )
	)

;;I change the value of this variable to enable me to run two process in different output buffers
;; without confirming I want to create another buffer
(setq async-shell-command-buffer 'new-buffer)

(defun start-semantic-search ()
  (interactive)
  (async-shell-command "source ~/.zshrc && conda activate ml3 && python /Users/luis.moneda/repos/org-roam-ai/semantic_search.py")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*"))))

(start-semantic-search)

;; Q&A

(defun call-chat-server (input-string)
  "Call Python server with INPUT-STRING and return the output string."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain"))))
    (with-current-buffer
        (url-retrieve-synchronously (concat "http://localhost:8880/api/" input-string))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(defun exchange-with-chat-api ()
  "Send the current message to the API and display the response."
  (interactive)
  (let* ((message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (response (call-chat-server message)))
    (goto-char (point-max))
    (insert "\n\n")
    (setq result (split-string-at-substring response "SOURCES:"))
    (insert-string-simulating-typing (nth 0 result))
    (if (nth 1 result)
		(insert (decode-coding-string (nth 1 result) 'utf-8)))
    (goto-char (point-max))
    (insert "\n\n> ")))

(defun start-qna ()
  (interactive)
  (async-shell-command "source ~/.zshrc && conda activate ml3 && python /Users/luis.moneda/repos/org-roam-ai/qna.py")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<2>"))))

(start-qna)

;; Colorful Chunk Semantic Search
(require 'cl-lib)
(require 'org-roam)
(require 'json)
(require 'popup)

(defun my/split-into-sentences (start end)
  "Split the region or buffer between START and END into sentences and return them as a list."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((sentences '()))
      (while (re-search-forward "[^.!?]+[.!?]*" end t)
        (push (string-trim (match-string 0)) sentences))
      (nreverse sentences))))

(defun my/generate-colors (length)
  "Generate a list of LENGTH repeating the base colors."
  (let ((base-colors '("#F4C1BD" "#D9E2B8" "#F7E5A6" "#F7D0B4" "#F2C1D6" "#AFC8EB" "#CDC3E5" "#B9E2DE"
)))
    (cl-loop for i below length
             collect (nth (mod i (length base-colors)) base-colors))))

(defun my/apply-colors-to-chunks (chunks)
  "Apply different colors to CHUNKS in the current buffer."
  (let ((colors (my/generate-colors (length chunks))))
    (cl-loop for chunk in chunks
             for color in colors
             for start = (car chunk)
             for end = (cdr chunk)
             do (overlay-put (make-overlay start end) 'face `(:background ,color)))))

(defun my/semantic-search (sentence callback)
  "Perform semantic search for SENTENCE and call CALLBACK with parsed results."
  (let ((response (call-roam-search-python-server sentence)))
    (funcall callback (my/parse-roam-search-results response))))

(defun my/parse-roam-search-results (response)
  "Parse the RESPONSE from Roam search into a list of titles and links."
  (let ((lines (split-string response "\n"))
        results)
    (dolist (line lines)
      (when (string-match "\\(\\[\\[id:\\([^\]]+\\)\\]\\[\\([^]]+\\)\\]\\)" line)
        (let ((id (match-string 2 line))
              (title (match-string 3 line)))
          (push (cons title id) results))))
    (nreverse results)))

;; (defun my/display-popup (results)
;;   "Display RESULTS in a popup menu and handle selection."
;;   (let ((titles (mapcar #'car results)))
;;     (when titles
;;       (let ((popup (popup-menu* titles
;;                                :keymap (let ((map (make-sparse-keymap)))
;;                                          (define-key map (kbd "RET") 'popup-select)
;;                                          (define-key map (kbd "TAB") 'popup-selected-item)
;;                                          (define-key map (kbd "C-n") 'popup-next)
;;                                          (define-key map (kbd "C-p") 'popup-previous)
;;                                          map))))
;;         (when popup
;;           (let ((entry (assoc popup results)))
;;             (message "Selected: %s" (car entry))
;;             (when entry
;;               ;; Open the org-roam node in a new buffer without changing the cursor
;;               (my/open-org-roam-in-new-buffer entry))))))))

(defun my/open-org-roam-node-in-side-buffer (entry)
  "Open an org-roam node by NODE-ID in a side buffer, without moving cursor."
  (interactive "sEnter node ID: ")
  (let* ((node-id (cdr entry))
  (node (org-roam-node-from-id node-id)))
    (if node
        (save-selected-window  ;; Prevent cursor from moving to the new window
          (let ((buffer (org-roam-node-visit node t t))) ;; Visit the node
			(message "Working on it!")
			(org-fold-show-subtree)  ;; Expand the subtree
			;; Center the content at the top of the window
			(recenter 0)
            ))
      (message "Node ID not found."))))

(defun my/show-results-on-hover (start end)
  "Show semantic search results in a popup for the chunk between START and END."
  (let ((sentence (buffer-substring-no-properties start end)))
    (my/semantic-search sentence
     (lambda (results)
       (overlay-put (make-overlay start end) 'my-results results)))))

(defun my/setup-interactions (chunks)
  "Set up key bindings and interactions for CHUNKS."
  (cl-loop for chunk in chunks
           for start = (car chunk)
           for end = (cdr chunk)
           do (my/show-results-on-hover start end)))

(defun my/navigate-and-display (results)
  "Set up navigation to dynamically display RESULTS in a side buffer."
  (let ((buffer (get-buffer-create "*Semantic Search Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (string-join results "\n")))
    (display-buffer buffer)))

;; This uses the simple regex parser
;; (defun my/analyze-region (start end)
;;   "Main function to analyze the region between START and END."
;;   (interactive "r")
;;   (let* ((sentences (my/split-into-sentences start end))
;;          (chunks (cl-loop with pos = start
;;                           for s in sentences
;;                           collect (let ((chunk-start pos)
;;                                         (chunk-end (+ pos (length s))))
;;                                     (setq pos (1+ chunk-end)) ;; Move pos to after the chunk
;;                                     (cons chunk-start chunk-end)))))
;;     (my/apply-colors-to-chunks chunks)
;;     (my/setup-interactions chunks))
;;   (deactivate-mark))

;; Using popup.el
;; (defun my/display-popup-at-point ()
;;   "Display popup with semantic search results for the chunk at point."
;;   (interactive)
;;   (let ((ov (car (overlays-at (point)))))
;;     (when ov
;;       (let* ((results (overlay-get ov 'my-results)))
;;         (my/display-popup results)))))

;; (global-set-key (kbd "C-c h") 'my/display-popup-at-point)

;; Using python code to do the parsing so I can apply ml models
(defun start-discourse-segmentation ()
  (interactive)
  (async-shell-command "source ~/.zshrc && conda activate ml3 && python /Users/luis.moneda/repos/org-roam-ai/discourse_segmentation.py")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<3>"))))

(start-discourse-segmentation)

(defun discourse-segmentation-api ()
  "Call the given Python SCRIPT-NAME with the given TEXT as input and
display the output in a new temporary buffer."
    (interactive)
	(let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
		   (api-output (my/get-segments text)))
	  api-output))

(defun my/get-segments (query)
  "Send QUERY to the Python HTTP server and return the segments as an Emacs Lisp list."
  (require 'url)
  (require 'json)
  (let* ((url (concat "http://localhost:8866/api/segment/" (url-encode-url query)))
         (response-buffer (url-retrieve-synchronously url))
         segments)
    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          ;; Debug: Display the entire response
          ;; (message "Response buffer: %s" (buffer-string))
          ;; Skip HTTP headers
          (if (search-forward "\n\n" nil t)
              (let* ((json-object-type 'alist) ;; Parse JSON objects as alists
                     (json-array-type 'list)   ;; Parse JSON arrays as lists
                     (json-key-type 'string)  ;; Use strings for keys
                     (parsed-json (condition-case err
                                      (json-read)
                                    (error (message "JSON read error: %s" err)
                                           nil))))
                ;; Debug: Show the parsed JSON content
                ;; (message "Parsed JSON: %s" parsed-json)
                ;; Extract segments key
                (setq segments (cdr (assoc "segments" parsed-json))))
            (message "Failed to locate JSON body in response"))
          (kill-buffer response-buffer)) ;; Clean up buffer
      (message "No response received"))
    ;; Debug: Show the final segments list
    ;; (message "Segments: %s" segments)
    segments))

(defun chunky-semantic-search (start end)
  "Main function to analyze the region between START and END."
  (interactive "r")
  (let* ((sentences (discourse-segmentation-api))
         (chunks (cl-loop with pos = start
         for s in sentences
         collect (progn
                   (goto-char pos) ;; Start searching from the current position
                   (when (search-forward s end t)
                     (let ((chunk-start (match-beginning 0))
                           (chunk-end (match-end 0)))
                       (setq pos (1+ chunk-end)) ;; Update `pos` for the next iteration
                       (cons chunk-start chunk-end)))))))
    ;; Filter out any nil results (in case a sentence isn't found)
    (setq chunks (cl-remove-if-not #'identity chunks))
    ;; Apply colors and interactions
    (my/apply-colors-to-chunks chunks)
    (my/setup-interactions chunks))
  (deactivate-mark))

;; Using consult as the interface
(require 'consult)

;; This function keeps the menu. It is flaky due to the re-openning
(defun my/display-popup-at-point ()
  "Display search results using consult for the chunk at point, without closing the menu after selection."
  (interactive)
  (let ((ov (car (overlays-at (point)))))
    (when ov
      (let ((results (overlay-get ov 'my-results))
			(ivy-height 17)
            (ivy-posframe-height 17)
            (ivy-posframe-width 100) ;; Adjust width
			(ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point))))
        (when results
          (let* ((titles (mapcar #'car results))
                 (last-selection nil)) ;; Store the last selection
            (catch 'exit
              (while t
                (let* ((selected (consult--read
                                  titles
                                  :prompt "Select entry: "
                                  :category 'org-roam
                                  :require-match t
                                  :sort nil
                                  :history t
                                  :default last-selection))) ;; Default to the last selection
                  (if selected
                      (progn
                        (setq last-selection selected) ;; Update last selection
                        (let ((entry (assoc selected results)))
                          (when entry
                            (my/open-org-roam-node-in-side-buffer entry))))
                    ;; Exit the menu if no selection is made
                    (throw 'exit nil)))))))))))

(global-set-key (kbd "C-c s") 'my/display-popup-at-point)




(provide 'gpt-settings)
;;; gpt-settings.el ends here
