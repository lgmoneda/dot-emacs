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
;;   :straight t)
;; (use-package ob-dall-e-shell
;;   :straight t)

;; ChatGPT Shell
(use-package chatgpt-shell
  ;; :quelpa ((chatgpt-shell :fetcher git :url "https://github.com/xenodium/chatgpt-shell") :upgrade t)
  :straight t
  :init
  ;; (setq chatgpt-repo-path (expand-file-name "chatgpt-shell/" quelpa-build-dir))
  (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))
  (setq chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY"))
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
  ("C-c d" . chatgpt-shell-send-and-review-region)
  ;; ("C-c d" . dall-e-shell)
  )

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

;; Org-roam semantic search
(require 'button)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)

(defgroup org-roam-semantic-search nil
  "Semantic search interface for Org-roam."
  :group 'org-roam)

(defcustom org-roam-semantic-search-default-view 'detailed
  "Default presentation used for semantic search results."
  :type '(choice (const :tag "Detailed result list" detailed)
                 (const :tag "Simple Org response" simple))
  :group 'org-roam-semantic-search)

(defcustom org-roam-semantic-search-result-count 30
  "Default number of semantic search results."
  :type 'integer
  :group 'org-roam-semantic-search)

(defcustom org-roam-semantic-search-query-preview-length 200
  "Maximum visible characters before a search query is folded."
  :type 'integer
  :group 'org-roam-semantic-search)

(defcustom org-roam-semantic-search-separate-scores t
  "Add a thin background-colored separation between score badges."
  :type 'boolean
  :group 'org-roam-semantic-search)

;; Set to nil for a continuous purple score column.
(setq org-roam-semantic-search-separate-scores t)

(defface org-roam-semantic-search-score
  '((t (:inherit org-todo :inverse-video t :weight bold)))
  "Face used for semantic distance scores."
  :group 'org-roam-semantic-search)

(defface org-roam-semantic-search-title
  '((t (:inherit org-agenda-structure :height 1.15)))
  "Face used for the semantic search buffer title."
  :group 'org-roam-semantic-search)

(defface org-roam-semantic-search-query
  '((t (:inherit org-quote :extend t)))
  "Face used for the semantic search query."
  :group 'org-roam-semantic-search)

(defface org-roam-semantic-search-hierarchy-arrow
  '((t (:inherit org-todo :weight bold)))
  "Face used for separators between hierarchy levels."
  :group 'org-roam-semantic-search)

(defface org-roam-semantic-search-nearest-parent
  '((t (:inherit shadow :weight semibold)))
  "Face used for the nearest parent in a hierarchy."
  :group 'org-roam-semantic-search)

(defvar-local org-roam-semantic-search--query nil)
(defvar-local org-roam-semantic-search--result-markers nil)
(defvar-local org-roam-semantic-search--detail-overlays nil)
(defvar-local org-roam-semantic-search--query-overlay nil)
(defvar-local org-roam-semantic-search--result-count nil)
(defvar-local org-roam-semantic-search--view nil)

(defun call-roam-search-python-server (input-string &optional k format)
  "Search for INPUT-STRING and return the response body.
K is the result count.  FORMAT is either `org' or `json'."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (query (url-hexify-string input-string))
         (parameters
          (format "?k=%d&format=%s"
                  (or k org-roam-semantic-search-result-count)
                  (or format "org")))
         (response-buffer
          (url-retrieve-synchronously
           (concat "http://localhost:8800/api/" query parameters))))
    (unless response-buffer
      (user-error "Semantic search service is unavailable"))
    (unwind-protect
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (user-error "Malformed response from semantic search service"))
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))
      (kill-buffer response-buffer))))

(defvar org-roam-semantic-search-mode-map (make-sparse-keymap)
  "Keymap for `org-roam-semantic-search-mode'.")

(set-keymap-parent org-roam-semantic-search-mode-map special-mode-map)
(define-key org-roam-semantic-search-mode-map
            (kbd "n") #'org-roam-semantic-search-next-result)
(define-key org-roam-semantic-search-mode-map
            (kbd "p") #'org-roam-semantic-search-previous-result)
(define-key org-roam-semantic-search-mode-map
            (kbd "RET") #'org-roam-semantic-search-open-result)
(define-key org-roam-semantic-search-mode-map
            (kbd "o") #'org-roam-semantic-search-open-result-other-window)
(define-key org-roam-semantic-search-mode-map
            (kbd "TAB") #'org-roam-semantic-search-toggle-details)
(define-key org-roam-semantic-search-mode-map
            (kbd "<tab>") #'org-roam-semantic-search-toggle-details)
(define-key org-roam-semantic-search-mode-map
            (kbd "<backtab>") #'org-roam-semantic-search-toggle-all-details)
(define-key org-roam-semantic-search-mode-map
            (kbd "S-TAB") #'org-roam-semantic-search-toggle-all-details)
(define-key org-roam-semantic-search-mode-map
            (kbd "g") #'org-roam-semantic-search-refresh)
(define-key org-roam-semantic-search-mode-map
            (kbd "/") #'org-roam-semantic-search-edit-query)
(define-key org-roam-semantic-search-mode-map
            (kbd "v") #'org-roam-semantic-search-toggle-view)

(define-derived-mode org-roam-semantic-search-mode special-mode
  "Semantic Search"
  "Major mode for browsing Org-roam semantic search results."
  (setq-local truncate-lines nil)
  (hl-line-mode 1))

(defun org-roam-semantic-search--move-result (direction)
  "Move to the next result in DIRECTION, which is 1 or -1."
  (let* ((positions (mapcar #'marker-position
                            org-roam-semantic-search--result-markers))
         (line-start (line-beginning-position))
         (destination
          (if (> direction 0)
              (seq-find (lambda (position) (> position line-start)) positions)
            (car (last (seq-filter
                        (lambda (position) (< position line-start))
                        positions))))))
    (if destination
        (goto-char destination)
      (message "No %s result"
               (if (> direction 0) "next" "previous")))))

(defun org-roam-semantic-search-next-result ()
  "Move point to the next semantic search result."
  (interactive)
  (org-roam-semantic-search--move-result 1))

(defun org-roam-semantic-search-previous-result ()
  "Move point to the previous semantic search result."
  (interactive)
  (org-roam-semantic-search--move-result -1))

(defun org-roam-semantic-search-open-result ()
  "Open the Org-roam node represented by the result at point."
  (interactive)
  (let ((node-id (get-text-property (point) 'org-roam-node-id)))
    (unless node-id
      (user-error "No semantic search result at point"))
    (org-roam-id-open node-id nil)))

(defun org-roam-semantic-search-open-result-other-window ()
  "Open the Org-roam node at point in another window."
  (interactive)
  (let* ((node-id (get-text-property (point) 'org-roam-node-id))
         (marker (and node-id (org-id-find node-id 'marker))))
    (unless node-id
      (user-error "No semantic search result at point"))
    (unless marker
      (user-error "Cannot find Org-roam node %s" node-id))
    (org-mark-ring-push)
    (switch-to-buffer-other-window (marker-buffer marker))
    (goto-char marker)
    (move-marker marker nil)
    (org-fold-show-context)))

(defun org-roam-semantic-search--detail-overlay-at-point ()
  "Return the detail overlay belonging to the result at point."
  (let ((index (get-text-property (point) 'org-roam-result-index)))
    (seq-find
     (lambda (overlay)
       (eq (overlay-get overlay 'org-roam-result-index) index))
     org-roam-semantic-search--detail-overlays)))

(defun org-roam-semantic-search--set-details-visible (overlay visible)
  "Set details in OVERLAY to VISIBLE."
  (overlay-put overlay 'invisible (not visible))
  (overlay-put (overlay-get overlay 'fold-indicator)
               'after-string
               (unless visible (propertize " …" 'face 'shadow))))

(defun org-roam-semantic-search--set-query-visible (visible)
  "Set the folded query text to VISIBLE."
  (overlay-put org-roam-semantic-search--query-overlay
               'invisible
               (not visible))
  (overlay-put
   (overlay-get org-roam-semantic-search--query-overlay 'fold-indicator)
   'after-string
   (unless visible
     (propertize " …" 'face '(org-roam-semantic-search-query shadow)))))

(defun org-roam-semantic-search-toggle-details ()
  "Toggle the query or result hierarchy at point."
  (interactive)
  (if (get-text-property (point) 'org-roam-semantic-search-query)
      (if org-roam-semantic-search--query-overlay
          (org-roam-semantic-search--set-query-visible
           (overlay-get org-roam-semantic-search--query-overlay 'invisible))
        (user-error "The search query is already fully visible"))
    (let ((overlay (org-roam-semantic-search--detail-overlay-at-point)))
      (unless overlay
        (user-error "No foldable semantic search result at point"))
      (org-roam-semantic-search--set-details-visible
       overlay
       (overlay-get overlay 'invisible)))))

(defun org-roam-semantic-search-toggle-all-details ()
  "Show all folded result details, or fold all when all are visible."
  (interactive)
  (let ((show (seq-some
               (lambda (overlay) (overlay-get overlay 'invisible))
               org-roam-semantic-search--detail-overlays)))
    (dolist (overlay org-roam-semantic-search--detail-overlays)
      (org-roam-semantic-search--set-details-visible overlay show))))

(defun org-roam-semantic-search--parent-headings (hierarchy title)
  "Return parent headings from HIERARCHY, excluding TITLE."
  (let ((headings (split-string hierarchy " > " t "[[:space:]\n]+")))
    (when (and headings (string= (car (last headings)) title))
      (setq headings (butlast headings)))
    headings))

(defun org-roam-semantic-search--insert-hierarchy (headings)
  "Insert parent HEADINGS as a vertical hierarchy trail."
  (let ((last-index (1- (length headings))))
    (cl-loop for heading in headings
             for index from 0
             for nested = (> index 0)
             for prefix = (if nested "            " "          ")
             for wrap-prefix = (if nested "              " "          ")
             for line-start = (point)
             do
             (insert prefix)
             (when nested
               (insert (propertize
                        "↳ "
                        'face 'org-roam-semantic-search-hierarchy-arrow)))
             (insert (propertize
                      heading
                      'face
                      (if (= index last-index)
                          'org-roam-semantic-search-nearest-parent
                        'shadow)))
             (insert "\n")
             (add-text-properties
              line-start
              (point)
              `(wrap-prefix ,wrap-prefix)))))

(defun org-roam-semantic-search--insert-result (result index)
  "Insert RESULT at one-based INDEX into the current result buffer."
  (let* ((id (alist-get 'id result))
         (title (alist-get 'title result))
         (hierarchy (alist-get 'hierarchy result))
         (score (alist-get 'score result))
         (parent-headings
          (org-roam-semantic-search--parent-headings hierarchy title))
         (start (point))
         (marker (copy-marker (point))))
    (push marker org-roam-semantic-search--result-markers)
    (insert
     (propertize
      (format " %.2f " score)
      'face
      (if org-roam-semantic-search-separate-scores
          `(:inherit org-roam-semantic-search-score
            :box (:line-width (0 . -1)
                  :color ,(face-background 'default nil t)))
        'org-roam-semantic-search-score)))
    (insert "  ")
    (insert-text-button
     title
     'face 'link
     'follow-link t
     'help-echo "Open Org-roam node"
     'node-id id
     'action (lambda (button)
               (org-roam-id-open (button-get button 'node-id) nil)))
    (let ((title-end (point)))
      (insert "\n")
      (let ((details-start (point)))
        (when parent-headings
          (org-roam-semantic-search--insert-hierarchy parent-headings))
        (let ((details-end (point)))
          (when (> details-end details-start)
            (let ((details (make-overlay details-start details-end))
                  (indicator (make-overlay title-end title-end)))
              (overlay-put details 'org-roam-result-index index)
              (overlay-put details 'fold-indicator indicator)
              (push details org-roam-semantic-search--detail-overlays)
              (org-roam-semantic-search--set-details-visible details nil)))
          (add-text-properties start (point)
                               `(org-roam-node-id ,id
                                 org-roam-result-index ,index)))))))

(defun org-roam-semantic-search--render-detailed (payload k)
  "Render structured search PAYLOAD containing up to K results."
  (let* ((buf (get-buffer-create "*org-roam-similar-nodes*"))
         (query (alist-get 'query payload))
         (results (alist-get 'results payload)))
    (with-current-buffer buf
      (remove-overlays)
      (org-roam-semantic-search-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local org-roam-semantic-search--query query)
        (setq-local org-roam-semantic-search--result-count k)
        (setq-local org-roam-semantic-search--view 'detailed)
        (setq-local org-roam-semantic-search--result-markers nil)
        (setq-local org-roam-semantic-search--detail-overlays nil)
        (setq-local org-roam-semantic-search--query-overlay nil)
        (insert (propertize "Semantic search"
                            'face 'org-roam-semantic-search-title)
                "\n")
        (insert (make-string 38 ?─) "\n\n")
        (let ((query-start (point)))
          (insert (propertize (concat " " query)
                              'face 'org-roam-semantic-search-query
                              'org-roam-semantic-search-query t))
          (let ((query-end (point)))
            (when (> (length query)
                     org-roam-semantic-search-query-preview-length)
              (let* ((fold-start
                      (+ query-start
                         1
                         org-roam-semantic-search-query-preview-length))
                     (query-overlay (make-overlay fold-start query-end))
                     (indicator (make-overlay fold-start fold-start)))
                (overlay-put query-overlay 'fold-indicator indicator)
                (setq-local org-roam-semantic-search--query-overlay
                            query-overlay)
                (org-roam-semantic-search--set-query-visible nil))))
          (insert (propertize "\n"
                              'face 'org-roam-semantic-search-query
                              'org-roam-semantic-search-query t))
          (insert "\n"))
        (cl-loop for result in results
                 for index from 1
                 do (org-roam-semantic-search--insert-result result index))
        (setq org-roam-semantic-search--result-markers
              (nreverse org-roam-semantic-search--result-markers))
        (setq org-roam-semantic-search--detail-overlays
              (nreverse org-roam-semantic-search--detail-overlays))
        (goto-char (point-min))
        (when org-roam-semantic-search--result-markers
          (goto-char (marker-position
                      (car org-roam-semantic-search--result-markers))))))
    (pop-to-buffer buf)))

(defun org-roam-semantic-search--show-simple (query k)
  "Display the plain Org search response for QUERY with K results."
  (let ((buf (get-buffer-create "*org-roam-similar-nodes*"))
        (api-output (call-roam-search-python-server query k "org")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert api-output)
        (org-shifttab)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun org-roam-semantic-search-api (&optional k view query)
  "Search Org-roam and display K results using VIEW for QUERY.
VIEW is `detailed' or `simple'.  When QUERY is nil, use the active
region or prompt for it.  The current plain Org interface remains
available with `(org-roam-semantic-search-api nil \\='simple)'."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let* ((query (or query
                    (if (use-region-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (read-string "Enter search: "))))
         (k (or k org-roam-semantic-search-result-count))
         (view (or view org-roam-semantic-search-default-view)))
    (pcase view
      ('simple (org-roam-semantic-search--show-simple query k))
      ('detailed
       (org-roam-semantic-search--render-detailed
        (json-parse-string
         (call-roam-search-python-server query k "json")
         :object-type 'alist
         :array-type 'list)
        k))
      (_ (user-error "Unknown semantic search view: %s" view)))))

(defun org-roam-semantic-search-refresh ()
  "Repeat the search displayed in the current result buffer."
  (interactive)
  (org-roam-semantic-search-api
   org-roam-semantic-search--result-count
   org-roam-semantic-search--view
   org-roam-semantic-search--query))

(defun org-roam-semantic-search-edit-query ()
  "Edit and repeat the current semantic search query."
  (interactive)
  (org-roam-semantic-search-api
   org-roam-semantic-search--result-count
   'detailed
   (read-string "Enter search: " org-roam-semantic-search--query)))

(defun org-roam-semantic-search-toggle-view ()
  "Show the current detailed search using the simple Org view."
  (interactive)
  (org-roam-semantic-search-api
   org-roam-semantic-search--result-count
   'simple
   org-roam-semantic-search--query))

(prefer-coding-system 'utf-8)

;;I change the value of this variable to enable me to run two process in different output buffers
;; without confirming I want to create another buffer
(setq async-shell-command-buffer 'new-buffer)

(defcustom lgm/start-org-roam-ai-services-on-startup t
  "Start org-roam AI Python services automatically after Emacs startup."
  :type 'boolean
  :group 'lgm)

(defun lgm/async-shell-command-no-window (command buffer-name)
  "Run COMMAND asynchronously in BUFFER-NAME without displaying its window."
  (let ((display-buffer-alist
         (cons `(,(regexp-quote buffer-name) (display-buffer-no-window))
               display-buffer-alist)))
    (async-shell-command command buffer-name)))

(defun start-semantic-search ()
  (interactive)
  (lgm/async-shell-command-no-window
   "source ~/.zshrc && conda activate ml3 && cd /Users/luis.moneda/repos/org-roam-ai && python -m core.semantic_search"
   "*org-roam-semantic-search-service*"))

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
  (lgm/async-shell-command-no-window
   "source ~/.zshrc && conda activate ml3 && cd /Users/luis.moneda/repos/org-roam-ai && python -m core.qna"
   "*org-roam-qna-service*"))

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
  (let ((base-colors
		 ;; Higher contrast colors
		 ;; '("#F4C1BD" "#D9E2B8" "#F7E5A6" "#F7D0B4" "#F2C1D6" "#AFC8EB" "#CDC3E5" "#B9E2DE")
		 '("#F4DBD9" "#E7EBD6" "#F6EDCD" "#F6E2D4" "#F3DBE5" "#D2DEF0" "#E1DCED" "#D7EBE9")
					 ))
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
  (lgm/async-shell-command-no-window
   "source ~/.zshrc && conda activate ml3 && cd /Users/luis.moneda/repos/org-roam-ai && python -m core.discourse_segmentation"
   "*org-roam-discourse-segmentation-service*"))

(when lgm/start-org-roam-ai-services-on-startup
  (add-hook 'emacs-startup-hook #'start-semantic-search)
  (add-hook 'emacs-startup-hook #'start-qna)
  (add-hook 'emacs-startup-hook #'start-discourse-segmentation))

(defun discourse-segmentation-api ()
  "Call the given Python SCRIPT-NAME with the given TEXT as input and
display the output in a new temporary buffer."
    (interactive)
	(let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
		   (api-output (my/get-segments text)))
	  api-output))

(setq chunky-semantic-search/segmentation-granularity 0.25)

(defun set-chunky-semantic-search-granularity ()
  "Set the value of `chunky-semantic-search/segmentation-granularity`.
Provides a selection from a predefined list, but also allows custom input."
  (interactive)
  (let* ((choices '(0 0.25 0.5 0.75 0.9 1))
         (choice (completing-read
                  "Select granularity (or type a custom value); 0 generates a single chunk, 1 splits by sentence: "
                  (mapcar #'number-to-string choices)
                  nil nil)))
    (setq chunky-semantic-search/segmentation-granularity
          (min 1 (max 0 (string-to-number choice))))
    (message "Granularity set to %f" chunky-semantic-search/segmentation-granularity)))

(defun my/get-segments (query)
  "Send QUERY to the Python HTTP server and return the segments as an Emacs Lisp list."
  (require 'url)
  (require 'json)
  (let* ((encoded-query (url-hexify-string query))  ;; Encode special characters
         (url (format "http://localhost:8866/api/segment?text=%s&granularity=%s"
                      encoded-query
                      (number-to-string chunky-semantic-search/segmentation-granularity)))
         (response-buffer (url-retrieve-synchronously url))
         segments)
    (if response-buffer
        (with-current-buffer response-buffer
          (goto-char (point-min))
          ;; (message "Raw response: %s" (buffer-string))  ;; 🔍 DEBUG: Print raw response
          (if (search-forward "\n\n" nil t)
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (parsed-json (condition-case err
                                      (json-read)
                                    (error (message "JSON read error: %s" err)
                                           nil))))
                (setq segments (cdr (assoc "segments" parsed-json))))
            (message "Failed to locate JSON body in response"))
          (kill-buffer response-buffer))
      (message "No response received"))
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
;; (require 'consult)

;; This function keeps the menu. It is flaky due to the re-openning
(defun my/display-popup-at-point ()
  "Display search results using consult for the chunk at point, without closing the menu after selection."
  (interactive)
  (let ((ov (car (overlays-at (point)))))
    (when ov
      (let ((results (overlay-get ov 'my-results))
            (ivy-height 16)
            (ivy-posframe-height 16)
            (ivy-posframe-width 100) ;; Adjust width
            (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point))))
        (when results
          (let* ((titles (mapcar (lambda (s) (decode-coding-string s 'utf-8)) (mapcar #'car results)))
                 (last-selection nil)) ;; Store the last selection
            (catch 'exit
              (while t
                (let* ((selected (decode-coding-string (consult--read
                                                         titles
                                                         :prompt "Select entry: "
                                                         :category 'org-roam
                                                         :require-match t
                                                         :sort nil
                                                         :history t
                                                         :default last-selection) 'utf-8))) ;; Decode user selection
                  (if selected
                      (progn
                        (setq last-selection selected) ;; Update last selection
                        (let ((entry (assoc selected (mapcar (lambda (r)
                                                               (cons (decode-coding-string (car r) 'utf-8) (cdr r)))
                                                             results)))) ;; Match correctly
                          (when entry
                            (my/open-org-roam-node-in-side-buffer entry))))
                    ;; Exit the menu if no selection is made
                    (throw 'exit nil)))))))))))

;; (global-set-key (kbd "C-c e") 'my/display-popup-at-point)

;; To clean the faces if I'm writing
(defun chunky-semantic-search/clear-faces (&optional start end)
  "Remove all face overlays applied by `chunky-semantic-search` from the selection or entire buffer."
  (interactive (if (use-region-p) (list (region-beginning) (region-end)) (list nil nil)))
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (dolist (ov (overlays-in start end))
      (when (overlay-get ov 'face)  ;; Only remove overlays that set a face
        (delete-overlay ov))))
  (message "Chunky Semantic Search highlights cleared."))


;; Textual topography
(defun textual-topography ()
  "Prompt for domain, characteristic/anti-characteristic pair, and selected text.
   Save selected text to a file and call a Python script with the arguments."
  (interactive)

  ;; Define the domains and their associated characteristic/anti-characteristic pairs
  (setq domain-characteristics
        '(("Mood" . (("Happy" . "Unhappy") ("Calm" . "Agitated") ("Optimistic" . "Pessimist")))
          ("Style" . (("Complex" . "Simple") ("Specific" . "Generic")))
          ("Personality" . (("Outgoing" . "Shy") ("Confident" . "Timid")))))

  ;; Prompt user to select a domain
  (setq domain (completing-read "Select a domain: " (mapcar 'car domain-characteristics)))

  ;; Retrieve the list of characteristic/anti-characteristic pairs for the selected domain
  (setq char-anti-char-pairs (cdr (assoc domain domain-characteristics)))

  ;; Prompt user to select a characteristic/anti-characteristic pair
  (setq selected-pair (completing-read "Select a characteristic/anti-characteristic: "
                                      (mapcar (lambda (pair) (concat (car pair) " / " (cdr pair))) char-anti-char-pairs)))

  ;; Split the selected pair into characteristic and anti-characteristic
  (let ((selected-pair-split (assoc selected-pair (mapcar (lambda (pair)
                                                           (cons (concat (car pair) " / " (cdr pair)) pair))
                                                         char-anti-char-pairs))))
    (setq characteristic (car (cdr selected-pair-split)))
    (setq anti-characteristic (cdr (cdr selected-pair-split))))

  ;; Ask for the selected text region
  (if (use-region-p)
      (setq selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
    (setq selected-text (read-string "Enter text manually: ")))

  ;; Define the file location outside of the let block to ensure it's accessible later
  (setq file-location (concat "~/Documents/" domain "-" characteristic "-" anti-characteristic ".txt"))

  ;; Save the selected text to a file in ~/Documents/
  (with-temp-file file-location
    (insert selected-text))
  (message "Text saved to %s" file-location)

  ;; Call the Python script with the four arguments
  (let ((python-script "~/repos/org-roam-ai/writing/textual_topography.py"))
	(shell-command (format "source ~/.zshrc && conda activate ml3 && python3 %s %s %s %s %s"
                           python-script domain characteristic anti-characteristic file-location)))

  ;; Create the org-mode buffer with an image and link
  (let ((svg-file (concat "~/Documents/minimalistic_" domain "_" characteristic ".svg"))
        (html-file (concat "~/Documents/" domain "_" characteristic ".html"))
		(html-file-granular (concat "~/Documents/" domain "_" characteristic "_wo_moving_average.html"))
        (org-buffer (generate-new-buffer "*Domain Characteristics*")))
    (switch-to-buffer org-buffer)
    (org-mode)

	;; Optionally, add a heading
    (insert (format "#+TITLE: Textual Topography: %s - %s / %s\n" domain characteristic anti-characteristic))
    (insert "\n")

    ;; Insert the image using org-mode syntax
    (insert (format "[[file:%s]]\n" svg-file svg-file))
    (insert "\n")

    ;; Insert the link to the HTML file
    (insert (format "[[file:%s][Interactive plot]]\n" html-file html-file))
    (insert "\n")

    ;; Insert the link to the HTML file with granular scores
    (insert (format "[[file:%s][Interactive without the moving average]]\n" html-file-granular html-file-granular))

	(org-toggle-inline-images)

	(message "Org buffer created with image and link.")))


;;gpt.el
;; (use-package gptel
;;   :straight t
;;   :init
;;   (setq gptel-api-key (getenv "OPENAI_API_KEY"))
;;   (gptel-make-anthropic "Claude" :stream t :key (getenv "ANTHROPIC_API_KEY"))
;;   )

(provide 'gpt-settings)
;;; gpt-settings.el ends here
