(defun org-roam-node-content ()
  "Get the content of the current org-roam node (excluding sub-nodes)."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let ((content ""))
        ;; Skip the heading
        (forward-line 1)
        ;; Skip any properties drawer
        (when (looking-at-p "[ \t]*:PROPERTIES:")
          (re-search-forward ":END:" nil t)
          (forward-line 1))
        ;; Collect content until the first subheading or end of buffer
        (let ((start (point))
              (end (or (and (re-search-forward "^\\*" nil t)
                            (match-beginning 0))
                       (point-max))))
          (buffer-substring-no-properties start end))))))

(defun org-roam-current-node-id ()
  "Get the ID of the current org-roam node."
  (if (fboundp 'org-roam-node-at-point)
      (when-let ((node (org-roam-node-at-point)))
        (org-roam-node-id node))
    (org-id-get)))

(defun org-roam-link-recommendations-api (&optional k)
  "Call the link recommendation API with text as input and display the results.
If region is active, use the selected text as query.
If called inside an org-roam node, use the node's content and ID.
Otherwise, prompt for input text.
Optional argument K specifies the number of initial candidates to retrieve."
  (interactive "P")
  (let* ((k-value (or k 100)) ;; Default to 100 if not provided
         (buf (get-buffer-create "*org-roam-link-recommendations*"))
         api-output)

    ;; Determine the input method based on context
    (cond
     ;; Case 1: Region is selected - use the selected text
     ((use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (setq api-output (call-link-rec-python-server text k-value))))

     ;; Case 2: Inside an org-roam node - use node content and ID
     ((and (derived-mode-p 'org-mode)
           (fboundp 'org-roam-current-node-id)
           (org-roam-current-node-id))
      (let ((node-content (org-roam-node-content))
            (node-id (org-roam-current-node-id)))
        (setq api-output (call-link-rec-python-server-with-node-id node-content node-id k-value))))

     ;; Case 3: Neither region nor org-roam node - prompt for input
     (t
      (let ((text (read-string "Enter search: ")))
        (setq api-output (call-link-rec-python-server text k-value)))))

    ;; Display the results
    (with-current-buffer buf
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8)
      (org-mode)
      (insert (decode-coding-string api-output 'utf-8))
      (org-shifttab 2) ;; Show level 2 headings (class headings)
      ;; Enable org-link-minor-mode for clickable links
      (when (fboundp 'org-link-minor-mode)
        (org-link-minor-mode 1))
      (display-buffer buf))))

(defun call-link-rec-python-server (input-string &optional k)
  "Call link recommendation Python server with INPUT-STRING and return the output.
Optional argument K specifies the number of initial candidates to retrieve."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (k-param (if k (format "?k=%d" k) "")))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://localhost:8801/api/"
                 (url-hexify-string input-string)
                 k-param))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(defun call-link-rec-python-server-with-node-id (input-string node-id &optional k)
  "Call link recommendation Python server with INPUT-STRING and NODE-ID.
Optional argument K specifies the number of initial candidates to retrieve."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (k-param (if k (format "?k=%d" k) "?k=100"))
        (node-id-param (if node-id (format "&node_id=%s" node-id) "")))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://localhost:8801/api/"
                 (url-hexify-string input-string)
                 k-param
                 node-id-param))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

;; I'm defining this function twice in my files...
(defun org-roam-semantic-search-api (&optional k)
  "Call the semantic search API with text as input and display the results.
If region is active, use the selected text as query.
Otherwise, prompt for input text.
Optional argument K specifies the number of initial candidates to retrieve."
  (interactive "P")
  (let* ((k-value (or k 30)) ;; Default to 30 if not provided
         (buf (get-buffer-create "*org-roam-similar-nodes*"))
         api-output)

    ;; Determine the input method based on context
    (cond
     ;; Case 1: Region is selected - use the selected text
     ((use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (setq api-output (call-roam-search-python-server text k-value))))

     ;; Case 3: Neither region nor org-roam node - prompt for input
     (t
      (let ((text (read-string "Enter search: ")))
        (setq api-output (call-roam-search-python-server text k-value)))))

    ;; Display the results
    (with-current-buffer buf
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8)
      (org-mode)
      (insert (decode-coding-string api-output 'utf-8))
      (org-shifttab 2) ;; Show level 2 headings (class headings)
      (goto-char (point-min)) ;; Move to beginning of buffer
      (org-next-visible-heading 1) ;; Move to first heading
      (display-buffer buf)))
  )


(defun call-roam-search-python-server (input-string &optional k)
  "Call semantic search Python server with INPUT-STRING and return the output.
Optional argument K specifies the number of initial candidates to retrieve."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (k-param (if k (format "?k=%d" k) "")))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://localhost:8800/api/"
                 (url-hexify-string input-string)
                 k-param))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))

(defun call-roam-search-python-server-with-node-id (input-string node-id &optional k)
  "Call semantic search Python server with INPUT-STRING and NODE-ID.
Optional argument K specifies the number of initial candidates to retrieve."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         '(("Content-Type" . "text/plain")))
        (k-param (if k (format "?k=%d" k) "?k=30"))
        (node-id-param (if node-id (format "&node_id=%s" node-id) "")))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://localhost:8800/api/"
                 (url-hexify-string input-string)
                 k-param
                 node-id-param))
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (buffer-substring (point) (point-max)))))


(defun my/get-drawer-content (drawer-name)
  "Get content of a drawer with DRAWER-NAME in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((drawer-regex (format ":%s:\\(\\(?:.\\|\n\\)*?\\)\\(:END:\\|\\'\\)" drawer-name))
          content)
      (when (re-search-forward drawer-regex nil t)
        (setq content (match-string 1))
        (string-trim content)))))

(defun org-roam-link-rec-accept-recommendation (query-node-id recommended-node-id link-type score &optional feedback-type)
  "Send feedback to the link recommendation server that the user accepted a recommendation.
Records the datetime, query node ID, recommended node ID, link type, and score in a CSV file for future model training.
FEEDBACK-TYPE can be 'accept' or 'correct'."
  (interactive)
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          '(("Content-Type" . "text/plain")))
         ;; Get query content from the recommendations buffer using the drawer function
         (query-content (with-current-buffer "*org-roam-link-recommendations*"
                          (my/get-drawer-content "QUERY")))
         (feedback-type (or feedback-type "accept"))
         (feedback-url (format "http://localhost:8801/feedback/?query_node_id=%s&query_content=%s&recommended_node_id=%s&link_type=%s&score=%s&feedback_type=%s"
                               (url-hexify-string (or query-node-id ""))
                               (url-hexify-string (or query-content ""))
                               (url-hexify-string (or recommended-node-id ""))
                               (url-hexify-string (or link-type ""))
                               (url-hexify-string (or score "0.0"))
                               (url-hexify-string feedback-type))))
    ;; Send the feedback
    (with-current-buffer
        (url-retrieve-synchronously feedback-url)
      (goto-char (point-min))
      (search-forward-regexp "\n\n")
      (let ((response (buffer-substring (point) (point-max))))
        (message "Feedback sent: %s" response)))

    ;; Create the link in the current buffer if we're in an org-roam node
    (when (and (derived-mode-p 'org-mode)
               (fboundp 'org-roam-current-node-id)
               (org-roam-current-node-id)
               (not (string-empty-p query-node-id)))
      (save-excursion
        (goto-char (point-max))
        ;; Get the title from the recommendations buffer
        (let ((title (with-current-buffer "*org-roam-link-recommendations*"
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward (format "\\*\\* \\[\\[id:%s\\]\\[\\(.*?\\)\\]\\]"
                                                          (regexp-quote recommended-node-id)) nil t)
                           (match-string 1))))))
          (insert (format "\n** [[id:%s][%s]]\n"
                          recommended-node-id
                          (or title recommended-node-id)))
          (message "Added link to current node"))))))

;; Set up custom org link type for recommendation feedback
(org-link-set-parameters
 "rec-feedback"
 :follow (lambda (path)
           (let* ((parts (split-string path "|"))
                  (query-node-id (nth 0 parts))
                  (recommended-node-id (nth 1 parts))
                  (link-type (nth 2 parts))
                  (score (nth 3 parts))
                  (feedback-type (nth 4 parts)))
             (message "Processing feedback: node=%s, rec=%s, type=%s"
                      query-node-id recommended-node-id link-type)
             (org-roam-link-rec-accept-recommendation
              query-node-id recommended-node-id link-type score feedback-type)))
 :help-echo "Accept this link recommendation and save feedback")

;; Set up custom org link type for correcting link type
(org-link-set-parameters
 "rec-feedback-correct"
 :follow (lambda (path)
           (let* ((parts (split-string path "|"))
                  (query-node-id (nth 0 parts))
                  (recommended-node-id (nth 1 parts))
                  (score (nth 2 parts))
                  (link-types '("Compositional" "Referential" "Comparison"))
                  (selected-type (completing-read "Select correct link type: " link-types nil t)))
             (message "Correcting link type to %s for node %s"
                      selected-type recommended-node-id)
             (org-roam-link-rec-accept-recommendation
              query-node-id recommended-node-id selected-type score "correct")))
 :help-echo "Correct the link type for this recommendation")

(defun start-link-type-rec-sys ()
  (interactive)
  (async-shell-command "source ~/.zshrc && conda activate ml3 && cd /Users/luis.moneda/repos/org-roam-ai && python -m core.link_rec_sys")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<6>"))))

(start-link-type-rec-sys)

(defun start-link-type-classifier ()
  (interactive)
  (async-shell-command "source ~/.zshrc && conda activate ml3 && cd /Users/luis.moneda/repos/org-roam-ai && python -m classifiers.link_type_model_serving --host 0.0.0.0 --port 8000")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<7>"))))

(start-link-type-classifier)

;; Provide the package
(provide 'org-roam-link-recommendations-settings)
