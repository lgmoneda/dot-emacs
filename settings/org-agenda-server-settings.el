(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'htmlize)
(require 'simple-httpd)
(setq httpd-host "0.0.0.0")
(setq httpd-port 8080) ;; change if you like

;; Store for capture feedback messages
(defvar my/capture-feedback-message nil
  "Store feedback message for last capture attempt.")

;; Compact only day view for remote access
;; Custom agenda command: today's scheduled tasks, compact format, no late tasks
(add-to-list 'org-agenda-custom-commands
             '("r" "Compact today remote"
               agenda ""
               ((org-agenda-span 'day)
                (org-agenda-start-day "+0d")
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-scheduled-delay-if-deadline nil)
                (org-agenda-skip-scheduled-if-deadline-is-shown nil)
                (org-agenda-skip-timestamp-if-deadline-is-shown nil)
                (org-scheduled-past-days 0)  ;; Don't show past scheduled items
                (org-agenda-use-time-grid nil)
                (org-agenda-remove-tags t)
                (org-agenda-remove-priority t)
                (org-agenda-prefix-format '((agenda . " %s")))
                (org-agenda-scheduled-leaders '("" ""))  ;; No "Sched.Nx:" prefix
                (org-agenda-overriding-header "Today Agenda:"))))

(defun my/org-agenda-today-html ()
  "Return today's agenda as a full HTML page string with capture form, styled for small screens."
  (let* ((temp-agenda-buffer " *temp-agenda-html*")
         (existing-agenda-buffer (get-buffer "*Org Agenda*"))
         (agenda-visible-window (when existing-agenda-buffer
                                  (get-buffer-window existing-agenda-buffer)))
         htmlized-css
         htmlized-body
         (feedback-html (if my/capture-feedback-message
                           (format "<div id='feedback' class='feedback'>%s</div>"
                                   my/capture-feedback-message)
                         "")))

    ;; Clear feedback after displaying
    (setq my/capture-feedback-message nil)

    (if agenda-visible-window
        ;; If agenda is already visible, use its current contents
        (with-current-buffer existing-agenda-buffer
          (let ((htmlbuf (htmlize-buffer)))
            (with-current-buffer htmlbuf
              ;; Extract CSS and body content
              (goto-char (point-min))
              (when (re-search-forward "<style[^>]*>\\(\\(.\\|\n\\)*?\\)</style>" nil t)
                (setq htmlized-css (match-string 1)))
              (goto-char (point-min))
              (when (re-search-forward "<pre>\\(\\(.\\|\n\\)*?\\)</pre>" nil t)
                (setq htmlized-body (match-string 1))))
            (kill-buffer htmlbuf)))

      ;; Otherwise, create agenda in temporary buffer
      (save-window-excursion
        (let ((temp-buffer (get-buffer-create temp-agenda-buffer)))
          (with-current-buffer temp-buffer
            (let ((org-agenda-sticky nil)  ; Disable sticky agendas
                  (inhibit-redisplay t))   ; Prevent visual updates
              (org-agenda nil "r")

              ;; Now htmlize the current buffer content
              (let ((htmlbuf (htmlize-buffer)))
                (with-current-buffer htmlbuf
                  ;; Extract CSS styles from htmlize output
                  (goto-char (point-min))
                  (when (re-search-forward "<style[^>]*>\\(\\(.\\|\n\\)*?\\)</style>" nil t)
                    (setq htmlized-css (match-string 1)))
                  ;; Extract body content (everything inside <pre>...</pre>)
                  (goto-char (point-min))
                  (when (re-search-forward "<pre>\\(\\(.\\|\n\\)*?\\)</pre>" nil t)
                    (setq htmlized-body (match-string 1))))
                (kill-buffer htmlbuf)))

            ;; Clean up the temporary buffer
            (kill-buffer temp-buffer)))))

    ;; Build the final HTML with our custom CSS and the extracted content
    (format "<!DOCTYPE html>
<html>
<head>
<meta charset='UTF-8'>
<meta name='viewport' content='width=device-width, initial-scale=1.0'>
<title>Emacs Agenda</title>
<style>
  /* Base styles */
  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    margin: 0.8em;
    padding: 0;
    background: #F5F5F5;
    color: #2e3338;
    font-size: 18px;
    line-height: 1.5;
  }

  /* Prevent line wrapping - each task on single line */
  pre {
    white-space: pre;     /* Preserve spaces and prevent wrapping */
    overflow-x: auto;     /* Allow horizontal scroll if needed */
    word-wrap: normal;    /* Disable word wrapping */
    font-family: monospace;
    margin: 0;
    padding: 0;
  }

  /* Htmlize-generated styles */
  %s

  /* Override some htmlize styles for better mobile display */
  .org-agenda-structure {
    font-size: 1.3em;
    margin-top: 0.5em;
    margin-bottom: 0.3em;
    display: block;
    border-bottom: 2px solid #444;
    padding-bottom: 0.2em;
  }

  .org-agenda-date-weekend-today,
  .org-agenda-date-today,
  .org-agenda-date-weekend,
  .org-agenda-date {
    font-weight: bold;
    font-size: 1.1em;
    display: block;
    margin-top: 0.5em;
    margin-bottom: 0.3em;
  }

  /* Ensure each line stays on one line */
  pre > * {
    white-space: nowrap;
    display: inline;
  }

  /* Make sure spans don't break */
  span {
    white-space: nowrap;
    display: inline;
  }

  .org-agenda-done {
     color: #000000;
     background-color: #e5e5e5;
  }

  /* Capture form styles */
  .capture-form {
    margin-top: 2em;
    padding: 1em;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }

  .capture-form h3 {
    margin-top: 0;
    color: #333;
    font-size: 1.2em;
  }

  .capture-form textarea {
    width: 100%%;
    box-sizing: border-box;
    padding: 0.5em;
    font-size: 16px;
    border: 1px solid #ddd;
    border-radius: 4px;
    resize: vertical;
    min-height: 80px;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  }

  .capture-form button {
    margin-top: 0.5em;
    padding: 0.6em 1.5em;
    font-size: 16px;
    background: #007AFF;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-weight: 500;
  }

  .capture-form button:hover {
    background: #0051D5;
  }

  .capture-form button:active {
    transform: translateY(1px);
  }

  .feedback {
    margin-top: 0.5em;
    padding: 0.5em;
    border-radius: 4px;
    animation: fadeIn 0.3s ease-in;
  }

  .feedback.success {
    background: #d4edda;
    color: #155724;
    border: 1px solid #c3e6cb;
  }

  .feedback.error {
    background: #f8d7da;
    color: #721c24;
    border: 1px solid #f5c6cb;
  }

  @keyframes fadeIn {
    from { opacity: 0; }
    to { opacity: 1; }
  }

  /* Responsive handling */
  @media (max-width: 768px) {
    body {
      margin: 0.5em;
      font-size: 16px;
    }
    pre {
      font-size: 14px;
    }
  }
</style>
<script>
function submitCapture() {
  const textarea = document.getElementById('capture-text');
  const text = textarea.value.trim();
  const todayChecked = document.getElementById('capture-today').checked;
  const feedback = document.getElementById('feedback');

  if (!text) {
    feedback.textContent = 'Please enter some text';
    feedback.className = 'feedback error';
    return;
  }

  // Show loading state
  const button = document.querySelector('.capture-form button');
  button.disabled = true;
  button.textContent = 'Sending...';

  // Send POST request with today flag
  fetch('/capture', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
    },
    body: 'text=' + encodeURIComponent(text) + '&today=' + (todayChecked ? '1' : '0')
  })
  .then(response => response.text())
  .then(data => {
    // Clear the textarea and checkbox
    textarea.value = '';
    document.getElementById('capture-today').checked = false;

    // Show success feedback
    feedback.textContent = 'Captured successfully!';
    feedback.className = 'feedback success';

    // Reset button
    button.disabled = false;
    button.textContent = 'Send to Org Capture';

    // Reload page after a delay to show updated agenda
    setTimeout(() => {
      window.location.reload();
    }, 1500);
  })
  .catch(error => {
    // Show error feedback
    feedback.textContent = 'Error: ' + error.message;
    feedback.className = 'feedback error';

    // Reset button
    button.disabled = false;
    button.textContent = 'Send to Org Capture';
  });
}

// Allow Enter+Ctrl/Cmd to submit
document.addEventListener('DOMContentLoaded', function() {
  const textarea = document.getElementById('capture-text');
  textarea.addEventListener('keydown', function(e) {
    if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
      e.preventDefault();
      submitCapture();
    }
  });
});
</script>
</head>
<body>
<pre>%s</pre>

<div class='capture-form'>
  <h3>Quick Capture</h3>
  <label style=\"display:block; margin-top:0.5em;\">
    <input type=\"checkbox\" id=\"capture-today\"> Schedule for today
  </label>
  <textarea id='capture-text' placeholder='Enter task or note to capture...'></textarea>
  <button onclick='submitCapture()'>Send to Org Capture</button>
  <div id='feedback'></div>
  %s
</div>

</body>
</html>"
            (or htmlized-css "")
            (or htmlized-body "No agenda content available")
            feedback-html)))

(defun httpd/agenda (proc path query req)
  "HTTP handler for /agenda."
  (with-httpd-buffer proc "text/html"
    (insert (my/org-agenda-today-html))))

(defun my/parse-post-data (data)
  "Parse URL-encoded POST data."
  (let ((pairs (split-string data "&"))
        result)
    (dolist (pair pairs)
      (let ((kv (split-string pair "=")))
        (when (= (length kv) 2)
          (push (cons (url-unhex-string (car kv))
                      (url-unhex-string (cadr kv)))
                result))))
    result))

(defun httpd/capture (proc path query req)
  "HTTP handler for /capture - handles POST requests for org-capture."
  (let* ((headers (cdr req))  ; Skip the first element which is the request line
         (content-header (assoc "Content" headers))
         (post-data-raw (cdr content-header))
         ;; Extract the string from the list if it's a list
         (post-data (if (listp post-data-raw)
                        (car post-data-raw)
                      post-data-raw)))

    ;; Debug output
    (message "REQ DEBUG: %S" req)
    (message "POST DATA RAW: %S" post-data-raw)
    (message "POST DATA: %S" post-data)

    (if (and (string= (caar req) "POST") post-data)
        (let* ((parsed-data (my/parse-post-data post-data))
               (text (cdr (assoc "text" parsed-data)))
			   (today-flag (cdr (assoc "today" parsed-data)))
               )

          (message "PARSED DATA: %S" parsed-data)
          (message "PARSED TEXT: %S" text)
          (message "TODAY FLAG: %S" today-flag)

          (if text
              (progn
                ;; Perform the capture
                (condition-case err
                    (progn

					  (if today-flag
						  (org-capture-string text "td")
						(org-capture-string text "D"))
                      ;; Use org-capture-string which is simpler and more reliable


                      ;; Set success message
                      (setq my/capture-feedback-message
                            (format "✓ Captured%s: \"%s\""
                                    (if (and today-flag (string= today-flag "1"))
                                        " (scheduled today)" "")
                                    (if (> (length text) 50)
                                        (concat (substring text 0 50) "...")
                                      text)))

                      (message "CAPTURE SUCCESS: %s" text)

                      ;; Send success response
                      (with-httpd-buffer proc "text/plain"
                        (insert "OK")))

                  (error
                   ;; Set error message
                   (setq my/capture-feedback-message
                         (format "✗ Capture failed: %s" (error-message-string err)))

                   (message "CAPTURE ERROR: %s" (error-message-string err))

                   ;; Send error response
                   (with-httpd-buffer proc "text/plain"
                     (insert (format "Error: %s" (error-message-string err))))))

              ;; No text provided
              (progn
                (message "NO TEXT PROVIDED")
                (with-httpd-buffer proc "text/plain"
                  (insert "Error: No text provided")))))

      ;; Not a POST request or no data
      (progn
        (message "INVALID REQUEST - Method: %s, Data: %s" (caar req) post-data)
        (with-httpd-buffer proc "text/plain"
          (insert "Error: Invalid request")))))))

(defun my/get-mac-ip ()
  "Return the current Mac LAN IP address (for en0 or en1)."
  (or (string-trim (shell-command-to-string "ipconfig getifaddr en0"))
      (string-trim (shell-command-to-string "ipconfig getifaddr en1"))
      "127.0.0.1"))

(defun my/start-agenda-server ()
  "Start the agenda server and show the LAN URL."
  (interactive)
  (httpd-start)
  (let ((ip (my/get-mac-ip)))
    (message "Agenda with capture available at: http://%s:%d/agenda" ip httpd-port)))

(add-hook 'emacs-startup-hook #'my/start-agenda-server)

(provide 'org-agenda-server-settings)
;;; org-agenda-server-settings.el ends here
