(require 'org)
(require 'org-agenda)
(require 'htmlize)
(require 'simple-httpd)
(setq httpd-host "0.0.0.0")
(setq httpd-port 8080) ;; change if you like

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
		(org-agenda-remove-priority-from-line t)  ;; Remove priority indicators
                (org-agenda-prefix-format '((agenda . " %s")))
                (org-agenda-scheduled-leaders '("" ""))  ;; No "Sched.Nx:" prefix
                (org-agenda-overriding-header "Today Agenda:"))))

(defun my/org-agenda-today-html ()
  "Return today's agenda as a full HTML page string, styled for small screens."
  (let* ((temp-agenda-buffer " *temp-agenda-html*")
         (existing-agenda-buffer (get-buffer "*Org Agenda*"))
         (agenda-visible-window (when existing-agenda-buffer 
                                  (get-buffer-window existing-agenda-buffer)))
         htmlized-css
         htmlized-body)
    
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
</head>
<body>
<pre>%s</pre>
</body>
</html>"
            (or htmlized-css "")
            (or htmlized-body "No agenda content available"))))

(defun httpd/agenda (proc path query req)
  "HTTP handler for /agenda."
  (with-httpd-buffer proc "text/html"
    (insert (my/org-agenda-today-html))))

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
    (message "Agenda available at: http://%s:%d/agenda" ip httpd-port)))

(add-hook 'emacs-startup-hook #'my/start-agenda-server)

(provide 'org-agenda-server-settings)
;;; org-agenda-server-settings.el ends here
