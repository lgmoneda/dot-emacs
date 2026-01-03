;;; media-settings.el --- Settings for media (audio, video)

(use-package smudge
  :quelpa ((smudge :fetcher git :url "https://github.com/danielfm/smudge") :upgrade t)
  :config
  (setq smudge-transport 'connect)
  (define-key smudge-command-map (kbd "l") #'lgm/smudge-lyrics-popup)
  ;; (define-key smudge-mode-map (kbd "C-c .") smudge-command-map)
  :bind-keymap ("C-c ." . smudge-command-map)
  :custom
  (smudge-oauth2-client-id (getenv "SMUDGE_CLIENT_ID"))
  (smudge-oauth2-client-secret (getenv "SMUDGE_CLIENT_SECRET"))
  (smudge-player-use-transient-map t)
  )

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players))

(use-package org-emms
  :ensure t)

;; To record voice or when I want to play something quickly and remind it.
(defun my/list-sounddevice-audio-devices ()
  "Return a list of audio input devices using Python's sounddevice module."
  (let* ((python-exe (expand-file-name "~/miniconda3/envs/edge/bin/python"))
         (py-code "
import sounddevice as sd
for i, d in enumerate(sd.query_devices()):
    if d['max_input_channels'] > 0:
        print(f'{i}: {d[\"name\"]}')
")
         ;; Use shell-quote-argument to safely handle quoting
         (cmd (format "%s -c %s"
                      (shell-quote-argument python-exe)
                      (shell-quote-argument py-code)))
         (output (shell-command-to-string cmd))
         (lines (split-string output "\n" t))) ; t = omit empty lines
    ;; Parse and return (name . index) pairs
    (cl-loop for line in lines
             when (string-match "^\\([0-9]+\\): \\(.*\\)$" line)
             collect (cons (match-string 2 line)
                           (match-string 1 line)))))

(defun my/audio-note ()
  (interactive)
  (let* ((python-script (expand-file-name "~/.emacs.d/audio_recorder.py"))
         (output-dir "~/Dropbox/Agenda/roam/audio-notes/")
         (_ (unless (file-directory-p output-dir)
              (make-directory output-dir t)))
         ;; Get list of audio devices and prompt user to select one
         (devices (my/list-sounddevice-audio-devices))
         (selected-device-name (completing-read "Select audio input device: "
                                              (mapcar #'car devices)))
         (device (cdr (assoc selected-device-name devices)))
         ;; Added emoji to filename
         (filename (format-time-string "%Y%m%d-%H%M%S.ogg"))
         (fullpath (expand-file-name filename output-dir))
         (buffer "*Audio Note Recording*")
         ;; Command to run Python with conda environment
         (conda-python-cmd "~/miniconda3/envs/edge/bin/python"))

    ;; Debug output
    (message "Python script path: %s" python-script)
    (message "Output file path: %s" fullpath)
    (message "Selected device: %s (index: %s)" selected-device-name device)

    ;; Create and display buffer
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
	  ;; I don't need to see it
      ;; (display-buffer buffer)
	  )

    ;; Start the process with conda Python
    (let ((proc (start-process "audio-note" buffer
                               (expand-file-name conda-python-cmd)
                               python-script
                               device  ; Use the selected device index
                               fullpath)))

      ;; Make the process accept input
      (set-process-query-on-exit-flag proc nil)

      ;; Print helpful instructions
      (message "Recording from device '%s'. Press ENTER in minibuffer to stop."
               selected-device-name)

      ;; Wait for user input to stop recording
      (read-string "Press ENTER to stop recording: ")

      ;; Send ENTER to the Python process and wait for it to finish
      (process-send-string proc "\n")

      ;; Wait for the process to finish (with timeout)
      (let ((max-wait 5)  ; maximum wait time in seconds
            (waited 0))
        (while (and (process-live-p proc) (< waited max-wait))
          (sit-for 0.1)
          (setq waited (+ waited 0.1))))

      ;; Check the output
      (with-current-buffer buffer
        (goto-char (point-min))
        (message "Buffer content: %s" (buffer-string))  ; Debug message

        (message "")  ;; clear echo area

        (if (re-search-forward "DURATION:\\([0-9.]+\\)" nil t)
            (let* ((duration (string-to-number (match-string 1)))
                   (desc (read-string "Description: "))
                   (org-link (format "[[file:%s][🎤 %s (%.1fs)]]"
                                    fullpath
                                    desc duration)))
              (kill-new org-link)
              (message "Org link copied to clipboard: %s" org-link))
          (message "Recording failed or duration not found. Check the *Audio Note Recording* buffer."))))))

;; Smudge → LRCLIB (async) → read-only lyrics buffer
(with-eval-after-load 'smudge-controller
  (require 'url)
  (require 'json)
  (require 'subr-x)

  (defgroup my-smudge-lyrics nil
    "Fetch lyrics for the current Smudge track."
    :group 'smudge)

  (defcustom my-smudge-lyrics-auto-popup nil
    "When non-nil, fetch lyrics automatically on track change."
    :type 'boolean
    :group 'my-smudge-lyrics)

  (defcustom my-smudge-lyrics-service-url
    "https://lrclib.net/api/get"
    "Base URL for LRCLIB lyrics."
    :type 'string
    :group 'my-smudge-lyrics)

  (defcustom my-smudge-lyrics-timeout 15
    "Timeout in seconds for lyrics requests."
    :type 'integer
    :group 'my-smudge-lyrics)

  (defcustom my-smudge-lyrics-debug nil
    "When non-nil, log a snippet of the raw response body."
    :type 'boolean
    :group 'my-smudge-lyrics)

  (defvar my-smudge-lyrics--last-track nil)
  (defvar my-smudge-lyrics--pending-token nil)
  (defvar my-smudge-lyrics--manual-pending nil)
  (defvar my-smudge-lyrics--manual-timer nil)

  (defvar my-smudge-lyrics-buffer-name "*Smudge Lyrics*")

  (defvar my-smudge-lyrics-mode-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map special-mode-map)
      (define-key map (kbd "q") #'kill-buffer-and-window)
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      map))

  (define-derived-mode my-smudge-lyrics-mode special-mode "Smudge-Lyrics"
    "Major mode for displaying Smudge lyrics."
    (setq buffer-read-only t)
    (setq-local buffer-offer-save nil)
    (setq-local truncate-lines nil))

  (defun my-smudge-lyrics--clean (s)
    (let ((s (string-trim s)))
      (setq s (replace-regexp-in-string " *[(\\[].*?[])]" "" s))
      (setq s (replace-regexp-in-string " *- *.*$" "" s))
      (setq s (replace-regexp-in-string " *feat\\..*$" "" s))
      (setq s (replace-regexp-in-string " *ft\\..*$" "" s))
      (string-trim s)))

  (defun my-smudge-lyrics--current-track ()
    (when (hash-table-p smudge-controller-player-metadata)
      (let ((artist (gethash "artist" smudge-controller-player-metadata))
            (title  (gethash "name" smudge-controller-player-metadata))
            (state  (gethash "player_state" smudge-controller-player-metadata))
            (dur-ms (gethash "duration" smudge-controller-player-metadata)))
        (when (and artist title (string= state "playing"))
          (list :artist artist
                :title title
                :duration (when (numberp dur-ms)
                            (max 1 (round (/ dur-ms 1000.0)))))))))

  (defun my-smudge-lyrics--lrclib-url (artist title &optional duration)
    (let* ((params `(("artist_name" . ,artist)
                     ("track_name"  . ,title)))
           (params (if duration
                       (append params `(("duration" . ,(number-to-string duration))))
                     params)))
      (concat my-smudge-lyrics-service-url "?"
              (mapconcat (lambda (kv)
                           (format "%s=%s" (car kv) (url-hexify-string (cdr kv))))
                         params "&"))))

  (defun my-smudge-lyrics--parse-json-body (body)
    (when (and body (not (string-empty-p (string-trim body))))
      (condition-case nil
          (json-parse-string body :object-type 'hash-table)
        (error nil))))

  (defun my-smudge-lyrics--show-buffer (artist title lyrics)
    (let ((buf (get-buffer-create my-smudge-lyrics-buffer-name)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%s — %s\n\n" artist title))
          (insert lyrics))
        (goto-char (point-min))
        (my-smudge-lyrics-mode))
      (pop-to-buffer buf)))

  (defun my-smudge-lyrics--handle-response (status artist title token)
  (let* ((urlbuf (current-buffer)) ;; <-- capture the url-retrieve buffer
         (stale (and my-smudge-lyrics--pending-token
                     (not (equal token my-smudge-lyrics--pending-token)))))
    (unwind-protect
        (if stale
            (message "Lyrics response ignored (stale)")
          (let ((err (plist-get status :error)))
            (if err
                (message "Lyrics request failed: %s"
                         (if (stringp err) err (error-message-string err)))
              (with-current-buffer urlbuf
                (goto-char (point-min))
                (let ((code url-http-response-status))
                  ;; Headers can be \r\n\r\n or \n\n
                  (re-search-forward "\r?\n\r?\n" nil 'move)
                  (let* ((body (buffer-substring-no-properties (point) (point-max)))
                         (json (my-smudge-lyrics--parse-json-body body))
                         (lyrics (and json (gethash "plainLyrics" json))))
                    (when my-smudge-lyrics-debug
                      (message "Lyrics raw body: %s"
                               (truncate-string-to-width body 200 0 nil "…")))
                    (cond
                     ((eq code 404)
                      (message "No lyrics found for %s — %s" artist title))
                     ((and (numberp code) (>= code 400))
                      (message "Lyrics service error: %s" code))
                     ((and lyrics (not (string-empty-p (string-trim lyrics))))
                      (my-smudge-lyrics--show-buffer artist title lyrics))
                     (t
                      (message "No lyrics returned for %s — %s" artist title))))))))))
      ;; Always kill ONLY the url buffer, never the displayed lyrics buffer
      (when (buffer-live-p urlbuf)
        (kill-buffer urlbuf))))

  (defun my-smudge-lyrics--request (artist title duration token)
    (let* ((artist (my-smudge-lyrics--clean artist))
           (title  (my-smudge-lyrics--clean title))
           (url (my-smudge-lyrics--lrclib-url artist title duration))
           (url-request-method "GET")
           (url-request-timeout my-smudge-lyrics-timeout))
      (url-retrieve url #'my-smudge-lyrics--handle-response
                    (list artist title token) t t)))

  (defun my-smudge-lyrics--request-track (track)
    (let* ((artist (plist-get track :artist))
           (title  (plist-get track :title))
           (dur    (plist-get track :duration))
           (token  (float-time)))
      (setq my-smudge-lyrics--pending-token token)
      (my-smudge-lyrics--request artist title dur token)
      (message "Fetching lyrics for %s — %s..." artist title)))

  (defun my-smudge-lyrics--manual-timeout ()
    (when my-smudge-lyrics--manual-pending
      (setq my-smudge-lyrics--manual-pending nil)
      (let ((track (my-smudge-lyrics--current-track)))
        (when track
          (my-smudge-lyrics--request-track track)))))

  (defun lgm/smudge-lyrics-popup ()
    "Fetch lyrics for the current Smudge track and show a read-only buffer (async)."
    (interactive)
    ;; Force a fresh metadata refresh to avoid stale track info.
    (setq my-smudge-lyrics--manual-pending t)
    (when (timerp my-smudge-lyrics--manual-timer)
      (cancel-timer my-smudge-lyrics--manual-timer))
    (setq my-smudge-lyrics--manual-timer
          (run-at-time 0.8 nil #'my-smudge-lyrics--manual-timeout))
    (smudge-controller-player-status))

  (defun my-smudge-lyrics--maybe-auto (&rest _)
    (let ((track (my-smudge-lyrics--current-track)))
      (when track
        ;; If a manual request is pending, honor it with the fresh metadata.
        (when my-smudge-lyrics--manual-pending
          (setq my-smudge-lyrics--manual-pending nil)
          (my-smudge-lyrics--request-track track))
        ;; Auto popup on track change, if enabled.
        (when my-smudge-lyrics-auto-popup
          (let ((id (cons (plist-get track :artist)
                          (plist-get track :title))))
            (unless (equal id my-smudge-lyrics--last-track)
              (setq my-smudge-lyrics--last-track id)
              (my-smudge-lyrics--request-track track)))))))

  (advice-add 'smudge-controller-update-metadata :after #'my-smudge-lyrics--maybe-auto))
	
(provide 'media-settings)
;;; media-settings.el ends here
