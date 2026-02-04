;;; media-settings.el --- Settings for media (audio, video)

;; Keeps the token for an emacs session
(setq plstore-cache-passphrase-for-symmetric-encryption t)

;; (use-package smudge
;;       :load-path "~/repos/smudge"
;;       :bind-keymap ("C-c ." . smudge-command-map)
;;       :custom
;;       (smudge-transport 'connect)
;;   (smudge-oauth2-client-id (getenv "SMUDGE_CLIENT_ID"))
;;   (smudge-oauth2-client-secret (getenv "SMUDGE_CLIENT_SECRET"))
;;   (smudge-player-use-transient-map t))

(use-package smudge
  :ensure t
  :commands (smudge-command-map)
  :config
  (setq smudge-transport 'connect)
  ;; (define-key smudge-command-map (kbd "l") #'lgm/smudge-lyrics-popup)
  :bind-keymap ("C-c ." . smudge-command-map)
  :custom
  (smudge-oauth2-client-id (getenv "SMUDGE_CLIENT_ID"))
  (smudge-oauth2-client-secret (getenv "SMUDGE_CLIENT_SECRET"))
  (smudge-player-use-transient-map t))

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

(provide 'media-settings)
;;; media-settings.el ends here
