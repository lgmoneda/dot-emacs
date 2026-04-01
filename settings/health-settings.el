;;; health-settings.el --- Weight tracking helper -*- lexical-binding: t; -*-

(require 'subr-x)

(defconst lgm/weight--tracker-dir "~/repos/my-life-scripts/weight-tracker/")
(defconst lgm/weight--python "/Users/luis.moneda/miniconda3/envs/edge/bin/python3")
(defconst lgm/weight--listen-seconds 45)
(defconst lgm/weight--raw-file "~/Dropbox/data-about-luis/weight-tracker/weights_log.csv")
(defconst lgm/weight--events-file "~/Dropbox/data-about-luis/weight-tracker/s200_events.log")
(defconst lgm/weight--stable-file "~/Dropbox/data-about-luis/weight-tracker/stable_weights.csv")
(defconst lgm/physical-prog--script "~/repos/my-life-scripts/physical-progress-tracker/physical_progress_tracker.py")
(defconst lgm/physical-prog--timer-seconds 10)

(defun lgm/weight--path (file-name)
  "Expand FILE-NAME inside the weight tracker directory."
  (expand-file-name file-name lgm/weight--tracker-dir))

(defun lgm/weight--latest-stable-entry ()
  "Return latest stable entry plist or nil.

Plist keys: :timestamp, :weight, :profile."
  (let ((stable-file (expand-file-name lgm/weight--stable-file)))
    (when (file-exists-p stable-file)
      (with-temp-buffer
        (insert-file-contents stable-file)
        (let* ((lines (split-string (buffer-string) "\n" t))
               (data-lines (cdr lines))
               (last-line (car (last data-lines))))
          (when last-line
            (let ((fields (split-string last-line ",")))
              (when (>= (length fields) 5)
                (list :timestamp (nth 0 fields)
                      :weight (nth 3 fields)
                      :profile (nth 4 fields))))))))))

(defun lgm/weight--same-entry-p (entry-a entry-b)
  "Return non-nil when ENTRY-A and ENTRY-B represent the same reading."
  (and (equal (plist-get entry-a :timestamp) (plist-get entry-b :timestamp))
       (equal (plist-get entry-a :weight) (plist-get entry-b :weight))
       (equal (plist-get entry-a :profile) (plist-get entry-b :profile))))

(defun lgm/weight--process-sentinel (process _event)
  "Sentinel for PROCESS used by `lgm/weight`."
  (when (memq (process-status process) '(exit signal))
    (let ((exit-code (process-exit-status process))
          (before-entry (process-get process :before-entry))
          (events-file (process-get process :events-file)))
      (if (= exit-code 0)
          (let ((after-entry (lgm/weight--latest-stable-entry)))
            (if (or (null after-entry)
                    (lgm/weight--same-entry-p before-entry after-entry))
                (message "No new stable weight detected.")
              (message "Weight registered: %s kg (profile %s)"
                       (plist-get after-entry :weight)
                       (or (plist-get after-entry :profile) "?"))))
        (message "Weight capture failed (exit %s). Check %s"
                 exit-code
                 events-file)))))

;;;###autoload
(defun lgm/weight ()
  "Capture weight for 45s and report a concise result."
  (interactive)
  (let ((running-process (get-process "lgm/weight-process")))
    (if (and running-process (process-live-p running-process))
        (message "Weight capture already running.")
      (let* ((tracker-script (lgm/weight--path "s200_logger.py"))
             (raw-output (expand-file-name lgm/weight--raw-file))
             (events-output (expand-file-name lgm/weight--events-file))
             (stable-output (expand-file-name lgm/weight--stable-file))
             (before-entry (lgm/weight--latest-stable-entry)))
        (cond
         ((not (file-exists-p tracker-script))
          (message "Weight tracker script not found: %s" tracker-script))
         (t
          (let* ((command (mapconcat
                           #'identity
                           (list
                            "source ~/.zshrc >/dev/null 2>&1;"
                            (shell-quote-argument lgm/weight--python)
                            (shell-quote-argument tracker-script)
                            "--mode" "adv"
                            "--listen-seconds" (number-to-string lgm/weight--listen-seconds)
                            "--output" (shell-quote-argument raw-output)
                            "--event-log" (shell-quote-argument events-output)
                            "--stable-output" (shell-quote-argument stable-output))
                           " "))
                 (process-buffer (get-buffer-create " *lgm/weight*"))
                 (default-directory (file-name-as-directory
                                     (expand-file-name lgm/weight--tracker-dir)))
                 (process (make-process
                           :name "lgm/weight-process"
                           :buffer process-buffer
                           :command (list "zsh" "-lc" command)
                           :noquery t
                           :sentinel #'lgm/weight--process-sentinel)))
            (with-current-buffer process-buffer
              (erase-buffer))
            (process-put process :before-entry before-entry)
            (process-put process :events-file events-output)
            (message "Weight capture started for %ss. Step on the scale now."
                     lgm/weight--listen-seconds))))))))


(defun lgm/physical-prog--process-sentinel (process _event)
  "Sentinel for PROCESS used by `lgm/physical-prog`."
  (when (memq (process-status process) '(exit signal))
    (let ((exit-code (process-exit-status process)))
      (cond
       ((= exit-code 0)
        (message "Physical progress capture completed. Check ~/Dropbox/data-about-luis/physical-evolution-tracker/"))
       ((= exit-code 2)
        (message "Physical progress capture failed: Camera not accessible. Grant Camera permission to Emacs/Terminal and retry."))
       (t
        (message "Physical progress capture failed (exit %s). Check buffer *lgm/physical-prog*" exit-code))))))

;;;###autoload
(defun lgm/physical-prog ()
  "Run the physical progress capture script with an automatic timer."
  (interactive)
  (let ((running-process (get-process "lgm/physical-prog-process")))
    (if (and running-process (process-live-p running-process))
        (message "Physical progress capture already running.")
      (let* ((script-path (expand-file-name lgm/physical-prog--script))
             (default-directory (file-name-directory script-path)))
        (cond
         ((not (file-exists-p script-path))
          (message "Physical progress script not found: %s" script-path))
         (t
          (let* ((command (mapconcat
                           #'identity
                           (list
                            (shell-quote-argument lgm/weight--python)
                            (shell-quote-argument script-path)
                            "--timer-seconds"
                            (number-to-string lgm/physical-prog--timer-seconds))
                           " "))
                 (process-buffer (get-buffer-create " *lgm/physical-prog*"))
                 (process (make-process
                           :name "lgm/physical-prog-process"
                           :buffer process-buffer
                           :command (list "zsh" "-lc" command)
                           :noquery t
                           :sentinel #'lgm/physical-prog--process-sentinel)))
            (with-current-buffer process-buffer
              (erase-buffer))
            (message "Physical progress capture started. Auto-photo in %ss."
                     lgm/physical-prog--timer-seconds))))))))

(provide 'health-settings)
;;; health-settings.el ends here
