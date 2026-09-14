;;; org-roam-embeddings-settings.el --- Refresh Org-roam embeddings -*- lexical-binding: t; -*-

(require 'conda-settings)

(defgroup lmoneda/org-roam-embeddings nil
  "Keep the external Org-roam embedding database current."
  :group 'org-roam)

(defcustom lmoneda/org-roam-embeddings-repository
  "~/repos/org-roam-ai/"
  "Directory containing the Org-roam embedding update script."
  :type 'directory)

(defcustom lmoneda/org-roam-embeddings-conda-environment "ml3"
  "Conda environment used by the Org-roam embedding update script."
  :type 'string)

(defvar lmoneda/org-roam-embeddings--process nil
  "Current Org-roam embedding update process, or nil.")

(defconst lmoneda/org-roam-embeddings--buffer-name
  "*org-roam-embeddings-update*"
  "Name of the buffer that receives update output.")

(defun lmoneda/org-roam-embeddings--sentinel (process _event)
  "Report the final status of the embedding update PROCESS."
  (when (memq (process-status process) '(exit signal))
    (setq lmoneda/org-roam-embeddings--process nil)
    (if (= (process-exit-status process) 0)
        (message "Org-roam embeddings updated.")
      (message
       "Org-roam embedding update failed safely (exit %s); see %s"
       (process-exit-status process)
       lmoneda/org-roam-embeddings--buffer-name))))

;;;###autoload
(defun lmoneda/org-roam-embeddings-update ()
  "Update Org-roam embeddings asynchronously using the configured Conda env.

If an update is already running, do nothing.  Missing local dependencies and
network failures do not interrupt Emacs startup; diagnostics are retained in
`lmoneda/org-roam-embeddings--buffer-name'."
  (interactive)
  (if (process-live-p lmoneda/org-roam-embeddings--process)
      (message "Org-roam embedding update is already running.")
    (let* ((repository
            (file-name-as-directory
             (expand-file-name lmoneda/org-roam-embeddings-repository)))
           (script (expand-file-name "roam_search_update.py" repository))
           (python
            (lgm/conda-env-executable
             lmoneda/org-roam-embeddings-conda-environment "python")))
      (cond
       ((not (file-directory-p repository))
        (message "Skipping Org-roam embedding update: repository not found: %s"
                 repository))
       ((not (file-readable-p script))
        (message "Skipping Org-roam embedding update: script not found: %s"
                 script))
       ((not (and python (file-executable-p python)))
        (message "Skipping Org-roam embedding update: ml3 Python not found."))
       (t
        (let ((default-directory repository)
              (buffer (get-buffer-create
                       lmoneda/org-roam-embeddings--buffer-name)))
          (with-current-buffer buffer
            (erase-buffer))
          (condition-case error-data
              (progn
                (setq lmoneda/org-roam-embeddings--process
                      (make-process
                       :name "org-roam-embeddings-update"
                       :buffer buffer
                       :command (list python script)
                       :connection-type 'pipe
                       :noquery t
                       :sentinel #'lmoneda/org-roam-embeddings--sentinel))
                (message
                 "Org-roam embedding update started in the background."))
            (file-error
             (message "Skipping Org-roam embedding update: %s"
                      (error-message-string error-data))))))))))

(add-hook 'emacs-startup-hook #'lmoneda/org-roam-embeddings-update 100)

(provide 'org-roam-embeddings-settings)
;;; org-roam-embeddings-settings.el ends here
