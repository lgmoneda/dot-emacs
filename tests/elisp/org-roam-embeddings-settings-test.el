;;; org-roam-embeddings-settings-test.el --- Embedding updater tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let ((settings-directory
       (expand-file-name "../../settings"
                         (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path settings-directory))

(require 'conda-settings)
(require 'org-roam-embeddings-settings)

(ert-deftest lmoneda/org-roam-embeddings-update-starts-directly-in-repository ()
  (let* ((repository (make-temp-file "org-roam-ai-" t))
         (script (expand-file-name "roam_search_update.py" repository))
         (python (expand-file-name "python" repository))
         (lmoneda/org-roam-embeddings-repository repository)
         (lmoneda/org-roam-embeddings--process nil)
         captured-command
         captured-directory)
    (unwind-protect
        (progn
          (write-region "" nil script nil 'silent)
          (write-region "" nil python nil 'silent)
          (set-file-modes python #o755)
          (cl-letf (((symbol-function 'lgm/conda-env-executable)
                     (lambda (_environment _executable) python))
                    ((symbol-function 'make-process)
                     (lambda (&rest arguments)
                       (setq captured-command (plist-get arguments :command)
                             captured-directory default-directory)
                       'fake-process)))
            (lmoneda/org-roam-embeddings-update))
          (should (equal captured-command (list python script)))
          (should (equal captured-directory
                         (file-name-as-directory repository))))
      (delete-directory repository t))))

(ert-deftest lmoneda/org-roam-embeddings-update-skips-when-already-running ()
  (let ((lmoneda/org-roam-embeddings--process 'existing-process)
        process-created)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_process) t))
              ((symbol-function 'make-process)
               (lambda (&rest _arguments) (setq process-created t))))
      (lmoneda/org-roam-embeddings-update)
      (should-not process-created))))

(ert-deftest lmoneda/org-roam-embeddings-update-skips-missing-repository ()
  (let ((lmoneda/org-roam-embeddings-repository
         "/path/that/does/not/exist/")
        (lmoneda/org-roam-embeddings--process nil)
        process-created)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _arguments) (setq process-created t))))
      (lmoneda/org-roam-embeddings-update)
      (should-not process-created))))

(provide 'org-roam-embeddings-settings-test)
;;; org-roam-embeddings-settings-test.el ends here
