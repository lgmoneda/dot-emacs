;;; org-roam-catalog-settings.el --- Export desktop titles for BOOX -*- lexical-binding: t; -*-

(require 'json)
(require 'cl-lib)

(defvar org-roam-directory)
(declare-function org-roam-db-sync "org-roam-db" (&optional force))
(declare-function org-roam-db-query "org-roam-db" (sql &rest args))

(defgroup lmoneda/roam-catalog nil
  "Desktop note catalog for the tablet." :group 'org)

(defcustom lmoneda/org-roam-catalog-directory
  (expand-file-name "~/Dropbox/Agenda/desktop-org-roam-catalog/")
  "Directory synced to OrgSync/desktop-org-roam-catalog on the tablet."
  :type 'directory)

(defun lmoneda/org-roam-export-desktop-catalog (&optional sync-first)
  "Export the existing Org-roam index without parsing the note collection.
With prefix SYNC-FIRST, synchronize the desktop database before exporting.
Export IDs, titles, aliases, and citation keys from the database.
Only .org files below `org-roam-directory' are included.  The roam-boox
subdirectory is excluded: those notes have their own tablet index."
  (interactive "P")
  (require 'org-roam)
  (when sync-first (org-roam-db-sync))
  (let* ((root (file-name-as-directory (file-truename org-roam-directory)))
         (aliases (make-hash-table :test #'equal))
         (citekeys (make-hash-table :test #'equal))
         (rows (org-roam-db-query
                [:select [id title file level] :from nodes]))
         (entries nil))
    (dolist (row (org-roam-db-query [:select [node-id alias] :from aliases]))
      (push (cadr row) (gethash (car row) aliases)))
    (dolist (row (org-roam-db-query
                  [:select [node-id ref] :from refs :where (= type "cite")]))
      (push (cadr row) (gethash (car row) citekeys)))
    (dolist (row rows)
      (pcase-let* ((`(,id ,title ,file ,level) row)
                   (relative (file-relative-name (file-truename file) root)))
        (when (and (string-suffix-p ".org" relative)
                   (not (file-name-absolute-p relative))
                   (not (string-prefix-p "../" relative))
                   (not (string-prefix-p "roam-boox/" relative)))
          (push `((id . ,id) (title . ,title) (file . ,relative)
                  (level . ,level)
                  (aliases . ,(vconcat (gethash id aliases)))
                  (citekeys . ,(vconcat (gethash id citekeys)))) entries))))
    (setq entries (sort entries (lambda (a b)
                                 (string-lessp (alist-get 'id a)
                                               (alist-get 'id b)))))
    (make-directory lmoneda/org-roam-catalog-directory t)
    (let* ((target (expand-file-name "catalog.json" lmoneda/org-roam-catalog-directory))
           (temporary (make-temp-file (expand-file-name ".catalog-"
                                                        lmoneda/org-roam-catalog-directory)))
           (coding-system-for-write 'utf-8-unix))
      (unwind-protect
          (progn
            (with-temp-file temporary
              (insert (json-encode
                       `((version . 1)
                         (generated_at . ,(format-time-string "%FT%TZ" nil t))
                         (nodes . ,(vconcat entries)))))
              (insert "\n"))
            (rename-file temporary target t))
        (when (file-exists-p temporary) (delete-file temporary)))
      (message "Exported %d desktop nodes to %s" (length entries) target)
      target)))

(provide 'org-roam-catalog-settings)
;;; org-roam-catalog-settings.el ends here
