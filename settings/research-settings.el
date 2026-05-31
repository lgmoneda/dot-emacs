;;; research-settings.el --- Research experiment workspace helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defvar org-roam-directory)

(defgroup lgm/research nil
  "Research experiment workspace helpers."
  :group 'convenience)

(defcustom lgm/research-repositories
  nil
  "Known experiment repositories.
Each entry is a plist with at least :name and :url.  Adding a new
repository only requires adding another plist here.

Keep private or work repository names in
`lgm/research-private-settings-file' instead of this public config."
  :type '(repeat plist))

(defcustom lgm/research-repos-directory "~/repos"
  "Directory where research repositories are checked out."
  :type 'directory)

(defcustom lgm/research-new-repository-accounts nil
  "GitHub accounts offered when creating a new research repository."
  :type '(repeat string))

(defcustom lgm/research-private-settings-file
  "~/Dropbox/Projetos/Emacs/research-private-settings.el"
  "Private file loaded after the public research defaults.
Use it for repository names, organization names, and other values that
should not be committed to this public Emacs config."
  :type 'file)

(defcustom lgm/research-create-project-marker t
  "When non-nil, create a .project marker in each experiment folder."
  :type 'boolean)

(with-eval-after-load 'project
  (when (boundp 'project-vc-extra-root-markers)
    (add-to-list 'project-vc-extra-root-markers ".project")))

(let ((private-settings (expand-file-name lgm/research-private-settings-file)))
  (when (file-exists-p private-settings)
    (load private-settings nil 'nomessage)))

(defun lgm/research--org-roam-directory ()
  "Return the configured org-roam directory."
  (cond
   ((boundp 'org-roam-directory)
    (file-truename (expand-file-name org-roam-directory)))
   ((require 'org-roam nil t)
    (file-truename (expand-file-name org-roam-directory)))
   (t
    (error "org-roam is not available"))))

(defun lgm/research--inside-directory-p (file directory)
  "Return non-nil when FILE is inside DIRECTORY."
  (let ((file (file-truename file))
        (directory (file-name-as-directory (file-truename directory))))
    (string-prefix-p directory file)))

(defun lgm/research--experiment-slug (file)
  "Return the experiment slug derived from org-roam FILE."
  (let* ((base (file-name-sans-extension (file-name-nondirectory file)))
         (without-date (replace-regexp-in-string
                        "\\`[0-9]+-" "" base))
         (slug (replace-regexp-in-string "_" "-" without-date)))
    (unless (string-match-p "\\S-" slug)
      (error "Could not derive an experiment name from %s" file))
    slug))

(defun lgm/research--repo-local-directory (repo)
  "Return the local checkout directory for REPO."
  (expand-file-name
   (or (plist-get repo :local-dir)
       (plist-get repo :name))
   lgm/research-repos-directory))

(defun lgm/research--repo-choice ()
  "Prompt for an experiment repository or a new repository."
  (let* ((new-label "Create new private repository")
         (choices (append (mapcar (lambda (repo)
                                    (plist-get repo :name))
                                  lgm/research-repositories)
                          (list new-label)))
         (choice (completing-read "Research repository: " choices nil t)))
    (if (string= choice new-label)
        :new
      (cl-find choice lgm/research-repositories
               :key (lambda (repo) (plist-get repo :name))
               :test #'string=))))

(defun lgm/research--directory-candidates (root)
  "Return relative directory candidates under ROOT."
  (let ((root (file-name-as-directory (expand-file-name root)))
        (result '(".")))
    (cl-labels
        ((walk (dir)
           (dolist (entry (directory-files dir t "\\`[^.]"))
             (when (file-directory-p entry)
               (let ((name (file-name-nondirectory entry)))
                 (unless (member name '(".git" "node_modules" ".venv" "__pycache__"))
                   (push (directory-file-name
                          (file-relative-name entry root))
                         result)
                   (walk entry)))))))
      (walk root))
    (sort result #'string<)))

(defun lgm/research--choose-parent-directory (repo-dir)
  "Prompt for the parent directory inside REPO-DIR."
  (let* ((candidates (lgm/research--directory-candidates repo-dir))
         (choice (completing-read
                  "Create experiment under directory (. = repo root): "
                  candidates nil nil nil nil ".")))
    (expand-file-name choice repo-dir)))

(defun lgm/research--run-gh-create-repo (owner repo-name)
  "Create OWNER/REPO-NAME on GitHub and clone it locally."
  (let* ((repos-dir (file-name-as-directory
                     (expand-file-name lgm/research-repos-directory)))
         (target-dir (expand-file-name repo-name repos-dir))
         (default-directory repos-dir)
         (buffer (get-buffer-create "*lgm-research-gh*"))
         (repo (format "%s/%s" owner repo-name)))
    (when (file-exists-p target-dir)
      (error "Local repository already exists: %s" target-dir))
    (make-directory repos-dir t)
    (with-current-buffer buffer
      (erase-buffer))
    (unless (zerop (apply #'process-file
                          "gh" nil buffer nil
                          (list "repo" "create" repo
                                "--private" "--add-readme" "--clone")))
      (error "gh repo create failed for %s:\n%s"
             repo
             (with-current-buffer buffer
               (string-trim (buffer-string)))))
    target-dir))

(defun lgm/research--create-new-repository (slug)
  "Prompt for a new repository owner and name, using SLUG as default."
  (unless lgm/research-new-repository-accounts
    (error "Configure `lgm/research-new-repository-accounts' first"))
  (let* ((owner (completing-read "GitHub account: "
                                 lgm/research-new-repository-accounts nil t))
         (repo-name (read-string "New repository name: " slug)))
    (lgm/research--run-gh-create-repo owner repo-name)))

(defun lgm/research--move-roam-file (file slug roam-dir)
  "Move FILE into ROAM-DIR/SLUG and return the new file path."
  (let* ((roam-experiment-dir (expand-file-name slug roam-dir))
         (target-file (expand-file-name (file-name-nondirectory file)
                                        roam-experiment-dir)))
    (when (file-exists-p roam-experiment-dir)
      (error "Org-roam experiment folder already exists: %s" roam-experiment-dir))
    (make-directory roam-experiment-dir t)
    (rename-file file target-file)
    target-file))

(defun lgm/research--roam-experiment-directory (slug roam-dir)
  "Return the org-roam experiment directory for SLUG under ROAM-DIR."
  (expand-file-name slug roam-dir))

(defun lgm/research--write-project-marker (experiment-dir)
  "Create a .project marker in EXPERIMENT-DIR when configured."
  (when lgm/research-create-project-marker
    (let ((marker (expand-file-name ".project" experiment-dir)))
      (when (file-exists-p marker)
        (error "Project marker already exists: %s" marker))
      (with-temp-file marker
        (insert ";;; Project marker for this research experiment.\n")))))

(defun lgm/setup-research-experiment ()
  "Create a research experiment folder from the current org-roam file.

The org-roam file is moved into a same-named folder under
`org-roam-directory'.  The experiment repository receives a hard link to
that org file, so the note remains available to org-roam even if the
repository checkout is later removed."
  (interactive)
  (unless buffer-file-name
    (error "Current buffer is not visiting a file"))
  (let* ((source-file (file-truename buffer-file-name))
         (roam-dir (lgm/research--org-roam-directory)))
    (unless (and (derived-mode-p 'org-mode)
                 (string= (file-name-extension source-file) "org"))
      (error "Current buffer must be an org file"))
    (unless (lgm/research--inside-directory-p source-file roam-dir)
      (error "Current file is not inside org-roam-directory: %s" roam-dir))
    (when (buffer-modified-p)
      (save-buffer))
    (let* ((slug (lgm/research--experiment-slug source-file))
           (roam-experiment-dir
            (lgm/research--roam-experiment-directory slug roam-dir)))
      (when (file-exists-p roam-experiment-dir)
        (error "Org-roam experiment folder already exists: %s" roam-experiment-dir))
      (let* ((repo-choice (lgm/research--repo-choice))
             (new-repo-p (eq repo-choice :new))
             (repo-dir (if new-repo-p
                           (lgm/research--create-new-repository slug)
                         (lgm/research--repo-local-directory repo-choice)))
             parent-dir experiment-dir roam-file repo-org-file)
        (unless (file-directory-p repo-dir)
          (error "Local repository does not exist: %s" repo-dir))
        (setq parent-dir (if new-repo-p
                             nil
                           (lgm/research--choose-parent-directory repo-dir)))
        (setq experiment-dir (if new-repo-p
                                 repo-dir
                               (expand-file-name slug parent-dir)))
        (when (and (not new-repo-p)
                   (file-exists-p experiment-dir))
          (error "Experiment folder already exists: %s" experiment-dir))
        (setq repo-org-file (expand-file-name
                             (file-name-nondirectory source-file)
                             experiment-dir))
        (when (file-exists-p repo-org-file)
          (error "Repository org link already exists: %s" repo-org-file))
        (unless new-repo-p
          (make-directory experiment-dir t))
        (setq roam-file (lgm/research--move-roam-file source-file slug roam-dir))
        (add-name-to-file roam-file repo-org-file)
        (lgm/research--write-project-marker experiment-dir)
        (set-visited-file-name roam-file t t)
        (when (fboundp 'org-roam-db-sync)
          (org-roam-db-sync))
        (message "Research experiment ready: %s" experiment-dir)
        experiment-dir))))

(provide 'research-settings)
