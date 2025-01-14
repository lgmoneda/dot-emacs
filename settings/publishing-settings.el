;;; publishing-settings.el --- Settings for publishing in my blog

;Sunday, January 12, 2025
;============================
;==        Publishing      ==
;============================

;; It is fixed for my personal blog case, so I don't have to add it to org roam files
(setq org-html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"../org-roam/org.css\" />")

;; Publishing org roam files to my personal website so I can easily share notes
(defun lgm/publish-org-roam-to-github (github-repo-dir output-dir branch)
  "Publish the current Org Roam file to a GitHub Pages site, including images.

GITHUB-REPO-DIR: Path to your GitHub Pages repository.
OUTPUT-DIR: Directory inside the repository where the HTML file will be placed.
BRANCH: Branch to which changes should be committed (e.g., 'gh-pages')."
  (interactive
   (list (read-directory-name "GitHub Repo Directory: " "~/repos/lgmoneda.github.io/")
         (read-string "Output Directory (relative to repo root): " "org-roam/")
         (read-string "Branch: " "master")))
  (let* ((org-file (buffer-file-name))
         (org-html-export-file (concat (file-name-sans-extension org-file) ".html"))
         (output-path (expand-file-name output-dir github-repo-dir))
         (output-file (expand-file-name (file-name-nondirectory org-html-export-file) output-path))
         (image-dir (expand-file-name "images/org-roam/" github-repo-dir))
         (image-paths (make-hash-table :test 'equal)))
    (unless org-file
      (error "This buffer is not visiting a file"))
    (unless (derived-mode-p 'org-mode)
      (error "This function only works in Org mode buffers"))
	;; Sync org-roam db
	(org-roam-db-sync)
    ;; Ensure the image directory exists
    (unless (file-directory-p image-dir)
      (make-directory image-dir t))
    ;; Find and copy images referenced in the Org file
    (org-element-map (org-element-parse-buffer) 'link
  (lambda (link)
    (when (string= (org-element-property :type link) "file")
      (let* ((image-path (org-element-property :path link)) ;; Path to the image
             (absolute-image-path (expand-file-name image-path (file-name-directory org-file)))
             (relative-image-path (file-relative-name absolute-image-path (file-name-directory org-file)))
             (image-filename (file-name-nondirectory image-path))
             (destination-image-path (expand-file-name image-filename image-dir)))
        (when (and image-path (file-exists-p absolute-image-path))
          ;; Copy the image and store the mapping for replacement
          (copy-file absolute-image-path destination-image-path t)
          (puthash relative-image-path ;; Store full relative path as the key
                   (concat "../images/org-roam/" image-filename) ;; Store updated path
                   image-paths))))))
    (message "Image Paths: %s" (mapcar (lambda (key) (list key (gethash key image-paths))) (hash-table-keys image-paths)))
    ;; Export the Org file to HTML
    (org-html-export-to-html)
    ;; Ensure the output directory exists
    (unless (file-directory-p output-path)
      (make-directory output-path t))
    ;; Move the exported HTML to the output directory
    (copy-file org-html-export-file output-file t)
    ;; Post-process the HTML file to update image paths
    (with-temp-buffer
  (insert-file-contents output-file)
  (let ((keys (hash-table-keys image-paths)))
    (message "Hash table keys: %s" keys)
    (dolist (key keys)
      (message "Processing key: %s" key)
      (goto-char (point-min))
      (while (search-forward key nil t)
        (message "Replacing key: %s with value: %s" key (gethash key image-paths))
        (replace-match (gethash key image-paths) t t))))
  (write-file output-file))

    (message "Exported %s to %s" org-file output-file)
    ;; Commit and push the changes to GitHub
    (let ((default-directory github-repo-dir))
      (shell-command (format "git add %s" (shell-quote-argument output-file)))
      (shell-command (format "git add %s" (shell-quote-argument (expand-file-name "images/org-roam/" github-repo-dir))))
      (shell-command (format "git commit -m 'Publish %s with images'" (file-name-nondirectory output-file)))
      (shell-command (format "git push origin %s" branch))
      (message "Published to GitHub Pages branch: %s" branch))))


;; Blogging
;; The following function helps me to write in a single file in org-roam to produce my personal blog posts.
;; It avoids copying and pasting to the markdown file for jekyll.

(defun lgm/publish-org-roam-to-jekyll-html (github-repo-dir output-dir branch)
  "Export current Org Roam file to Jekyll-compatible HTML5 with YAML front matter.

GITHUB-REPO-DIR: Path to your GitHub Pages repository.
OUTPUT-DIR: Directory inside the repository where the HTML file will be placed.
BRANCH: Branch to which changes should be committed (e.g., 'master')."
  (interactive
   (list (read-directory-name "GitHub Repo Directory: " "~/repos/lgmoneda.github.io/")
         (read-string "Output Directory (relative to repo root): " "_posts/")
         (read-string "Branch: " "master")))
  (let* ((org-file (buffer-file-name))
         (file-keywords '("TITLE" "DATE" "AUTHOR" "TAGS" "DESCRIPTION" "LANG" "LAYOUT" "IMAGE" "REF" "COMMENTS"))
         (keywords (org-collect-keywords file-keywords))
         (properties (org-entry-properties nil 'standard))
         (metadata (lgm/merge-keywords-and-properties keywords properties))
         (yaml-front-matter (lgm/generate-yaml-front-matter metadata))
         (title (or (plist-get metadata :title) (file-name-base org-file)))
         (title-slug (lgm/remove-leading-timestamp title))
         (publish-date (or (plist-get metadata :date) (format-time-string "%Y-%m-%d")))
         (slug (lgm/slugify title-slug))
         (ref (plist-get metadata :ref))
         (output-filename (format "%s-%s.md" publish-date slug))
         (output-path (expand-file-name output-dir github-repo-dir))
         (output-file (expand-file-name output-filename output-path))
         (image-dir (expand-file-name (format "images/%s/" ref) github-repo-dir))
         (image-paths (make-hash-table :test 'equal))
         (temp-html-file (concat (file-name-sans-extension org-file) ".html")))
    ;; Verify org-file exists and is in org-mode
    (unless org-file
      (error "This buffer is not visiting a file"))
    (unless (derived-mode-p 'org-mode)
      (error "This function only works in Org mode buffers"))
    ;; Sync org-roam db
    (org-roam-db-sync)
    ;; Ensure the image directory exists
    (unless (file-directory-p image-dir)
      (make-directory image-dir t))
    ;; Find and copy images referenced in the Org file
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (let* ((image-path (org-element-property :path link))
                 (absolute-image-path (expand-file-name image-path (file-name-directory org-file)))
                 (image-filename (file-name-nondirectory image-path))
                 (destination-image-path (expand-file-name image-filename image-dir)))
            (when (and image-path (file-exists-p absolute-image-path))
              (copy-file absolute-image-path destination-image-path t)
              (puthash image-path
                       (concat "/images/" ref "/" image-filename)
                       image-paths))))))
    ;; Export the Org file to HTML5 using org-pandoc-export-to-html5
    (let ((html-body nil)
		  (pandoc-command (format "pandoc %s -o %s --from=org --to=html5 --shift-heading-level-by=1"
                                 (shell-quote-argument org-file-path)
                                 (shell-quote-argument temp-html-file))))

	;; Export the org file to HTML
      (shell-command-to-string pandoc-command)

;; Exclude org-roam links from the temp-html-file
      (with-temp-buffer
        (insert-file-contents temp-html-file)
        ;; Find and replace org-roam links by removing the <a> tag, leaving only the link text
        (goto-char (point-min))
        (while (re-search-forward "<a\\s-+href=\"id:[^\"]*\">\\([^<]*\\)</a>" nil t)
          (replace-match "\\1" nil nil))
        ;; Replace image paths in the exported body
        (let ((keys (hash-table-keys image-paths)))
          (dolist (key keys)
            (goto-char (point-min))
            (while (search-forward key nil t)
              (replace-match (gethash key image-paths) t t))))
        ;; Prepend YAML front matter
        (goto-char (point-min))
        (insert yaml-front-matter "\n")
        ;; Write the final content to the output file
        (write-region (point-min) (point-max) output-file)))

	;; Delete the temporary HTML file
	(delete-file temp-html-file)
    ;; Commit and push the changes to GitHub
    (let ((default-directory github-repo-dir))
      ;; Uncomment the following commands to enable Git automation:
      ;; (shell-command (format "git add %s" (shell-quote-argument output-file)))
      ;; (shell-command (format "git add %s" (shell-quote-argument image-dir)))
      ;; (shell-command (format "git commit -m 'Publish %s with images'" output-filename))
      ;; (shell-command (format "git push origin %s" branch))
      (message "Published to GitHub Pages branch: %s" branch))))

(defun lgm/merge-keywords-and-properties (keywords properties)
  "Merge file KEYWORDS and PROPERTIES into a single plist.
KEYWORDS is an alist from `org-collect-keywords`.
PROPERTIES is an alist from `org-entry-properties`."
  (let ((meta-plist nil))
    ;; Add keywords to plist
    (dolist (keyword keywords)
      (let ((key (intern (concat ":" (downcase (car keyword)))))
            (value (mapconcat 'identity (cdr keyword) " ")))
        (setq meta-plist (plist-put meta-plist key value))))
    ;; Add properties to plist
    (dolist (prop properties)
      (let ((key (intern (concat ":" (downcase (car prop)))))
            (value (cdr prop)))
        (setq meta-plist (plist-put meta-plist key value))))
    meta-plist))

(defun lgm/generate-yaml-front-matter (metadata)
  "Generate YAML front matter from METADATA plist, treating all keys uniformly."
  (let* ((yaml-front-matter "---\n")
         ;; Rename :jekyll-tags to :tags
         (metadata (plist-put metadata :tags (plist-get metadata :jekyll-tags))))
    (mapc (lambda (key)
            (let ((value (plist-get metadata key)))
              (when value
                (setq yaml-front-matter
                      (concat yaml-front-matter
                              (format "%s: %s\n"
                                      (substring (symbol-name key) 1) ;; Remove leading colon
                                      value))))))
          '(:layout :title :date :lang :ref :comments :author :tags :description :image))
    (concat yaml-front-matter "---\n")))

(defun lgm/slugify (title)
  "Convert TITLE to a slug suitable for filenames."
  (let ((slug (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title))))
    (replace-regexp-in-string "^-\\|-+$" "" slug)))

(defun lgm/remove-leading-timestamp (title)
  "Remove leading date or timestamp from TITLE."
  (replace-regexp-in-string "^[0-9]+[-_]*" "" title))

(provide 'publishing-settings)
;;; publishing-settings.el ends here
