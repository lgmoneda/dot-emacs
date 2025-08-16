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
      (shell-command (format "git commit --no-gpg-sign -m 'Publish %s with images'" (file-name-nondirectory output-file)))
      (shell-command (format "git push origin %s" branch))
      (message "Published to GitHub Pages branch: %s" branch))))


;; Blogging
;; The following function helps me to write in a single file in org-roam to produce my personal blog posts.
;; It avoids copying and pasting to the markdown file for jekyll.
(defvar working-publishing-repository "~/repos/lgmoneda.github.io/"
  "Path to the GitHub repository used for publishing.")

(defvar working-publishing-directory "_posts/"
  "Directory inside the repository where published files will be stored.")

(defvar working-publishing-branch "master"
  "Git branch used for publishing changes.")

(defun lgm/set-working-publishing-vars (repo dir branch)
  "Set the working publishing variables for the current project.

REPO: Path to the GitHub repository.
DIR: Directory inside the repository for publishing.
BRANCH: Git branch to which changes will be committed."
  (interactive
   (list (read-directory-name "GitHub Repo Directory: " working-publishing-repository)
         (read-string "Output Directory (relative to repo root): " working-publishing-directory)
         (read-string "Branch: " working-publishing-branch)))
  (setq working-publishing-repository repo)
  (setq working-publishing-directory dir)
  (setq working-publishing-branch branch)
  (message "Publishing variables updated:\nRepository: %s\nDirectory: %s\nBranch: %s"
           working-publishing-repository
           working-publishing-directory
           working-publishing-branch))


(defun lgm/publish-org-roam-to-jekyll-html (github-repo-dir output-dir branch)
  "Export current Org Roam file to Jekyll-compatible HTML5 with YAML front matter.

GITHUB-REPO-DIR: Path to your GitHub Pages repository.
OUTPUT-DIR: Directory inside the repository where the HTML file will be placed.
BRANCH: Branch to which changes should be committed (e.g., 'master')."
  (interactive
   (list (read-directory-name "GitHub Repo Directory: " working-publishing-repository)
         (read-string "Output Directory (relative to repo root): " working-publishing-directory)
         (read-string "Branch: " working-publishing-branch)))
  (let* ((org-file (buffer-file-name))
         (file-keywords '("TITLE" "DATE" "AUTHOR" "TAGS" "DESCRIPTION" "LANG" "LAYOUT" "IMAGE" "REF" "COMMENTS" "PARENT" "NAV_ORDER" "MATHJAX"))
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
		  (pandoc-command (format "pandoc %s -o %s --from=org --to=html5 --shift-heading-level-by=1 --citeproc --metadata link-citations=true   --lua-filter=/Users/luis.moneda/Dropbox/Research/add-citation-backlinks.lua \ --bibliography=/Users/luis.moneda/Dropbox/Research/library.bib --csl=/Users/luis.moneda/Dropbox/Research/custom-style-blog.csl"
                                 (shell-quote-argument org-file)
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
    (let* ((choices '("No" "Yes"))
		 (choice (completing-read "Do you want to commit and push to GitHub? " choices nil t nil nil "No")))
	    (when (string= choice "Yes")
	      (let ((default-directory github-repo-dir))
	  ;; Uncomment the following commands to enable Git automation:
	  (shell-command (format "git add %s" (shell-quote-argument output-file)))
	  (shell-command (format "git add %s" (shell-quote-argument image-dir)))
	  (shell-command (format "git commit -m 'Publish %s with images'" output-filename))
	  (shell-command (format "git push origin %s" branch))
	  (message "Published to GitHub Pages branch: %s" branch)
	      )))))

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
                                      (if (eq key :title)
                                          (format "\"%s\"" value) ;; Enclose title in quotes
                                          value)))))))
          '(:layout :title :date :lang :ref :comments :author :tags :description :image :parent :nav_order :mathjax))
    (concat yaml-front-matter "---\n")))

(defun lgm/slugify (title)
  "Convert TITLE to a slug suitable for filenames."
  (let ((slug (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title))))
    (replace-regexp-in-string "^-\\|-+$" "" slug)))

(defun lgm/remove-leading-timestamp (title)
  "Remove leading date or timestamp from TITLE."
  (replace-regexp-in-string "^[0-9]+[-_]*" "" title))

;; When using lgm/publish-org-roam-to-jekyll-html, if I use the transclude tag,
;; I need to use this function since I call pandoc directly on the file so the
;; original file doesn't contain the content of the transclusion.
;; I still don't get why this works while my other solutions not.
(defun lgm/publish-org-roam-with-transclusions (github-repo-dir output-dir branch)
  "Export current Org-roam file to Jekyll-compatible HTML5 with transclusions expanded."
  (interactive
   (list (read-directory-name "GitHub Repo Directory: " working-publishing-repository)
         (read-string "Output Directory (relative to repo root): " working-publishing-directory)
         (read-string "Branch: " working-publishing-branch)))
  (let* ((org-file (buffer-file-name))
         (temp-file (concat (file-name-sans-extension org-file) "-expanded.org")))

    (unless org-file
      (error "This buffer is not visiting a file"))
    (unless (derived-mode-p 'org-mode)
      (error "This function only works in Org mode buffers"))

    ;; Create and open the temp file
    (with-temp-file temp-file
      (insert-file-contents org-file)
      (org-mode)  ;; Ensure we are in Org mode
      (org-transclusion-mode 1)
      (org-transclusion-add-all)  ;; Expand transclusions
      (org-roam-db-sync))  ;; Make sure Org-roam recognizes it

    ;; Publish using the expanded file
    (let ((buffer (find-file-noselect temp-file)))
      (with-current-buffer buffer
        (setq-local buffer-file-name temp-file)
        (save-buffer)
        (lgm/publish-org-roam-to-jekyll-html github-repo-dir output-dir branch)))

    ;; Clean up
    (delete-file temp-file)
    (message "Published with transclusions expanded.")))

;; The attic
(defun lgm/publish-the-attic-node (github-repo-dir output-dir branch)
  "Publish the current Org Roam node (or file) to the GitHub Pages site under /the-attic/.
If called inside a heading with :the_attic: tag, exports just that node. Otherwise, exports the entire file.
Uses TITLE property (if any) and DATE to build the HTML file name.
Updates _data/attic.yml with the filename and title.
Asks whether to commit and push to GitHub after export."
  (interactive
   (list (read-directory-name "GitHub Repo Directory: " "~/repos/lgmoneda.github.io/")
         (read-string "Output Directory (relative to repo root): " "the-attic/")
         (read-string "Branch: " "master")))
  (unless (derived-mode-p 'org-mode)
    (error "This function only works in Org mode buffers"))
  (let* ((org-file (buffer-file-name))
         (node (org-roam-node-at-point))
         (is-node node)
         (tags (when is-node (org-get-tags)))
         (date (org-entry-get (point) "DATE"))
		 (author (org-entry-get (point) "AUTHOR"))
		 (collection (org-entry-get (point) "COLLECTION"))
         (custom-title (org-entry-get (point) "TITLE"))
         (export-html-file nil)
         (basename nil)
         (title-for-yml (or custom-title (and node (org-roam-node-title node))))
         (output-path (expand-file-name output-dir github-repo-dir))
         (image-dir (expand-file-name "images/the-attic/" github-repo-dir))
         (image-paths (make-hash-table :test 'equal))
         (attic-yml-path (expand-file-name "_data/attic.yml" github-repo-dir)))

    (when (and is-node (not (member "the_attic" tags)))
      (error "Node is not marked with :the_attic: tag"))
    (make-directory output-path t)
    (make-directory image-dir t)
    (org-roam-db-sync)

    ;; Sanitize title to be used in filename (remove accents, symbols)
    (defun lgm/sanitize-filename (s)
  "Sanitize string S to be used in a filename: remove accents and special chars."
  (let* ((decomposed (ucs-normalize-NFD-string s))
         ;; Remove diacritics (Unicode marks)
         (no-diacritics (replace-regexp-in-string "[\u0300-\u036f]" "" decomposed))
         ;; Replace anything not a-z, A-Z, 0-9, hyphen or underscore with hyphen
         (ascii-only (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" no-diacritics))
         ;; Collapse multiple hyphens
         (clean (replace-regexp-in-string "--+" "-" ascii-only)))
    (downcase (string-trim clean "-+" "-+"))))

    (setq basename
          (if is-node
              (let ((slug (lgm/sanitize-filename (or custom-title (org-roam-node-title node)))))
                (if date
                    (format "%s-%s.html"
                            (format-time-string "%Y-%m-%d" (org-time-string-to-time date))
                            slug)
                  (error "Node is missing DATE in the property drawer")))
            (file-name-nondirectory (file-name-sans-extension org-file))))

    ;; Inject custom TITLE into buffer for export
    (when custom-title
      (add-hook 'org-export-before-processing-hook
                (lambda (_backend)
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\+TITLE:.*$" nil t)
                      (replace-match (concat "#+TITLE: " custom-title))
                    (goto-char (point-min))
                    (insert (concat "#+TITLE: " custom-title "\n"))))
                nil t))

    ;; Export HTML
    (setq export-html-file
          (if is-node
              (let ((org-export-with-toc nil)
                    (org-export-with-section-numbers nil))
                (org-export-to-file 'html (make-temp-file "attic" nil ".html") nil t))
            (org-html-export-to-html)))

;; Handle images
(let ((link-parser
       (if is-node
           ;; Parse only the content of the current node (subtree)
           (let* ((beg (save-excursion
                         (org-back-to-heading t)
                         (point)))
                  (end (save-excursion
                         (org-end-of-subtree t t)
                         (point)))
                  (content (buffer-substring-no-properties beg end)))
             (with-temp-buffer
               (insert content)
               (org-mode)
               (org-element-parse-buffer)))
         ;; Else parse the whole buffer
         (org-element-parse-buffer))))
  (org-element-map link-parser 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "file")
        (let* ((image-path (org-element-property :path link))
               (absolute-image-path (expand-file-name image-path (file-name-directory org-file)))
               (relative-image-path (file-relative-name absolute-image-path (file-name-directory org-file)))
               (image-filename (file-name-nondirectory image-path))
               (destination-image-path (expand-file-name image-filename image-dir)))
          (when (file-exists-p absolute-image-path)
            (copy-file absolute-image-path destination-image-path t)
            (puthash relative-image-path
                     (concat "../images/the-attic/" image-filename)
                     image-paths)))))))


    ;; Clean and finalize HTML
    (let ((final-output-file (expand-file-name basename output-path)))
      (copy-file export-html-file final-output-file t)
      (with-temp-buffer
        (insert-file-contents final-output-file)

		;; Replace <title> in <head> with custom-title, if present
        (when (and custom-title (not (string-empty-p custom-title)))
          ;; Replace HTML <title> tag (tab title)
          (goto-char (point-min))
          (when (re-search-forward "<title>.*?</title>" nil t)
            (replace-match (format "<title>%s</title>" (org-html-encode-plain-text custom-title)) t t))

		;; Replace main <h1> heading with custom-title, if present
        (when (and custom-title (not (string-empty-p custom-title)))
          (goto-char (point-min))
          (when (re-search-forward "<h1[^>]*>.*?</h1>" nil t)
            (replace-match (format "<h1 class=\"title\">%s</h1>" (org-html-encode-plain-text custom-title)) t t)))

        ;; Remove postamble div
        (goto-char (point-min))
        (when (re-search-forward "<div id=\"postamble\" class=\"status\">\\(?:.\\|\n\\)+?</div>" nil t)
          (replace-match ""))

        ;; Replace image paths
        (maphash
         (lambda (key value)
           (goto-char (point-min))
           (while (search-forward key nil t)
             (replace-match value t t)))
         image-paths)
        (write-file final-output-file)

		;; Add collection title at the end if present
		(when (and collection (not (string-empty-p collection)))
		  (goto-char (point-max))
		  (insert (format "<br><br><p><i>%s</i></p>" (org-html-encode-plain-text collection))))

		;; Add author at the end if present
		(when (and author (not (string-empty-p author)))
		  (goto-char (point-max))
		  (insert (format "<p>- %s</p>\n" (org-html-encode-plain-text author))))
		)

      (message "Exported to: %s" final-output-file)

      ;; Update _data/attic.yml
      (when is-node
        (let* ((yml-full-path attic-yml-path)
               (yml-filename (file-name-sans-extension basename))
               (yml-entry (format "  - filename: \"%s\"\n    title: \"%s\"\n"
                                  yml-filename title-for-yml))
               (new-entries nil)
               (found nil))
          (with-temp-buffer
            (if (file-exists-p yml-full-path)
                (insert-file-contents yml-full-path)
              (insert "entries:\n"))

            (goto-char (point-min))
            (if (re-search-forward "^entries:" nil t)
                (forward-line))

            ;; Parse entries and check for duplicates
            (while (re-search-forward "  - filename: \"\\([^\"]+\\)\"" nil t)
              (let ((existing-fn (match-string 1)))
                (if (string= existing-fn yml-filename)
                    (setq found t))))

            ;; If not found, append new entry
            (unless found
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert yml-entry)
              (write-region (point-min) (point-max) yml-full-path)
              (message "Added new entry to %s" yml-full-path)))))

      ;; Ask whether to commit
      (let* ((choices '("No" "Yes"))
             (choice (completing-read "Do you want to commit and push to GitHub? " choices nil t nil nil "No")))
        (when (string= choice "Yes")
          (let ((default-directory github-repo-dir))
            (shell-command (format "git add %s" (shell-quote-argument final-output-file)))
            (shell-command (format "git add %s" (shell-quote-argument attic-yml-path)))
            (shell-command (format "git add %s" (shell-quote-argument image-dir)))
            (shell-command (format "git commit --no-gpg-sign -m 'Publish attic piece: %s'" basename))
            (shell-command (format "git push origin %s" branch))
            (message "Changes committed and pushed to GitHub"))))))))

;; em busca das referências perdidas
(defun lgm/org-roam-export-proust-page ()
  "Export Org-roam file into custom HTML for Proust immersive experience, handling images, decorations, and bibliography."
  (interactive)
  (let* ((repo-root "/Users/luis.moneda/repos/lgmoneda.github.io/")
         (output-file (expand-file-name "em-busca-do-tempo-perdido/test.html" repo-root))
         (image-dir (expand-file-name "images/em-busca-do-tempo-perdido/" repo-root))
         (sections '())
         (image-paths (make-hash-table :test 'equal))
         (org-file (buffer-file-name))
         (current-buffer (current-buffer))
         (bib-html-file (make-temp-file "bib-" nil ".html")))  ;; temp file for bib

    (make-directory image-dir t)

    ;; Process each top-level node (LEVEL=1)
    (with-current-buffer current-buffer
      (org-map-entries
       (lambda ()
         (let* ((props (org-entry-properties))
                (id (cdr (assoc "ID" props)))
                (title (or (cdr (assoc "TITLE" props))
                           (nth 4 (org-heading-components))))
                (img (cdr (assoc "IMG" props)))
                (alt (cdr (assoc "ALT" props)))
                (decor-class (cdr (assoc "DECORATIONS" props)))
                (subtree-begin (save-excursion (org-back-to-heading t) (point)))
                (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
                (content-begin (save-excursion
                                 (org-back-to-heading t)
                                 (forward-line 1)
                                 (point)))
                (subtree-content (if (< content-begin subtree-end)
                                   (buffer-substring-no-properties content-begin subtree-end)
                                   ""))
                (subtree-html
                 (with-temp-buffer
                   (insert subtree-content)
                   (org-mode)
                   (let ((org-export-with-toc nil)
                         (org-export-with-section-numbers nil)
                         (org-export-with-title nil)
                         (org-html-with-latex nil)
                         (org-export-with-sub-superscripts nil)
                         (org-export-with-author nil)
                         (org-export-with-date nil)
                         (org-export-with-creator nil)
                         ;; Bibliography settings
                         (org-cite-global-bibliography '("/Users/luis.moneda/Dropbox/Research/library.bib"))
                         (org-cite-export-processors '((html csl "/Users/luis.moneda/Dropbox/Research/custom-style-blog.csl"))))
                     (org-export-string-as (buffer-string) 'html t)))))

           ;; Parse and copy images
           (let ((parsed-subtree
                  (with-temp-buffer
                    (insert subtree-content)
                    (org-mode)
                    (org-element-parse-buffer))))
             (org-element-map parsed-subtree 'link
               (lambda (link)
                 (when (string= (org-element-property :type link) "file")
                   (let* ((image-path (org-element-property :path link))
                          (absolute-path (expand-file-name image-path (file-name-directory org-file)))
                          (filename (file-name-nondirectory image-path))
                          (dest-path (expand-file-name filename image-dir)))
                     (when (file-exists-p absolute-path)
                       (copy-file absolute-path dest-path t)
                       (puthash image-path (concat "../images/em-busca-do-tempo-perdido/" filename) image-paths)))))))

           ;; Replace image paths in subtree HTML
           (maphash
            (lambda (key value)
              (setq subtree-html (replace-regexp-in-string (regexp-quote key) value subtree-html)))
            image-paths)

           ;; Build section HTML
           (push
            (cond
             ((string= id "intro")
              (format "
<section class=\"intro-section\" id=\"%s\">
  <div class=\"intro-decorations\">
    <div class=\"decoration-element proust-portrait\">
      <img src=\"%s\" alt=\"%s\" />
    </div>
  </div>
  <div class=\"intro-content\">
    <h2>%s</h2>
    <div class=\"intro-text\">
      %s
    </div>
  </div>
</section>" id img (or alt "") title subtree-html))

             (decor-class
              (format "
<section class=\"full-bleed-section\" id=\"%s\">
  <div class=\"image-container\">
    <img src=\"%s\" alt=\"%s\" />
    <div class=\"text\">%s</div>
  </div>
  <div class=\"content-section %s-decoration\">
    <div class=\"text-content\">
      %s
    </div>
  </div>
</section>" id img (or alt "") title decor-class subtree-html))

             (t
              (format "
<section class=\"full-bleed-section\" id=\"%s\">
  <div class=\"image-container\">
    <img src=\"%s\" alt=\"%s\" />
    <div class=\"text\">%s</div>
  </div>
  <div class=\"content-section\">
    <div class=\"text-content\">
      %s
    </div>
  </div>
</section>" id img (or alt "") title subtree-html)))
            sections)))
       "LEVEL=1"))

    ;; Step: Export full buffer bibliography using Pandoc
    (let ((pandoc-command
           (format "pandoc %s -o %s --from=org --to=html5 --citeproc --bibliography=/Users/luis.moneda/Dropbox/Research/library.bib --csl=/Users/luis.moneda/Dropbox/Research/custom-style-blog.csl"
                   (shell-quote-argument org-file)
                   (shell-quote-argument bib-html-file))))
      (shell-command-to-string pandoc-command))

    ;; Read bibliography fragment
    ;; Read bibliography fragment (excluding footnotes)
    (let ((bibliography-html
	   (with-temp-buffer
             (insert-file-contents bib-html-file)
             (goto-char (point-min))
             (if (re-search-forward "<div[^>]+id=\"refs\"[^>]*>" nil t)
		 (let ((start (match-beginning 0)))
		   (if (re-search-forward "</div>" nil t)
                       (buffer-substring-no-properties start (point))
                     ""))  ;; If no closing </div>, return nothing
               ""))))  ;; If no refs found, return nothing

      ;; Generate final HTML
      (let ((main-content (mapconcat #'identity (nreverse sections) "\n\n")))
        (with-temp-file output-file
          (insert main-content "\n\n<section class=\"bibliography\">\n"
                  bibliography-html
                  "\n</section>"))
        (delete-file bib-html-file)
        (message "✅ Exported Proust HTML with bibliography to: %s" output-file)))))

(provide 'publishing-settings)
;;; publishing-settings.el ends here
