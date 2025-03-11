;;; reading-settings.el --- Settings for support reading

;Monday, March 10, 2025
;============================
;==        Reading         ==
;============================

(require 'url)
(require 'dom)
(require 'shr)
(require 'cl-lib)

(defun url-to-epub (url title)
  "Download the webpage at URL and export it as a valid EPUB book with TITLE."
  (interactive "sEnter URL: \nsEnter EPUB title: ")
  (unless (stringp url)
    (error "Expected a string, but got: %S" url))

  (let* ((parsed-url (url-generic-parse-url url))
         (base-url (if parsed-url (url-recreate-url parsed-url)
                     (error "Invalid URL: %s" url)))
         (temp-buffer (url-retrieve-synchronously url))
         (content "")
         (dom nil)
         (body nil)
         (html-content "")
         (images '())
         (epub-dir (concat (file-name-as-directory "/tmp/url-epub") title))
         (epub-oebps-dir (concat epub-dir "/OEBPS"))
         (epub-images-dir (concat epub-oebps-dir "/images"))
         (epub-meta-dir (concat epub-dir "/META-INF"))
         (output-epub (concat (expand-file-name "~/Downloads/") title ".epub"))
         (epub-ncx (concat epub-oebps-dir "/toc.ncx"))
         (image-items "")  ; String to collect image manifest entries
         (image-counter 0))  ; Counter for unique image IDs

    (when temp-buffer
      (with-current-buffer temp-buffer
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n" nil t)
        (setq content (buffer-substring-no-properties (point) (point-max)))
        (setq dom (libxml-parse-html-region (point) (point-max)))
        (setq body (dom-by-tag dom 'body))
        (setq html-content (shr-dom-to-xml body))
        (setq images (dom-by-tag dom 'img)))
      (kill-buffer temp-buffer))

    ;; Create necessary directories
    (mkdir epub-dir t)
    (mkdir epub-oebps-dir t)
    (mkdir epub-images-dir t)
    (mkdir epub-meta-dir t)

    ;; Create mimetype file (must be uncompressed in the EPUB archive)
    (with-temp-file (concat epub-dir "/mimetype")
      (insert "application/epub+zip"))

    ;; Create META-INF/container.xml
    (with-temp-file (concat epub-meta-dir "/container.xml")
      (insert "<?xml version=\"1.0\"?>\n"
              "<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">\n"
              "  <rootfiles>\n"
              "    <rootfile full-path=\"OEBPS/content.opf\" media-type=\"application/oebps-package+xml\"/>\n"
              "  </rootfiles>\n"
              "</container>"))

    ;; Download and prepare images
    (dolist (img images)
      (let* ((src (dom-attr img 'src))
             (absolute-src (cond
                            ((not src) nil)
                            ((string-match-p "\\`https?://" src) src)
                            ((string-prefix-p "//" src) (concat (url-type parsed-url) ":" src))
                            ((string-prefix-p "/" src) (concat (url-type parsed-url) "://" (url-host parsed-url) src))
                            (t (url-expand-file-name src base-url))))
             (image-id (format "img%d" (cl-incf image-counter)))
             (image-filename (and absolute-src (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "_" 
                                                                         (file-name-nondirectory absolute-src))))
             (local-filename (and image-filename (concat epub-images-dir "/" image-filename))))
        
        (when (and absolute-src local-filename)
          (condition-case err
              (progn
                (message "Downloading image: %s" absolute-src)
                (url-copy-file absolute-src local-filename t)
                ;; Add to manifest
                (setq image-items 
                      (concat image-items 
                              (format "<item id=\"%s\" href=\"images/%s\" media-type=\"image/%s\"/>\n"
                                      image-id
                                      image-filename
                                      (cond
                                       ((string-match-p "\\.jpe?g$" image-filename) "jpeg")
                                       ((string-match-p "\\.png$" image-filename) "png")
                                       ((string-match-p "\\.gif$" image-filename) "gif")
                                       ((string-match-p "\\.svg$" image-filename) "svg+xml")
                                       (t "png")))))
                ;; Update src attribute to point to the local file
                (setf (dom-attr img 'src) (concat "images/" image-filename)))
            (error (message "Error downloading image %s: %s" absolute-src (error-message-string err)))))))

    ;; Save XHTML content
    (with-temp-file (concat epub-oebps-dir "/index.xhtml")
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")
      (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n")
      (insert "<head>\n")
      (insert "<title>" title "</title>\n")
      (insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n")
      (insert "</head>\n")
      (insert "<body>" (shr-dom-to-xml body) "</body>\n</html>"))

    ;; Create OEBPS/toc.ncx
    (with-temp-file epub-ncx
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\">\n")
      (insert "<head>\n")
      (insert "<meta name=\"dtb:uid\" content=\"12345678-1234-1234-1234-123456789abc\"/>\n")
      (insert "</head>\n")
      (insert "<docTitle><text>" title "</text></docTitle>\n")
      (insert "<navMap>\n")
      (insert "<navPoint id=\"index\" playOrder=\"1\">\n")
      (insert "<navLabel><text>" title "</text></navLabel>\n")
      (insert "<content src=\"index.xhtml\"/>\n")
      (insert "</navPoint>\n")
      (insert "</navMap>\n")
      (insert "</ncx>"))

    ;; Create OEBPS/content.opf with images in manifest
    (with-temp-file (concat epub-oebps-dir "/content.opf")
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"2.0\" unique-identifier=\"BookID\">\n")
      (insert "<metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n")
      (insert "<dc:title>" title "</dc:title>\n")
      (insert "<dc:identifier id=\"BookID\">urn:uuid:12345678-1234-1234-1234-123456789abc</dc:identifier>\n")
      (insert "<dc:language>en</dc:language>\n")
      (insert "</metadata>\n")
      (insert "<manifest>\n")
      (insert "<item id=\"ncx\" href=\"toc.ncx\" media-type=\"application/x-dtbncx+xml\"/>\n")
      (insert "<item id=\"index\" href=\"index.xhtml\" media-type=\"application/xhtml+xml\"/>\n")
      ;; Add all image items to manifest
      (insert image-items)
      (insert "</manifest>\n")
      (insert "<spine toc=\"ncx\">\n<itemref idref=\"index\"/>\n</spine>\n")
      (insert "</package>"))

    ;; Zip files into an EPUB
    (shell-command (format "cd %s && zip -X0 %s mimetype && zip -r %s META-INF OEBPS"
                           epub-dir output-epub output-epub))
    (message "EPUB saved as %s" output-epub)))

(defun url-to-epub-clean (url title)
  "Download the webpage at URL, extract the main content, and export it as a valid EPUB book with TITLE."
  (interactive "sEnter URL: \nsEnter EPUB title: ")
  (unless (stringp url)
    (error "Expected a string, but got: %S" url))

  (let* ((parsed-url (url-generic-parse-url url))
         (base-url (if parsed-url (url-recreate-url parsed-url)
                     (error "Invalid URL: %s" url)))
         (temp-buffer (url-retrieve-synchronously url))
         (content "")
         (dom nil)
         (main-content nil)
         (page-title nil)
         (html-content "")
         (images '())
         (epub-dir (concat (file-name-as-directory "/tmp/url-epub") title))
         (epub-oebps-dir (concat epub-dir "/OEBPS"))
         (epub-images-dir (concat epub-oebps-dir "/images"))
         (epub-meta-dir (concat epub-dir "/META-INF"))
         (output-epub (concat (expand-file-name "~/Downloads/") title ".epub"))
         (epub-ncx (concat epub-oebps-dir "/toc.ncx"))
         (image-items "")
         (image-counter 0))

    (when temp-buffer
      (with-current-buffer temp-buffer
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n" nil t)
        (setq content (buffer-substring-no-properties (point) (point-max)))
        (setq dom (libxml-parse-html-region (point) (point-max)))
        
        ;; Extract page title if available
        (let ((title-element (car (dom-by-tag dom 'title))))
          (when title-element
            (setq page-title (dom-text title-element))))
        
        ;; Extract meaningful content using various content selectors
        (setq main-content 
              (or
               ;; Try to find article element
               (car (dom-by-tag dom 'article))
               ;; Try to find main element
               (car (dom-by-tag dom 'main))
               ;; Try common content container IDs
               (car (dom-by-id dom "content"))
               (car (dom-by-id dom "main-content"))
               (car (dom-by-id dom "post-content"))
               (car (dom-by-id dom "article-content"))
               ;; Try by class names (common in many CMS)
               (car (dom-by-class dom "content"))
               (car (dom-by-class dom "post"))
               (car (dom-by-class dom "article"))
               (car (dom-by-class dom "entry-content"))
               (car (dom-by-class dom "post-content"))
               ;; Fallback to body if no content identifiers found
               (car (dom-by-tag dom 'body))))
        
        ;; If no main content was found, use the body
        (unless main-content
          (setq main-content (car (dom-by-tag dom 'body))))
        
        ;; Make a deep copy of main-content to prevent modifying original DOM
        (setq main-content (copy-tree main-content))
        
        ;; Remove unwanted elements from main content
        (when main-content
          ;; Remove navigation elements (nav tags)
          (let ((navs (dom-by-tag main-content 'nav)))
            (dolist (nav navs)
              (dom-remove-node main-content nav)))
          
          ;; Remove headers
          (let ((headers (dom-by-tag main-content 'header)))
            (dolist (header headers)
              (dom-remove-node main-content header)))
          
          ;; Remove footers
          (let ((footers (dom-by-tag main-content 'footer)))
            (dolist (footer footers)
              (dom-remove-node main-content footer)))
          
          ;; Remove asides
          (let ((asides (dom-by-tag main-content 'aside)))
            (dolist (aside asides)
              (dom-remove-node main-content aside)))
          
          ;; Remove elements by common class names
          (dolist (class '("nav" "navigation" "menu" "sidebar" "widget" "footer" "header" 
                           "comments" "social" "related" "meta" "tags" "author-info"))
            (let ((elements (dom-by-class main-content class)))
              (dolist (el elements)
                (dom-remove-node main-content el))))
          
          ;; Extract all remaining images for processing
          (setq images (dom-by-tag main-content 'img))
          
          ;; Extract all images before converting to HTML
          (setq images (dom-by-tag main-content 'img))))
      (kill-buffer temp-buffer))

    ;; Create necessary directories
    (mkdir epub-dir t)
    (mkdir epub-oebps-dir t)
    (mkdir epub-images-dir t)
    (mkdir epub-meta-dir t)

    ;; Create mimetype file (must be uncompressed in the EPUB archive)
    (with-temp-file (concat epub-dir "/mimetype")
      (insert "application/epub+zip"))

    ;; Create META-INF/container.xml
    (with-temp-file (concat epub-meta-dir "/container.xml")
      (insert "<?xml version=\"1.0\"?>\n"
              "<container version=\"1.0\" xmlns=\"urn:oasis:names:tc:opendocument:xmlns:container\">\n"
              "  <rootfiles>\n"
              "    <rootfile full-path=\"OEBPS/content.opf\" media-type=\"application/oebps-package+xml\"/>\n"
              "  </rootfiles>\n"
              "</container>"))

    ;; Download and prepare images
    (let ((image-map (make-hash-table :test 'equal)))
      (dolist (img images)
        (let* ((src (dom-attr img 'src))
               (absolute-src (cond
                              ((not src) nil)
                              ((string-match-p "\\`https?://" src) src)
                              ((string-prefix-p "//" src) (concat (url-type parsed-url) ":" src))
                              ((string-prefix-p "/" src) (concat (url-type parsed-url) "://" (url-host parsed-url) src))
                              (t (url-expand-file-name src base-url))))
               (image-id (format "img%d" (cl-incf image-counter)))
               (image-filename nil)
               (local-filename nil))

          ;; Generate a safe filename for the image
          (when absolute-src
            (setq image-filename 
                  (if (string-match "/\\([^/]*\\)$" absolute-src)
                      (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "_" 
                                               (match-string 1 absolute-src))
                    (format "image_%d.png" image-counter)))
            
            ;; Ensure we have a file extension
            (unless (string-match "\\.[a-zA-Z0-9]+$" image-filename)
              (setq image-filename (concat image-filename ".png")))
            
            (setq local-filename (concat epub-images-dir "/" image-filename))
            
            (condition-case err
                (progn
                  (message "Downloading image: %s" absolute-src)
                  ;; Create a safe URL for downloading
                  (let ((encoded-url (url-encode-url absolute-src)))
                    (url-copy-file encoded-url local-filename t))
                  
                  ;; Add to manifest if file exists
                  (when (file-exists-p local-filename)
                    (setq image-items 
                          (concat image-items 
                                  (format "<item id=\"%s\" href=\"images/%s\" media-type=\"image/%s\"/>\n"
                                          image-id
                                          image-filename
                                          (cond
                                           ((string-match-p "\\.jpe?g$" image-filename) "jpeg")
                                           ((string-match-p "\\.png$" image-filename) "png")
                                           ((string-match-p "\\.gif$" image-filename) "gif")
                                           ((string-match-p "\\.svg$" image-filename) "svg+xml")
                                           (t "png")))))
                    ;; Store the mapping between original src and local filename
                    (puthash src (concat "images/" image-filename) image-map)
                    ;; Update src attribute to point to the local file
                    (setf (dom-attr img 'src) (concat "images/" image-filename))))
              (error (message "Error downloading image %s: %s" absolute-src (error-message-string err)))))))
      
      ;; Convert main content to HTML after updating all image src attributes
      (setq html-content (shr-dom-to-xml main-content)))

    ;; Preserve original title if found, otherwise use the provided title
    (unless page-title
      (setq page-title title))
    
    ;; Add the page title as an H1 heading at the beginning of the content
    (setq html-content (concat "<h1>" page-title "</h1>" html-content))

    ;; Save XHTML content with proper styling
    (with-temp-file (concat epub-oebps-dir "/index.xhtml")
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")
      (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n")
      (insert "<head>\n")
      (insert "<title>" page-title "</title>\n")
      (insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n")
      (insert "<style type=\"text/css\">\n")
      (insert "body { font-family: serif; margin: 1em; line-height: 1.4; }\n")
      (insert "h1, h2, h3, h4, h5, h6 { font-family: sans-serif; }\n")
      (insert "img { max-width: 100%; height: auto; display: block; margin: 1em 0; }\n") 
      (insert "a { color: #0066cc; text-decoration: none; }\n")
      (insert "p { margin: 0.5em 0; }\n")
      (insert "</style>\n")
      (insert "</head>\n")
      (insert "<body>" html-content "</body>\n</html>"))

    ;; Create OEBPS/toc.ncx with original page title
    (with-temp-file epub-ncx
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<ncx xmlns=\"http://www.daisy.org/z3986/2005/ncx/\" version=\"2005-1\">\n")
      (insert "<head>\n")
      (insert "<meta name=\"dtb:uid\" content=\"12345678-1234-1234-1234-123456789abc\"/>\n")
      (insert "</head>\n")
      (insert "<docTitle><text>" page-title "</text></docTitle>\n")
      (insert "<navMap>\n")
      (insert "<navPoint id=\"index\" playOrder=\"1\">\n")
      (insert "<navLabel><text>" page-title "</text></navLabel>\n")
      (insert "<content src=\"index.xhtml\"/>\n")
      (insert "</navPoint>\n")
      (insert "</navMap>\n")
      (insert "</ncx>"))

    ;; Create OEBPS/content.opf with original page title
    (with-temp-file (concat epub-oebps-dir "/content.opf")
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (insert "<package xmlns=\"http://www.idpf.org/2007/opf\" version=\"2.0\" unique-identifier=\"BookID\">\n")
      (insert "<metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n")
      (insert "<dc:title>" page-title "</dc:title>\n")
      (insert "<dc:identifier id=\"BookID\">urn:uuid:12345678-1234-1234-1234-123456789abc</dc:identifier>\n")
      (insert "<dc:language>en</dc:language>\n")
      (insert "</metadata>\n")
      (insert "<manifest>\n")
      (insert "<item id=\"ncx\" href=\"toc.ncx\" media-type=\"application/x-dtbncx+xml\"/>\n")
      (insert "<item id=\"index\" href=\"index.xhtml\" media-type=\"application/xhtml+xml\"/>\n")
      ;; Add all image items to manifest
      (insert image-items)
      (insert "</manifest>\n")
      (insert "<spine toc=\"ncx\">\n<itemref idref=\"index\"/>\n</spine>\n")
      (insert "</package>"))

    ;; Zip files into an EPUB
    (shell-command (format "cd %s && zip -X0 %s mimetype && zip -r %s META-INF OEBPS"
                           epub-dir output-epub output-epub))
    (message "EPUB saved as %s" output-epub)))

(provide 'reading-settings)
;;; reading-settings.el ends here
