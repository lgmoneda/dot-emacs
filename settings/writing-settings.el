;;; editor-settings.el --- Settings for writing

;; LaTeX!
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
;; (setq TeX-PDF-mode t)

;; automatic formatting of a section: C-c C-q C-s;
;; section preview: C-c C-p C-s;

(require 'flymake)

(defun flymake-get-tex-args (file-name)
(list "pdflatex"
      (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

 ;; Built-in preview from AUCTeX
(use-package tex
    :ensure auctex
    :hook ((LaTeX-mode . TeX-PDF-mode)          ; always work in PDF mode
           (LaTeX-mode . LaTeX-math-mode)
           (LaTeX-mode . LaTeX-preview-setup))  ; enable preview.el integration
    :custom
    (preview-image-type 'png)                   ; crisp previews
    (preview-auto-cache-preamble t)             ; cache the preamble to speed up rerenders
    (preview-scale-function 1.2))               ; adjust size to taste

;; Optional: richer, font-aware previews without external viewers
(use-package engrave-faces
  :straight (:type git :host github :repo "tecosaur/engrave-faces" :branch "master")
    :hook ((LaTeX-mode . engrave-faces-mode)))

;; Exclude if the above config works fine after using it a couple of times
;; (use-package latex-preview-pane
;; 	     :ensure t)
;;(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

;;(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; use RefTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

(setq reftex-plug-into-AUCTeX t)

(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
'(reftex-use-external-file-finders t)

;;To hide all the contents of your current section, use C-c C-o C-l. You can apply it to a chapter, subsection, etc. You can also move to a next unit of your document with C-c C-o C-n, or to a previous one with C-c C-o C-p. If you’re lost and want to see the whole document again, use C-c C-o C-a.
(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;; Markdown mode and preview
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; I define it differently in another place, take a look when needed
  :init (setq markdown-command "multimarkdown"))

;; (add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/markdown-preview-mode-20230707.803")
;; (load "markdown-preview-mode")
(autoload 'markdown-preview-mode "markdown-preview-mode" "" t)

(use-package websocket
  :ensure t)

;; Use M-x markdown-preview-mode in a md buffer
(use-package markdown-preview-mode
  :ensure t)

;; Use flymd-flyit
(use-package flymd
  :ensure t)

;; PATH append
(setenv "PATH" (concat "/Users/luis.moneda/miniconda3/bin:" (getenv "PATH")))

;;; LATEX

;; latex editor
;packages
;;; Install epdfinfo via 'brew install --HEAD dunn/homebrew-emacs/pdf-tools' and then install the
;;; pdf-tools elisp via the use-package below. To upgrade the epdfinfo
;;; server, just do 'brew upgrade pdf-tools' prior to upgrading to newest
;;; pdf-tools package using Emacs package system. If things get messed
;;; up, just do 'brew uninstall pdf-tools', wipe out the elpa
;;; pdf-tools package and reinstall both as at the start.
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
    '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
;; (pdf-tools-install)

(use-package reftex
             :ensure t)

;;In order to get support for many of the LaTeX packages you will use in your documents,
;;you should enable document parsing as well, which can be achieved by putting
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

(setq TeX-electric-math '("$" . "$"))
(defun lgm/single-dollar-sign (&optional arg)
  (interactive "p")
  (insert "$ $")
  (backward-char 2)
  (insert (read-char ""))
  (delete-forward-char 1)
  )

(defun lgm/jump-out-of-math()
  (interactive)
  (forward-char 2)
  (insert (make-string 1 ? ))
  )

(eval-after-load 'tex-mode
  '(define-key latex-mode-map (kbd "C-c k") 'lgm/single-dollar-sign))

(global-set-key (kbd "C-c k") 'lgm/single-dollar-sign)
(global-set-key (kbd "C-c j") 'lgm/jump-out-of-math)

(setq TeX-view-program-selection '((output-pdf "pdf-tools"))
      TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server t
      )

;; Update PDF buffers after successful LaTeX runs
;;Autorevert works by polling the file-system every auto-revert-interval seconds, optionally combined with some event-based reverting via file notification. But this currently does not work reliably, such that Emacs may revert the PDF-buffer while the corresponding file is still being written to (e.g. by LaTeX), leading to a potential error.

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(global-set-key (kbd "C-c C-g") 'pdf-sync-forward-search)
(setq mouse-wheel-follow-mouse t)
(setq pdf-view-resize-factor 1.10)

;; use RefTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(setq reftex-plug-into-AUCTeX t)

(use-package biblio-core
  :ensure t
  :init
  (setq biblio-bibtex-file "~/Dropbox/Research/library.bib")
  )

(setq org-cite-global-bibliography '("/Users/luis.moneda/Dropbox/Research/library.bib"))

;; telling bibtex-completion where your bibliographies can be found
(setq bibtex-completion-bibliography "~/Dropbox/Research/library.bib")

;; References
;; https://kristofferbalintona.me/posts/202206141852/
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html
(use-package citar
  ;; :straight t
  :ensure t
  :after (org-roam-bibtex org)
  :init
  ;; Set it early, so citar sees it even before :config
  (setq citar-bibliography '("~/Dropbox/Research/library.bib"))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-org-roam-capture-template-key "r")
  (citar-symbols
   `((file ,(all-the-icons-faicon "file-pdf-o" :face 'all-the-icons-red) . " ")
     (note ,(all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow) . " ")))
  (citar-symbol-separator "  ")
  ;; Have citation link faces look closer to as they were for `org-ref'
  (org-cite ((t (:foreground "DarkSeaGreen4"))))
  (org-cite-key ((t (:foreground "forest green" :slant italic))))
  :hook
  ((LaTeX-mode . citar-capf-setup)
   (org-mode . citar-capf-setup))
  :config
  (require 'citar-embark)
  (citar-embark-mode 1)
  (global-set-key (kbd "C-c r") 'org-cite-insert))

;; This removes visually the [] from org official cite syntax
;; It is important to not actually remove to keep functionality
(add-hook 'org-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\\(\\[\\)\\([a-zA-Z0-9]+\\(?:/[a-zA-Z0-9]+\\)*:[^]]+\\)\\(\\]\\)"
                (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "")))
                (3 (prog1 () (compose-region (match-beginning 3) (match-end 3) "")))))
             'append)))


(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;; (setq citar-at-point-function 'embark-act)
(setq citar-at-point-function 'embark-dwim)

(use-package gscholar-bibtex
  :ensure t)
;; Where to add bibtex from google scholar
(setq gscholar-bibtex-database-file "~/Dropbox/Research/library.bib")
(setq gscholar-bibtex-default-source "Google Scholar")

;; Since gscholar sometimes fails and I have to edit my bib file directly, here's a convenient way of doing it
(defun lgm/open-bib ()
  "Open the BibTeX library file."
  (interactive)
  (find-file "~/Dropbox/Research/library.bib"))

;;If one file per publication is preferred, bibtex-completion-notes-path should point to the directory used for storing the notes files:
(setq bibtex-completion-notes-path "~/Dropbox/Agenda/roam/")

;; Customize layout of search results
;; first add journal and booktitle to the search fields
(setq bibtex-completion-additional-search-fields '(journal booktitle))
(setq bibtex-completion-display-formats
      '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
        (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
        (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
        (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
        (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))

;; Symbols used for indicating the availability of notes and PDF files
(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")

;; default is to open pdf - change that to insert citation
(setq bibtex-completion-pdf-field "File")
;; the pdf should be renamed with the BibTeX key
(setq bibtex-completion-library-path '("~/Dropbox/Research/Literature"))

;; adds an action with Evince as an external viewer bound to P,
;; in addition to the regular Emacs viewer with p
(defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  (let ((bibtex-completion-pdf-open-function
         (lambda (fpath) (start-process "zathura" "/usr/bin/zathura" fpath))))
    (bibtex-completion-open-pdf keys fallback-action)))

;;uncomment
;;(ivy-bibtex-ivify-action bibtex-completion-insert-citation ivy-bibtex-insert-citation)

;; If you store files in various formats, then you can specify a list
;; Extensions in this list are then tried sequentially until a file is found.
(setq bibtex-completion-pdf-extension '(".pdf"))

;; format of ciations
;; For example, people who don’t use Ebib might prefer links to the PDFs
;; instead of Ebib-links in org mode files

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))

;; use org-ref
(use-package org-ref
  :ensure t
  :after org
  :hook (org-mode . (lambda () (require 'org-ref))))

;; (add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/org-ref-20230131.1743")
;; (autoload 'org-ref "org-ref" "" t)

(setq org-ref-bibliography-notes "~/Dropbox/Agenda/roam"
      org-ref-default-bibliography '("~/Dropbox/Research/library.bib")
      org-ref-pdf-directory "~/Dropbox/Research/Literature/"
	  org-ref-default-citation-link 'citet
      )

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; Sometimes it is necessary to tell bibtex what dialect you are using to support the different bibtex entries that are possible in biblatex. You can do it like this globally.
(bibtex-set-dialect 'BibTeX)

;; open pdf
(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(provide 'init-latex)

;; Google translation to support writing in another language
(use-package google-translate
  :ensure t
  :init
  (setq google-translate-backend-method 'curl)
  (setq google-translate-default-source-language "pt")
  (setq google-translate-default-target-language "en")
  ;; So it doesn't jump to it
  (setq google-translate-pop-up-buffer-set-focus nil)
  (setq google-translate-enable-ido-completion t)
  (require 'google-translate-smooth-ui)
  ;; cycle between languages using C-n and C-p
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist
      '(("pt" . "en") ("en" . "pt") ("pt" . "fr") ("fr" . "pt")))
  )

;; Fixes the tkk error from google-translate
(defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))

;;
(setq ispell-program-name "/opt/homebrew/bin/aspell")
;; Useful to activate while I write my cards
(global-set-key "\C-cf" 'flyspell-mode)

(use-package lsp-mode
  :ensure t)
;; Hide the filename when active
(setq lsp-headerline-breadcrumb-enable nil)

;; https://github.com/emacs-lsp/lsp-ui
;; Show the recommendation in a box when I hover the mouse
(setq lsp-ui-doc-show-with-cursor t)

;; annotate-mode
;; C-c C-a to annotate
;; C-c C-d to exclude
;; C-c [ to jump to next note
;; Enable annotation-mode to do it
;; Alternatively, they can be integrated annotate-integrate-annotations as comments into the current buffer
;; C-c C-s (function annotate-show-annotation-summary)
(use-package annotate
  :ensure t)

;; Reading epubs in Emacs
(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Useful to use in images in eww and using org-download-clipboard
;; Press w over an image in eww, run the function, then use org-download-clipboard in a org buffer
(defun my/copy-image-from-url ()
  "Download an image from a URL in the kill-ring and copy it to the clipboard."
  (interactive)
  (let* ((url (current-kill 0))
         (temp-file (make-temp-file "emacs-image" nil ".png")))
    (url-copy-file url temp-file t)
    (cond
     ((eq system-type 'darwin)
      (shell-command (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as JPEG picture)'" temp-file)))
     ((eq system-type 'gnu/linux)
      (shell-command (format "xclip -selection clipboard -t image/png -i %s" temp-file)))
     ((eq system-type 'windows-nt)
      (message "Windows support not implemented yet!")))
    (message "Image copied to clipboard from %s" url)))

;; I'm unsure if I will need it. Check in the future.
;; (use-package lsp-ltex
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-ltex)
;;                        (lsp)))  ; or lsp-deferred
;;   :init
;;   (setq lsp-ltex-version "15.2.0"))  ; make sure you have set this, see below

(defun lgm/youtube-generate-bibtex (url)
  "Generate a BibTeX citation for a given YouTube URL and append it to the library.bib file."
  (interactive "sEnter YouTube URL: ")
  (message "Processing URL: %s" url)

  (let* ((api-key (getenv "GOOGLE_API_KEY"))
         (video-id (cond
                    ;; Match standard YouTube URLs
                    ((string-match "youtube\\.com/watch\\?v=\\([^&]+\\)" url)
                     (match-string 1 url))
                    ;; Match shortened youtu.be URLs
                    ((string-match "youtu\\.be/\\([^?&]+\\)" url)
                     (match-string 1 url))
                    ;; Match embedded URLs
                    ((string-match "youtube\\.com/embed/\\([^?&]+\\)" url)
                     (match-string 1 url))
                    ;; If no pattern matches, throw an error
                    (t (error "Invalid YouTube URL")))))

    (if (not api-key)
        (error "GOOGLE_API_KEY environment variable not set. Please set it before using this function.")

      (message "Extracted video ID: %s" video-id)

      (let* ((data-api-url (concat "https://www.googleapis.com/youtube/v3/videos?id="
                                   video-id
                                   "&part=snippet&key=" api-key))
             (curl-command (concat "curl -s \"" data-api-url "\""))
             json-string
             json-data
             items
             video-data
             snippet
             title
             author
             published-at
             year
             access-date
             entry)

        (message "Fetching data from YouTube Data API using curl")

        (condition-case err
            (progn
              (setq json-string (shell-command-to-string curl-command))
              (message "Received raw data (first 100 chars): %s"
                       (if (> (length json-string) 100)
                           (concat (substring json-string 0 100) "...")
                         json-string))

              ;; Parse JSON using alist objects instead of hash tables
              (setq json-data (json-parse-string json-string :object-type 'alist))

              ;; Extract items array
              (setq items (cdr (assoc 'items json-data)))

              ;; Check if we have items in the response
              (if (= 0 (length items))
                  (error "No video data found for ID: %s" video-id)

                ;; Get the first video item (should be the only one)
                (setq video-data (aref items 0))
                (setq snippet (cdr (assoc 'snippet video-data)))

                (setq title (cdr (assoc 'title snippet)))
                (setq author (cdr (assoc 'channelTitle snippet)))
                (setq published-at (cdr (assoc 'publishedAt snippet)))

                ;; Extract year from ISO date format (YYYY-MM-DDThh:mm:ss.sssZ)
                (setq year (if published-at
                               (substring published-at 0 4)
                             (format-time-string "%Y" (current-time))))

                (setq access-date (format-time-string "%Y-%m-%d" (current-time)))

                (message "Successfully retrieved video info: %s by %s (%s)" title author year)

                (setq entry (format "@misc{youtube:%s,
  author = {%s},
  title = {%s},
  year = {%s},
  url = {%s},
  note = {Accessed: %s}
}
" video-id author title year url access-date))

                (with-temp-buffer
                  (insert entry)
                  (append-to-file (point-min) (point-max) "~/Dropbox/Research/library.bib"))
                (message "BibTeX entry added: %s" entry)))

          (error (message "Error occurred: %s" (error-message-string err))))))))

;; Check it in the future: https://github.com/bradmont/page-view/

(provide 'writing-settings)
;;; writing-settings.el ends here
