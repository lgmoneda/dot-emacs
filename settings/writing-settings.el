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

(use-package latex-preview-pane
	     :ensure t)

;;(add-hook 'LaTeX-mode-hook 'flymake-mode)
;;(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
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
;; (use-package markdown-mode
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "multimarkdown"))

(add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/markdown-preview-mode-20230707.803")
(load "markdown-preview-mode")
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
  :ensure t)

;; ivy-bibtex
;; (use-package helm-bibtex
;;   :ensure t)
(add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/helm-bibtex")
(autoload 'helm-bibtex "helm-bibtex" "" t)
(load "helm-bibtex")

;; (use-package ivy-bibtex
;;   :ensure t)
(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(global-set-key (kbd "C-c r") 'org-ref-insert-cite-link)
(setq org-cite-global-bibliography '("~/Dropbox/Research/library.bib"))
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

;; telling bibtex-completion where your bibliographies can be found
(setq bibtex-completion-bibliography "~/Dropbox/Research/library.bib")

(use-package gscholar-bibtex
  :ensure t)
;; Where to add bibtex from google scholar
(setq gscholar-bibtex-database-file "~/Dropbox/Research/library.bib")

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
         (lambda (fpath) (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath))))
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
  :ensure t)
;; (add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/org-ref-20230131.1743")
;; (autoload 'org-ref "org-ref" "" t)

(setq org-ref-bibliography-notes "~/Dropbox/Agenda/roam"
      org-ref-default-bibliography '("~/Dropbox/Research/library.bib")
      org-ref-pdf-directory "~/Dropbox/Research/Literature/"
      org-ref-completion-library 'org-ref-ivy-cite
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
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-enable-ido-completion t)
  (require 'google-translate-smooth-ui)
  (global-set-key "\C-ce" 'google-translate-smooth-translate)
  (global-set-key "\C-ct" 'google-translate-query-translate-reverse)
  (global-set-key "\C-cE" 'google-translate-at-point)
  (global-set-key "\C-cT" 'google-translate-at-point-reverse)
  (setq google-translate-translation-directions-alist
      '(("pt" . "en") ("en" . "pt") ("pt" . "fr") ("fr" . "pt")))
  )

;; Fixes the tkk error
(defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))

;; Useful to activate while I write my cards
(global-set-key "\C-cf" 'flyspell-mode)

;; (add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/lsp-grammarly-20230814.1827")
;; (add-to-list 'load-path "/Users/luis.moneda/.emacs.d/elpa/lsp-mode-20230401.434")
(use-package lsp-mode
  :ensure t)
;; Hide the filename when active
(setq lsp-headerline-breadcrumb-enable nil)
;; (load "lsp-grammarly")
;; (autoload 'lsp-grammarly "lsp-grammarly" "" t)

(use-package lsp-grammarly
  :ensure t)

;; https://github.com/emacs-lsp/lsp-ui
;; Show the recommendation in a box when I hover the mouse
(setq lsp-ui-doc-show-with-cursor t)


;; Make sure lsp-auto-guess-root is not set to t.
;; run M-x lsp-workspace-blacklist-remove, if you get an error
;; Use s-l a a to fix the error (lsp-execute-code-action)
(defun lgm/activate-lsp-grammarly ()
  (interactive)
    (require 'lsp-grammarly)
    (lsp)
    )

;; annotate-mode
;; c-c c-a to annotate
;; c-c c-d to exclude
;; c-c [ to jump to next note
(use-package annotate
  :ensure t)

(provide 'writing-settings)
;;; writing-settings.el ends here
