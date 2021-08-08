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
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Useful to copy Latex chunks
(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C--") 'lgm/select-until-next-occurence))

;; Use M-x markdown-preview-mode in a md buffer
(use-package markdown-preview-mode
  :ensure t)

;; Use flymd-flyit
(use-package flymd
  :ensure t)

;; Dict.cc wrap
(add-to-list 'load-path "~/.emacs.d/elisp/dict-cc" t)
(require 'dict-cc)
;; PATH append
(setenv "PATH" (concat "/Users/luis.moneda/opt/miniconda3/bin:" (getenv "PATH")))

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


;; Suggests good academic phrases
;; Use m-x academic-phrases or academic-phrases-by-section
(use-package academic-phrases
  :ensure t)

(use-package reftex
             :ensure t)

;; (use-package ivy-bibtex
;;   :ensure t)

;;In order to get support for many of the LaTeX packages you will use in your documents,
;;you should enable document parsing as well, which can be achieved by putting
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;;if you often use \include or \input, you should make AUCTeX aware of
;;the multi-file document structure. You can do this by inserting

;;(setq-default TeX-master nil)

;;  automatically insert  ‘\(...\)’ in LaTeX files by pressing $
;; (add-hook 'LaTeX-mode-hook
;;           (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
;;                      (cons "\\(" "\\)" ))))
;; The linebreaks will be inserted automatically if auto-fill-mode is enabled.
;; In this case the source is not only filled but also indented automatically
;; as you write it.

;; Avoid sp messing with $ for math, but loses it for ()
;; (add-hook 'LaTeX-mode-hook (lambda () (sp-pair "(" nil :actions nil)))

;; (add-hook 'LaTeX-mode-hook (lambda () (sp-pair ")" nil :actions nil)))
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
  (forward-char 1)
  (insert (spaces-string 1))
  )

(eval-after-load 'tex-mode
  '(define-key latex-mode-map (kbd "C-c k") 'lgm/single-dollar-sign))

(global-set-key (kbd "C-c k") 'lgm/single-dollar-sign)
(global-set-key (kbd "C-c j") 'lgm/jump-out-of-math)
;; Useful to copy math chunks in latex
(eval-after-load 'tex-mode
  '(define-key latex-mode-map (kbd "C--") 'lgm/select-until-next-occurence))

;; use PDF-Tools
(pdf-tools-install)

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

;; ivy-bibtex

(use-package helm-bibtex
  :ensure t)
(use-package ivy-bibtex
  :ensure t)
(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(global-set-key (kbd "C-c r") 'ivy-bibtex)
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

;; telling bibtex-completion where your bibliographies can be found
(setq bibtex-completion-bibliography "~/Dropbox/Research/library.bib")

(use-package gscholar-bibtex
  :ensure t)
;; Where to add bibtex from google scholar
(setq gscholar-bibtex-database-file "~/Dropbox/Research/library.bib")


;; specify the path of the note
;; (setq bibtex-completion-notes-path "~/Documents/Research/ref.org")
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
(setq bibtex-completion-library-path '("~/Documents/Research/Literature"))
;;(setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation)

;; adds an action with Evince as an external viewer bound to P,
;; in addition to the regular Emacs viewer with p
(defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  (let ((bibtex-completion-pdf-open-function
         (lambda (fpath) (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath))))
    (bibtex-completion-open-pdf keys fallback-action)))

;;uncomment
(ivy-bibtex-ivify-action bibtex-completion-insert-citation ivy-bibtex-insert-citation)

;; (ivy-add-actions
;;  'ivy-bibtex
;;  '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))

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

;; This is a small utility for Web of Science/Knowledge (WOK) (http://apps.webofknowledge.com).
;; (require 'org-ref-wos)

;; (require 'doi-utils)


(provide 'init-latex)

(use-package magic-latex-buffer
  :ensure t)

;; Use flymd-flyit
(use-package writegood-mode
  :ensure t)

;; Google translation to support writing in another language
(use-package google-translate
  :ensure t
  :init
  (setq google-translate-backend-method 'curl)
  (setq google-translate-default-source-language "pt")
  (setq google-translate-default-target-language "eng")
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-enable-ido-completion t)
  (require 'google-translate-smooth-ui)
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  (global-set-key "\C-ce" 'google-translate-query-translate-reverse)
  (global-set-key "\C-cT" 'google-translate-at-point)
  (global-set-key "\C-cE" 'google-translate-at-point-reverse)
  (setq google-translate-translation-directions-alist
      '(("pt" . "en") ("en" . "pt") ("pt" . "fr") ("fr" . "pt")))
  )

;; Fixes the tkk error
(defun google-translate--get-b-d1 ()
    ;; TKK='427110.1469889687'
  (list 427110 1469889687))

;; Useful to activate while I write my cards
(global-set-key "\C-cf" 'flyspell-mode)

(use-package langtool
  :ensure t
  :init
  (setq langtool-language-tool-jar "~/Documents/LanguageTool-5.1/languagetool-commandline.jar"))

(use-package writegood-mode
  :ensure t)

(use-package  wwg
  :ensure t)

(provide 'writing-settings)
;;; writing-settings.el ends here
