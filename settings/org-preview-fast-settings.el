;;; org-preview-fast-settings.el --- Faster Org LaTeX previews  -*- lexical-binding: t; -*-

;; Temporary speedup layer for Org LaTeX previews.
;;
;; This intentionally lives apart from `org-settings.el' so it can be removed
;; when the upstream Org preview improvements are available here.  To remove:
;;
;;   1. Delete this file.
;;   2. Delete `(require 'org-preview-fast-settings)' from init.el.
;;
;; Based on the batching/async approach from:
;; https://github.com/karthink/org-preview

(defun lgm/org-preview-fast--region-has-unpreviewed-latex-p (beg end)
  "Return non-nil when BEG..END contains a LaTeX fragment needing preview."
  (let ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"))
    (save-excursion
      (goto-char (or beg (point-min)))
      (catch 'found
        (while (re-search-forward math-regexp (or end (point-max)) t)
          (unless (eq (get-char-property (point) 'org-overlay-type)
                      'org-latex-overlay)
            (let ((type (org-element-type (org-element-context))))
              (when (memq type '(latex-environment latex-fragment))
                (throw 'found t)))))
        nil))))

(defun lgm/org-preview-fast--skip-empty-input (orig prefix &optional beg end dir overlays msg forbuffer processing-type)
  "Avoid `org-preview' errors when Org asks to preview an empty range."
  (when (lgm/org-preview-fast--region-has-unpreviewed-latex-p beg end)
    (funcall orig prefix beg end dir overlays msg forbuffer processing-type)))

(defvar-local lgm/org-preview-fast--startup-refresh-timer nil
  "Delayed refresh timer for Org LaTeX previews in the current buffer.")

(defun lgm/org-preview-fast--run-startup-refresh (buffer)
  "Refresh missing LaTeX previews in BUFFER after Org startup preview."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'org-mode)
                 org-startup-with-latex-preview
                 (bound-and-true-p org-preview-mode)
                 (lgm/org-preview-fast--region-has-unpreviewed-latex-p
                  (point-min) (point-max)))
        (condition-case err
            (save-excursion
              (save-restriction
                (widen)
                (org-latex-preview '(16))))
          (error
           (message "Delayed Org LaTeX preview refresh failed: %s"
                    (error-message-string err))))))))

(defun lgm/org-preview-fast--schedule-startup-refresh ()
  "Schedule a delayed refresh for buffers using `#+STARTUP: latexpreview'."
  (when lgm/org-preview-fast--startup-refresh-timer
    (cancel-timer lgm/org-preview-fast--startup-refresh-timer))
  (setq lgm/org-preview-fast--startup-refresh-timer
        (run-with-idle-timer
         1.25 nil #'lgm/org-preview-fast--run-startup-refresh
         (current-buffer))))

(use-package org-preview
  :straight (:host github :repo "karthink/org-preview")
  :after org
  :config
  (advice-add 'org-preview-format-latex
              :around #'lgm/org-preview-fast--skip-empty-input)

  (org-preview-mode 1)

  ;; The fast path batches all fragments into one temporary TeX document and
  ;; asks dvisvgm to convert every page in one process.
  ;;
  ;; `-jobname %b' is important: org-preview feeds the TeX file on stdin, so
  ;; without an explicit jobname xelatex writes texput.xdv instead of the file
  ;; dvisvgm is told to read.
  (let ((dvisvgm-process (alist-get 'dvisvgm org-preview-latex-process-alist)))
    (setq dvisvgm-process
          (plist-put dvisvgm-process
                     :latex-compiler
                     '("xelatex -no-pdf -jobname %b -interaction nonstopmode -output-directory %o")))
    (setq dvisvgm-process
          (plist-put dvisvgm-process
                     :image-converter
                     '("dvisvgm --page=1- -n -b min -c %S -o %B-%%9p.svg %O")))
    (setf (alist-get 'dvisvgm org-preview-latex-process-alist)
          dvisvgm-process))

  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview--debug-msg nil)

  (add-hook 'org-mode-hook #'lgm/org-preview-fast--schedule-startup-refresh)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (lgm/org-preview-fast--schedule-startup-refresh)))))

(provide 'org-preview-fast-settings)
;;; org-preview-fast-settings.el ends here
