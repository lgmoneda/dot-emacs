(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'subr-x)
(require 'url-util)

(defconst lgm/test-image-clipboard-settings-file
  (expand-file-name "../settings/productivity-settings.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst lgm/test-image-clipboard-sample-path
  "/Users/luis.moneda/Dropbox/Agenda/roam/internal_money_transfer_agent/scripts/data/imt_ssr.png")

(defconst lgm/test-image-clipboard-sample-markdown
  "![IMT SSR Plot](/Users/luis.moneda/Dropbox/Agenda/roam/internal_money_transfer_agent/scripts/data/imt_ssr.png)")

(defconst lgm/test-image-clipboard-sample-log-line
  "   done  read  /Users/luis.moneda/Dropbox/manager-ai-agent/context/quicksight_snapshots/agents_report_comparison.png")

(defun lgm/test-load-image-clipboard-definitions ()
  "Load only the image clipboard definitions from the productivity settings file."
  (with-temp-buffer
    (insert-file-contents lgm/test-image-clipboard-settings-file)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((form (condition-case nil
                      (read (current-buffer))
                    (end-of-file nil))))
        (when (and (listp form)
                   (memq (car form) '(defun defconst))
                   (let ((name (nth 1 form)))
                     (memq name '(lgm/image-clipboard-mime-types
                                  lgm/image-clipboard-extensions
                                  lgm/macos-image-clipboard-types
                                  lgm--image-mime-type
                                  lgm--resolve-local-file-path
                                  lgm--existing-image-file-p
                                  lgm--image-path-on-current-line
                                  lgm--markdown-link-target-at-point
                                  lgm--org-file-link-at-point
                                  lgm--image-file-at-point
                                  lgm--call-process-or-user-error
                                  lgm--copy-image-file-to-clipboard-macos
                                  lgm--copy-image-file-to-clipboard-linux
                                  lgm--copy-image-file-to-clipboard
                                  lgm/copy-image-at-point-to-clipboard))))
          (eval form))))))

(defun lgm/test-appkit-available-p ()
  "Return non-nil if `python3' can import AppKit."
  (and (executable-find "python3")
       (zerop
        (call-process "python3" nil nil nil
                      "-c" "from AppKit import NSPasteboard"))))

(defun lgm/test-macos-clipboard-has-png-p ()
  "Return non-nil if the macOS clipboard currently contains PNG data."
  (with-temp-buffer
    (let ((status
           (call-process
            "python3" nil t nil
            "-c"
            (concat
             "from AppKit import NSPasteboard, NSPasteboardTypePNG\n"
             "pb = NSPasteboard.generalPasteboard()\n"
             "print('true' if pb.dataForType_(NSPasteboardTypePNG) is not None else 'false')\n"))))
      (and (zerop status)
           (string= (string-trim (buffer-string)) "true")))))

(defun lgm/test-with-sample-markdown-buffer (fn)
  "Call FN in a temp buffer containing the sample Markdown image link."
  (with-temp-buffer
    (insert lgm/test-image-clipboard-sample-markdown "\n")
    (goto-char (point-min))
    (search-forward "imt_ssr.png")
    (backward-char 5)
    (funcall fn)))

(defun lgm/test-with-sample-log-line-buffer (fn)
  "Call FN in a temp buffer containing the sample log line."
  (with-temp-buffer
    (insert lgm/test-image-clipboard-sample-log-line "\n")
    (goto-char (point-min))
    (search-forward "read")
    (backward-char 2)
    (funcall fn)))

(lgm/test-load-image-clipboard-definitions)

(ert-deftest lgm/copy-image-at-point-resolves-markdown-image ()
  (should (file-exists-p lgm/test-image-clipboard-sample-path))
  (lgm/test-with-sample-markdown-buffer
   (lambda ()
     (should (equal (lgm--image-file-at-point)
                    lgm/test-image-clipboard-sample-path)))))

(ert-deftest lgm/copy-image-at-point-resolves-image-from-log-line ()
  (should (file-exists-p "/Users/luis.moneda/Dropbox/manager-ai-agent/context/quicksight_snapshots/agents_report_comparison.png"))
  (lgm/test-with-sample-log-line-buffer
   (lambda ()
     (should
      (equal (lgm--image-file-at-point)
             "/Users/luis.moneda/Dropbox/manager-ai-agent/context/quicksight_snapshots/agents_report_comparison.png")))))

(ert-deftest lgm/copy-image-at-point-copies-png-to-macos-clipboard ()
  (skip-unless (eq system-type 'darwin))
  (skip-unless (file-exists-p lgm/test-image-clipboard-sample-path))
  (skip-unless (lgm/test-appkit-available-p))
  (lgm/test-with-sample-markdown-buffer
   (lambda ()
     (lgm/copy-image-at-point-to-clipboard)))
  (should (lgm/test-macos-clipboard-has-png-p)))
