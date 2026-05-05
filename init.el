;Sunday, December 10, 2017
;============================
;==   Package Management   ==
;============================

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu" . 20)
        ("nongnu" . 15)
        ("melpa" . 10)
        ("melpa-stable" . 5)))
(setq package-install-upgrade-built-in t)
(package-initialize)

(require 'cl-lib)
(unless (fboundp 'incf)
  (defalias 'incf #'cl-incf))
(unless (fboundp 'decf)
  (defalias 'decf #'cl-decf))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
   (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el by default with use-package
;; (setq straight-use-package-by-default t)

;; Install use-package via straight
(straight-use-package 'use-package)
(straight-use-package 'compat)
(require 'compat)
(unless (fboundp 'compat--seconds-to-string)
  (defun compat--seconds-to-string (delay &optional readable abbrev precision)
    "Compatibility fallback for Emacs 31's extended `seconds-to-string'."
    (if (not readable)
        (seconds-to-string delay)
      (let* ((delay (abs delay))
             (units '((31536000 "year" "y")
                      (604800 "week" "w")
                      (86400 "day" "d")
                      (3600 "hour" "h")
                      (60 "minute" "m")
                      (1 "second" "s")))
             (parts nil)
             (remaining delay))
        (dolist (unit units)
          (let* ((seconds (nth 0 unit))
                 (name (nth 1 unit))
                 (short (nth 2 unit))
                 (count (floor (/ remaining seconds))))
            (when (and (> count 0)
                       (or (eq readable 'expanded)
                           (null parts)))
              (push (if abbrev
                        (format "%d%s" count short)
                      (format "%d %s%s" count name (if (= count 1) "" "s")))
                    parts)
              (setq remaining (- remaining (* count seconds))))))
        (or (mapconcat #'identity (nreverse parts) " ")
            (if abbrev "0s" "0 seconds"))))))
(straight-use-package 'transient)
(require 'transient)

;; Force ELPA Org to shadow built-in Org
(add-to-list 'load-path "~/.emacs.d/elpa/org-9.7.34/" t)

;; Fast init.el open
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; Loading my setting files
(defconst setting-dir (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path setting-dir)

;; Change custom file to the aesthetics one
(setq custom-file "~/.emacs.d/settings/aesthetics-settings.el")

;; Add my AI assistant configs
(add-to-list 'load-path "~/repos/catalyst-assistant/")

(require 'editor-settings)
(require 'productivity-settings)
(require 'aesthetics-settings)
(require 'elisp-settings)
(require 'programming-settings)
(require 'writing-settings)
(require 'git-settings)
(require 'os-settings)
(require 'org-settings)
(require 'org-agenda-server-settings)
(require 'python-settings)
(require 'gpt-settings)
(require 'assistant-settings)
(require 'catalyst-settings)
(require 'org-roam-link-recommendations-settings)
(require 'publishing-settings)
(require 'reading-settings)
(require 'copilot-settings)
(require 'claude-code-ide-settings)
(require 'custom-emacs-mcp-server-settings)
(require 'media-settings)
;; (require 'scala-settings)
;; (require 'clojure-settings)
;; (require 'nu-settings)
;; (require 'processing-settings)
;; (require 'email-settings)
;; (require 'elfeed-settings)
