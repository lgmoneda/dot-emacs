;Sunday, December 10, 2017
;============================
;==   Package Management   ==
;============================

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

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

;; Ensure Emacs doesn’t ignore Org updates from ELPA
(setq package-install-upgrade-built-in t)

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
