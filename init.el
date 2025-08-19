;Sunday, December 10, 2017
;============================
;==   Package Management   ==
;============================

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
     '("melpa-stable" . "https://stable.melpa.org/packages/"))
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

;; Fast init.el open
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

(defconst setting-dir (expand-file-name "settings" user-emacs-directory))

;; Change custom file to the aesthetics one
(setq custom-file "~/.emacs.d/settings/aesthetics-settings.el")

(add-to-list 'load-path setting-dir)
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
(require 'python-settings)
(require 'gpt-settings)
(require 'assistant-settings)
(require 'catalyst-settings)
(require 'org-roam-link-recommendations-settings)
(require 'publishing-settings)
(require 'reading-settings)
(require 'copilot-settings)
(require 'claude-code-ide-settings)
(require 'emacs-mcp-server-settings)
;; (require 'processing-settings)
;; (require 'nu-settings)
;; (require 'email-settings)
(require 'media-settings)
;; (require 'clojure-settings)
;; (require 'scala-settings)
;; (require 'elfeed-settings)
