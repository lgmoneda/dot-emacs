;Sunday, December 10, 2017  -*- lexical-binding: t; -*-
;Sunday, December 10, 2017
;============================
;==   Package Management   ==
;============================

(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(require 'cl-lib)
(unless (fboundp 'incf)
  (defalias 'incf #'cl-incf))
(unless (fboundp 'decf)
  (defalias 'decf #'cl-decf))

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

;; Keep package.el out of package installation. Package-managed use-package
;; forms must opt into straight with :straight.
(setq straight-use-package-by-default nil)
(setq use-package-always-ensure nil)

(straight-use-package 'use-package)
(require 'use-package)
(straight-use-package 'org)
(require 'org)
(straight-use-package 'compat)
(require 'compat)
(unless (fboundp 'set-local)
  (defun set-local (variable value)
    "Make VARIABLE buffer-local and set it to VALUE."
    (set (make-local-variable variable) value)))
(straight-use-package 'transient)
(require 'transient)

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
;; (require 'org-agenda-server-settings)
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
