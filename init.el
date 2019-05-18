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
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
   (package-install 'use-package))

;; Paradox (package-list)
(use-package paradox
    :ensure t)

;; Fast init.el open
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

(defconst setting-dir (expand-file-name "settings" user-emacs-directory))

;; Change custom file to the aesthetics one
(setq custom-file "~/.emacs.d/settings/aesthetics-settings.el")

(add-to-list 'load-path setting-dir)

(require 'editor-settings)
(require 'aesthetics-settings)
(require 'elisp-settings)
(require 'programming-settings)
(require 'writing-settings)
(require 'git-settings)
(require 'org-settings)
(require 'nu-settings)
(require 'os-settings)
(require 'email-settings)
(require 'productivity-settings)
(require 'media-settings)
(require 'python-settings)

(eval-after-load "scala" '(require 'scala-settings))
(eval-after-load "clojure" '(require 'clojure-settings))
