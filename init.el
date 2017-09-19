;; Package Management
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

;; High contrast Zenburn theme
;; (use-package hc-zenburn-theme
;;   :ensure t)

;; Base16-theme, base16-dracula's home 
(use-package base16-theme 
  :ensure t)

;; Load Theme
;; Themes to use: monokai, deeper-blue and hc-zenburn
;; 'base16-dracula 'base16-gruvbox-dark-medium
;; 'base16-circus
(setq custom-safe-themes t)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (load-theme 'base16-circus)))

;; Custom faces:
;; Make selected text background #012050
;; Set matching parens background color and bold
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-search ((t (:inherit highlight :background "steel blue"))))
 '(company-tooltip-search-selection ((t (:background "steel blue"))))
 '(diff-hl-change ((t (:background "#3a81c3"))))
 '(diff-hl-delete ((t (:background "#ee6363"))))
 '(diff-hl-insert ((t (:background "#7ccd7c"))))
 '(ein:cell-input-area ((t (:background "black"))))
 '(isearch ((t (:foreground "white" :background "DarkOrchid"))))
 '(lazy-highlight ((t (:foreground "white" :background "SteelBlue"))))
 '(org-hide ((t (:foreground "#191919"))))
 '(region ((t (:background "#4C516D"))))
 '(show-paren-match ((t (:background "#5C888B" :weight bold)))))

;;

;; Fast init.el open
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
;; Open todo.org
(global-set-key (kbd "<f10>") (lambda() (interactive)(find-file "~/Dropbox/Agenda/todo.org")))

;; Save place
;; Start from the last place you were in a file the next time you visit it
;; It's a emacs24.5 or older way to do it
;; (require 'saveplace)
;; (setq-default save-place t)
;; For emacs 25+
(save-place-mode 1) 
(setq save-place-file "~/.emacs.d/saveplace.log")

;; Fast jump to elisp function
;; In Debian derived distros, the package emacs24 or similar
;; does not include the source elisp code, you need it to
;; jump to the definition (.el or .el.gz files). To download it
;; install emacs24-el package.
(defun lgm/describe-func ()
  (interactive)
  (describe-function (function-called-at-point)))

(defun lgm/jump-to-elisp-func-def ()
  (interactive)
  (find-function (function-called-at-point))
  ) 

(global-set-key (kbd "C-h C-j") 'lgm/jump-to-elisp-func-def)
(global-set-key (kbd "C-h C-f") 'lgm/describe-func)

;; Garbage Collector teste
(setq gc-cons-threshold 20000000)

;; Adjusting Mouse sensitivity
(setq mouse-wheel-progressive-speed nil)

;; Let Elisp be less conservative
(setq max-specpdl-size (* 15 max-specpdl-size))
(setq max-list-eval-depth (* 15 max-lisp-eval-depth))

;; Move cursor after split
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Do not centralize cursor
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Writeroom, a focus mode!
(use-package writeroom-mode
  :ensure t)

;; Magit
(use-package magit
  :ensure t)

;; Git-timemachine
(use-package git-timemachine
  :ensure t)

;; Dash
(use-package dash
  :ensure t)

;; Helm
(use-package helm
  :ensure t)

;; Try
(use-package try
  :ensure t)

;; Expand-region
(use-package expand-region
  :ensure t)

;; Try
(use-package column-enforce
  :ensure t
  :config (add-hook 'python-mode-hook 'column-enforce-mode))

;; Unique color identifier
;; (use-package color-identifiers-mode
;;   :ensure t
;;   :config (add-hook 'after-init-hook 'global-color-identifiers-mode))

;; Rainbow delimiters in Elisp mode 
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; thing-cmd
(use-package thing-cmds
  :ensure t)

;; Anzu shows total matchs for searchs
(use-package anzu 
	     :ensure t
	     :config (global-anzu-mode))

;; ;; Ido
;; (use-package ido
;;   :ensure t
;;   :init (ido-mode)
;;         (setq ido-enable-flex-matching t)
;;         (ido-everywhere t)
;; 	(setq ido-file-extensions-order '(".py" ".org" ".txt" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")))

;; (setq ido-use-faces nil)

;; ;; Ido-vertical
;; (use-package ido-vertical-mode
;;   :ensure t
;;   :init (ido-vertical-mode))

;; ;; Ido ubiquitous
;; (use-package ido-completing-read+
;;   :ensure t
;;   :init (ido-ubiquitous-mode 1)
;; )

;; ;; Flx-ido (fuzzy for ido)
;; (use-package flx-ido
;;   :ensure t
;;   :init (flx-ido-mode))

;Monday, September 18, 2017
;============================
;==          Ivy           ==
;============================

(use-package ivy
	     :ensure t
	     :init 
	     (ivy-mode 1)
	     (setq ivy-initial-inputs-alist nil)
	     ;; (setq ivy-use-virtual-buffers t)
	     ;; (setq enable-recursive-minibuffers t)

	     (setq ivy-re-builders-alist
		   '((ivy-switch-buffer . ivy--regex-plus)
		     (t . ivy--regex-fuzzy)))

	     (setq ivy-use-virtual-buffers t)
	     (setq ivy-count-format "(%d/%d) ")

	     ;; Use C-j for immediate termination with the current value, and RET
	     ;; for continuing completion for that directory. This is the ido
	     ;; behaviour.
	     (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
	     (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
	     )

(use-package counsel
	     :ensure t
	     :init (counsel-mode)
	     (global-set-key (kbd "M-X") 'counsel-M-x)
	     ;; (global-set-key (kbd "C-s") 'swiper)
	     (global-set-key (kbd "M-x") 'counsel-M-x)
	     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
	     (global-set-key (kbd "<f1> f") 'counsel-describe-function)
	     (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
	     (global-set-key (kbd "<f1> l") 'counsel-find-library)
	     (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
	     (global-set-key (kbd "<f2> u") 'counsel-unicode-char))

(use-package counsel-projectile
	     :ensure t
	     :init
	     (counsel-projectile-on))
;; imenu-anywhere
;; Changes the C-c C-j behavior
(use-package imenu-anywhere
  :ensure t
  :init (imenu-anywhere))

;; Ag (search)
(use-package ag
  :ensure t)

;; Smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :config
  ;; Using counsel now
  ;;(global-set-key (kbd "M-X") 'smex)
  ;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  ;;(global-set-key (kbd "M-x") 'execute-extended-command)
  )

;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config (setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))
  :bind (("C-c p s" . projectile-ag)
         ("C-c p g" . projectile-grep)))

;; Auto-complete
(use-package auto-complete
  :ensure t)

;; Neotree
(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle))

;Wednesday, August 23, 2017
;============================
;==         Scala          ==
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(add-hook 'scala-mode-hook (lambda () (setq tab-width 4)))

(cond ( (string-equal system-type "darwin")
    (progn (setq sbt:program-name "/usr/local/bin/sbt")
    (use-package ensime
      :ensure t
      :init
    (add-hook 'scala-mode-hook 'ensime-mode-hook)
      (setq
      ensime-sbt-command "/usr/local/bin/sbt"
      sbt:program-name "/usr/local/bin/sbt")
      )
    (setq ensime-exec-path "/usr/local/bin/sbt")
    )

    )

      ((string-equal system-type "gnu/linux")
	(setq sbt:program-name "/usr/bin/sbt")
	(use-package ensime
	  :ensure t
	  :init
	(add-hook 'scala-mode-hook 'ensime-mode-hook)
	  (setq
	  ensime-sbt-command "/usr/bin/sbt"
	  sbt:program-name "/usr/bin/sbt")
	)
	(setq ensime-exec-path "/usr/bin/sbt")
       )
      )

(defun scala-run () 
    (interactive)   
   (ensime-sbt-action "run")
   (ensime-sbt-action "~compile")
(let ((c (current-buffer)))
    (switch-to-buffer-other-window
   (get-buffer-create (ensime-sbt-build-buffer-name)))
 (switch-to-buffer-other-window c))) 
 (setq exec-path
(append exec-path (list ensime-exec-path))) ;;REPLACE THIS with the directory of your scalac executable!

(setq ensime-startup-snapshot-notification nil)
(setq ensime-eldoc-hints 'all)

;; Anaconda Anaconda+Eldoc
(use-package anaconda-mode
    :ensure t
    :diminish anaconda-mode
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (add-hook 'python-mode-hook 'python--add-debug-highlight)
    )

;; Column enforce
(use-package column-enforce-mode
	     :ensure t
	     :config
	     (add-hook 'python-mode-hook 'column-enforce-mode))

;; Company-anaconda
(use-package company-anaconda
  :ensure t
  :diminish
  :config
  (eval-after-load "company"
     '(add-to-list 'company-backends '(company-anaconda company-capf))))
    ;;'(add-to-list 'company-backends '(company-anaconda company-dabbrev))))

(add-hook 'python-mode-hook 'company-mode)

;; Enable eldoc in your programming modes
(use-package eldoc
  :ensure t
  :diminish 
  :commands eldoc-mode
  :init
  (setq eldoc-idle-delay 0.1
	eldoc-echo-area-use-multiline-p nil)
  (eldoc-mode 1)
  :config
  (add-hook 'prog-mode-hook 'turn-on-eldoc-mode))

;; Enable hide definitions functions
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key [f4] 'hs-toggle-hiding)

(use-package company
  :ensure t
  :diminish
  :defer 4
  :init (progn
          (global-company-mode)
          (setq company-global-modes '(not python-mode cython-mode sage-mode ein:notebook-modes org-mode markdown-mode))
          )
  :config (progn
            (setq company-tooltip-limit 12 
                  company-idle-delay 1.0
                  company-echo-delay 0.5
                  company-begin-commands '(self-insert-command  self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash )
                  company-transformers '(company-sort-by-occurrence)
                  company-selection-wrap-around t
                  company-minimum-prefix-length 2
                  company-dabbrev-downcase nil
		  company-require-match nil
                  )
;;	    (bind-keys :map company-mode-map
;;		       ("<tab>" . company-complete))
            (bind-keys :map company-active-map
		       ("C-s" . company-filter-candidates)
                       ("C-n" . company-select-next)
                       ("C-p" . company-select-previous)
		       ("C-d" . company-quickhelp-manual-begin)
                       ;;("C-d" . company-show-doc-buffer)
                       ("<tab>" . company--insert-candidate)
                       ("<escape>" . company-abort)
                       )
            )
  )

;; I don't want to see the error buffer
(remove-hook 'anaconda-mode-response-read-fail-hook
             'anaconda-mode-show-unreadable-response)

(use-package company-flx
  :ensure t)

(with-eval-after-load 'company
  (company-flx-mode +1))

;; Pop documentation help for Company
;; M-x customize-group <RET> company-quickhelp <RET>
(use-package company-quickhelp
  :ensure t
  ;; To see doc just press C-d in company candidate
  :init (company-quickhelp-mode 0)
  :config
  (eval-after-load 'company
  '(define-key company-active-map (kbd "C-d") #'company-quickhelp-manual-begin)))

;; Not so useful, but eventually...
(use-package helm-company
  :ensure t
  :config (eval-after-load 'company
  '(progn
     ;;(define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company))))

;; Pair parenthesis
(use-package smartparens
  :ensure t
  :init (smartparens-global-mode)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  :config
  ;; Now i can express sadness in erc-mode 
  (sp-local-pair 'erc-mode "(" nil :actions nil))

;; Smartparens keyibinding management
;; Sexp is a Symbolic Expression, something like (+ 40 2)

;; Big jump, jumps the outer ()
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; Enter the next at beginning ()
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
;; Enter the last sexp at end
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
;; Goes to the begining of the current sexp you're in
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
;; (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
;; Changes position under cursor element with the next
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)


(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)


;; (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;; (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;; (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;; (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;; (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)


(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

;; ?
(--each '(python-mode python)
  (eval-after-load it '(require 'smartparens-python)))

;; Display time in the mode-line
(setq display-time-format "%Hh%M ")
(setq display-time-default-load-average nil)
;; Hide the "MAIL" from mode-line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "dark slate gray")
 '(company-quickhelp-color-foreground "wheat")
 '(display-time-mail-string "")
 '(ein:use-auto-complete t t)
 '(ein:use-auto-complete-superpack t t)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   (quote
    (counsel-projectile counsel ivy exec-path-from-shell auctex default-text-scale org-gcal ess slack ensime writeroom-mode writeroom darkroom column-enforce-mode org-bullets latex-preview-pane scheme-complete quack org-dashboard org-journal restclient pyimport electric-operator multi diff-hl avy markdown-preview-mode markdown-mode ein beacon which-key highlight-current-line multiple-cursors smartparens helm-company company-quickhelp company-flx company-anaconda anaconda-mode neotree auto-complete projectile smex ag imenu-anywhere flx-ido ido-vertical-mode anzu thing-cmds rainbow-delimiters expand-region try helm magit base16-theme paradox use-package spinner monokai-theme hydra)))
 '(paradox-github-token t)
 '(region ((t (:background "#102050"))))
 '(show-paren-match ((t (:weight (quote extra-bold))))))
;;(display-time-mode 1)

;; Show time in mode-line when using Emacs in fullscreen,
;; avoiding using it three days in a row without sleeping
(global-set-key (kbd "<f9>") (lambda()
				(interactive)
				(toggle-frame-fullscreen)
				;; Now it works with multiple screens :)
				(if (eq (cdr (assoc 'fullscreen (frame-parameters))) 'fullboth)
				;; (if (eq display-time-mode nil)
				    (display-time-mode 1)
				    (display-time-mode 0))
				))

;; To activate nupy environment
(defun anupy ()
  (interactive)
  (pythonic-activate "~/miniconda2/envs/nupy")
  )

;; Check if i'm at work and activate
;; the right environment
(defun activate-work-env ()
  (if (string= (system-name) "deb3550")
      (pythonic-activate "~/miniconda2/envs/npytevec"))
  (if (string= (system-name) "rc530")
      (pythonic-activate "~/miniconda2/envs/ml")
      )
  (if (string-match "lgmac" (system-name))
      (pythonic-activate "~/miniconda2/envs/nupy")
      )
  )

(activate-work-env)

;; Multiple cursors
;; First mark the word, then add more cursors
;; If you want to insert a new line in multiple cursors mode, use C-j
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-x , m" . mc/edit-lines)
   ("C-x , n" . mc/edit-ends-of-lines)))

;; Some initial confis
;; UTF-8 please
(setq locale-coding-system 'utf-8) 
(set-terminal-coding-system 'utf-8) 
(set-keyboard-coding-system 'utf-8) 
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq  truncate-lines nil
       inhibit-startup-screen t
       initial-scratch-message ""
       fill-column 80)

;; Highligh current line! 
(use-package highlight-current-line
  :ensure t
  :config (highlight-current-line-on t)
  (set-face-background 'highlight-current-line-face "black")
  )

;; Replace highlighted text
(delete-selection-mode 1)


;; Make annoying yes or no less annoying
(defalias 'yes-or-no-p 'y-or-n-p)

;; Global visual line
(global-visual-line-mode t)
(diminish 'visual-line-mode)

;; Hiding menu, tool bar and scroll bar
(menu-bar-mode -99)
(tool-bar-mode -99)
(scroll-bar-mode -1)

;; More thinner window divisions
(fringe-mode '(3 . 2))

;; Outside border to make it better in fullscreen mode
(add-to-list 'default-frame-alist '(internal-border-width . 2))

;; Apply fringe width
;; (defun lgm/set-fringe(arg)
;;   (interactive)

;;   (print arg)
;;   ;; (fringe-mode '(arg . arg))
;;   )
;; (lgm/set-fringe 100)
;; (setq wel 100)
;; (fringe-mode '('wel . 'wel))


;; Fullscreen 
;;(toggle-frame-fullscreen)
(defun fullscreen ()
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

;;(run-with-idle-timer 0.1 nil (lambda () (fullscreen)) )
;;(toggle-frame-fullscreen)

;; Enable paren mode at start
(show-paren-mode 1)

;; Enable line numbers
(global-linum-mode 0)

;; Defining switch tabs commands
(global-set-key [C-iso-lefttab] 
    (lambda ()
      (interactive)
      (other-window -1)))

;; MacOS version
(global-set-key (kbd "C-S-<tab>") 
    (lambda ()
      (interactive)
      (other-window -1)))

;; Defining switch tabs commands
(global-set-key [C-tab] 
    (lambda ()
      (interactive)
      (other-window 1)))

;; Defining switch buffer command
(global-set-key (kbd "C-1")
    (lambda ()
      (interactive)
      (switch-to-prev-buffer)))

;; Defining switch buffer command
(global-set-key (kbd "C-'")
    (lambda ()
      (interactive)
      (bury-buffer)))

;; Defining switch frames command
(global-set-key (kbd "C-2")
    (lambda ()
      (interactive)
      (other-frame 1)))

;; Tryng to save my hand
(global-set-key (kbd "C-0")
    (lambda ()
      (interactive)
      (other-frame 1)))

;; Initialize in full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'upcase-region 'disabled nil)

;; Which-key minor mode
(use-package which-key
  :ensure t
  :init (which-key-mode))

;;Turn the system sound off
(setq ring-bell-function 'ignore)

;;Beacon minor mode
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init (beacon-mode 1)
        ;; For deeper-blue theme
        ;; (setq beacon-color "#00ff00")
        ;; For monokai theme
        ;;(setq beacon-color "#AE81FF")
	;; base16-dracula
        ;; (setq beacon-color "#ea51b2")
	;; gru-dark-medium
        ;; (setq beacon-color "#fb4934")
        ;; base16-circus
  	(setq beacon-color "#dc657d")
        (setq beacon-size 100)
	(setq beacon-blink-delay 0.5))

;; Emacs Ipython Notebook
(use-package ein
  :ensure t
  :init ;; (setq ein:use-auto-complete t)
;;  (setq ein:use-smartrep t)
  (setq auto-complete-mode t)
  (setq ein:output-type-prefer-pretty-text-over-html t)
  ;; (setq ein:output-type-preference
  ;; 	'(emacs-lisp image image/png svg image/svg image/png jpeg image/jpeg text html text/html latex text/latex javascript))
  )

;; (setq ein:use-auto-complete-superpack t)
;;(setq ein:use-smartrep t)

(setq ein:notebook-modes '(ein:notebook-multilang-mode ein:notebook-python-mode))

;; Hide your password when prompted
(require 'comint)
(setq comint-password-prompt-regexp
		    (concat comint-password-prompt-regexp
			    "\\|^Password for .*:\\s *\\'"))

;; Markdown mode and preview
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Use M-x markdown-preview-mode in a md buffer
(use-package markdown-preview-mode
  :ensure t)


;; Change selected text highlight color #102050


;; Disable edebug key binds

;; avy
(use-package avy
  :ensure t)

(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "C-x C-a") 'avy-goto-char)
(global-set-key (kbd "C-x a") 'avy-goto-char-timer)
(global-set-key (kbd "C-?") 'avy-goto-line)

;; Show differences between local and repo
(use-package diff-hl
  :ensure t
  :init
  (setq diff-hl-side 'left)
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)

  ;; defining the custom colors to the diff-hl

  (custom-set-faces
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-delete ((t (:background "#ee6363"))))))

;; Show in mode line info about current func body
;; It's only enabled in python-mode
;; (use-package which-func
;;   :ensure t
;;   :config
;;   (setq which-func-modes '(python-mode))
;;   (add-hook 'python-mode-hook '(lambda () (which-function-mode t))))

;; Add Hydra
(use-package hydra
  :ensure t)

;; custom package
;; load the custom helm-spotify-plus
(use-package multi
  :ensure t)

(load-file "~/repos/helm-spotify-plus/helm-spotify-plus.el")

;; Helm-spotify-plus key binds 
(global-set-key (kbd "C-c C-s") 'helm-spotify-plus)
(defhydra hydra-spotify (global-map "C-c s")
  "helm-spotify-plus"
 ("s" helm-spotify-plus)
 ("f" helm-spotify-plus-next)
 ("b" helm-spotify-plus-previous)
 ("p" helm-spotify-plus-play)
 ("g" helm-spotify-plus-pause))

;; Highlight matching tags
(load-file "~/.emacs.d/others/hl-tags-mode/hl-tags-mode.el")
(require 'hl-tags-mode)
(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode)))
(add-hook 'html-mode-hook (lambda () (hl-tags-mode)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode)))

;; Disable auto-save
(setq auto-save-default nil)
;; Disable backup
(setq backup-inhibited t)

;; Python
(require 'python)
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
(define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
                               (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t))))))

;; Insert a execution time print
(defun lgm/insert-timer()
  (interactive)
  (save-excursion 
   (back-to-indentation)
   (split-line)
   (insert "from time import time; start = time() ## Timing from here")
   (next-line)
   (insert "print('Execution time: {0}m{1}s'.format(int((time()-start)/60), int((time()-start)%60 )))"))
  )

(define-key python-mode-map (kbd "<f4>") 'lgm/insert-timer)


;; Run python and pop-up its shell.
;; Kill process to solve the reload modules problem.
(defun my-python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
     (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
     (kill-process (get-buffer-process "*Python*"))
     ;; If you want to clean the buffer too.
     ;;(kill-buffer "*Python*")
     ;; Not so fast!
     (sleep-for 0.5)
     )
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  ;; Pop new window only if shell isn't visible
  ;; in any frame.
  (unless (get-buffer-window "*Python*" t) 
    (python-shell-switch-to-shell)
    )
 )

(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell)
  )

(eval-after-load "python"
  '(progn
     (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-run)
     (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)
     ))

;; Trying to make TAB great again
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (dabbrev-expand nil)
    (indent-for-tab-command)
    ))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(dabbrev-expand nil))
    (if mark-active
	(indent-region (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (dabbrev-expand nil)
	(indent-for-tab-command)))))

(defun my-smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
    the minibuffer. Else, if mark is active, indents region. Else if
    point is at the end of a symbol, expands it. Else indents the
    current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
	(dabbrev-expand nil))
   (if mark-active
	(python-indent-shift-right (region-beginning)
		       (region-end))
      (if (looking-at "\\_>")
	  (dabbrev-expand nil)
	(indent-for-tab-command)))))

(defun my-smart-backtab ()
  (interactive)
  (if mark-active
      (python-indent-shift-left (region-beginning)
				 (region-end))
    (if (looking-at "\\_>")
	(dabbrev-expand nil)
      (python-indent-dedent-line))))

;;(define-key python-mode-map (kbd "TAB") 'indent-or-complete)
(define-key python-mode-map (kbd "TAB") 'my-smart-tab)
(define-key python-mode-map (kbd "<backtab>") 'my-smart-backtab)

;; Maybe it's a good idea to use it in all prog modes 
(define-key prog-mode-map (kbd "TAB") 'my-smart-tab)
(define-key prog-mode-map (kbd "<backtab>") 'my-smart-backtab)

(define-key markdown-mode-map (kbd "TAB") 'indent-or-complete)

;; Put white spaces between operators in Python
(use-package electric-operator
  :ensure t
  :config (add-hook 'python-mode-hook #'electric-operator-mode))

;; Warning about imports in python
;; Requires pyflakes
;; M-x pyimport-remove-unused
;; M-x pyimport-insert-missing
;; https://github.com/Wilfred/pyimport
(use-package pyimport
  :ensure t)

(define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)

;; Test http rest webservices inside emacs
;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t)

;;(define-key company-mode-map (kbd "TAB") 'company-complete-common)

;;Tuesday, August 15, 2017
;;============================
;;==          ERC           ==
;;============================
(add-to-list 'load-path "~/.emacs.d/elisp/erc-extras" t)

;; (require 'erc-hl-nicks)
;;(require 'erc-nicklist)
;; (require 'erc-notify)
;; (require 'erc-match)

;; Trying to display nicely
(erc-spelling-mode 1)
(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))
(make-variable-buffer-local 'erc-fill-column)
 (add-hook 'window-configuration-change-hook 
	   '(lambda ()
	      (save-excursion
	        (walk-windows
		 (lambda (w)
		   (let ((buffer (window-buffer w)))
		     (set-buffer buffer)
		     (when (eq major-mode 'erc-mode)
		       (setq erc-fill-column (- (window-width w) 2)))))))))


(when (assoc "en0" (network-interface-list))
  (erc :server "irc.freenode.net" :port 6667 :nick "lgmoneda"))


(progn
     (require 'erc)
     (require 'erc-track)
     (erc-track-mode +1)

     ;; keywords to track
     (setq erc-keywords '("bayes" "reinforcement" "unsupervised" "bayesian"))

     ;; Ignore Server Buffer
     (setq erc-track-exclude-server-buffer t)

     ;; show only when my nickname is mentioned in any channel
     (setq erc-current-nick-highlight-type 'nick)
     (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
     (setq erc-track-use-faces t)
     (setq erc-track-faces-priority-list
           '(erc-current-nick-face
             erc-keyword-face
             erc-direct-msg-face))
     (setq erc-track-priority-faces-only 'all))

(load "~/.ercfile")
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode (("lgmoneda" . ,lgmonedanick)))))

;; Prevents Erc buffers flashing at start
(erc-autojoin-mode t)
(setq erc-autojoin-timing :ident)
(setq erc-autojoin-delay 40)
(setq erc-join-buffer 'bury)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#sptk" "##machinelearning"
	 "#scikit-learn" "#tensorflow")))

(erc-autojoin-after-ident "irc.freenode.net" "lgmoneda")

(add-hook 'erc-nickserv-identified-hook 'erc-autojoin-after-ident)

;; Log in a buffer when people talk to me
(setq erc-log-matches-flag t)
(setq erc-log-matches-types-alist
          '((keyword . "### Keywords Log ###")
            (current-nick . "### Me Log ###")))

;; (setq erc-keywords '("keras" "bayes" "bayesian" "causality" "reinforcement"))

;; ;; Erc-tracking
;; (require 'erc-track)
;; (erc-track-mode t)

;; ;; Exclude not interesting messages
;; ;; Blue for keyword
;; (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"))
;; 				;; "324" "329" "332" "333" "353" "477"))

;; ;; Ignore Server Buffer
;; (setq erc-track-exclude-server-buffer t)

;; ;; That fixes the modeline notification for nick mention
;; (setq erc-current-nick-highlight-type 'nick)

;; (setq erc-track-priority-faces-only 'all)

;; ;; Track only mentions and keywords
;; (setq erc-track-use-faces t)
;; ;;(setq erc-format-query-as-channel-p t)

;; ;;Show pvt, current-nick and keyword
;; (setq erc-track-faces-priority-list
;;       '(erc-current-nick-face
;; 	erc-keyword-face
;; 	erc-direct-msg-face))

;;(setq erc-query-display 'bury)
;; Prevent the new created buffer from pvt to be brought visible
(setq erc-auto-query 'bury)

;; Both defadvices to track pvts using channels mode-line face
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p) 
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

(defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
  ad-do-it
  (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))

(require 'erc-nicklist)
;; bk's toggle nicklist!
(defun bk/nicklist-toggle ()
  "Function to toggle the nicklist in ERC mode."
  (interactive)
  (let ((nicklist-buffer-name (format " *%s-nicklist*" (buffer-name))))
    (if (get-buffer nicklist-buffer-name)
        (kill-buffer nicklist-buffer-name)
      (erc-nicklist))))

(define-key erc-mode-map (kbd "<f7>") 'bk/nicklist-toggle)

;; Smarter beep
;; Remember to apt-get install mplayer!
;; todo: no beep when buffer is visible
(add-hook 'erc-text-matched-hook 'erc-sound-if-not-server)
(defun erc-sound-if-not-server (match-type nickuserhost msg)
  (unless (or
	   (string-match "Serv" nickuserhost)
	   (string-match nickuserhost (erc-current-nick))
	   (string-match "Server" nickuserhost))
	(when (string= match-type "current-nick")
	(start-process-shell-command "lolsound" nil "mplayer ~/.emacs.d/sounds/icq-message.wav"))
	;;(setq mode-line-end-spaces
        ;; to-do use message-truncate-lines or messages-buffer-max-lines

       	(message
	      (format "[%s|<%s:%s> %s]"
	      	      (format-time-string "%Hh%M" (date-to-time (current-time-string)))
	      	      (subseq nickuserhost 0 (string-match "!" nickuserhost))
		      (or (erc-default-target) "")
		      (subseq msg 0 (- (length msg) 1))
		      ;; (if (eq (string-match (erc-current-nick) msg) 0)
		      ;; 	  (subseq msg (+ 1 (length (erc-current-nick))) 40)
		      ;; 	  msg
		      ;; 	  )
		      )
	      ;; Show msg for 20s
	       (run-with-timer 20 nil
                  (lambda ()
                    (message nil)))
	      )))

;; (defun notify-privmsg-mode-line (proc parsed)
;;   (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
;;         (target (car (erc-response.command-args parsed)))
;;         (msg (erc-response.contents parsed)))
;;     (when (and (erc-current-nick-p target)
;;                (not (erc-is-message-ctcp-and-not-action-p msg)))
;;       (setq mode-line-end-spaces (format "[pvt:%s]" nick)
;;                          msg
;;                          nil)
;;       ))
;;   nil)


;; ;; TODO
;; ;; 
;; (setq unread-pvt-msgs '())
;; (defun notify-privmsg-mode-line (proc parsed)
;;   (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
;;         (target (car (erc-response.command-args parsed)))
;;         (msg (erc-response.contents parsed)))
;;     (when (and (erc-current-nick-p target)
;;                (not (erc-is-message-ctcp-and-not-action-p msg)))
;;       ;; (setq mode-line-end-spaces (format "[last pvt:%s]" nick)
;;       ;;                    msg
;;       ;;                    nil)
;;       (if (eq (cdr (assoc nick unread-pvt-msgs)) nil)
;; 	  (add-to-list 'unread-pvt-msgs `(,nick . 1))
;; 	(progn
;; 	  (setq new-value  (+ (cdr (assoc nick unread-pvt-msgs)) 1)    )
;; 	  (setf (cdr (assoc nick unread-pvt-msgs)) new-value)
;; 	  )
;; 	)
;; 	(setq mode-line-end-spaces (format "[%s:%s (%d)]"
;; 					   (format-time-string "%Hh%M" (date-to-time (current-time-string)))
;; 					   nick
;; 					   (cdr (assoc nick unread-pvt-msgs)) )
;; 	      msg
;; 	      nil)
;; 	(display-unread-pvts)      
;;       ;;(print unread-pvt-msgs)
;;       ))
;;   nil)

;; (add-hook 'after-change-major-mode-hook 'read-erc-msgs)

;; (defun display-unread-pvts ()
;;   (interactive)
;;   (setq pvt-status-string "[unread pvt:")
;;   (mapcar (lambda (element)
;; 	    (print element)
;; 	   (setq pvt-status-string (concat pvt-status-string (format " %s(%d)," (car element) (cdr element))))	    
;; 	    )
;; 	  unread-pvt-msgs
;; 	  )
;;   (setq pvt-status-string (subseq pvt-status-string 0 (- (length pvt-status-string) 1)))

;;   (setq pvt-status-string (concat  pvt-status-string "]"))
;;   (message pvt-status-string)
;;   )

;; ;; Reference: https://www.emacswiki.org/emacs/SwitchToErc
;; ;; Use erc-query-buffer-p
;; (defun read-erc-msgs ())

;; (add-hook 'erc-server-PRIVMSG-functions 'notify-privmsg-mode-line t)

;; E-mail config
(setq user-mail-address "lg.moneda@gmail.com")
(setq user-full-name "Luis Moneda")
(setq
send-mail-function 'smtpmail-send-it
message-send-mail-function 'smtpmail-send-it
user-mail-address "lg.moneda@gmail.com"
smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
smtpmail-auth-credentials (expand-file-name "~/.authinfo")
smtpmail-default-smtp-server "smtp.gmail.com"
smtpmail-smtp-server "smtp.gmail.com"
smtpmail-smtp-service 587
smtpmail-debug-info t
starttls-extra-arguments nil
starttls-gnutls-program "/usr/bin/gnutls-cli"
starttls-extra-arguments nil
starttls-use-gnutls t
message-signature "Luis Moneda
http://lgmoneda.github.io/"
)

;; Mode line α
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . "")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (hs-minor-mode . "")
    (which-key-mode . "")
    (smartparens-mode . "")
    (abbrev-mode . "")
    (column-enforce-mode . "")
    (company-mode . "")
    (eldoc-mode . "")
    (Org-Indent . "")
    (visual-line-mode . "")
    (anzu-mode . "")
    (flyspell-mode . "")
    (color-identifiers-mode . "")
    (auto-revert-mode . "")
    ;; Major modes
    (fundamental-mode . "Fund")
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;Sunday, August 20, 2017
;============================
;==        Org-mode        ==
;============================
(require 'org)

;; Indent tasks
;; Old star color: 626483
(setq org-startup-indented t)

;; Init hiding everything
(setq org-startup-folded t)

;; Always hide stars
(setq org-hide-leading-stars t)

;; Fix helm-org-heading style
(setq helm-org-headings-fontify t)

;; No blank lines between headers
(setq org-cycle-separator-lines 0)

;; Show deadlines 30 days before
(setq org-deadline-warning-days 60)

;; Consider everything under the tree to todo statistics
(setq org-hierarchical-todo-statistics nil)

;; Add close time when changing to DONE
(setq org-log-done 'time)

;; Configs to use org mode to track time in tasks

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

(defun lgm/clock-in-when-started ()
"Automatically clock in a task when status is changed to STARTED"
    (when (string= org-state "STARTED")
      (org-clock-in)))

(add-hook 'org-after-todo-state-change-hook 'lgm/clock-in-when-started)

;; Easy jump, clock in and clock out
(global-set-key (kbd "<f12>") 'org-clock-goto)
(global-set-key (kbd "C-<f12>") 'org-clock-in)
(global-set-key (kbd "M-<f12>") 'org-clock-out)

;; Wrap clock tags in logbook
(setq org-clock-into-drawer t)
(setq org-log-into-drawer t)

;; Change viewer apps C-c C-o
;; When not in MAC
(unless (string-equal system-type "darwin")
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.x?html?\\'" . "xdg-open %s")
        ("\\.pdf\\'" . "evince \"%s\"")
        ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
        ("\\.pdf.xoj" . "xournal %s")))
      )

;;(add-hook 'org-trigger-hook 'lgm/clock-in-when-started)
;; From cashestocashes.com
;; Once you've included this, activate org-columns with C-c C-x C-c while on a top-level heading, which will allow you to view the time you've spent at the different levels (you can exit the view by pressing q)
;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16CLOSED")

;; New states to to-do
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)" "INACTIVE(i)" "FAIL(f)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("NEXT" . "pink")
	("STARTED" . "yellow")
	("WAIT" . "purple")
	("INACTIVE", "white")
        ("CANCELED" . (:foreground "blue" :weight bold))
        ("FAIL" . (:foreground "blue" :weight bold))))

;; No line number in org mode, please
(add-hook 'org-mode-hook (linum-mode 0))
;; Open org-agenda
;; (add-hook 'after-init-hook 'org-agenda-list)
(setq org-agenda-block-separator "-")

(org-defkey org-mode-map (kbd "C-S-s /") 'helm-org-agenda-files-headings)

(defun org-tell-me-first-header ()
  (interactive)
  (save-excursion
    (outline-up-heading 3)
    (print (substring-no-properties (org-get-heading t t)))
  )
 )

;; TODO entry automatically change to done when all children are done (from orgmode.org)
(defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
     
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (python . t)
   (sh . t)
   (dot . t)
   (sql . nil)
   (sqlite . t)))


(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; Agenda views / configs

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))
(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
	nil)))

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))


;; Org Journal
(use-package org-journal
  :ensure t
  :init (setq org-journal-dir "~/Dropbox/Agenda/Journal"))

;; Build progress bars
;; #+BEGIN: block-dashboard
;; #+END:
;; C-c C-c to build/update bars
(use-package org-dashboard
   :ensure t)

;; I want imenu, not new journal entry!
(global-set-key (kbd "C-c C-j") 'imenu-anywhere)
(define-key python-mode-map (kbd "C-c C-j") 'imenu-anywhere)

(setf org-journal-dir "~/Dropbox/Agenda/Journal")
(setf org-journal-file-format "my-journal.org")
(setf org-journal-date-prefix "* ")
(setf org-journal-time-prefix "** ")

;; Make Org Journal remember me about
;; writing my day thoughts like in Memento mode
(defcustom journal-file "~/Dropbox/Agenda/Journal/my-journal.org" 
  "Customizable variable to specify any file, which will be used for Memento." 
  :type 'string
  :group 'journal)

(defun lgm/org-journal-new-today-entry (prefix &optional event)
  "Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time heading. If a
prefix is given, don't add a new heading."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (current-time)))
    (lgm/org-journal-new-entry prefix time)))

(defun lgm/org-journal-new-entry (prefix &optional time)
  "Open today's journal file and start a new entry.
Giving the command a PREFIX arg will just open a today's file,
without adding an entry. If given a TIME, create an entry for the
time's day.

Whenever a journal entry is created the
`org-journal-after-entry-create-hook' hook is run"
  (interactive "P")
  (org-journal-dir-check-or-create)
  (let* ((entry-path (org-journal-get-entry-path time))
         (should-add-entry-p (not prefix)))

    ;; open journal file
    (unless (string= entry-path (buffer-file-name))
      (funcall org-journal-find-file entry-path))
    (org-journal-decrypt)
    (goto-char (point-max))
    (let ((unsaved (buffer-modified-p))
          (new-file-p (equal (point-max) 1)))

      ;; empty file? Add a date timestamp
      (insert "\n")
      (insert org-journal-date-prefix
              (format-time-string org-journal-date-format time))

      ;; add crypt tag if encryption is enabled and tag is not present
      (when org-journal-enable-encryption
        (goto-char (point-min))
        (unless (member org-crypt-tag-matcher (org-get-tags))
          (org-set-tags-to org-crypt-tag-matcher))
        (goto-char (point-max)))

      ;; move TODOs from previous day here
      (when (and new-file-p org-journal-carryover-items)
        (save-excursion (org-journal-carryover)))

      ;; insert the header of the entry
      (when should-add-entry-p
        (unless (eq (current-column) 0) (insert "\n"))
        (let ((timestamp (if (= (time-to-days (current-time)) (time-to-days time))
                             (format-time-string org-journal-time-format)
                           "")))
          (insert "\n" org-journal-time-prefix timestamp))
        (run-hooks 'org-journal-after-entry-create-hook))

      ;; switch to the outline, hide subtrees
      (org-journal-mode)
      (if (and org-journal-hide-entries-p (org-journal-time-entry-level))
          (hide-sublevels (org-journal-time-entry-level))
        (show-all))

      ;; open the recent entry when the prefix is given
      (when should-add-entry-p
        (show-entry))

      (set-buffer-modified-p unsaved))))

(defun journal-get-modification-date ()
  "Returns the last modified date of the current memento file."
  (format-time-string "%Y-%m-%d"
                      (nth 5 (file-attributes journal-file))))

(defun journal-check-when-quit ()
  (interactive)
  (if (file-exists-p journal-file)
      ;; Check if there was a log written today. If this is not the case, then check if it's already tonight except the night.
      (if (and (string< (journal-get-modification-date) (format-time-string "%Y-%m-%d")) (or (string< (format-time-string "%k") " 6") (string< "20" (format-time-string "%k"))))
          ;; Invoke Memento if the user wants to proceed. 
          (if (yes-or-no-p "Do you want to write your Journal?")
              (progn (call-interactively 'lgm/org-journal-new-today-entry))))
    ;; If the Memento file doesn't exist yet, create a file and proceed with creating a log.
    (write-region "" nil journal-file)
    (progn (call-interactively 'lgm/org-journal-new-today-entry))))


(add-hook 'kill-emacs-hook 'journal-check-when-quit)

;; Dict.cc wrap
(add-to-list 'load-path "~/.emacs.d/elisp/dict-cc" t)
(require 'dict-cc)
;; PATH append
(setenv "PATH" (concat "/home/lgmoneda/miniconda2/bin:" (getenv "PATH")))

;; Python Experiment!
;; (add-to-list 'load-path "~/.emacs.d/site-packages/python-experiment")
;; (require 'python-experiment)

;; (global-set-key (kbd "<f9>") 'python-experiment)
;; (global-set-key (kbd "<f10>") 'python-experiment-lived-too-long)
;; (global-set-key (kbd "<f11>") 'python-experiment-reload)
;; (global-set-key (kbd "<f12>") 'python-experiment-buffer-to-file)


;; Functions to copy words at point, from:
;; https://www.emacswiki.org/emacs/CopyWithoutSelection
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
	       (progn (comint-next-prompt 25535) (yank))
	     (progn (goto-char (mark)) (yank) )))))
    (if arg
	(if (= arg 1)
	    nil
	  (funcall pasteMe))
      (funcall pasteMe))
    ))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
	  (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )


(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (kill-new (thing-at-point 'word))
  ;;(copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w") (quote copy-word))

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c l")         (quote copy-line))

;; Trying to start using marker
;; C-<space> C-<space> to leave mark
;; C-u C-<space>
;; C-<space> again to previous marks
;; C-<space> C-s "char" to selec region between mark and I-search
;; ^ or use ace-jump!
;; C-x C-<space> global jump to last mark 
(setq set-mark-command-repeat-pop t)

;; Selection functions utilities
;; Stop using arrows for selection!
;; It requires thing-cmds 
(defun mark-a-word-or-thing (arg)
   "..."
  (interactive "P")
  (cond ((or arg mark-active)
         (when (consp arg) (setq current-prefix-arg nil))
         (call-interactively 'mark-thing))
        (t
         (skip-syntax-backward "w_")
         (mark-thing 'word))))

(global-set-key (kbd "C-ç") 'mark-a-word-or-thing)
(global-set-key (kbd "C-=") 'er/expand-region)


;; Fix it later
;; (setq package-enable-at-startup nil)
;; (package-initialize)

;; ;; Enable arrow keys in some modes
;; (defun arrow-keys-disable ()
;; (mapc 'global-unset-key '([left] [right] [up] [down]))

;; (let ((arrow-key-mode-maps '(help-mode-map org-agenda-mode-map Info-mode-map org-mode-map)))
;;   (mapc
;;    (lambda (map)
;;      (define-key (symbol-value map) [left] 'left-char)
;;      (define-key (symbol-value map) [right] 'right-char)
;;      (define-key (symbol-value map) [up] 'previous-line)
;;      (define-key (symbol-value map) [down] 'next-line))
;;    arrow-key-mode-maps)))

;; (add-hook 'after-init-hook 'arrow-keys-disable)

;; Creates a new line without breaking the current line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; Changes the search face


;; Schema 
(use-package quack
  :ensure t)

(use-package scheme-complete
  :ensure t)

;; If you use eldoc-mode (included in Emacs), you can also get live
;; scheme documentation with:
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
  (lambda ()
    (make-local-variable 'eldoc-documentation-function)
    (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
    (eldoc-mode)))

(eval-after-load 'scheme
  '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))

(setq lisp-indent-function 'scheme-smart-indent-function)

;; Some functions from EmacsWiki
(add-to-list 'load-path "~/.emacs.d/elisp/misc/" t)

;; Jump to last change 
(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)

;; LaTeX!
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
;(setq TeX-PDF-mode t)

;; automatic formatting of a section: C-c C-q C-s;
;; section preview: C-c C-p C-s; 

(require 'flymake)

(defun flymake-get-tex-args (file-name)
(list "pdflatex"
      (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))



(use-package latex-preview-pane
	     :ensure t)

(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;;To hide all the contents of your current section, use C-c C-o C-l. You can apply it to a chapter, subsection, etc. You can also move to a next unit of your document with C-c C-o C-n, or to a previous one with C-c C-o C-p. If you’re lost and want to see the whole document again, use C-c C-o C-a.
(defun turn-on-outline-minor-mode ()
(outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

; Annoying 25.2 message when running Python
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
(setq python-shell-completion-native-enable nil)

;; Slack
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  )

;; (load-file "~/.emacs.d/slack-credentials.el")

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;;Saturday, August 12, 2017
;;============================
;;==  Header func head :P   ==
;;============================
(defun lgm/insert-comment-header()
  (interactive)
  (setq header (read-string "Enter header: "))
  (setq header-length (length header))
  (back-to-indentation)
  (open-line 3)
  (insert comment-start)
  (insert (calendar-date-string (calendar-current-date)))
  (next-line)
  (insert (concat comment-start "============================"))
  (next-line)
  (insert (concat comment-start "=="))
  (setq n-white-spaces (- 24 header-length))
  (setq count 0)
  (while (< count n-white-spaces)
    (insert " ")
    (setq count (1+ count))
    (if (= count (/ n-white-spaces 2))
	(insert header)
	)
    )
  (insert "==")
  (next-line)
  (insert (concat comment-start "============================"))
  )

(define-key emacs-lisp-mode-map (kbd "<f5>") 'lgm/insert-lisp-comment-header)


;;Thursday, August 10, 2017
;;============================
;;==           R            ==
;;============================

(setq inferior-R-program-name "/Library/Frameworks/R.framework/Resources/R")

(use-package ess
	     :ensure t
	     :init
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  (require 'ess-eldoc))

;;Tuesday, August 15, 2017
;;============================
;;==       G Calendar       ==
;;============================

(use-package org-gcal 
  :init
  :ensure t
  )
 
;; Load Calendar
(load "~/Dropbox/Projetos/Emacs/.gcalsync.el")

;; Start with my to-do
;; The org mode file is opened with
(find-file "~/Dropbox/Agenda/todo.org")
(switch-to-buffer "todo.org")
(setq org-agenda-window-setup 'other-window) 
(org-agenda-list)


;;Tuesday, August 15, 2017
;;============================
;;==       OS Configs       ==
;;============================
(cond
    ((string-equal system-type "gnu/linux")
     (progn
      (+ 1 1)
    ))
    ((string-equal system-type "darwin")
        (progn
	 (add-to-list 'exec-path "/usr/local/bin" "/Library/TeX/texbin/pdflatex")
	 ;;add hookup shell
	 (add-hook 'shell-mode-hook (lambda ()
				      (setenv "PATH" (shell-command-to-string "$SHELL -cl \"printf %s \\\"\\\$PATH\\\"\""))
				      (setq exec-path (append (parse-colon-path (getenv "PATH")) (list exec-directory)))

				      ))
	 (setq ispell-program-name "/usr/local/bin/ispell/")
	 (setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
	 ;; Shell
	 ;; There's a .emacs_bash with .~/bash_profile
	 ;; Maybe i should use this:
	 ;;https://github.com/purcell/exec-path-from-shell
        )
    )
    )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package default-text-scale
	     :ensure t)

(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

