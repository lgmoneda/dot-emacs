;;; python-settings.el --- Settings for the programming language Python

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq-default indent-tabs-mode t)
	    (setq-default tab-width 4)
	    (setq-default py-indent-tabs-mode t)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Column enforce
(use-package column-enforce-mode
	     :ensure t
	     :config
	     (add-hook 'python-mode-hook 'column-enforce-mode)
	     )

;; Company-anaconda
(use-package company-anaconda
  :ensure t
  :diminish
  :config
  (eval-after-load "company"
     '(add-to-list 'company-backends '(company-anaconda company-capf))))

(add-hook 'python-mode-hook 'company-mode)

;; To activate nupy environment
(defun anupy ()
  (interactive)
  (pythonic-activate "~/miniconda2/envs/nupy")
  )

;; Check if i'm at work and activate
;; the right environment
(defun activate-work-env ()
  (if (string= (system-name) "rc530")
      (pythonic-activate "~/miniconda2/envs/ml")
      )
  (if (string-match "lgmoneda-MacBook.local" (system-name))
      (pythonic-activate "~/miniconda2/envs/ml3")
      )
  )

(activate-work-env)

;; I don't want to see the error buffer
(remove-hook 'anaconda-mode-response-read-fail-hook
             'anaconda-mode-show-unreadable-response)

;; ?
(--each '(python-mode python)
	(eval-after-load it '(require 'smartparens-python)))

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
   (insert "print(\"Execution time: {0}m{1}s\".format(int((time()-start)/60), int((time()-start)%60 )))"))
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

(define-key python-mode-map (kbd "C-c C-j") 'imenu-anywhere)

;; Put white spaces between operators in Python
(use-package electric-operator
  :ensure t
  :config (add-hook 'python-mode-hook 'electric-operator-mode))

;; Warning about imports in python
;; Requires pyflakes
;; M-x pyimport-remove-unused
;; M-x pyimport-insert-missing
;; https://github.com/Wilfred/pyimport
(use-package pyimport
  :ensure t)

(define-key python-mode-map (kbd "C-c C-i") 'pyimport-insert-missing)

(use-package py-autopep8
  :ensure t
  :init (progn
           (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
           ))

(setq py-autopep8-options '("--max-line-length=120"))

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

(provide 'python-settings)
;;; python-settings.el ends here
