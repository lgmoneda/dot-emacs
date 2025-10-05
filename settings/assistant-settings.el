;;; assistant-settings.el --- Functions for my AI assistant

(add-to-list 'load-path "~/repos/catalyst-assistant/context/")
(require 'org-todo-diff)
(require 'org-agenda-export-file)

;; Org todo diff
(add-hook 'after-save-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string= (expand-file-name (buffer-file-name))
                                (expand-file-name my/todo-file)))
              (run-with-idle-timer 1 nil #'save-todo-diff-postsave))))

;; Org Agenda
;; --------------------------------------------------------------------
;;  1️⃣  Run once when starting Emacs (after 2s idle)
;; --------------------------------------------------------------------
(run-with-idle-timer
 2 nil
 (lambda ()
   (export-org-agenda-batch-async my/org-agenda-export-file)))

;; --------------------------------------------------------------------
;;  2️⃣  Run after saving todo.org (debounced)
;; --------------------------------------------------------------------
(add-hook 'after-save-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string= (expand-file-name (buffer-file-name))
                                (expand-file-name my/org-agenda-todo-file)))
              (my/export-org-agenda-debounced))))

;; Knowledge info module
;; Tracks changes in my knowledge base files, which is the Org roam folder
(defun start-kb-tracking ()
  (interactive)
  (pyvenv-activate "/Users/luis.moneda/miniconda3/envs/ml3")
  (async-shell-command "fswatch --exclude '.git/' --exclude '.*\.#' --one-per-batch --latency 30 /Users/luis.moneda/Dropbox/Agenda/roam | xargs -n1 -I{} /Users/luis.moneda/repos/org-roam-ai/catalyst/dump_kb_changes.sh")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<4>"))))

(start-kb-tracking)

;; The catalyst server
(defun start-catalyst-server ()
  (interactive)
  (async-shell-command "source ~/.zshrc && conda activate edge && python /Users/luis.moneda/repos/catalyst-assistant/src/main.py")
  (delete-window (get-buffer-window (get-buffer "*Async Shell Command*<5>"))))

(start-catalyst-server)

(provide 'assistant-settings)
;;; assistant-settings.el ends here
