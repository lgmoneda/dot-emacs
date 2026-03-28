;;; manager-ai-agent-settings.el --- Emacs integration for manager-ai-agent
;;
;; Replaces assistant-settings.el (catalyst).
;; No server to start — routines run via launchd + claude CLI.
;; Data is read live at skill-run time through MCP tools.
;;
;; Loads:
;;   manager-agent.el    — modeline notifications, file-notify watcher,
;;                         agent-shell launcher, C-c y command menu
;;   manager-agent-ui.el — Vertico/Consult/Orderless browser for
;;                         sessions, insights, reports
;;   nu-org-diff.el      — tracks additions/deletions in nu.org (work agenda)
;;                         and appends to context/nu_changelog.txt
;;   kb-org-diff.el      — tracks changes to org-roam KB files (skips :personal:)
;;                         and appends to context/kb_changelog.txt

(add-to-list 'load-path "~/repos/manager-ai-agent/elisp/")

(require 'manager-agent)
(require 'manager-agent-ui)
(require 'nu-org-diff)
(ma/nu-org-diff-install-hook)
(require 'kb-org-diff)
(ma/kb-org-diff-install-hook)

(provide 'manager-ai-agent-settings)
;;; manager-ai-agent-settings.el ends here
