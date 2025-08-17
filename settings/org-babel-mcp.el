;;; org-babel-mcp.el --- MCP server for Org Babel execution -*- lexical-binding: t -*-
;; Copyright (C) 2025
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (mcp-server-lib "0.1.0"))
;; Keywords: tools, org-mode, babel
;; URL: https://github.com/your-username/org-babel-mcp
;;; Commentary:
;; This package provides an MCP server for executing Org Babel blocks.
;; It allows LLMs to execute individual source blocks or entire buffers
;; containing Org Babel code.
;;; Code:

(require 'mcp-server-lib)
(require 'org)
(require 'ob)
(require 'json)

;;; Utility Functions

(defun org-babel-mcp--format-success (message &optional details)
  "Format a success response as a string.
MESSAGE is the success message.
DETAILS is an optional alist of additional details."
  (if details
      (concat message "\n\nDetails:\n"
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    message))

(defun org-babel-mcp--format-error (message &optional details)
  "Format an error response as a string.
MESSAGE is the error message.
DETAILS is an optional alist of additional details."
  (let ((formatted-message (concat "Error: " message)))
    (if details
        (concat formatted-message "\n\nDetails:\n"
                (mapconcat (lambda (pair)
                            (format "- %s: %s" (car pair) (cdr pair)))
                          details "\n"))
      formatted-message)))

(defun org-babel-mcp--validate-file-path (file-path)
  "Validate that FILE-PATH is a non-empty string pointing to an existing org file.
Throws an error if validation fails."
  (unless (stringp file-path)
    (mcp-server-lib-tool-throw "Invalid file path: must be a string"))
  (when (string-empty-p file-path)
    (mcp-server-lib-tool-throw "Empty file path"))
  (unless (file-exists-p file-path)
    (mcp-server-lib-tool-throw (format "File does not exist: %s" file-path)))
  (unless (string-match-p "\\.org$" file-path)
    (mcp-server-lib-tool-throw "File must have .org extension")))

(defun org-babel-mcp--get-org-buffer (file-path)
  "Get or create buffer for FILE-PATH.
Returns the buffer object."
  (let ((buffer (find-file-noselect file-path)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buffer))

(defun org-babel-mcp--format-success (message &optional details)
  "Format a success response as string.
MESSAGE is the success message.
DETAILS is an optional alist of additional details."
  (if details
      (format "%s\n\nDetails:\n%s" message
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    message))

(defun org-babel-mcp--format-error (message &optional details)
  "Format an error response as string.
MESSAGE is the error message.
DETAILS is an optional alist of additional details."
  (if details
      (format "Error: %s\n\nDetails:\n%s" message
              (mapconcat (lambda (pair)
                          (format "- %s: %s" (car pair) (cdr pair)))
                        details "\n"))
    (format "Error: %s" message)))

(defun org-babel-mcp--capture-output (func)
  "Capture output from FUNC execution.
Returns a list (SUCCESS-P OUTPUT ERROR-OUTPUT RESULT)."
  (let ((success t)
        (result nil)
        (error-output ""))
    (condition-case err
        (progn
          (setq result (funcall func))
          (setq success t))
      (error
       (setq success nil)
       (setq error-output (error-message-string err))))
    (list success "" error-output result)))

;;; MCP Tool Functions

(defun org-babel-mcp-execute-src-block (file-path &optional block-name)
  "Execute a specific source block in an org file.
FILE-PATH is the path to the org file.
BLOCK-NAME is the optional name of the block to execute. If not provided,
executes the block at point or the first block found."
  (org-babel-mcp--validate-file-path file-path)

  (let ((buffer (org-babel-mcp--get-org-buffer file-path))
        (executed nil)
        (block-info nil))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (if block-name
            ;; Find specific named block
            (progn
              (unless (re-search-forward
                       (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$"
                               (regexp-quote block-name)) nil t)
                (mcp-server-lib-tool-throw
                 (format "Block with name '%s' not found" block-name)))
              ;; Move to the actual source block
              (unless (re-search-forward "^[ \t]*#\\+begin_src" nil t)
                (mcp-server-lib-tool-throw
                 "No source block found after name declaration"))
              (beginning-of-line))
          ;; Find first source block
          (unless (re-search-forward "^[ \t]*#\\+begin_src" nil t)
            (mcp-server-lib-tool-throw "No source blocks found in file"))
          (beginning-of-line))

        ;; Get block info
        (setq block-info (org-babel-get-src-block-info))
        (unless block-info
          (mcp-server-lib-tool-throw "Could not get source block information"))

        ;; Execute the block
        (let* ((execution-result (org-babel-mcp--capture-output
                                  (lambda () (org-babel-execute-src-block))))
               (success (nth 0 execution-result))
               (output (nth 1 execution-result))
               (error-output (nth 2 execution-result))
               (result (nth 3 execution-result)))

          (setq executed t)

          (if success
              (progn
                ;; Save the buffer to persist execution results
                (save-buffer)
                (concat
                 (org-babel-mcp--format-success
                  (format "Successfully executed source block%s"
                          (if block-name (format " '%s'" block-name) ""))
                  `(("File Path" . ,file-path)
                    ("Block Name" . ,(or block-name "(unnamed)"))
                    ("Language" . ,(nth 0 block-info))
                    ("Output" . ,(or output "(no output)"))
                    ("Result" . ,(if result (format "%s" result) "(no result)"))
                    ("File Saved" . "true")))
                 "\n\nREMINDER: File has been modified. Use Read tool to see changes."))
            (mcp-server-lib-tool-throw
             (org-babel-mcp--format-error
              (format "Failed to execute source block%s"
                      (if block-name (format " '%s'" block-name) ""))
              `(("File Path" . ,file-path)
                ("Block Name" . ,(or block-name "(unnamed)"))
                ("Language" . ,(nth 0 block-info))
                ("Error Output" . ,error-output))))))))))

(defun org-babel-mcp-execute-buffer (file-path)
  "Execute all source blocks in an org file buffer.
FILE-PATH is the path to the org file."
  (org-babel-mcp--validate-file-path file-path)

  (let ((buffer (org-babel-mcp--get-org-buffer file-path))
        (executed-count 0)
        (failed-count 0)
        (execution-details '()))

    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))

        ;; Count total blocks first
        (let ((total-blocks 0))
          (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
            (setq total-blocks (1+ total-blocks)))

          (when (= total-blocks 0)
            (mcp-server-lib-tool-throw "No source blocks found in file"))

          ;; Execute all blocks
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
            (beginning-of-line)
            (let* ((block-info (org-babel-get-src-block-info))
                   (language (when block-info (nth 0 block-info)))
                   (execution-result (org-babel-mcp--capture-output
                                      (lambda () (org-babel-execute-src-block))))
                   (success (nth 0 execution-result))
                   (output (nth 1 execution-result))
                   (error-output (nth 2 execution-result))
                   (result (nth 3 execution-result)))

              (if success
                  (setq executed-count (1+ executed-count))
                (setq failed-count (1+ failed-count)))

              (push `((line . ,(line-number-at-pos))
                      (language . ,(or language "unknown"))
                      (success . ,(if success "true" "false"))
                      (output . ,(or output ""))
                      (error . ,(or error-output ""))
                      (result . ,(if result (format "%s" result) "")))
                    execution-details))

            ;; Move past the current block
            (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
              (forward-line 1))))

        ;; Return results
        (let ((details-str (mapconcat
                           (lambda (detail)
                             (format "Line %d (%s): %s"
                                     (cdr (assoc 'line detail))
                                     (cdr (assoc 'language detail))
                                     (if (string= (cdr (assoc 'success detail)) "true")
                                         "Success"
                                       (format "Failed - %s" (cdr (assoc 'error detail))))))
                           (reverse execution-details) "\n")))
          ;; Save the buffer to persist execution results
          (when (> executed-count 0)
            (save-buffer))
          (if (> failed-count 0)
              (mcp-server-lib-tool-throw
               (org-babel-mcp--format-error
                (format "Executed buffer with %d successes and %d failures"
                        executed-count failed-count)
                `(("File Path" . ,file-path)
                  ("Executed Blocks" . ,(number-to-string executed-count))
                  ("Failed Blocks" . ,(number-to-string failed-count))
                  ("Total Blocks" . ,(number-to-string (+ executed-count failed-count)))
                  ("Execution Details" . ,details-str)
                  ("File Saved" . ,(if (> executed-count 0) "true" "false")))))
            (concat
             (org-babel-mcp--format-success
              (format "Successfully executed all %d source blocks" executed-count)
              `(("File Path" . ,file-path)
                ("Executed Blocks" . ,(number-to-string executed-count))
                ("Total Blocks" . ,(number-to-string executed-count))
                ("Execution Details" . ,details-str)
                ("File Saved" . "true")))
             "\n\nREMINDER: File has been modified. Use Read tool to see changes."))))))))

;;; MCP Tool Handler Functions

(defun org-babel-mcp--execute-src-block-handler (params)
  "Handler for execute_src_block tool.

MCP Parameters:
  params - String containing tool parameters as association list"
  (mcp-server-lib-with-error-handling
    (let* ((params-alist (if (stringp params)
                             (car (read-from-string params))
                           params))
           (file-path (cdr (assoc 'file_path params-alist)))
           (block-name (cdr (assoc 'block_name params-alist))))
      (org-babel-mcp-execute-src-block file-path block-name))))

(defun org-babel-mcp--execute-buffer-handler (params)
  "Handler for execute_buffer tool.

MCP Parameters:
  params - String containing tool parameters as association list"
  (mcp-server-lib-with-error-handling
    (let* ((params-alist (if (stringp params)
                             (car (read-from-string params))
                           params))
           (file-path (cdr (assoc 'file_path params-alist))))
      (org-babel-mcp-execute-buffer file-path))))

;;; MCP Server Registration

(mcp-server-lib-register-tool
 #'org-babel-mcp--execute-src-block-handler
 :id "execute_src_block"
 :title "Execute Org Babel Source Block"
 :description "Execute a specific Org Babel source block in an org file.

Parameters:
  file_path - Path to the .org file (required)
  block_name - Optional name of the block to execute. If not provided,
               executes the first source block found.

Returns execution results including:
- success status
- output from the block execution
- error messages if any
- block metadata (language, line number, etc.)

The block name should match a #+NAME: declaration in the org file.
If no block name is provided, the first source block in the file will be executed.")

(mcp-server-lib-register-tool
 #'org-babel-mcp--execute-buffer-handler
 :id "execute_buffer"
 :title "Execute All Org Babel Blocks"
 :description "Execute all Org Babel source blocks in an org file buffer.

Parameters:
  file_path - Path to the .org file (required)

Returns execution results including:
- total number of blocks executed
- success and failure counts
- detailed results for each block
- execution output and errors

All source blocks in the file will be executed sequentially in the order
they appear in the file. The results include information about each
individual block execution.")

;;; Server Startup and Shutdown

(defun org-babel-mcp-start-server ()
  "Start the Org Babel MCP server."
  (interactive)
  (unless mcp-server-lib--running
    (mcp-server-lib-start)))

(defun org-babel-mcp-stop-server ()
  "Stop the Org Babel MCP server."
  (interactive)
  (mcp-server-lib-stop))

;; Auto-start server when loaded as a script
(when (and (bound-and-true-p command-line-args)
           (member "--start-server" command-line-args))
  (org-babel-mcp-start-server))

(provide 'org-babel-mcp)
;;; org-babel-mcp.el ends here
