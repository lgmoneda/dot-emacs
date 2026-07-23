;;; conda-settings.el --- Shared Conda path helpers -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'seq)

(defconst lgm/conda-base-candidates
  '("/opt/homebrew/Caskroom/miniforge/base"
    "/opt/homebrew/opt/miniforge/base"
    "/opt/homebrew/Caskroom/miniconda/base"
    "/opt/homebrew/opt/miniconda/base"
    "~/miniforge3"
    "~/mambaforge"
    "~/miniconda3"
    "~/anaconda3")
  "Fallback Conda/Mamba base directories to try when shell discovery fails.")

(defun lgm/conda--shell-base ()
  "Return Conda's base directory as reported by the user's login shell."
  (let ((base (string-trim
               (shell-command-to-string
                "zsh -lc 'conda info --base 2>/dev/null || mamba info --base 2>/dev/null'"))))
    (unless (string-empty-p base)
      base)))

(defun lgm/conda--base-from-executable ()
  "Return Conda base directory derived from `conda' or `mamba' in `exec-path'."
  (when-let* ((exe (or (executable-find "conda")
                       (executable-find "mamba")))
              (bin-dir (file-name-directory (file-truename exe)))
              (base-dir (file-name-directory (directory-file-name bin-dir))))
    (directory-file-name base-dir)))

(defun lgm/conda--base-from-candidates ()
  "Return the first existing fallback Conda base directory."
  (seq-find #'file-directory-p
            (mapcar #'expand-file-name lgm/conda-base-candidates)))

(defconst lgm/conda-base-dir
  (or (getenv "CONDA_ROOT")
      (getenv "MAMBA_ROOT_PREFIX")
      (lgm/conda--shell-base)
      (lgm/conda--base-from-executable)
      (lgm/conda--base-from-candidates))
  "Detected Conda/Mamba base directory.")

(defconst lgm/conda-envs-dir
  (when lgm/conda-base-dir
    (expand-file-name "envs" lgm/conda-base-dir))
  "Directory containing Conda/Mamba environments.")

(defun lgm/conda-env-dir (env-name)
  "Return the directory for Conda environment ENV-NAME."
  (when lgm/conda-envs-dir
    (expand-file-name env-name lgm/conda-envs-dir)))

(defun lgm/conda-env-executable (env-name executable)
  "Return EXECUTABLE inside Conda environment ENV-NAME."
  (when-let* ((env-dir (lgm/conda-env-dir env-name)))
    (expand-file-name executable (expand-file-name "bin" env-dir))))

(defun lgm/conda-prepend-base-bin-to-path ()
  "Add Conda's base bin directory to `exec-path' and PATH when available."
  (when-let* ((base-dir lgm/conda-base-dir))
    (let ((bin-dir (expand-file-name "bin" base-dir)))
      (when (file-directory-p bin-dir)
        (add-to-list 'exec-path bin-dir)
        (setenv "PATH" (concat bin-dir path-separator (or (getenv "PATH") "")))))))

(provide 'conda-settings)
;;; conda-settings.el ends here
