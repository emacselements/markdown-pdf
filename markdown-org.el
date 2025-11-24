;;; markdown-org.el --- Markdown to Org-mode export for Emacs -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, org, export

;;; Commentary:
;; This package provides markdown to Org-mode export functionality
;; using pandoc.

;;; Code:

(require 'markdown-mode nil t)

(defgroup markdown-org nil
  "Markdown to Org export settings."
  :group 'markdown)

(defcustom markdown-org-pandoc-command "pandoc"
  "Command to run pandoc."
  :type 'string
  :group 'markdown-org)

(defcustom markdown-org-markdown-format "markdown+smart"
  "Pandoc markdown format to use for parsing.
Common options:
- markdown+smart (default, smart quotes and typography)
- gfm (GitHub Flavored Markdown)
- markdown_strict (strict markdown)
- markdown+all_symbols_escapable (allows escaping any symbol)"
  :type 'string
  :group 'markdown-org)

(defcustom markdown-org-open-after-export t
  "Whether to open the Org file after export."
  :type 'boolean
  :group 'markdown-org)

(defcustom markdown-org-output-directory nil
  "Directory to save Org files."
  :type '(choice (const nil) directory)
  :group 'markdown-org)

(defun markdown-org--get-output-path (input-file)
  "Get the output path for the Org file based on INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-dir (or markdown-org-output-directory (file-name-directory input-file)))
         (output-file (concat base-name ".org")))
    (expand-file-name output-file output-dir)))

(defun markdown-org--pandoc-available-p ()
  "Check if pandoc is available."
  (executable-find markdown-org-pandoc-command))

(defun markdown-org--open-org (org-file)
  "Open ORG-FILE in Emacs."
  (find-file org-file))

;;;###autoload
(defun markdown-org-export ()
  "Export the current markdown buffer to Org format."
  (interactive)
  (unless (markdown-org--pandoc-available-p)
    (error "Pandoc is not available"))
  (let ((input-file (buffer-file-name))
        output-file result)
    (unless input-file
      (error "Buffer is not visiting a file"))
    (setq output-file (markdown-org--get-output-path input-file))
    (when (buffer-modified-p)
      (when (y-or-n-p "Save buffer? ")
        (save-buffer)))
    (message "Exporting markdown to Org...")
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    (let* ((pandoc-args (list "-f" markdown-org-markdown-format input-file "-o" output-file "-t" "org")))
      (setq result (apply 'call-process markdown-org-pandoc-command nil "*pandoc-output*" t pandoc-args)))
    (if (= result 0)
        (progn
          (message "Org exported successfully: %s" output-file)
          (when markdown-org-open-after-export
            (markdown-org--open-org output-file)))
      (let ((error-output (when (get-buffer "*pandoc-output*")
                            (with-current-buffer "*pandoc-output*"
                              (buffer-string)))))
        (error "Pandoc export failed with exit code %d%s"
               result
               (if error-output (format "\nOutput: %s" error-output) ""))))))

;;;###autoload
(defun markdown-org-export-and-open ()
  "Export the current markdown buffer to Org and open it."
  (interactive)
  (let ((markdown-org-open-after-export t))
    (markdown-org-export)))

;; Keybinding is now handled by markdown-export.el for unified interface
;; Users can still call markdown-org-export or markdown-org-export-and-open directly

(provide 'markdown-org)

;;; markdown-org.el ends here
