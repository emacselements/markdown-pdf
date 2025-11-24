;;; org-markdown.el --- Org-mode to Markdown export for Emacs -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: org, markdown, export

;;; Commentary:
;; This package provides Org-mode to Markdown export functionality
;; using pandoc.

;;; Code:

(require 'org nil t)

(defgroup org-markdown nil
  "Org to Markdown export settings."
  :group 'org)

(defcustom org-markdown-pandoc-command "pandoc"
  "Command to run pandoc."
  :type 'string
  :group 'org-markdown)

(defcustom org-markdown-format "gfm"
  "Pandoc markdown format to use for output.
Common options:
- gfm (GitHub Flavored Markdown, default)
- markdown (Pandoc's markdown)
- markdown_strict (strict markdown)
- markdown_phpextra (PHP Markdown Extra)
- markdown_mmd (MultiMarkdown)"
  :type 'string
  :group 'org-markdown)

(defcustom org-markdown-open-after-export t
  "Whether to open the Markdown file after export."
  :type 'boolean
  :group 'org-markdown)

(defcustom org-markdown-output-directory nil
  "Directory to save Markdown files."
  :type '(choice (const nil) directory)
  :group 'org-markdown)

(defun org-markdown--get-output-path (input-file)
  "Get the output path for the Markdown file based on INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-dir (or org-markdown-output-directory (file-name-directory input-file)))
         (output-file (concat base-name ".md")))
    (expand-file-name output-file output-dir)))

(defun org-markdown--pandoc-available-p ()
  "Check if pandoc is available."
  (executable-find org-markdown-pandoc-command))

(defun org-markdown--open-markdown (md-file)
  "Open MD-FILE in Emacs."
  (find-file md-file))

;;;###autoload
(defun org-markdown-export ()
  "Export the current org buffer to Markdown format."
  (interactive)
  (unless (org-markdown--pandoc-available-p)
    (error "Pandoc is not available"))
  (let ((input-file (buffer-file-name))
        output-file result)
    (unless input-file
      (error "Buffer is not visiting a file"))
    (setq output-file (org-markdown--get-output-path input-file))
    (when (buffer-modified-p)
      (when (y-or-n-p "Save buffer? ")
        (save-buffer)))
    (message "Exporting Org to Markdown...")
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    (let* ((pandoc-args (list "-f" "org" input-file "-o" output-file "-t" org-markdown-format)))
      (setq result (apply 'call-process org-markdown-pandoc-command nil "*pandoc-output*" t pandoc-args)))
    (if (= result 0)
        (progn
          (message "Markdown exported successfully: %s" output-file)
          (when org-markdown-open-after-export
            (org-markdown--open-markdown output-file)))
      (let ((error-output (when (get-buffer "*pandoc-output*")
                            (with-current-buffer "*pandoc-output*"
                              (buffer-string)))))
        (error "Pandoc export failed with exit code %d%s"
               result
               (if error-output (format "\nOutput: %s" error-output) ""))))))

;;;###autoload
(defun org-markdown-export-and-open ()
  "Export the current org buffer to Markdown and open it."
  (interactive)
  (let ((org-markdown-open-after-export t))
    (org-markdown-export)))

;; Keybinding is now handled by markdown-export.el for unified interface
;; Users can still call org-markdown-export or org-markdown-export-and-open directly

(provide 'org-markdown)

;;; org-markdown.el ends here
