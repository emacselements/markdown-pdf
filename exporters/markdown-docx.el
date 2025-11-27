;;; markdown-docx.el --- Pretty markdown to DOCX export for Emacs -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, docx, export

;;; Commentary:
;; This package provides markdown to DOCX export functionality
;; using pandoc.

;;; Code:

(require 'markdown-mode nil t)

(defgroup markdown-docx nil
  "Markdown to DOCX export settings."
  :group 'markdown)

(defcustom markdown-docx-pandoc-command "pandoc"
  "Command to run pandoc."
  :type 'string
  :group 'markdown-docx)

(defcustom markdown-docx-reference-doc nil
  "Path to reference DOCX document for styling."
  :type '(choice (const nil) file)
  :group 'markdown-docx)

(defcustom markdown-docx-markdown-format "markdown+smart"
  "Pandoc markdown format to use for parsing.
Common options:
- markdown+smart (default, smart quotes and typography)
- gfm (GitHub Flavored Markdown)
- markdown_strict (strict markdown)
- markdown+all_symbols_escapable (allows escaping any symbol)"
  :type 'string
  :group 'markdown-docx)

(defcustom markdown-docx-open-after-export t
  "Whether to open the DOCX file after export."
  :type 'boolean
  :group 'markdown-docx)

(defcustom markdown-docx-output-directory nil
  "Directory to save DOCX files."
  :type '(choice (const nil) directory)
  :group 'markdown-docx)

(defcustom markdown-docx-include-footer t
  "Whether to include filename and date footer in the DOCX."
  :type 'boolean
  :group 'markdown-docx)

(defun markdown-docx--get-output-path (input-file)
  "Get the output path for the DOCX file based on INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-dir (or markdown-docx-output-directory (file-name-directory input-file)))
         (output-file (concat base-name ".docx")))
    (expand-file-name output-file output-dir)))

(defun markdown-docx--pandoc-available-p ()
  "Check if pandoc is available."
  (executable-find markdown-docx-pandoc-command))

(defun markdown-docx--open-docx (docx-file)
  "Open DOCX-FILE with the default system application."
  (cond
   ((eq system-type 'darwin) (call-process "open" nil nil nil docx-file))
   ((eq system-type 'gnu/linux) (call-process "xdg-open" nil nil nil docx-file))
   ((eq system-type 'windows-nt) (w32-shell-execute "open" docx-file))
   (t (message "Cannot open DOCX automatically"))))

(defun markdown-docx--preprocess-for-export (input-file)
  "Preprocess INPUT-FILE for export.
Returns path to preprocessed temporary file."
  (let ((temp-file (make-temp-file "markdown-docx-prep" nil ".md"))
        (content (with-temp-buffer
                   (insert-file-contents input-file)
                   (buffer-string))))
    (with-temp-file temp-file
      (insert content)
      (when markdown-docx-include-footer
        (goto-char (point-max))
        (insert "\n\n---\n\n")
        (insert (format "**%s** | %s"
                        (file-name-nondirectory input-file)
                        (format-time-string "%Y-%m-%d")))))
    temp-file))

;;;###autoload
(defun markdown-docx-export ()
  "Export the current markdown buffer to DOCX."
  (interactive)
  (unless (markdown-docx--pandoc-available-p)
    (error "Pandoc is not available"))
  (let ((input-file (buffer-file-name))
        output-file temp-md result)
    (unless input-file
      (error "Buffer is not visiting a file"))
    (setq output-file (markdown-docx--get-output-path input-file))
    (when (buffer-modified-p)
      (when (y-or-n-p "Save buffer? ")
        (save-buffer)))
    ;; Preprocess markdown
    (setq temp-md (markdown-docx--preprocess-for-export input-file))
    (message "Exporting markdown to DOCX...")
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    (let* ((pandoc-args (append
                        (list "-f" markdown-docx-markdown-format temp-md "-o" output-file "--standalone")
                        (when markdown-docx-reference-doc
                          (list "--reference-doc" markdown-docx-reference-doc)))))
      (setq result (apply 'call-process markdown-docx-pandoc-command nil "*pandoc-output*" t pandoc-args)))
    (when temp-md (delete-file temp-md))
    (if (= result 0)
        (progn
          (message "DOCX exported successfully: %s" output-file)
          (when markdown-docx-open-after-export
            (markdown-docx--open-docx output-file)))
      (let ((error-output (when (get-buffer "*pandoc-output*")
                            (with-current-buffer "*pandoc-output*"
                              (buffer-string)))))
        (error "Pandoc export failed with exit code %d%s"
               result
               (if error-output (format "\nOutput: %s" error-output) ""))))))

;;;###autoload
(defun markdown-docx-export-and-open ()
  "Export the current markdown buffer to DOCX and open it."
  (interactive)
  (let ((markdown-docx-open-after-export t))
    (markdown-docx-export)))

;; Keybinding is now handled by markdown-export.el for unified interface
;; Users can still call markdown-docx-export or markdown-docx-export-and-open directly

(provide 'markdown-docx)

;;; markdown-docx.el ends here
