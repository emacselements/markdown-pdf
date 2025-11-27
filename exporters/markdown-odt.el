;;; markdown-odt.el --- Pretty markdown to ODT export for Emacs -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, odt, export

;;; Commentary:
;; This package provides markdown to ODT export functionality
;; using pandoc.

;;; Code:

(require 'markdown-mode nil t)

(defgroup markdown-odt nil
  "Markdown to ODT export settings."
  :group 'markdown)

(defcustom markdown-odt-pandoc-command "pandoc"
  "Command to run pandoc."
  :type 'string
  :group 'markdown-odt)

(defcustom markdown-odt-reference-doc nil
  "Path to reference ODT document for styling."
  :type '(choice (const nil) file)
  :group 'markdown-odt)

(defcustom markdown-odt-markdown-format "markdown+smart"
  "Pandoc markdown format to use for parsing.
Common options:
- markdown+smart (default, smart quotes and typography)
- gfm (GitHub Flavored Markdown)
- markdown_strict (strict markdown)
- markdown+all_symbols_escapable (allows escaping any symbol)"
  :type 'string
  :group 'markdown-odt)

(defcustom markdown-odt-open-after-export t
  "Whether to open the ODT file after export."
  :type 'boolean
  :group 'markdown-odt)

(defcustom markdown-odt-output-directory nil
  "Directory to save ODT files."
  :type '(choice (const nil) directory)
  :group 'markdown-odt)

(defcustom markdown-odt-include-footer t
  "Whether to include filename and date footer in the ODT."
  :type 'boolean
  :group 'markdown-odt)

(defun markdown-odt--get-output-path (input-file)
  "Get the output path for the ODT file based on INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-dir (or markdown-odt-output-directory (file-name-directory input-file)))
         (output-file (concat base-name ".odt")))
    (expand-file-name output-file output-dir)))

(defun markdown-odt--pandoc-available-p ()
  "Check if pandoc is available."
  (executable-find markdown-odt-pandoc-command))

(defun markdown-odt--open-odt (odt-file)
  "Open ODT-FILE with the default system application."
  (cond
   ((eq system-type 'darwin) (call-process "open" nil nil nil odt-file))
   ((eq system-type 'gnu/linux) (call-process "xdg-open" nil nil nil odt-file))
   ((eq system-type 'windows-nt) (w32-shell-execute "open" odt-file))
   (t (message "Cannot open ODT automatically"))))

(defun markdown-odt--preprocess-for-export (input-file)
  "Preprocess INPUT-FILE for export.
Returns path to preprocessed temporary file."
  (let ((temp-file (make-temp-file "markdown-odt-prep" nil ".md"))
        (content (with-temp-buffer
                   (insert-file-contents input-file)
                   (buffer-string))))
    (with-temp-file temp-file
      (insert content)
      (when markdown-odt-include-footer
        (goto-char (point-max))
        (insert "\n\n---\n\n")
        (insert (format "**%s** | %s"
                        (file-name-nondirectory input-file)
                        (format-time-string "%Y-%m-%d")))))
    temp-file))

;;;###autoload
(defun markdown-odt-export ()
  "Export the current markdown buffer to ODT."
  (interactive)
  (unless (markdown-odt--pandoc-available-p)
    (error "Pandoc is not available"))
  (let ((input-file (buffer-file-name))
        output-file temp-md result)
    (unless input-file
      (error "Buffer is not visiting a file"))
    (setq output-file (markdown-odt--get-output-path input-file))
    (when (buffer-modified-p)
      (when (y-or-n-p "Save buffer? ")
        (save-buffer)))
    ;; Preprocess markdown
    (setq temp-md (markdown-odt--preprocess-for-export input-file))
    (message "Exporting markdown to ODT...")
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    (let* ((pandoc-args (append
                        (list "-f" markdown-odt-markdown-format temp-md "-o" output-file "--standalone")
                        (when markdown-odt-reference-doc
                          (list "--reference-doc" markdown-odt-reference-doc)))))
      (setq result (apply 'call-process markdown-odt-pandoc-command nil "*pandoc-output*" t pandoc-args)))
    (when temp-md (delete-file temp-md))
    (if (= result 0)
        (progn
          (message "ODT exported successfully: %s" output-file)
          (when markdown-odt-open-after-export
            (markdown-odt--open-odt output-file)))
      (let ((error-output (when (get-buffer "*pandoc-output*")
                            (with-current-buffer "*pandoc-output*"
                              (buffer-string)))))
        (error "Pandoc export failed with exit code %d%s"
               result
               (if error-output (format "\nOutput: %s" error-output) ""))))))

;;;###autoload
(defun markdown-odt-export-and-open ()
  "Export the current markdown buffer to ODT and open it."
  (interactive)
  (let ((markdown-odt-open-after-export t))
    (markdown-odt-export)))

;; Keybinding is now handled by markdown-export.el for unified interface
;; Users can still call markdown-odt-export or markdown-odt-export-and-open directly

(provide 'markdown-odt)

;;; markdown-odt.el ends here
