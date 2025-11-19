;;; markdown-html.el --- Pretty markdown to HTML export for Emacs -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, html, export

;;; Commentary:
;; This package provides markdown to HTML export functionality
;; using pandoc.

;;; Code:

(require 'markdown-mode nil t)

(defgroup markdown-html nil
  "Markdown to HTML export settings."
  :group 'markdown)

(defcustom markdown-html-pandoc-command "pandoc"
  "Command to run pandoc."
  :type 'string
  :group 'markdown-html)

(defcustom markdown-html-css-file nil
  "Path to custom CSS file for styling the HTML output."
  :type '(choice (const nil) file)
  :group 'markdown-html)

(defcustom markdown-html-markdown-format "markdown+smart"
  "Pandoc markdown format to use for parsing.
Common options:
- markdown+smart (default, smart quotes and typography)
- gfm (GitHub Flavored Markdown)
- markdown_strict (strict markdown)
- markdown+all_symbols_escapable (allows escaping any symbol)"
  :type 'string
  :group 'markdown-html)

(defcustom markdown-html-open-after-export t
  "Whether to open the HTML file after export."
  :type 'boolean
  :group 'markdown-html)

(defcustom markdown-html-output-directory nil
  "Directory to save HTML files."
  :type '(choice (const nil) directory)
  :group 'markdown-html)

(defcustom markdown-html-include-footer t
  "Whether to include filename and date footer in the HTML."
  :type 'boolean
  :group 'markdown-html)

(defvar markdown-html-default-css
  "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif;font-size:16px;line-height:1.7;padding:40px;margin:0 auto;max-width:900px;color:#2c3e50;background:#ffffff}h1,h2,h3,h4,h5,h6{font-weight:600;margin-top:32px;margin-bottom:16px;line-height:1.3;color:#1a1a1a}h1{font-size:2.5em;margin-top:0;padding-bottom:0.3em;border-bottom:2px solid #e1e4e8}h2{font-size:2em;padding-bottom:0.2em;border-bottom:1px solid #eaecef}h3{font-size:1.5em}h4{font-size:1.25em}h5{font-size:1em}h6{font-size:0.875em;color:#6a737d}p{margin-top:0;margin-bottom:16px}a{text-decoration:none;color:#0366d6;font-weight:500}a:hover{text-decoration:underline;color:#0056b3}ul,ol{margin-bottom:16px;padding-left:2em}li{margin-bottom:0.25em}code{font-family:'SF Mono',Monaco,Consolas,'Liberation Mono','Courier New',monospace;font-size:0.9em}:not(pre)>code{background-color:#f6f8fa;padding:0.2em 0.4em;border-radius:4px;color:#d73a49;border:1px solid #e1e4e8}pre{background-color:#f6f8fa;border:1px solid #d1d5da;border-radius:6px;padding:16px;margin-bottom:16px;overflow:auto;line-height:1.45}pre code{background-color:transparent;padding:0;border:none;color:#24292e}blockquote{margin:0 0 16px 0;padding:0 1em;color:#6a737d;border-left:4px solid #dfe2e5;background:#f8f9fa}blockquote>:first-child{margin-top:0}blockquote>:last-child{margin-bottom:0}table{border-collapse:collapse;margin-bottom:16px;width:100%;overflow:auto;display:block;border:1px solid #d1d5da;border-radius:6px}thead{background-color:#f6f8fa}th{text-align:left;border-bottom:2px solid #d1d5da;padding:10px 13px;font-weight:600}td{padding:10px 13px;border-bottom:1px solid #e1e4e8}tr:last-child td{border-bottom:none}tbody tr:hover{background-color:#f6f8fa}hr{border:0;height:2px;background:#e1e4e8;margin:32px 0}img{max-width:100%;height:auto;border-radius:4px;box-shadow:0 2px 8px rgba(0,0,0,0.1)}strong{font-weight:600;color:#1a1a1a}em{font-style:italic;color:#5a5a5a}"
  "Default CSS styling for markdown HTML export.")

(defun markdown-html--get-css-file ()
  "Get the CSS file to use for styling."
  (if markdown-html-css-file
      markdown-html-css-file
    (let ((temp-css (make-temp-file "markdown-html" nil ".css")))
      (with-temp-file temp-css
        (insert markdown-html-default-css))
      temp-css)))

(defun markdown-html--get-output-path (input-file)
  "Get the output path for the HTML file based on INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-dir (or markdown-html-output-directory (file-name-directory input-file)))
         (output-file (concat base-name ".html")))
    (expand-file-name output-file output-dir)))

(defun markdown-html--pandoc-available-p ()
  "Check if pandoc is available."
  (executable-find markdown-html-pandoc-command))

(defun markdown-html--open-html (html-file)
  "Open HTML-FILE with the default system application."
  (cond
   ((eq system-type 'darwin) (call-process "open" nil nil nil html-file))
   ((eq system-type 'gnu/linux) (call-process "xdg-open" nil nil nil html-file))
   ((eq system-type 'windows-nt) (w32-shell-execute "open" html-file))
   (t (message "Cannot open HTML automatically"))))

(defun markdown-html--preprocess-for-export (input-file)
  "Preprocess INPUT-FILE for export.
Returns path to preprocessed temporary file."
  (let ((temp-file (make-temp-file "markdown-html-prep" nil ".md")))
    (copy-file input-file temp-file t)
    temp-file))

;;;###autoload
(defun markdown-html-export ()
  "Export the current markdown buffer to HTML."
  (interactive)
  (unless (markdown-html--pandoc-available-p)
    (error "Pandoc is not available"))
  (let ((input-file (buffer-file-name))
        output-file css-file temp-css-p temp-footer temp-md result)
    (unless input-file
      (error "Buffer is not visiting a file"))
    (setq output-file (markdown-html--get-output-path input-file))
    (setq css-file (markdown-html--get-css-file))
    (setq temp-css-p (not markdown-html-css-file))
    (when (buffer-modified-p)
      (when (y-or-n-p "Save buffer? ")
        (save-buffer)))
    ;; Preprocess markdown
    (setq temp-md (markdown-html--preprocess-for-export input-file))
    (message "Exporting markdown to HTML...")
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    (when markdown-html-include-footer
      (setq temp-footer (make-temp-file "markdown-html" nil ".html"))
      (with-temp-file temp-footer
        (insert (format "<div style='font-size:13px;padding:16px 0;border-top:2px solid #e1e4e8;margin-top:48px;color:#6a737d;font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,sans-serif'><span style='float:left;font-weight:500'>%s</span><span style='float:right'>%s</span><div style='clear:both'></div></div>"
                        (file-name-nondirectory input-file)
                        (format-time-string "%Y-%m-%d")))))
    (let* ((pandoc-args (append
                        (list "-f" markdown-html-markdown-format temp-md "-o" output-file
                              "--css" css-file "--embed-resources" "--standalone")
                        (when markdown-html-include-footer
                          (list "--include-after-body" temp-footer)))))
      (setq result (apply 'call-process markdown-html-pandoc-command nil "*pandoc-output*" t pandoc-args)))
    (when temp-css-p (delete-file css-file))
    (when (and markdown-html-include-footer temp-footer) (delete-file temp-footer))
    (when temp-md (delete-file temp-md))
    (if (= result 0)
        (progn
          (message "HTML exported successfully: %s" output-file)
          (when markdown-html-open-after-export
            (markdown-html--open-html output-file)))
      (let ((error-output (when (get-buffer "*pandoc-output*")
                            (with-current-buffer "*pandoc-output*"
                              (buffer-string)))))
        (error "Pandoc export failed with exit code %d%s"
               result
               (if error-output (format "\nOutput: %s" error-output) ""))))))

;;;###autoload
(defun markdown-html-export-and-open ()
  "Export the current markdown buffer to HTML and open it."
  (interactive)
  (let ((markdown-html-open-after-export t))
    (markdown-html-export)))

;; Keybinding is now handled by markdown-export.el for unified interface
;; Users can still call markdown-html-export or markdown-html-export-and-open directly

(provide 'markdown-html)

;;; markdown-html.el ends here
