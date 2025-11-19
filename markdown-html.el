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
  "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe WPC','Segoe UI',sans-serif;font-size:14px;line-height:1.6;padding:20px;margin:0 auto;max-width:800px;color:rgb(34,34,34);background:white}h1,h2,h3,h4,h5,h6{font-weight:normal;margin-top:24px;margin-bottom:16px;line-height:1.25;color:rgb(34,34,34)}h1{font-size:2em;padding-bottom:0.3em;border-bottom:1px solid rgb(234,234,234)}h2{font-size:1.5em}h3{font-size:1.25em}h4{font-size:1em}h5{font-size:0.875em}h6{font-size:0.85em}p{margin-top:0;margin-bottom:16px}a{text-decoration:none;color:rgb(0,122,204)}a:hover{text-decoration:underline}ul,ol{margin-bottom:16px}code{font-family:'SF Mono',Monaco,Consolas,monospace;font-size:0.9em}:not(pre)>code{background-color:rgba(27,31,35,0.05);padding:0.2em 0.4em;border-radius:3px}pre{background-color:rgb(248,248,248);border:1px solid rgb(204,204,204);border-radius:3px;padding:16px;margin-bottom:16px;overflow:auto}blockquote{margin:0;padding:0 16px;border-left:4px solid rgba(0,122,204,0.5);background:rgba(127,127,127,0.05)}table{border-collapse:collapse;margin-bottom:16px;width:100%}th{text-align:left;border-bottom:1px solid rgba(0,0,0,0.2);padding:8px}td{padding:8px;border-bottom:1px solid rgba(0,0,0,0.1)}hr{border:0;height:1px;border-bottom:1px solid rgba(0,0,0,0.1);margin:24px 0}img{max-width:100%;height:auto}"
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
        (insert (format "<div style='font-size:12px;padding:12px 0;border-top:1px solid rgb(220,220,220);margin-top:24px;color:rgb(100,100,100)'><span style='float:left'>%s</span><span style='float:right'>%s</span><div style='clear:both'></div></div>"
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
