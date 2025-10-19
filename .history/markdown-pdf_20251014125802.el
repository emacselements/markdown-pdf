;;; markdown-pdf.el --- Pretty markdown to PDF export for Emacs

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, pdf, export

;;; Commentary:
;; This package provides pretty markdown to PDF export functionality
;; similar to the VSCode "Markdown PDF" extension.

;;; Code:

(require 'markdown-mode nil t)

(defgroup markdown-pdf nil
  "Markdown to PDF export settings."
  :group 'markdown)

(defcustom markdown-pdf-pandoc-command "pandoc"
  "Command to run pandoc."
  :type 'string
  :group 'markdown-pdf)

(defcustom markdown-pdf-css-file nil
  "Path to custom CSS file for styling the PDF output."
  :type '(choice (const nil) file)
  :group 'markdown-pdf)

(defcustom markdown-pdf-markdown-format "markdown+smart"
  "Pandoc markdown format to use for parsing.
Common options:
- markdown+smart (default, smart quotes and typography)
- gfm (GitHub Flavored Markdown)
- markdown_strict (strict markdown)
- markdown+all_symbols_escapable (allows escaping any symbol)"
  :type 'string
  :group 'markdown-pdf)

(defcustom markdown-pdf-convert-slash-emphasis t
  "Convert /text/ to *text* for italic formatting.
When non-nil, text enclosed in forward slashes will be converted
to standard markdown italic syntax before processing."
  :type 'boolean
  :group 'markdown-pdf)

(defcustom markdown-pdf-open-after-export t
  "Whether to open the PDF file after export."
  :type 'boolean
  :group 'markdown-pdf)

(defcustom markdown-pdf-output-directory nil
  "Directory to save PDF files."
  :type '(choice (const nil) directory)
  :group 'markdown-pdf)

(defvar markdown-pdf-default-css
  "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe WPC','Segoe UI',sans-serif;font-size:14px;line-height:22px;padding:5px;margin:0;color:rgb(34,34,34);background:white;font-weight:500}h1,h2,h3,h4,h5,h6{font-weight:normal;margin-top:24px;margin-bottom:16px;line-height:1.25;color:rgb(34,34,34)}h1{font-size:2em;margin-top:0;padding-bottom:0.3em;border-bottom:1px solid rgb(34,34,34)}h2{font-size:1.5em}h3{font-size:1.25em}h4{font-size:1em}h5{font-size:0.875em}h6{font-size:0.85em}p{margin-top:0;margin-bottom:16px}li p{margin-bottom:0.7em}a{text-decoration:none}a:hover{text-decoration:underline}ul,ol{margin-bottom:0.7em}code{font-family:'SF Mono',Monaco,Consolas,monospace;font-size:1em}:not(pre):not(.hljs)>code{color:rgb(201,174,117)}pre{background-color:rgb(248,248,248);border:1px solid rgb(204,204,204);border-radius:3px;padding:16px;margin-bottom:0.7em}blockquote{margin:0;padding:0 16px 0 10px;border-left:5px solid rgba(0,122,204,0.5);background:rgba(127,127,127,0.1)}table{border-collapse:collapse;margin-bottom:0.7em}th{text-align:left;border-bottom:1px solid rgba(0,0,0,0.69);padding:5px 10px}td{padding:5px 10px}hr{border:0;height:1px;border-bottom:1px solid rgba(0,0,0,0.18)}img{max-width:100%}.page{page-break-after:always}"
  "Default CSS styling for markdown PDF export.")

(defun markdown-pdf--preprocess-buffer ()
  "Preprocess the current buffer for PDF export.
Currently handles conversion of /text/ to *text* for italic formatting
if `markdown-pdf-convert-slash-emphasis' is non-nil."
  (when markdown-pdf-convert-slash-emphasis
    (save-excursion
      (goto-char (point-min))
      ;; Convert /text/ to *text* but avoid URLs and file paths
      (while (re-search-forward "/\\([^/\n\r]+\\)/" nil t)
        (let ((matched-text (match-string 1)))
          ;; Skip if it looks like a URL or file path
          (unless (or (string-match-p "^https?:" matched-text)
                      (string-match-p "^[a-zA-Z]:" matched-text)
                      (string-match-p "/" matched-text))
            (replace-match (concat "*" matched-text "*"))))))))

(defun markdown-pdf--get-css-file ()
  "Get the CSS file to use for styling."
  (if markdown-pdf-css-file
      markdown-pdf-css-file
    (let ((temp-css (make-temp-file "markdown-pdf" nil ".css")))
      (with-temp-file temp-css
        (insert markdown-pdf-default-css))
      temp-css)))

(defun markdown-pdf--get-output-path (input-file)
  "Get the output path for the PDF file based on INPUT-FILE."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory input-file)))
         (output-dir (or markdown-pdf-output-directory (file-name-directory input-file)))
         (output-file (concat base-name ".pdf")))
    (expand-file-name output-file output-dir)))

(defun markdown-pdf--pandoc-available-p ()
  "Check if pandoc is available."
  (executable-find markdown-pdf-pandoc-command))

(defun markdown-pdf--find-available-engine (engines)
  "Find the first available PDF engine from ENGINES list."
  (catch 'found
    (dolist (engine engines)
      (when (executable-find engine)
        (throw 'found engine)))
    nil))

(defun markdown-pdf--open-pdf (pdf-file)
  "Open PDF-FILE with the default system application."
  (cond
   ((eq system-type 'darwin) (call-process "open" nil nil nil pdf-file))
   ((eq system-type 'gnu/linux) (call-process "xdg-open" nil nil nil pdf-file))
   ((eq system-type 'windows-nt) (w32-shell-execute "open" pdf-file))
   (t (message "Cannot open PDF automatically"))))

;;;###autoload
(defun markdown-pdf-export ()
  "Export the current markdown buffer to PDF."
  (interactive)
  (unless (markdown-pdf--pandoc-available-p)
    (error "Pandoc is not available"))
  (let ((input-file (buffer-file-name))
        (original-input-file (buffer-file-name))
        output-file css-file temp-css-p temp-html temp-input-p result)
    (unless input-file
      (error "Buffer is not visiting a file"))
    (setq output-file (markdown-pdf--get-output-path input-file))
    (setq css-file (markdown-pdf--get-css-file))
    (setq temp-css-p (not markdown-pdf-css-file))
    (when (buffer-modified-p)
      (when (y-or-n-p "Save buffer? ")
        (save-buffer)))
    ;; Preprocess buffer if needed, save to temporary file
    (let ((temp-input-file input-file))
      (when markdown-pdf-convert-slash-emphasis
        (setq temp-input-file (make-temp-file "markdown-pdf-input" nil ".md"))
        (with-temp-file temp-input-file
          (insert-buffer-substring (current-buffer))
          (markdown-pdf--preprocess-buffer)))
      (setq input-file temp-input-file))
    (message "Exporting markdown to PDF...")
    (let ((output-dir (file-name-directory output-file)))
      (unless (file-directory-p output-dir)
        (make-directory output-dir t)))
    (setq temp-html (make-temp-file "markdown-pdf" nil ".html"))
    (with-temp-file temp-html
      (insert (format "<div style='font-size:12px;padding:8px 0;border-bottom:1px solid rgb(180,180,180);margin-bottom:12px;font-weight:500'><span style='float:left'>%s</span><span style='float:right'>%s</span><div style='clear:both'></div></div>"
                      (file-name-nondirectory input-file)
                      (format-time-string "%Y-%m-%d"))))
    (let* ((pdf-engines '("weasyprint" "pdflatex" "xelatex"))
           (available-engine (markdown-pdf--find-available-engine pdf-engines))
           (pandoc-args (append
                        (list "-f" markdown-pdf-markdown-format input-file "-o" output-file "--css" css-file "--self-contained")
                        (when available-engine (list "--pdf-engine" available-engine))
                        (list "--metadata" "margin-top=0.5in"
                              "--metadata" "margin-bottom=0.1in"
                              "--metadata" "margin-left=0.1in"
                              "--metadata" "margin-right=0.1in"
                              "--include-before-body" temp-html))))
      (setq result (apply 'call-process markdown-pdf-pandoc-command nil "*pandoc-output*" nil pandoc-args)))
    (when temp-css-p (delete-file css-file))
    (when temp-html (delete-file temp-html))
    (if (= result 0)
        (progn
          (message "PDF exported successfully: %s" output-file)
          (when markdown-pdf-open-after-export
            (markdown-pdf--open-pdf output-file)))
      (error "Pandoc export failed with exit code %d" result))))

;;;###autoload
(defun markdown-pdf-export-and-open ()
  "Export the current markdown buffer to PDF and open it."
  (interactive)
  (let ((markdown-pdf-open-after-export t))
    (markdown-pdf-export)))

;;;###autoload
(defun markdown-pdf-setup-keybinding ()
  "Set up the keybinding for markdown PDF export."
  (when (featurep 'markdown-mode)
    (define-key markdown-mode-map (kbd "C-c ,") 'markdown-pdf-export-and-open)))

(eval-after-load 'markdown-mode
  '(markdown-pdf-setup-keybinding))

(when (featurep 'markdown-mode)
  (markdown-pdf-setup-keybinding))

(provide 'markdown-pdf)

;;; markdown-pdf.el ends here
