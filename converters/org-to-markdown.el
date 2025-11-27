;;; org-to-markdown.el --- Convert org files to markdown  -*- lexical-binding: t; -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org, markdown, conversion

;;; Commentary:

;; Simple org to markdown converter for notes files.
;; Handles headings, dates, emphasis, lists, and tags.
;;
;; Usage:
;; M-x org-to-markdown-convert-file RET filename.org RET
;; M-x org-to-markdown-convert-directory RET
;;
;; To convert all org files in current directory:
;; M-x org-to-markdown-convert-directory RET

;;; Code:

(require 'subr-x)

(defun org-to-markdown--convert-heading (line)
  "Convert org heading LINE to markdown."
  (when (string-match "^\\(\\*+\\)\\s-+\\(.*?\\)\\(\\s-+:\\([^:]+\\):\\)?\\s-*$" line)
    (let* ((stars (match-string 1 line))
           (heading-text (match-string 2 line))
           (tags-str (match-string 4 line))
           (level (length stars))
           (md-heading (concat (make-string level ?#) " " heading-text)))
      ;; Add tags in markdown format
      (if tags-str
          (let ((tags (split-string tags-str ":" t)))
            (concat md-heading " "
                    (mapconcat (lambda (tag) (concat "#" tag)) tags " ")))
        md-heading))))

(defun org-to-markdown--convert-date (line)
  "Convert org date LINE to plain text."
  (replace-regexp-in-string
   "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\s-+[A-Za-z]+>"
   "\\1"
   line))

(defun org-to-markdown--convert-emphasis (line)
  "Convert org emphasis in LINE to markdown."
  ;; *bold* to **bold** (but not at start of line, those are headings)
  (setq line (replace-regexp-in-string
              "\\(?:^\\|[^*]\\)\\(\\*\\)\\([^*\n]+\\)\\(\\*\\)\\(?:[^*]\\|$\\)"
              "**\\2**"
              line))
  ;; /italic/ to *italic*
  (setq line (replace-regexp-in-string
              "/\\([^/\n]+\\)/"
              "*\\1*"
              line))
  ;; _underline_ to __underline__
  (setq line (replace-regexp-in-string
              "_\\([^_\n]+\\)_"
              "__\\1__"
              line))
  ;; =code= to `code`
  (setq line (replace-regexp-in-string
              "=\\([^=\n]+\\)="
              "`\\1`"
              line))
  line)

(defun org-to-markdown--convert-line (line)
  "Convert a single org LINE to markdown."
  (cond
   ;; Org headings
   ((string-match "^\\*+" line)
    (or (org-to-markdown--convert-heading line) line))
   ;; Lines with dates
   ((string-match "<[0-9]\\{4\\}-" line)
    (org-to-markdown--convert-date line))
   ;; Lines with emphasis (but not headings)
   ((and (or (string-match "[*/=_]" line))
         (not (string-prefix-p "*" line)))
    (org-to-markdown--convert-emphasis line))
   ;; Everything else stays the same
   (t line)))

(defun org-to-markdown--convert-buffer-content ()
  "Convert current buffer from org to markdown."
  (let ((lines (split-string (buffer-string) "\n")))
    (mapconcat #'org-to-markdown--convert-line lines "\n")))

;;;###autoload
(defun org-to-markdown-convert-file (org-file)
  "Convert ORG-FILE to markdown.
Creates a new .md file alongside the original .org file."
  (interactive "fOrg file to convert: ")
  (let* ((org-path (expand-file-name org-file))
         (md-path (concat (file-name-sans-extension org-path) ".md")))

    (unless (file-exists-p org-path)
      (user-error "File does not exist: %s" org-path))

    (when (and (file-exists-p md-path)
               (not (y-or-n-p (format "File %s exists. Overwrite? "
                                      (file-name-nondirectory md-path)))))
      (user-error "Conversion cancelled"))

    (with-temp-buffer
      (insert-file-contents org-path)
      (let ((markdown-content (org-to-markdown--convert-buffer-content)))
        (with-temp-file md-path
          (insert markdown-content))))

    (message "Converted: %s -> %s"
             (file-name-nondirectory org-path)
             (file-name-nondirectory md-path))

    ;; Offer to open the new file
    (when (y-or-n-p "Open converted file? ")
      (find-file md-path))

    md-path))

;;;###autoload
(defun org-to-markdown-convert-directory (&optional directory)
  "Convert all .org files in DIRECTORY to markdown.
If DIRECTORY is nil, use current directory."
  (interactive)
  (let* ((dir (or directory default-directory))
         (org-files (directory-files dir t "\\.org$"))
         (count 0))

    (unless org-files
      (user-error "No .org files found in %s" dir))

    (when (y-or-n-p (format "Convert %d org file(s) to markdown? "
                            (length org-files)))
      (dolist (org-file org-files)
        (condition-case err
            (progn
              (org-to-markdown-convert-file org-file)
              (setq count (1+ count)))
          (error
           (message "Error converting %s: %s"
                    (file-name-nondirectory org-file)
                    (error-message-string err)))))

      (message "Converted %d of %d files" count (length org-files))

      ;; Offer to delete original .org files
      (when (and (> count 0)
                 (y-or-n-p "Delete original .org files? "))
        (dolist (org-file org-files)
          (let ((md-file (concat (file-name-sans-extension org-file) ".md")))
            (when (file-exists-p md-file)
              (delete-file org-file)
              (message "Deleted: %s" (file-name-nondirectory org-file)))))
        (message "Cleanup complete")))))

;;;###autoload
(defun org-to-markdown-convert-current-buffer ()
  "Convert current org buffer to markdown and save as .md file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in org-mode"))

  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))

  (org-to-markdown-convert-file (buffer-file-name)))

(provide 'org-to-markdown)

;;; org-to-markdown.el ends here
