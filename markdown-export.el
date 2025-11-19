;;; markdown-export.el --- Unified markdown export to PDF or ODT -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, pdf, odt, export

;;; Commentary:
;; This package provides a unified interface for exporting markdown
;; to PDF, ODT, DOCX, or HTML format using C-c RET.
;; Automatically loads markdown-pdf, markdown-odt, markdown-docx, and markdown-html.

;;; Code:

(require 'markdown-mode nil t)

;; Add the directory of this file to load-path if needed
(eval-and-compile
  (let ((dir (file-name-directory (or load-file-name
                                      (buffer-file-name)
                                      default-directory))))
    (add-to-list 'load-path dir)))

(require 'markdown-pdf)
(require 'markdown-odt)
(require 'markdown-docx)
(require 'markdown-html)

(defcustom markdown-export-default-format 'pdf
  "Default export format for markdown files."
  :type '(choice (const :tag "PDF" pdf)
                 (const :tag "ODT" odt)
                 (const :tag "DOCX" docx)
                 (const :tag "HTML" html))
  :group 'markdown)

;;;###autoload
(defun markdown-export-choose-format ()
  "Export markdown to PDF, ODT, DOCX, or HTML based on user choice.
Press 'p' for PDF, 'o' for ODT, 'd' for DOCX, or 'h' for HTML."
  (interactive)
  (let ((choice (read-char-choice "Export format: (p)df, (o)dt, (d)ocx, or (h)tml? " '(?p ?o ?d ?h))))
    (cond
     ((eq choice ?p)
      (if (fboundp 'markdown-pdf-export-and-open)
          (markdown-pdf-export-and-open)
        (error "markdown-pdf not loaded")))
     ((eq choice ?o)
      (if (fboundp 'markdown-odt-export-and-open)
          (markdown-odt-export-and-open)
        (error "markdown-odt not loaded")))
     ((eq choice ?d)
      (if (fboundp 'markdown-docx-export-and-open)
          (markdown-docx-export-and-open)
        (error "markdown-docx not loaded")))
     ((eq choice ?h)
      (if (fboundp 'markdown-html-export-and-open)
          (markdown-html-export-and-open)
        (error "markdown-html not loaded")))
     (t (error "Invalid format selected")))))

;;;###autoload
(defun markdown-export-setup-keybinding ()
  "Set up the unified keybinding for markdown export."
  (when (featurep 'markdown-mode)
    (define-key markdown-mode-map (kbd "C-c RET") #'markdown-export-choose-format)))

(eval-after-load 'markdown-mode
  '(markdown-export-setup-keybinding))

(when (featurep 'markdown-mode)
  (markdown-export-setup-keybinding))

(provide 'markdown-export)

;;; markdown-export.el ends here
