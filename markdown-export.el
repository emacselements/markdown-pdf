;;; markdown-export.el --- Unified markdown export to PDF or ODT -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, pdf, odt, export

;;; Commentary:
;; This package provides a unified interface for exporting markdown
;; to PDF, ODT, or DOCX format using C-c RET.
;; Automatically loads markdown-pdf, markdown-odt, and markdown-docx.

;;; Code:

(require 'markdown-mode nil t)
(require 'markdown-pdf)
(require 'markdown-odt)
(require 'markdown-docx)

(defcustom markdown-export-default-format 'pdf
  "Default export format for markdown files."
  :type '(choice (const :tag "PDF" pdf)
                 (const :tag "ODT" odt)
                 (const :tag "DOCX" docx))
  :group 'markdown)

;;;###autoload
(defun markdown-export-choose-format ()
  "Export markdown to PDF, ODT, or DOCX based on user choice.
Press 'p' for PDF, 'o' for ODT, or 'd' for DOCX."
  (interactive)
  (let ((choice (read-char-choice "Export format: (p)df, (o)dt, or (d)ocx? " '(?p ?o ?d))))
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
