;;; markdown-export.el --- Unified markdown export to PDF or ODT -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, pdf, odt, export

;;; Commentary:
;; This package provides a unified interface for exporting markdown
;; to either PDF or ODT format using C-c RET.
;; Automatically loads both markdown-pdf and markdown-odt.

;;; Code:

(require 'markdown-mode nil t)
(require 'markdown-pdf)
(require 'markdown-odt)

(defcustom markdown-export-default-format 'pdf
  "Default export format for markdown files."
  :type '(choice (const :tag "PDF" pdf)
                 (const :tag "ODT" odt))
  :group 'markdown)

;;;###autoload
(defun markdown-export-choose-format ()
  "Export markdown to PDF or ODT based on user choice.
Press 'p' for PDF or 'o' for ODT."
  (interactive)
  (let ((choice (read-char-choice "Export format: (p)df or (o)dt? " '(?p ?o))))
    (cond
     ((eq choice ?p)
      (if (fboundp 'markdown-pdf-export-and-open)
          (markdown-pdf-export-and-open)
        (error "markdown-pdf not loaded")))
     ((eq choice ?o)
      (if (fboundp 'markdown-odt-export-and-open)
          (markdown-odt-export-and-open)
        (error "markdown-odt not loaded")))
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
