;;; markdown-export.el --- Unified markdown export to PDF or ODT -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: markdown, pdf, odt, export

;;; Commentary:
;; This package provides a unified interface for exporting markdown
;; to PDF, ODT, DOCX, HTML, or Org format using C-c RET.
;; Also provides Org to Markdown conversion.
;; Automatically loads all export modules.

;;; Code:

(require 'markdown-mode nil t)
(require 'org nil t)

;; Add the directory of this file to load-path if needed
(eval-and-compile
  (let ((dir (file-name-directory (or load-file-name
                                      (buffer-file-name)
                                      default-directory))))
    (add-to-list 'load-path dir)
    (add-to-list 'load-path (expand-file-name "exporters" dir))
    (add-to-list 'load-path (expand-file-name "converters" dir))))

(require 'markdown-pdf)
(require 'markdown-odt)
(require 'markdown-docx)
(require 'markdown-html)
(require 'markdown-org)
(require 'org-markdown)

(defcustom markdown-export-default-format 'pdf
  "Default export format for markdown files."
  :type '(choice (const :tag "PDF" pdf)
                 (const :tag "ODT" odt)
                 (const :tag "DOCX" docx)
                 (const :tag "HTML" html)
                 (const :tag "Org" org))
  :group 'markdown)

;;;###autoload
(defun markdown-export-choose-format ()
  "Export markdown to various formats based on user choice.
Press 'p' for PDF, 'o' for ODT, 'd' for DOCX, 'h' for HTML, or 'g' for Org."
  (interactive)
  (let ((choice (read-char-choice "Export: (p)df, (o)dt, (d)ocx, (h)tml, or or(g)? " '(?p ?o ?d ?h ?g))))
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
     ((eq choice ?g)
      (if (fboundp 'markdown-org-export-and-open)
          (markdown-org-export-and-open)
        (error "markdown-org not loaded")))
     (t (error "Invalid format selected")))))

;;;###autoload
(defun org-export-choose-format ()
  "Export org to Markdown.
Currently only supports Markdown export from Org-mode."
  (interactive)
  (if (fboundp 'org-markdown-export-and-open)
      (org-markdown-export-and-open)
    (error "org-markdown not loaded")))

;;;###autoload
(defun markdown-export-setup-keybinding ()
  "Set up the unified keybinding for markdown export."
  (when (featurep 'markdown-mode)
    (define-key markdown-mode-map (kbd "C-c RET") #'markdown-export-choose-format)))

;;;###autoload
(defun org-export-setup-keybinding ()
  "Set up the unified keybinding for org export."
  (when (featurep 'org)
    (define-key org-mode-map (kbd "C-c RET") #'org-export-choose-format)))

(eval-after-load 'markdown-mode
  '(markdown-export-setup-keybinding))

(eval-after-load 'org
  '(org-export-setup-keybinding))

(when (featurep 'markdown-mode)
  (markdown-export-setup-keybinding))

(when (featurep 'org)
  (org-export-setup-keybinding))

(provide 'markdown-export)

;;; markdown-export.el ends here
