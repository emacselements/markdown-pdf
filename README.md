# markdown-pdf / markdown-odt / markdown-docx / markdown-html / markdown-org

Export markdown files to beautifully formatted PDFs, ODT, DOCX, HTML, or Org-mode documents from Emacs. Also convert Org-mode to Markdown. Includes helpful markdown editing utilities. Inspired by the VSCode "Markdown PDF" extension.

## Support

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

## Features

### Export
- Export to PDF, ODT, DOCX, HTML, or Org format with a single keybinding
- Convert between Markdown and Org-mode formats
- Clean, professional output with optimized page layout
- Natural page break handling for improved readability
- Optional filename and date footer (for PDF/HTML exports)
- Smart typography support via Pandoc
- Customizable styling (CSS for PDF/HTML, reference docs for ODT/DOCX)
- Multiple PDF engine support (WeasyPrint, pdflatex, xelatex)

### Markdown Editing Utilities (markdown-tweaks)
- Insert current date in bold markdown format
- Add line breaks (two spaces) to selected text
- Format text as rubric with section sign (¶) and italics
- Smart file saving based on markdown title with directory selection

## Requirements

- Emacs 25.1+
- [Pandoc](https://pandoc.org/)
- [WeasyPrint](https://weasyprint.org/) (recommended) or pdflatex/xelatex

## Installation

### Manual

```elisp
;; Load the export functionality
(require 'markdown-export)

;; Optional: Load markdown editing utilities
(require 'markdown-tweaks)
```

### use-package

```elisp
(use-package markdown-export
  :load-path "/path/to/markdown-pdf")

;; Optional: markdown editing utilities
(use-package markdown-tweaks
  :load-path "/path/to/markdown-pdf"
  :after markdown-mode)
```

## Usage

### Export

#### From Markdown files

`C-c RET` - Choose export format: press `p` for PDF, `o` for ODT, `d` for DOCX, `h` for HTML, or `g` for Org

Or use the individual commands:
- `M-x markdown-pdf-export-and-open`
- `M-x markdown-odt-export-and-open`
- `M-x markdown-docx-export-and-open`
- `M-x markdown-html-export-and-open`
- `M-x markdown-org-export-and-open`

#### From Org files

`C-c RET` - Export to Markdown

Or use the command:
- `M-x org-markdown-export-and-open`

### Markdown Editing Utilities

When `markdown-tweaks` is loaded, the following keybindings are available in `markdown-mode`:

- `C-c .` - Insert current date in bold format: **2025-11-27 Wed**
- `C-c C-b` - Add two spaces at end of each line in selected region (for line breaks)
- `C-c C-r` - Format selected text as rubric (adds ¶ prefix, italics, and line breaks)
  - If no region is selected, inserts `*¶ *` template with cursor positioned for typing
- `C-x C-a` or `C-c C-s` - Save markdown file with title-based filename

**Title-based saving**: Choose destination directory via hydra menu:
- `d` - Documents
- `c` - code
- `n` - notes
- `s` - sermons

The filename is automatically generated from the first `# Title` in your markdown file, converted to lowercase with dashes.

## Configuration

### PDF Export

```elisp
;; Custom CSS file
(setq markdown-pdf-css-file "/path/to/custom.css")

;; Output directory (nil = same as source file)
(setq markdown-pdf-output-directory "~/Documents/PDFs/")

;; Auto-open after export
(setq markdown-pdf-open-after-export t)

;; Include filename and date footer (default: t)
(setq markdown-pdf-include-footer t)

;; Markdown format (default: "markdown+smart")
;; Options: "gfm", "markdown_strict", etc.
(setq markdown-pdf-markdown-format "markdown+smart")
```

### ODT Export

```elisp
;; Reference ODT document for styling
(setq markdown-odt-reference-doc "/path/to/reference.odt")

;; Output directory (nil = same as source file)
(setq markdown-odt-output-directory "~/Documents/ODTs/")

;; Auto-open after export
(setq markdown-odt-open-after-export t)

;; Include filename and date footer (default: t)
(setq markdown-odt-include-footer t)

;; Markdown format (same options as PDF)
(setq markdown-odt-markdown-format "markdown+smart")
```

### DOCX Export

```elisp
;; Reference DOCX document for styling
(setq markdown-docx-reference-doc "/path/to/reference.docx")

;; Output directory (nil = same as source file)
(setq markdown-docx-output-directory "~/Documents/DOCX/")

;; Auto-open after export
(setq markdown-docx-open-after-export t)

;; Include filename and date footer (default: t)
(setq markdown-docx-include-footer t)

;; Markdown format (same options as PDF)
(setq markdown-docx-markdown-format "markdown+smart")
```

### HTML Export

```elisp
;; Custom CSS file
(setq markdown-html-css-file "/path/to/custom.css")

;; Output directory (nil = same as source file)
(setq markdown-html-output-directory "~/Documents/HTML/")

;; Auto-open after export
(setq markdown-html-open-after-export t)

;; Include filename and date footer (default: t)
(setq markdown-html-include-footer t)

;; Markdown format (same options as PDF)
(setq markdown-html-markdown-format "markdown+smart")
```

### Org Export (Markdown to Org)

```elisp
;; Output directory (nil = same as source file)
(setq markdown-org-output-directory "~/Documents/Org/")

;; Auto-open after export
(setq markdown-org-open-after-export t)

;; Markdown format (same options as PDF)
(setq markdown-org-markdown-format "markdown+smart")
```

### Markdown Export (Org to Markdown)

```elisp
;; Output directory (nil = same as source file)
(setq org-markdown-output-directory "~/Documents/Markdown/")

;; Auto-open after export
(setq org-markdown-open-after-export t)

;; Markdown output format (default: "gfm")
;; Options: "gfm" (GitHub), "markdown", "markdown_strict", etc.
(setq org-markdown-format "gfm")
```

### Markdown Editing Utilities (markdown-tweaks)

To customize the directories for title-based file saving, edit the `hydra-markdown-save-directory` function in `markdown-tweaks.el`:

```elisp
(defhydra hydra-markdown-save-directory (:color blue :hint nil)
  "Save markdown file to directory:"
  ("c" (markdown-save-with-title-to-dir "/home/user/code"))
  ("d" (markdown-save-with-title-to-dir "/home/user/Documents"))
  ("n" (markdown-save-with-title-to-dir "/home/user/notes"))
  ("s" (markdown-save-with-title-to-dir "/home/user/sermons"))
  ("q" nil))
```

## Files in this Repository

- `markdown-export.el` - Main entry point, loads all export modules
- `markdown-pdf.el` - PDF export functionality
- `markdown-odt.el` - ODT export functionality
- `markdown-docx.el` - DOCX export functionality
- `markdown-html.el` - HTML export functionality
- `markdown-org.el` - Markdown to Org export
- `org-markdown.el` - Org to Markdown export (main implementation)
- `org-to-markdown.el` - Alternative Org to Markdown converter
- `markdown-tweaks.el` - Editing utilities and helper functions

## License

GPL-3.0
