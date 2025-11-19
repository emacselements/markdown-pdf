# markdown-pdf / markdown-odt

Export markdown files to beautifully formatted PDFs or ODT documents from Emacs. Inspired by the VSCode "Markdown PDF" extension.

## Support

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

## Features

- Export to PDF or ODT format with a single keybinding
- Clean, professional output with optimized page layout
- Natural page break handling for improved readability
- Optional filename and date footer
- Smart typography support via Pandoc
- Customizable styling (CSS for PDF, reference docs for ODT)
- Multiple PDF engine support (WeasyPrint, pdflatex, xelatex)

## Requirements

- Emacs 25.1+
- [Pandoc](https://pandoc.org/)
- [WeasyPrint](https://weasyprint.org/) (recommended) or pdflatex/xelatex

## Installation

### Manual

```elisp
(require 'markdown-export)  ; This loads everything
```

### use-package

```elisp
(use-package markdown-export
  :load-path "/path/to/markdown-pdf")
```

## Usage

`C-c RET` - Choose export format: press `p` for PDF or `o` for ODT

Or use the individual commands:
`M-x markdown-pdf-export-and-open`
`M-x markdown-odt-export-and-open`

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

## License

GPL-3.0
