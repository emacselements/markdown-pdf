# markdown-pdf.el

Export markdown files to beautifully formatted PDFs from Emacs. Inspired by the VSCode "Markdown PDF" extension.

## Support

If you find this project helpful, consider supporting it!

[Donate via PayPal](https://www.paypal.com/paypalme/revrari)

## Requirements

- Emacs 25.1+
- [Pandoc](https://pandoc.org/)
- [WeasyPrint](https://weasyprint.org/) (recommended) or pdflatex/xelatex

## Installation

### Manual

```elisp
(require 'markdown-pdf)
```

### use-package

```elisp
(use-package markdown-pdf
  :load-path "/path/to/markdown-pdf")
```

## Usage

`C-c RET` - Export and open PDF (in markdown-mode)

Or use `M-x markdown-pdf-export-and-open`

## Configuration

```elisp
;; Custom CSS file
(setq markdown-pdf-css-file "/path/to/custom.css")

;; Output directory (nil = same as source file)
(setq markdown-pdf-output-directory "~/Documents/PDFs/")

;; Auto-open after export
(setq markdown-pdf-open-after-export t)
```

## License

GPL-3.0
