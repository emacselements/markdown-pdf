# Emacs Markdown Tools

A set of tools to enhance markdown in Emacs consisting of:

a) Export functions: Export markdown files to beautifully formatted PDFs (inspired by the VSCode "Markdown PDF" extension), ODT, DOCX, HTML, or Org-mode documents. Convert between Markdown and Org-mode formats bidirectionally. 

b) Markdown editing utilities for date insertion, line break formatting, rubric formatting, and smart title-based file saving.

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
;; Load the export functionality
(require 'markdown-export)

;; Optional: Load markdown editing utilities
(require 'markdown-tweaks)
```

## License

GPL-3.0
