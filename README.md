# Markdown Tools for Emacs

Markdown export and editing utilities for Emacs.

## Features

**Export Functions** - Press `C-c RET` to export markdown to:
- PDF (with pretty styling)
- ODT, DOCX, HTML  
- Org-mode

**Org Conversion** - Export org files to markdown using pandoc or a custom converter.

**Editing Utilities** - Additional markdown keybindings:
- `C-c .` - Insert formatted date
- `C-c C-b` - Add line breaks (double spaces)
- `C-c C-r` - Format rubric text
- `C-c C-s` - Save with title-based filename

## Requirements

- Emacs 25.1+
- [Pandoc](https://pandoc.org/)
- [WeasyPrint](https://weasyprint.org/) (recommended) or pdflatex/xelatex for PDF export

## Installation

```elisp
;; Load export functionality (includes all formats)
(require 'markdown-export)

;; Optional: Load editing utilities
(require 'markdown-tweaks)
```

## Usage

In markdown files, press `C-c RET` and choose format:
- `p` for PDF
- `o` for ODT  
- `d` for DOCX
- `h` for HTML
- `g` for Org

In org files, `C-c RET` exports to markdown.

## License

GPL-3.0
