;; -*- lexical-binding: t; -*-
;; Author: Rari Comninos

;; -------------------------------
;; Markdown Keybindings
;; -------------------------------

(defun insert-markdown-date ()
  "Insert today's date in bold markdown format: **2025-11-24 Sun**
Author: Raoul Comninos"
  (interactive)
  (insert (format-time-string "**%Y-%m-%d %a**")))

(defun my-markdown-setup-keys ()
  "Set up keybindings for markdown-mode."
  (define-key markdown-mode-map (kbd "C-x C-a") 'markdown-save-with-title)
  (define-key markdown-mode-map (kbd "C-c C-s") 'markdown-save-with-title)
  (define-key markdown-mode-map (kbd "C-c C-b") 'markdown-add-line-breaks)
  (define-key markdown-mode-map (kbd "C-c .") 'insert-markdown-date)
  (define-key markdown-mode-map (kbd "C-c C-r") 'my-format-markdown-rubric))

(add-hook 'markdown-mode-hook 'my-markdown-setup-keys)

(with-eval-after-load 'markdown-mode
  (my-markdown-setup-keys))

;; -------------------------------
;; Markdown Utility Functions
;; -------------------------------

(defun markdown-add-line-breaks (start end)
  "Add two spaces at the end of each line in the selected region.
This creates hard line breaks in Markdown.
Author: Raoul Comninos"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        ;; Remove existing trailing spaces first
        (while (and (> (point) (line-beginning-position))
                    (= (char-before) ?\s))
          (delete-char -1))
        ;; Add two spaces unless the line is empty
        (unless (= (point) (line-beginning-position))
          (insert "  "))
        (forward-line 1)))))

(defun markdown-title-to-filename ()
  "Create a filename from the Markdown # title.
Author: Rari Comninos"
  (let* ((title (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward "^#\\s-+\\(.*\\)" nil t)
                      (match-string 1)
                    nil))))
    (setq title (or title (read-string "Enter title for the new file: ")))
    (setq title (string-trim title))
    (setq title (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" title))
    (setq title (replace-regexp-in-string "[[:space:]]+" "-" title))
    (setq title (replace-regexp-in-string "-+$" "" title))
    (downcase title)))

(defun markdown-save-with-title-to-dir (directory)
  "Save the current markdown buffer with title-based filename to specified directory.
Author: Rari Comninos"
  (let* ((filename (markdown-title-to-filename))
         (filepath (concat (file-name-as-directory directory) filename ".md")))
    (write-file filepath)))

(defhydra hydra-markdown-save-directory (:color blue :hint nil)
  "
Save markdown file to directory:
_d_: Documents        _c_: code
_s_: sermons          _n_: notes
_q_: quit
"
  ("c" (markdown-save-with-title-to-dir "/home/yal/code"))
  ("d" (markdown-save-with-title-to-dir "/home/yal/Documents"))
  ("n" (markdown-save-with-title-to-dir "/home/yal/src/org/notes-videos"))
  ("s" (markdown-save-with-title-to-dir "/home/yal/Documents/sermons"))
  ("q" nil))

(defun markdown-save-with-title ()
  "Save markdown file with title-based filename using hydra directory selection.
Author: Rari Comninos"
  (interactive)
  (hydra-markdown-save-directory/body))

(defun my-format-markdown-rubric (start end)
  "Format selected text as a rubric in markdown.
Adds section sign (¶) at the start if not present, makes text italic,
and adds two spaces at the end of each line (except the last) for proper line breaks.
If no region is selected, inserts '*¶ *' with cursor before final asterisk.
Author: Raoul Comninos"
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties start end)))
        ;; Add section sign if not present
        (unless (string-prefix-p "¶" (string-trim text))
          (setq text (concat "¶ " text)))

        ;; Add two spaces at end of each line for markdown line breaks (except last line)
        (setq text (replace-regexp-in-string
                    "\\([^[:space:]]\\)\n"
                    "\\1  \n"
                    text))

        ;; Wrap in italic markdown
        (setq text (concat "*" text "*"))

        ;; Replace the region with formatted text
        (delete-region start end)
        (insert text))
    ;; No region selected - insert template with cursor before final asterisk
    (insert "*¶ ")
    (save-excursion
      (insert "*"))))

;; Easily add double spaces to end of lines

(defun my-format-markdown-region ()
  "Format selected text for markdown: add two spaces at end of each line for line breaks."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (end-of-line)
            (unless (looking-at-p "$")
              (just-one-space)
              (delete-char -1))
            (insert "  ")
            (forward-line 1)
            (setq end (+ end 2)))))
    (message "No region selected")))

(global-set-key (kbd "C-c m") 'my-format-markdown-region)

(provide 'markdown-tweaks)
