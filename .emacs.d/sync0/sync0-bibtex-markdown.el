(require 'sync0-citar)

(defun sync0-markdown-open-attachment-at-point ()
  "Open the PDF associated with the nearest citation in Markdown.
Searches backward for a citation in visual lines, or prompts for a BibTeX key if none is found."
  (interactive)
  (save-excursion
    (let ((citation-id nil)
          (search-regex "\\[@\\([a-zA-Z0-9-]+\\)[^]]*\\]")) ;; Regex to match citation
      ;; Check if the point is already at a citation
      (if (looking-at search-regex)
          (setq citation-id (match-string 1))
        ;; Otherwise, search backward within the current visual line
        (progn
          (beginning-of-visual-line)
          (if (re-search-backward search-regex (line-beginning-position) t)
              (setq citation-id (match-string 1))
            ;; If no citation is found, prompt for a key
            (setq citation-id (sync0-bibtex-choose-key "Attachment to search: ")))))
      ;; Open the PDF if a citation ID is found
      (if citation-id
          (sync0-bibtex-open-pdf citation-id)
        (message "No citation found or provided.")))))

(provide 'sync0-bibtex-markdown)
