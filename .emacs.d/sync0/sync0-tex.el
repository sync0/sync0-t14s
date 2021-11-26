(require 'sync0-markdown)

(defun sync0-latex-open-pdf-in-zathura ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((full-path (buffer-file-name))
        (current-file (when (string-match "^.+/\\([[:alnum:]_-]+\\)\\.tex$" full-path)
                        (match-string-no-properties 1 full-path)))
        (current-path (when (string-match "\\(^.+/\\)[[:alnum:]_-]+\\.tex$" full-path)
                        (match-string-no-properties 1 full-path)))
        (pdf-path (concat current-path current-file ".pdf")))
    (if (file-exists-p pdf-path)
        (call-process "zathura" nil 0 nil pdf-path)
      (message "No PDF found for %s.md" current-file))))

(major-mode-hydra-define latex-mode nil 
  ;; ("Links"
  ;;  ;; ("s" org-store-link)
  ;;  (("i" markdown-insert-wiki-link "Insert wiki-link")
  ;;   ("k" markdown-insert-link "Insert markdown link")
  ;;   ("I" markdown-insert-image "Insert image"))
  ("Scholarly"
   ;; (("f" markdown-insert-footnote "Insert footnote")
    (("c" ivy-bibtex "Show bibtex entry"))
   "Visualization"
   ;; (("m" markdown-toggle-markup-hiding "Toggle markup")
   ;;  ("b" markdown-narrow-to-subtree "Narrow to header")
    (("z" sync0-latex-open-pdf-in-zathura "Show pdf")
    ("r" sync0-org-ref-open-pdf-at-point-zathura "Open reference pdf"))
   ;; "Compilation"
   ;; (("p" sync0-pandoc-export-md-to-pdf "Pdf")
   ;;  ("l" sync0-pandoc-export-md-to-tex "TeX")
   ;;  ("d" sync0-pandoc-export-md-to-docx "Docx"))
   ;; ("b" org-epub-export-to-epub)
   ;; ("t" sync0-pandoc-export-md-to-tex)
   ;; ("E" sync0-org-export-headlines-to-latex)
   "Etc"
   (("a" sync0-define-local-abbrev "Define abbrev"))))
