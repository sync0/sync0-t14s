(require 'sync0-markdown)

(defun sync0-latex-open-pdf-in-zathura ()
  "Open the corresponding PDF for the current LaTeX file if it exists."
  (interactive)
  (let* ((base-name (file-name-sans-extension (buffer-file-name))) ;; Get file path without extension
         (pdf-path (concat base-name ".pdf"))) ;; Append .pdf to the base file name
    (if (file-exists-p pdf-path)
        (call-process "zathura" nil 0 nil pdf-path) ;; Open the PDF with Zathura
      (message "No PDF found for %s" (buffer-file-name))))) ;; Message if no PDF is found

;; (major-mode-hydra-define tex-mode nil 
;;   ;; ("Links"
;;   ;;  ;; ("s" org-store-link)
;;   ;;  (("i" markdown-insert-wiki-link "Insert wiki-link")
;;   ;;   ("k" markdown-insert-link "Insert markdown link")
;;   ;;   ("I" markdown-insert-image "Insert image"))
;;   ("Scholarly"
;;    ;; (("f" markdown-insert-footnote "Insert footnote")
;;     (("c" ivy-bibtex "Show bibtex entry"))
;;    "Visualization"
;;    ;; (("m" markdown-toggle-markup-hiding "Toggle markup")
;;    ;;  ("b" markdown-narrow-to-subtree "Narrow to header")
;;     (("z" sync0-latex-open-pdf-in-zathura "Show pdf")
;;     ("r" sync0-org-ref-open-pdf-at-point-zathura "Open reference pdf"))
;;    ;; "Compilation"
;;    ;; (("p" sync0-pandoc-export-md-to-pdf "Pdf")
;;    ;;  ("l" sync0-pandoc-export-md-to-tex "TeX")
;;    ;;  ("d" sync0-pandoc-export-md-to-docx "Docx"))
;;    ;; ("b" org-epub-export-to-epub)
;;    ;; ("t" sync0-pandoc-export-md-to-tex)
;;    ;; ("E" sync0-org-export-headlines-to-latex)
;;    "Etc"
;;    (("a" sync0-define-local-abbrev "Define abbrev"))))

(defun sync0-tex-copy-resulting-pdf (&optional in-path)
  "Copy the PDF generated from the current TeX file to a specified path."
  (interactive)
  (let* ((tex-file (buffer-file-name))
         (pdf-file (concat (file-name-sans-extension tex-file) ".pdf"))
         (default-path sync0-goodreads-directory)
         (path (sync0-validate-path (or in-path
                                        (read-directory-name "Où envoyer ce fichier ? " default-path))))
         (filename (file-name-nondirectory pdf-file))
         (destination (concat path filename)))
    (if (file-exists-p pdf-file)
        (progn
          (copy-file pdf-file destination t)
          (message "PDF copied to %s" destination))
      (message "No PDF found for %s" tex-file))))

(major-mode-hydra-define TeX-mode
  (:quit-key "q" :foreign-keys warn :title "TeX Mode Hydra")
  ("Scholarly"
   (
    ;; ("c" ivy-bibtex "Show bibtex entry")
    ("c" sync0-tex-copy-resulting-pdf "Copy resulting pdf")
    ("z" sync0-latex-open-pdf-in-zathura "Show pdf")
    ("r" sync0-org-ref-open-pdf-at-point-zathura "Open reference pdf"))
   "Etc"
   (("a" sync0-define-local-abbrev "Define abbrev"))))

(provide 'sync0-tex)
