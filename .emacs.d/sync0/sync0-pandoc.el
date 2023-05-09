
(defvar sync0-pandoc-export-md-to-tex-settings-alist
      '(("arvore" (lambda ()
                    (concat
                     " --metadata-file=/home/sync0/Gdrive/typography/pandoc/defaults_arvore.yaml")))
        ("scrartcl" (lambda ()
                     (concat
                      " --metadata-file=/home/sync0/Gdrive/typography/pandoc/defaults_scrartcl.yaml")))
        ("scrartcl_a5" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_scrartcl_a5.yaml")))
        ("scrbook" (lambda ()
                     (concat
                      " --metadata-file=/home/sync0/Gdrive/typography/pandoc/defaults_scrbook.yaml")))
        ;; ("zettel (A6)" (lambda ()
        ;;              (concat
        ;;               " --metadata-file=/home/sync0/Gdrive/typography/pandoc/defaults_scrbook.yaml")))
        ("scrreprt" (lambda ()
                     (concat
                      " --metadata-file=/home/sync0/Gdrive/typography/pandoc/defaults_scrreprt.yaml"))))
      "List of concatenated strings defining the styles for markdown to pdf export")

(defvar sync0-pandoc-export-md-to-pdf-settings-alist
      '(("arvore" (lambda ()
                    (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_arvore.yaml")))
        ("scrartcl" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_scrartcl.yaml")))
        ("scrartcl_numbered" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_scrartcl_numbered.yaml")))
        ("scrartcl_a5" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_scrartcl_a5.yaml")))
        ("scrlttr2" (lambda ()
                     (concat
                      " --template scrlttr2")))
        ("scrbook" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_scrbook.yaml")))
        ("scrreprt" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_scrreprt.yaml")))
        ("zettel" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_zettel.yaml")))
        ("zkn_literature_notes" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_zkn_lit.yaml"))))
      "List of concatenated strings defining the styles for markdown to pdf export")

(defvar sync0-pandoc-export-epub-to-pdf-settings-alist
      '(("scrbook" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Gdrive/typography/pandoc/defaults_epub-scrbook.yaml"))))
      "List of concatenated strings defining the styles for markdown to pdf export")

  (defvar sync0-pandoc-md-to-pdf-command-base
    (concat
     "pandoc"
     " --from=markdown --to=pdf"
     " --standalone"
     " --pdf-engine=lualatex"
     " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/typography/pandoc:/home/sync0/Gdrive/bibliographies:/home/sync0/Gdrive/typography/pandoc:/home/sync0/Gdrive/obsidian/img:/home/sync0/Pictures/archives:/home/sync0/Documents/pdfs"
     " --shift-heading-level-by=1" 
     ;; " --shift-heading-level-by=-1" 
     " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
     ;; " --lua-filter=diagram-generator.lua "
     " -H preamble.tex")
     ;; " --filter pandoc-xnos"
    "String of basic export settings for pandoc to convert from markdown to pdf.")

  (defvar sync0-pandoc-epub-to-pdf-command-base
    (concat
     "pandoc"
     " --from=epub --to=pdf"
     " --standalone"
     " --pdf-engine=lualatex"
     " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/typography/pandoc:/home/sync0/Gdrive/bibliographies:/home/sync0/Gdrive/typography/pandoc:/home/sync0/Gdrive/obsidian/img:/home/sync0/Pictures/archives:/home/sync0/Documents/pdfs"
     ;; " --shift-heading-level-by=1" 
     ;; " --shift-heading-level-by=-1" 
     ;; " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
     ;; " --lua-filter=diagram-generator.lua "
     " -H preamble.tex")
     ;; " --filter pandoc-xnos"
    "String of basic export settings for pandoc to convert from markdown to pdf.")

  (defvar sync0-pandoc-md-to-tex-command-base
    (concat
     "pandoc"
     " --from=markdown --to=latex"
     " --standalone"
     " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/typography/pandoc:/home/sync0/Gdrive/bibliographies:/home/sync0/Gdrive/typography/pandoc:/home/sync0/Gdrive/obsidian/img:/home/sync0/Pictures/archives"
     " --shift-heading-level-by=1" 
     ;; " --shift-heading-level-by=-1" 
     " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
     " --lua-filter=diagram-generator.lua "
     " -H preamble.tex")
    "String of basic export settings for pandoc to convert from markdown to pdf.")


  (defvar sync0-pandoc-md-to-pdf-command-base-language
    '(("pt-BR" . " -H language_portuguese.tex")
      ("pt-PT" . " -H language_portuguese.tex")
      ("fr-FR" . " -H language_french.tex")
      ("fr-CA" . " -H language_french.tex")
      ("es-CO" . " -H language_spanish.tex")
      ("es-ES" . " -H language_spanish.tex")
      ("en-US" . " -H language_english.tex")
      ("en-GB" . " -H language_english.tex")))

  (defvar sync0-pandoc-md-to-pdf-command-citations-language
    '(("pt-BR" . " --metadata=reference-section-title:Bibliografia")
      ("pt-PT" . " --metadata=reference-section-title:Bibliografia")
      ("fr-FR" . " --metadata=reference-section-title:Références")
      ("fr-CA" . " --metadata=reference-section-title:Références")
      ("es-CO" . " --metadata=reference-section-title:Bibliografía")
      ("es-ES" . " --metadata=reference-section-title:Bibliografía")
      ("en-US" . " --metadata=reference-section-title:References")
      ("en-GB" . " --metadata=reference-section-title:References")))

(defvar sync0-pandoc-md-to-pdf-command-citations
          (concat
           " --citeproc"
           " --bibliography=/home/sync0/Gdrive/bibliographies/bibliography.bib"
           " --csl=chicago-fullnote-bibliography.csl")
    "String of citation export settings for pandoc to convert from markdown to pdf.")

(defun sync0-pandoc-export-md-to-pdf ()
  (interactive)
  (let* ((type (if (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^export_template: \\([[:graph:]]+\\)$" nil t 1))
                   (match-string-no-properties 1)
                 (completing-read "Choose document type for export: " sync0-pandoc-export-md-to-pdf-settings-alist)))
         (lang (if (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^lang: \\([[:graph:]]+\\)$" nil t 1))
                   (match-string-no-properties 1)
                 (completing-read "Choose export language: " 
                                  '("en-US" "en-GB" "pt-BR" "pt-PT" "de-DE" "fr-FR" "es-CO"))))
         (type-settings (funcall
                         (cadr (assoc type sync0-pandoc-export-md-to-pdf-settings-alist))))
         (raw-command (concat "pandoc" type-settings))
         (current-path (buffer-file-name))
         (current-file (when (string-match "^.+/\\([[:alnum:]]+\\)\\.md$" current-path)
                         (match-string-no-properties 1 current-path)))
         (command (concat raw-command " " current-file ".md -o" current-file ".pdf")))
    (shell-command command)))

(defun sync0-pandoc-export-md-to-tex ()
  (interactive)
  (let* ((type (completing-read "Choose document type for export: " sync0-pandoc-export-md-to-tex-settings-alist))
         ;; (citationp (yes-or-no-p "Use citations module? "))
         (lang (if (progn
                     (goto-char (point-min))
                     (re-search-forward "^lang: \\([[:graph:]]+\\)$" nil t 1))
                   (match-string-no-properties 1)
                 (completing-read "Choose export language: " 
                                  '("en-US" "en-GB" "pt-BR" "pt-PT" "de-DE" "fr-FR" "es-CO"))))
         ;; (citations (concat sync0-pandoc-md-to-pdf-command-citations
         ;;               (cdr (assoc lang sync0-pandoc-md-to-pdf-command-citations-language))))
         ;; (base (concat sync0-pandoc-md-to-tex-command-base
         ;;               (cdr (assoc lang sync0-pandoc-md-to-pdf-command-base-language))))
         (type-settings (funcall
                         (cadr (assoc type sync0-pandoc-export-md-to-tex-settings-alist))))
         ;; (raw-command (if citationp 
         ;;                  (concat base type-settings citations)
         ;;                (concat base type-settings)))
         (raw-command (concat "pandoc" type-settings))
         (current-path (buffer-file-name))
         (current-file (when (string-match "^.+/\\([[:alnum:]]+\\)\\.md$" current-path)
                         (match-string-no-properties 1 current-path)))
         ;; (file (read-string "Which file to convert to PDF? " current-file))
         (command (concat raw-command " " current-file ".md -o" current-file ".tex")))
    (shell-command command)))

(setq sync0-pandoc-md-to-docx-command
      (concat
       "pandoc"
       " --from=markdown --to=docx"
       " --standalone"
       " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/pandoc/yaml:/home/sync0/Gdrive/pandoc/templates:/home/sync0/Gdrive/bibliographies"
       " --shift-heading-level-by=1" 
       ;; " --shift-heading-level-by=-1" 
       " --metadata=reference-section-title:Références"
       " --citeproc"
       " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
       ))

(defun sync0-pandoc-export-md-to-docx ()
  (interactive)
  (let* ((current-path (buffer-file-name))
         (current-file (when (string-match "^.+/\\([0-9]+\\)\\.md$" current-path)
                         (match-string-no-properties 1 current-path)))
         (file (read-string "Which file to convert to docx? " current-file))
         (command (concat sync0-pandoc-md-to-docx-command " " file ".md -o" file ".docx")))
    (shell-command command)))

;; (defun sync0-pandoc-export-md-to-tex ()
;;   (interactive)
;;   (let* ((current-path (buffer-file-name))
;;          (metadata-file
;; " --citeproc"
;; " --bibliography=bibliography.bib"
;;          (current-file (when (string-match "^.+/\\([0-9]+\\)\\.md$" current-path)
;;                          (match-string-no-properties 1 current-path)))
;;          (file (read-string "Which file to convert to PDF? " current-file))
;;          (command (concat sync0-pandoc-md-to-pdf-command " " file ".md -o" file ".pdf")))
;;       (shell-command command)))

;; (setq sync0-pandoc-epub-to-pdf-command
;;       (concat
;;        "pandoc"
;;        " --from=markdown --to=docx"
;;        " --standalone"
;;        " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/pandoc/yaml:/home/sync0/Gdrive/pandoc/templates:/home/sync0/Gdrive/bibliographies"
;;        " --shift-heading-level-by=1" 
;;        ;; " --shift-heading-level-by=-1" 
;;        " --metadata=reference-section-title:Références"
;;        " --citeproc"
;;        " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
;;        ))

;; Epub to pdf export settings

(provide 'sync0-pandoc)
