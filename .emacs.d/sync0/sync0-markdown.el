(setq sync0-markdown-zettel-template 
      (concat
"---
zettel_type: 
aliases: []
created: " (format-time-string "%Y-%m-%d") 
"\ntags: [inbox]
---
#"))

(defun sync0-markdown-new-zettel ()
  "Create new Zettel in Obsidian vault."
  (interactive)
  (let* ((filename (format-time-string "%Y%m%d%H%M%S"))
         (obsidian-file
          (concat sync0-obsidian-directory filename ".md")))
    (with-temp-buffer 
      (insert sync0-markdown-zettel-template)
      (write-file obsidian-file))
    (find-file obsidian-file)))

  ;; (setq markdown-command "pandoc")

(setq markdown-command
      (concat
       ;; "/usr/local/bin/pandoc"
       "pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments"
       ;; " --resource-path=.:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/bibliographies"
       " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/bibliographies"
       ;; " --resource-path=.:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/bibliographies"
       " --shift-heading-level-by=-1" 
       " --css=markdown.css"
       ;; " --csl=history-of-political-economy.csl"
       " --csl=histoire-at-politique.csl"
       ;; " --csl=histoire-et-mesure.csl"
       ;; " --quiet"
       ;; " --number-sections"
       ;; " --lua-filter=lua.lua"
       " --metadata=reference-section-title:Références"
       " --citeproc"
       " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
       " --bibliography=bibliography.bib"
       ))

(defvar sync0-pandoc-export-md-to-tex-settings-alist
      '(("arvore" (lambda ()
                    (concat
                     " --metadata-file=/home/sync0/Dropbox/typography/pandoc/defaults_arvore.yaml")))
        ("scrartcl" (lambda ()
                     (concat
                      " --metadata-file=/home/sync0/Dropbox/typography/pandoc/defaults_scrartcl.yaml")))
        ("scrartcl_a5" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Dropbox/typography/pandoc/defaults_scrartcl_a5.yaml")))
        ("scrbook" (lambda ()
                     (concat
                      " --metadata-file=/home/sync0/Dropbox/typography/pandoc/defaults_scrbook.yaml")))
        ("scrreprt" (lambda ()
                     (concat
                      " --metadata-file=/home/sync0/Dropbox/typography/pandoc/defaults_scrreprt.yaml"))))
      "List of concatenated strings defining the styles for markdown to pdf export")

(defvar sync0-pandoc-export-md-to-pdf-settings-alist
      '(("arvore" (lambda ()
                    (concat
                      " --defaults=/home/sync0/Dropbox/typography/pandoc/defaults_arvore.yaml")))
        ("scrartcl" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Dropbox/typography/pandoc/defaults_scrartcl.yaml")))
        ("scrartcl_a5" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Dropbox/typography/pandoc/defaults_scrartcl_a5.yaml")))
        ("scrbook" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Dropbox/typography/pandoc/defaults_scrbook.yaml")))
        ("scrreprt" (lambda ()
                     (concat
                      " --defaults=/home/sync0/Dropbox/typography/pandoc/defaults_scrreprt.yaml"))))
      "List of concatenated strings defining the styles for markdown to pdf export")

  (defvar sync0-pandoc-md-to-pdf-command-base
    (concat
     "pandoc"
     " --from=markdown --to=pdf"
     " --standalone"
     " --pdf-engine=lualatex"
     " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/typography/pandoc:/home/sync0/Dropbox/bibliographies:/home/sync0/Dropbox/typography/pandoc:/home/sync0/Dropbox/obsidian/img"
     " --shift-heading-level-by=-1" 
     " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
     " --lua-filter=diagram-generator.lua "
     " -H preamble.tex")
     ;; " --filter pandoc-xnos"
    "String of basic export settings for pandoc to convert from markdown to pdf.")

  (defvar sync0-pandoc-md-to-tex-command-base
    (concat
     "pandoc"
     " --from=markdown --to=latex"
     " --standalone"
     " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/typography/pandoc:/home/sync0/Dropbox/bibliographies:/home/sync0/Dropbox/typography/pandoc:/home/sync0/Dropbox/obsidian/img"
     " --shift-heading-level-by=-1" 
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
           " --bibliography=/home/sync0/Dropbox/bibliographies/bibliography.bib"
           " --csl=chicago-fullnote-bibliography.csl")
    "String of citation export settings for pandoc to convert from markdown to pdf.")
  
(defun sync0-pandoc-export-md-to-pdf ()
  (interactive)
  (let* ((type (completing-read "Choose document type for export: " sync0-pandoc-export-md-to-pdf-settings-alist))
         (lang (if (progn
                     (goto-char (point-min))
                     (re-search-forward "^lang: \\([A-z-]+\\)\n" nil t 1))
                   (match-string-no-properties 1)
                 (completing-read "Choose export language: " 
                                  '("en-US" "en-GB" "pt-BR" "pt-PT" "de-DE" "fr-FR" "es-CO"))))
         (type-settings (funcall
                         (cadr (assoc type sync0-pandoc-export-md-to-pdf-settings-alist))))
         (raw-command (concat "pandoc" type-settings))
         (current-path (buffer-file-name))
         (current-file (when (string-match "^.+/\\([0-9]+\\)\\.md$" current-path)
                         (match-string-no-properties 1 current-path)))
         (command (concat raw-command " " current-file ".md -o" current-file ".pdf")))
    (shell-command command)))

(defun sync0-pandoc-export-md-to-tex ()
  (interactive)
  (let* ((type (completing-read "Choose document type for export: " sync0-pandoc-export-md-to-tex-settings-alist))
         ;; (citationp (yes-or-no-p "Use citations module? "))
         (lang (if (progn
                     (goto-char (point-min))
                     (re-search-forward "^lang: \\([A-z-]+\\)\n" nil t 1))
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
         (current-file (when (string-match "^.+/\\([0-9]+\\)\\.md$" current-path)
                         (match-string-no-properties 1 current-path)))
         ;; (file (read-string "Which file to convert to PDF? " current-file))
         (command (concat raw-command " " current-file ".md -o" current-file ".tex")))
    (shell-command command)))

(defun sync0-markdown-open-pdf-in-zathura ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((full-path (buffer-file-name))
        (current-file (when (string-match "^.+/\\([0-9]+\\)\\.md$" full-path)
                        (match-string-no-properties 1 full-path)))
        (current-path (when (string-match "\\(^.+/\\)[0-9]+\\.md$" full-path)
                        (match-string-no-properties 1 full-path)))
        (pdf-path (concat current-path current-file ".pdf")))
    (if (file-exists-p pdf-path)
        (call-process "zathura" nil 0 nil pdf-path)
      (message "No PDF found for %s.md" current-file))))

(setq sync0-pandoc-md-to-docx-command
      (concat
       "pandoc"
       " --from=markdown --to=docx"
       " --standalone"
       " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/pandoc/yaml:/home/sync0/Dropbox/pandoc/templates:/home/sync0/Dropbox/bibliographies"
       " --shift-heading-level-by=-1" 
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

(major-mode-hydra-define markdown-mode nil 
  ("Links"
   ;; ("s" org-store-link)
   (("i" markdown-insert-wiki-link "Insert wiki-link")
    ("k" markdown-insert-link "Insert markdown link")
    ("I" markdown-insert-image "Insert image"))
   "Scholarly"
   (("f" markdown-insert-footnote "Insert footnote")
    ("c" ivy-bibtex "Show bibtex entry"))
   "Visualization"
   (("m" markdown-toggle-markup-hiding "Toggle markup")
    ("b" markdown-narrow-to-subtree "Narrow to header")
    ("z" sync0-markdown-open-pdf-in-zathura "Show pdf")
    ("r" sync0-org-ref-open-pdf-at-point-zathura "Open reference pdf"))
   "Export"
   (("p" sync0-pandoc-export-md-to-pdf "Pdf")
    ("l" sync0-pandoc-export-md-to-tex "TeX")
    ("d" sync0-pandoc-export-md-to-docx "Docx"))
   ;; ("b" org-epub-export-to-epub)
   ;; ("t" sync0-pandoc-export-md-to-tex)
   ;; ("E" sync0-org-export-headlines-to-latex)
   "Etc"
   (("a" sync0-define-local-abbrev "Define abbrev"))))
;; ("d" org-insert-drawer)

;; (evil-leader/set-key-for-mode 'markdown-mode "z" 'sync0-hydra-markdown-functions/body)


(provide 'sync0-markdown)
