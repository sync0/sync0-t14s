(require 'sync0-pandoc)
(require 'sync0-print)
(require 'sync0-yaml)

;; (setq markdown-list-item-bullets '("●" "◎" "○" "◆" "◇" "►" "•"))
(setq markdown-list-item-bullets '("•"))


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
       ;; " --resource-path=.:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/bibliographies"
       " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/bibliographies:/home/sync0/Pictures/archives"
       ;; " --resource-path=.:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/bibliographies"
       " --shift-heading-level-by=1" 
       ;; " --shift-heading-level-by=-1" 
       " --css=markdown.css"
       ;; " --csl=history-of-political-economy.csl"
       " --csl=histoire-at-politique.csl"
       ;; " --csl=histoire-et-mesure.csl"
       ;; " --quiet"
       ;; " --number-sections"
       ;; " --lua-filter=lua.lua"
       ;; " --lua-filter=/home/sync0/.local/share/pandoc/filters/noexport-subtrees.lua"
       " --metadata=reference-section-title:Références"
       " --citeproc"
       " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
       " --bibliography=bibliography.bib"
       ))

(defun sync0-markdown-open-pdf-in-zathura ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((full-path (buffer-file-name))
        (current-file (when (string-match "^.+/\\([[:alnum:]]+\\)\\.md$" full-path)
                        (match-string-no-properties 1 full-path)))
        (current-path (when (string-match "\\(^.+/\\)[[:alnum:]]+\\.md$" full-path)
                        (match-string-no-properties 1 full-path)))
        (pdf-path (concat current-path current-file ".pdf")))
    (if (file-exists-p pdf-path)
        (call-process "zathura" nil 0 nil pdf-path)
      (message "No PDF found for %s.md" current-file))))

(defun sync0-markdown-print-pdf ()
  "Print the pdf provided in the argument. Generalized for
interactive use and use in pipes to output to the printer."
  (interactive)
  (let ((pdf (concat (substring (buffer-file-name) 0 -2) "pdf"))
         (command (sync0-print-define-command))) 
    (if (file-exists-p pdf)
        (shell-command (concat command pdf))
      (message "No pdf found for current markdown note"))))

(defun sync0-markdown-copy-pdf-to-path ()
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (let* ((pdf (concat (substring (buffer-file-name) 0 -2) "pdf"))
         (date (format-time-string "%Y-%m-%d"))
         (regex-title "^title: \"\\(.+\\)\"")
         (regex-subtitle "^subtitle: \"\\(.+\\)\"")
         (title (save-excursion
                  (goto-char (point-min))
                  (when  (re-search-forward regex-title nil t 1)
                    (match-string 1))))
         (subtitle (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward regex-subtitle nil t 1)
                       (match-string 1))))
         (target-path (read-string "Où envoyer ce pdf ? (finir en /) "))
         (command (concat "cp "
                          pdf
                          " \""
                          target-path
                          "Rivera Carreño_"
                          date
                          "_"
                          title
                          "_"
                          subtitle
                          ".pdf\"")))
    (if (file-exists-p pdf)
        (progn 
          (shell-command command)
          ;; (message "%s" command)
          (message "PDF moved to target location"))
      (message "No PDF found"))))

(defun sync0-markdown-save-exported-pdf-in-cabinet ()
  "Create a copy of pdf corresponding to current file in our
cabinet (defined by sync0-zettelkasten-exported-pdfs-directory)."
  (interactive)
  (let* ((current-path (file-name-directory buffer-file-name))
         (current-file (sync0-yaml-get-property "id"))
         (current-pdf (concat current-path current-file ".pdf"))
         (target-pdf (concat sync0-zettelkasten-exported-pdfs-directory current-file ".pdf"))
         (command (concat "cp " current-pdf " " target-pdf)))
    (cond ((and  (file-exists-p current-pdf)
                 (file-exists-p target-pdf)
                 (yes-or-no-p "Overwrite copy in cabinet with Obsidian vault version?"))
           (shell-command command))
          ((and  (file-exists-p current-pdf)
                 (not (file-exists-p target-pdf))
                 (yes-or-no-p "Create copy in cabinet from Obsidian vault version?"))
           (shell-command command))
          (t (message "Command failed. Probably no pdf corresponds to current file.")))))

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
   (("a" sync0-define-local-abbrev "Define abbrev")
   ("P" sync0-markdown-print-pdf "Print corresp. pdf")
   ("C" sync0-markdown-save-exported-pdf-in-cabinet "Copy pdf to cabinet")
   ("M" sync0-markdown-copy-pdf-to-path "Move to path"))))
;; ("d" org-insert-drawer)

;; (evil-leader/set-key-for-mode 'markdown-mode "z" 'sync0-hydra-markdown-functions/body)

(provide 'sync0-markdown)
