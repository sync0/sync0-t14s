(use-package consult-bibtex
  :after bibtex-completion
  :straight (consult-bibtex :type git :host github :repo "mohkale/consult-bibtex")
  :custom
  (consult-bibtex-default-action 'consult-bibtex-show-entry))

(use-package bibtex-completion
  :custom 
  (bibtex-completion-bibliography sync0-bibtex-bibliographies)
  (bibtex-completion-notes-path sync0-zettelkasten-references-directory)
  (bibtex-completion-library-path (list sync0-zettelkasten-attachments-directory))
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-pdf-symbol "P")
  (bibtex-completion-notes-symbol "N")
  (bibtex-completion-notes-extension ".md")
  ;; (bibtex-completion-notes-extension ".org")
  (bibtex-completion-pdf-extension '(".pdf" ".epub" ".doc" ".docx" ".org" ".ppt" ".pptx" ".md" ".rtf" ".tex" ".mp3" ".mp4" ".png" ".jpg" ".txt" ".m4a"))
  ;; (bibtex-completion-pdf-extension sync0-bibtex-completion-extension)
  (bibtex-completion-additional-search-fields '(date volume edition))
  :config 

  ;; (setq bu-keywords-values sync0-bibtex-completion-keywords)

  (setq bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title}: ${subtitle} @ ${journaltitle} [${=type=}:${=key=}]")
	  (mvbook          . "${=has-pdf=:1}${=has-note=:1}| ${author} [${origdate:4}](${date}) ${title} ${volume}: ${subtitle} Ed. ${edition} [${=type=}:${=key=}]")
	  (book          . "${=has-pdf=:1}${=has-note=:1}| ${author} [${origdate:4}](${date}) ${title} ${volume}: ${subtitle} Ed. ${edition} [${=type=}:${=key=}]")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title:55} [${=type=}:${=key=}]")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title:55} [${=type=}:${=key=}]")
	  (collection    . "${=has-pdf=:1}${=has-note=:1}| ${editor} (${date}) ${title:55} ${volume}: ${subtitle} [${=type=}:${=key=}]")
	  (mvcollection    . "${=has-pdf=:1}${=has-note=:1}| ${editor} (${date}) ${title:55} ${volume}: ${subtitle} [${=type=}:${=key=}]")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title:55} [${=type=}:${=key=}]")
	  (t             . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title}: ${subtitle} [${=type=}:${=key=}]")))

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))

  (setq bibtex-completion-notes-template-multiple-files  
        (concat 
	 "---\n"
         "citekey: ${=key=}\n"
         "biblatex_type: ${=type=}\n"
         "zettel_type: reference\n"
         "aliases: [\"%(sync0-bibtex-extract-lastname (sync0-bibtex-completion-reverse-author ${=key=})) (${date}) ${title} : ${subtitle}\"]\n"
         "created: %(format-time-string \"%Y-%m-%d\")\n"
         "parent: \"${booktitle}\"\n" 
         "parent: \"${journaltitle}\"\n"
         "author: \"${author-or-editor}\"\n"
         "language: ${language}\n"
         "date: ${date}\n"
         "tags: [reference/${=type=},bibkey/${=key=},author,language/${language},date/${date}]\n"
         "---\n"
         "# %(sync0-bibtex-extract-lastname (sync0-bibtex-completion-reverse-author ${=key=})) (${date}) ${title} : ${subtitle}\n\n"
         "## Description\n\n"
         "## Progrès de la lecture\n\n"
         "## Annotations\n\n"
         "## Références\n\n"))

  (require 'sync0-ivy-bibtex)
  (require 'sync0-ivy-bibtex-functions)
  (require 'sync0-bibtex-markdown))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map)))

(provide 'sync0-bibtex-completion)
