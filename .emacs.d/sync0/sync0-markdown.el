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
       ;; " --filter=delink.hs"
       ;; " --css='/home/sync0/Dropbox/typography/css/markdown.css'"
       ;; " --resource-path=.:\"/home/sync0/Dropbox/typography/css\":\"/home/sync0/Dropbox/bibliographies\""
       ;; " --resource-path='/home/sync0/Dropbox/typography/css':'/home/sync0/Dropbox/bibliographies'"
       ;; " --resource-path='/home/sync0/Dropbox/typography/css':'/home/sync0/Dropbox/bibliographies'"
       ;; " --resource-path=.:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/bibliographies"
       ;; " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Dropbox/typography/css:/home/sync0/Dropbox/typography/csl:/home/sync0/Dropbox/bibliographies"
       " --css=markdown.css"
       ;; " --csl=history-of-political-economy.csl"
       " --csl=histoire-at-politique.csl"
       ;; " --csl=histoire-et-mesure.csl"
       ;; " --quiet"
       ;; " --number-sections"
       ;; " --lua-filter=lua.lua"
       ;; " --filter delink.hs"
       ;; " --metadata-file=metadata.yml"
       ;; " --metadata=reference-section-title:'References'"
       " --metadata=reference-section-title:Références"
       " --citeproc"
       ;; " --filter ./delink.hs"
       ;; " --filter ./delink.hs"
       ;; " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
       " --bibliography=bibliography.bib"
       ))

(provide 'sync0-markdown)
