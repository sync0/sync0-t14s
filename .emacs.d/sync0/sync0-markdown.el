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

(provide 'sync0-markdown)
