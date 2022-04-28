(defvar sync0-obsidian-archive-directory
     "/home/sync0/Pictures/archives/")

;; (defvar sync0-obsidian-default-zettel-body
;;   (concat "---\n"
;;                                  "id: " filename "\n"
;;                                  "zettel_type: " type "\n"
;;                                  "created: " creation "\n"
;;                                  "title: \"" title "\"\n"
;;                                  "aliases: [\"" title "\"]\n"
;;                                  "tags: [" type "]\n"
;;                                  "---\n")

(defun sync0-obsidian-create-zettel ()
  (interactive)
  (let* ((type (completing-read "Choose Zettel type: " sync0-zettelkasten-zettel-types))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (title (read-string "Titre du texte : " nil nil nil t))
         (obsidian-file (concat sync0-obsidian-directory filename ".md")) 
         (obsidian-entry (concat "---\n"
                                 "id: " filename "\n"
                                 "zettel_type: " type "\n"
                                 "created: " creation "\n"
                                 ;; "title: \"" title "\"\n"
                                 "aliases: [\"" title "\"]\n"
                                 "tags: [" type "]\n"
                                 "---\n" 
                                 "# " title "\n\n")))
    (with-temp-buffer 
      (insert obsidian-entry)
      (write-file obsidian-file))
    (find-file obsidian-file)))

(defun sync0-obsidian-create-freewriting-zettel ()
  (interactive)
  (let* ((filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (year (format-time-string "%Y")) 
         (month (format-time-string "%m")) 
         (day (format-time-string "%d")) 
         (obsidian-file (concat sync0-obsidian-directory filename ".md")) 
         (obsidian-entry (concat "---\n"
                                 "id: " filename "\n"
                                 "zettel_type: writing\n"
                                 "created: " creation "\n"
                                 ;; "title: \"Écriture libre, " creation "\"\n"
                                 "aliases: [\"Écriture libre, " creation "\"]\n"
                                 "tags: [writing/freewriting, date/" year "/" month "/" day "]\n"
                                 "---\n" 
                                 "# Écriture libre, " creation "\n"
                                 "[Index de l’écriture libre](20211004120832.md)\n")))
    (with-temp-buffer 
      (insert obsidian-entry)
      (write-file obsidian-file))
    (find-file obsidian-file)))


(defhydra sync0-hydra-obsidian-functions (:color amaranth :hint nil :exit t)
  "
 ^Obsidian functions^
 ^^^----------------
 Zettel: _n_ew
 Zettel: _f_reewriting

 _q_uit
 "
  ("f" sync0-obsidian-create-freewriting-zettel)
  ("n" sync0-obsidian-create-zettel)
  ("q" nil :color blue))

(evil-leader/set-key
  "c" 'sync0-hydra-obsidian-functions/body)

(provide 'sync0-obsidian)
