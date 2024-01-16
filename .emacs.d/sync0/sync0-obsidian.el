(require ' sync0-bibtex-key-functions)
(require ' sync0-bibtex-corrections)
(require ' sync0-bibtex-url)

(defvar sync0-obsidian-archive-directory
     "/home/sync0/Pictures/archives/")

(defvar sync0-obsidian-templates-directory
     (concat sync0-zettelkasten-directory "templates/")
  "Directory for templates.")

(defvar sync0-obsidian-people-directory
  (concat sync0-zettelkasten-directory "people/")
  "Directory for people zettels.")

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
                                 "key: " filename "\n"
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
                                 "key: " filename "\n"
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

(defun sync0-obsidian-create-people-fiche (person-name)
  "Create an obsidian markdown fiche for a person in people folder."
  (if (stringp person-name)
      (let* ((filename (sync0-bibtex-entry-key-define t 3))
             (name-string-fixed (xah-replace-pairs-in-string
                                 person-name
                                 [["_" ", "]
                                  ["+" " "]]))
             (name-fixed (sync0-bibtex-corrections-reverse-name name-string-fixed))
             (creation (format-time-string "%Y-%m-%d")) 
             (tag-creation (format-time-string "%Y/%m/%d")) 
             (tag-people (downcase
                          (xah-replace-pairs-in-string
                           (unidecode name-string-fixed)
                           [[", " "_"]
                            [" " "-"]
                            ["de " ""]
                            [" de la " "-"]
                            [" de " "-"]
                            ["d’" ""]
                            ["l’" ""]
                            ["-de-" "-"]])))
             (obsidian-file (concat sync0-obsidian-people-directory filename ".md")) 
             (works (concat "## Œuvre\n
```dataviewjs\n
function capitalizeFirst(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}\n
let pages = dv.pages(\"(#reference or #fiche/reference) and #author/" tag-people "\");\n
for (let group of pages.groupBy(p => p.biblatex_type)) {
    dv.header(3, capitalizeFirst(group.key));
    dv.table([\"Date\", \"Titre\"],
             group.rows
             .sort(p => parseInt(p.date), 'asc')
             .map(k => [parseInt(k.date), `[[${k.file.name}|${k.title}]]`]
                 ))
}
```"))
             (obsidian-entry (concat "---\n"
                                     "key: " filename "\n"
                                     "zettel_type: fiche\n"
                                     "zettel_subtype: people\n"
                                     "created: " creation "\n"
                                     "title: \"" name-fixed "\"\n"
                                     "people: \"" name-string-fixed "\"\n"
                                     "aliases:\n  - \"" name-string-fixed "\"\n  - \"" name-fixed "\"\n"
                                     "tags:\n  - fiche/people\n  - created/" tag-creation  "\n  - people/" tag-people "\n"
                                     "---\n" 
                                     "# " name-fixed "\n\n" works)))
        ;; Sanity check before creation
        (unless (and (member person-name sync0-bibtex-completion-author)
                     (not (yes-or-no-p "Author already present in completion file. Proceed to create fiche?")))
          (sync0-create-file-with-content obsidian-entry obsidian-file)))
    (error "Argument provided is not a string")))

(defun sync0-obsidian-create-multiple-people-fiche ()
  "Create an obsidian markdown fiche for a person in people folder."
  (interactive)
  (let* (x
         (crm-separator  "[ 	]*;[ 	]*")
         (people  (completing-read-multiple "People to crete new md fiches: " sync0-bibtex-completion-author)))
    (dolist (element people x)
      (sync0-obsidian-create-people-fiche element)
      (setq x (concat x element "\n")))
    (message "Created md fiches for %s" x)))

(defhydra sync0-hydra-obsidian-functions (:color amaranth :hint nil :exit t)
  "
 ^Obsidian functions^
 ^^^----------------
 Zettel: _n_ew
 Fiche: _p_eople
 Zettel: _f_reewriting
 ^^^----------------
 ^Other functions^
 ^^^----------------
 Search: _s_earch in catalogs

 _q_uit
 "
  ("f" sync0-obsidian-create-freewriting-zettel)
  ("p" sync0-obsidian-create-multiple-people-fiche)
  ("n" sync0-obsidian-create-zettel)
  ("s" sync0-search-in-catalogs)
  ("q" nil :color blue))

(evil-leader/set-key
  "c" 'sync0-hydra-obsidian-functions/body)

(provide 'sync0-obsidian)
