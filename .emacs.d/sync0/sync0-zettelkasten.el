
;; (defvar sync0-zkn-jump-cache (make-hash-table :test 'equal))

;; (defvar obsidian-cache-file "~/.emacs.d/obsidian-cache.el"
;;   "File path to save the cached Obsidian data.")

(defvar sync0-zkn-zettel-types-alist
  '(("main" . ("annotation" "bibliography" "chronology" "citation" "list" "snippet"))
    ("fiche" . ("character" "people" "concept" "event" "institution" "keyword" "place" "publication" "reference" "oeuvre"))
    ("index" . ("dashboard" "bibliography" "list"))
    ("project" . ("itineris" "task" "note")) ;; No subtypes
    ("writing" . ("collage" "draft" "freewriting" "plan")))
  "An alist mapping Zettel types to their corresponding subtypes for sync0's Zettelkasten system.")

(defun sync0-zkn-generate-unique-filename (dir)
  "Save Obsidian cache data to a file."
  (let ((existing-files (directory-files dir nil "\\.md$")))  ;; List current .md files in directory
    (sync0-generate-unique-filename existing-files))) ;; Generate a unique filename

(defvar sync0-zkn-zettel-folge
  "\n\n[← Vorgängerzettel](.md) | [Ursprungszettel](.md) | [Folgezettel →](.md)")

(defvar sync0-zkn-zettel-coda
  "\n\n\n\n\n\n\n\n##  Fontes\n\n## Segmenta\n\n## Calculi\n\n## Componentes\n\n## Scripta\n\n## Relationes\n\n## Index\n\n")

(defvar sync0-zkn-main-coda
  "## Incipit\n\n## Adversaria\n\n## Calculi\n\n## Relationes\n\n## Fontes\n\n## Segmenta\n\n## Componentes\n\n## Scripturae\n\n## Index\n\n")

(defvar sync0-zkn-org-main-coda
  "* Incipit\n\n* Adversaria\n\n* Calculi\n\n* Relationes\n\n* Fontes\n\n* Segmenta\n\n* Componentes\n\n* Scripturae\n\n* Index\n\n")

(defvar sync0-zkn-archive-dir
     "/home/sync0/Pictures/archives/")

(defvar sync0-zkn-doctype nil)

(defvar sync0-zkn-vorgangerzettel nil)

(defvar sync0-zkn-ursprungszettel nil)

(defvar sync0-zkn-folgezettel nil)

(defvar sync0-zkn-writing-version nil)

(defvar sync0-zkn-writing-last-version nil)

(defvar sync0-zkn-notes-citations-dir
  (concat sync0-zkn-dir "citations/"))

(defvar sync0-zkn-notes-inbox-dir
  (concat sync0-zkn-dir "inbox/"))

(defvar sync0-zkn-notes-projects-dir
  (concat sync0-zkn-dir "project_notes/"))

(defvar sync0-zkn-notes-plans-dir
  (concat sync0-zkn-dir "plans/"))

(defvar sync0-zkn-notes-institutions-dir
  (concat sync0-zkn-dir "institutions/"))

(defvar sync0-zkn-notes-chronologies-dir
  (concat sync0-zkn-dir "chronologies/"))

(defvar sync0-zkn-notes-snippets-dir
  (concat sync0-zkn-dir "snippets/"))

(defvar sync0-zkn-notes-freewriting-dir
  (concat sync0-zkn-dir "freewriting/"))

(defvar sync0-zkn-notes-lists-dir
  (concat sync0-zkn-dir "lists/"))

(defvar sync0-zkn-notes-structure-dir
  (concat sync0-zkn-dir "structures/"))

(defvar sync0-zkn-notes-annotations-dir
  (concat sync0-zkn-dir "annotations/"))

(defvar sync0-zkn-notes-main-dir
  (concat sync0-zkn-dir "main/"))

(defvar sync0-zkn-notes-fiches-dir
  (concat sync0-zkn-dir "fiches/"))

(defvar sync0-zkn-notes-oeuvres-dir
  (concat sync0-zkn-dir "oeuvres/"))

(defvar sync0-zkn-notes-keywords-dir
  (concat sync0-zkn-dir "keywords/"))

(defvar sync0-zkn-templates-dir
     (concat sync0-zkn-dir "templates/")
  "Directory for templates.")

(defvar sync0-zkn-notes-people-dir
  (concat sync0-zkn-dir "people/")
  "Directory for people zettels.")

(defvar sync0-zkn-notes-itineris-dir
  (concat sync0-zkn-dir "itineris/")
  "Directory for people zettels.")

(defvar sync0-zkn-notes-tasks-dir
  (concat sync0-zkn-dir "tasks/")
  "Directory for people zettels.")

(defvar sync0-zkn-notes-publications-dir
  (concat sync0-zkn-dir "publications/"))

(defvar sync0-zkn-notes-concepts-dir
  (concat sync0-zkn-dir "concepts/"))

(defvar sync0-zkn-notes-composants-dir
  (concat sync0-zkn-dir "composants/"))

(defvar sync0-zkn-notes-scratches-dir
  (concat sync0-zkn-dir "scratches/"))

(defvar sync0-zkn-notes-events-dir
  (concat sync0-zkn-dir "events/"))

(defvar sync0-zkn-notes-drafts-dir
  (concat sync0-zkn-dir "drafts/"))

(defvar sync0-zkn-notes-writings-dir
  (concat sync0-zkn-dir "writings/"))

(defvar sync0-zkn-notes-references-dir
  (concat sync0-zkn-dir "references/"))

(defvar sync0-zkn-notes-collages-dir
  (concat sync0-zkn-dir "collages/"))

(defvar sync0-zkn-notes-indices-dir
  (concat sync0-zkn-dir "indices/"))

(defvar sync0-zkn-notes-dashboards-dir
  (concat sync0-zkn-dir "dashboards/"))

(defvar sync0-zkn-notes-bibliographies-dir
  (concat sync0-zkn-dir "bibliographies/"))

(defvar sync0-zkn-notes-places-dir
  (concat sync0-zkn-dir "places/"))

(defun sync0-zkn-calc-folgezettel-line (&optional urs)
  (let ((ursprung (or urs sync0-zkn-ursprungszettel ""))
        (vorganger (or sync0-zkn-vorgangerzettel ""))
        (folge (or sync0-zkn-folgezettel "")))
    (format "[← Vorgängerzettel](%s.md) | [Ursprungszettel](%s.md) | [Folgezettel →](%s.md)\n\n" vorganger ursprung folge)))

(defun sync0-zkn-get-dir (type subtype)
  "Return the directory path based on the Zettel type and subtype."
  (cond
   ;; Main Zettel types (default to their respective main directory)
   ((string= type "main") (pcase subtype
                            ("annotation" sync0-zkn-notes-annotations-dir)
                            ("bibliography" sync0-zkn-notes-bibliographies-dir)
                            ("chronology" sync0-zkn-notes-chronologies-dir)
                            ("structure" sync0-zkn-notes-structure-dir)
                            ("citation" sync0-zkn-notes-citations-dir)
                            ("list" sync0-zkn-notes-bibliographies-dir)
                            ("snippet" sync0-zkn-notes-snippets-dir)
			    (_ sync0-zkn-notes-main-dir)))
   ((string= type "fiche") (pcase subtype
                             ("character" sync0-zkn-notes-people-dir)
                             ("people" sync0-zkn-notes-people-dir)
                             ("concept" sync0-zkn-notes-concepts-dir)
                             ("event" sync0-zkn-notes-events-dir)
                             ("institution" sync0-zkn-notes-institutions-dir)
                             ("keyword" sync0-zkn-notes-keywords-dir)
                             ("place" sync0-zkn-notes-places-dir)
                             ("publication" sync0-zkn-notes-publications-dir)
                             ("reference" sync0-zkn-notes-publications-dir)
                             ("oeuvre" sync0-zkn-notes-oeuvres-dir)
                             (_ sync0-zkn-notes-fiches-dir))) ;; Default directory for fiche type
   ((string= type "index") (pcase subtype
                            ("dashboard" sync0-zkn-notes-dashboards-dir)
                            ("bibliography" sync0-zkn-notes-bibliographies-dir)
                            ("list" sync0-zkn-notes-lists-dir)
			    (_ sync0-zkn-notes-indices-dir)))
   ((string= type "project") (pcase subtype
                            ("itineris" sync0-zkn-notes-itineris-dir)
                            ("task" sync0-zkn-notes-tasks-dir)
                            ("note" sync0-zkn-notes-projects-dir)
			    (_ sync0-zkn-notes-projects-dir)))
   ((string= type "writing") (pcase subtype
                               ("collage" sync0-zkn-notes-collages-dir)
                               ("draft" sync0-zkn-notes-drafts-dir)
                               ("freewriting" sync0-zkn-notes-freewriting-dir)
                               ("plan" sync0-zkn-notes-plans-dir)
                               (_ sync0-zkn-notes-writings-dir))) ;; Default directory for writing type
   ;; If the type is not found, return a default directory
   (t sync0-zkn-notes-inbox-dir)))

(defvar sync0-zkn-search-directories
  (seq-filter (lambda (dir)
               (and (file-directory-p dir)
                    (not (string-match-p "/\\." dir))))  ;; Exclude hidden directories
              (directory-files-recursively sync0-zkn-dir ".*" t))
  "List of all subdirectories within the zkn directory, excluding hidden folders.")

(defvar sync0-zkn-zettel-filename nil)
(defvar sync0-zkn-zettel-creation nil)
(defvar sync0-zkn-zettel-title nil)
(defvar sync0-zkn-zettel-current-key nil)
(defvar sync0-zkn-zettel-up-line nil)
(defvar sync0-zkn-zettel-folgezettel-line nil)
(defvar sync0-zkn-zettel-entry nil)

(provide 'sync0-zettelkasten)
