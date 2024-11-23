(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-corrections)
(require 'sync0-bibtex-url)

(defvar sync0-obsidian-jump-cache (make-hash-table :test 'equal))

(defvar obsidian-cache-file "~/.emacs.d/obsidian-cache.el"
  "File path to save the cached Obsidian data.")

(defun sync0-obsidian-save-cache ()
  "Save Obsidian cache data to a file."
  (when (and (boundp 'obsidian-files-cache) (boundp 'obsidian--aliases-map))
    (with-temp-file obsidian-cache-file
      (prin1 (list obsidian-files-cache obsidian-cache-timestamp obsidian--aliases-map)
             (current-buffer)))))

(defun sync0-obsidian-load-cache ()
  "Load Obsidian cache data from a file, if it exists."
  (when (file-exists-p obsidian-cache-file)
    (with-temp-buffer
      (insert-file-contents obsidian-cache-file)
      (let ((data (read (current-buffer))))
        (setq obsidian-files-cache (nth 0 data))
        (setq obsidian-cache-timestamp (nth 1 data))
        (setq obsidian--aliases-map (nth 2 data))))))

(defun sync0-obsidian-generate-unique-filename (dir)
  "Save Obsidian cache data to a file."
  (let ((existing-files (directory-files dir nil "\\.md$")))  ;; List current .md files in directory
    (sync0-generate-unique-filename existing-files))) ;; Generate a unique filename

(defvar sync0-obsidian-zettel-folge
  "\n\n[← Vorgängerzettel](.md) | [Ursprungszettel](.md) | [Folgezettel →](.md)")

(defvar sync0-obsidian-zettel-coda
  "\n\n\n\n\n\n\n\n##  Fontes\n\n## Segmenta\n\n## Calculi\n\n## Componentes\n\n## Scripta\n\n## Relationes\n\n## Index\n\n")

(defvar sync0-obsidian-main-coda
  "## Incipit\n\n## Adversaria\n\n## Calculi\n\n## Relationes\n\n## Fontes\n\n## Segmenta\n\n## Componentes\n\n## Scripturae\n\n## Index\n\n")

(defvar sync0-obsidian-archive-directory
     "/home/sync0/Pictures/archives/")

(defvar sync0-obsidian-doctype nil)

(defvar sync0-obsidian-vorgangerzettel nil)

(defvar sync0-obsidian-ursprungszettel nil)

(defvar sync0-obsidian-folgezettel nil)

(defvar sync0-obsidian-writing-version 0)

(defvar sync0-obsidian-writing-last-version 0)

(defvar sync0-obsidian-notes-citations-directory
  (concat sync0-zettelkasten-directory "citations/"))

(defvar sync0-obsidian-notes-inbox-directory
  (concat sync0-zettelkasten-directory "inbox/"))

(defvar sync0-obsidian-notes-projects-directory
  (concat sync0-zettelkasten-directory "project_notes/"))

(defvar sync0-obsidian-notes-plans-directory
  (concat sync0-zettelkasten-directory "plans/"))

(defvar sync0-obsidian-notes-institutions-directory
  (concat sync0-zettelkasten-directory "institutions/"))

(defvar sync0-obsidian-notes-chronologies-directory
  (concat sync0-zettelkasten-directory "chronologies/"))

(defvar sync0-obsidian-notes-snippets-directory
  (concat sync0-zettelkasten-directory "snippets/"))

(defvar sync0-obsidian-notes-freewriting-directory
  (concat sync0-zettelkasten-directory "freewriting/"))

(defvar sync0-obsidian-notes-lists-directory
  (concat sync0-zettelkasten-directory "lists/"))

(defvar sync0-obsidian-notes-structure-directory
  (concat sync0-zettelkasten-directory "structures/"))

(defvar sync0-obsidian-notes-annotations-directory
  (concat sync0-zettelkasten-directory "annotations/"))

(defvar sync0-obsidian-notes-main-directory
  (concat sync0-zettelkasten-directory "main/"))

(defvar sync0-obsidian-notes-fiches-directory
  (concat sync0-zettelkasten-directory "fiches/"))

(defvar sync0-obsidian-notes-oeuvres-directory
  (concat sync0-zettelkasten-directory "oeuvres/"))

(defvar sync0-obsidian-notes-keywords-directory
  (concat sync0-zettelkasten-directory "keywords/"))

(defvar sync0-obsidian-templates-directory
     (concat sync0-zettelkasten-directory "templates/")
  "Directory for templates.")

(defvar sync0-obsidian-notes-people-directory
  (concat sync0-zettelkasten-directory "people/")
  "Directory for people zettels.")

(defvar sync0-obsidian-notes-publications-directory
  (concat sync0-zettelkasten-directory "publications/"))

(defvar sync0-obsidian-notes-concepts-directory
  (concat sync0-zettelkasten-directory "concepts/"))

(defvar sync0-obsidian-notes-composants-directory
  (concat sync0-zettelkasten-directory "composants/"))

(defvar sync0-obsidian-notes-scratches-directory
  (concat sync0-zettelkasten-directory "scratches/"))

(defvar sync0-obsidian-notes-events-directory
  (concat sync0-zettelkasten-directory "events/"))

(defvar sync0-obsidian-notes-drafts-directory
  (concat sync0-zettelkasten-directory "drafts/"))

(defvar sync0-obsidian-notes-writings-directory
  (concat sync0-zettelkasten-directory "writings/"))

(defvar sync0-obsidian-notes-references-directory
  (concat sync0-zettelkasten-directory "references/"))

(defvar sync0-obsidian-notes-collages-directory
  (concat sync0-zettelkasten-directory "collages/"))

(defvar sync0-obsidian-notes-indices-directory
  (concat sync0-zettelkasten-directory "indices/"))

(defvar sync0-obsidian-notes-dashboards-directory
  (concat sync0-zettelkasten-directory "dashboards/"))

(defvar sync0-obsidian-notes-bibliographies-directory
  (concat sync0-zettelkasten-directory "bibliographies/"))

(defvar sync0-obsidian-notes-places-directory
  (concat sync0-zettelkasten-directory "places/"))

(defun sync0-obsidian-calc-folgezettel-line (&optional urs)
  (let ((ursprung (or urs sync0-obsidian-ursprungszettel ""))
        (vorganger (or sync0-obsidian-vorgangerzettel ""))
        (folge (or sync0-obsidian-folgezettel "")))
    (format "[← Vorgängerzettel](%s.md) | [Ursprungszettel](%s.md) | [Folgezettel →](%s.md)\n\n" vorganger ursprung folge)))

(defun sync0-obsidian-get-directory (type subtype)
  "Return the directory path based on the Zettel type and subtype."
  (cond
   ;; Main Zettel types (default to their respective main directory)
   ((string= type "main") (pcase subtype
                            ("annotation" sync0-obsidian-notes-annotations-directory)
                            ("bibliography" sync0-obsidian-notes-bibliographies-directory)
                            ("chronology" sync0-obsidian-notes-chronologies-directory)
                            ("citation" sync0-obsidian-notes-citations-directory)
                            ("list" sync0-obsidian-notes-bibliographies-directory)
                            ("snippet" sync0-obsidian-notes-snippets-directory)
			    (_ sync0-obsidian-notes-main-directory)))
   ((string= type "fiche") (pcase subtype
                             ("character" sync0-obsidian-notes-people-directory)
                             ("people" sync0-obsidian-notes-people-directory)
                             ("concept" sync0-obsidian-notes-concepts-directory)
                             ("event" sync0-obsidian-notes-events-directory)
                             ("institution" sync0-obsidian-notes-institutions-directory)
                             ("keyword" sync0-obsidian-notes-keywords-directory)
                             ("place" sync0-obsidian-notes-places-directory)
                             ("publication" sync0-obsidian-notes-publications-directory)
                             ("reference" sync0-obsidian-notes-publications-directory)
                             ("oeuvre" sync0-obsidian-notes-oeuvres-directory)
                             (_ sync0-obsidian-notes-fiche-directory))) ;; Default directory for fiche type
   ((string= type "index") (pcase subtype
                            ("dashboard" sync0-obsidian-notes-dashboards-directory)
                            ("bibliography" sync0-obsidian-notes-bibliographies-directory)
                            ("list" sync0-obsidian-notes-lists-directory)
			    (_ sync0-obsidian-notes-indices-directory)))
   ((string= type "project") sync0-obsidian-notes-projects-directory)
   ((string= type "writing") (pcase subtype
                               ("collage" sync0-obsidian-notes-collages-directory)
                               ("draft" sync0-obsidian-notes-drafts-directory)
                               ("freewriting" sync0-obsidian-notes-freewriting-directory)
                               ("plan" sync0-obsidian-notes-plans-directory)
                               (_ sync0-obsidian-notes-writings-directory))) ;; Default directory for writing type
   ;; If the type is not found, return a default directory
   (t sync0-obsidian-notes-inbox-directory)))

(defvar sync0-obsidian-search-directories
  (seq-filter (lambda (dir)
               (and (file-directory-p dir)
                    (not (string-match-p "/\\." dir))))  ;; Exclude hidden directories
              (directory-files-recursively sync0-zettelkasten-directory ".*" t))
  "List of all subdirectories within the zettelkasten directory, excluding hidden folders.")

(defvar sync0-obsidian-zettel-filename nil)
(defvar sync0-obsidian-zettel-creation nil)
(defvar sync0-obsidian-zettel-title nil)
(defvar sync0-obsidian-zettel-current-key nil)
(defvar sync0-obsidian-zettel-up-line nil)
(defvar sync0-obsidian-zettel-folgezettel-line nil)
(defvar sync0-obsidian-zettel-entry nil)

(defun sync0-obsidian-find-existing-markdown-files (directory)
  "Find markdown files in DIRECTORY whose names match the current day number."
  (let* ((current-date sync0-bibtex-timeday)
         (regex (concat "^" current-date "[a-zA-Z]\\{3\\}$"))
         (files (directory-files directory t "^[0-9]+[a-zA-Z]\\{3\\}$"))
         (matching-files (seq-filter (lambda (file) (string-match-p regex (file-name-nondirectory file))) files)))
    matching-files))

(defun sync0-obsidian-define-unique-filename-list (directory &optional times)
  "Create new unique filename(s) that do not exist in DIRECTORY.
If TIMES is provided, create that many filenames; otherwise, create one.
Return a string if TIMES is 1; otherwise, return a list."
  (setq times (or times 1))  ;; Use 1 as default value if times is not provided
  (let ((values '())
        (existing-files (sync0-obsidian-find-existing-markdown-files directory)))
    (dotimes (i times)
      (let ((new-filename (sync0-generate-unique-filename existing-files)))
        (push new-filename values)
        (push new-filename existing-files)))
    (if (= times 1)
        (car values)  ;; Return the single filename as a string
      values)))  ;; Return the list of filenames

(defun sync0-obsidian-format-zettel-body (filename type subtype title after-type extra-aliases extra-meta tags)
  (let* ((mcreation (format-time-string "%Y-%m-%d"))
         (zcreation (format-time-string "%Y/%m/%d"))
         (current-key (when (and (buffer-file-name)
				 (string-match-p "\\.md\\'" (buffer-file-name)))
			(sync0-yaml-get-property "key")))
         (up-line (if current-key
                      (concat "up: \"[[" current-key "]]\"\n")
                    ""))
         (folgezettel-line (if current-key
                               (sync0-obsidian-calc-folgezettel-line current-key) 
                             (sync0-obsidian-calc-folgezettel-line)))
	 (obsidian-entry (concat "---\n"
				 "key: " filename "\n"
				 "zettel_type: " type "\n"
				 after-type
				 "title: \"" title "\"\n"
				 "aliases:\n"
				 "  - \"" filename "\"\n"
				 "  - \"" title "\"\n"
				 extra-aliases
				 "created: " mcreation "\n"
				 up-line
				 extra-meta
				 "tags:\n"
				 tags
				 "  - created/" zcreation "\n"
				 "---\n"
				 "# " title "\n\n"
				 folgezettel-line
				 sync0-obsidian-main-coda)))
    (setq sync0-obsidian-zettel-entry obsidian-entry)))

(defun sync0-obsidian-format-zettel (filename type &optional subtype)
  (let* ((zsubtype (or subtype ""))
	 (doctype sync0-obsidian-doctype)
	 (after-type (concat
		      (when		      subtype
			(format "zettel_subtype: %s\n" zsubtype))  
		      (when		      doctype
			(format "doctype: %s\n" doctype))
		      ""))  
	 (version (if (eq sync0-obsidian-writing-version 0)
		      1
		    sync0-obsidian-writing-version))
	 (last-version (if (eq sync0-obsidian-writing-last-version 0)
			   1
			 sync0-obsidian-writing-last-version))
	 (mwriting (if (string= doctype "draft")
		       (concat
			"lang: en-US\n"
			(format "version: %s\n" version) 
			(format "lastversion: %s\n" lastversion)) 
		     ""))
         (title (if (use-region-p)  ;; Check if text is highlighted
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Titre du texte : ")))
	 (tags (concat (if subtype 
			   (format "  - %s/%s\n" type zsubtype)
			   (format "  - %s\n" type))
		       (when doctype
			 (format "  - doctype/%s\n" doctype)))))
    (sync0-obsidian-format-zettel-body filename type subtype title after-type "" mwriting tags)))

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



(defun sync0-obsidian-create-note (title directory ztype zsubtype)
  "Create an Obsidian markdown note with minimal input.
Prompt for TITLE of the note, generate a unique filename, and
populate YAML frontmatter with predefined metadata. Save the note
in DIRECTORY."
  (interactive "sEnter the title of the note: \nDDirectory: ")
  (let* ((filename (sync0-obsidian-define-unique-filename-list directory))
         (creation (format-time-string "%Y-%m-%d"))
         (tag-creation (format-time-string "%Y/%m/%d"))
         (tags-ztype (if (string= zsubtype "nil")
			 (format "\n  - %s" ztype)
		       (format "\n  - %s/%s" ztype zsubtype)))
         (obsidian-file (concat directory filename ".md")) 
         (obsidian-entry (format
                          "---\nkey: %s\nzettel_type: %s\ncreated: %s\ntitle: \"%s\"\naliases:\n  - \"%s\"\ntags:%s\n  - created/%s\n---\n# %s%s%s"
                          filename ztype creation title title tags-ztype tag-creation title sync0-obsidian-zettel-folge sync0-obsidian-zettel-coda)))
    (sync0-create-file-with-content obsidian-entry obsidian-file)
    (message "Note created: %s" (file-name-nondirectory obsidian-file))))

(defun sync0-obsidian-create-multiple-notes ()
  "Create multiple Obsidian markdown notes interactively."
  (interactive)
  (let* ((separator ";")  ;; Change to any separator you prefer
	 (ztype (completing-read "Zettel type? " '("permanent" "fiche")))
	 (zsubtype (if (string= ztype "permanent")
		       (completing-read "Main subtype? " '("structure" "nil"))
		     (completing-read "Fiche subtype? " '("publication" "place" "people" "oeuvre" "institution" "keyword" "concept" "event" "nil"))))
	 (directory-name (cond ((and (string= ztype "permanent")
                                     (string= zsubtype "structure"))
				sync0-obsidian-notes-structure-directory)
			       ((and (string= ztype "permanent")
                                     (string= zsubtype "nil"))
				sync0-obsidian-notes-main-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "publication"))
				sync0-obsidian-notes-publications-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "event"))
				sync0-obsidian-notes-event-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "place"))
				sync0-obsidian-notes-place-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "people"))
				sync0-obsidian-notes-people-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "oeuvre"))
				sync0-obsidian-notes-oeuvres-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "institution"))
				sync0-obsidian-notes-fiches-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "keyword"))
				sync0-obsidian-notes-keywords-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "concept"))
				sync0-obsidian-notes-concepts-directory)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "nil"))
				sync0-obsidian-notes-fiches-directory)
			       (t (error "No valid directory found for Zettel type"))))
         (titles (if (use-region-p)
                     (split-string (buffer-substring (region-beginning) (region-end)) "\n" t)
                   (sync0-define-list-interactively "Enter note title: " "Add another note? "))))
    (if (= (length titles) 1)
        (sync0-obsidian-create-note (car titles) directory-name ztype zsubtype)
      (dolist (title titles)
        (sync0-obsidian-create-note title directory-name ztype zsubtype))
      (message "Created %d notes." (length titles)))))

(defun sync0-obsidian-replace-region-with-markdown-link (filename)
    (when (use-region-p)
      (let ((highlighted-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[%s](%s.md)" highlighted-text filename)))))

(defun sync0-obsidian-create-zettel-process (filename filepath type &optional subtype)
  "Common process to run when creating a new zettel"
  (let ((dsubtype (or subtype 'nil)))
    ;; Format the note with the Zettel template and write it to file
    (sync0-obsidian-format-zettel filename type dsubtype)
    (sync0-write-to-file sync0-obsidian-zettel-entry filepath)
    ;; Add the new file to `obsidian-files-cache` in memory
    (add-to-list 'obsidian-files-cache filepath)
    ;; Update the persistent cache file with the new file path
    (sync0-obsidian-save-cache)
    ;; Optionally, open the new note in the buffer
    ;; (find-file obsidian-file)
    (sync0-obsidian-replace-region-with-markdown-link filename)
    (message "New zettel created and added to cache: %s" filepath)))

(defun sync0-obsidian-new-draft ()
  "Create a new draft note in Obsidian's drafts directory based on a selected Zettel template."
  (interactive)
  (let* ((type "writing")
	 (subtype "draft")
	 (directory (sync0-obsidian-get-directory type subtype))  
	 (filename (sync0-obsidian-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md"))
	 (doctype (sync0-yaml-get-property "doctype"))
	 (subtitle (or (sync0-yaml-get-property "title") ""))
	 (after-type (concat
		      (format "subtype: %s\n" subtype) 
		      (unless (sync0-null-p doctype)
			(format "doctype: %s\n" doctype)) 
		      (unless (sync0-null-p subtitle)
			(format "subtitle: %s\n" subtitle))))
	 (title (sync0-yaml-get-property "title"))
         (current-key (sync0-yaml-get-property "key"))
	 (lang (sync0-yaml-get-property "lang"))
	 (project (sync0-yaml-get-property "project"))
	 (version  (1+ (sync0-yaml-get-property "version")))
         (extra-aliases (format "  - \"%s, v%s\"\n" title version))
	 (lastversion (+1 (sync0-yaml-get-property "lastversion")))
	 (mwriting (concat
		    (format "lang: %s\n" lang) 
		    (format "project: %s\n" project) 
		    (format "version: %s\n" version) 
		    (format "lastversion: %s\n" lastversion))) 
	 (tags (concat "  - writing/draft\n"
		       (format "  - doctype/%s\n" doctype) 
		       (format "  - version/%s\n" version) 
		       (format "  - lastversion/%s\n" lastversion)))
	 (body (save-excursion
		 (goto-char (point-min))
		 (when (re-search-forward "^---\n" nil t 2)  ;; Skip YAML frontmatter
		   (buffer-substring-no-properties (point) (point-max))))))
    (setq sync0-obsidian-vorgangerzettel current-key)
    (setq sync0-obsidian-ursprungszettel project)
    (sync0-obsidian-format-zettel-body filename type subtype title after-type extra-aliases mwriting tags)
    (sync0-write-to-file (concat sync0-obsidian-zettel-entry "\n\n" body) obsidian-file)
    ;; Add the new file to `obsidian-files-cache` in memory
    (add-to-list 'obsidian-files-cache obsidian-file)
    ;; Update the persistent cache file with the new file path
    (sync0-obsidian-save-cache)
    (setq sync0-obsidian-ursprungszettel nil)
    (setq sync0-obsidian-vorgangerzettel nil)
    (message "New zettel created and added to cache: %s" obsidian-file)))

(defun sync0-obsidian-create-zettel ()
  "Create a new zettel note in Obsidian and update the cache."
  (interactive)
  (let* ((type (completing-read "Choose Zettel type: " sync0-zettelkasten-zettel-types))
         (subtype (completing-read "Choose Zettel subtype: " (cdr (assoc type sync0-zettelkasten-zettel-types-alist)))) ;; Choose the subtype based on type
         (doctype (when (string= subtype "draft")
		    (completing-read "Choose Doc. type: " sync0-zettelkasten-doctypes)))
         (directory (sync0-obsidian-get-directory type subtype))  ;; Get the directory based on type and subtype
         (filename (sync0-obsidian-generate-unique-filename directory))
         (obsidian-file (expand-file-name (concat filename ".md") directory)))
    (setq sync0-obsidian-doctype doctype)
    (sync0-obsidian-create-zettel-process filename obsidian-file type subtype)))

(defun sync0-obsidian-create-quick-zettel ()
  "Create a new zettel note in Obsidian and update the cache."
  (interactive)
  (let* ((type "main")
         (directory sync0-obsidian-notes-main-directory)
         (filename (sync0-obsidian-generate-unique-filename directory))
         (obsidian-file (expand-file-name (concat filename ".md") directory)))
    (sync0-obsidian-create-zettel-process filename obsidian-file type)))

(defun sync0-obsidian-create-structure-zettel ()
  (interactive)
  (let* ((type "main")
         (subtype "structure")
	 (directory sync0-obsidian-notes-structure-directory)
         (filename (sync0-obsidian-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md")))
    ;; Format the note with the Zettel template and write it to file
    (sync0-obsidian-create-zettel-process filename obsidian-file type subtype)))

(defun sync0-obsidian-create-fiche-zettel ()
  (interactive)
  (let* ((type "fiche")
         (subtype (completing-read "Choose Zettel subtype: " (cdr (assoc type sync0-zettelkasten-zettel-types-alist)))) ;; Choose the subtype based on type
	 (directory sync0-obsidian-notes-fiches-directory)
         (filename (sync0-obsidian-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md")))
    (sync0-obsidian-create-zettel-process filename obsidian-file type subtype)))

(defun sync0-obsidian-create-keyword-zettel ()
  (interactive)
  (let* ((type "fiche")
         (subtype "keyword")
	 (directory sync0-obsidian-notes-keywords-directory)
         (filename (sync0-obsidian-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md")))
    (sync0-obsidian-create-zettel-process filename obsidian-file type subtype)))


(defun sync0-search-obsidian-notes ()
  "Search Obsidian notes by filename, displaying all filenames with their directories and allowing real-time filtering using ivy."
  (interactive)
  (let ((results '()))
    ;; Collect all filenames and directories
    (dolist (directory sync0-obsidian-search-directories)
      (dolist (file (directory-files directory t "\\.md$"))
        (let ((file-name (file-name-nondirectory file))
              (parent-dir (file-name-directory file)))
          (push (cons (format "%s (%s)" file-name (file-name-nondirectory (directory-file-name parent-dir))) file) results))))
    
    ;; Sort results alphabetically
    (setq results (sort results (lambda (a b) (string< (car a) (car b)))))

    ;; Use ivy to allow real-time filtering and selection
    (ivy-read "Select a note: "
              (mapcar 'car results)  ;; Use only the formatted names for display
              :action (lambda (selected)
                        (find-file (cdr (assoc selected results)))))))

(defun sync0-obsidian-jump ()
  "Jump to Obsidian note."
  (interactive)
  (let* ((files (obsidian-list-all-files))  ; Step 1: Get the list of all files
         (dict (make-hash-table :test 'equal))  ; Step 2: Initialize hash table
         (_ (-map (lambda (f)
                    (puthash (file-relative-name f obsidian-directory) f dict)) files))
         ;; Step 3: Cache all aliases and file names to speed up repeated access
         (cached-aliases (or (gethash 'all-aliases sync0-obsidian-jump-cache) 
                             (let ((all-aliases (obsidian--all-aliases)))
                               (puthash 'all-aliases all-aliases sync0-obsidian-jump-cache)
                               all-aliases)))
         (candidates (-sort #'string< (-distinct (-concat cached-aliases
                                                       (hash-table-keys dict)))))) ; Merge aliases and filenames
      (ivy-read "Jump to: " candidates  ;; Use Ivy for selection
                :action (lambda (choice)
                          (let ((target (obsidian--get-alias choice (gethash choice dict))))
                            (if target
                                (find-file target)  ; Open the file if found
                              (user-error "Note not found: %s" choice)))))))

(evil-leader/set-key "S" 'sync0-search-obsidian-notes)
(evil-leader/set-key "J" 'sync0-obsidian-jump)

(defun find-file-with-invalid-tags (directory)
  "Search for a file in DIRECTORY with YAML frontmatter that contains tags starting with non-alphabetic characters."
  (interactive "DSelect directory: ")
  (let ((found nil))  ;; Flag to track if we found an invalid file
    (dolist (file (directory-files-recursively directory ".*\\.md$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((case-fold-search nil)  ;; Case-sensitive search
              (invalid-tag-found nil))  ;; Flag to detect invalid tag in the current file
          ;; Check if the file has YAML frontmatter
          (when (re-search-forward "^---$" nil t)
            (let ((yaml-end (and (re-search-forward "^---$" nil t) (match-beginning 0))))
              (when yaml-end
                (goto-char (point-min))
                ;; Search for the 'tags' field in YAML frontmatter
                (when (re-search-forward "^tags:\\(.*\\)$" yaml-end t)
                  (let* ((tags-line (match-string 1))  ;; Extract tags
                         (tags (split-string tags-line "[ ,]+" t)))  ;; Split tags by commas or spaces
                    ;; Check if any tag starts with a non-alphabetic character
                    (dolist (tag tags)
                      (when (string-match-p "^[0-9]" tag)  ;; Non-alphabetic start
                        (setq invalid-tag-found t)))))
              ;; If we found an invalid tag, output the file and stop
              (when invalid-tag-found
                (message "Found invalid tag in: %s" file)  ;; Display the message
                (shell-command (format "kate %s" file))  ;; Display in minibuffer
                (setq found t)  ;; Set flag to stop further search
                (return)))))
    ;; If no invalid file was found
    (unless found
      (message "No invalid files found.")))))))


(defun sync0-obsidian-create-people-fiche (person-name)
  "Create an obsidian markdown fiche for a person in people folder."
  (interactive "sEnter the person's name: ")
  (unless (and (member person-name sync0-bibtex-completion-author)
               (not (yes-or-no-p "Author already present in completion file. Proceed to create fiche?")))
    (let* ((type "fiche")
	   (subtype "people")
	   (directory (sync0-obsidian-get-directory type subtype))  
	   (filename (sync0-obsidian-generate-unique-filename directory))
           (obsidian-file (concat directory filename ".md"))
           (name-string-fixed (xah-replace-pairs-in-string
			       person-name
			       [["_" ", "]
				["+" " "]]))
           (name-fixed (sync0-bibtex-corrections-reverse-name name-string-fixed))
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
           (works (format "## Œuvre\n
```dataviewjs\n
function capitalizeFirst(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}\n
let pages = dv.pages(\"(#reference or #fiche/reference) and #author/%s\");\n
for (let group of pages.groupBy(p => p.biblatex_type)) {
    dv.header(3, capitalizeFirst(group.key));
    dv.table([\"Date\", \"Titre\"],
             group.rows
             .sort(p => parseInt(p.date), 'asc')
             .map(k => [parseInt(k.date), `[[${k.file.name}|${k.title}]]`]
                 ))
}
```" tag-people))
           (extra-aliases (format "  - \"%s\"\n  - \"%s\"\n" name-string-fixed name-fixed))
	   (after-type (format "people: \"%s\"\n" name-string-fixed))
	   (tags (concat "  - fiche/people\n"
			 (format "  - people/%s\n" tag-people)))) 
      (sync0-obsidian-format-zettel-body filename type subtype name-fixed after-type extra-aliases "" tags)
      (sync0-write-to-file (concat sync0-obsidian-zettel-entry "\n\n" works) obsidian-file)
      ;; Add the new file to `obsidian-files-cache` in memory
      (add-to-list 'obsidian-files-cache obsidian-file)
      ;; Update the persistent cache file with the new file path
      (sync0-obsidian-save-cache)
      (message "New zettel created for %s added to cache: %s" person-name obsidian-file))))

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
^Zettel Functions^               ^Other Obsidian Functions^
^^^----------------------------   ^^^-----------------------------
_z_ettel quick                    _o_pen at point
_s_tructure                       _j_ump to note
_f_iche                           _t_ag find
_k_eyword                         _x_ search by expr
_p_eople fiche                    _u_pdate tags/aliases
_m_ultinote                       _d_raft create
_S_earch in catalogs              _l_ink
                                  _w_ikilink
                                  _c_apture new note
                                  _q_uit
"
  ("z" sync0-obsidian-create-quick-zettel)
  ("s" sync0-obsidian-create-structure-zettel)
  ("f" sync0-obsidian-create-fiche-zettel)
  ("k" sync0-obsidian-create-keyword-zettel)
  ("p" sync0-obsidian-create-multiple-people-fiche)
  ("m" sync0-obsidian-create-multiple-notes)
  ("S" sync0-search-in-catalogs)
  ("o" obsidian-follow-link-at-point)
  ("j" sync0-obsidian-jump)
  ("t" obsidian-tag-find)
  ("x" obsidian-search)
  ("u" obsidian-update)
  ("d" sync0-obsidian-new-draft)
  ("l" obsidian-insert-link :color blue)
  ("w" obsidian-insert-wikilink :color blue)
  ("c" obsidian-capture)
  ("q" nil :color blue))

(evil-leader/set-key-for-mode 'markdown-mode "O" 'obsidian-follow-link-at-point)

(evil-leader/set-key
  "c" 'sync0-hydra-obsidian-functions/body)

  ;; Step 3: Add hooks to load/save cache on startup and exit
  (add-hook 'emacs-startup-hook 'sync0-obsidian-load-cache)
  (add-hook 'kill-emacs-hook 'sync0-obsidian-save-cache)

(provide 'sync0-obsidian)
