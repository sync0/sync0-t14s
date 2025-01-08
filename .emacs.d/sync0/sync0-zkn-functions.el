(require 'sync0-vars)
(require 'sync0-zettelkasten)
(require 'sync0-zkn)

(defun sync0-zkn-find-existing-markdown-files (directory)
  "Find markdown files in DIRECTORY whose names match the current day number."
  (let* ((current-date sync0-bibtex-timeday)
         (regex (concat "^" current-date "[a-zA-Z]\\{3\\}$"))
         (files (directory-files directory t "^[0-9]+[a-zA-Z]\\{3\\}$"))
         (matching-files (seq-filter (lambda (file) (string-match-p regex (file-name-nondirectory file))) files)))
    matching-files))

(defun sync0-zkn-define-unique-filename-list (directory &optional times)
  "Create new unique filename(s) that do not exist in DIRECTORY.
If TIMES is provided, create that many filenames; otherwise, create one.
Return a string if TIMES is 1; otherwise, return a list."
  (setq times (or times 1))  ;; Use 1 as default value if times is not provided
  (let ((values '())
        (existing-files (sync0-zkn-find-existing-markdown-files directory)))
    (dotimes (i times)
      (let ((new-filename (sync0-zkn-generate-filename existing-files)))
        (push new-filename values)
        (push new-filename existing-files)))
    (if (= times 1)
        (car values)  ;; Return the single filename as a string
      values)))  ;; Return the list of filenames

(defun sync0-zkn-format-zettel-body (filename type subtype title after-type extra-aliases extra-meta tags)
  (let* ((mcreation (format-time-string "%Y-%m-%d"))
         (zcreation (format-time-string "%Y/%m/%d"))
         (current-key (when (and (buffer-file-name)
				 (string-match-p "\\.md\\'" (buffer-file-name)))
			(sync0-yaml-get-property "key")))
         (up-line (if current-key
                      (concat "up: \"[[" current-key "]]\"\n")
                    ""))
         (folgezettel-line (if current-key
                               (sync0-zkn-calc-folgezettel-line current-key) 
                             (sync0-zkn-calc-folgezettel-line)))
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
				 sync0-zkn-main-coda)))
    (setq sync0-zkn-zettel-entry obsidian-entry)))

(defun sync0-zkn-format-zettel (filename type &optional subtype)
  (let* ((zsubtype (or subtype ""))
	 (doctype sync0-zkn-doctype)
	 (after-type (concat
		      (when		      subtype
			(format "zettel_subtype: %s\n" zsubtype))  
		      (when		      doctype
			(format "doctype: %s\n" doctype))
		      ""))  
	 (version (if (null sync0-zkn-writing-version)
		      1
		    sync0-zkn-writing-version))
	 (last-version (if (null sync0-zkn-writing-last-version)
			   1
			 sync0-zkn-writing-last-version))
	 (mwriting (if (string= doctype "draft")
		       (concat
			"lang: en-US\n"
			(format "version: %d\n" version) 
			(format "last_version: %d\n" lastversion)) 
		     ""))
         (title (if (use-region-p)  ;; Check if text is highlighted
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Titre du texte : ")))
	 (tags (concat (if subtype 
			   (format "  - %s/%s\n" type zsubtype)
			   (format "  - %s\n" type))
		       (when doctype
			 (format "  - doctype/%s\n" doctype)))))
    (sync0-zkn-format-zettel-body filename type subtype title after-type "" mwriting tags)))

(defun sync0-zkn-create-freewriting-zettel ()
  (interactive)
  (let* ((filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (year (format-time-string "%Y")) 
         (month (format-time-string "%m")) 
         (day (format-time-string "%d")) 
         (obsidian-file (concat sync0-zkn-dir filename ".md")) 
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

(defun sync0-zkn-create-note (title directory ztype zsubtype)
  "Create an Obsidian markdown note with minimal input.
Prompt for TITLE of the note, generate a unique filename, and
populate YAML frontmatter with predefined metadata. Save the note
in DIRECTORY."
  (interactive "sEnter the title of the note: \nDDirectory: ")
  (let* ((filename (sync0-zkn-define-unique-filename-list directory))
         (creation (format-time-string "%Y-%m-%d"))
         (tag-creation (format-time-string "%Y/%m/%d"))
         (tags-ztype (if (string= zsubtype "nil")
			 (format "\n  - %s" ztype)
		       (format "\n  - %s/%s" ztype zsubtype)))
         (obsidian-file (concat directory filename ".md")) 
         (obsidian-entry (format
                          "---\nkey: %s\nzettel_type: %s\ncreated: %s\ntitle: \"%s\"\naliases:\n  - \"%s\"\ntags:%s\n  - created/%s\n---\n# %s%s%s"
                          filename ztype creation title title tags-ztype tag-creation title sync0-zkn-zettel-folge sync0-zkn-zettel-coda)))
    (sync0-create-file-with-content obsidian-entry obsidian-file)
    (message "Note created: %s" (file-name-nondirectory obsidian-file))))

(defun sync0-zkn-create-multiple-notes ()
  "Create multiple Obsidian markdown notes interactively."
  (interactive)
  (let* ((separator ";")  ;; Change to any separator you prefer
	 (ztype (completing-read "Zettel type? " '("permanent" "fiche")))
	 (zsubtype (if (string= ztype "permanent")
		       (completing-read "Main subtype? " '("structure" "nil"))
		     (completing-read "Fiche subtype? " '("publication" "place" "people" "oeuvre" "institution" "keyword" "concept" "event" "nil"))))
	 (directory-name (cond ((and (string= ztype "permanent")
                                     (string= zsubtype "structure"))
				sync0-zkn-notes-structure-dir)
			       ((and (string= ztype "permanent")
                                     (string= zsubtype "nil"))
				sync0-zkn-notes-main-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "publication"))
				sync0-zkn-notes-publications-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "event"))
				sync0-zkn-notes-event-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "place"))
				sync0-zkn-notes-place-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "people"))
				sync0-zkn-notes-people-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "oeuvre"))
				sync0-zkn-notes-oeuvres-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "institution"))
				sync0-zkn-notes-fiches-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "keyword"))
				sync0-zkn-notes-keywords-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "concept"))
				sync0-zkn-notes-concepts-dir)
			       ((and (string= ztype "fiche")
                                     (string= zsubtype "nil"))
				sync0-zkn-notes-fiches-dir)
			       (t (error "No valid directory found for Zettel type"))))
         (titles (if (use-region-p)
                     (split-string (buffer-substring (region-beginning) (region-end)) "\n" t)
                   (sync0-define-list-interactively "Enter note title: " "Add another note? "))))
    (if (= (length titles) 1)
        (sync0-zkn-create-note (car titles) directory-name ztype zsubtype)
      (dolist (title titles)
        (sync0-zkn-create-note title directory-name ztype zsubtype))
      (message "Created %d notes." (length titles)))))

(defun sync0-zkn-replace-region-with-markdown-link (filename)
    (when (use-region-p)
      (let ((highlighted-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[%s](%s.md)" highlighted-text filename)))))

(defun sync0-zkn-create-zettel-process (filename filepath type &optional subtype)
  "Common process to run when creating a new zettel"
  (let ((dsubtype (or subtype 'nil)))
    ;; Format the note with the Zettel template and write it to file
    (sync0-zkn-format-zettel filename type dsubtype)
    (sync0-write-to-file sync0-zkn-zettel-entry filepath)
    ;; Add the new file to `obsidian-files-cache` in memory
    (add-to-list 'obsidian-files-cache filepath)
    ;; Update the persistent cache file with the new file path
    (sync0-zkn-save-cache)
    (sync0-zkn-insert-into-db filepath)
    ;; Optionally, open the new note in the buffer
    ;; (find-file obsidian-file)
    (sync0-zkn-replace-region-with-markdown-link filename)
    (message "New zettel created and added to cache: %s" filepath)))

(defun sync0-zkn-new-draft ()
  "Create a new draft note in Obsidian's drafts directory based on a selected Zettel template."
  (interactive)
  (let* ((type "writing")
	 (subtype "draft")
	 (directory (sync0-zkn-get-dir type subtype))  
	 (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md"))
	 (doctype (sync0-yaml-get-property "doctype"))
	 (subtitle (or (sync0-yaml-get-property "subtitle") ""))
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
         (extra-aliases (format "  - \"%s, v%d\"\n" title version))
	 (lastversion (1+ (sync0-yaml-get-property "last_version")))
	 (mwriting (concat
		    (format "lang: %s\n" lang) 
		    (format "project: %s\n" project) 
		    (format "version: %d\n" version) 
		    (format "last_version: %d\n" lastversion))) 
	 (tags (concat "  - writing/draft\n"
		       (format "  - doctype/%s\n" doctype) 
		       (format "  - version/%d\n" version) 
		       (format "  - last_version/%d\n" lastversion)))
	 (body (save-excursion
		 (goto-char (point-min))
		 (when (re-search-forward "^---\n" nil t 2)  ;; Skip YAML frontmatter
		   (buffer-substring-no-properties (point) (point-max))))))
    (setq sync0-zkn-vorgangerzettel current-key)
    (setq sync0-zkn-ursprungszettel project)
    (setq sync0-zkn-writing-last-version lastversion)
    (setq sync0-zkn-writing-version version)
    (sync0-zkn-format-zettel-body filename type subtype title after-type extra-aliases mwriting tags)
    (sync0-write-to-file (concat sync0-zkn-zettel-entry "\n\n" body) obsidian-file)
    ;; Add the new file to `obsidian-files-cache` in memory
    (add-to-list 'obsidian-files-cache obsidian-file)
    ;; Update the persistent cache file with the new file path
    (sync0-zkn-save-cache)
    (setq sync0-zkn-ursprungszettel nil)
    (setq sync0-zkn-vorgangerzettel nil)
    (setq sync0-zkn-writing-last-version nil)
    (setq sync0-zkn-writing-version nil)
    (message "New zettel created and added to cache: %s" obsidian-file)))

(defun sync0-zkn-create-zettel ()
  "Create a new zettel note in Obsidian and update the cache. This
is the main function to create Zettels."
  (interactive)
  (let* ((type (completing-read "Choose Zettel type: " sync0-zkn-zettel-types))
         (subtype (completing-read "Choose Zettel subtype: " (cdr (assoc type sync0-zkn-zettel-types-alist)))) ;; Choose the subtype based on type
         (doctype (when (string= subtype "draft")
		    (completing-read "Choose Doc. type: " sync0-zkn-doctypes)))
         (directory (sync0-zkn-get-dir type subtype))  ;; Get the directory based on type and subtype
         (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (expand-file-name (concat filename ".md") directory)))
    (setq sync0-zkn-doctype doctype)
    (sync0-zkn-create-zettel-process filename obsidian-file type subtype)
    (setq sync0-zkn-writing-last-version nil)
    (setq sync0-zkn-writing-version nil)))

;; (global-set-key (kbd "C-c c") 'sync0-zkn-create-zettel)

(defun sync0-zkn-create-quick-zettel ()
  "Create a new zettel note in Obsidian and update the cache."
  (interactive)
  (let* ((type "main")
         (directory sync0-zkn-notes-main-dir)
         (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (expand-file-name (concat filename ".md") directory)))
    (sync0-zkn-create-zettel-process filename obsidian-file type)))

(defun sync0-zkn-create-structure-zettel ()
  (interactive)
  (let* ((type "main")
         (subtype "structure")
	 (directory sync0-zkn-notes-structure-dir)
         (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md")))
    ;; Format the note with the Zettel template and write it to file
    (sync0-zkn-create-zettel-process filename obsidian-file type subtype)))

(defun sync0-zkn-create-fiche-zettel ()
  (interactive)
  (let* ((type "fiche")
         (subtype (completing-read "Choose Zettel subtype: " (cdr (assoc type sync0-zkn-zettel-types-alist)))) ;; Choose the subtype based on type
	 (directory sync0-zkn-notes-fiches-dir)
         (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md")))
    (sync0-zkn-create-zettel-process filename obsidian-file type subtype)))

(defun sync0-zkn-create-keyword-zettel ()
  (interactive)
  (let* ((type "fiche")
         (subtype "keyword")
	 (directory sync0-zkn-notes-keywords-dir)
         (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (concat directory filename ".md")))
    (sync0-zkn-create-zettel-process filename obsidian-file type subtype)))


(defun sync0-search-obsidian-notes ()
  "Search Obsidian notes by filename, displaying all filenames with their directories and allowing real-time filtering using ivy."
  (interactive)
  (let ((results '()))
    ;; Collect all filenames and directories
    (dolist (directory sync0-zkn-search-directories)
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

(defun sync0-zkn-create-people-fiche (person-name)
  "Create an obsidian markdown fiche for a person in people folder."
  (interactive "sEnter the person's name: ")
  (unless (and (member person-name sync0-bibtex-completion-author)
               (not (yes-or-no-p "Author already present in completion file. Proceed to create fiche?")))
    (let* ((type "fiche")
	   (subtype "people")
	   (directory (sync0-zkn-get-dir type subtype))  
	   (filename (sync0-zkn-generate-unique-filename directory))
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
      (sync0-zkn-format-zettel-body filename type subtype name-fixed after-type extra-aliases "" tags)
      (sync0-write-to-file (concat sync0-zkn-zettel-entry "\n\n" works) obsidian-file)
      ;; Add the new file to `obsidian-files-cache` in memory
      (add-to-list 'obsidian-files-cache obsidian-file)
      ;; Update the persistent cache file with the new file path
      (sync0-zkn-save-cache)
      (message "New zettel created for %s added to cache: %s" person-name obsidian-file))))

(defun sync0-zkn-create-multiple-people-fiche ()
  "Create an obsidian markdown fiche for a person in people folder."
  (interactive)
  (let* (x
         (crm-separator  "[ 	]*;[ 	]*")
         (people  (completing-read-multiple "People to crete new md fiches: " sync0-bibtex-completion-author)))
    (dolist (element people x)
      (sync0-zkn-create-people-fiche element)
      (setq x (concat x element "\n")))
    (message "Created md fiches for %s" x)))

(defun sync0-zkn-rename-file ()
  "Rename the file in the current buffer and update the filename in the database.
Operates only if the file is a Markdown file and located within the Zettelkasten directory.
If the file is in the 'references' directory, use `sync0-zkn-generate-ref-filename`
to generate the new filename. Otherwise, use `sync0-zkn-generate-filename`."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (or (not current-file) ;; No file is visited
            (not (string-suffix-p ".md" current-file)) ;; Not a Markdown file
            (not (string-prefix-p (expand-file-name sync0-zkn-dir) ;; Not in Zettelkasten
                                  (expand-file-name current-file))))
        (message "This function only works for Markdown files in the Zettelkasten directory.")
      (let* ((db (sync0-zkn-get-db))
             (current-dir (file-name-directory current-file))
             ;; Fetch existing files in the directory using your function
             (existing-files (sync0-zkn-find-existing-markdown-files current-dir))
             ;; Determine the appropriate filename generator
             (new-filename
              (if (string-prefix-p (expand-file-name sync0-zkn-references-dir)
                                   (expand-file-name current-dir))
                  (sync0-zkn-generate-ref-filename existing-files)
                (sync0-zkn-generate-filename existing-files)))
             ;; Construct the new filepath
             (new-filepath (concat (file-name-as-directory current-dir) new-filename ".md")))
        (if (file-exists-p new-filepath)
            (error "Cannot rename: a file with the same name already exists")
          ;; Rename the file
          (rename-file current-file new-filepath)
          ;; Update the database
          (sqlite-transaction db)
          (sqlite-execute
           db
           (format "UPDATE files SET filename = '%s' WHERE filename = '%s';"
                   (sync0-shorten-filepath new-filepath)
                   (sync0-shorten-filepath current-file)))
          (sqlite-commit db)
          ;; Update the current buffer
          (set-visited-file-name new-filepath)
          (rename-buffer (file-name-nondirectory new-filepath))
          (message "File successfully renamed to: %s" new-filepath))))))

(provide 'sync0-zkn-functions)
