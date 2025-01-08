(require 'sync0-yaml)
(require 'sync0-sql)
(require 'sync0-zettelkasten)
;; (require 'sqlite3)

(defvar sync0-zkn-db-dirty t
  "Function to determine whether it is necessary to update cache in emacs from database.")

(defvar sync0-zkn-db-excluded-dirs '("templates" "journal" "variables" "symlinks" "spreadsheets" "projects" "img" "excalidraw" "mermaid-images" "markwhen" "kanban" "canvas" "banners" "auto" "etc") 
  "List of directories in zkn directory to exclude from database.")

(defvar sync0-zkn-db-cache nil
  "Function to hold cached data from database")

(defvar sync0-zkn-db-path
  (concat sync0-databases-dir "sync0-zkn.sqlite")
  "Path to the SQLite database for managing BibTeX entries.")

(defvar sync0-zkn-db-connection nil
  "Holds the connection to the BibTeX SQLite database.")

(defvar sync0-zkn-db-cache nil
  "Holds the connection to the BibTeX SQLite database.")

(defun sync0-zkn-get-db ()
  "Return a connection to the BibTeX database, initializing if necessary."
  (unless (and sync0-zkn-db-connection
               (sqlitep sync0-zkn-db-connection))
    (setq sync0-zkn-db-connection
          (sqlite-open sync0-zkn-db-path)))
  sync0-zkn-db-connection)

(defun sync0-zkn-db-rollback ()
  (interactive)
  (let ((db (sync0-zkn-get-db)))
    (sqlite-rollback db)))

(defun sync0-vector-to-quoted-csv (vec)
  "Convert a vector to a comma-separated string with each element in double quotes."
  (mapconcat (lambda (x) (format "\"%s\"" x)) vec ", "))

(defun sync0-vector-to-quoted-list (vec)
  "Convert a vector to a list of strings, with each element in double quotes."
  (mapcar (lambda (x) (format "\"%s\"" x)) vec))

(defun sync0-convert-vector-to-list (vec)
  "Convert a vector to a list of strings, with each element in double quotes."
  (mapcar (lambda (x) (format "%s" x)) vec))

(defun sync0-db-extract-title-and-aliases (file-path)
  "Extract the title and aliases from the YAML front matter of FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer 
      (insert-file-contents file-path)
      (let* ((yaml (sync0-parse-yaml-front-matter))
             (title (if yaml (gethash 'title yaml) nil))
             (aliases (if yaml (gethash 'aliases yaml) nil)))
	(when title
	  (list title (if aliases
			  (sync0-convert-vector-to-list aliases) ;; Split aliases into list
			nil)))))))

(defun scan-zkn-dir (dir)
  "Recursively scan DIR for Markdown files and extract title/aliases."
  (let (results)
    (dolist (file (directory-files-recursively dir "\\.md$") results)
      (let ((data (sync0-db-extract-title-and-aliases file)))
        (when (car data)  ; Ensure a title exists
          (push (list file (car data) (cadr data)) results))))))

(defun sync0-shorten-filepath (filepath)
  "Shorten the full title by removing the prefix specified in `sync0-zkn-dir`.
Also ensures the filename structure is alphanumeric with no
spaces before shortening to prevent errors in database."
  ;; Extract the filename from the full path
  (let ((filename (file-name-nondirectory filepath)))
    ;; Check if the filename matches the expected alphanumeric pattern
    (if (string-match-p "^[a-zA-Z0-9_-]+$" (file-name-sans-extension filename))
	(file-relative-name filepath sync0-zkn-dir)
      (error "Filename `%s` does not have the correct alphanumeric structure" filename))))

(defun sync0-zkn-db-insert-into-db (db file title aliases)
  "Insert FILE, TITLE, and ALIASES into the database DB."
  (let ((checksum (sha1 (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
	(short-file (sync0-shorten-filepath file)))
    (sqlite-execute db
                  (format "INSERT OR REPLACE INTO files (filename, title, checksum)
                           VALUES ('%s', '%s', '%s');"
                          short-file
                          (sync0-db-sanitize-value title)
                          checksum))
    (when aliases
      (sqlite-execute db
                    (format "DELETE FROM aliases WHERE file_id = (SELECT id FROM files WHERE filename = '%s');"
                            short-file)))
    (dolist (alias aliases)
      (sqlite-execute db
                    (format "INSERT INTO aliases (file_id, alias)
                             VALUES ((SELECT id FROM files WHERE filename = '%s'), '%s');"
                            short-file
                            (sync0-db-sanitize-value alias))))))

(defun sync0-zkn-db-update-file-if-modified (db file title aliases)
  "Update the database with FILE's TITLE and ALIASES if the file content has changed."
  (let* ((short-file (sync0-shorten-filepath file))
         (new-checksum (sha1 (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))))
         ;; Retrieve the current checksum from the database
         (existing-entry (sqlite-select db
                                        (format "SELECT checksum FROM files WHERE filename = '%s';"
                                                short-file))))
    (if existing-entry
        (let ((stored-checksum (nth 0 (car existing-entry)))) ;; Extract the stored checksum
          (if (string= new-checksum stored-checksum)
              (message "No changes detected for %s, skipping update." short-file)
            (progn
              ;; Update the title and checksum in the database
              (sqlite-execute db
                              (format "UPDATE files
                                       SET title = '%s', checksum = '%s'
                                       WHERE filename = '%s';"
                                      (sync0-db-sanitize-value title)
                                      new-checksum
                                      short-file))
              ;; Update aliases
              (when aliases
                (sqlite-execute db
                                (format "DELETE FROM aliases WHERE file_id = (SELECT id FROM files WHERE filename = '%s');"
                                        short-file))
                (dolist (alias aliases)
                  (sqlite-execute db
                                  (format "INSERT INTO aliases (file_id, alias)
                                           VALUES ((SELECT id FROM files WHERE filename = '%s'), '%s');"
                                          short-file
                                          (sync0-db-sanitize-value alias)))))
              (message "File %s updated in the database." short-file))))
      (progn
        ;; If the file doesn't exist in the database, insert it
        (sqlite-execute db
                        (format "INSERT INTO files (filename, title, checksum)
                                 VALUES ('%s', '%s', '%s');"
                                 short-file
                                (sync0-db-sanitize-value title)
                                new-checksum))
        (when aliases
          (dolist (alias aliases)
            (sqlite-execute db
                            (format "INSERT INTO aliases (file_id, alias)
                                     VALUES ((SELECT id FROM files WHERE filename = '%s'), '%s');"
                                    short-file
                                    (sync0-db-sanitize-value alias)))))
        (message "File %s inserted into the database." short-file)))))

;; (defun sync0-zkn-update-db-from-folders (db folders)
;;   "Update the database DB with files from FOLDERS."
;;   (dolist (folder folders)
;;     (let ((files (directory-files-recursively folder "\\.md$"))) ;; Adjust for desired file types
;;       (dolist (file files)
;;         (let* ((metadata (sync0-db-extract-title-and-aliases file)) ;; Extract title and aliases
;;                (title (car metadata))
;;                (aliases (cadr metadata))) ;; Aliases as a list
;;           (when title ;; Only process files with valid titles
;;             (sync0-zkn-db-update-file-if-modified db file title aliases)))))))

(defun sync0-zkn-update-db ()
  "Update the Zkn database with files from `sync0-zkn-dir`,
excluding directories listed in `sync0-zkn-db-excluded-dirs`."
  (let* ((db (sync0-zkn-get-db)) ;; Retrieve the database
         (zkn-dir (expand-file-name sync0-zkn-dir))
         (files (directory-files-recursively zkn-dir "\\.md$"))) ;; Find all Markdown files
    (dolist (file files)
      (let* ((relative-path (file-relative-name file zkn-dir))
             (excluded? (seq-some (lambda (dir)
                                    (string-prefix-p (expand-file-name dir zkn-dir)
                                                     (expand-file-name file)))
                                  sync0-zkn-db-excluded-dirs)))
        (unless excluded?
          (let* ((metadata (sync0-db-extract-title-and-aliases file)) ;; Extract title and aliases
                 (title (car metadata))
                 (aliases (cadr metadata))) ;; Aliases as a list
            (when title ;; Only process files with valid titles
              (sync0-zkn-db-update-file-if-modified db file title aliases))))))))

;; Example Usage:
;; (sync0-zkn-update-db)

(defun sync0-zkn-after-save-hook ()
  "Update the Zkn database when a file is saved.
Only runs for files in `sync0-zkn-dir` or its subfolders."
  (interactive)
  (let ((db (sync0-zkn-get-db))
        (file-path (buffer-file-name))
        (zkn-dir (expand-file-name sync0-zkn-dir))) ;; Ensure full path for comparison
    (when (and file-path ;; Ensure the buffer is associated with a file
               (string-prefix-p zkn-dir (expand-file-name file-path))
	       (string= (file-name-extension file-path) "md") ;; Check for .md extensi
	       (not (seq-some ;; Ensure file is not in excluded subdirectories
                     (lambda (folder)
                       (string-prefix-p
                        (expand-file-name folder zkn-dir)
                        (expand-file-name file-path)))
		     sync0-zkn-db-excluded-dirs)))
      (let* ((metadata (sync0-db-extract-title-and-aliases file-path))
             (title (car metadata))
             (aliases (cadr metadata)) ;; Aliases as a list
             (checksum (sha1 (with-temp-buffer
                               (insert-file-contents file-path)
                               (buffer-string))))
             ;; Check if file exists in the database
             (existing (sqlite-select
                        db
                        (format "SELECT checksum FROM files WHERE filename = '%s';"
                                 (sync0-shorten-filepath file-path)))))
        (if existing
            ;; File is in database: check checksum
            (let ((db-checksum (caar existing)))
              (unless (string= checksum db-checksum)
		(sqlite-transaction db)
                (sync0-zkn-db-update-file-if-modified db file-path title aliases)
		(sqlite-commit db)
		(setq sync0-zkn-db-dirty t))) ;; Update if checksum differs
          ;; File is not in database: insert it
	  (progn 
	    (sqlite-transaction db)
	    (sync0-zkn-db-insert-into-db db file-path title aliases)
	    (sqlite-commit db)
	    (setq sync0-zkn-db-dirty t)))))))

;; Register the hook
;; (add-hook 'after-save-hook #'sync0-zkn-after-save-hook)

;; (defun update-file-title-or-aliases (db file title aliases)
;;   "Update title and aliases for the given file in the database."
;;   (let ((short-file (sync0-shorten-filepath file)))
;;     ;; Update title if it has changed
;;     (when title
;;       (sqlite-execute db
;;                       (format "UPDATE files SET title = '%s' WHERE filename = '%s';"
;;                               (sync0-db-sanitize-value title)
;;                               (sync0-db-sanitize-value short-file)))
;;     ;; Update aliases
;;     (when aliases
;;       (sqlite-execute db
;;                       (format "DELETE FROM aliases WHERE file_id = (SELECT id FROM files WHERE filename = '%s');"
;;                               (sync0-db-sanitize-value short-file)))
;;       (dolist (alias aliases)
;;         (sqlite-execute db
;;                         (format "INSERT INTO aliases (file_id, alias)
;;                                  VALUES ((SELECT id FROM files WHERE filename = '%s'), '%s');"
;;                                 (sync0-db-sanitize-value short-file)
;;                                 (sync0-db-sanitize-value alias))))))))

(defun sync0-zkn-insert-into-db (file)
  "Insert FILE, TITLE, and ALIASES into the database DB."
  (let* ((db (sync0-zkn-get-db))
	 (checksum (sha1 (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))))
	 (short-file (sync0-shorten-filepath file))
         (data (sync0-db-extract-title-and-aliases file))
	 (title (car data))
	 (aliases (cadr data)))
    (sqlite-transaction db)
    (sqlite-execute db
                    (format "INSERT OR REPLACE INTO files (filename, title, checksum)
                           VALUES ('%s', '%s', '%s');"
                            short-file
                            (sync0-db-sanitize-value title)
                            checksum))
    (when aliases
      (sqlite-execute db
                      (format "DELETE FROM aliases WHERE file_id = (SELECT id FROM files WHERE filename = '%s');"
                              short-file)))
    (dolist (alias aliases)
      (sqlite-execute db
                      (format "INSERT INTO aliases (file_id, alias)
                             VALUES ((SELECT id FROM files WHERE filename = '%s'), '%s');"
                              short-file
                              (sync0-db-sanitize-value alias))))
    (sqlite-commit db)
    (setq sync0-zkn-db-dirty t)))

;; (defun sync0-zkn-db-populate-db (dir)
;;   "Scan DIR and populate the SQLite database at DB-PATH with title
;; and aliases. Function to add unadded folders since some are
;; already present."
;;   (let ((db (sync0-zkn-get-db)))
;;     (sqlite-transaction db)
;;     (unwind-protect
;;         (dolist (entry (scan-zkn-dir dir))
;;           (let ((file (nth 0 entry))
;;                 (title (nth 1 entry))
;;                 (aliases (nth 2 entry)))
;; 	    (when title
;;               (sync0-zkn-db-insert-into-db db file title aliases)))))
;;     (sqlite-commit db)))

(defun sync0-zkn-db-populate-db (dir)
  "Scan DIR and populate the SQLite database with title and aliases.
Skips files already present in the database."
  (let ((db (sync0-zkn-get-db)))
    (unwind-protect
        (dolist (entry (scan-zkn-dir dir))
          (let ((file (nth 0 entry))
                (title (nth 1 entry))
                (aliases (nth 2 entry)))
            ;; Check if the file is already in the database
            (let ((exists (sqlite-select
                           db
                           (format "SELECT 1 FROM files WHERE filename = '%s';"
                                   (sync0-shorten-filepath file)))))
              (if exists
                  (message "File present in database: %s" file)
                (when title
		  (sqlite-transaction db)
                  (sync0-zkn-db-insert-into-db db file title aliases)
		  (sqlite-commit db)))))))))

(defun sync0-zkn-db-rebuild-full-cache ()
  "Rebuild the full cache of Zkn entries with titles and aliases."
  (let* ((db (sync0-zkn-get-db))
         (query "SELECT f.title, f.filename, a.alias
                 FROM files f
                 LEFT JOIN aliases a ON f.id = a.file_id;")
         (raw-results (sqlite-select db query))
         (candidates (mapcar (lambda (row)
                               (let ((title (nth 0 row))
                                     (filename (nth 1 row))
                                     (alias (nth 2 row)))
                                 ;; Use relative path from the database to build display text
                                 (cons (format "%s [%s%s]"
                                               (or alias title)
                                               (file-name-directory filename)  ;; Extract subfolder
                                               (file-name-sans-extension (file-name-nondirectory filename)))  ;; Extract filename without extension
                                       filename)))
                             raw-results)))
    (setq sync0-zkn-db-dirty nil)
    (setq sync0-zkn-db-cache candidates)))

(defun sync0-zkn-db-query (&optional prompt)
  "Query the Zkn database for a markdown note annd returns a list
with title/alias, subfolder, and filename: (TITLE/ALIAS SUBFOLDER
FILENAME). In this list, TITLE/ALIAS is either a title or an
alias as a string. When one or more aliases are defined in the
database, they will have their own tripartite list defined by
this function. SUBFOLDER is the subfolder withinn the ZKN in
which the note is located. FILENAME is the unique filename
without the markdown extension."
  (interactive)
  (let* ((candidates
          (if sync0-zkn-db-dirty
              (sync0-zkn-db-rebuild-full-cache)
            sync0-zkn-db-cache))
         (sorted-candidates candidates)
         (prompt (or prompt "Choose entry: "))
         (selection (completing-read prompt sorted-candidates)))
    (let* ((selected-entry selection)  ;; The selected entry from completing-read
           (title-or-alias (car (split-string selected-entry " \\[")))  ;; Extract the title/alias
           (subfolder-filename (cadr (split-string selected-entry " \\[")))  ;; Extract the subfolder/filename
           (subfolder (car (split-string subfolder-filename "/")))  ;; Extract the subfolder
           (filename (substring (file-name-sans-extension (file-name-nondirectory subfolder-filename)) 0 -1)))  ;; Extract the filename without extension
      (list title-or-alias subfolder filename))))  ;; Return the title/alias, subfolder, and filename

(defun sync0-zkn-visit-file (filename)
  "Visit the Zkn file corresponding to FILENAME."
  (let ((full-path (expand-file-name filename sync0-zkn-dir)))  ;; Use the base directory
    (find-file full-path)))

(defun sync0-zkn-find-file ()
  "Query the Zkn database, display candidates, and visit the selected file."
  (interactive)
  (let* ((file-info (sync0-zkn-db-query))  ;; Query the database for the selected entry
         (subfolder (cadr file-info))        ;; Extract the subfolder
         (filename (caddr file-info)))       ;; Extract the filename
    (sync0-zkn-visit-file (concat subfolder "/" filename ".md"))))  ;; Open the file with the correct path

(defun sync0-zkn-insert-md-link ()
  "Insert a markdown link to the selected entry."
  (interactive)
  (let* ((selected-entry (sync0-zkn-db-query))  ;; Query the database for the selected entry
         (title-or-alias (car selected-entry))  ;; Get the title or alias
         (filename (caddr selected-entry)))     ;; Get the filename
    (insert (format "[%s](%s.md)"
                    title-or-alias  ;; The title or alias
                    filename))))    ;; The filename without extension

;; (global-set-key (kbd "C-c I") 'sync0-zkn-insert-md-link)

;; (evil-leader/set-key-for-mode 'markdown-mode "I" 'sync0-zkn-insert-md-link)

(defun sync0-zkn-follow-link-at-point (&optional arg)
  "Find and follow markdown link at point.
Opens markdown links in other window if ARG is non-nil."
  (interactive "P")
  (let* ((link (markdown-link-url))  ;; Get the link at point (e.g., '2472.md')
	 (db (sync0-zkn-get-db))  ;; Get the database connection
         ;; Query the database for filenames that contain the link (substring search)
         (query (when link
		  (format "SELECT f.filename FROM files f WHERE f.filename LIKE '%%%s%%';"
                          link)))  ;; Use LIKE with wildcards to match the filename
         (result (sqlite-select db query)))  ;; Execute the query
    (if result
        (sync0-zkn-visit-file (caar result))  ;; Visit the file if found
      (message "No file found for the link: %s" link)))) ;; Notify if no file found

(defun sync0-zkn-move-file ()
  "Move a file in the Zkn dir to a new location.
If visiting a file, the user is queried to move that file. Otherwise, query for a target file.
The file will be moved only if it does not overwrite an existing file in the target location."
  (interactive)
  (let* ((db (sync0-zkn-get-db))
         (current-file (if (and (buffer-file-name)
                                (string-prefix-p (expand-file-name sync0-zkn-dir)
                                                 (expand-file-name (buffer-file-name))))
                           (buffer-file-name)
                         (let ((result (sync0-zkn-db-query "Select file to move: ")))
                           (when result
                             (expand-file-name (concat sync0-zkn-dir (cadr result) "/" (caddr result) ".md"))))))
	 (target-dir (completing-read
		      "Select target directory: "
		      (cl-remove-if
		       (lambda (dir)
			 ;; Exclude non-directories and directories in the excluded list
			 (or (not (file-directory-p dir)) ;; Exclude non-directories
			     (member (file-name-nondirectory (directory-file-name dir)) 
				     sync0-zkn-db-excluded-dirs)))
		       (directory-files sync0-zkn-dir t "^[^.]" t))
		      nil t))
         (target-path (concat (file-name-as-directory target-dir) (file-name-nondirectory current-file))))
    (if (not current-file)
        (message "No file selected or found.")
      (if (file-exists-p target-path)
          (error "Cannot move: a file with the same name already exists in the target directory")
        ;; Move the file
        (rename-file current-file target-path)
        ;; Update the database
	(sqlite-transaction db)
        (sqlite-execute db
                        (format "UPDATE files SET filename = '%s' WHERE filename = '%s';"
                                 (sync0-shorten-filepath target-path)
                                 (sync0-shorten-filepath current-file)))

	(sqlite-commit db)
        ;; Update the current buffer if visiting the moved file
        (when (string= (buffer-file-name) current-file)
          (set-visited-file-name target-path)
          (rename-buffer (file-name-nondirectory target-path)))
        (message "File successfully moved to: %s" target-path)))))

;; untested as of 2024-11-28
(defun sync0-zkn-note-refactor ()
  "Merge two notes in the Zkn system.
The content of the selected note is appended to the current buffer's note or the first note selected.
The database entry for the merged note is removed."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (db (sync0-zkn-get-db))
         (target-note
          (if (and current-file
                   (string-prefix-p (expand-file-name sync0-zkn-dir) current-file))
              (progn
                (when (yes-or-no-p "Merge the note visited in this buffer?")
                  current-file))
            ;; If not visiting a file, let the user choose the first note
            (let* ((file-info (sync0-zkn-db-query "Select the first note to merge: "))
                   (file-path (expand-file-name (caddr file-info) sync0-zkn-dir)))
              (concat file-path ".md")))))
    (if target-note
        (let* ((second-note-info (sync0-zkn-db-query "Select the second note to merge: "))
               (second-note-path (concat (expand-file-name (caddr second-note-info) sync0-zkn-dir) ".md")))
          ;; Verify the second note exists
          (if (not (file-exists-p second-note-path))
              (error "Second note not found: %s" second-note-path)
            ;; Append content of the second note to the first note
            (with-current-buffer (find-file-noselect target-note)
              (goto-char (point-max))
              (insert "\n\n---\n\n") ;; Optional separator
              (insert-file-contents second-note-path)
              (save-buffer))
            ;; Remove the second note from the database
	    (sqlite-transaction db)
            (sqlite-execute db
                            (format "DELETE FROM files WHERE filename = '%s';"
                                    (sync0-shorten-filepath second-note-path)))
            ;; Delete the second note file
	    (sqlite-commit db)
            (delete-file second-note-path)
            (message "Notes merged: %s into %s" (file-name-nondirectory second-note-path) (file-name-nondirectory target-note))))
      (message "No target note selected for merging."))))


(provide 'sync0-zkn)
