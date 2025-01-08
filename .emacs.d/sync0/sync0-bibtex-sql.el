(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-sql-utils)
(require 'sync0-bibtex-entry-functions)

(defun sync0-bibtex-db-retrieve-bibkeys (db)
  "Retrieve all BibTeX keys currently in the database."
  (let* ((query "SELECT citekey FROM entries")
         (result (sqlite-select db query)))
    ;; Extract the keys as a flat list of strings
    (mapcar (lambda (row) (car row)) result)))

(defun sync0-bibtex-db-import-bibkey-validate-field (field-value field-name)
  "Validate a FIELD-VALUE for invalid characters.
FIELD-NAME is used for error messages. Throws an error if FIELD-VALUE contains single or double quotes."
  (when (and field-value (string-match-p "['\"]" field-value))
    (error "The field '%s' contains invalid characters: \"%s\". Manual correction required."
           field-name field-value)))

(defun sync0-bibtex-db-import-bibkey-validate-value (value)
  "Validate a VALUE for invalid characters.
Throws an error if VALUE contains single or double quotes."
  (when (string-match-p "'['\"]'" value)
    (error "Manual correction required: \"%s\"" value)))

(defun sync0-bibtex-db-get-citekeys-for-person (person db)
  "Retrieve and display the citekeys associated with AUTHOR in the entry_people table."
  (let* ((person-id (sync0-bibtex-db-get-person-id person))
        (query (format "SELECT citekey FROM entry_people WHERE person_id = %d;" person-id)))
    (if-let ((result (sqlite-select db query)))
            (message "Citekeys for person %s: %s" person result)
        (message "No entries found for person %s." person))))


(defun sync0-bibtex-db-get-first-20-people (db)
  "Retrieve the first 20 people from the people table in the database DB."
  (let ((query "SELECT person_id, person_name FROM people LIMIT 20;"))
    (sqlite-select db query)))

(defun sync0-bibtex-db-import-bibkey-into-db (citekey db)
  "Dummy version of the function that only prints the SQL commands it would execute."
  ;; Load the selected BibTeX entry (dummy)
  (sync0-bibtex-completion-load-entry citekey)
  (let* ((type sync0-bibtex-entry-type)
         (title (sync0-bibtex-db-sanitize-value sync0-bibtex-entry-title))
         ;; New fields for subtitle, date, and origdate
         (subtitle (sync0-bibtex-db-sanitize-value sync0-bibtex-entry-subtitle))
         (date (sync0-bibtex-db-sanitize-value sync0-bibtex-entry-date))
         (origdate (sync0-bibtex-db-sanitize-value sync0-bibtex-entry-origdate))
         ;; Main fields (citekey, type, title, subtitle, date, origdate)
         (main-fields sync0-bibtex-db-main-fields)
         (main-values `(,citekey ,type ,title))
	 (extra-main-fields
	  `(("subtitle" . ,subtitle)
            ("date" . ,date)
            ("origdate" . ,origdate))))

    ;; Check if subtitle, date, or origdate are defined and add them to main fields and values
    (dolist (field extra-main-fields)
      (let ((value (cdr field)))
	(when value
	  (push (car field) main-fields)
	  (push value main-values))))

    ;; Wrap in a transaction
    (sqlite-transaction db)
    (unwind-protect
        (progn
          ;; Insert main entry
          (let ((insert-query
		 (format "INSERT INTO entries (%s) VALUES (%s);"
			 (mapconcat 'identity main-fields ", ")
			 (mapconcat (lambda (v) (format "'%s'" v)) main-values ", "))))
            (sqlite-execute db insert-query))
	  
	  ;; **Insert People**
	  (sync0-bibtex-db-insert-people citekey)
	  ;; **Insert Keywords**
	  (sync0-bibtex-db-insert-keywords citekey)
	  ;; **Insert Extra fields**
          (sync0-bibtex-db-insert-extra-fields citekey db))
      (sqlite-commit db)
      (setq sync0-bibtex-db-dirty t))
      (sync0-bibtex-nullify-all-variables)))

(defun sync0-bibtex-db-import-single-bibkey ()
  "Import single bibkey into my database."
  (interactive)
  (let ((db (sync0-bibtex-db-get-database))
	(bibkey (sync0-bibtex-choose-key)))
    (sync0-bibtex-db-import-bibkey-into-db bibkey db)))

(defun sync0-bibtex-db-import-bibkey-at-point ()
  "Import single bibkey into my database."
  (interactive)
  (let* ((db (sync0-bibtex-db-get-database))
	 (entry (save-excursion (bibtex-beginning-of-entry)
                                (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry))))
    (sync0-bibtex-db-import-bibkey-into-db bibkey db)))

(defun sync0-bibtex-db-rebuild-full-cache ()
  (let* ((db (sqlite-open sync0-bibtex-database-path))
         ;; Updated query
         (query "SELECT 
                  e.citekey,
                  COALESCE(p.person_name, 'No Author') AS author,
                  e.title,
                  e.subtitle,
                  e.origdate,
                  e.date,
                  e.type
                 FROM entries e
                 LEFT JOIN entry_people ep 
                   ON e.citekey = ep.citekey AND ep.role IN ('author', 'editor')
                 LEFT JOIN people p 
                   ON ep.person_id = p.person_id;")
         (raw-results (sqlite-select db query))
         ;; Format candidates as "Author [origdate] (date) Title: Subtitle [type:@citekey]"
         (candidates (mapcar (lambda (row)
                               (let ((citekey (nth 0 row))
                                     (author (nth 1 row))
                                     (title (nth 2 row))
                                     (subtitle (nth 3 row))
                                     (origdate (nth 4 row))
                                     (date (nth 5 row))
                                     (type (nth 6 row)))
                                 (cons (format "%s%s%s %s%s [%s:@%s]"
                                               author
                                               (if origdate (format " [%s]" origdate) "")
                                               (if date (format " (%s)" date) "")
                                               title
                                               (if subtitle (format ": %s" subtitle) "")
                                               type
                                               citekey)
                                       citekey)))
                             raw-results)))
    (setq sync0-bibtex-db-dirty nil)
    (setq sync0-bibtex-db-cache candidates)))

(defun sync0-bibtex-db-query-entry ()
  "Query the database for BibTeX entries and return formatted candidates."
  (interactive)
  (let* ((candidates
	  (if sync0-bibtex-db-dirty 
	      (sync0-bibtex-db-rebuild-full-cache)
	    sync0-bibtex-db-cache))
         (cached-key sync0-bibtex-completion-cache-key)
	 (preselected-candidate (and cached-key
                                     (cl-find-if
                                      (lambda (cand)
					(string= cached-key (cdr cand)))
                                      candidates)))
	 (sorted-candidates (if preselected-candidate
				(cons preselected-candidate
                                      (remove preselected-candidate candidates))
                              candidates))
	 (selection (completing-read "Choose entry: " sorted-candidates)))
    (message "%s" (cdr (assoc selection candidates)))
    (cdr (assoc selection candidates))))

;; (defun sync0-bibtex-db-query-entry ()
;;   "Query the database for BibTeX entries and return formatted candidates."
;;   (interactive)
;;   (let* ((db (sqlite-open sync0-bibtex-database-path))
;;          ;; Query adjusted for the new schema
;;          (query "SELECT 
;;                   e.citekey,
;;                   COALESCE(p.person_name, 'No Author') AS author,
;;                   e.title,
;;                   e.subtitle,
;;                   e.origdate,
;;                   e.date,
;;                   e.type
;;                  FROM entries e
;;                  LEFT JOIN entry_people ep ON e.citekey = ep.citekey
;;                  LEFT JOIN people p ON ep.person_id = p.person_id;")
;;          (raw-results (sqlite-select db query))
;;          ;; Format candidates as "Author [origdate] (date) Title: Subtitle [type:@citekey]"
;;          (candidates (mapcar (lambda (row)
;;                                (let ((citekey (nth 0 row))
;;                                      (author (nth 1 row))
;;                                      (title (nth 2 row))
;;                                      (subtitle (nth 3 row))
;;                                      (origdate (nth 4 row))
;;                                      (date (nth 5 row))
;;                                      (type (nth 6 row)))
;;                                  (cons (format "%s%s%s %s%s [%s:@%s]"
;;                                                author
;;                                                (if origdate (format " [%s]" origdate) "")
;;                                                (if date (format " (%s)" date) "")
;;                                                title
;;                                                (if subtitle (format ": %s" subtitle) "")
;;                                                type
;;                                                citekey)
;;                                        citekey)))
;;                              raw-results))
;;          (cached-key sync0-bibtex-completion-cache-key)
;;          (preselect (and cached-key
;;                          (cl-position-if
;;                           (lambda (cand)
;;                             (string= cached-key (cdr cand)))
;;                           candidates)))
;;          (selection (ivy-read "Choose entry: " candidates
;;                         :preselect preselect)))
;;     (message "%s" (cdr (assoc selection candidates)))
;;     (cdr (assoc selection candidates))))

;; (defun sync0-bibtex-db-query-entry ()
;;   (interactive)
;;   (let* ((db (sqlite-open sync0-bibtex-database-path))
;; 	 (query (format "select concat (
;;                         COALESCE(author, 'No Author'), 
;;                         CASE 
;;                           WHEN origdate IS NOT NULL THEN 
;;                             concat(' [', origdate, ']')
;;                           ELSE 
;;                             ''
;;                         END, 
;;                         CASE 
;;                           WHEN date IS NOT NULL THEN 
;;                             concat(' (', date, ')')
;;                           ELSE 
;;                             ''
;;                         END, 
;;                         ' ', 
;;                         title, 
;;                         CASE 
;;                           WHEN subtitle IS NOT NULL THEN 
;;                             concat(': ', subtitle)
;;                           ELSE 
;;                             ''
;;                         END, 
;;                         concat(' [', type, ':@', citekey, ']')
;;                         ) from entries"))
;; 	 (candidates (sqlite-select db query))
;; 	 (selection (ivy-read (format "Choose entry: ")
;; 			      candidates)))
;;     (message "%s" selection)))

;; (defun sync0-bibtex-init-database ()
;;   "Initialize the SQLite database for the bibliography, if it doesn't exist."
;;   (unless (file-exists-p sync0-bibtex-database-path)
;;     (let ((db (emacdb-sqlite sync0-bibtex-database-path)))
;;       (condition-case err
;;           (progn
;;             ;; Check if the 'entries' table already exists
;;             (unless (emacsql db "SELECT name FROM sqlite_master WHERE type='table' AND name='entries';")
;;               (emacsql db
;;                        "CREATE TABLE entries (
;;                            citekey TEXT PRIMARY KEY,
;;                            type TEXT NOT NULL,
;;                            title TEXT NOT NULL,
;;                            subtitle TEXT,
;;                            author TEXT,
;;                            editor TEXT,
;;                            date TEXT,
;;                            origdate TEXT
;;                         );"))

;;             ;; Check if the 'custom_fields' table already exists
;;             (unless (emacsql db "SELECT name FROM sqlite_master WHERE type='table' AND name='custom_fields';")
;;               (emacsql db
;;                        "CREATE TABLE custom_fields (
;;                            citekey TEXT NOT NULL,
;;                            field TEXT NOT NULL,
;;                            value TEXT NOT NULL,
;;                            PRIMARY KEY (citekey, field),
;;                            FOREIGN KEY (citekey) REFERENCES entries (citekey) ON DELETE CASCADE
;;                         );"))

;;             ;; Create indexes if they don't exist
;;             (unless (emacsql db "SELECT name FROM sqlite_master WHERE type='index' AND name='idx_custom_fields_field';")
;;               (emacsql db
;;                        "CREATE INDEX idx_custom_fields_field ON custom_fields (field);"))
;;             (unless (emacsql db "SELECT name FROM sqlite_master WHERE type='index' AND name='idx_custom_fields_value';")
;;               (emacsql db
;;                        "CREATE INDEX idx_custom_fields_value ON custom_fields (value);"))
            
;;             (emacdb-close db)
;;             (message "Initialized BibTeX database."))
;;         (error (message "Error initializing BibTeX database: %s" (error-message-string err)))))))

(defun my-db-get-entry (bibkey)
  "Retrieve a BibTeX entry from the SQL database as an alist for the given BIBKEY, including dynamic extra fields."
  (let ((db-connection (my-sql-connect)))  ;; Replace with your actual DB connection function
    (let* ((entry-result (my-sql-query db-connection
                                       "SELECT title, subtitle, type, date, origdate 
                                        FROM entries 
                                        WHERE citekey = ?"
                                       bibkey))
           (entry (car entry-result))
           (people-result (my-sql-query db-connection
                                        "SELECT p.person_name, ep.role
                                         FROM entry_people ep
                                         JOIN people p ON ep.person_id = p.person_id
                                         WHERE ep.citekey = ?"
                                        bibkey))
           (keywords-result (my-sql-query db-connection
                                          "SELECT k.keyword
                                           FROM entry_keywords ek
                                           JOIN keywords k ON ek.keyword_id = k.keyword_id
                                           WHERE ek.citekey = ?"
                                          bibkey))
           (extra-fields-result (my-sql-query db-connection
                                              "SELECT * FROM extra_fields
                                               WHERE citekey = ?"
                                              bibkey)))
      (if (null entry)
          (error "Entry not found")
        (let* ((alist `(("author" . ,(mapconcat (lambda (person) (concat (cdr person))) people-result ", "))
                        ("title" . ,(cdr (assoc "title" entry)))
                        ("subtitle" . ,(cdr (assoc "subtitle" entry)))
                        ("type" . ,(cdr (assoc "type" entry)))
                        ("date" . ,(cdr (assoc "date" entry)))
                        ("origdate" . ,(cdr (assoc "origdate" entry)))
                        ("keywords" . ,(mapconcat (lambda (keyword) (cdr keyword)) keywords-result ", "))))
               (extra-fields-alist nil)) ;; To store dynamic extra fields
          
          ;; Dynamically add extra fields to the alist based on what is available
          (dolist (field '("titleaddon" "origtitle" "eventtitle" "eventdate" "journaltitle"
                           "edition" "booktitle" "booksubtitle" "crossref" "chapter" "volume"
                           "volumes" "number" "series" "publisher" "location" "pages" "note" "doi"
                           "url" "urldate" "language" "origlanguage" "medium" "institution" "library"
                           "related" "relatedtype" "relatedstring" "file" "created" "password"
                           "shorttitle" "doctype" "shorthand" "description" "pagetotal" "verba"
                           "cote" "project" "site" "version" "country" "lecture" "seminar" "currency"
                           "podcast" "source" "status" "expages" "aliases" "scanstatus" "issuetitle"
                           "priority" "scheduled" "deadline" "lastseen" "format" "bookloan" "pubstate"
                           "columns" "section" "paragraphs" "verses" "lines" "course" "origpublisher"
                           "issuedate" "expirydate" "csl" "conference" "languages"))
            (let ((value (cdr (assoc field extra-fields-result))))
              (when value
                (push (cons field value) extra-fields-alist))))
          
          ;; Add the dynamically built extra fields to the main alist
          (setq alist (append alist `(("extra_fields" . ,extra-fields-alist))))
          alist)))))

(defun sync0-bibtex-db-update-main-fields (citekey)
  "Update the fields in the entries table of my SQL database for the
given CITEKEY."
  (let* ((main-fields '("type" "title" "subtitle" "date" "origdate")) ;; Replace or extend with actual main fields
         (updated-fields '())) ;; Track main updated fields
    (dolist (field main-fields)
      (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
             (new-value (symbol-value field-symbol))
             (existing-value (sync0-bibtex-db-get-value citekey field)))
        (cond
         ;; Case 1: New value is non-nil and different from existing value
         ((and new-value (not (equal new-value existing-value)))
          (push field updated-fields)
          (sync0-bibtex-db-execute
           (format "UPDATE entries SET %s = ? WHERE citekey = ?" field) new-value citekey))
         ;; Case 2: New value is nil, but the existing value is not NULL
         ((and (not new-value) existing-value)
          (push field updated-fields)
          (sync0-bibtex-db-execute
           (format "UPDATE entries SET %s = NULL WHERE citekey = ?" field) citekey)))))
    (message "Updated fields: %s"
             (mapconcat 'identity updated-fields ", "))))

(defun sync0-bibtex-db-update-extra-fields (citekey db)
  "Update extra fields for the entry identified by CITEKEY in the database.
This function checks and updates fields in the `extra_fields` table,
adding missing columns as needed, and setting NULL for `nil` values."
  (let ((extra-fields (cons "bibentry" sync0-bibtex-db-purged-extra-fields)) ;; Extra fields from your configuration
        (updated-extra-fields '()) ;; Track updated fields
        (columns (mapcar #'cadr
                         (sync0-bibtex-db-query "PRAGMA table_info(extra_fields);")))) ;; Get current columns
    ;; Process each extra field
    (dolist (field extra-fields)
      (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
             (raw-value (symbol-value field-symbol))
             (new-value (when raw-value
                          (if (string= field "aliases")
                              raw-value
                            (sync0-bibtex-db-sanitize-value raw-value))))
             (existing-value (when (member field columns)
			       (sync0-bibtex-db-get-value citekey field))))
        ;; Add missing columns
        (unless (sync0-bibtex-db-column-exists-p field "extra_fields")
          (sync0-bibtex-db-add-column field db))
        ;; Update or insert values
        (cond
         ;; Case 1: Field has a new value and it differs from the existing value
         ((and new-value existing-value (not (equal new-value existing-value)))
          (sync0-bibtex-db-execute
           (format "UPDATE extra_fields SET %s = ? WHERE citekey = ?" field) new-value citekey)
          (push field updated-extra-fields))
         ;; Case 2: Field has a new value but no existing value
         ((and new-value (not existing-value))
          (sync0-bibtex-db-execute
           (format "INSERT OR REPLACE INTO extra_fields (citekey, %s) VALUES (?, ?)" field) citekey new-value)
          (push field updated-extra-fields))
         ;; Case 3: Field has no new value, but an existing value needs to be set to NULL
         ((and (not new-value) existing-value)
          (sync0-bibtex-db-execute
           (format "UPDATE extra_fields SET %s = NULL WHERE citekey = ?" field) citekey)
          (push field updated-extra-fields)))))
    (message "Updated extra fields: %s"
             (mapconcat 'identity updated-extra-fields ", "))))

(defun sync0-bibtex-update-bibkey-in-db (citekey db)
  "Update the database DB with the BibTeX entry identified by CITEKEY.
Compares values in the database and performs actual SQL updates or inserts."
  (sync0-bibtex-completion-load-entry citekey)
  (sqlite-transaction db)
  (unwind-protect
      (progn
	;; **Main Fields Update**
        (sync0-bibtex-db-update-main-fields citekey)
	;; **Extra Fields Update**
        (sync0-bibtex-db-update-extra-fields citekey db)
	;; **People Fields Update**
        (sync0-bibtex-db-update-people-roles citekey)
	;; **Keyword Fields Update**
        (sync0-bibtex-db-update-keywords citekey)
        ;; First, get the list of existing columns in extra_fields
	(sqlite-commit db)
	(setq sync0-bibtex-db-dirty t)))
      (sync0-bibtex-nullify-all-variables))

    ;; **People and Keywords Update** (Optional: Implement actual logic here if needed)
    ;; (when (not (null (symbol-value 'sync0-bibtex-entry-people)))
    ;;   (sync0-bibtex-update-people-in-db citekey db))
    ;; (when (not (null (symbol-value 'sync0-bibtex-entry-keywords)))
    ;;   (sync0-bibtex-update-keywords-in-db citekey db))


;; (defun sync0-bibtex-db-update-entry-at-point ()
;;   "Update the BibTeX entry at point in the database DB."
;;   (interactive)
;;   (sync0-bibtex-load-entry-at-point)
;;   (let ((db (sync0-bibtex-db-get-database))
;; 	(bibkey sync0-bibtex-entry-key))
;;     (when (yes-or-no-p (format "Import bibkey %s into database? " bibkey))
;;       ;; Call the core update function
;;       (sync0-bibtex-dummy-update-entry-in-db bibkey db))))

(defun sync0-bibtex-db-update-or-import-bibkey-at-point ()
  "Update or import the BibTeX entry at point into the database DB.
If the entry doesn't exist, it will be imported; if it exists, it will be updated."
  (interactive)
  (sync0-bibtex-load-entry-at-point)
  (let ((db (sync0-bibtex-db-get-database))
        (bibkey sync0-bibtex-entry-key))
    (if (= 0 (caar (sqlite-select db
                                  "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list bibkey))))
        (when (yes-or-no-p (format "Citekey %s not present in database. Import?" bibkey))
          ;; Entry not found, import it
          (sync0-bibtex-db-import-bibkey-into-db bibkey db))
      (when (yes-or-no-p (format "Bibkey %s found in database. Update entry? " bibkey))
        ;; Entry found, update it
        (sync0-bibtex-update-bibkey-in-db bibkey db)))))

(defun sync0-bibtex-db-get-bibentry (citekey db)
  "Query the database DB for the value of bibentry in the extra_fields table for the given CITEKEY."
  (let ((result (sqlite-select db
                               "SELECT bibentry FROM extra_fields WHERE citekey = ?;"
                               (list citekey))))
    (if result
        (message "The value of bibentry for citekey %s is:\n%s"
                 citekey (car (car result)))
      (message "No bibentry found for citekey %s." citekey))))

(defun sync0-bibtex-db-query-bibentry ()
  "Query the database DB for the value of bibentry in the extra_fields table for the given CITEKEY."
  (interactive)
  (let ((key (sync0-bibtex-db-query-entry))
	(db (sqlite-open sync0-bibtex-database-path)))
    (sync0-bibtex-db-get-bibentry key db)))

(defun sync0-bibtex-db-update-or-import-bibkey (bibkey db)
  "Update or import the BibTeX entry at point into the database DB.
If the entry doesn't exist, it will be imported; if it exists, it will be updated."
  (if (= 0 (caar (sqlite-select db
                                "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list bibkey))))
      ;; Entry not found, import it
      (sync0-bibtex-db-import-bibkey-into-db bibkey db)
    ;; Entry found, update it
    (sync0-bibtex-update-bibkey-in-db bibkey db)))

(defun sync0-bibtex-db-bulk-import-into-db (file-path)
  "Process a BibLaTeX file at FILE-PATH, generating SQL commands for all keys.
Skip keys that are already present in the database."
  (interactive "fSelect BibTeX file: ")
  (let* ((db (sync0-bibtex-db-get-database))
	 (existing-keys (sync0-bibtex-db-retrieve-bibkeys db)))
    (with-temp-buffer
      ;; Load the BibTeX file into the buffer
      (insert-file-contents file-path)
      ;; Extract all BibTeX keys (assuming standard @type{key,} format)
      (let ((keys ()))
        (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
          (push (match-string 1) keys))
        (let ((successes '())
              (updated '())
              (failures '()))
          (dolist (key (reverse keys))
            (if (member key existing-keys)
                (progn
                  (sync0-bibtex-update-bibkey-in-db key db)
                  (message "Key '%s' is already in the database. Updated." key)
                  (push key updated))
              (condition-case err
                  (progn
                    ;; Call foo and print the command
                    (sync0-bibtex-db-import-bibkey-into-db key db)
                    (push key successes))
                (error
                 ;; Log failure and error message
                 (message "Failed to process key: %s. Error: %s" key err)
                 (push key failures)))))
          ;; Summary of results
          (message "Processing complete: %d successful, %d updated, %d failed."
                   (length successes) (length updated) (length failures))
          (when updated
            (message "Updated keys: %s" (string-join updated ", ")))
          (when failures
            (message "Failed keys: %s" (string-join failures ", "))))))))

(defun sync0-bibtex-db-bulk-import-current-bibfile ()
  "Check if the current buffer is a BibLaTeX file. If so, call `sync0-bibtex-db-bulk-import-into-db`."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (if (and file-path
             (or (string-match-p "\\.bib\\'" file-path) ;; Match .bib extension
                 (save-excursion
                   ;; Check if the buffer contains the @string or @article, etc.
                   (goto-char (point-min))
                   (re-search-forward "@\\w+{" nil t))))
        (progn
          (message "This is a BibLaTeX file, starting import.")
          (sync0-bibtex-db-bulk-import-into-db file-path))
      (message "The current file is not a BibLaTeX file."))))

;; (defun sync0-bibtex-db-get-value (citekey field)
;;   "Retrieve the value of FIELD for the entry identified by CITEKEY in the database.
;; FIELD is first checked in the `entries` table, and if not found,
;; it is looked up in the `extra_fields` table."
;;   (let ((query-main (format "SELECT %s FROM entries WHERE citekey = ?" (symbol-name field)))
;;         (query-extra "SELECT value FROM extra_fields WHERE citekey = ? AND field = ?"))
;;     (or (sync0-bibtex-db-query query-main citekey)
;;         (sync0-bibtex-db-query query-extra citekey (symbol-name field)))))

(defun sync0-bibtex-db-choose-attachment (bibkey &optional extension)
  "Choose an attachment associated with BIBKEY. Optionally filter by
EXTENSION."
  (let* ((file-field (sync0-bibtex-db-get-value bibkey "file"))
         (attachments (when file-field
			(mapcar (lambda (attachment)
				  ;; Handle cases with leading colons and split properly
				  (let* ((cleaned-attachment (string-remove-prefix ":" attachment)) ;; Remove leading colon
					 (path (car (split-string cleaned-attachment ":"))))          ;; Split by colon
				    (string-trim path)))                                             ;; Trim any excess spaces
				;; Split on semicolons, ignoring empty strings
				(split-string file-field ";" t)))))
    (cond
     ((null attachments)
      (error "No attachments found for entry %s" bibkey))
     ((= (length attachments) 1)
      (car attachments))  ; Return the only attachment if there's just one
     (t
      (completing-read "Select an attachment: " attachments)))))

(defun sync0-bibtex-db-generate-markdown-list (author-name start-year end-year)
  "Generate a markdown list of entries by AUTHOR-NAME within the date range from START-YEAR to END-YEAR."
  (let* ((start-date (format "%s-01-01" start-year))  ;; Full start date
         (end-date (format "%s-12-31" end-year))      ;; Full end date
         (query (format "
SELECT e.citekey, e.type, e.title, e.subtitle, e.date
FROM entries e
JOIN entry_people ep ON e.citekey = ep.citekey
JOIN people p ON ep.person_id = p.person_id
WHERE (e.type = 'InCollection' OR e.type = 'Article')
  AND p.person_name = '%s'
  AND ep.role = 'author'
  AND (
        (e.date >= '%s' AND e.date <= '%s')  -- Full date range
     OR (e.date >= '%s' AND e.date <= '%s')    -- Year-Month range
     OR (e.date >= '%s' AND e.date <= '%s')    -- Year range
  )
ORDER BY e.date ASC;" author-name start-date end-date
(format "%s-01" start-year) (format "%s-12" end-year)
start-year end-year))
         (entries (sync0-bibtex-db-query query)))  ;; Function to run the query and return the result
    ;; Debugging output to check entries structure
    (message "Entries: %s" entries)  ;; Print the result to *Messages* buffer for inspection
    (if (not entries)
	"No entries found."
      (let (x)
	(dolist (entry entries x)
	  (when-let ((citekey (nth 0 entry))    ;; Citekey is the 1st element
		     (title (nth 2 entry))     ;; Title is the 3rd element
		     (date (nth 4 entry)))     ;; Date is the 5th element
            (push (format "[(%s) *%s*](%s.md)" date title citekey) x)))
	(sync0-show-elements-of-list x "\n")))))

(provide 'sync0-bibtex-sql)

