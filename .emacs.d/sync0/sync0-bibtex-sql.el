;; (require 'sqlite-mode)
(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-sql-utils)
(require 'sync0-bibtex-entry-functions)

(defun sync0-bibtex-sql-get-bibentry (citekey db)
  "Query the database DB for the value of bibentry in the extra_fields table for the given CITEKEY."
  (let ((result (sqlite-select db
                               "SELECT bibentry FROM extra_fields WHERE citekey = ?;"
                               (list citekey))))
    (if result
        (message "%s" (car (car result)))
      (message "No bibentry found for citekey %s." citekey))))

(defun sync0-bibtex-sql-get-database ()
  "Return a connection to the BibTeX database, initializing if necessary."
  (unless (and sync0-bibtex-db-connection
               (sqlitep sync0-bibtex-db-connection))
    (setq sync0-bibtex-db-connection
          (sqlite-open sync0-bibtex-database-path)))
  sync0-bibtex-db-connection)

(defun sync0-bibtex-sql-retrieve-bibkeys (db)
  "Retrieve all BibTeX keys currently in the database."
  (let* ((query "SELECT citekey FROM entries")
         (result (sqlite-select db query)))
    ;; Extract the keys as a flat list of strings
    (mapcar (lambda (row) (car row)) result)))

(defun sync0-bibtex-sql-import-bibkey-validate-field (field-value field-name)
  "Validate a FIELD-VALUE for invalid characters.
FIELD-NAME is used for error messages. Throws an error if FIELD-VALUE contains single or double quotes."
  (when (and field-value (string-match-p "['\"]" field-value))
    (error "The field '%s' contains invalid characters: \"%s\". Manual correction required."
           field-name field-value)))

(defun sync0-bibtex-sql-import-bibkey-validate-value (value)
  "Validate a VALUE for invalid characters.
Throws an error if VALUE contains single or double quotes."
  (when (string-match-p "'['\"]'" value)
    (error "Manual correction required: \"%s\"" value)))

(defun sync0-bibtex-sql-get-person-id (person db)
  "Retrieve the PERSON_ID for a given PERSON name from the database DB.
If the person is not found, insert the person into the database and return the new ID."
  (let* ((query (format "SELECT person_id FROM people WHERE person_name = '%s';" person))
	 (result (or (sqlite-select db query)
		     (progn
		       ;; Insert the person if not found
		       (sqlite-execute db (format "INSERT INTO people (person_name) VALUES ('%s');" person))
		       ;; Retrieve the new ID
		       (sqlite-select db query)))))
    (caar result)))

(defun sync0-bibtex-sql-rollback ()
  (interactive)
  (let ((db (sync0-bibtex-sql-get-database)))
    (sqlite-rollback db)))

;;   (let ((db (sync0-bibtex-sql-get-database)))
;; (sync0-bibtex-sql-add-authors db sync0-bibtex-completion-author))

;; (defun sync0-bibtex-sql-add-column-if-not-exists (db column-name column-type)
;;   "Add a new COLUMN-NAME with type COLUMN-TYPE to the extra_fields table in DB if it doesn't already exist."
;;   (let* ((check-query (format
;;                        "SELECT COUNT(*) 
;;                         FROM pragma_table_info('extra_fields') 
;;                         WHERE name = '%s';" column-name))
;;          (exists (caar (sqlite-select db check-query))))
;;     (unless (> exists 0) ;; Column does not exist
;;       (sqlite-execute db (format "ALTER TABLE extra_fields ADD COLUMN %s %s;"
;;                                   column-name column-type)))))

(defun sync0-bibtex-sql-column-exists-p (column-name db)
  (let* ((check-query (format
                       "SELECT COUNT(*) 
                        FROM pragma_table_info('extra_fields') 
                        WHERE name = '%s';" column-name))
         (exists (caar (sqlite-select db check-query))))
    (> exists 0)))

(defun sync0-bibtex-sql-add-column (column-name db)
  "Add a new COLUMN-NAME to the extra_fields table in DB, with user-defined column type.
The column type is selected interactively via completing-read. Checks if the column already exists first."
  (let* ((data-types '("TEXT" "INTEGER" "REAL" "BLOB" "NUMERIC"))
         (column-type (completing-read
                       (format "Choose a type for column '%s': " column-name)
                       data-types nil t)))
    (sqlite-execute db (format "ALTER TABLE extra_fields ADD COLUMN %s %s;"
                               column-name column-type))))

(defun sync0-bibtex-sql-add-authors (authors db)
  "Add a list of AUTHORS to the people table in the database DB.
If an author already exists, retrieve their person_id, otherwise insert them."
  (dolist (author authors)
    (sync0-bibtex-sql-get-person-id author db)))

(defun sync0-bibtex-sql-get-keyword-id (keyword db)
  "Retrieve the KEYWORD_ID for a given KEYWORD from the database DB.
If the keyword is not found, insert it into the database and return the new ID."
  (let* ((query (format "SELECT keyword_id FROM keywords WHERE keyword = '%s';" keyword))
	 (result (or (sqlite-select db query)
		     (progn
		       ;; Insert the keyword if not found
		       (sqlite-execute db (format "INSERT INTO keywords (keyword) VALUES ('%s');" keyword))
		       ;; Retrieve the new ID
		       (sqlite-select db query)))))
    (caar result)))

(defun sync0-bibtex-sql-get-citekeys-for-person (person db)
  "Retrieve and display the citekeys associated with AUTHOR in the entry_people table."
  (let* ((person-id (sync0-bibtex-sql-get-person-id person db))
        (query (format "SELECT citekey FROM entry_people WHERE person_id = %d;" person-id)))
    (if-let ((result (sqlite-select db query)))
            (message "Citekeys for person %s: %s" person result)
        (message "No entries found for person %s." person))))

(defun sync0-bibtex-sql-add-keywords (keywords db)
  "Add a list of KEYWORDS to the keywords table in the database DB.
If a keyword already exists, retrieve its keyword_id, otherwise insert it."
  (dolist (keyword keywords)
    (sync0-bibtex-sql-get-keyword-id keyword db)))

(defun sync0-bibtex-sql-get-first-20-people (db)
  "Retrieve the first 20 people from the people table in the database DB."
  (let ((query "SELECT person_id, person_name FROM people LIMIT 20;"))
    (sqlite-select db query)))

(defun sync0-bibtex-sql-insert-person-role (citekey db person role &optional roletype)
  "Insert a person with a specific ROLE for a given CITEKEY into the database DB."
  (let* ((person-id (sync0-bibtex-sql-get-person-id person db))
	 (command (if roletype
	  (format "INSERT INTO entry_people (citekey, person_id, role, roletype) VALUES ('%s', %d, '%s', '%s');" citekey person-id role roletype)
	  (format "INSERT INTO entry_people (citekey, person_id, role) VALUES ('%s', %d, '%s');" citekey person-id role))))
    ;; Insert the person-role relationship
    (sqlite-execute db command)))

;; This function has been tested and works properly to insert the
;; relations between people, their roles and the citekeys.
(defun sync0-bibtex-sql-insert-people (citekey db)
  "Insert people from the predefined `sync0-bibtex-people-fields` into the database DB and associate with ENTRY."
  (dolist (people-field sync0-bibtex-people-fields)
    (let ((field-value (symbol-value (intern (concat "sync0-bibtex-entry-" people-field)))))
      (when field-value
        (let ((people-list (split-string field-value " and ")))
          ;; Handle case where there is only one person (no "and")
          (dolist (person people-list)
            ;; Check if the person already exists, if not, insert them
            (let ((person-id (sync0-bibtex-sql-get-person-id person db))
		  (roletype (sync0-bibtex-get-editortype people-field)))  
              ;; Insert the person into `entry_people` with the correct role
	      (if roletype
              (sync0-bibtex-sql-insert-person-role citekey db person people-field roletype)
              (sync0-bibtex-sql-insert-person-role citekey db person people-field)))))))))

(defun sync0-bibtex-sql-insert-keywords (citekey db)
  "Insert keywords from the predefined `sync0-bibtex-people-theme`
into the database DB and associate with ENTRY."
  (when sync0-bibtex-entry-theme
    (let ((keywords-list (split-string sync0-bibtex-entry-theme ", ")))
      ;; Handle case where there is only one person (no "and")
      (dolist (keyword keywords-list)
        ;; Check if the person already exists, if not, insert them
        (when-let ((keyword-id (sync0-bibtex-sql-get-keyword-id keyword db)))
          ;; Insert the person into `entry_people` with the correct role
	  (sqlite-execute db
			  (format "INSERT INTO entry_keywords (citekey, keyword_id)
                             VALUES ('%s', %d);" citekey keyword-id)))))))

;; (defun sync0-bibtex-sql-insert-keyword (db citekey keyword)
;;   "Insert a KEYWORD for a given CITEKEY into the database DB."
;;   (let ((keyword-id (sync0-bibtex-sql-get-keyword-id db keyword)))
;;     ;; Insert the keyword-relationship
;;     (sqlite-execute db
;;                     (format "INSERT INTO entry_keywords (citekey, keyword_id)
;;                              VALUES ('%s', %d);"
;;                             citekey keyword-id))))



;; The following function hasn't been tested and requires tweeking to
;; be able to work properly.
;; (defun sync0-bibtex-sql-import-bibkey-into-db (key db)
;;   "Do not use yet. Insert a BibTeX entry identified by KEY into the SQLite database DB using the new schema."
;;   (let ((bibkey key))
;;     ;; Load the selected BibTeX entry
;;     (sync0-bibtex-completion-load-entry bibkey)
;;     (let* ((citekey bibkey)
;;            (type sync0-bibtex-entry-type)
;;            (title sync0-bibtex-entry-title)
;;            ;; (subtitle sync0-bibtex-entry-subtitle)
;;            ;; (date sync0-bibtex-entry-date)
;;            ;; (origdate sync0-bibtex-entry-origdate)
;;            (keywords sync0-bibtex-entry-theme) ;; List of keywords
;;            ;; Fields for extra_fields table
;;            (fields sync0-bibtex-fields)
;;            ;; (values '())
;;            ;; Helper variables
;;            (main-fields-to-insert '("citekey" "type" "title"))
;;            (main-extra-fields-to-insert '())
;;            (main-extra-fields '("subtitle" "date" "origdate"))
;;            (main-values-to-insert '(citekey type title))
;;            (fields-to-insert '())
;;            (extra-fields-to-insert)
;;            (extra-columns '())
;;            (extra-values '())
;;            (extra-fields '())
;;            (differenc-list (append sync0-bibtex-db-entries-fields sync0-bibtex-db-people-fields))
;;            (extra-main-values '()))
      
;;       ;; Validate all values before insert
;;       (dolist (pair sync0-bibtex-entry-fields-alist fields-to-insert)
;; 	(when-let ((stringvar (car pair))
;; 		   (value (cdr pair)))
;;           (sync0-bibtex-sql-import-bibkey-validate-value value)
;; 	  (push stringvar fields-to-insert)))

;;       (setq extra-fields-to-insert (cl-set-difference differenc-list fields-to-insert :test #'string=))

;;       ;; Prepare fields for insert into entries table 
;;       (dolist (field main-extra-fields)
;;         (let ((field-name (intern (concat "sync0-bibtex-entry-" field)))
;;               (field-value (symbol-value (intern field-name))))
;; 	  (when field-value
;;             (push field main-extra-fields-to-insert)
;;             (push (format "'%s'" field-value) extra-main-values))))

;;       (let ((columns-str (mapconcat 'identity main-extra-fields-to-insert ", "))
;;             (values-str (mapconcat 'identity extra-main-values ", ")))
;;         (let ((extra-command
;;                (format "INSERT INTO entries (citekey, type, title %s) VALUES ('%s', %s);"
;;                        columns-str citekey values-str)))
;;           (sqlite-execute db extra-command)))

;;       ;; Insert people into `entry_people` table: relations between
;;       ;; citekeys, roles and people's names
;;       (sync0-bibtex-sql-insert-people db key)
;;       ;; Insert keywords into `entry_keywords` table
;;       (sync0-bibtex-sql-insert-keywords db citekey)

;;       ;; Prepare extra_fields table insert
;;       (dolist (field extra-fields-to-insert)
;;         (let* ((field-name (car field))
;;                (field-name-short
;; 		(progn 
;; 		  (string-match "sync0-bibtex-entry-\\(.+\\)" field-name)
;; 		  (match-string 1 field-name)))
;;                (field-value (symbol-value (cdr field))))
;;           (when field-value
;;             (sync0-bibtex-sql-add-column-if-not-exists db field-name-short)
;;             (push field-name-short extra-columns)
;;             (push (format "'%s'" field-value) extra-values))))

;;       (when extra-columns
;;         (let ((columns-str (mapconcat 'identity extra-columns ", "))
;;               (values-str (mapconcat 'identity extra-values ", ")))
;;           (let ((extra-command
;;                  (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', %s);"
;;                          columns-str citekey values-str)))
;;             (sqlite-execute db extra-command))))))

;; (defun sync0-bibtex-sql-import-bibkey-into-db (key db)
;;   "Insert a BibLaTeX entry identified by KEY into the SQLite database DB using the new schema."
;;   (let ((bibkey key))
;;     ;; Load the selected BibTeX entry
;;     (sync0-bibtex-completion-load-entry bibkey)
;;     (let* ((citekey bibkey)
;;            (type sync0-bibtex-entry-type)
;;            (title (sync0-bibtex-sql-sanitize-value sync0-bibtex-entry-title))
;;            (main-fields '("citekey" "type" "title"))
;;            (main-values `(,citekey ,type ,title))
;;            (extra-fields (cl-set-difference sync0-bibtex-fields main-fields :test #'string=))
;;            (extra-columns '())
;;            (extra-values '()))

;;       ;; **Insert into `entries` table**
;;       (let ((exists-query (format "SELECT COUNT(*) FROM entries WHERE citekey = '%s';" citekey))
;;             (insert-query nil))
;;         (unless (> (caar (sqlite-select db exists-query)) 0)
;;           (setq insert-query
;;                 (format "INSERT INTO entries (%s) VALUES (%s);"
;;                         (mapconcat 'identity main-fields ", ")
;;                         (mapconcat (lambda (v) (format "'%s'" v)) main-values ", ")))
;;           (sqlite-execute db insert-query)))

;;       ;; **Insert People**
;;       (sync0-bibtex-sql-insert-people db citekey)

;;       ;; **Insert Keywords**
;;       (sync0-bibtex-sql-insert-keywords db citekey)

;;       ;; **Insert into `extra_fields`**
;;       (dolist (field extra-fields)
;;         (let* ((field-name (concat "sync0-bibtex-entry-" field))
;;                (field-value (symbol-value (intern field-name))))
;;           (when field-value
;;             (sync0-bibtex-sql-add-column-if-not-exists db field)
;;             (push field extra-columns)
;;             (push (format "'%s'" (sync0-bibtex-sql-sanitize-value field-value)) extra-values))))
;;       (when extra-columns
;;         (let ((insert-extra-query
;;                (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', %s);"
;;                        (mapconcat 'identity extra-columns ", ")
;;                        citekey
;;                        (mapconcat 'identity extra-values ", "))))
;;           (sqlite-execute db insert-extra-query))))))

(defun sync0-bibtex-sql-insert-extra-fields (citekey db)
  (let* ((extra-fields sync0-bibtex-db-purged-extra-fields)
         (extra-columns '())
         (extra-values '()))
    (dolist (field extra-fields)
      (let* ((field-name (concat "sync0-bibtex-entry-" field))
             (field-value (symbol-value (intern field-name))))
        (when field-value
          (unless (sync0-bibtex-sql-column-exists-p field db)
            (sync0-bibtex-sql-add-column field db))
	  (push field extra-columns)
	  (if (string= field "aliases")
	      (push (format "'%s'" field-value) extra-values)
	    (push (format "'%s'" (sync0-bibtex-sql-sanitize-value field-value)) extra-values)))))

    ;; **Push bibentry into extra-fields**
    (push "bibentry" extra-columns)
    (push (format "'%s'" (sync0-bibtex-sql-sanitize-value sync0-bibtex-entry-bibentry)) extra-values)

    (when extra-columns
      (let ((insert-extra-query
             (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', %s);"
                     (mapconcat 'identity extra-columns ", ")
                     citekey
                     (mapconcat 'identity extra-values ", "))))
        (sqlite-execute db insert-extra-query)))))

(defun sync0-bibtex-sql-import-bibkey-into-db (citekey db)
  "Dummy version of the function that only prints the SQL commands it would execute."
  ;; Load the selected BibTeX entry (dummy)
  (sync0-bibtex-completion-load-entry citekey)
  (let* ((type sync0-bibtex-entry-type)
         (title (sync0-bibtex-sql-sanitize-value sync0-bibtex-entry-title))
         ;; New fields for subtitle, date, and origdate
         (subtitle (sync0-bibtex-sql-sanitize-value sync0-bibtex-entry-subtitle))
         (date (sync0-bibtex-sql-sanitize-value sync0-bibtex-entry-date))
         (origdate (sync0-bibtex-sql-sanitize-value sync0-bibtex-entry-origdate))
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
	  (sync0-bibtex-sql-insert-people citekey db)
	  ;; **Insert Keywords**
	  (sync0-bibtex-sql-insert-keywords citekey db)
	  ;; **Insert Extra fields**
          (sync0-bibtex-sql-insert-extra-fields citekey db))
      (sqlite-commit db)
      (setq sync0-bibtex-db-dirty t))))

      ;; **Print SQL for inserting into `extra_fields`**

;; (defun sync0-bibtex-sql-import-bibkey-into-db (key db)
;;   "Generate an SQL INSERT command dynamically from BibTeX fields for a given KEY and print it."
;;   (let ((bibkey key))
;;     ;; Load the selected BibTeX entry
;;     (sync0-bibtex-completion-load-entry bibkey)
;;     (let* ((citekey bibkey)
;;            (type sync0-bibtex-entry-type)
;;            (title sync0-bibtex-entry-title)
;;            ;; Optional fields
;;            (fields '(("author" . sync0-bibtex-entry-author)
;;                      ("subtitle" . sync0-bibtex-entry-subtitle)
;;                      ("editor" . sync0-bibtex-entry-editor)
;;                      ("date" . sync0-bibtex-entry-date)
;;                      ("origdate" . sync0-bibtex-entry-origdate)))
;;            ;; Initial required columns and values
;;            (columns '("citekey" "type" "title"))
;;            (values-check (list citekey type title))
;;            (values (list (format "'%s'" citekey)
;;                          (format "'%s'" type)
;;                          (format "'%s'" title))))
;;       ;; Validate required values
;;       (dolist (value values-check)
;;         (sync0-bibtex-sql-import-bibkey-validate-value value))

;;       ;; Dynamically add optional fields
;;       (dolist (field fields)
;;         (let ((field-name (car field))
;;               (field-value (symbol-value (cdr field))))
;;           (when field-value
;;             (sync0-bibtex-sql-import-bibkey-validate-field field-value field-name)
;;             (push field-name columns)
;;             (push (format "'%s'" field-value) values))))

;;       ;; Construct and print the SQL command
;;       (let ((columns-str (mapconcat 'identity columns ", "))
;;             (values-str (mapconcat 'identity values ", ")))
;;         (let ((complete-command (format "INSERT INTO entries (%s) VALUES (%s)"
;;                                         columns-str values-str)))
;; 	  (sqlite-execute db complete-command))))))

(defun sync0-bibtex-sql-import-single-bibkey ()
  "Import single bibkey into my database."
  (interactive)
  (let ((db (sync0-bibtex-sql-get-database))
	(bibkey (sync0-bibtex-completion-choose-key t t)))
    (sync0-bibtex-sql-import-bibkey-into-db bibkey db)))

(defun sync0-bibtex-sql-import-bibkey-at-point ()
  "Import single bibkey into my database."
  (interactive)
  (let* ((db (sync0-bibtex-sql-get-database))
	 (entry (save-excursion (bibtex-beginning-of-entry)
                                (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry))))
    (sync0-bibtex-sql-import-bibkey-into-db bibkey db)))

;; (defun sync0-bibtex-sql-import-single-bibkey-dummy ()
;;   "Import single bibkey into my database."
;;   (interactive)
;;   (let ((db (sync0-bibtex-sql-get-database))
;; 	(bibkey (sync0-bibtex-completion-choose-key t t)))
;;     (sync0-bibtex-sql-import-bibkey-into-db-dummy bibkey db)))

;; (defun sync0-bibtex-sql-bulk-import-into-db-dummy (file-path)
;;   "Process a BibLaTeX file at FILE-PATH, generating SQL commands for all keys.
;; Skip keys that are already present in the database."
;;   (interactive "fSelect BibTeX file: ")
;;   (let* ((db (sync0-bibtex-sql-get-database))
;; 	 (existing-keys (sync0-bibtex-sql-retrieve-bibkeys db)))
;;     (with-temp-buffer
;;       ;; Load the BibTeX file into the buffer
;;       (insert-file-contents file-path)
;;       ;; Extract all BibTeX keys (assuming standard @type{key,} format)
;;       (let ((keys ()))
;;         (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
;;           (push (match-string 1) keys))
;;         (let ((successes '())
;;               (skipped '())
;;               (failures '()))
;;           (dolist (key (reverse keys))
;;             (if (member key existing-keys)
;;                 (progn
;;                   (message "Key '%s' is already in the database. Skipping..." key)
;;                   (push key skipped))
;;               (condition-case err
;;                   (progn
;;                     ;; Call foo and print the command
;;                     (sync0-bibtex-sql-import-bibkey-into-db-dummy key db)
;;                     (push key successes))
;;                 (error
;;                  ;; Log failure and error message
;;                  (message "Failed to process key: %s. Error: %s" key err)
;;                  (push key failures)))))
;;           ;; Summary of results
;;           (message "Processing complete: %d successful, %d skipped, %d failed."
;;                    (length successes) (length skipped) (length failures))
;;           (when skipped
;;             (message "Skipped keys: %s" (string-join skipped ", ")))
;;           (when failures
;;             (message "Failed keys: %s" (string-join failures ", "))))))))


;; (defun sync0-bibtex-sql-bulk-import-current-bibfile-dummy ()
;;   "Check if the current buffer is a BibLaTeX file. If so, call `sync0-bibtex-sql-bulk-import-into-db`."
;;   (interactive)
;;   (let ((file-path (buffer-file-name)))
;;     (if (and file-path
;;              (or (string-match-p "\\.bib\\'" file-path) ;; Match .bib extension
;;                  (save-excursion
;;                    ;; Check if the buffer contains the @string or @article, etc.
;;                    (goto-char (point-min))
;;                    (re-search-forward "@\\w+{" nil t))))
;;         (progn
;;           (message "This is a BibLaTeX file, starting import.")
;;           (sync0-bibtex-sql-bulk-import-into-db-dummy file-path))
;;       (message "The current file is not a BibLaTeX file."))))

;; (defun sync0-bibtex-sql-query-bibkey (citekey)
;;   "Query the BibTeX entry by citekey from the database."
;;   (interactive)
;;   (let* ((db (sync0-bibtex-sql-get-database))
;; 	 (bibkey (sync0-bibtex-completion-choose-key t))
;; 	 (query (format "select concat (
;;                         COALESCE(author, 'No Author'), 
;;                         ' ', 
;;                         CASE 
;;                           WHEN origdate IS NOT NULL THEN 
;;                             concat('[', origdate, ']')
;;                           ELSE 
;;                             ''
;;                         END, 
;;                         ' ', 
;;                         CASE 
;;                           WHEN date IS NOT NULL THEN 
;;                             concat('(', date, ')')
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
;;                         ' [@',
;;                         citekey,
;;                         ']') from entries  where citekey =  '%s'" bibkey))
;; 	 (result (sqlite-select db query)))
;;     (message "Entry: %s" result)))

(defun sync0-bibtex-sql-rebuild-full-cache ()
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

(defun sync0-bibtex-sql-query-entry ()
  "Query the database for BibTeX entries and return formatted candidates."
  (interactive)
  (let* ((candidates
	  (if sync0-bibtex-db-dirty 
	      (sync0-bibtex-sql-rebuild-full-cache)
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

;; (defun sync0-bibtex-sql-query-entry ()
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

;; (defun sync0-bibtex-sql-query-entry ()
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

;; this query is awesome! this can format retrieved entries in a readable format!
;; (let* ((db (sqlite-open sync0-bibtex-database-path))
;;        (citekey "22364rb")
;;        (type "Book")
;;        (title "Traité théorique et pratique des entreprises industrielles, commerciales et agricoles")
;;        (subtitle "Manuel des affaires")
;;        (author "Courcelle-Seneuil, Jean-Gustave")
;;        (date "1855")
;;        (base-command "INSERT INTO entries (citekey, type, title, subtitle, author, date)")
;;        (values (format "values ('%s', '%s', '%s', '%s', '%s', '%s')" citekey type title subtitle author date))
;;        (complete-command (concat base-command values)))
;; (sqlite-execute db complete-command))

;; (defun sync0-bibtex-init-database ()
;;   "Initialize the SQLite database for the bibliography, if it doesn't exist."
;;   (unless (file-exists-p sync0-bibtex-database-path)
;;     (let ((db (emacsql-sqlite sync0-bibtex-database-path)))
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
            
;;             (emacsql-close db)
;;             (message "Initialized BibTeX database."))
;;         (error (message "Error initializing BibTeX database: %s" (error-message-string err)))))))

(defun my-sql-get-entry (bibkey)
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

;; (defun sync0-bibtex-update-entry-at-point ()
;;   "Update the BibTeX entry at point in the database DB."
;;   (interactive)
;;   (sync0-bibtex-load-entry-at-point)
;;   (let ((db (sync0-bibtex-sql-get-database))
;; 	(bibkey sync0-bibtex-entry-key))
;;     (when (yes-or-no-p "Import bibkey %s into database? "bibkey)
;;       ;; Call the core update function
;;       (sync0-bibtex-update-entry-in-db db bibkey))))

(defun sync0-bibtex-sql-update-main-fields (citekey db)
  (let* ((main-fields '("type" "title" "subtitle" "date" "origdate")) ;; Replace or extend with actual main fields
         (updated-fields '())) ;; Track main updated fields
    (dolist (field main-fields)
      (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
	     (new-value (symbol-value field-symbol))
	     (attempted-query
	      (when new-value
		(sqlite-select db
			       (format "SELECT %s FROM entries WHERE citekey = ?;" field)
			       (list citekey))))
	     (existing-value (when attempted-query
			       (caar attempted-query))))
        (when (and new-value
		   (not (equal new-value existing-value)))
	  (push field updated-fields)
	  (sqlite-execute db
			  (format "UPDATE entries SET %s = '%s' WHERE citekey = '%s';"
				  field new-value citekey)))))
    (message "Updated fields: %s"
             (mapconcat 'identity updated-fields ", "))))

(defun sync0-bibtex-sql-update-extra-fields (citekey db)
  (let ((extra-fields (cons "bibentry"  sync0-bibtex-db-purged-extra-fields)) ;; Extra fields from your configuration
        (updated-extra-fields '()) ;; Track extra updated fields
	(columns (mapcar #'cadr (sqlite-select db "PRAGMA table_info(extra_fields);")))) ;; Get current columns
    ;; Filter out fields that already exist in the table
    (dolist (field extra-fields)
      (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
	     (raw-value (symbol-value field-symbol))
             (new-value (when raw-value
			  (if (string= field "aliases")
			      raw-value
			    (sync0-bibtex-sql-sanitize-value raw-value))))
             (existing-row
              (when (member field columns)
		(sqlite-select db (format "SELECT %s FROM extra_fields WHERE citekey = ?;" field) (list citekey))))
             (existing-value (if existing-row (caar existing-row) nil)))
	;; Add missing columns
	(unless (sync0-bibtex-sql-column-exists-p field db)
	  (sync0-bibtex-sql-add-column field db))
	(if (and new-value existing-value)
	    (unless (equal new-value existing-value) ;; If value differs, update
	      (sqlite-execute db
			      (format "UPDATE extra_fields SET %s = '%s' WHERE citekey = '%s';"
				      field new-value citekey))
	      (push field updated-extra-fields))
	  (progn 
            (if new-value
		(sqlite-execute db
				(format "INSERT OR REPLACE INTO extra_fields (citekey, %s) VALUES ('%s', '%s');"
					field citekey new-value))
	      (unless (null existing-value)
		(sqlite-execute db
				(format "UPDATE extra_fields SET %s = NULL WHERE citekey = '%s';"
					field citekey)))
	      (push field updated-extra-fields))))))
    (message "Updated extra fields: %s"
             (mapconcat 'identity updated-extra-fields ", "))))

(defun sync0-bibtex-update-bibkey-in-db (citekey db)
  "Update the database DB with the BibTeX entry identified by CITEKEY.
Compares values in the database and performs actual SQL updates or inserts."
  (sqlite-transaction db)
  (unwind-protect
      (progn
	;; **Main Fields Update**
        (sync0-bibtex-sql-update-main-fields citekey db)
	;; **Extra Fields Update**
        (sync0-bibtex-sql-update-extra-fields citekey db)
        ;; First, get the list of existing columns in extra_fields
	(sqlite-commit db)
	(setq sync0-bibtex-db-dirty t))))

    ;; **People and Keywords Update** (Optional: Implement actual logic here if needed)
    ;; (when (not (null (symbol-value 'sync0-bibtex-entry-people)))
    ;;   (sync0-bibtex-update-people-in-db citekey db))
    ;; (when (not (null (symbol-value 'sync0-bibtex-entry-keywords)))
    ;;   (sync0-bibtex-update-keywords-in-db citekey db))


;; (defun sync0-bibtex-sql-update-entry-at-point ()
;;   "Update the BibTeX entry at point in the database DB."
;;   (interactive)
;;   (sync0-bibtex-load-entry-at-point)
;;   (let ((db (sync0-bibtex-sql-get-database))
;; 	(bibkey sync0-bibtex-entry-key))
;;     (when (yes-or-no-p (format "Import bibkey %s into database? " bibkey))
;;       ;; Call the core update function
;;       (sync0-bibtex-dummy-update-entry-in-db bibkey db))))

(defun sync0-bibtex-sql-update-or-import-bibkey-at-point ()
  "Update or import the BibTeX entry at point into the database DB.
If the entry doesn't exist, it will be imported; if it exists, it will be updated."
  (interactive)
  (sync0-bibtex-load-entry-at-point)
  (let ((db (sync0-bibtex-sql-get-database))
        (bibkey sync0-bibtex-entry-key))
    (if (= 0 (caar (sqlite-select db
                                  "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list bibkey))))
        (when (yes-or-no-p (format "Citekey %s not present in database. Import?" bibkey))
          ;; Entry not found, import it
          (sync0-bibtex-sql-import-bibkey-into-db bibkey db))
      (when (yes-or-no-p (format "Bibkey %s found in database. Update entry? " bibkey))
        ;; Entry found, update it
        (sync0-bibtex-update-bibkey-in-db bibkey db)))))

(defun sync0-bibtex-sql-get-bibentry (citekey db)
  "Query the database DB for the value of bibentry in the extra_fields table for the given CITEKEY."
  (let ((result (sqlite-select db
                               "SELECT bibentry FROM extra_fields WHERE citekey = ?;"
                               (list citekey))))
    (if result
        (message "The value of bibentry for citekey %s is:\n%s"
                 citekey (car (car result)))
      (message "No bibentry found for citekey %s." citekey))))

(defun sync0-bibtex-sql-query-bibentry ()
  "Query the database DB for the value of bibentry in the extra_fields table for the given CITEKEY."
  (interactive)
  (let ((key (sync0-bibtex-sql-query-entry))
	(db (sqlite-open sync0-bibtex-database-path)))
    (sync0-bibtex-sql-get-bibentry key db)))

(defun sync0-bibtex-sql-update-or-import-bibkey (bibkey db)
  "Update or import the BibTeX entry at point into the database DB.
If the entry doesn't exist, it will be imported; if it exists, it will be updated."
  (if (= 0 (caar (sqlite-select db
                                "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list bibkey))))
      ;; Entry not found, import it
      (sync0-bibtex-sql-import-bibkey-into-db bibkey db)
    ;; Entry found, update it
    (sync0-bibtex-update-bibkey-in-db bibkey db)))

(defun sync0-bibtex-sql-bulk-import-into-db (file-path)
  "Process a BibLaTeX file at FILE-PATH, generating SQL commands for all keys.
Skip keys that are already present in the database."
  (interactive "fSelect BibTeX file: ")
  (let* ((db (sync0-bibtex-sql-get-database))
	 (existing-keys (sync0-bibtex-sql-retrieve-bibkeys db)))
    (with-temp-buffer
      ;; Load the BibTeX file into the buffer
      (insert-file-contents file-path)
      ;; Extract all BibTeX keys (assuming standard @type{key,} format)
      (let ((keys ()))
        (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
          (push (match-string 1) keys))
        (let ((successes '())
              (skipped '())
              (failures '()))
          (dolist (key (reverse keys))
            (if (member key existing-keys)
                (progn
                  (sync0-bibtex-update-bibkey-in-db key db)
                  (message "Key '%s' is already in the database. Updated." key)
                  (push key skipped))
              (condition-case err
                  (progn
                    ;; Call foo and print the command
                    (sync0-bibtex-sql-import-bibkey-into-db key db)
                    (push key successes))
                (error
                 ;; Log failure and error message
                 (message "Failed to process key: %s. Error: %s" key err)
                 (push key failures)))))
          ;; Summary of results
          (message "Processing complete: %d successful, %d skipped, %d failed."
                   (length successes) (length skipped) (length failures))
          (when skipped
            (message "Skipped keys: %s" (string-join skipped ", ")))
          (when failures
            (message "Failed keys: %s" (string-join failures ", "))))))))

(defun sync0-bibtex-sql-bulk-import-current-bibfile ()
  "Check if the current buffer is a BibLaTeX file. If so, call `sync0-bibtex-sql-bulk-import-into-db`."
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
          (sync0-bibtex-sql-bulk-import-into-db file-path))
      (message "The current file is not a BibLaTeX file."))))


(provide 'sync0-bibtex-sql)

