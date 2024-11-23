

(defun foo ()
  (interactive)
  (let* ((db (sync0-bibtex-sql-get-database))
	(firstp (sqlite-select db "PRAGMA table_info(extra_fields);"))
	(product (mapcar #'cadr firstp)))
    (message "SQL: %s" firstp)
    (message "REST: %s" product)))

(foo)

(defun sync0-bibtex-dummy-update-entry-in-db (citekey db)
  "Simulate updating the database DB with the BibTeX entry identified by CITEKEY.
Prints the SQL commands that would be executed instead of actually modifying the database."
  
  (let* ((main-fields '("key" "type" "title"))
         (extra-fields sync0-bibtex-db-purged-extra-fields) ;; Extra fields
         (sql-commands '()) ;; Collect SQL commands here
         (updated-fields '()) ;; Track main updated fields
         (updated-extra-fields '())) ;; Track extra updated fields
    ;; Simulate checking if the entry exists
    (if (not (format "SELECT COUNT(*) FROM entries WHERE citekey = '%s';" citekey))
        (message "Entry with citekey %s does not exist in the database." citekey)
      (progn
        ;; **Simulate Main Fields Update**
        (dolist (field main-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value (format "Existing value for %s" field))) ;; Dummy existing value
            (when (and new-value (not (equal new-value existing-value)))
              (push field updated-fields)
              (push (format "UPDATE entries SET %s = '%s' WHERE citekey = '%s';"
                            field new-value citekey)
                    sql-commands))))

        ;; **Simulate Extra Fields Update**
        (dolist (field extra-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value (format "Existing value for %s" field))) ;; Dummy existing value
            (when new-value
              (push (format "ALTER TABLE extra_fields ADD COLUMN IF NOT EXISTS %s;" field)
                    sql-commands) ;; Simulate adding column
              (if (not existing-value) ;; Simulate missing field value
                  (push (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', '%s');"
                                field citekey new-value)
                        sql-commands)
                (when (not (equal new-value existing-value)) ;; Simulate update
                  (push field updated-extra-fields)
                  (push (format "UPDATE extra_fields SET %s = '%s' WHERE citekey = '%s';"
                                field new-value citekey)
                        sql-commands))))))

        ;; **Simulate People and Keywords Update**
        (push (format "Simulating call to insert people for %s" citekey) sql-commands)
        (push (format "Simulating call to insert keywords for %s" citekey) sql-commands)

        ;; **Feedback**
        (message "Simulated SQL Commands:\n%s\nUpdated fields: %s\nUpdated extra fields: %s"
                 (mapconcat 'identity sql-commands "\n")
                 (mapconcat 'identity updated-fields ", ")
                 (mapconcat 'identity updated-extra-fields ", "))))))

(defun sync0-bibtex-dummy-update-entry-in-db (citekey db)
  "Simulate updating the database DB with the BibTeX entry identified by CITEKEY.
Queries the database to compare values and prints the SQL commands that would be executed."
  (let* ((main-fields '("type" "title" "subtitle" "date" "origdate")) ;; Replace or extend with actual main fields
         (extra-fields sync0-bibtex-db-purged-extra-fields) ;; Extra fields from your configuration
         (sql-commands '()) ;; Collect SQL commands here
         (updated-fields '()) ;; Track main updated fields
         (updated-extra-fields '())) ;; Track extra updated fields
    ;; Check if the entry exists
    (if (= 0 (caar (sqlite-select db "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list citekey))))
        (message "Entry with citekey %s does not exist in the database." citekey)
      (progn
        ;; **Simulate Main Fields Update**
        (dolist (field main-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value  (caar (sqlite-select
                                         db
                                         (format "SELECT %s FROM entries WHERE citekey = ?;" field)
                                         (list citekey)))))
            (when (and new-value (not (equal new-value existing-value)))
              (push field updated-fields)
              (push (format "UPDATE entries SET %s = '%s' WHERE citekey = '%s';"
                            field new-value citekey)
                    sql-commands))))

        ;; **Simulate Extra Fields Update**
        (dolist (field extra-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value (caar (sqlite-select
                                        db
                                        (format "SELECT %s FROM extra_fields WHERE citekey = ?;" field)
                                        (list citekey)))))
            (when (and new-value (not (equal new-value existing-value)))
              (push (format "ALTER TABLE extra_fields ADD COLUMN IF NOT EXISTS %s;" field)
                    sql-commands) ;; Simulate adding column
              (if (not existing-value) ;; If no existing value, insert
                  (push (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', '%s');"
                                field citekey new-value)
                        sql-commands)
                (when (not (equal new-value existing-value)) ;; If value differs, update
                  (push field updated-extra-fields)
                  (push (format "UPDATE extra_fields SET %s = '%s' WHERE citekey = '%s';"
                                field new-value citekey)
                        sql-commands))))))

        ;; **Simulate People and Keywords Update**
        (push (format "Simulating call to insert people for %s" citekey) sql-commands)
        (push (format "Simulating call to insert keywords for %s" citekey) sql-commands)

        ;; **Feedback**
        (message "Simulated SQL Commands:\n%s\nUpdated fields: %s\nUpdated extra fields: %s"
                 (mapconcat 'identity sql-commands "\n")
                 (mapconcat 'identity updated-fields ", ")
                 (mapconcat 'identity updated-extra-fields ", "))))))


(defun sync0-bibtex-dummy-update-entry-in-db (citekey db)
  "Simulate updating the database DB with the BibTeX entry identified by CITEKEY.
Queries the database to compare values and prints the SQL commands that would be executed."
  (let* ((main-fields '("type" "title" "subtitle" "date" "origdate")) ;; Replace or extend with actual main fields
         (extra-fields sync0-bibtex-db-purged-extra-fields) ;; Extra fields from your configuration
         (sql-commands '()) ;; Collect SQL commands here
         (updated-fields '()) ;; Track main updated fields
         (updated-extra-fields '())) ;; Track extra updated fields
    ;; Check if the entry exists
    (if (= 0 (caar (sqlite-select db "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list citekey))))
        (message "Entry with citekey %s does not exist in the database." citekey)
      (progn
        ;; **Simulate Main Fields Update**
        (dolist (field main-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value (caar (sqlite-select
                                        db
                                        (format "SELECT %s FROM entries WHERE citekey = ?;" field)
                                        (list citekey)))))
            (when (and new-value (not (equal new-value existing-value)))
              (push field updated-fields)
              (push (format "UPDATE entries SET %s = '%s' WHERE citekey = '%s';"
                            field new-value citekey)
                    sql-commands))))

        ;; **Simulate Extra Fields Update**
        (dolist (field extra-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-row (sqlite-select
                                db
                                (format "SELECT %s FROM extra_fields WHERE citekey = ?;" field)
                                (list citekey)))
                 (existing-value (if existing-row (car (car existing-row)) nil))
                 (columns (mapcar #'car (sqlite-select db "PRAGMA table_info(extra_fields);")))) ;; Get columns
            ;; Simulate adding a column if the field does not exist
            (unless (member field columns)
              (push (format "ALTER TABLE extra_fields ADD COLUMN %s;" field) sql-commands))
            ;; Simulate inserting or updating the value
            (when new-value
              (if (not existing-value) ;; If no existing value, insert
                  (push (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', '%s');"
                                field citekey new-value)
                        sql-commands)
                (when (not (equal new-value existing-value)) ;; If value differs, update
                  (push field updated-extra-fields)
                  (push (format "UPDATE extra_fields SET %s = '%s' WHERE citekey = '%s';"
                                field new-value citekey)
                        sql-commands))))))

        ;; **Simulate People and Keywords Update**
        (push (format "Simulating call to insert people for %s" citekey) sql-commands)
        (push (format "Simulating call to insert keywords for %s" citekey) sql-commands)

        ;; **Feedback**
        (message "Simulated SQL Commands:\n%s\nUpdated fields: %s\nUpdated extra fields: %s"
                 (mapconcat 'identity sql-commands "\n")
                 (mapconcat 'identity updated-fields ", ")
                 (mapconcat 'identity updated-extra-fields ", "))))))

(defun sync0-bibtex-dummy-update-entry-in-db (citekey db)
  "Simulate updating the database DB with the BibTeX entry identified by CITEKEY.
Queries the database to compare values and prints the SQL commands that would be executed."
  (let* ((main-fields '("type" "title" "subtitle" "date" "origdate")) ;; Replace or extend with actual main fields
         (extra-fields sync0-bibtex-db-purged-extra-fields) ;; Extra fields from your configuration
         (sql-commands '()) ;; Collect SQL commands here
         (updated-fields '()) ;; Track main updated fields
         (updated-extra-fields '())) ;; Track extra updated fields
    ;; Check if the entry exists
    (if (= 0 (caar (sqlite-select db "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list citekey))))
        (message "Entry with citekey %s does not exist in the database." citekey)
      (progn
        ;; **Simulate Main Fields Update**
        (dolist (field main-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value (caar (sqlite-select
                                        db
                                        (format "SELECT %s FROM entries WHERE citekey = ?;" field)
                                        (list citekey)))))
            (when (and new-value (not (equal new-value existing-value)))
              (push field updated-fields)
              (push (format "UPDATE entries SET %s = '%s' WHERE citekey = '%s';"
                            field new-value citekey)
                    sql-commands))))

        ;; **Simulate Extra Fields Update**
	;; First, get the list of existing columns in extra_fields
	(let ((columns (mapcar #'cadr (sqlite-select db "PRAGMA table_info(extra_fields);")))) ;; Get current columns
	  ;; Filter out fields that already exist in the table
	  (dolist (field extra-fields)
	    (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
		   (new-value (symbol-value field-symbol))
		   (existing-row
		    (when (member field columns)
		      (sqlite-select
		       db
		       (format "SELECT %s FROM extra_fields WHERE citekey = ?;" field)
		       (list citekey))))
		   (existing-value (if existing-row (car (car existing-row)) nil)))
	      ;; Add missing columns
	      (unless (or (null new-value)
			  (member field columns))
		(push (format "ALTER TABLE extra_fields ADD COLUMN %s TEXT;" field) sql-commands) ;; Assuming TEXT type for the new fields
		(push field updated-extra-fields))  ;; Track the fields being added
	      ;; If the field has a new value, either insert or update it
	      (when new-value
		(if (not existing-value) ;; If no existing value, insert
		    (push (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', '%s');"
				  field citekey new-value)
			  sql-commands)
		  (when (not (equal new-value existing-value)) ;; If value differs, update
		    (push (format "UPDATE extra_fields SET %s = '%s' WHERE citekey = '%s';"
				  field new-value citekey)
			  sql-commands)))
	      (push field updated-extra-fields)))))

        ;; **Simulate People and Keywords Update**
        (push (format "Simulating call to insert people for %s" citekey) sql-commands)
        (push (format "Simulating call to insert keywords for %s" citekey) sql-commands)

        ;; **Feedback**
        (message "Simulated SQL Commands:\n%s\nUpdated fields: %s\nUpdated extra fields: %s"
                 (mapconcat 'identity sql-commands "\n")
                 (mapconcat 'identity updated-fields ", ")
                 (mapconcat 'identity updated-extra-fields ", "))))))


(defun sync0-bibtex-dummy-update-entry-in-db (citekey db)
  "Simulate updating the database DB with the BibTeX entry identified by CITEKEY.
Queries the database to compare values and prints the SQL commands that would be executed."
  (let* ((main-fields '("type" "title" "subtitle" "date" "origdate")) ;; Replace or extend with actual main fields
         (extra-fields sync0-bibtex-db-purged-extra-fields) ;; Extra fields from your configuration
         (sql-commands '()) ;; Collect SQL commands here
         (updated-fields '()) ;; Track main updated fields
         (updated-extra-fields '())) ;; Track extra updated fields
    ;; Check if the entry exists
    (if (= 0 (caar (sqlite-select db "SELECT COUNT(*) FROM entries WHERE citekey = ?;" (list citekey))))
        (message "Entry with citekey %s does not exist in the database." citekey)
      (progn
        ;; **Simulate Main Fields Update**
        (dolist (field main-fields)
          (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
                 (new-value (symbol-value field-symbol))
                 (existing-value (caar (sqlite-select
                                        db
                                        (format "SELECT %s FROM entries WHERE citekey = ?;" field)
                                        (list citekey)))))
            (when (and new-value (not (equal new-value existing-value)))
              (push field updated-fields)
              (push (format "UPDATE entries SET %s = '%s' WHERE citekey = '%s';"
                            field new-value citekey)
                    sql-commands))))

        ;; **Simulate Extra Fields Update**
	;; First, get the list of existing columns in extra_fields
	(let ((columns (mapcar #'cadr (sqlite-select db "PRAGMA table_info(extra_fields);")))) ;; Get current columns
	  ;; Filter out fields that already exist in the table
	  (dolist (field extra-fields)
	    (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" field)))
		   (new-value (symbol-value field-symbol))
		   (existing-row
		    (when (member field columns)
		      (sqlite-select
		       db
		       (format "SELECT %s FROM extra_fields WHERE citekey = ?;" field)
		       (list citekey))))
		   (existing-value (if existing-row (car (car existing-row)) nil)))
	      ;; Add missing columns
	      (unless (or (null new-value)
			  (member field columns))
		(push (format "ALTER TABLE extra_fields ADD COLUMN %s TEXT;" field) sql-commands) ;; Assuming TEXT type for the new fields
		(push field updated-extra-fields))  ;; Track the fields being added
	      ;; If the field has a new value, either insert or update it
	      (when new-value
		(if (not existing-value) ;; If no existing value, insert
		    (push (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', '%s');"
				  field citekey new-value)
			  sql-commands)
		  (when (not (equal new-value existing-value)) ;; If value differs, update
		    (push (format "UPDATE extra_fields SET %s = '%s' WHERE citekey = '%s';"
				  field new-value citekey)
			  sql-commands)))
	      (push field updated-extra-fields)))))

        ;; **Simulate People and Keywords Update**
        (push (format "Simulating call to insert people for %s" citekey) sql-commands)
        (push (format "Simulating call to insert keywords for %s" citekey) sql-commands)

        ;; **Feedback**
        (message "Simulated SQL Commands:\n%s\nUpdated fields: %s\nUpdated extra fields: %s"
                 (mapconcat 'identity sql-commands "\n")
                 (mapconcat 'identity updated-fields ", ")
                 (mapconcat 'identity updated-extra-fields ", "))))))




(defun sync0-bibtex-sql-update-people (db citekey)
  "Update people from the predefined `sync0-bibtex-people-fields` in the database DB and associate with ENTRY."
  (dolist (people-field sync0-bibtex-people-fields)
    (let ((field-value (symbol-value (intern (concat "sync0-bibtex-entry-" people-field)))))
      (when field-value
        (let ((people-list (split-string field-value " and ")))
          ;; Handle case where there is only one person (no "and")
          (dolist (person people-list)
            (let ((person-id (sync0-bibtex-sql-get-person-id db person)))
              ;; If person already exists, update the role; otherwise, insert
              (if person-id
                  (sync0-bibtex-sql-update-person-role db citekey person people-field person-id)
                (sync0-bibtex-sql-insert-person-role db citekey person people-field))))))))

(defun sync0-bibtex-sql-update-person-role (db citekey person people-field person-id)
  "Update the role of an existing person in the `entry_people` table for the given CITEKEY."
  (let ((existing-role (caar (sqlite-select db
                                            "SELECT role FROM entry_people WHERE citekey = ? AND person_id = ?;"
                                            (list citekey person-id)))))
    (unless (equal existing-role people-field) ;; If role has changed, update it
      (sqlite-execute db
                      (format "UPDATE entry_people SET role = '%s' WHERE citekey = '%s' AND person_id = '%d';"
                              people-field citekey person-id)))))
(defun sync0-bibtex-sql-update-keywords (db citekey)
  "Update keywords from the predefined `sync0-bibtex-entry-theme` in the database DB and associate with ENTRY."
  (when sync0-bibtex-entry-theme
    (let ((keywords-list (split-string sync0-bibtex-entry-theme ", ")))
      (dolist (keyword keywords-list)
        (let ((keyword-id (sync0-bibtex-sql-get-keyword-id db keyword)))
          ;; If keyword already exists, update the association; otherwise, insert
          (if keyword-id
              (sync0-bibtex-sql-update-keyword-association db citekey keyword-id)
            (sync0-bibtex-sql-insert-keyword db citekey keyword))))))

(defun sync0-bibtex-sql-update-keyword-association (db citekey keyword-id)
  "Update the association of an existing keyword with the ENTRY in `entry_keywords`."
  (let ((existing-entry (caar (sqlite-select db
                                              "SELECT keyword_id FROM entry_keywords WHERE citekey = ? AND keyword_id = ?;"
                                              (list citekey keyword-id)))))
    (unless existing-entry ;; If the keyword is not already associated, insert
      (sqlite-execute db
                      (format "INSERT INTO entry_keywords (citekey, keyword_id) VALUES ('%s', %d);"
                              citekey keyword-id)))))


