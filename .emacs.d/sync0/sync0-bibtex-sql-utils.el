(require 'sync0-bibtex-vars)
(require 'xah-replace-pairs)

(defun sync0-bibtex-db-get-database ()
  "Return a connection to the BibTeX database, initializing if necessary."
  (unless (and sync0-bibtex-db-connection
               (sqlitep sync0-bibtex-db-connection))
    (setq sync0-bibtex-db-connection
          (sqlite-open sync0-bibtex-database-path)))
  sync0-bibtex-db-connection)

(defun sync0-bibtex-db-sanitize-value (value)
  "Sanitize VALUE by replacing straight quotes with smart quotes (opening and closing)."
  (when value
    (let ((sanitized (format "%s" value)))

      ;; Replace apostrophes (') with a closing single quote (’)
      (setq sanitized (replace-regexp-in-string "'" "’" sanitized))

      ;; Replace single quotes (') used for quotation (like in "quoted") with opening and closing quotes
      (setq sanitized (replace-regexp-in-string "‘" "’" sanitized))

      ;; Handle single quotes in words, replace with opening and closing quotes based on position
      (setq sanitized (replace-regexp-in-string "'" 
                                                (lambda (x)
                                                  (if (or (zerop (length x)) (not (string-match "[a-zA-Z0-9]" (substring x 0 1))))
                                                      "’"  ;; closing single quote
                                                    "‘")) sanitized))

      ;; Handle straight double quotes (") by replacing them with opening or closing smart quotes
      (setq sanitized (replace-regexp-in-string "\"" 
                                                (lambda (x)
                                                  (if (zerop (length x)) 
                                                      "“"  ;; opening double quote
                                                    (if (string-match "[a-zA-Z0-9]" (substring x 0 1))
                                                        "”"  ;; closing double quote
                                                      "“"))) sanitized))

      sanitized)))


;; (defun sync0-bibtex-db-query (query &rest args)
;;   "Execute QUERY with ARGS on the bibliographic database.
;; Returns the result of the query."
;;   (let ((connection (sync0-bibtex-db-get-database)))
;;     (sqlite-select connection query (apply 'vector args))))

(defun sync0-bibtex-db-query (query &rest args)
  "Execute QUERY with ARGS on the bibliographic database.
QUERY is a SQL string that may include placeholders for parameters.
ARGS should be a list of arguments that match the placeholders in QUERY.
Each argument will automatically be wrapped in a vector, so the user does not need to handle this.
Example usage:
  (sync0-bibtex-db-query \"SELECT title FROM entries WHERE citekey = ?\" \"some-citekey\")
  (sync0-bibtex-db-query \"SELECT * FROM extra_fields WHERE field = ? AND citekey = ?\" \"file\" \"some-citekey\")
Returns the result of the query as a list of rows."
  (let ((connection (sync0-bibtex-db-get-database)))
    (sqlite-select connection query (apply 'vector args))))

(defun sync0-bibtex-db-execute (query &rest args)
  "Execute a non-SELECT QUERY with ARGS on the bibliographic database.
QUERY is a SQL string that may include placeholders for parameters.
ARGS should be a list of arguments that match the placeholders in QUERY.
Each argument will automatically be wrapped in a vector.
Example usage:
  (sync0-bibtex-db-execute \"UPDATE entries SET title = ? WHERE citekey = ?\" \"New Title\" \"some-citekey\")
  (sync0-bibtex-db-execute \"INSERT INTO extra_fields (citekey, field, value) VALUES (?, ?, ?)\" \"some-citekey\" \"note\" \"Some note\")
Returns t if the execution was successful, nil otherwise."
  (let ((connection (sync0-bibtex-db-get-database)))
    (sqlite-execute connection query (apply 'vector args))))

(defun sync0-bibtex-db-rollback ()
  (interactive)
  (let ((db (sync0-bibtex-db-get-database)))
    (sqlite-rollback db)))

;; (defun sync0-bibtex-db-get-bibentry (citekey db)
;;   "Query the database DB for the value of bibentry in the extra_fields table for the given CITEKEY."
;;   (let ((result (sqlite-select db
;;                                "SELECT bibentry FROM extra_fields WHERE citekey = ?;"
;;                                (list citekey))))
;;     (if result
;;         (message "%s" (car (car result)))
;;       (message "No bibentry found for citekey %s." citekey))))

(defun sync0-bibtex-db-get-bibentry (citekey)
  "Query the database for the value of bibentry in the extra_fields table for the given CITEKEY."
  (let ((result (sync0-bibtex-db-query
                 "SELECT bibentry FROM extra_fields WHERE citekey = ?;" citekey)))
    (if result
        (message "%s" (caar result))
      (message "No bibentry found for citekey %s." citekey))))

;; (defun sync0-bibtex-db-column-exists-p (column-name table-name db)
;;   "Check if COLUMN-NAME exists in TABLE-NAME in the database DB."
;;   (let* ((check-query
;;           (format
;;            "SELECT COUNT(*) 
;;             FROM pragma_table_info('%s') 
;;             WHERE name = '%s';"
;;            table-name column-name))
;;          (exists (caar (sqlite-select db check-query))))
;;     (> exists 0)))

(defun sync0-bibtex-db-column-exists-p (column-name table-name)
  "Check if COLUMN-NAME exists in TABLE-NAME in the database."
  (let* ((query (format
                 "SELECT COUNT(*) 
                  FROM pragma_table_info('%s') 
                  WHERE name = ?;" table-name))
         (exists (caar (sync0-bibtex-db-query query column-name))))
    (> exists 0)))

(defun sync0-bibtex-db-add-column (column-name db)
  "Add a new COLUMN-NAME to the extra_fields table in DB, with user-defined column type.
The column type is selected interactively via completing-read. Checks if the column already exists first."
  (let* ((data-types '("TEXT" "INTEGER" "REAL" "BLOB" "NUMERIC"))
         (column-type (completing-read
                       (format "Choose a type for column '%s': " column-name)
                       data-types nil t)))
    (sqlite-execute db (format "ALTER TABLE extra_fields ADD COLUMN %s %s;"
                               column-name column-type))))

;; (defun sync0-bibtex-db-add-column (column-name)
;;   "Add a new COLUMN-NAME to the extra_fields table, with user-defined column type.
;; The column type is selected interactively via completing-read. Checks if the column already exists first."
;;   (unless (sync0-bibtex-db-column-exists-p column-name "extra_fields")
;;     (let* ((data-types '("TEXT" "INTEGER" "REAL" "BLOB" "NUMERIC"))
;;            (column-type (completing-read
;;                          (format "Choose a type for column '%s': " column-name)
;;                          data-types nil t)))
;;       (sync0-bibtex-db-execute
;;        (format "ALTER TABLE extra_fields ADD COLUMN %s %s;" column-name column-type)))))

;; (defun sync0-bibtex-db-get-person-id (person db)
;;   "Retrieve the PERSON_ID for a given PERSON name from the database DB.
;; If the person is not found, insert the person into the database and return the new ID."
;;   (let* ((query (format "SELECT person_id FROM people WHERE person_name = '%s';" person))
;; 	 (result (or (sqlite-select db query)
;; 		     (progn
;; 		       ;; Insert the person if not found
;; 		       (sqlite-execute db (format "INSERT INTO people (person_name) VALUES ('%s');" person))
;; 		       ;; Retrieve the new ID
;; 		       (sqlite-select db query)))))
;;     (caar result)))

(defun sync0-bibtex-db-get-person-id (person)
  "Retrieve the PERSON_ID for a given PERSON name from the database.
If the person is not found, insert the person into the database and return the new ID."
  (let* ((query "SELECT person_id FROM people WHERE person_name = ?;")
         (result (sync0-bibtex-db-query query person)))
    (if result
        (caar result)
      (progn
        ;; Insert the person if not found
        (sync0-bibtex-db-execute "INSERT INTO people (person_name) VALUES (?);" person)
        ;; Retrieve the new ID
        (caar (sync0-bibtex-db-query query person))))))

(defun sync0-bibtex-db-insert-person-role (citekey person role &optional roletype)
  "Insert or update a person with a specific ROLE for a given CITEKEY in the database.
ROLETYPE is optional."
  (let* ((person-id (sync0-bibtex-db-get-person-id person))
         (query (if roletype
                    "INSERT OR REPLACE INTO entry_people (citekey, person_id, role, roletype) VALUES (?, ?, ?, ?);"
                  "INSERT OR REPLACE INTO entry_people (citekey, person_id, role) VALUES (?, ?, ?);")))
    (if roletype
        (sync0-bibtex-db-execute query citekey person-id role roletype)
      (sync0-bibtex-db-execute query citekey person-id role))))

(defun sync0-bibtex-db-add-authors (authors)
  "Add a list of AUTHORS to the people table in the database DB.
If an author already exists, retrieve their person_id, otherwise insert them."
  (dolist (author authors)
    (sync0-bibtex-db-get-person-id author)))

(defun sync0-bibtex-db-update-people-roles (citekey)
  "Update the roles of people associated with a given CITEKEY in the database.
This function dynamically checks variables like `sync0-bibtex-entry-ROLE` 
(e.g., `sync0-bibtex-entry-author`) to update the `entry_people` table, including roletypes
from `sync0-bibtex-editortype-fields`."
  (let ((roles sync0-bibtex-people-fields)
        (roletypes sync0-bibtex-editortype-fields)
        (updated-roles '()) ;; Track updated roles
        (existing-roles (sync0-bibtex-db-query
                         "SELECT person_id, role, roletype FROM entry_people WHERE citekey = ?;" citekey)))
    ;; Convert existing roles into a hash table for efficient lookup
    (let ((existing-hash (make-hash-table :test 'equal)))
      (dolist (entry existing-roles)
        (puthash (list (nth 0 entry) (nth 1 entry) (nth 2 entry)) t existing-hash))
      ;; Process each role field
      (dolist (role roles)
        (let* ((field-symbol (intern (concat "sync0-bibtex-entry-" role)))
               (roletype-symbol (and (member role roletypes)
                                     (intern (concat "sync0-bibtex-entry-" (substring role 0 -4) "type"))))
               (raw-value (symbol-value field-symbol))
               (raw-roletype (and roletype-symbol (symbol-value roletype-symbol)))
               (people (and raw-value (split-string raw-value " and " t)))) ;; Split people names by " and "
          (when people
            (dolist (person people)
              (let* ((person-id (sync0-bibtex-db-get-person-id person))
                    (entry-key (list person-id role raw-roletype)))
                (if (gethash entry-key existing-hash)
                    ;; Already exists, no update needed; remove it from hash
                    (remhash entry-key existing-hash)
                  ;; Insert or update the person-role pair
                  (sync0-bibtex-db-insert-person-role citekey person role raw-roletype)
                  (push role updated-roles)))))))
      ;; Remove any remaining roles in the hash table (not in the updated list)
      (maphash
       (lambda (entry _)
         (let ((person-id (nth 0 entry))
               (role (nth 1 entry))
               (roletype (nth 2 entry)))
           (sync0-bibtex-db-execute
            "DELETE FROM entry_people WHERE citekey = ? AND person_id = ? AND role = ? AND roletype IS ?;"
            citekey person-id role roletype)))
       existing-hash)
      (message "Updated roles: %s"
               (mapconcat 'identity (delete-dups updated-roles) ", ")))))

(defun sync0-bibtex-db-get-keyword-id (keyword)
  "Retrieve the KEYWORD_ID for a given KEYWORD from the database.
If the keyword is not found, insert it into the database and return the new ID."
  (let* ((query "SELECT keyword_id FROM keywords WHERE keyword = ?;")
         (result (sync0-bibtex-db-query query keyword)))
    (if result
        (caar result)
      (progn
        ;; Insert the keyword if not found
        (sync0-bibtex-db-execute "INSERT INTO keywords (keyword) VALUES (?);" keyword)
        ;; Retrieve the new ID
        (caar (sync0-bibtex-db-query query keyword))))))

(defun sync0-bibtex-db-add-keywords (keywords)
  "Add a list of KEYWORDS to the keywords table in the database DB.
If a keyword already exists, retrieve its keyword_id, otherwise insert it."
  (dolist (keyword keywords)
    (sync0-bibtex-db-get-keyword-id keyword)))

;; ;; This function has been tested and works properly to insert the
;; ;; relations between people, their roles and the citekeys.
;; (defun sync0-bibtex-db-insert-people (citekey db)
;;   "Insert people from the predefined `sync0-bibtex-people-fields` into the database DB and associate with ENTRY."
;;   (dolist (people-field sync0-bibtex-people-fields)
;;     (let ((field-value (symbol-value (intern (concat "sync0-bibtex-entry-" people-field)))))
;;       (when field-value
;;         (let ((people-list (split-string field-value " and ")))
;;           ;; Handle case where there is only one person (no "and")
;;           (dolist (person people-list)
;;             ;; Check if the person already exists, if not, insert them
;;             (let ((person-id (sync0-bibtex-db-get-person-id person))
;; 		  (roletype (sync0-bibtex-get-editortype people-field)))  
;;               ;; Insert the person into `entry_people` with the correct role
;; 	      (if roletype
;;               (sync0-bibtex-db-insert-person-role citekey db person people-field roletype)
;;               (sync0-bibtex-db-insert-person-role citekey db person people-field)))))))))


(defun sync0-bibtex-db-insert-people (citekey)
  "Insert people from the predefined `sync0-bibtex-people-fields` into the database and associate with ENTRY."
  (dolist (people-field sync0-bibtex-people-fields)
    (let ((field-value (symbol-value (intern (concat "sync0-bibtex-entry-" people-field)))))
      (when field-value
        (let ((people-list (split-string field-value " and "))) ;; Split people by "and"
          (dolist (person people-list)
            (let* ((roletype (sync0-bibtex-get-editortype people-field)))
              (if roletype
                  (sync0-bibtex-db-insert-person-role citekey person people-field roletype)
                (sync0-bibtex-db-insert-person-role citekey person people-field)))))))))

;; (defun sync0-bibtex-db-insert-keywords (citekey db)
;;   "Insert keywords from the predefined `sync0-bibtex-people-theme`
;; into the database DB and associate with ENTRY."
;;   (when sync0-bibtex-entry-theme
;;     (let ((keywords-list (split-string sync0-bibtex-entry-theme ", ")))
;;       ;; Handle case where there is only one person (no "and")
;;       (dolist (keyword keywords-list)
;;         ;; Check if the person already exists, if not, insert them
;;         (when-let ((keyword-id (sync0-bibtex-db-get-keyword-id keyword)))
;;           ;; Insert the person into `entry_people` with the correct role
;; 	  (sqlite-execute db
;; 			  (format "INSERT INTO entry_keywords (citekey, keyword_id)
;;                              VALUES ('%s', %d);" citekey keyword-id)))))))

(defun sync0-bibtex-db-insert-keywords (citekey)
  "Insert keywords from the predefined `sync0-bibtex-entry-theme` into the database and associate with ENTRY."
  (when sync0-bibtex-entry-theme
    (let ((keywords-list (split-string sync0-bibtex-entry-theme ", "))) ;; Split keywords by ", "
      (dolist (keyword keywords-list)
        (when-let ((keyword-id (sync0-bibtex-db-get-keyword-id keyword)))
          (sync0-bibtex-db-execute
           "INSERT INTO entry_keywords (citekey, keyword_id) VALUES (?, ?);"
           citekey keyword-id))))))

(defun sync0-bibtex-db-update-keywords (citekey)
  "Update the keywords associated with a given CITEKEY in the database.
Keywords are taken from the `sync0-bibtex-entry-theme` variable."
  (let* ((raw-keywords (symbol-value 'sync0-bibtex-entry-theme)) ;; Extract keywords from the dummy variable
         (new-keywords (and raw-keywords (split-string raw-keywords ", " t))) ;; Split into a list
         (existing-keywords (mapcar #'car
                                     (sync0-bibtex-db-query
                                      "SELECT keyword FROM entry_keywords 
                                       INNER JOIN keywords 
                                       ON entry_keywords.keyword_id = keywords.keyword_id 
                                       WHERE citekey = ?;" citekey)))
         (new-keyword-ids (mapcar #'sync0-bibtex-db-get-keyword-id new-keywords))
         (existing-keyword-ids (mapcar #'sync0-bibtex-db-get-keyword-id existing-keywords)))
    ;; Add new keywords
    (dolist (keyword-id new-keyword-ids)
      (unless (member keyword-id existing-keyword-ids)
        (sync0-bibtex-db-execute
         "INSERT INTO entry_keywords (citekey, keyword_id) VALUES (?, ?);"
         citekey keyword-id)))
    ;; Remove old keywords that are no longer present
    (dolist (keyword-id existing-keyword-ids)
      (unless (member keyword-id new-keyword-ids)
        (sync0-bibtex-db-execute
         "DELETE FROM entry_keywords WHERE citekey = ? AND keyword_id = ?;"
         citekey keyword-id)))
    ;; Notify the user of the changes
    (message "Updated keywords for %s: %s"
             citekey (mapconcat 'identity new-keywords ", "))))

;; (defun sync0-bibtex-db-get-value (citekey field)
;;   "Retrieve the value of FIELD for the entry identified by CITEKEY in the database.
;; FIELD is checked in the `entries` table if it is one of the fixed fields.
;; Otherwise, it is looked up dynamically in the `extra_fields` table.
;; If FIELD does not exist in either table, an error message is shown."
;;   (let ((fixed-fields sync0-bibtex-db-entries-table-fields)
;;         (db (sync0-bibtex-db-get-database)))
;;     (cond
;;      ;; Check if it's a fixed field in the `entries` table
;;      ((member field fixed-fields)
;;       (let ((query (format "SELECT %s FROM entries WHERE citekey = ?" field)))
;;         (caar (sqlite-select db query (vector citekey)))))
;;      ;; Check if the column exists in the `extra_fields` table
;;      ((sync0-bibtex-db-column-exists-p field "extra_fields" db)
;;       (let ((query (format "SELECT %s FROM extra_fields WHERE citekey = ?" field)))
;;         (caar (sqlite-select db query (vector citekey)))))
;;      ;; Field not found in either table
;;      (t
;;       (message "Field '%s' not found in the database for citekey '%s'." field citekey)
;;       nil))))

(defun sync0-bibtex-db-insert-extra-fields (citekey db)
  (let* ((extra-fields sync0-bibtex-db-purged-extra-fields)
         (extra-columns '())
         (extra-values '()))
    (dolist (field extra-fields)
      (let* ((field-name (concat "sync0-bibtex-entry-" field))
             (field-value (symbol-value (intern field-name))))
        (when field-value
          (unless (sync0-bibtex-db-column-exists-p field "extra_fields")
            (sync0-bibtex-db-add-column field db))
	  (push field extra-columns)
	  (if (string= field "aliases")
	      (push (format "'%s'" field-value) extra-values)
	    (push (format "'%s'" (sync0-bibtex-db-sanitize-value field-value)) extra-values)))))

    ;; **Push bibentry into extra-fields**
    (push "bibentry" extra-columns)
    (push (format "'%s'" (sync0-bibtex-db-sanitize-value sync0-bibtex-entry-bibentry)) extra-values)

    (when extra-columns
      (let ((insert-extra-query
             (format "INSERT INTO extra_fields (citekey, %s) VALUES ('%s', %s);"
                     (mapconcat 'identity extra-columns ", ")
                     citekey
                     (mapconcat 'identity extra-values ", "))))
        (sqlite-execute db insert-extra-query)))))

(defun sync0-bibtex-db-get-value (citekey field)
  "Retrieve the value of FIELD for the entry identified by CITEKEY in the database.
FIELD is checked in the `entries` table if it is one of the fixed fields.
Otherwise, it is looked up dynamically in the `extra_fields` table.
If FIELD does not exist in either table, an error message is shown."
  (let ((fixed-fields sync0-bibtex-db-entries-table-fields))
    (cond
     ;; Check if it's a fixed field in the `entries` table
     ((member field fixed-fields)
      (caar (sync0-bibtex-db-query
             (format "SELECT %s FROM entries WHERE citekey = ?" field) citekey)))
     ;; Check if the column exists in the `extra_fields` table
     ((sync0-bibtex-db-column-exists-p field "extra_fields")
      (caar (sync0-bibtex-db-query
             (format "SELECT %s FROM extra_fields WHERE citekey = ?" field) citekey)))
     ;; Field not found in either table
     (t
      (message "Field '%s' not found in the database for citekey '%s'." field citekey)
      nil))))

(defun sync0-bibtex-db-query-citations (author journaltitle)
  "Query the bibliographic database for entries by AUTHOR in the journal JOURNALTITLE
and format the results into an Org-cite citation."
  (let* ((query "SELECT DISTINCT e.citekey
                 FROM entries e
                 JOIN entry_people ep ON e.citekey = ep.citekey
                 JOIN people p ON ep.person_id = p.person_id
                 LEFT JOIN extra_fields ef ON e.citekey = ef.citekey
                 WHERE p.person_name = ? AND ep.role = 'author' AND ef.journaltitle = ?")
         (bibkeys (sync0-bibtex-db-query query author journaltitle))  ; Execute the query with parameters
         (citations (mapconcat (lambda (citekey)
                                (format "@%s" (car citekey)))  ; Format each citekey as Org-cite
                              bibkeys ";")))  ; Join the formatted citations with commas
    (concat "[cite:" citations "]")))

(provide 'sync0-bibtex-sql-utils)
