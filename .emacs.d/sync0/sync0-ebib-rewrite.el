(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-var-functions)

(defun ebib-toggle-raw (field)
  "Toggle the \"special\" status of FIELD's contents."
  (let ((key (ebib--get-key-at-point)))
    (unless (member-ignore-case field '("=type=" "crossref" "xref" "related" "theme"))
      (let ((contents (ebib-get-field-value field key ebib--cur-db 'noerror)))
        (if (ebib--multiline-p contents) ; Multiline fields cannot be raw.
            (beep)
          (unless contents            ; If there is no value, the user can enter one,
            (ebib-edit-field field)   ; which we must then store unbraced.
            (setq contents (ebib-get-field-value field key ebib--cur-db 'noerror)))
          (when contents ; We must check to make sure the user entered some value.
            (ebib-set-field-value field contents key ebib--cur-db 'overwrite (not (ebib-unbraced-p contents)))
            (ebib--redisplay-field field)
            (ebib--set-modified t ebib--cur-db t (seq-filter (lambda (dependent)
                                                               (ebib-db-has-key key dependent))
                                                             (ebib--list-dependents ebib--cur-db)))))))))

(defun ebib--get-field-highlighted (field key &optional db match-str)
  "Return the contents of FIELD in entry KEY in DB with MATCH-STR highlighted."
  (or db (setq db ebib--cur-db))
  (let* ((case-fold-search t)
         (value (ebib-get-field-value field key db 'noerror nil 'xref 'expand-strings))
         (multiline "")
         (raw " ")
         (matched nil)
         (alias ""))
    ;; We have to do a couple of things now:
    ;; - Remove {} or "" around the value, if they're there.
    ;; - Search for `match-str'.
    ;; - Properly adjust the value if it's multiline.
    ;; But all this is not necessary if there was no value, so we test that first.
    (when value
      (if (get-text-property 0 'ebib--alias value)
          (setq alias (propertize (format "  [<== %s]" (cdr (assoc-string field ebib--field-aliases 'case-fold))) 'face 'ebib-alias-face)))
      (if (get-text-property 0 'ebib--expanded value)
          (setq value (propertize value 'face 'ebib-abbrev-face 'fontified t)))
      ;; Propertize any stretch of the value which has an `ebib--xref'
      ;; property with `ebib-crossref-face'
      (let* ((xref-lst (seq-filter (lambda (plist) (eq (caaddr plist) 'ebib--xref))
				   ;; This is a HACK, based on
				   ;; https://emacs.stackexchange.com/a/54181/34394
				   ;; FIXME Using `object-intervals' would be much quicker and more
				   ;; elegant here, but only once it's supported in a current
				   ;; version of Emacs!
				   (seq-partition (cdr-safe (read (substring (format "%S" value) 1))) 3)))
	     (int-lst (mapcar #'butlast xref-lst)))
	(mapc
	 (lambda (ints)
	   (add-text-properties
	    (car ints) (cadr ints)
	    `(face ebib-crossref-face
                   help-echo ,(format "Inherited from entry `%s'" (get-text-property (car ints) 'ebib--xref value)))
	    value))
	 int-lst))
      (if (and (member-ignore-case field '("crossref" "xref"))
               (not (ebib--find-db-for-key (ebib-unbrace value) ebib--cur-db)))
          (setq value (propertize value 'face 'ebib-warning-face)))
      (if (cl-equalp field "theme")
          (let* ((keywords (ebib--keywords-to-list (ebib-unbrace value)))
                 (new-value (mapconcat (lambda (keyword)
                                         (if (not (member-ignore-case keyword ebib--keywords-completion-list))
                                             (propertize keyword 'face 'ebib-warning-face)
                                           keyword))
                                       keywords
                                       ebib-keywords-separator)))
            (setq value (if (ebib-unbraced-p value)
                            new-value
                          (ebib-brace new-value)))))
      (if (ebib-unbraced-p value)
          (setq raw "*")
        (setq value (ebib-unbrace value))) ; We have to make the value look nice.
      (when match-str
        (cl-multiple-value-setq (value matched) (ebib--match-all-in-string match-str value)))
      (when (ebib--multiline-p value)
        (cl-multiple-value-setq (value multiline) (ebib--display-multiline-field value matched)))
      (if (cl-equalp field "file")
          (setq value (ebib--display-file-field value)))
      (if (cl-equalp field "related")
	  (setq value (ebib--display-related-field value)))
      (if (cl-equalp field "xdata")
          (setq value (ebib--display-xdata-field value)))
      (if (cl-equalp field "url")
          (setq value (ebib--display-url-field value)))
      (if (cl-equalp field "doi")
          (setq value (ebib--display-doi-field value)))
      (if (cl-equalp field "crossref")
          (setq value (ebib--display-crossref-field value))))
    (concat raw value alias multiline)))

(defun ebib--bib-read-entry (entry-type db &optional timestamp)
  "Read a BibTeX entry with type ENTRY-TYPE and store it in DB.
Return the entry key if an entry was found and could be stored,
nil otherwise.  Optional argument TIMESTAMP indicates whether a
timestamp is to be added.  (Whether a timestamp is actually added
also depends on `ebib-use-timestamp'.)"
  (let* ((beg (point)) ; Save the start of the entry in case something goes wrong.
         (entry (parsebib-read-entry entry-type)))
    (if entry
        (let ((entry-key (cdr (assoc-string "=key=" entry))))
          (if (ebib-db-dependent-p db)
              (if (ebib-db-has-key entry-key (ebib-db-get-main db))
                  (ebib-db-add-entries-to-dependent entry-key db)
                (ebib--log 'error "Entry key `%s' not found in main database" entry-key))
            (when (string= entry-key "")
              (setq entry-key (ebib--generate-tempkey db))
              (ebib--log 'warning "Line %d: Temporary key generated for entry." (line-number-at-pos beg)))
            (setq entry-key (ebib--store-entry entry-key entry db timestamp (if ebib-uniquify-keys 'uniquify 'noerror)))
            (when (and entry-key (not ebib-keywords))
              (if-let ((keywords (ebib-unbrace (cdr (assoc-string "theme" entry 'case-fold)))))
                  (mapc #'ebib--keywords-add-to-completion-list (ebib--keywords-to-list keywords))))
            (unless entry-key
              (ebib--log 'warning "Line %d: Entry `%s' duplicated. Skipping." (line-number-at-pos beg) entry-key))
            entry-key)) ; Return the entry key, or nil if no entry could be stored.
      (ebib--log 'warning "Line %d: Could not read a valid entry." (line-number-at-pos beg))
      nil))) ; Make sure to return nil upon failure.

(defun ebib--completing-read-keywords (collection)
  "Read keywords with completion from COLLECTION.
Return the keywords entered as a list.  If no keywords are
entered, the return value is nil."
  (let* ((prompt (format "Add keyword (%s to finish) [%%s]" (ebib--completion-finish-key 'completing-read))))
    (cl-loop for keyword = (completing-read (format prompt (mapconcat #'identity keywords " "))
					    collection nil nil nil 'ebib--keywords-history)
             until (string= keyword "nil")
             collecting keyword into keywords
             finally return keywords)))

(defun ebib-add-keywords-to-entry ()
  "Add keywords to the current entry.
If there are marked entries, the user is asked if they wish to
add keywords to all of them.  If not, the keywords are added to
the current entry."
  (interactive)
  (cl-flet ((add-keywords (entry-key keywords)
                          ;; KEYWORDS is a list of keywords to be added to entry ENTRY-KEY.
                          (let* ((conts (ebib-get-field-value "theme" entry-key ebib--cur-db 'noerror 'unbraced))
                                 (keywords-string (mapconcat #'identity keywords ebib-keywords-separator))
                                 (new-conts (if conts
                                                (concat conts ebib-keywords-separator keywords-string)
                                              keywords-string)))
                            (ebib-set-field-value "theme"
                                                  (if ebib-keywords-field-keep-sorted
                                                      (ebib--keywords-sort new-conts)
                                                    new-conts)
                                                  entry-key ebib--cur-db 'overwrite)
                            (ebib--maybe-add-keywords-to-canonical-list keywords))))
    (let* ((minibuffer-local-completion-map (make-composed-keymap '(keymap (32)) minibuffer-local-completion-map))
           (keywords (ebib--completing-read-keywords ebib--keywords-completion-list)))
      (when keywords
	(setq sync0-bibtex-entry-theme (sync0-show-elements-of-list keywords ", "))
	(sync0-bibtex-update-var "theme")
        (ebib--execute-when
          (entries
           (let ((to-be-modified (or (ebib-db-list-marked-entries ebib--cur-db)
                                     (ebib--get-key-at-point))))
             (if (and (listp to-be-modified) (y-or-n-p "Add keywords to all marked entries? "))
                 (progn
                   (dolist (entry to-be-modified)
                     (add-keywords entry keywords))
                   (message "Keywords added to marked entries."))
               (add-keywords to-be-modified keywords)
               (setq to-be-modified (list to-be-modified))) ; So we can use it in `seq-difference' below.
             (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
                                                              (= (ebib-db-count-entries dependent)
                                                                 (length (seq-difference (ebib-db-list-keys dependent) to-be-modified))))
                                                            (ebib--list-dependents ebib--cur-db)))))
          (default
            (beep)))
        (ebib--update-entry-buffer)))))

(defun ebib-add-canonical-keyword (pfx)
  "Add a keyword in the current entry to the list of canonical keywords.
With prefix argument PFX, add all keywords to the list of
canonical keywords."
  (interactive "P")
  ;; What this really does is add the keyword to the keyword completion list
  ;; (`ebib--keywords-completion-list') and mark this list as modified.  If the
  ;; user uses a canonical keywords list, this completion list will be saved as
  ;; the canonical list at the end of the session.  Hence the name of this
  ;; function, which is technically misleading.
  (let ((keywords (ebib--keywords-to-list (ebib-get-field-value "theme" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced))))
    (if pfx
        (mapc #'ebib--keywords-add-to-completion-list
              keywords)
      (let ((collection (seq-difference keywords ebib--keywords-completion-list)))
        (ebib--ifstring (keyword (completing-read "Add keyword to canonical list: " collection))
            (ebib--keywords-add-to-completion-list keyword)))))
  (ebib--update-entry-buffer))

(defun ebib-purge-keywords-field ()
  "Remove non-canonical keywords in the current entry.
Specifically, remove all keywords that are not on the list of
canonical keywords."
  (interactive)
  (let* ((keywords (ebib--keywords-to-list (ebib-get-field-value "theme" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced)))
         (canonical (seq-intersection keywords ebib--keywords-completion-list)))
    (unless (= (length keywords) (length canonical))
      (ebib-db-set-field-value "theme"
                               ;; `mapconcat' on an empty list returns an empty
                               ;; string, but if `canonical' is empty, we want to
                               ;; store nil, not "":
                               (if canonical
                                   (mapconcat #'identity canonical ebib-keywords-separator))
                               (ebib--get-key-at-point)
                               ebib--cur-db
                               'overwrite)
      (ebib--set-modified t ebib--cur-db t (ebib--list-dependents ebib--cur-db))
      (ebib--update-entry-buffer))))

(defun ebib--edit-separated-values-field (field-name fields init-contents)
  "Edit a \"separated values\" type field.
FIELD-NAME is the name of the field being edited.  FIELDS is a
list of fields from which to pull completion candidates.
INIT-CONTENTS is the original value of the field and should be a
Biblatex list (i.e., a string consisting of comma-separated
values).

See also `ebib-field-edit-functions'."
  ;; NOTE When used with BibLaTeX, this assumes that xsvsep is set to
  ;; "\s*,\s*", equivalent to "[[:space:]]*,[[:space:]]" in Emacs.
  (let* ((crm-local-completion-map (make-composed-keymap '(keymap (32)) crm-local-completion-map))
         (crm-separator "[[:space:]]*,[[:space:]]*")
         ;; If we're editing the "keywords" field, the completion candidates are
         ;; taken from `ebib--keywords-completion-list'. Otherwise, we collect
         ;; all the values for the current field in the currently open
         ;; databases. Since the field values are actually (Biblatex) list, we
         ;; need to split them on commas.))
	 (collection (if (string= field-name "theme")
                         ebib--keywords-completion-list
	               (delete-dups
	                (apply
	                 #'append
	                 (mapcar (lambda (str)
		                   (split-string str crm-separator t "[[:space:]]"))
		                 (ebib--create-collection-from-fields fields))))))
	 (result (completing-read-multiple
		  (format "%s: " field-name) collection nil nil
		  ;; Append a crm-separator to the result, so that
		  ;; `completing-read-multiple' treats it as a list of
		  ;; selected candidates
		  (when init-contents (concat init-contents ", ")))))
    (string-join result ", ")))



(provide 'sync0-ebib-rewrite)
