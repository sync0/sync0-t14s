;; (require 'all-the-icons)
(require 'sync0-projects)
(require 'sync0-bibtex-functions)
(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-var-functions)
(require 'sync0-ivy-bibtex-functions)

(defvar sync0-all-the-icons-file-max-width 2 "Max width of icons to display")

(defvar sync0-ebib-bibtex-default-separator ", " "Default separator for bibtex fields")

;; (setq ebib-extra-fields
;; '((biblatex "crossref" "xdata" "annotation" "abstract" "keywords" "file" "timestamp" "priority" "status" "theme" "cote" "note" "library" "origdate" "volume" "number" "pagetotal")
;;  (BibTeX "crossref" "annote" "abstract" "keywords" "file" "timestamp" "url" "doi")))

(setq ebib-extra-fields
'((biblatex "crossref" "xdata" "annotation" "keywords" "timestamp" "abstract" "description" "file" "priority" "status" "theme" "cote" "note" "library" "origdate" "volume" "number" "pagetotal" "project")
 (BibTeX "crossref" "annote" "abstract" "keywords" "file" "timestamp" "url" "doi")))

(setq ebib-hidden-fields
'("addendum" "afterword" "annotator" "archiveprefix" "bookauthor" "booksubtitle" "booktitleaddon" "chapter" "commentator" "edition" "editora" "editorb" "editorc" "eid" "eprint" "eprintclass" "eprinttype" "eventdate" "eventtitle" "foreword" "holder" "howpublished" "introduction" "isbn" "isrn" "issn" "issue" "issuesubtitle" "issuetitle" "issuetitleaddon" "journaltitleadddon" "journalsubtitle" "language" "location" "mainsubtitle" "maintitle" "maintitleaddon" "month" "origlanguage" "pagetotal" "part" "primaryclass" "remark" "timestamp" "titleaddon" "translator" "urldate" "venue" "version" "volumes" "annotation" "keywords" "xdata" "people" "created" "century" "langid" "pubstate"))

;; (defun sync0-ebib--notes-list-files ()
;;   "Return a list of notes files.
;; List all the files in `sync0-ebib-notes-locations' and all files in the
;; directories in `ebib-notes-locations' that have the extension in
;; `ebib-notes-file-extension'."
;;   (cond
;;    ;; If `ebib-notes-locations' is nil, we don't need to do all this, but we
;;    ;; still need to check `ebib-notes-default-file' (see below).
;;    (ebib-notes-locations
;;     (cl-flet ((list-files (loc)
;;                           (cond
;;                            ((file-directory-p loc)
;;                             (directory-files loc 'full (concat (regexp-quote ebib-notes-file-extension) "\\'") 'nosort))
;;                            ((string= (downcase (file-name-extension loc)) "org")
;;                             (list loc)))))
;;       (seq-reduce (lambda (lst loc)
;;                     (append (list-files loc) lst))
;;                   ebib-notes-locations (if ebib-notes-default-file
;;                                            (list ebib-notes-default-file)
;;                                          '()))))
;;    (ebib-notes-default-file
;;     (list ebib-notes-default-file))))

;; (ebib--notes-list-files)

;; a small convenience function to import into ebib from the clipboard
(defun sync0-ebib-import-from-clipboard ()
  "Attempt to import the contents in the kill ring/clipboard into `ebib'."
  (interactive)
  (with-temp-buffer
    (yank)
    (ebib-import-entries)
    (call-interactively #'ebib)))

(defun sync0-ebib-visit-entry-in-bib-file ()
  "Visit the corresponding entry in the BibTeX file being viewed in Ebib."
  (interactive)
  (when (and (eq major-mode 'ebib-index-mode)
             ebib--cur-db)
    (let* ((key (ebib--get-key-at-point))
	   (db ebib--cur-db)
	   (entry (ebib-get-entry key db  'noerror 'xref))
           (type (cdr (assoc "=type=" entry)))
           (bib-file (ebib-db-get-filename db)))
      (when (and key bib-file)
        (find-file bib-file)
        (goto-char (point-min))
        (when (re-search-forward (format "@%s{%s" type (regexp-quote key)) nil t)
          (recenter-top-bottom))))))

;; (defun ebib-choose-attachment ()
;;   "Choose an attachment to open for the current entry in Ebib."
;;   (interactive)
;;   (when (eq major-mode 'ebib-index-mode)
;;     (let* ((key (ebib--get-key-at-point))
;; 	   (db ebib--cur-db)
;; 	   ;; (entry (ebib-get-entry key db  'noerror 'xref))
;;            ;; (file-field (cdr (assoc "=file=" entry)))
;;            (file-field (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
;;       (when file-field
;;         (let* ((attach-list (split-string file-field ";"))
;;                (chosen-attachment (completing-read "Choose an attachment to open: " attach-list))
;;                (chosen-attachment-fix (substring ))
;;                (program	(completing-read "Which softare to open attachment ?" sync0-bibtex-attachment-programs)))
;;           (when chosen-attachment
;;              (call-process program nil 0 nil chosen-attachment-fix)))))))

(defun sync0-ebib-choose-attachment ()
  "Choose an attachment to open for the current entry in Ebib."
  (interactive)
  (when (eq major-mode 'ebib-index-mode)
    (let* ((key (ebib--get-key-at-point))
           (db ebib--cur-db)
           (file-field (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
      (when file-field
        (let* ((attach-list (split-string file-field ";"))
               (chosen-attachment (if (> (length attach-list) 1)
				      (completing-read "Choose an attachment to open: " attach-list)
				    (car attach-list)))
               (chosen-attachment-fix (if (string-match ":\\(.*\\)\\:[^.]*$" chosen-attachment)
                                           (match-string 1 chosen-attachment)
                                        chosen-attachment))
               (extension (file-name-extension chosen-attachment-fix))
	       (program (if (assoc extension sync0-default-file-associations)
                            (cdr (assoc extension sync0-default-file-associations))
			  (completing-read "Which software to open attachment with? " sync0-bibtex-attachment-programs))))
          (if chosen-attachment
              (call-process program nil 0 nil chosen-attachment-fix)
	     (message "No attachment found for key %s" key)))))))

(defun sync0-ebib-open-notes-at-point ()
  "Choose an attachment to open for the current entry in Ebib."
  (interactive)
  (when (eq major-mode 'ebib-index-mode)
    (let* ((key (ebib--get-key-at-point))
           (notes-file  (concat sync0-zettelkasten-references-directory key ".md")))
      (if (file-exists-p notes-file)
          (find-file notes-file)
	(progn
	  (sync0-bibtex-completion-load-entry key)
          (sync0-bibtex-entry-create-obsidian-note-from-entry key))))))

;; (defun ebib-download-pdf-from-doi ()
;;   "Download a PDF for the current entry."
;;   (interactive)
;;   (let* ((key (ebib--get-key-at-point))
;;          (doi (ebib-get-field-value "doi" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (publisher (ebib-get-field-value "publisher" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (eprinttype (ebib-get-field-value "eprinttype" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (eprint (ebib-get-field-value "eprint" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (journal (ebib-get-field-value "journal" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (journaltitle (ebib-get-field-value "journaltitle" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (organization (ebib-get-field-value "organization" key ebib--cur-db 'noerror 'unbraced 'xref))
;;          (url (ebib-get-field-value "url" key ebib--cur-db 'noerror 'unbraced 'xref)))
;;     (unless key
;;       (error "[Ebib] No key assigned to entry"))
;;     (download-pdf-from-doi key doi (or publisher eprinttype) eprint (or journal journaltitle) organization url)))

;; The following functions are taken from the response to an issue I
;; openened https://github.com/joostkremers/ebib/issues/291

(defun sync0-ebib-view-file (arg)
  "View a file specified in the 'file' field of the current entry in Ebib.
The 'file' field may contain more than one filename. In that case,
a numeric prefix argument ARG can be used to specify which file to choose."
  (interactive "P")
  (when (eq major-mode 'ebib-index-mode)
    (let* ((key (ebib--get-key-at-point))
           (db ebib--cur-db)
           (file-field (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
      (when file-field
        (let* ((attach-list (split-string file-field ";"))
               (chosen-attachment (if (> (length attach-list) 1)
                                      (completing-read "Choose an attachment to open: " attach-list)
                                    (car attach-list)))
               (chosen-attachment-fix (if (string-match ":\\(.*\\)\\:[^.]*$" chosen-attachment)
                                           (match-string 1 chosen-attachment)
                                         chosen-attachment))
               (extension (file-name-extension chosen-attachment-fix))
               (program (if (assoc extension sync0-default-file-associations)
                            (cdr (assoc extension sync0-default-file-associations))
                          (completing-read "Which software to open attachment with? " sync0-bibtex-attachment-programs))))
          (if chosen-attachment
              (call-process program nil 0 nil chosen-attachment-fix)
            (message "No attachment found for key %s" key)))))))

;; (defun sync0-ebib-display-status-icon (test icon action &optional button-data)
;;   "If TEST, display ICON with ACTION and BUTTON-DATA.
;; TEST can be:
;; - a symbol for a function, which will be called with no args.
;; - a variable
;; Either way, after calling/evaluation, ICON is displayed only if the
;; result is non-nil.
;; ICON should a single-character string.
;; ICON's text-property 'action is set to this ACTION. It is run
;; when the icon is clicked.
;; ICON's text-property 'button-data is set to BUTTON-DATA.
;; See info node `(elisp) Manipulating Buttons' for a description of how
;; these properties work."
;;   (if (if (functionp test) (funcall test) test)
;;       (propertize icon
;;                   'mouse-face 'highlight
;;                   'button t
;;                   'follow-link t
;;                   'category t
;;                   'keymap button-map
;;                   'button-data button-data
;;                   'action action)
;;     (let ((width (string-pixel-width icon)))
;;       (propertize " " 'display `(space . (:width (,width)))))))

(defun sync0-ebib-display-status-icon (test icon action &optional button-data)
  "If TEST, display ICON with ACTION and BUTTON-DATA.
TEST can be:
- a symbol for a function, which will be called with no args.
- a variable
Either way, after calling/evaluation, ICON is displayed only if the
result is non-nil.
ICON should a single-character string.
ICON's text-property 'action is set to this ACTION. It is run
when the icon is clicked.
ICON's text-property 'button-data is set to BUTTON-DATA.
See info node `(elisp) Manipulating Buttons' for a description of how
these properties work."
  (if (if (functionp test) (funcall test) test)
      (propertize icon
                  'mouse-face 'highlight
                  'button t
                  'follow-link t
                  'category t
                  'keymap button-map
                  'button-data button-data
                  'action action)
    icon))

;; (defun sync0-ebib-display-file-status (_ key db)
;;   "Display 'F' if entry has a file.
;; Always returns 'F'."
;;   (let* ((file-val (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref))
;; 	 (icon (sync0-ebib-display-status-icon
;; 		file-val ;; `sync0-ebib-display-status-icon' test whether to display
;; 		;; `sync0-ebib-display-status-icon': icon
;; 		(propertize "F" 'face '(:foreground red))
;; 		'sync0-ebib-view-file))
;;          (icon-width (string-pixel-width icon)))
;;     ;; Return icon (unmodified) followed by a space, propertized
;;     ;; such that the whole string is the same width as the icon.
;;     (concat
;;      icon
;;      (propertize
;;       " "
;;       'display `(space . (:width (,(- 1 icon-width))))))))

;; (defun sync0-ebib-display-file-status (_ key db)
;;   "Display 'F' if entry has a file.
;; Always returns 'F'."
;;   (let ((file-val (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
;;     (sync0-ebib-display-status-icon
;;      file-val
;;      (propertize "F" 'face '(:foreground red))
;;      'sync0-ebib-view-file)))

(defun sync0-ebib-display-file-status (_ key db)
  "Display 'F' if entry has a file.
Always returns 'F'."
  (let ((file-val (ebib-get-field-value "file" key db 'noerror 'unbraced 'xref)))
    (sync0-ebib-display-status-icon
     file-val
     (propertize "F" 'face '(:weight bold)) ;; Change here
     'sync0-ebib-view-file)))

(add-to-list 'ebib-field-transformation-functions '("File" . sync0-ebib-display-file-status))

;; (defun sync0-ebib-display-biblatex-type (_ key db)
;;   "Display biblatex type of entry."
;;   (let ((val (ebib-get-field-value "=type=" key db 'noerror 'unbraced 'xref)))
;;     ;; Return "P" followed by priority value if it exists, or an empty string otherwise.
;;     (if val
;; 	val
;;       "")))

(defun sync0-ebib-display-biblatex-type (_ key db)
  "Display shortened biblatex type of entry."
  (let ((val (ebib-get-field-value "=type=" key db 'noerror 'unbraced 'xref)))
    (cond
     ((equal val "Article") "ART")
     ((equal val "Book") "BOK")
     ((equal val "InBook") "INB")
     ((equal val "Collection") "COL")
     ((equal val "InCollection") "INC")
     ((equal val "Proceedings") "PRC")
     ((equal val "InProceedings") "INP")
     ((equal val "Report") "RPT")
     ((equal val "Thesis") "THS")
     ((equal val "Misc") "MSC")
     ((equal val "Unpublished") "UNP")
     ((equal val "MvBook") "MBK")
     ((equal val "MvCollection") "MCL")
     ((equal val "Manual") "MAN")
     ((equal val "Online") "WWW")
     (t "???"))))

(add-to-list 'ebib-field-transformation-functions '("Type" . sync0-ebib-display-biblatex-type))

(defun sync0-ebib-display-priority (_ key db)
  "Display priority if entry has a file."
  (let ((val (ebib-get-field-value "priority" key db 'noerror 'unbraced 'xref)))
    ;; Return "P" followed by priority value if it exists, or an empty string otherwise.
    (if val
        (concat "P" val)
      "")))

(add-to-list 'ebib-field-transformation-functions '("Priority" . sync0-ebib-display-priority))

(defun sync0-ebib-display-edition (_ key db)
  "Display priority if entry has a file."
  (let ((val (ebib-get-field-value "edition" key db 'noerror 'unbraced 'xref)))
    ;; Return "P" followed by priority value if it exists, or an empty string otherwise.
    (if val
        (concat val "e")
      "")))

(add-to-list 'ebib-field-transformation-functions '("Edition" . sync0-ebib-display-edition))

(defun sync0-ebib-display-origdate (_ key db)
  "Display priority if entry has a file."
  (let ((val (ebib-get-field-value "origdate" key db 'noerror 'unbraced 'xref)))
    ;; Return "P" followed by priority value if it exists, or an empty string otherwise.
    (if val
        (concat "[" val "]")
      "")))

(add-to-list 'ebib-field-transformation-functions '("Origdate" . sync0-ebib-display-origdate))

(defun sync0-ebib-display-volume (_ key db)
  "Display priority if entry has a file."
  (let ((val (ebib-get-field-value "volume" key db 'noerror 'unbraced 'xref)))
    ;; Return "P" followed by priority value if it exists, or an empty string otherwise.
    (if val
        (concat "V." val)
      "")))

(add-to-list 'ebib-field-transformation-functions '("Volume" . sync0-ebib-display-volume))

(defun sync0-ebib-display-number (_ key db)
  "Display priority if entry has a file."
  (let ((val (ebib-get-field-value "number" key db 'noerror 'unbraced 'xref)))
    ;; Return "P" followed by priority value if it exists, or an empty string otherwise.
    (if val
        (concat "N." val)
      "")))

(add-to-list 'ebib-field-transformation-functions '("Number" . sync0-ebib-display-number))

;; (defun ebib-adjust-priority (delta)
;;   "Increase or decrease the priority of the current entry by DELTA.
;; If DELTA is positive, the priority is increased; if negative, it's decreased.
;; If the priority field is not defined for an entry, set it to 1 when increasing,
;; or display a message when decreasing."
;;   (interactive "p")
;;   (cl-flet ((adjust-priority (entry-key delta)
;;                               ;; Adjusts the priority of entry ENTRY-KEY by DELTA.
;;                               (let* ((priority-str (ebib-get-field-value "priority" entry-key ebib--cur-db 'noerror 'unbraced))
;;                                      (priority (and priority-str (string-to-number priority-str)))
;;                                      (new-priority (+ (or priority 0) delta)))
;;                                 (cond
;;                                  ((<= new-priority 0)
;;                                   (ebib-delete-field-value "priority" entry-key ebib--cur-db)
;;                                   (message "Priority removed for %s" entry-key))
;;                                  ((<= 1 new-priority 5)
;;                                   (ebib-set-field-value "priority" (number-to-string new-priority) entry-key ebib--cur-db 'overwrite)
;;                                   (message "Priority adjusted to %d for %s" new-priority entry-key))
;;                                  (t
;;                                   (message "Priority already at maximum for %s" entry-key))))))
;;     (let ((entry-key (ebib--get-key-at-point)))
;;       (when entry-key
;;         (adjust-priority entry-key delta)
;;         (ebib--update-entry-buffer)))))

;; (defun ebib-increase-priority ()
;;   "Increase the priority of the current entry by 1.
;; If the priority field is not defined, set it to 1."
;;   (interactive)
;;   (ebib-adjust-priority 1))

;; (defun ebib-decrease-priority ()
;;   "Decrease the priority of the current entry by 1.
;; If the priority field is not defined, display a message."
;;   (interactive)
;;   (ebib-adjust-priority -1))

(defun sync0-ebib-adjust-priority (entry-key delta)
  "Increase or decrease the priority of ENTRY-KEY by DELTA.
If DELTA is positive, the priority is increased; if negative, it's decreased.
If the priority field is not defined for an entry, set it to 1 when increasing,
or display a message when decreasing."
  (interactive)
  (let* ((db ebib--cur-db)
	 (priority-str (ebib-get-field-value "priority" entry-key db 'noerror 'unbraced 'xref))
         (priority (and priority-str (string-to-number priority-str)))
         (new-priority (+ (or priority 0) delta)))
    (cond
     ((<= new-priority 0)
      (ebib-db-remove-field-value "priority" entry-key db)
      (ebib--update-entry-buffer)
      (ebib--set-modified t db)
      (message "Priority removed for %s" entry-key))
     ((<= 1 new-priority 5)
      (ebib-set-field-value "priority" (number-to-string new-priority) entry-key db 'overwrite)
      (ebib--update-entry-buffer)
      (ebib--set-modified t db)
      (message "Priority adjusted to %d for %s" new-priority entry-key))
     (t
      (message "Priority already at maximum for %s" entry-key)))))

(defun sync0-ebib-increase-priority ()
  "Increase the priority of the current entry by 1.
If the priority field is not defined, set it to 1."
  (interactive)
  (let ((entry-key (ebib--get-key-at-point)))
    (when entry-key
      (sync0-ebib-adjust-priority entry-key 1))))

(defun sync0-ebib-decrease-priority ()
  "Decrease the priority of the current entry by 1.
If the priority field is not defined, display a message."
  (interactive)
  (let ((entry-key (ebib--get-key-at-point)))
    (when entry-key
      (sync0-ebib-adjust-priority entry-key -1))))

;; (defun ebib-set-status ()
;;   "Change or set the value of the 'status' field for the entry at point.
;; If 'status' field is not defined, prompt the user to set it to one of the possible values from 'sync0-bibtex-completion-status'.
;; If the user chooses 'nil', remove the 'status' field from the entry."
;;   (interactive)
;;   (let* ((entry-key (ebib--get-key-at-point))
;;          (status-choices (cons "nil" sync0-bibtex-completion-status))
;;          (new-status (completing-read "Choose new status: " status-choices)))
;;     (when entry-key
;;       (if (string= new-status "nil")
;;           (ebib-delete-field-value "status" entry-key ebib--cur-db)
;;         (if (ebib-get-field-value "status" entry-key ebib--cur-db 'noerror)
;;             (ebib-set-field-value "status" new-status entry-key ebib--cur-db 'overwrite)
;;           (ebib-set-field-value "status" new-status entry-key ebib--cur-db 'append)))
;;       (message "Status set to '%s' for %s" new-status entry-key)
;;       (ebib--update-entry-buffer))))

;; (defun ebib-set-status ()
;;   "Change or set the value of the 'status' field for the entry at point.
;; If 'status' field is not defined, prompt the user to set it to one of the possible values from 'sync0-bibtex-completion-status'.
;; If the user chooses 'nil', remove the 'status' field from the entry."
;;   (interactive)
;;   (let* ((entry-key (ebib--get-key-at-point))
;;          (status-choices (cons "nil" sync0-bibtex-completion-status))
;;          (new-status (completing-read "Choose new status: " status-choices)))
;;     (when entry-key
;;       (if (string= new-status "nil")
;;           (ebib-delete-field-value "status" entry-key ebib--cur-db)
;;         (if (ebib-get-field-value "status" entry-key ebib--cur-db 'noerror)
;;             (ebib-set-field-value "status" new-status entry-key ebib--cur-db 'overwrite)
;;           (ebib-set-field-value "status" new-status entry-key ebib--cur-db 'append)))
;;       (message "Status set to '%s' for %s" new-status entry-key)
;;       (ebib--save-database) ; Save the changes to the BibTeX file
;;       (ebib--update-entry-buffer))))

(defun sync0-ebib-set-status ()
  "Change or set the value of the 'status' field for the entry at point.
If 'status' field is not defined, prompt the user to set it to one of the possible values from 'sync0-bibtex-completion-status'.
If the user chooses 'nil', remove the 'status' field from the entry."
  (interactive)
  (when-let* ((entry-key (ebib--get-key-at-point))
              (status-choices (cons "nil" sync0-bibtex-completion-status))
              (new-status (completing-read "Choose new status: " status-choices)))
    (setq sync0-bibtex-entry-status new-status)
    (sync0-bibtex-update-var "status")
    (ebib--execute-when
      (entries
       (let ((to-be-modified (or (ebib-db-list-marked-entries ebib--cur-db)
                                 (ebib--get-key-at-point))))
         (if (and (listp to-be-modified) (y-or-n-p "Set status for all marked entries? "))
             (progn
               (dolist (entry to-be-modified)
                 (sync0-ebib-set-status-for-entry entry new-status))
               (message "Status set to '%s' for marked entries." new-status))
           (sync0-ebib-set-status-for-entry to-be-modified new-status))
         (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
                                                          (= (ebib-db-count-entries dependent)
                                                             (length (seq-difference (ebib-db-list-keys dependent) to-be-modified))))
                                                        (ebib--list-dependents ebib--cur-db)))))
      (default
       (beep))))
  (ebib--update-entry-buffer))

(defun sync0-ebib-set-status-for-entry (entry-key new-status)
  "Set the status for ENTRY-KEY to NEW-STATUS."
  (if (string= new-status "nil")
      (ebib-db-remove-field-value "status" entry-key ebib--cur-db)
    (if (ebib-get-field-value "status" entry-key ebib--cur-db 'noerror)
        (ebib-set-field-value "status" new-status entry-key ebib--cur-db 'overwrite)
      (ebib-set-field-value "status" new-status entry-key ebib--cur-db 'append))))

(defun sync0-ebib-set-priority ()
  "Change or set the value of the 'status' field for the entry at point.
If 'status' field is not defined, prompt the user to set it to one of the possible values from 'sync0-bibtex-completion-status'.
If the user chooses 'nil', remove the 'status' field from the entry."
  (interactive)
  (let* ((entry-key (ebib--get-key-at-point))
         (status-choices (cons "nil" sync0-bibtex-completion-priority))
         (new-status (completing-read "Choose new status: " status-choices)))
    (when entry-key
      (ebib--execute-when
        (entries
         (let ((to-be-modified (or (ebib-db-list-marked-entries ebib--cur-db)
                                   (ebib--get-key-at-point))))
           (if (and (listp to-be-modified) (y-or-n-p "Set priority for all marked entries? "))
               (progn
                 (dolist (entry to-be-modified)
                   (sync0-ebib-set-priority-for-entry entry new-status))
                 (message "Priority set to '%s' for marked entries." new-status))
             (sync0-ebib-set-priority-for-entry to-be-modified new-status))
           (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
                                                              (= (ebib-db-count-entries dependent)
                                                                 (length (seq-difference (ebib-db-list-keys dependent) to-be-modified))))
                                                            (ebib--list-dependents ebib--cur-db)))))
        (default
          (beep))))
    (ebib--update-entry-buffer)))

(defun sync0-ebib-set-priority-for-entry (entry-key new-status)
  "Set the status for ENTRY-KEY to NEW-STATUS."
  (if (string= new-status "nil")
      (ebib-db-remove-field-value "priority" entry-key ebib--cur-db)
    (if (ebib-get-field-value "priority" entry-key ebib--cur-db 'noerror)
        (ebib-set-field-value "priority" new-status entry-key ebib--cur-db 'overwrite)
      (ebib-set-field-value "priority" new-status entry-key ebib--cur-db 'append))))

(defvar ebib--projects-history nil "Minibuffer history for projects.")

(defun sync0-ebib-add-projects-to-entry ()
  "Add projects to the current entry.
If there are marked entries, the user is asked if they wish to
add projects to all of them.  If not, the projects are added to
the current entry."
  (interactive)
  (cl-flet ((add-projects (entry-key projects)
                           ;; PROJECTS is a list of projects to be added to entry ENTRY-KEY.
                           (let* ((conts (ebib-get-field-value "project" entry-key ebib--cur-db 'noerror 'unbraced))
                                  (projects-string (mapconcat #'identity projects ", "))
                                  (new-conts (if conts
                                                 (concat conts ", " projects-string)
                                               projects-string)))
                            (ebib-set-field-value "project"
                                                  (if ebib-keywords-field-keep-sorted
                                                      (ebib--keywords-sort new-conts)
                                                    new-conts)
                                                  entry-key ebib--cur-db 'overwrite))))
    (let* ((minibuffer-local-completion-map (make-composed-keymap '(keymap (32)) minibuffer-local-completion-map))
	   (projects-to-display (mapcar (lambda (proj)
                                          (or (cdr (assoc proj sync0-projects-alist)) proj))
                                        sync0-bibtex-completion-project))
           (projects (sync0-completing-read-projects projects-to-display)))
      (when projects
	(setq sync0-bibtex-entry-project (sync0-show-elements-of-list projects ", "))
	(sync0-bibtex-update-var "project")
        (ebib--execute-when
          (entries
           (let ((to-be-modified (or (ebib-db-list-marked-entries ebib--cur-db)
                                     (ebib--get-key-at-point))))
             (if (and (listp to-be-modified) (y-or-n-p "Add projects to all marked entries? "))
                 (progn
                   (dolist (entry to-be-modified)
                     (add-projects entry projects))
                   (message "Projects added to marked entries."))
               (add-projects to-be-modified projects)
               (setq to-be-modified (list to-be-modified))) ; So we can use it in `seq-difference' below.
             (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
                                                              (= (ebib-db-count-entries dependent)
                                                                 (length (seq-difference (ebib-db-list-keys dependent) to-be-modified))))
                                                            (ebib--list-dependents ebib--cur-db)))))
          (default
            (beep)))
        (ebib--update-entry-buffer)))))

(defun sync0-ebib-move-entry-to-bibliography-for-entry (entry-key new-bibliography)
  "Move the biblatex ENTRY-KEY to a different BIBLIOGRAPHY."
    (sync0-bibtex-move-entry-to-bibfile entry-key new-bibliography)
    (ebib-db-remove-entry entry-key ebib--cur-db)
    (message "Entry moved to '%s' bibliography." new-bibliography))

(defun sync0-ebib-move-entry-to-bibliography ()
  "Move the biblatex entry at point to a different bibliography.
Prompt the user to choose one of the bibliographies defined in `sync0-bibtex-bibliographies`."
  (interactive)
  (let* ((entry-key (ebib--get-key-at-point))
         (bibliography-choices sync0-bibtex-bibliographies)
         (new-bibliography (completing-read "Choose new bibliography: " bibliography-choices)))
    (when entry-key
      (ebib--execute-when
       (entries
        (let ((to-be-modified (or (ebib-db-list-marked-entries ebib--cur-db)
                                  (ebib--get-key-at-point))))
          (if (and (listp to-be-modified) (y-or-n-p "Move entry to selected bibliography for all marked entries? "))
              (progn
                (dolist (entry to-be-modified)
                  (sync0-ebib-move-entry-to-bibliography-for-entry entry new-bibliography))
                (message "Entry moved to '%s' bibliography for marked entries." new-bibliography))
            (sync0-ebib-move-entry-to-bibliography-for-entry to-be-modified new-bibliography))
          (ebib--set-modified t ebib--cur-db (seq-filter (lambda (dependent)
                                                             (= (ebib-db-count-entries dependent)
                                                                (length (seq-difference (ebib-db-list-keys dependent) to-be-modified))))
                                                           (ebib--list-dependents ebib--cur-db)))))
       (default
         (beep))))
    (ebib--update-entry-buffer)))

;; (defun ebib-move-entry-to-bibliography-for-entry (entry-key new-bibliography)
;;   "Move the biblatex ENTRY-KEY to a different BIBLIOGRAPHY."
;;   (let ((current-bibliography (ebib-db-get-filename (ebib-db-get-main ebib--cur-db))))
;;     (when current-bibliography
;;         (let ((new-entry-key (ebib-create-entry)))
;;           ;; (ebib--copy-fields-to-entry entry-key new-entry-key)
;;           ;; (ebib--maybe-remove-crossref-from-entry new-entry-key)
;;           ;; (ebib-set-field-value "=type=" new-bibliography new-entry-key ebib--cur-db 'noerror 'unbraced 'xref)
;;           (ebib-db-remove-entry entry-key ebib--cur-db)
;;           (message "Entry moved to '%s' bibliography." new-bibliography)))))

;; (defun ebib-ivy-bibtex-action ()
;;   "Choose an action from `sync0-ivy-bibtex-actions` and execute it on the selected BibTeX entries."
;;   (interactive)
;;   (let* ((action (ivy-read "Choose action: " sync0-ivy-bibtex-actions
;;                             :require-match t
;;                             :sort t
;;                             :caller 'ebib-ivy-bibtex-action))
;;          (action-func (substring (nth 1 action) 4 -))
;;          (keys (if (ebib-buffer-has-marked-entries)
;;                    (ebib--get-marked-entries)
;;                  (list (ebib--get-key-at-point)))))
;;     (funcall action-func keys)))

;; (defun ebib-ivy-bibtex-action ()
;;   "Choose an action from `sync0-ivy-bibtex-actions` and execute it on the selected BibTeX entries."
;;   (interactive)
;;   (let* ((key (ivy-read "Choose action: " (mapcar #'car sync0-ivy-bibtex-actions)
;;                         :require-match t
;;                         :sort t
;;                         :caller 'ebib-ivy-bibtex-action))
;;          (action (assoc key sync0-ivy-bibtex-actions))
;;          (action-func (nth 3 action))
;; 	 (biby (substring (symbol-name action-func) 4))
;; 	 (biby-two (replace-regexp-in-string "bibtex" "bibtex-completion" biby))
;; 	 (keys (or (ebib-db-list-marked-entries ebib--cur-db)
;;                    (list (ebib--get-key-at-point)))))
;;     (funcall (intern biby-two) keys)))

;; (defun ebib-ivy-bibtex-action ()
;;   "Choose an action from `sync0-ivy-bibtex-actions` and execute it on the selected BibTeX entries."
;;   (interactive)
;;   (let* ((action-choices (mapcar (lambda (action)
;;                                    (list (format "[%s] (%s) %s" (car action) (cadr action) (caddr action))
;;                                          (car action)))
;;                                  sync0-ivy-bibtex-actions))
;;          (key (ivy-read "Choose action: " action-choices
;;                         :require-match t
;;                         :sort t
;;                         :caller 'ebib-ivy-bibtex-action))
;;          (action (assoc key sync0-ivy-bibtex-actions))
;;          (action-func (nth 3 action))
;;          (biby (substring (symbol-name action-func) 4))
;;          (biby-two (replace-regexp-in-string "bibtex" "bibtex-completion" biby))
;;          (keys (or (ebib-db-list-marked-entries ebib--cur-db)
;;                    (list (ebib--get-key-at-point)))))
;;     (funcall (intern biby-two) keys)))

(defun sync0-ebib-ivy-bibtex-action ()
  "Choose an action from `sync0-ivy-bibtex-actions` and execute it on the selected BibTeX entries."
  (interactive)
  (let* ((action-choices (mapcar (lambda (action)
                                   (cons (format "[%s] (%s) %s" (car action) (cadr action) (caddr action))
					 (car action)))
				 sync0-ivy-bibtex-actions))
	 (choice (ivy-read "Choose action: " action-choices
                           :require-match t
                           :sort t
                           :caller 'sync0-ebib-ivy-bibtex-action))
	 (key (substring choice 1 2))
	 (action (assoc key sync0-ivy-bibtex-actions))
	 (action-func (nth 3 action))
	 (biby (substring (symbol-name action-func) 4))
	 (biby-two (replace-regexp-in-string "bibtex" "bibtex-completion" biby))
	 (keys (or (ebib-db-list-marked-entries ebib--cur-db)
		   (ebib--get-key-at-point))))
    (funcall (intern biby-two) keys)))

  ;; (setq ebib-index-columns
  ;;  '(("Entry Key" 8 t)
  ;;    ("Note" 1 nil)
  ;;    ("File" 2 nil)
  ;;    ("Author/Editor" 10 nil)
  ;;    ("Origdate" 4 nil)
  ;;    ("Year" 6 t)
  ;;    ("Title" 40 t)
  ;;    ("Volume" 2 nil)
  ;;    ("Number" 2 nil)
  ;;    ("Edition" 2 nil)
  ;;    ("Priority" 4 nil)
  ;;    ("Status" 4 nil)
  ;;    ("Theme" 70 nil)))

(defun sync0-ebib-recalc-tags ()
  "Recalculate tags for marked entries or entry at point in Ebib."
  (interactive)
  (let* ((marked-keys (ebib-db-list-marked-entries ebib--cur-db))
         (entry-key (if marked-keys marked-keys (list (ebib--get-key-at-point)))))
    (when entry-key
      (ebib-save-current-database)
      (ebib-reload-current-database)
      (bibtex-completion-recalc-tags entry-key))))

(defun sync0-ebib-save-and-reload-current-database ()
  "Save and reload current database."
  (interactive)
  (progn
    (ebib-save-current-database t)
    (ebib-reload-current-database)))

(setq ebib-field-sort-functions-alist
      '(("Chapter" . ebib-compare-numerical-strings)
	("Sortyear" . ebib-compare-numerical-strings)
	("Origdate" . ebib-compare-numerical-strings)
	("Priority" . ebib-compare-numerical-strings)
	("Volume" . ebib-compare-numerical-strings)
	("Number" . ebib-compare-numerical-strings)
	("Edition" . ebib-compare-numerical-strings)
	("Volumes" . ebib-compare-numerical-strings)))

  (setq ebib-index-columns
   '(("Entry Key" 8 t)
     ("Note" 1 nil)
     ("File" 2 nil)
     ("Type" 3 nil)
     ("Author/Editor" 10 nil)
     ("Origdate" 6 t)
     ("Year" 6 t)
     ("Title" 40 t)
     ("Volume" 5 t)
     ("Number" 5 t)
     ("Edition" 3 t)
     ("Priority" 2 t)
     ("Status" 10 nil)
     ("Theme" 70 nil)))

(keymap-unset ebib-index-mode-map (kbd "f") t)
(keymap-unset ebib-index-mode-map (kbd "N") t)
(keymap-unset ebib-index-mode-map (kbd "P") t)
(keymap-unset ebib-entry-mode-map (kbd "k") t)
(keymap-unset ebib-index-mode-map (kbd "k") t)
(keymap-unset ebib-index-mode-map (kbd "r") t)
(keymap-unset ebib-index-mode-map (kbd "j") t)
(keymap-unset ebib-index-mode-map (kbd "J") t)
(keymap-unset ebib-index-mode-map (kbd "o") t)
(keymap-unset ebib-index-mode-map (kbd "O") t)
(keymap-unset ebib-index-mode-map (kbd "n") t)
(keymap-unset ebib-index-mode-map (kbd "s") t)
(keymap-unset ebib-index-mode-map (kbd "S") t)
(define-key ebib-index-mode-map (kbd "f") 'sync0-ebib-choose-attachment)
(define-key ebib-index-mode-map (kbd "n") 'sync0-ebib-open-notes-at-point)
(define-key ebib-index-mode-map (kbd "v") 'sync0-ebib-visit-entry-in-bib-file)
(define-key ebib-index-mode-map (kbd "t") 'ebib-add-keywords-to-entry)
;; (define-key ebib-index-mode-map (kbd "t") 'sync0-ebib-add-keywords-to-entry)
(define-key ebib-index-mode-map (kbd "+") 'sync0-ebib-increase-priority)
(define-key ebib-index-mode-map (kbd "-") 'sync0-ebib-decrease-priority)
(define-key ebib-index-mode-map (kbd "P") 'sync0-ebib-set-priority)
(define-key ebib-index-mode-map (kbd "r") 'sync0-ebib-recalc-tags)
(define-key ebib-index-mode-map (kbd "R") 'sync0-ebib-save-and-reload-current-database)
(define-key ebib-index-mode-map (kbd "o") 'sync0-ebib-choose-attachment)
(define-key ebib-index-mode-map (kbd "O") 'ebib-open-bibtex-file)
(define-key ebib-index-mode-map (kbd "j") 'ebib-next-entry)
(define-key ebib-index-mode-map (kbd "k") 'ebib-prev-entry)
(define-key ebib-index-mode-map (kbd "J") 'ebib-jump-to-entry)
(define-key ebib-index-mode-map (kbd "s") 'sync0-ebib-set-status)
(define-key ebib-index-mode-map (kbd "S") 'ebib-save-current-database)

(major-mode-hydra-define ebib-index-mode nil 
  ("Entries"
   (("o" sync0-ebib-choose-attachment "Attachment at point")
    ("n" sync0-ebib-open-notes-at-point "Notes at point")
    ("v" sync0-ebib-visit-entry-in-bib-file "View raw entry")
    ("e" ebib-edit-entry "Edit entry at point")
    ("+" sync0-ebib-increase-priority "Increase priority")
    ("-" sync0-ebib-decrease-priority "Decrease priority")
    ("=" sync0-ebib-set-priority "Set priority")
    ("s" sync0-ebib-set-status "Set status")
    ("i" sync0-ebib-ivy-bibtex-action "Ivy action on entries")
    ("m" sync0-ebib-move-entry-to-bibliography "Move to other bibliography")
    ("p" sync0-ebib-add-projects-to-entry "Set/Add projects")
    ("t" ebib-add-keywords-to-entry "Add themes to entry"))
   "Bibliography"
   (("r" sync0-ebib-recalc-tags "Recalc tags and bibnote")
    ("O" ebib-open-bibtex-file  "Open bibfile in current database")
    ("S" ebib-save-current-database  "Save current database")
    ("R" sync0-ebib-save-and-reload-current-database "Reload current bibliography"))))

(provide 'sync0-ebib)
