(require 'sync0-bibtex-key-functions)
;; (require 'sync0-bibtex-entry-functions)

(defun sync0-bibtex-entry-choose-bibliography-file ()
  "Outputs the full path of bibliography file .bib to create new biblatex entry." 
  (if (and (sync0-bibtex-buffer-p)
           (yes-or-no-p "Use current bibliography file?"))
      (buffer-file-name)
    (completing-read "Which bibliography file to append to ? "
                     sync0-bibtex-bibliographies nil t)))

(defun sync0-bibtex-set-biblatex-entry-fields-from-list (fields)
  "Set biblatex fields to certain values. The required input has to
be a list of biblatex fields. Calculation of values and setting
of entry variables is done within this function. This function
call other functions but its output is not important."
  (dolist (element fields)
    (let* ((var (concat "sync0-bibtex-entry-" element))
           (value (progn 
                    ;; first calculate the values, this function
                    ;; does not only calculate the value itself
                    ;; but also some other accompanying dummy
                    ;; variables that are necessary for
                    ;; calculating titles and else. Be careful,
                    ;; because the stdout of the function does
                    ;; not neceesarily match the definition of
                    ;; the value for "element"
                    (funcall (cadr (assoc element sync0-bibtex-entry-functions)))
                    ;; Retrieve the value for the corresponding
                    ;; variable
                    (symbol-value (intern var)))))
      ;; set the variable to the value
      (set (intern var) value)
      ;; Not sure whether this next functions works.
      (sync0-bibtex-update-var element))))

(defun sync0-bibtex-create-field-at-entry (field value &optional replace)
  "Create bibtex field with value at entry at point. When optional
old-value, search for and replace the string with the old value."
  (unless (null value)
    (let ((field-list (list field "Whatever string" value nil))
          (end (save-excursion (bibtex-end-of-entry)))
          (regex (concat "^[[:blank:]]*" field "[[:blank:]]+=")))
      (save-excursion
        (bibtex-beginning-of-entry)
        (when (and replace
                   (re-search-forward regex end t 1))
          (bibtex-kill-field nil t))
        (bibtex-make-field field-list t)))))

(defun sync0-bibtex-delete-field-at-entry (field)
  "Create bibtex field with value at entry at point. When optional
old-value, search for and replace the string with the old value."
  (let ((end (save-excursion (bibtex-end-of-entry)))
        (regex (concat "^[[:blank:]]*" field "[[:blank:]]+=")))
    (save-excursion
      (bibtex-beginning-of-entry)
        (re-search-forward regex end nil 1)
        (bibtex-kill-field nil t))))

(defun sync0-bibtex-buffer-p ()
  "Check whether current buffer is visiting a bibtex file."
  ;; Check whether the current buffer is visiting a file.
  (when (buffer-file-name)
    ;; Check whether file corresponding to current buffer has the bib
    ;; extension.
    (sync0-file-has-extension-p (buffer-file-name) "bib")))

(defun sync0-bibtex-entry-append-to-bibliography (bibkey &optional bibfile)
  "Append new BibLaTeX entry to default bibliography file.
   Beware, this function only constructs and appends the entry
   to the bib file; the values of the entry must have been defined
   elsewhere. For the sake of speed, this function does not
   perform any sanity checks on duplicate entries, and thus a
   unique correct entry is assumed to be supplied as mandatory
   argument bibkey."
  (let* ((definitions (mapcar #'eval sync0-bibtex-entry-definitions-list))
         (bibtex-fields sync0-bibtex-fields)
         (bibliography-file (or bibfile
                                (sync0-bibtex-entry-choose-bibliography-file)))
         (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields definitions))
         ;; define the bibtex entries
         (entries
          (let (x)
            (dolist (element fields x) 
              (unless (sync0-null-p (cadr element))
                (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
         (bibtex-entry (concat "@" sync0-bibtex-entry-type "{" bibkey ",\n" entries "}\n")))
    (progn
      (append-to-file bibtex-entry nil bibliography-file)
      (sync0-bibtex-entry-inform-new-entry))))

(defun sync0-bibtex-choose-attachment (&optional bibkey extension)
  "REWRITE! Function used to select an attachment to be read by
some other programs. This function is inteded to be used in
pipes and not standalone. This function requires package
bibtex-completion to be loaded; otherwise, fails."
  (interactive)
  (let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (predicate (when extension
                      (if (string-match-p "^\\." extension)
                          (lambda (x) (string-match extension x))
                        (lambda (x) (string-match (concat ".+\\." extension) x)))))
         (attach-list  (bibtex-completion-find-pdf refkey)))
    (cond ((> (length attach-list) 1)
           (completing-read "Choose an attachment to open: " attach-list predicate))
          ((equal (length attach-list) 1)
           (car attach-list))
          (t (error "File field for entry %s is empty." refkey)))))

  (defun sync0-bibtex-entry-inform-new-entry ()
    "Inform the user about a new entry that has been just created."
    (if (sync0-null-p sync0-bibtex-entry-author)
        (message "Entry %s %s has been defined with key %s" sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)
      (message "Entry %s %s %s has been defined with key %s" sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)))

(defun sync0-bibtex-get-field-value-at-point (field entry) 
  "Entry is the super list created by bibtex-mode function
bibtex-parse-entry, which requires that the pointer be placed a
the beginning of entry with bibtex-beginning-of-entry in order to
parse correctly."
  (when-let ((x (assoc field entry)))
    (sync0-bibtex-correct-smartquotes
     (substring (cdr x) 1 -1))))

(defun sync0-bibtex-define-attachment-copy-filename (bibkey path)
  "Calculate filename for attachments to be copied."
  (let* ((separator "_")
         (path-length (length path))
         (bibkey-length (length bibkey))
         (author (when sync0-bibtex-entry-author-or-editor-p
                 (sync0-bibtex-filesystem-cleanup
                   (concat  sync0-bibtex-entry-lastname separator))))
         (author-length (length author))
         (date (when sync0-bibtex-entry-date-or-origdate-p
                 (sync0-bibtex-filesystem-cleanup
                  (concat (eval (intern (concat "sync0-bibtex-entry-" sync0-bibtex-entry-date-or-origdate-p))) separator))))
         (date-length (length date))
         (but-title-length (+ author-length date-length path-length 2))
         (title-limit (- 255 but-title-length))
         (title-raw (sync0-bibtex-filesystem-cleanup
                     sync0-bibtex-entry-title-compatible))
         (title-length (length title-raw))
         (title (if (> title-length title-limit)
                    (substring title-raw 0 title-limit)
                title-raw)))
    (concat bibkey
            separator
            author
            date
            title)))

(provide 'sync0-bibtex-utils)
