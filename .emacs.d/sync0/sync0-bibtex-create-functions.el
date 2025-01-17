(require 'sync0-bibtex-entry-functions)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-corrections)
(require 'sync0-yaml)

(defun sync0-bibtex-define-entry (&optional quick)
  "Create new BibLaTeX entry in the default bibliography. When
   optional quick is non-nil, only capture the minimal fields
   required to create a new entry."
  (interactive)
  ;; Before calculating any values, reset all values in my
  ;; master list of variable (set them to nil).
  ;; (setq sync0-bibtex-entry-derivation (yes-or-no-p "Derive entry?"))
  (setq sync0-bibtex-entry-creation t)
  (setq sync0-bibtex-entry-file-old nil)
  (sync0-bibtex-completion-load-entry nil quick)
  ;; Extract sub-pdf from crossref
  (sync0-bibtex-derive-pdf-from-crossref)
  ;; Set some default values
  (setq sync0-bibtex-entry-status "fetch")
  ;; Insert entry in default bibliography file
  (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
  ;; the function below does not work; throws an error listp that I
  ;; have not been able to solve (sync0-bibtex-update-vars
  ;; sync0-bibtex-completion-variables-list)
  (setq sync0-bibtex-entry-creation nil)
  (when (and sync0-bibtex-entry-url
             (string-match "\\.pdf$" sync0-bibtex-entry-url)
             (yes-or-no-p "Download the attached pdf? "))
    (sync0-bibtex-download-pdf sync0-bibtex-entry-key t)))

;; change this function to be actually usable : do not delete this, just create a new one from using this as template
(defun sync0-bibtex-define-multiple-entries (num)
  "Create new BibLaTeX entries..."
  (interactive "nHow many new entries to create? ")
  (sync0-bibtex-nullify-all-variables)
  (let*    ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
            ;; Specify which fields to load
            (fields (append (cdr (assoc type sync0-bibtex-type-fields))
                            (remove "file" sync0-bibtex-base-fields)))
;;             (bibfile (sync0-bibtex-entry-choose-bibliography-file))
            (bibfile sync0-bibtex-inbox-bibliography)
            (keylist (sync0-bibtex-entry-define-keys-list num)))
    ;; Before calculating any values, reset all values in my
    ;; master list of variable (set them to nil).
    (setq sync0-bibtex-entry-creation t)
    (setq sync0-bibtex-entry-file-old nil)
    ;; (setq sync0-bibtex-entry-keywords
    ;;       (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions))))
    (setq sync0-bibtex-entry-type type)
    (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
    ;; Begin loop; specific settings for each note are to be defined
    ;; inside the loop.
    (dotimes (i num)
      (let ((filename (elt keylist i))
            x)
        (setq sync0-bibtex-entry-key filename)
        (dolist (element fields x)
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
            ;; create cons of the form  ("sync0-bibtex-entry-title" . "The good old days") for an alist
            (push (cons var value) x)
            ;; Not sure whether this next functions works.
            (sync0-bibtex-update-var element)))
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
        (sync0-bibtex-entry-constitute-bibentry filename)
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography filename bibfile)
	))))

(defun sync0-bibtex-derive-entries-from-collection (&optional glist)
  "Create new BibLaTeX entries from objects of the form (title
subtitle pages expages author), where title is the title of new entries, pages are
the page ranges of the biblatex entries and expages is the
range of pages of the actual pdf file to be used for extraction." 
  (interactive)
  ;; Before calculating any values, reset all values in my
  ;; master list of variable (set them to nil).
  (sync0-bibtex-nullify-all-variables)
  (let* ((unique-author-p (or (null glist)
                              (yes-or-no-p "Unique author for newly created entries?")))
         (unique-author (when unique-author-p
                   (funcall (cadr (assoc "author" sync0-bibtex-entry-functions)))))
         (type "InCollection")
         ;; (type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
         ;; Specify which fields to load
         (themes (sync0-bibtex-entry-calculate-bibfield "theme")) 
         ;; (fields (append (cdr (assoc type sync0-bibtex-type-fields))
         ;;                 (remove "file" sync0-bibtex-base-fields)))
         (unique (unless glist
                   (list 
                    (read-string "Title? ")
                    (read-string "Subtitle? ")
                    (read-string "Page range? ")
                    (when (y-or-n-p "Add expages? ")
                      (read-string "Expage range? ")))))
;;          (crossref (sync0-bibtex-completion-choose-key t t "Which crossref to derive entries from? "))
         (crossref (sync0-bibtex-choose-key "Which crossref to derive entries from? "))
;;          (crossref-entry (bibtex-completion-get-entry crossref))
         (crossref-entry (citar-get-entry crossref))
         (created (sync0-bibtex-entry-calculate-bibfield "created"))
         (date (sync0-bibtex-get-value "date" crossref-entry))
         (language (sync0-bibtex-get-value "language" crossref-entry))
         (crossref-title (sync0-bibtex-get-value "title" crossref-entry))
         (crossref-subtitle (sync0-bibtex-get-value "subtitle" crossref-entry))
;;          (bibfile (sync0-bibtex-entry-choose-bibliography-file))
         (bibfile sync0-bibtex-inbox-bibliography)
         (list-length (if glist
                          (length glist)
                        0))
         (num (max 1 list-length))
         (keylist (sync0-bibtex-entry-define-keys-list num))
         (extract-p (yes-or-no-p "Extract  PDF from crossref?"))
         (crossref-file (when extract-p
                          (sync0-bibtex-choose-attachment crossref "pdf"))))
    ;; Set some default values for all new entries
    (setq sync0-bibtex-entry-type type)
    (setq sync0-bibtex-entry-type-downcase (downcase type))
    (setq sync0-bibtex-entry-crossref crossref)
    (setq sync0-bibtex-entry-status "inspect")
    ;; (setq sync0-bibtex-entry-source "primary")
    ;; (setq sync0-bibtex-entry-theme themes)
    (setq sync0-bibtex-entry-date date)
    (setq sync0-bibtex-entry-booktitle crossref-title)
    (setq sync0-bibtex-entry-booksubtitle crossref-subtitle)
    (setq sync0-bibtex-entry-creation t)
    (setq sync0-bibtex-entry-file-old nil)
    ;; Begin loop; specific settings for each note are to be defined
    ;; inside the loop.
    (dotimes (i num)
      (let* ((sublist (or unique
                          (elt glist i)))
             (filename (elt keylist i))
             (title (car sublist))
             (subtitle (elt sublist 1))
             (pages (elt sublist 2))
             (expages (elt sublist 3))
             (author (or unique-author
                         (elt sublist 4))))
        (setq sync0-bibtex-entry-key filename)
        (setq sync0-bibtex-entry-title title)
        (setq sync0-bibtex-entry-subtitle subtitle)
        (setq sync0-bibtex-entry-pages pages)
        (setq sync0-bibtex-entry-expages expages)
        (unless unique-author
          (setq sync0-bibtex-entry-author author))
        (setq sync0-bibtex-entry-file (concat ":" sync0-zkn-attachments-dir sync0-bibtex-entry-key ".pdf:PDF"))
        ;; (dolist (element fields x)
        ;;   (let* ((var (concat "sync0-bibtex-entry-" element))
        ;;          (value (progn 
        ;;                   ;; first calculate the values, this function
        ;;                   ;; does not only calculate the value itself
        ;;                   ;; but also some other accompanying dummy
        ;;                   ;; variables that are necessary for
        ;;                   ;; calculating titles and else. Be careful,
        ;;                   ;; because the stdout of the function does
        ;;                   ;; not neceesarily match the definition of
        ;;                   ;; the value for "element"
        ;;                   (funcall (cadr (assoc element sync0-bibtex-entry-functions)))
        ;;                   ;; Retrieve the value for the corresponding
        ;;                   ;; variable
        ;;                   (symbol-value (intern var)))))
        ;;     ;; set the variable to the value
        ;;     (set (intern var) value)
        ;;     ;; Not sure whether this next functions works.
        ;;     (sync0-bibtex-update-var element)))
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
        (sync0-bibtex-entry-constitute-bibentry filename)
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography filename bibfile)
        ;; Fourth, extract the pdf from the crossref
        (when extract-p
        (sync0-bibtex-extract-pdf-from-crossref crossref-file t))))))

(defun sync0-bibtex-define-similar-type-entries (num)
  "Create new BibLaTeX entries with similar type and constant
values for certain user-defined entries"
  (interactive "nHow many new entries to create? ")
  ;; Clear all biblatex entry dummy variables.
  (sync0-bibtex-nullify-all-variables)
  ;; General settings for constant entry creation
  (let*    ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
            ;; Specify which fields to load
            (possible-fields (append (cdr (assoc type sync0-bibtex-type-fields))
                                     (remove "file" sync0-bibtex-base-fields)))
            (fields-completion (cl-set-difference (append (cdr (assoc type sync0-bibtex-type-fields)) sync0-bibtex-fields)
                                                  sync0-bibtex-automatic-fields :test #'string=))
            (constant-fields (completing-read-multiple "Constant fields?" fields-completion nil t))
            (fields (cl-set-difference possible-fields constant-fields :test #'string=))
;;             (bibfile (sync0-bibtex-entry-choose-bibliography-file))
            (bibfile sync0-bibtex-inbox-bibliography)
            (keylist (sync0-bibtex-entry-define-keys-list num)))
    ;; Before calculating any values, reset all values in my
    ;; master list of variable (set them to nil).
    ;; Set some basic common variable values for entry creation
    (setq sync0-bibtex-entry-creation t)
    (setq sync0-bibtex-entry-file-old nil)
    (setq sync0-bibtex-entry-type type)
    (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
    ;; Set constant variables before entry creation.
    (sync0-bibtex-set-biblatex-entry-fields-from-list constant-fields)
    ;; Begin loop; specific settings for each note are to be defined
    ;; inside the loop.
    (dotimes (i num)
      (let ((filename (elt keylist i)))
        (setq sync0-bibtex-entry-key filename)
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
            (sync0-bibtex-update-var element)))
        ;; Corrections
        (sync0-bibtex-correct-entry-fields)
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
        (sync0-bibtex-entry-constitute-bibentry filename)
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography filename bibfile)
	))))

(defun sync0-bibtex-define-entries-from-bibkey (&optional bibkey)
  "Creates copies of target entry but with different keys. This
function also creates markdown notes for the created entries."
  (interactive)

  (sync0-bibtex-completion-load-entry (or bibkey (sync0-bibtex-choose-key "Choose key for entry duplication")))
  
  (let* ((howmany (string-to-number (read-string "How many entries to create? ")))
         (has-attachment (sync0-bibtex-has-attachments-p sync0-bibtex-entry-key))
         (to-duplicate (when has-attachment
                         (yes-or-no-p "Duplicate attachment for newly created entries?")))
         (attachment (when has-attachment
                       (sync0-bibtex-choose-attachment sync0-bibtex-entry-key)))
         (extension (if attachment
                        (file-name-extension attachment)
                      (when (yes-or-no-p "Add file field?")
                        (completing-read "Choose extension: " sync0-bibtex-completion-extension))))
         (extension-coda (and extension
                              (concat "." extension ":" (upcase extension))))
         (keylist (sync0-bibtex-entry-define-keys-list howmany))
;;          (bibfile (sync0-bibtex-entry-choose-bibliography-file))
         (bibfile sync0-bibtex-inbox-bibliography)
         (keywords-list (and sync0-bibtex-entry-keywords
                             (split-string sync0-bibtex-entry-keywords ", "))))
    
    ;; Update keywords displayed
    (setq sync0-bibtex-entry-keywords (sync0-show-elements-of-list keywords-list ", "))
    
    ;; Process each key in the keylist
    (dolist (key keylist)
      (setq sync0-bibtex-entry-key key)
      (setq sync0-bibtex-entry-file
            (when extension-coda
              (concat ":" sync0-zkn-attachments-dir sync0-bibtex-entry-key extension-coda)))
      
      ;; Append entry to default bibliography file
      (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key bibfile)
      
      ;; Duplicate attachment if requested
      (when (and to-duplicate attachment)
        (copy-file attachment (concat sync0-zkn-attachments-dir sync0-bibtex-entry-key "." extension))))))


(defun sync0-bibtex-match-matrix-and-fields (num completion-table)
  (cl-loop repeat num collect (completing-read "Which fields correspond to matrix columns? " completion-table nil t)))

(defun sync0-bibtex-define-entries-from-matrix (my-list)
  "Create new BibLaTeX entries from matrix which is a list of lists
of the form author, title, etc, in which data types have to be
defined or nil, but are defined programatically inside this function." 
  (interactive)
  ;; Clear all biblatex entry dummy variables.
  (sync0-bibtex-nullify-all-variables)
  ;; General settings for constant entry creation
  (let*    ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
            ;; Specify which fields to load
            (iterations (length my-list))
            (sublist-length (length (car my-list)))
            (type-fields (cdr (assoc type sync0-bibtex-type-fields)))
            (possible-fields (append type-fields (remove "file" sync0-bibtex-base-fields)))
            (fields-completion (cl-set-difference sync0-bibtex-fields
                                                  sync0-bibtex-automatic-fields :test #'string=))
            (constant-fields (completing-read-multiple "Constant fields?" fields-completion nil t))
            ;; (define-fields (completing-read-multiple "Constant fields?" fields-completion nil t))
            (define-fields (sync0-bibtex-match-matrix-and-fields sublist-length fields-completion))
            ;; (constant-fields (completing-read-multiple "Constant fields?" fields-completion))
            (excluded-fields (cl-set-difference type-fields define-fields :test #'string=))
            (remove-fields (remove-duplicates
                            (append constant-fields define-fields excluded-fields) :test #'string=))
            (fields (cl-set-difference possible-fields remove-fields :test #'string=))
;;             (bibfile (sync0-bibtex-entry-choose-bibliography-file))
            (bibfile sync0-bibtex-inbox-bibliography)
            (keylist (sync0-bibtex-entry-define-keys-list iterations)))
    ;; Before calculating any values, reset all values in my
    ;; master list of variable (set them to nil).
    ;; Set some basic common variable values for entry creation
    (setq sync0-bibtex-entry-creation t)
    (setq sync0-bibtex-entry-file-old nil)
    (setq sync0-bibtex-entry-type type)
    (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
    ;; Set constant variables before entry creation.
    (sync0-bibtex-set-biblatex-entry-fields-from-list constant-fields)
    ;; Begin big loop with the super master list of lists
    (dotimes (k iterations)
      (let ((current-sublist (elt my-list k))
            (filename (elt keylist k)))
        (setq sync0-bibtex-entry-key filename)
        ;; First sub-loop: Set the values for bibtex fields for a given sublist.
        (dotimes (i sublist-length)
          (let* ((field (elt define-fields i))
                 (var (concat "sync0-bibtex-entry-" field))
                 (value (elt current-sublist i)))
            (sync0-bibtex-update-var field)
            ;; set the variable to the value
            (set (intern var) value)))
        ;; Second sub-loop: Set other fields required for a complete bibtex entry.
        (dolist (bibfield fields)
          (let* ((var (concat "sync0-bibtex-entry-" bibfield))
                 (value (progn 
                          ;; first calculate the values, this function
                          ;; does not only calculate the value itself
                          ;; but also some other accompanying dummy
                          ;; variables that are necessary for
                          ;; calculating titles and else. Be careful,
                          ;; because the stdout of the function does
                          ;; not neceesarily match the definition of
                          ;; the value for "element"
                          (funcall (cadr (assoc bibfield sync0-bibtex-entry-functions)))
                          ;; Retrieve the value for the corresponding
                          ;; variable
                          (symbol-value (intern var)))))
            ;; set the variable to the value
            (set (intern var) value)
            ;; Not sure whether this next functions works.
            (sync0-bibtex-update-var bibfield)))
        ;; Corrections
        (sync0-bibtex-correct-entry-fields)
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
        (sync0-bibtex-entry-constitute-bibentry filename)
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography filename bibfile)
	))))

(provide 'sync0-bibtex-create-functions)
