(require 'sync0-bibtex-corrections)

(defun sync0-bibtex-nullify-all-variables ()
  "Set all bibtex variables required to do operations to nil to
prevent undesired results."
  (sync0-nullify-variable-list (append sync0-bibtex-entry-helper-fields-list
                                       sync0-bibtex-entry-definitions-list
                                       sync0-bibtex-entry-crossref-definitions-list
                                       sync0-bibtex-entry-extraction-fields-list
                                       sync0-bibtex-entry-initial-fields-list)))

(defvar sync0-bibtex-entry-functions
  '(("origtitle" (lambda ()
                   (setq sync0-bibtex-entry-origtitle
                         (completing-read "Origtitle of entry: " sync0-bibtex-completion-title))))
    ("date" (lambda ()
              (setq sync0-bibtex-entry-date
                    (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-date))))
    ("year" (lambda ()
              (when sync0-bibtex-entry-date
                (setq sync0-bibtex-entry-year (substring-no-properties sync0-bibtex-entry-date 0 4)))))
    ("century" (lambda ()
                 (when sync0-bibtex-entry-date
                   (setq sync0-bibtex-entry-century
                         (let* ((base-string (substring-no-properties sync0-bibtex-entry-date 0 2))
                                (cenum  (1+ (string-to-number base-string))))
                           (format "%s" cenum))))))
    ("created" (lambda ()
                 (setq sync0-bibtex-entry-created-tag
                       (format-time-string "%Y/%m/%d"))
                 (setq sync0-bibtex-entry-created
                       (format-time-string "%Y-%m-%d"))))
    ("origdate" (lambda ()
                  (setq sync0-bibtex-entry-origdate
                        (read-string "Origdate (ex. 1890-18-12) : " sync0-bibtex-entry-initial-origdate))))
    ("urldate" (lambda ()
                 (setq sync0-bibtex-entry-urldate
                       (if sync0-bibtex-entry-creation
                           (when (bound-and-true-p sync0-bibtex-entry-url)
                             (format-time-string "%Y-%m-%d"))
                         (format-time-string "%Y-%m-%d")))))
    ("language" (lambda ()
                  (setq sync0-bibtex-entry-language
                        (completing-read "Choose language : "
                                         sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))
                  (setq sync0-bibtex-entry-langid sync0-bibtex-entry-language)))
    ("langid" (lambda ()
                (if (sync0-null-p sync0-bibtex-entry-language)
                    (setq sync0-bibtex-entry-langid
                          (completing-read "Choose language : "
                                           sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))
                  (setq sync0-bibtex-entry-langid sync0-bibtex-entry-language))))
    ("origlanguage" (lambda ()
                      (setq sync0-bibtex-entry-origlanguage
                            (completing-read "Choose original language : "
                                             sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))))
    ("file" (lambda ()
              (if sync0-bibtex-entry-creation
                  (setq sync0-bibtex-entry-file
                        (concat ":" sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key ".pdf:PDF"))
                (if sync0-bibtex-entry-file-old
                    (setq sync0-bibtex-entry-file
                          (let* ((attachments (bibtex-completion-find-pdf sync0-bibtex-entry-key))
                                 (new-extension (completing-read "Choose extension to add" bibtex-completion-pdf-extension))
                                 (new-extension-caps (upcase (substring new-extension 1)))
                                 (new-attach (concat ":" sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key new-extension ":" new-extension-caps)))
                            (cond ((> (length attachments) 1)
                                   (let (x)
                                     (dolist (element attachments x)
                                       (let* ((extension (file-name-extension element t))
                                              (extension-sans (upcase (substring extension 1 nil))))
                                         (setq x (concat x ":" element extension ":" extension-sans ";"))))
                                     (concat x new-attach)))
                                  ((equal (length attachments) 1)
                                   (let* ((single-attach (car attachments))
                                          (old-extension-caps (upcase (file-name-extension single-attach))))
                                     (concat ":" single-attach ":" old-extension-caps ";" new-attach)))
                                  (t new-attach))))
                  (setq sync0-bibtex-entry-file
                        (let* ((extension (completing-read "Choose extension to add" bibtex-completion-pdf-extension))
                               (extension-sans (upcase (substring extension 1))))
                          (concat ":" sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key extension ":" extension-sans)))))))
    ("booktitle" (lambda ()
                   (setq sync0-bibtex-entry-booktitle
                         (if sync0-bibtex-entry-crossref 
                             (bibtex-completion-get-value "title" sync0-bibtex-entry-crossref-entry)
                           (read-string "Book title : ")))))
    ("booksubtitle" (lambda ()
                      (setq sync0-bibtex-entry-booksubtitle
                            (if sync0-bibtex-entry-crossref 
                                (bibtex-completion-get-value "subtitle" sync0-bibtex-entry-crossref-entry)
                              (read-string "Book subtitle : ")))))
    ("crossref" (lambda ()
                  (when (yes-or-no-p "Load crossref? ")
                    (setq sync0-bibtex-entry-crossref 
                          (sync0-bibtex-completion-choose-key t t))
                    (setq sync0-bibtex-entry-crossref-entry
                          (bibtex-completion-get-entry sync0-bibtex-entry-crossref))
                    (setq sync0-bibtex-entry-initial-date
                          (bibtex-completion-get-value "date" sync0-bibtex-entry-crossref-entry)
                          sync0-bibtex-entry-initial-origdate
                          (bibtex-completion-get-value "origdate" sync0-bibtex-entry-crossref-entry)
                          sync0-bibtex-entry-initial-author
                          (if (or (string= sync0-bibtex-entry-type-downcase "collection")
                                  (string= sync0-bibtex-entry-type-downcase "incollection")
                                  (string= sync0-bibtex-entry-type-downcase "proceedings")
                                  (string= sync0-bibtex-entry-type-downcase "inproceedings"))
                              (bibtex-completion-get-value "editor" sync0-bibtex-entry-crossref-entry)
                            (bibtex-completion-get-value "author" sync0-bibtex-entry-crossref-entry))
                          sync0-bibtex-entry-initial-language
                          (bibtex-completion-get-value "language" sync0-bibtex-entry-crossref-entry)))))
    ("related" (lambda ()
                 (setq sync0-bibtex-entry-related (sync0-bibtex-completion-choose-key nil t))))
    ("keywords" (lambda ()
                  ;; Requires package unidecode for conversion to
                  ;; ASCII. See:
                  ;; https://github.com/sindikat/unidecode
                  (setq sync0-bibtex-entry-keywords
                        (unidecode (let (x)
                                     (dolist (element sync0-bibtex-tag-fields-list x) 
                                       (unless (sync0-null-p (symbol-value (caddr element)))
                                         (setq x (cons (concat (cadr element)  (sync0-bibtex-obsidian-keyword-tagify (eval (caddr element)))) x))))
                                     (sync0-show-elements-of-list x ", ")))))))
  "List of lists in which the car of each list is bibtex-field and
the cdr is a lambda function with all the actions that should be
carried to calculate the value it will take in a BibLaTeX entry.")

;; Set functions for biblatex fields that require completion of multiple
;; things.
(let ((x (remove "keywords" sync0-bibtex-string-multiple-fields)))
  ;; Remove keywords to prevent overwriting the function it currently has
  (dolist (element x)
    (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
           (comp-var (intern (concat "sync0-bibtex-completion-" element)))
           (help-string (concat (upcase-initials element) ": "))
           (my-base-func  (list  'completing-read-multiple help-string comp-var)) 
           (my-list-string (list 'sync0-show-elements-of-list my-base-func ", "))
           (my-macro  (list 'setq my-var  my-list-string)) 
           (my-func (list 'lambda () my-macro))
           (my-cons (cons element (list my-func))))
      (push my-cons sync0-bibtex-entry-functions))))

;; Set functions for biblatex fields that are defined with read-string
(dolist (element sync0-bibtex-string-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (help-string (concat (upcase-initials element) ": "))
         (my-base-func  (list  'read-string help-string nil nil nil t)) 
         (my-macro  (list 'setq my-var  my-base-func)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))

;; Set functions for biblatex fields that require completion of people
(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (help-string (concat (upcase-initials element) "(s): "))
         (my-macro  (list  'sync0-bibtex-normalize-name-string my-var help-string)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))

;; Set functions for biblatex fields that require completion of single
;; things.
(let* ((exceptions (list "language"))
       (x (cl-set-difference sync0-bibtex-completion-single-fields exceptions :test #'string=)))
  ;; Remove languages to prevent overwriting the function it currently has
  (dolist (element x)
    (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
           (comp-var (intern (concat "sync0-bibtex-completion-" element)))
           (help-string (concat (upcase-initials element) ": "))
           (my-base-func  (list  'completing-read help-string comp-var)) 
           (my-macro  (list 'setq my-var  my-base-func)) 
           (my-func (list 'lambda () my-macro))
           (my-cons (cons element (list my-func))))
      (push my-cons sync0-bibtex-entry-functions))))

(defun sync0-bibtex-completion-load-entry (&optional bibkey quick)
  "Load the contents of the biblatex fields corresponding to a
  biblatex key into their respective dummy variables. When
  creating a new biblatex entry, this function has the user input
  the contents of the fields defined in the variable
  sync0-bibtex-type-fields according to the content definition
  functions defined in sync0-bibtex-entry-functions for each
  field. When optional bibkey is defined, load the contents of
  the fields of such biblatex key. When optional quick is true,
  only prompt the user to define the fields defined in the
  variable sync0-bibtex-base fields; that is to say, ignore the
  definition of biblatex fields specific to a biblatex entry
  type."
  ;; Clear all dummy variables used for calculating the contents of
  ;; the biblatex entry.
  (interactive)
  (sync0-bibtex-nullify-all-variables)
  ;; When optional bibkey is specified, load that entry; otherwise,
  ;; define a new one
  (let* ((entry (when bibkey (bibtex-completion-get-entry bibkey)))
         (type (if entry
                   (sync0-bibtex-normalize-case
                    (bibtex-completion-get-value "=type="  entry))
                 (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types)))
         ;; Specify which fields to load
         (fields (cond (bibkey
                        sync0-bibtex-fields)
                       (quick
                        (cons "author" sync0-bibtex-base-fields))
                       (t (append
                           (cdr (assoc type sync0-bibtex-type-fields))
                           sync0-bibtex-base-fields)))))
    (setq sync0-bibtex-entry-key (or bibkey 
                                     (sync0-bibtex-entry-key-define)))
    (setq sync0-bibtex-entry-type type)
    ;; the following field is used to avoid inconsistensies in
    ;; case when using the type for boolean operations
    (setq sync0-bibtex-entry-type-downcase (downcase type))
    ;; Call the appropriate functions to define the biblatex fields.
    ;; Collect the result in an alist of the form:
    ;; ("sync0-bibtex-entry-title" . "The good old days"). Finally,
    ;; collect such conses into the list
    ;; sync0-bibtex-entry-fields-alist, which will be used by other
    ;; functions.
    (let (x)
      (dolist (element fields x)
        (let* ((var (concat "sync0-bibtex-entry-" element))
               (value (if entry
                          (bibtex-completion-get-value element entry)
                       (progn 
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
                         (symbol-value (intern var))))))
          ;; set the variable to the value
          (set (intern var) value)
          ;; create cons of the form  ("sync0-bibtex-entry-title" . "The good old days") for an alist
          (push (cons var value) x)
          ;; Not sure whether this next functions works.
          (sync0-bibtex-update-var element)))
      (setq sync0-bibtex-entry-fields-alist x))
    ;; After loading (or defining) the fields for the biblatex entry,
    ;; it is necessary to define these helper functions. Otherwise,
    ;; errors occur when calling functions to extract pdfs, creating
    ;; notes, etc.
    ;; 
    ;; Correct fields to prevent conflicts with the crossref fields
    ;; present in an entry.
    (when sync0-bibtex-entry-crossref
      (sync0-bibtex-correct-crossref-fields))
      ;;; The following three functions are used to calculate the titles
      ;;; (and their accompanying dummy variables) of the Obsidian
      ;;; markdown notes and others.
       ;;; Configure the author dummy
       ;;; variables avoiding conflicts with the editor field. This
       ;;; part is also necessary to calculate the name of the person
       ;;; used in calculating entries.
    ;; Set author or editor fields
    (sync0-bibtex-set-author-or-editor-extra-fields)
    ;; Set date fields
    (sync0-bibtex-set-date-extra-fields)
    ;; Set date fields
    (sync0-bibtex-set-title-extra-fields)
    ;; Fix problems with calculation of related tag.
    (when sync0-bibtex-entry-related
      (setq sync0-bibtex-entry-related-tag
            (cond ((sync0-null-p sync0-bibtex-entry-related)
                   "")
                  ((string-match ", " sync0-bibtex-entry-related)
                   (sync0-add-prefix-to-list-convert-to-string sync0-bibtex-entry-related ", " "related/"))
                  (t sync0-bibtex-entry-related))))
    ;; Fix problems with calculation of doctype tag.
    ;;;; (when sync0-bibtex-entry-doctype
    ;; (unless (sync0-null-p sync0-bibtex-entry-doctype)
    ;; (sync0-bibtex-tagify-var sync0-bibtex-entry-doctype))
    ;;
    ;; Fix problems with calculation of tags for biblatex fields that
    ;; allow multiple completion.
    (let* ((to-remove (list "keywords"))
           (my-list (cl-set-difference sync0-bibtex-string-multiple-fields to-remove :test #'string=)))
      (dolist (element my-list)
        (let ((my-var (intern (concat "sync0-bibtex-entry-" element))))
          (eval `(sync0-bibtex-tagify-var ,my-var)))))
    ;; Calculate the people fields; the reason for excluding author,
    ;; editor and translator fields is that these are necessary for
    ;; the cumbersome calculation of titles. 
    (let* ((to-remove (list "author" "editor" "translator"))
           (my-list (cl-set-difference sync0-bibtex-people-fields to-remove :test #'string=)))
      (dolist (element my-list)
        (let ((my-var (intern (concat "sync0-bibtex-entry-" element))))
          (eval `(sync0-bibtex-fix-names ,my-var)))))
    ;; Calculate the translator field. This is calculated separately
    ;; to allow for some flexibility in including the translator
    ;; field, when present, in Obsidian markdown notes.
    (unless (sync0-null-p sync0-bibtex-entry-translator)
      (sync0-bibtex-fix-names sync0-bibtex-entry-translator)
      (setq sync0-bibtex-entry-translator-lastname
            (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-translator-lastname))
      (setq sync0-bibtex-entry-lastname
            (concat sync0-bibtex-entry-lastname " (" sync0-bibtex-entry-translator-lastname ")")))
      ;; Fix problems with calculation of medium obsidian YAML property.
      (unless (null sync0-bibtex-entry-medium)
        (setq sync0-bibtex-entry-medium-fixed
              (if (string-match ", " sync0-bibtex-entry-medium)
                  (sync0-add-prefix-to-list-convert-to-string sync0-bibtex-entry-medium ", " "\"" "\"")
                (concat "\"" sync0-bibtex-entry-medium "\""))))
      (unless (null sync0-bibtex-entry-project)
        (setq sync0-bibtex-entry-project-fixed
              (if (string-match ", " sync0-bibtex-entry-project)
                  (sync0-add-prefix-to-list-convert-to-string sync0-bibtex-entry-project ", " "\"" "\"")
                (concat "\"" sync0-bibtex-entry-project "\""))))
      ;; Set parent extra field
      (setq sync0-bibtex-entry-parent
            (cond ((string= sync0-bibtex-entry-type-downcase "article")
                   sync0-bibtex-entry-journaltitle)
                  ((or (string= sync0-bibtex-entry-type-downcase "inbook")
                       (string= sync0-bibtex-entry-type-downcase "incollection")
                       (string= sync0-bibtex-entry-type-downcase "inproceedings"))
                   (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
                       sync0-bibtex-entry-booktitle
                     (concat sync0-bibtex-entry-booktitle sync0-bibtex-entry-separator sync0-bibtex-entry-booksubtitle)))
                  (t sync0-bibtex-entry-series)))
      ;; keywors have to be calculated last in order to prevent empty
      ;; fields when defining the keywords
      (if bibkey
          (bibtex-completion-get-value "keywords" entry)
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions))))))

(provide 'sync0-bibtex-entry-functions)