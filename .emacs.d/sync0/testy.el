
;; (defun sync0-bibtex-completion-load-bibtex-fields (&optional bibkey)

;;       ;; (let ((entry (bibtex-completion-get-entry bibkey)))
;; ;; for each field load 
;; (when bibkey
;;   (let ((entry (bibtex-completion-get-entry bibkey)))
;;     (dolist ()
;;     ;; (set sync0-bibtex-entry-key bibkey)
;;     (when-let
;;         ;; (bibtex-completion-get-value "edition" entry))
;;         ((content (bibtex-completion-get-value field entry)))
;;       (funcall set-entry-according-to-alist)

;;     (set entry-variable content)))

;;     for those things that are part in the alist define them
;;     according to the method that is defined for them in another alist

    ;; therefoer, two alists are required, one with the fields to be
    ;; defined according to the type of entry and another one with
    ;; the, ways that each of those ; fields is to be defined to ; be
    ;; called with the funcall

;; (setq sync0-bibtex-entry-extra-fields
;;       '(("title" . (sync0-bibtex-entry-title sync0-bibtex-entry-initial-title (lambda ()
;;                                             )


          ;; '(("Article" . (sync0-bibtex-entry-title
          ;;                 sync0-bibtex-entry-subtitle
          ;;                 sync0-bibtex-entry-eventtitle
          ;;                 sync0-bibtex-entry-date
          ;;                 sync0-bibtex-entry-origdate
          ;;                 sync0-bibtex-entry-eventdate
          ;;                 sync0-bibtex-entry-author
          ;;                 sync0-bibtex-entry-journaltitle
          ;;                 sync0-bibtex-entry-edition
          ;;                 sync0-bibtex-entry-booktitle
          ;;                 sync0-bibtex-entry-booksubtitle
          ;;                 sync0-bibtex-entry-crossref
          ;;                 sync0-bibtex-entry-chapter
          ;;                 sync0-bibtex-entry-volume
          ;;                 sync0-bibtex-entry-number
          ;;                 sync0-bibtex-entry-series
          ;;                 sync0-bibtex-entry-publisher
          ;;                 sync0-bibtex-entry-location
          ;;                 sync0-bibtex-entry-pages
          ;;                 sync0-bibtex-entry-note
          ;;                 sync0-bibtex-entry-doi
          ;;                 sync0-bibtex-entry-url
          ;;                 sync0-bibtex-entry-creation-date
          ;;                 sync0-bibtex-entry-language
          ;;                 sync0-bibtex-entry-language
          ;;                 sync0-bibtex-entry-medium
          ;;                 sync0-bibtex-entry-institution
          ;;                 sync0-bibtex-entry-library
          ;;                 sync0-bibtex-entry-related
          ;;                 sync0-bibtex-entry-relatedtype
          ;;                 sync0-bibtex-entry-file
          ;;                 sync0-bibtex-entry-shorthand
          ;;                 sync0-bibtex-entry-keywords))

(setq sync0-bibtex-base-fields
      '("title"
        "subtitle"
        "date"
        "author"
        "url"
        "language"
        "file"
        "keywords"))

(setq sync0-bibtex-type-fields
      '(("Article" "journaltitle"
         "volume"
         "number"
         "series"
         "pages"
         "doi")
        ("Book" "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "media"
         "library")
        ("Collection" "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "media"
         "library")
        ("InBook" "crossref"
         "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "chapter"
         "booktitle"
         "booksubtitle"
         "media"
         "library")
        ("InCollection" "crossref"
         "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "chapter"
         "booktitle"
         "booksubtitle"
         "media"
         "library")
        ("InProceedings" "crossref"
         "eventtitle"
         "eventdate"
         "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         ;; "chapter"
         "booktitle"
         "booksubtitle"
         "media"
         "library")
("Manual" "institution"
         "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "media"
         "library"
         "note")
    ("Online" "institution"
         "series"
         "publisher"
         "media"
         "library"
         "note")
    ("Report" "institution"
         "volume"
         "number"
         "series"
         "publisher"
         "location"
         "media"
         "library"
         "note")
    ("Unpublished" "institution"
         "volume"
         "number"
         "series"
         "publisher"
         "location"
         "media"
         "library"
         "note")
    ("Misc" "institution"
         "series"
         "publisher"
         "location"
         "media"
         "library"
         "note")
    ("Thesis" "institution"
         "number"
         "publisher"
         "location"
         "media"
         "library"
         "note")
    ("Proceedings" "institution"
         "eventtitle"
         "eventdate"
         "volume"
         "number"
         "series"
         "publisher"
         "location"
         "media"
         "library"
         "note")))

    ;; (setq sync0-bibtex-type-fields
    ;;       '(("Article" "title"
    ;;          "subtitle"
    ;;          "date"
    ;;          "author"
    ;;          "journaltitle"
    ;;          "volume"
    ;;          "number"
    ;;          "series"
    ;;          "pages"
    ;;          "doi"
    ;;          "url"
    ;;          "language"
    ;;          "parent"
    ;;          "file"
    ;;          "keywords")
    ;;         ("Book" "title"
    ;;          "subtitle"
    ;;          "date"
    ;;          "origdate"
    ;;          "author"
    ;;          "edition"
    ;;          "volume"
    ;;          "series"
    ;;          "publisher"
    ;;          "location"
    ;;          "url"
    ;;          "language"
    ;;          "media"
    ;;          "library"
    ;;          "parent"
    ;;          "file"
    ;;          "keywords")
    ;; ("Collection" "title"
    ;;          "subtitle"
    ;;          "date"
    ;;          "origdate"
    ;;          "author"
    ;;          "edition"
    ;;          "volume"
    ;;          "series"
    ;;          "publisher"
    ;;          "location"
    ;;          "url"
    ;;          "language"
    ;;          "media"
    ;;          "library"
    ;;          "parent"
    ;;          "file"
    ;;          "keywords")
    ;; ("InBook" "crossref"
    ;;           "title"
    ;;          "subtitle"
    ;;          "date"
    ;;          "origdate"
    ;;          "author"
    ;;          "edition"
    ;;          "volume"
    ;;          "series"
    ;;          "publisher"
    ;;          "location"
    ;;          "url"
    ;;          "chapter"
    ;;          "booktitle"
    ;;          "booksubtitle"
    ;;          "language"
    ;;          "media"
    ;;          "library"
    ;;          "parent"
    ;;          "file"
    ;;          "keywords")
    ;; ("InCollection" "crossref"
    ;;           "title"
    ;;          "subtitle"
    ;;          "date"
    ;;          "origdate"
    ;;          "author"
    ;;          "edition"
    ;;          "volume"
    ;;          "series"
    ;;          "publisher"
    ;;          "location"
    ;;          "url"
    ;;          "chapter"
    ;;          "booktitle"
    ;;          "booksubtitle"
    ;;          "language"
    ;;          "media"
    ;;          "library"
    ;;          "parent"
    ;;          "file"
    ;;          "keywords")
    ;; ("InProceedings" "crossref"
    ;;           "title"
    ;;           "eventtitle"
    ;;          "subtitle"
    ;;          "date"
    ;;          "eventdate"
    ;;          "origdate"
    ;;          "author"
    ;;          "edition"
    ;;          "volume"
    ;;          "series"
    ;;          "publisher"
    ;;          "location"
    ;;          "url"
    ;;          ;; "chapter"
    ;;          "booktitle"
    ;;          "booksubtitle"
    ;;          "language"
    ;;          "media"
    ;;          "library"
    ;;          "parent"
    ;;          "file"
    ;;          "keywords")

    ;; ("Manual" "title" 

    ;; ("Online" "title" 

    ;; ("Report" "title" 

    ;; ("Unpublished" "title" 

    ;; ("Misc" "title" 

    ;; ("Thesis" "title" 

    ;; ("Proceedings" "title" 

    ;;         ))

    (setq sync0-bibtex-fields-functions-one
          '(("title" (lambda ()
                       (setq sync0-bibtex-entry-title
                             (completing-read "Input title of new BibLaTeX entry: " sync0-bibtex-completion-title))))
            ("subtitle" (lambda ()
                          (setq sync0-bibtex-entry-subtitle
                                (read-string "Sous-titre du texte : " nil nil nil t))))
            ("date" (lambda ()
                      (setq sync0-bibtex-entry-date
                            (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-date))))
            ("origdate" (lambda ()
                          (setq sync0-bibtex-entry-origdate
                                (read-string "Origdate (ex. 1890-18-12) : " sync0-bibtex-entry-initial-origdate))))
            ("author" (lambda ()
                        (setq sync0-bibtex-entry-author
                              (let* ((result)
                                     (crm-separator  "[ 	]*;[ 	]*")
                                     (initial-input  (completing-read-multiple "Authors: "
                                                                               sync0-bibtex-completion-author nil nil sync0-bibtex-entry-initial-author))
                                     (author-string (sync0-show-elements-of-list initial-input " and ")))
                                (setq result author-string)
                                (when (string-match sync0-bibtex-author-separator author-string)
                                  (setq result (replace-regexp-in-string sync0-bibtex-author-separator ", " result)))
                                (when (string-match sync0-bibtex-author-lastname-separator author-string)
                                  (setq result (replace-regexp-in-string sync0-bibtex-author-lastname-separator " " result)))
                                result))))
            ("edition" (lambda ()
                         (setq sync0-bibtex-entry-edition
                               (read-string "Édition : "))))
            ("volume" (lambda ()
                        (setq sync0-bibtex-entry-volume
                              (read-string "Tome : "))))
            ("number" (lambda ()
                 (setq sync0-bibtex-entry-number
                       (read-string "Numero : "))))
            ("series" (lambda ()
                        (setq sync0-bibtex-entry-series
                              (read-string "Series : "))))
            ("publisher" (lambda ()
                           (setq sync0-bibtex-entry-publisher
                                 (completing-read "Maison d'edition : " sync0-bibtex-completion-publisher))))
            ("doi" (lambda ()
                     (setq sync0-bibtex-entry-doi
                           (read-string "DOI : " nil nil nil t))))
            ("url" (lambda ()
                     (setq sync0-bibtex-entry-url
                           (when (sync0-null-p sync0-bibtex-entry-doi) 
                             (read-string "Url : " nil nil nil t)))))
            ("language" (lambda ()
                          (setq sync0-bibtex-entry-language
                                (completing-read "Choose language : "
                                                 sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))
                          (setq sync0-bibtex-entry-langid sync0-bibtex-entry-language)))
            ("library" (lambda ()
                         (setq sync0-bibtex-entry-library
                               (completing-read "Choose location to trace back: "
                                                sync0-bibtex-completion-library nil nil nil))))
            ("file" (lambda ()
                      (setq sync0-bibtex-entry-file
                            (concat "/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf"))))
            ("journaltitle" (lambda ()
                              (setq sync0-bibtex-entry-journaltitle
                                    (completing-read "Titre du journal : " sync0-bibtex-completion-journaltitle))))
            ("location" (lambda ()
                          (setq sync0-bibtex-entry-location
                                (completing-read "Location : " sync0-bibtex-completion-location))))
            ("note" (lambda ()
                      (setq sync0-bibtex-entry-note
                            (completing-read "Quelle note ou addenda ? : "
                                             sync0-bibtex-completion-note))))
            ("eventtitle" (lambda ()
                            (setq sync0-bibtex-entry-eventtitle
                                  (read-string "Event title : "))))
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
            ("chapter" (lambda ()
                         (setq sync0-bibtex-entry-chapter
                               (read-string "Chapter : "))))
            ("pages" (lambda ()
                       (setq sync0-bibtex-entry-pages
                             (read-string "Pages (ex. : 90-180) : "))))
            ("doi" (lambda ()
                     (setq sync0-bibtex-entry-doi
                           (read-string "DOI : " nil nil nil t))))
            ("medium" (lambda ()
                        (setq sync0-bibtex-entry-medium
                              (string-trim
                               (prin1-to-string
                                (completing-read-multiple "Quel support ?"
                                                          sync0-bibtex-completion-medium)) "(\"" "\")"))))
            ("institution" (lambda ()
                             (setq sync0-bibtex-entry-institution
                                   (read-string "Institution : "))))
            ("eventdate" (lambda ()
                           (setq sync0-bibtex-entry-eventdate
                                 (read-string "Event date (ex. 1990-12-28) : "))))
            ("shorthand" (lambda ()
                           (setq sync0-bibtex-entry-shorthand
                                 (read-string "Shorthand : "))))
            ("shorttitle" (lambda ()
                            (setq sync0-bibtex-entry-shorttitle
                                  (read-string "Shorthand : "))))
            ("crossref" (lambda ()
                          (when sync0-bibtex-entry-derivation
                            (setq sync0-bibtex-entry-crossref 
                                  (sync0-bibtex-completion-choose-key t t))
                            (setq sync0-bibtex-entry-crossref-entry
                                  (bibtex-completion-get-entry sync0-bibtex-entry-crossref))
                            (setq sync0-bibtex-entry-initial-date
                          (bibtex-completion-get-value "date" sync0-bibtex-entry-crossref-entry)
                          sync0-bibtex-entry-initial-origdate
                          (bibtex-completion-get-value "origdate" sync0-bibtex-entry-crossref-entry)
                          sync0-bibtex-entry-initial-author
                          (if (or (string= type "Collection")
                                  (string= type "InCollection")
                                  (string= type "Proceedings")
                                  (string= type "InProceedings"))
                              (bibtex-completion-get-value "editor" sync0-bibtex-entry-crossref-entry)
                            (bibtex-completion-get-value "author" sync0-bibtex-entry-crossref-entry))
                          sync0-bibtex-entry-initial-language
                          (bibtex-completion-get-value "language" sync0-bibtex-entry-crossref-entry)))))
            ("related" (lambda ()
                         (setq sync0-bibtex-entry-related (sync0-bibtex-completion-choose-key nil t))
                         (setq sync0-bibtex-entry-relatedtype
                               (completing-read "Type de relation : "
                                                '("multivolume" "origpubas" "reviewof" "reprintof" "reprintas" "reprintfrom" "translationas" "translationfrom" "translationof")))))
            ("keywords" (lambda ()
                          (setq sync0-bibtex-entry-keywords
                                (concat "reference/" (downcase sync0-bibtex-entry-type) ", "
                                        (unless (sync0-null-p sync0-bibtex-entry-date)
                                          (concat "date/" sync0-bibtex-entry-date ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-origdate)
                                          (concat "origdate/" sync0-bibtex-entry-origdate ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-crossref)
                                          (concat "crossref/" sync0-bibtex-entry-crossref ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-language)
                                          (concat "language/" sync0-bibtex-entry-language ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-edition)
                                          (concat "edition/" sync0-bibtex-entry-edition ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-related)
                                          (concat "related/" sync0-bibtex-entry-related ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-relatedtype)
                                          (concat "relatedtype/" sync0-bibtex-entry-relatedtype ", "))
                                        (unless (sync0-null-p sync0-bibtex-entry-author-tag)
                                          (concat "author/" sync0-bibtex-entry-author-tag))
                                        (when-let* ((extra-keywords (completing-read-multiple "Input extra keywords: " 
                                                                                              sync0-bibtex-completion-keywords))
                                                    (keywords-string
                                                     (unless (sync0-null-p extra-keywords)
                                                       (sync0-show-elements-of-list extra-keywords ", ")))) 
                                          (concat ", " keywords-string))))))))


    (defun sync0-bibtex-completion-load-entry (&optional bibkey)
      "Function to load the variables required for the different
  bibtex functions to work. It loads the values according to the
  predefined rules set in ... or according to the value of the
  bibtex entry in the bibliography files."
      (interactive)
      (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
      (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
      (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
      (let* ((entry (when bibkey (bibtex-completion-get-entry bibkey)))
             (type (if entry
                       (bibtex-completion-get-value "=type="  entry)
                     (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types)))
             ;; Make sure that all bibtex fields are taken into
             ;; account when gathering info. from the bibtex file.
             ;; When creatin a field, use only the default fields for
             ;; each entry type.
             (fields (if bibkey
                         sync0-bibtex-fields
                       (cdr (assoc type sync0-bibtex-type-fields)))))
        (setq sync0-bibtex-entry-key
              (or bibkey 
                  (format-time-string "%Y%m%d%H%M%S")))
        (setq sync0-bibtex-entry-type type)
        (dolist (element fields)
          (if entry
              (when-let ((value (bibtex-completion-get-value element entry)))
                (set (intern (concat "sync0-bibtex-entry-" element)) value))
            (funcall (cadr (assoc element sync0-bibtex-fields-functions-one))))))
      ;; define the helper fields for the other functions to work
      ;; correctly. Otherwise, only defining the bibtex fields will
      ;; generate errors when calling the functions to extract pdfs or
      ;; crete reading notes.
      ;; author
      (setq sync0-bibtex-entry-author-fixed
            (unless (sync0-null-p sync0-bibtex-entry-author)
              (cond ((string-match " and " sync0-bibtex-entry-author)
                     ;; create a list with parts 
                     (let* ((author-list  (split-string sync0-bibtex-entry-author " and "))
                            (names (let (x)
                                     (dolist  (element author-list x)
                                       (setq x (concat x element "\", \""))))))
                       (concat "\"" (substring names 0 -2))))
                    ;; check when author is an organization
                    ((string-match "^{" sync0-bibtex-entry-author)
                     (concat "\"" (substring sync0-bibtex-entry-author 1 -1) "\""))
                    ;; other cases
                    (t (concat "\"" sync0-bibtex-entry-author "\"")))))
      (setq sync0-bibtex-entry-lastname
            (unless (sync0-null-p sync0-bibtex-entry-author)
              (cond ((string-match " and " sync0-bibtex-entry-author)
                     ;; create a list with parts 
                     (let* ((author-list  (split-string sync0-bibtex-entry-author " and "))
                            (last-names (let (x)
                                          (dolist  (element author-list x)
                                            (setq x (concat x
                                                            (progn
                                                              (string-match "\\([[:print:]]+\\),"   element)
                                                              (match-string 1 element))
                                                            ", "))))))
                       (substring last-names 0 -2)))
                    ((string-match "^{" sync0-bibtex-entry-author)
                     (string-match "{\\([[:print:]]+\\)}" sync0-bibtex-entry-author)
                     (match-string 1 sync0-bibtex-entry-author))
                    (t (nth 0 (split-string sync0-bibtex-entry-author ", "))))))
      (setq sync0-bibtex-entry-author-tag
            (unless (sync0-null-p sync0-bibtex-entry-author)
              (cond ((string-match " and " sync0-bibtex-entry-author)
                     (let* ((no-comma (replace-regexp-in-string ", " "_" (downcase sync0-bibtex-entry-author)))
                            (no-space (replace-regexp-in-string "[[:space:]]+" "-" no-comma)))
                       (replace-regexp-in-string "-and-" ", author/" no-space)))
                    ((string-match "^{" sync0-bibtex-entry-author)
                     (string-match "{\\([[:print:]]+\\)}" sync0-bibtex-entry-author)
                     (downcase (match-string 1 sync0-bibtex-entry-author)))
                    (t (let ((raw (replace-regexp-in-string ", " "_" sync0-bibtex-entry-author)))
                         (downcase (replace-regexp-in-string " " "-" raw)))))))
      ;; dates
      (setq sync0-bibtex-entry-date-tag
            (unless (sync0-null-p sync0-bibtex-entry-date)
              (replace-regexp-in-string "-" "/" sync0-bibtex-entry-date)))
      (setq sync0-bibtex-entry-date-fixed
            (if (or (sync0-null-p sync0-bibtex-entry-origdate)
                    (string= sync0-bibtex-entry-date sync0-bibtex-entry-origdate))
                (concat "(" sync0-bibtex-entry-date ")")
              (concat "(" sync0-bibtex-entry-origdate ") (" sync0-bibtex-entry-date ")")))
      ;; title
      (setq sync0-bibtex-entry-title-fixed
            (if (sync0-null-p sync0-bibtex-entry-subtitle)
                (concat sync0-bibtex-entry-title
                        (unless (sync0-null-p sync0-bibtex-entry-volume) (concat ", T." sync0-bibtex-entry-volume)))
              (concat sync0-bibtex-entry-title
                      (unless (sync0-null-p sync0-bibtex-entry-volume) (concat ", T." sync0-bibtex-entry-volume)) " : " sync0-bibtex-entry-subtitle)))
      (setq sync0-bibtex-entry-title-compatible
            (replace-regexp-in-string "[[:blank:]]*:[[:blank:]]*" "_" sync0-bibtex-entry-title-fixed))
      ;; parent
      ;; (setq sync0-bibtex-entry-parent
      ;;       (cond ((string= sync0-bibtex-entry-type "Article")

      ;;              (bibtex-completion-get-value "journaltitle" entry))
      ;;             ((or (string= sync0-bibtex-entry-type "InBook")
      ;;                  (string= sync0-bibtex-entry-type "InCollection")
      ;;                  (string= sync0-bibtex-entry-type "InProceedings"))

      ;;              (if bibkey
      ;;                  (sync0-bibtex-entry-define-booktitle bibkey)
      ;;                (sync0-bibtex-entry-define-booktitle))

      ;;              (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
      ;;                  sync0-bibtex-entry-booktitle
      ;;                (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
      ;;             (t (bibtex-completion-get-value "series" entry))))
      ) 

    (defun sync0-bibtex-define-entry ()
      "Create new BibLaTeX entry in the default bibliography. When
   optional quick is non-nil, only capture the minimal fields
   required to create a new entry."
      (interactive)
      ;; Before calculating any values, reset all values in my
      ;; master list of variable (set them to nil).
      (setq sync0-bibtex-entry-derivation (yes-or-no-p "Derive entry?"))
      (sync0-bibtex-completion-load-entry)
      (when (or (string= sync0-bibtex-entry-type "InCollection")
                (string= sync0-bibtex-entry-type "InBook")
                (string= sync0-bibtex-entry-type "InProceedings"))
        (sync0-bibtex-extract-pdf))
      ;; Insert entry in default bibliography file
      (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
      (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
      ;; Reset these two values to prevent unwanted extractions
      (unless (sync0-null-p sync0-bibtex-entry-url)
        (when (yes-or-no-p "Download the attached pdf? ")
          (sync0-bibtex-download-pdf t))))

    (defun sync0-bibtex-extract-pdf ()
      "Extract a pdf from a parent entry."
      (when (yes-or-no-p "Extract  PDF from existing entry?")
        (if (yes-or-no-p "Use page numbers from extraction?")
            (let ((beg (progn
                         (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
                         (match-string 1 sync0-bibtex-entry-pages)))
                  (end (progn
                         (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
                         (match-string 1 sync0-bibtex-entry-pages))))
              (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end))
          (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key))))

;; (defun sync0-bibtex-extract-subpdf ()
;;   "Extract pdf from an existing pdf."
;;   ;; Set all fields to nil 
;;   (interactive)
;;   (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
;;             (input-file (car (bibtex-completion-find-pdf bibkey)))
;;             (output-file (concat sync0-pdfs-folder bibkey "a.pdf"))
;;             (range (read-string "Page range: "))
;;             (command (concat "pdftk " input-file " cat " range " output " output-file)))
;;     (if (file-exists-p input-file)
;;         (shell-command command)
;;       (message "No PDF found for %s" bibkey))))



    ;; (foo "20210525144408")

    ;; (let ((element "author")
    ;;       (entry (bibtex-completion-get-entry "20210525144408")))
    ;;           (set (intern (concat "sync0-bibtex-entry-" element)) (bibtex-completion-get-value element entry)))

    ;; (foo "20210525144408")

    ;; (defun foo (&optional bibkey)
    ;;   (interactive)
    ;; (setq sync0-bibtex-entry-key
    ;;       (or bibkey 
    ;;       (format-time-string "%Y%m%d%H%M%S"))))



;; (defun my-modify-citations ()
;;   "Modify citation commands in the current buffer based on my-bib-alist."
;;   (interactive)
;;   (let ((bib-key-alist '())) ; Temporary alist to store bibkey and labels
;;     ;; Build the bib-key-alist from my-bib-alist
;;     (dolist (bibfile my-bib-alist)
;;       (let ((label (car bibfile))
;;             (path (cdr bibfile)))
;;         ;; Read the bib file and extract citation keys
;;         (with-temp-buffer
;;           (insert-file-contents path)
;;           (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
;;             (let ((bibkey (match-string 1)))
;;               ;; Add the bibkey and its corresponding label to the alist
;;               (push (cons bibkey label) bib-key-alist))))))
    
;;     ;; Search for citation commands in the current buffer
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward "\\\\cite\\w*\\(\\[.*?\\]\\)?{\\([^}]+\\)}" nil t)
;;         (let ((options (match-string 1))
;;               (key (match-string 2)))
;;           ;; Find the corresponding label for the bibkey
;;           (let ((label (cdr (assoc key bib-key-alist))))
;;             (when label
;;               ;; Modify the citation command to include the label
;;               (replace-match (concat "\\\\cite" label options "{" key "}") t))))))))

;; (defun my-modify-citations ()
;;   "Modify citation commands in the current buffer based on my-bib-alist."
;;   (interactive)
;;   (let ((bib-key-alist '()))  ;; Temporary alist to store bibkey and labels
;;     ;; Build the bib-key-alist from my-bib-alist
;;     (dolist (bibfile my-bib-alist)
;;       (let ((label (car bibfile))
;;             (path (cdr bibfile)))
;;         ;; Read the bib file and extract citation keys
;;         (with-temp-buffer
;;           (insert-file-contents path)
;;           (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
;;             (let ((bibkey (match-string 1)))
;;               ;; Add the bibkey and its corresponding label to the alist
;;               (push (cons bibkey label) bib-key-alist))))))
    
;;     ;; Regex to match different citation commands
;;     (let ((citation-regexp "\\\\cite\\(t\\|p\\|alp\\|alt\\|author\\|year\\|text\\)*\\(\\[.*?\\]\\)?{\\([^}]+\\)}"))
;;       ;; Search and modify citation commands in the current buffer
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward citation-regexp nil t)
;;           (let ((cmd (match-string 0)) ;; Full match (entire command)
;;                 (options (match-string 2)) ;; Citation options (like page numbers)
;;                 (key (match-string 3))) ;; Bib key
;;             ;; Find the corresponding label for the bibkey
;;             (let ((label (cdr (assoc key bib-key-alist))))
;;               (when label
;;                 ;; Modify the citation command to include the label

;;                 (replace-match (concat "\\\\cite" label options "{" key "}") t)))))))))

