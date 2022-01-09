;; -*- lexical-binding: t -*-

 ;; (defmacro defvar* (vars initial-value)
 ;;  `(progn
 ;;     ,@(loop for var in vars
 ;;             do (check-type var symbol)
 ;;             collect `(defvar ,var ,initial-value))))

(require 'sync0-print)
(require 'sync0-pdf)

(defvar sync0-bibtex-archived-bibliography "/home/sync0/Dropbox/bibliographies/archived.bib"
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-tocs-directory "/home/sync0/Documents/tocs/"
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-author-separator "_"
  "Separating character used for distinguishing lastname from
  name. Beware, this variable should only bet set to
  non-alphabetic characters or any other character that could
  appear in names.")

(defvar sync0-bibtex-author-lastname-separator "+"
  "Separating character used for distinguishing when an author
  has several lastnames. Beware, this variable should be set only
  to non-alphabetic characters or any other character that could
  appear in last names to prevent unwanted results.")

;; use this to define all the dummy fields required for creating
;; bibtex entries
(let ((prefix "sync0-bibtex-entry-")
      x)
  (dolist (element sync0-bibtex-fields x)
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var x)))
  (setq sync0-bibtex-entry-definitions-list (nreverse x)))

;; Define dummy variables for initial fields
(let ((prefix "sync0-bibtex-entry-initial-")
      x)
  (dolist (element '("date" "origdate" "author" "language") x)
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var x)))
  (setq sync0-bibtex-entry-initial-fields-list x))

;; define helper variables 
(defvar sync0-bibtex-entry-crossref-entry nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-title-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-title-compatible nil
  "Dummy variable used for title of files to be copied to a usb or other media.")

(defvar sync0-bibtex-entry-date-tag nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-date-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-author-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-lastname nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-author-tag nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-key nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-extraction-fields-list 
  '(sync0-bibtex-entry-crossref-entry))

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
         "medium"
         "library")
        ("Collection" "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "medium"
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
         "pages"
         "medium"
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
         "pages"
         "medium"
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
         "pages"
         "medium"
         "library")
        ("Manual" "institution"
         "origdate"
         "edition"
         "volume"
         "series"
         "publisher"
         "location"
         "medium"
         "library"
         "note")
        ("Online" "institution"
         "series"
         "publisher"
         "medium"
         "library"
         "note")
        ("Report" "institution"
         "volume"
         "number"
         "series"
         "publisher"
         "location"
         "medium"
         "library"
         "note")
        ("Unpublished" "institution"
         "volume"
         "number"
         "series"
         "publisher"
         "location"
         "medium"
         "library"
         "note")
        ("Misc" "institution"
         "series"
         "publisher"
         "location"
         "medium"
         "library"
         "note")
        ("Thesis" "institution"
         "number"
         "publisher"
         "location"
         "medium"
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
         "medium"
         "library"
         "note")))

(setq sync0-bibtex-entry-functions
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
                        (concat ":/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf:PDF"))))
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
                       (let ((initial-input
                              (concat sync0-bibtex-entry-title-fixed ", p. " sync0-bibtex-entry-pages)))
                         (setq sync0-bibtex-entry-shorthand
                               (read-string "Shorthand : " initial-input)))))
        ("shorttitle" (lambda ()
                        (setq sync0-bibtex-entry-shorttitle
                              (read-string "Shorthand : "))))
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
                     (setq sync0-bibtex-entry-related (sync0-bibtex-completion-choose-key nil t))
                     (setq sync0-bibtex-entry-relatedtype
                           (completing-read "Type de relation : "
                                            '("multivolume" "origpubas" "reviewof" "reprintof" "reprintas" "reprintfrom" "translationas" "translationfrom" "translationof")))))
        ("keywords" (lambda ()
                      (setq sync0-bibtex-entry-keywords
                            (concat "reference/" sync0-bibtex-entry-type-downcase ", "
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
                                    (if-let* ((extra-keywords (completing-read-multiple "Input extra keywords: " 
                                                                                          sync0-bibtex-completion-keywords))
                                                (keywords-string
                                                 (unless (sync0-null-p extra-keywords)
                                                   (sync0-show-elements-of-list extra-keywords ", ")))) 
                                        (concat ", " keywords-string)
                                      "")))))))

    (defun sync0-bibtex-completion-load-entry (&optional bibkey quick)
      "Function to load the variables required for the different
  bibtex functions to work. It loads the values according to the
  predefined rules set in ... or according to the value of the
  bibtex entry in the bibliography files."
      (interactive)
      (sync0-nullify-variable-list (append 
                                    sync0-bibtex-entry-definitions-list
                                    sync0-bibtex-entry-extraction-fields-list
                                    sync0-bibtex-entry-initial-fields-list))
      ;; First load fields
      (let* ((entry (when bibkey (bibtex-completion-get-entry bibkey)))
             (type (if entry
                       (bibtex-completion-get-value "=type="  entry)
                     (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types)))
             ;; Make sure that all bibtex fields are taken into
             ;; account when gathering info. from the bibtex file.
             ;; When creatin a field, use only the default fields for
             ;; each entry type.
             (fields (cond (bibkey
                            sync0-bibtex-fields)
                           (quick
                            sync0-bibtex-base-fields)
                           (t (append
                               (cdr (assoc type sync0-bibtex-type-fields))
                               sync0-bibtex-base-fields)))))
        (setq sync0-bibtex-entry-key
              (or bibkey 
                  (format-time-string "%Y%m%d%H%M%S")))
        (setq sync0-bibtex-entry-urldate
              (format-time-string "%Y-%m-%d")) 
        (setq sync0-bibtex-entry-type type)
        ;; the following field is used to avoid inconsistensies in
        ;; case when using the type for boolean operations
        (setq sync0-bibtex-entry-type-downcase (downcase type))
        (dolist (element fields)
          (if entry
              (when-let ((value (bibtex-completion-get-value element entry)))
                (set (intern (concat "sync0-bibtex-entry-" element)) value))
            (funcall (cadr (assoc element sync0-bibtex-entry-functions))))
          ;; added this last function to see if I can get completion to work
          (sync0-bibtex-update-var element)))
      ;; define the helper fields for the other functions to work
      ;; correctly. Otherwise, only defining the bibtex fields will
      ;; generate errors when calling the functions to extract pdfs or
      ;; crete reading notes.
      ;; author
      ;; 
      ;; 
      ;;;; load the following to be used, if not called, then the
      ;;;; crossref fields will not be loaded at all when calling
      ;;;; bibkey to load existing fields and this can lead to
      ;;;; unexpected results
      (when (and bibkey
                 sync0-bibtex-entry-crossref)
        (setq sync0-bibtex-entry-crossref-entry
              (bibtex-completion-get-entry sync0-bibtex-entry-crossref)))
      ;;; used for titles and such
      (setq sync0-bibtex-entry-author-fixed
            (unless (sync0-null-p sync0-bibtex-entry-author)
              (cond ((string-match " and " sync0-bibtex-entry-author)
                     ;; create a list with parts 
                     (let* ((author-list  (split-string sync0-bibtex-entry-author " and "))
                            (names (let (x)
                                     (dolist  (element author-list x)
                                       (setq x (concat x element "\", \""))))))
                       (concat "\"" (substring names 0 -3))))
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
                        (unless (or (sync0-null-p sync0-bibtex-entry-volume)
                                    (when sync0-bibtex-entry-crossref
                                      (bibtex-completion-get-value "volume" sync0-bibtex-entry-crossref-entry)))
                          (concat ", T." sync0-bibtex-entry-volume)))
              (concat sync0-bibtex-entry-title
                      (unless (or (sync0-null-p sync0-bibtex-entry-volume)
                                  (when sync0-bibtex-entry-crossref
                                    (bibtex-completion-get-value "volume" sync0-bibtex-entry-crossref-entry)))
                        (concat ", T." sync0-bibtex-entry-volume)) " : " sync0-bibtex-entry-subtitle)))
      (setq sync0-bibtex-entry-title-compatible
            (replace-regexp-in-string "[[:blank:]]*:[[:blank:]]*" "_" sync0-bibtex-entry-title-fixed))
      ;; (setq sync0-bibtex-entry-title-aliases-full
      ;;    (if (sync0-null-p sync0-bibtex-entry-subtitle)
      ;;     (concat (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " "))
      ;;             sync0-bibtex-entry-date-fixed
      ;;             " "
      ;;             sync0-bibtex-entry-title-fixed
      ;;             (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")))
      ;;     (concat (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " "))
      ;;             sync0-bibtex-entry-date-fixed
      ;;             " "
      ;;             sync0-bibtex-entry-title-fixed
      ;;             (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")))))
      ;; (setq sync0-bibtex-entry-title-aliases-short
      ;;       (concat (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " "))
      ;;               sync0-bibtex-entry-date-fixed
      ;;               " "
      ;;               sync0-bibtex-entry-title
      ;;               (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)"))))
      ;; parent
      (setq sync0-bibtex-entry-parent
            (cond ((string= sync0-bibtex-entry-type-downcase "article")
                   sync0-bibtex-entry-journaltitle)
                  ((or (string= sync0-bibtex-entry-type-downcase "inbook")
                       (string= sync0-bibtex-entry-type-downcase "incollection")
                       (string= sync0-bibtex-entry-type-downcase "inproceedings"))
                   (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
                       sync0-bibtex-entry-booktitle
                     (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
                  (t sync0-bibtex-entry-series)))) 

(defun sync0-bibtex-choose-attachment (&optional input)
  "REWRITE! Function used to select an attachment to be read by
some other programs. This function is intenede to be used in
pipes and not standalone. This function requires package
bibtex-completion to be loaded; otherwise, fails."
  (interactive)
  (let  ((raw-string (or input
                         sync0-bibtex-entry-file))
         (my-match nil)
         (my-sequence bibtex-completion-pdf-extension)
         result)
    (if (string-match ";" raw-string)
        (let* ((parts
                (split-string raw-string ";"))
               (selection
                (completing-read "Choose an attachment to open: " parts)))
          (while (null my-match)
            (let* ((my-car (upcase (substring (car my-sequence) 1 nil)))
                   (regex (concat my-car "$"))
                   (tail (concat ":" my-car)))
              (if (string-match regex selection)
                  (progn 
                    (setq my-match t)
                    (setq result (string-trim selection ":" tail)))
                (setq my-sequence (cdr my-sequence)))))
          result)
      (progn 
        (while (null my-match)
          (let* ((my-car (upcase (substring (car my-sequence) 1)))
                 (regex (concat my-car "$"))
                 (tail (concat ":" my-car)))
            (if (string-match regex raw-string)
                (progn 
                  (setq my-match t)
                  (setq result (string-trim raw-string ":" tail)))
              (setq my-sequence (cdr my-sequence)))))
        result))))

;; (defun sync0-bibtex-choose-attachment (&optional input)
;;   "Function used to select an attachment to be read by some other
;; programs. This function is intenede to be used in pipes and not
;; standalone."
;;   (interactive)
;;   (let  ((raw-string (or input
;;                          sync0-bibtex-entry-file)))
;;     (if (string-match ";" raw-string)
;;         (let*              ((parts
;;                              (split-string raw-string ";"))
;;                             (selection
;;                              (completing-read "Choose an attachment to open: " parts))
;;                             (my-match nil)
;;                             (my-sequence bibtex-completion-pdf-extension))
;;           (while (null my-match)
;;             (let* ((my-car (upcase (substring (car my-sequence) 1 nil)))
;;                    (regex (concat my-car "$"))
;;                    (tail (concat ":" my-car)))
;;               (if (string-match regex selection)
;;                   (progn 
;;                     (setq my-match t)
;;                     (string-trim selection ":" tail))
;;                 (setq my-sequence (cdr my-sequence))))))
;;       (let*              ((my-match nil)
;;                           (my-sequence bibtex-completion-pdf-extension))
;;         (while (null my-match)
;;           (let* ((my-car (upcase (substring (car my-sequence) 1 nil)))
;;                  (regex (concat my-car "$"))
;;                  (tail (concat ":" my-car)))
;;             (if (string-match regex raw-string)
;;                 (progn 
;;                   (setq my-match t)
;;                   (string-trim raw-string ":" tail))
;;               (setq my-sequence (cdr my-sequence)))))))))

      ;; (if (string-match "EPUB$" raw-string)
      ;;     (string-trim raw-string ":" ":EPUB")
      ;;   (string-trim raw-string ":" ":PDF")))))

(defun sync0-bibtex-crossref-extract-pdf (&optional beg end)
  "Extract pdf from crossref defined by
   sync0-bibtex-entry-crossref. Be careful! Filename is not
   automatically calculated because this function is not
   standalone; it is intended to be part of pipes. Therefore,
   sync0-bibtex-entry-key, which will be used to name the
   extracted pdf must have been set correctly previously by a
   function."
  (when (yes-or-no-p "Extract  PDF from existing entry?")
    (when-let* ((origfile (when sync0-bibtex-entry-crossref
                            (car (bibtex-completion-find-pdf sync0-bibtex-entry-crossref))))
                (file (when origfile
                        (concat sync0-pdfs-folder sync0-bibtex-entry-key ".pdf" origfile)))
                (message-function (lambda ()
                                    (message "Pdf for entry %s already present in default pdf folder." sync0-bibtex-entry-key)))
                (base-command "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="))
      (cond ((and beg end)
             (let ((command (concat
                             base-command
                             beg
                             " -dLastPage="
                             end
                             " -sOutputFile="
                             file)))
               (if (file-exists-p file)
                   (funcall message-function)
                 (shell-command command))))
            ((yes-or-no-p "Use page numbers from extraction?")
             (let* ((beg (progn
                           (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
                           (match-string 1 sync0-bibtex-entry-pages)))
                    (end (progn
                           (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
                           (match-string 1 sync0-bibtex-entry-pages)))
                    (command (concat
                              base-command
                              beg
                              " -dLastPage="
                              end
                              " -sOutputFile="
                              file)))
               (if (file-exists-p file)
                   (funcall message-function)
                 (shell-command command))))
            (t (let* ((beg (read-string "First page for extraction: "))
                      (end (read-string "Last page for extraction: "))
                      (command (concat
                                base-command
                                beg
                                " -dLastPage="
                                end
                                " -sOutputFile="
                                file)))
                 (if (file-exists-p file)
                   (funcall message-function)
                   (shell-command command))))))))

    (defun sync0-bibtex-define-entry (&optional quick)
      "Create new BibLaTeX entry in the default bibliography. When
   optional quick is non-nil, only capture the minimal fields
   required to create a new entry."
      (interactive)
      ;; Before calculating any values, reset all values in my
      ;; master list of variable (set them to nil).
      ;; (setq sync0-bibtex-entry-derivation (yes-or-no-p "Derive entry?"))
      (sync0-bibtex-completion-load-entry nil quick)
      (when (or (string= sync0-bibtex-entry-type-downcase "incollection")
                (string= sync0-bibtex-entry-type-downcase "inbook")
                (string= sync0-bibtex-entry-type-downcase "inproceedings"))
        (sync0-bibtex-crossref-extract-pdf))
      ;; Insert entry in default bibliography file
      (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
      (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
      ;; the function below does not work; throws an error listp that I
      ;; have not been able to solve (sync0-bibtex-update-vars
      ;; sync0-bibtex-completion-variables-list)
      (unless (sync0-null-p sync0-bibtex-entry-url)
        (when (yes-or-no-p "Download the attached pdf? ")
          (sync0-bibtex-download-pdf t))))

;; (defun sync0-bibtex-entry-define-related (&optional bibkey)
;;   "Define the author for new BibLaTeX entry."
;;   (if bibkey 
;;       (let ((entry (bibtex-completion-get-entry bibkey)))
;;         (setq sync0-bibtex-entry-related
;;               (bibtex-completion-get-value "related" entry))
;;         (setq sync0-bibtex-entry-relatedtype
;;               (bibtex-completion-get-value "relatedtype" entry)))
;;     (progn
;;         (setq sync0-bibtex-entry-related (sync0-bibtex-completion-choose-key nil t))
;;         (setq sync0-bibtex-entry-relatedtype
;;               (completing-read "Type de relation : " '("multivolume" "origpubas" "reviewof" "reprintof" "reprintas" "reprintfrom" "translationas" "translationfrom" "translationof"))))))

  (defvar sync0-bibtex-completion-variables-list
    '(("publisher" sync0-bibtex-entry-publisher sync0-bibtex-completion-publisher "~/.emacs.d/sync0-vars/bibtex-completion-publisher.txt")
      ("journaltitle" sync0-bibtex-entry-journaltitle sync0-bibtex-completion-journaltitle "~/.emacs.d/sync0-vars/bibtex-completion-journaltitle.txt")
      ("location" sync0-bibtex-entry-location sync0-bibtex-completion-location "~/.emacs.d/sync0-vars/bibtex-completion-location.txt")
      ("title" sync0-bibtex-entry-title sync0-bibtex-completion-title "~/.emacs.d/sync0-vars/bibtex-completion-title.txt")
      ("note" sync0-bibtex-entry-note sync0-bibtex-completion-note "~/.emacs.d/sync0-vars/bibtex-completion-note.txt")
      ("author" sync0-bibtex-entry-author sync0-bibtex-completion-author "~/.emacs.d/sync0-vars/bibtex-completion-author.txt")
      ("library" sync0-bibtex-entry-library sync0-bibtex-completion-library "~/.emacs.d/sync0-vars/bibtex-completion-library.txt")
      ("institution" sync0-bibtex-entry-institution sync0-bibtex-completion-institution "~/.emacs.d/sync0-vars/bibtex-completion-institution.txt")
      ("keywords" sync0-bibtex-entry-keywords sync0-bibtex-completion-keywords "~/.emacs.d/sync0-vars/bibtex-completion-keywords.txt")
      ("language" sync0-bibtex-entry-language sync0-bibtex-completion-language  "~/.emacs.d/sync0-vars/bibtex-completion-language.txt"))
    "List of variables used for updating completion files.")

(defun sync0-bibtex-update-var (field)
  "Update variables used for completion based on the information
   provided by the new entry."
  (when-let* ((my-list (assoc field sync0-bibtex-completion-variables-list))
             (new-object  (eval (cadr my-list)))
             (completion-list  (eval (caddr my-list)))
             (completion-file   (cadddr my-list)))
    (cond ((string= field "author")
           (let (aggregate)
           (cond ((string-match " and " new-object)
                  ;; create a list with parts 
                  (setq aggregate (append (split-string new-object " and ") aggregate)))
                 ;; check when author is an organization
                 ((string-match "^{" new-object)
                  (push  (substring new-object 1 -1) aggregate))
                 ;; other cases
                 (t (push new-object aggregate)))
           (dolist (element aggregate)
             ;; Check whether the item to be added is already present.
             (unless (member  element   completion-list)
               ;; Send the element to the list.
               (push element completion-list)
               ;; Send the element to the file.
               (append-to-file (concat element "\n") nil completion-file)))))
          ((string= field "keywords")
           (let (aggregate)
           (if (string-match ", " new-object)
               ;; create a list with parts 
               (setq aggregate (append (split-string new-object ", ") aggregate))
             ;; other cases
             (push new-object aggregate))
           (dolist (element aggregate)
             (unless (member  element   completion-list)
               ;; Send the element to the list.
               (push element completion-list)
               ;; Send the element to the file.
               (append-to-file (concat element "\n") nil completion-file)))))
          ;; Check whether the item to be added is already present.
          (t (unless (member  new-object  completion-list)
               ;; Send the element to the list.
               (push new-object completion-list)
               ;; Send the element to the file.
               (append-to-file (concat new-object "\n") nil completion-file))))))

;; (defun sync0-bibtex-update-var (field)
;;   "Update variables used for completion based on the information
;;    provided by the new entry."
;;   (when-let ((my-list (assoc field sync0-bibtex-completion-variables-list)))
;;     ;; Check whether the item to be added is empty.
;;     (unless (or (sync0-null-p  (cadr my-list))
;;                 ;; Check whether the item to be added is already present.
;;                 (member  (eval (cadr my-list))   (eval (caddr my-list))))
;;       ;; Send the element to the list.
;;       (push (cadr my-list) (caddr my-list))
;;       ;; Send the element to the file.
;;       (append-to-file (concat (eval (cadr my-list)) "\n") nil (cadddr my-list)))))

;; (defun sync0-bibtex-update-vars (seqlists)
;;   "Update variables used for completion based on the information
;;    provided by the new entry."
;;   (dolist (element seqlists)
;;     ;; Check whether the item to be added is empty.
;;     (unless (or (sync0-null-p  (cadr element))
;;                  ;; Check whether the item to be added is already present.
;;                  (member (eval (cadr element))  (eval (caddr element))))
;;       ;; Send the element to the list.
;;       (push (cadr element) (caddr element))
;;       ;; Send the element to the file.
;;       (append-to-file (concat (eval (cadr element)) "\n") nil (cadddr element)))))


;; FIX!
;; (defun sync0-bibtex-update-completion-files (seqlists)
;;   (interactive)
;;   (dolist (element seqlists)
;;     (let ((current-elements)
;;           (bib-elements (delete-duplicates 
;;                          (cond ((string= (car element) "author")
;;                                 (let ((my-list (bibtex-completion-candidates))
;;                                       aggregate)
;;                                   (dolist (current-author my-list aggregate)
;;                                     (when-let ((author  (cdr (assoc  "author" current-author))))
;;                                       (cond ((string-match " and " author)
;;                                              ;; create a list with parts 
;;                                             (setq aggregate (append (split-string author " and ") aggregate)))
;;                                             ;; check when author is an organization
;;                                             ((string-match "^{" author)
;;                                              (push  (substring author 1 -1) aggregate))
;;                                             ;; other cases
;;                                             (t (push author aggregate)))))))
;;                                ((string= (car element) "keywords")
;;                                 (let ((my-list (bibtex-completion-candidates))
;;                                       aggregate)
;;                                   (dolist (current-key my-list aggregate)
;;                                     (when-let ((keywords  (cdr (assoc  "keywords" current-key))))
;;                                       (if (string-match ", " keywords)
;;                                           ;; create a list with parts 
;;                                          (setq aggregate (append (split-string keywords ", ") aggregate))
;;                                         ;; other cases
;;                                            (push keywords aggregate))))))
;;                                (t (mapcar #'(lambda (x) (cdr (assoc (car element) x)))
;;                                           (bibtex-completion-candidates)))))))
;;       (with-temp-buffer
;;         (insert-file-contents (cadddr element))
;;         (goto-char (point-min))
;;         ;; (keep-lines "contexts" (point-min) (point-max)) 
;;         (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
;;           (push  (match-string 1) current-elements)))
;;       (with-temp-file (cadddr element)
;;         (if (string= (car element) "keywords")
;;             (set (caddr element) (delete-dups (append current-elements bib-elements bu-keywords-values)))
;;           (set (caddr element) (delete-dups (append current-elements bib-elements))))
;;         (sync0-insert-elements-of-list (eval (caddr element)))))))

(defun sync0-bibtex-entry-inform-new-entry ()
  "Inform the user about a new entry that has been just created."
  (if (sync0-null-p sync0-bibtex-entry-author)
      (message "Entry %s %s has been defined with key %s" sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)
    (message "Entry %s %s %s has been defined with key %s" sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)))

(defun sync0-bibtex-entry-append-to-bibliography (bibkey)
  "Append new BibLaTeX entry to default bibliography file.
   Beware, this function only constructs and appends the entry
   to the bib file; the values of the entry must have been defined
   elsewhere. For the sake of speed, this function does not
   perform any sanity checks on duplicate entries, and thus a
   unique correct entry is assumed to be supplied as mandatory
   argument bibkey."
  (let* ((definitions (mapcar #'eval sync0-bibtex-entry-definitions-list))
         (bibtex-fields (if (or (string= sync0-bibtex-entry-type-downcase "collection")
                                (string= sync0-bibtex-entry-type-downcase "proceedings"))
                            (cl-substitute "editor" "author" sync0-bibtex-fields :test #'string=)
                          sync0-bibtex-fields))
         (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields definitions))
         ;; define the bibtex entries
         (entries
          (let (x)
            (dolist (element fields x) 
              (unless (sync0-null-p (cadr element))
                (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
         (bibtex-entry (concat "@" sync0-bibtex-entry-type "{" bibkey ",\n" entries "}\n")))
    (if (string= sync0-default-bibliography (buffer-file-name))
        (progn
          (goto-char (point-max))
          (insert bibtex-entry)
          (sync0-bibtex-entry-inform-new-entry))
    (progn
      (append-to-file bibtex-entry nil sync0-default-bibliography)
      (sync0-bibtex-entry-inform-new-entry)))))

(defun sync0-bibtex-entry-inform-new-note (&optional rewrite)
  "Inform the user about a new entry that has been just created."
  (cond ((and rewrite
              (sync0-null-p sync0-bibtex-entry-author))
         (message "Note for %s %s with key %s has been updated." sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key))
        (rewrite 
         (message "Note for %s %s %s with key %s has been updated." sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key))
        (t (if (sync0-null-p sync0-bibtex-entry-author)
               (message "New note created for %s %s with key %s" sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)
             (message "New note created for %s %s %s with key %s" sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)))))

(defun sync0-bibtex-entry-create-obsidian-note-from-entry (bibkey &optional rewrite)
  "Create new markdown note for corresponding bibkey in default
obsidian vault. This function in not intended for interactive
use, but as a function to be included in pipes. When optional
rewrite is true, this function rewrites the YAML frontmatter of
the note, instead of attempting to create a new note."
  (let* ((obsidian-file (concat sync0-obsidian-directory bibkey ".md")) 
         (obsidian-yaml (concat "---\n"
                                "zettel_type: reference\n"
                                "id: " bibkey "\n"
                                "citekey: " bibkey "\n"
                                "created: " sync0-bibtex-entry-urldate "\n"
                                "biblatex_type: " (downcase sync0-bibtex-entry-type)  "\n"
                                "title: \"" sync0-bibtex-entry-title "\"\n"
                                (unless (sync0-null-p sync0-bibtex-entry-subtitle)
                                  (concat "subtitle: \"" sync0-bibtex-entry-subtitle "\"\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-author)
                                  (concat "author: [" sync0-bibtex-entry-author-fixed "]\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-crossref)
                                  (concat "crossref: " sync0-bibtex-entry-crossref "\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-parent)
                                  (concat "parent: \"" sync0-bibtex-entry-parent "\"\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-related)
                                  (concat "related: [" sync0-bibtex-entry-related "]\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-relatedtype)
                                  (concat "relatedtype: " sync0-bibtex-entry-relatedtype "\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-edition)
                                  (concat "edition: " sync0-bibtex-entry-edition "\n"))
                                (if (sync0-null-p sync0-bibtex-entry-subtitle)
                                    (concat "aliases: [\"" (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")) "\"]\n")
                                  (concat "aliases: [\"" (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")) "\", "
                                          "\"" (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed  " " sync0-bibtex-entry-title (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")) "\"]\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-url)
                                  (concat "url: \"" sync0-bibtex-entry-url "\"\n"))
                                (unless (sync0-null-p sync0-bibtex-entry-origdate)
                                  (concat "origdate: " sync0-bibtex-entry-origdate "\n"))
                                "date: " sync0-bibtex-entry-date "\n"
                                (unless (sync0-null-p sync0-bibtex-entry-medium)
                                  (concat "medium: [\"" sync0-bibtex-entry-medium "\"]\n"))
                                "language: " sync0-bibtex-entry-language "\n"
                                (unless (sync0-null-p sync0-bibtex-entry-library)
                                  (concat "library: [\"" sync0-bibtex-entry-library "\"]\n"))
                                "tags: [bibkey/" bibkey ", " sync0-bibtex-entry-keywords "]\n"
                                "---\n"
                                "# " (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")) "\n"))
         (obsidian-rest (concat  "\n## Description\n\n" 
                                 "## Progrès de la lecture\n\n" 
                                 "## Annotations\n\n"))
         (obsidian-entry (concat obsidian-yaml obsidian-rest)))
    (cond ((and rewrite
                (file-exists-p obsidian-file))
           (with-temp-file  obsidian-file
             (insert-file-contents obsidian-file)
             (goto-char (point-min))
             (let* ((regex "^# .+\n")
                    (end (re-search-forward regex nil t 1)))
               (kill-region (point-min) end)
               (insert obsidian-yaml)))
           (sync0-bibtex-entry-inform-new-note t))
          ((file-exists-p obsidian-file)
           (message "Error: %s.md already present in Obsidian vault." bibkey))
          (t (with-temp-buffer 
               (insert obsidian-entry)
               (write-file obsidian-file))
             (sync0-bibtex-entry-inform-new-note)))))

(defun sync0-bibtex-completion-choose-key (&optional unique pointer)
  "Choose key with completion. When optional pointer is t,
   preselect the entry at point."
  (if unique
  (let* ((entry (when (or pointer
                          (string= major-mode "bibtex-mode"))
                  (save-excursion (bibtex-beginning-of-entry)
			          (bibtex-parse-entry))))
         (preselect (when entry
                      (cdr (assoc "=key=" entry))))
         (candidates (bibtex-completion-candidates))
         (selection (ivy-read "Bibliography candidates: "
                              candidates
                              :preselect preselect
                              :caller 'ivy-bibtex
                              :history 'ivy-bibtex-history)))
    (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
  (let* ((entry (when (or pointer
                          (string= major-mode "bibtex-mode"))
                  (save-excursion (bibtex-beginning-of-entry)
			          (bibtex-parse-entry))))
         (preselect (when entry
                      (cdr (assoc "=key=" entry))))
         (candidates (bibtex-completion-candidates))
         (counter 0)
         selection
         x)
    (while (not (equal selection "nilnil"))
      (setq selection (ivy-read (format "Bibliography candidates%s: "
                                        (if (> counter 0)
                                            (concat " (selected: " (number-to-string counter) ")") ""))
                                candidates
                                :preselect preselect
                                :caller 'ivy-bibtex
                                :history 'ivy-bibtex-history))
      (setq counter (1+ counter))
      (setq x (concat (cdr (assoc "=key=" (cdr (assoc selection candidates)))) "," x)))
    (if (equal counter 1)
        (format "%s" x)
      (format "%s" (substring x 1 -1))))))

;; (defun sync0-bibtex-download-pdf (&optional creation)
;;   (interactive)
;;   (if creation
;;       (if (y-or-n-p "Call the buchaneers? ")
;;           (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF") t)
;;         (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF")))
;;     (when-let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
;;                    (entry (bibtex-completion-get-entry bibkey))
;;                    (url (bibtex-completion-get-value "url" entry))
;;                    (raw-pdf (bibtex-completion-get-value "file" entry))
;;                    (pdf (substring raw-pdf 1 -4)))
;;       (if (y-or-n-p "Call the buchaneers? ")
;;           (sync0-pdf-download-from-url url pdf t)
;;         (sync0-pdf-download-from-url url pdf)))))

(defun sync0-bibtex-download-pdf (&optional creation)
  (interactive)
  (if creation
      (if (y-or-n-p "Call the buchaneers? ")
          (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF") t)
        (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF")))
    (when-let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
                   (entry (bibtex-completion-get-entry bibkey))
                   (url (bibtex-completion-get-value "url" entry))
                   (raw-file (bibtex-completion-get-value "file" entry))
                   (pdf (sync0-bibtex-choose-attachment raw-file)))
      (if (y-or-n-p "Call the buchaneers? ")
          (sync0-pdf-download-from-url url pdf t)
        (sync0-pdf-download-from-url url pdf)))))

(defun sync0-bibtex-next-key ()
  "Print the bibtex key of the document"
  (interactive)
  (let ((bibtex-key (re-search-forward "@.+{" nil nil 1)))
    (goto-char bibtex-key)))

(defun sync0-bibtex-previous-key ()
  "Print the bibtex key of the document"
  (interactive)
  (let ((bibtex-key (re-search-backward "@.+{" nil nil 2)))
    (goto-char bibtex-key)
    (re-search-forward "@.+{" nil nil 1)))

(defun sync0-bibtex-update-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S"))
         (new-path (concat ":" sync0-pdfs-folder new-key ".pdf:PDF"))
         (entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (old-key (cdr (assoc "=key=" entry)))
         (attachment (when-let (x (cdr (assoc "file" entry)))
                       (string-trim x "{:" ":PDF}")))
         (type (cdr (assoc "=type=" entry)))
         (end
          (save-excursion
            (bibtex-end-of-entry)))
         (path-list (list "file" "Attachment path" new-path nil))
         (type-string
          (concat "@" type "{" new-key ",\n")))
    (bibtex-beginning-of-entry)
    (kill-whole-line 1)
    (insert type-string)
    (when (and (file-exists-p attachment)
               (yes-or-no-p "Rename existing attachment? "))
      (rename-file attachment (string-trim new-path ":" ":PDF")))
    ;; (unless (string= type "Online")
      ;; (if (re-search-forward "file[[:blank:]]+=[[:blank:]]+{" end t 1)
          ;; (progn 
          ;;   (kill-whole-line 1)
          ;;   (bibtex-make-field path-list t))
      (when attachment 
        (bibtex-make-field path-list t))
    (message "Key for entry %s has been replaced with key %s" old-key new-key)))

(defun sync0-bibtex-clean-entry ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S"))
         (directory "/home/sync0/Documents/pdfs/")
         (new-path (concat directory new-key ".pdf"))
         (regex "^@\\([[A-z]+\\){[[:alnum:]]+,")
         (beg
          (save-excursion
            (bibtex-beginning-of-entry)))
         (end
          (save-excursion
            (bibtex-end-of-entry)))
         (type
          (save-excursion
            (re-search-forward regex end t 1)
            (match-string-no-properties 1)))
         (language (completing-read "Choose language : "
                                    sync0-bibtex-completion-language))
         (language-string
          (concat "  language          = {"
                  language
                  "},\n  langid          = {"
                  language
                  "},\n" 
                  ))
         (type-string
          (concat "@" (upcase-initials type) "{" new-key ",\n")))
    (re-search-forward regex end t 1) 
    (kill-whole-line 1)
    (insert type-string)
    (save-excursion
      (when (re-search-forward "\\(journal\\)[[:blank:]]+=" end t 1)
        (replace-match "journaltitle" nil nil nil 1)))
    (save-excursion
      (when (re-search-forward "\\(year\\)[[:blank:]]+=" end t 1)
        (replace-match "date" nil nil nil 1)))
    (forward-line)
    (insert language-string)))

(defun sync0-bibtex-create-note-from-entry (&optional rewrite refkey no-extract)
  "Create a new Obsidian markdown note from an existing BibLaTeX
   entry in the default bibliography file. When optional rewrite
   is t, do not create a new file but simply rewrite an existing
   entry with the data of the corresponding bibtex entry in the
   default .bib file. When optional no-extract is true, do not
   attempt to extract a sub-pdf from its crossref (this feature
   is only useful when calling this function in loops to prevent
   undesired behavior)."
       (interactive)
       (let    ((bibkey (or refkey 
                            (sync0-bibtex-completion-choose-key t t))))
         (sync0-bibtex-completion-load-entry bibkey)
         (unless no-extract
           (when (or (string= sync0-bibtex-entry-type-downcase "incollection")
                     (string= sync0-bibtex-entry-type-downcase "inbook")
                     (string= sync0-bibtex-entry-type-downcase "inproceedings"))
             (sync0-bibtex-crossref-extract-pdf)))
         (if rewrite
             (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key t)
           (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key))))

(defun sync0-bibtex-extract-multiple-entries-from-pdf (seqlists)
  "Extract pdfs and create bibtex entries and markdown entries
   for each extranction. This function requires an input 'seqlists'
   that is a list composed of n lists of the form: (name,
   first-page, last-page). Likewise, for this to work, it is
   necessary that the pages of the pdf match the page numbers of the
   actual book. Otherwise, the entries will have wrong data."
  (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
            (extra-keyword (completing-read "Extra keywords to add : " sync0-bibtex-completion-keywords))
            (input-file (car (bibtex-completion-find-pdf bibkey)))
            (counter 1)
            (obsidian-master-note (concat sync0-obsidian-directory bibkey ".md")))
    (sync0-bibtex-completion-load-entry bibkey)
    (setq sync0-bibtex-entry-parent sync0-bibtex-entry-title-fixed)
    ;; Begin loop; specific settings for each note are to be defined
    ;; inside the loop.
    (dolist (elt seqlists)
      (setq counter (1+ counter))
      (let* ((raw-filename (+ (string-to-number (format-time-string "%Y%m%d%H%M%S")) counter))
             (filename (number-to-string raw-filename))
             ;; (file-pdf (concat ":/home/sync0/Documents/pdfs/" filename ".pdf:PDF"))
             (title (nth 0 elt))
             (beg (number-to-string (nth 1 elt)))
             (end (number-to-string (nth 2 elt)))
             (pages (if (string= beg end)
                        beg
                      (concat beg "-" end)))
             (pages-fixed (replace-regexp-in-string "-" "--" pages))
             (obsidian-reference
              (concat "- [" sync0-bibtex-entry-lastname " " sync0-bibtex-entry-date-fixed " " title "](" filename ".md)\n")))
        (setq sync0-bibtex-entry-key filename)
        (setq sync0-bibtex-entry-title title)
        (setq sync0-bibtex-entry-subtitle nil)
        (setq sync0-bibtex-entry-title-fixed sync0-bibtex-entry-title)
        (unless (sync0-null-p extra-keyword)
          (setq sync0-bibtex-entry-keywords
                (concat sync0-bibtex-entry-keywords ", " extra-keyword)))
        (setq sync0-bibtex-entry-pages pages)
        (setq sync0-bibtex-entry-shorthand
              (concat title ", p. " pages-fixed))
        (setq sync0-bibtex-entry-file
                (concat ":/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf:PDF"))
        ;; Beginning of loop actions
        ;; First, extract entry
        (sync0-bibtex-crossref-extract-pdf beg end)
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
        ;; Third, create an obsidian markdown note for the entry.
        (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
        ;; Fourth, add a markdown link in the obsidian master note
        ;; (ie., the note corresponding to the file used as the source
        ;; for extraction).
        (append-to-file obsidian-reference nil obsidian-master-note)))))

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

(defun sync0-bibtex-open-pdf-at-point (&optional zathura)
  "Open the pdf for bibtex key under point if it exists. If
   optional zathura is t, use zathura to open the pdf."
  (interactive)
  (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
         (entry (bibtex-completion-get-entry  bibkey))
         (raw-file (bibtex-completion-get-value "file" entry))
         (file (sync0-bibtex-choose-attachment raw-file)))
    (cond ((and zathura
                (file-exists-p file))
           (call-process "zathura" nil 0 nil file))
          ((file-exists-p file)
           (org-open-file file))
          (t (message "No PDF found for %s" bibkey)))))

(defun sync0-bibtex-open-url ()
  "Open the url for bibtex key under point if it exists."
  (interactive)
  (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
         (entry (bibtex-completion-get-entry bibkey))
         (url  (bibtex-completion-get-value "url" entry)))
    (if (not (sync0-null-p url))
        (bibtex-url)
      (message "No url found for %s" bibkey))))

(defun sync0-bibtex-open-notes ()
  "Open the notes for bibtex key under point in a cite link in a
buffer. Can also be called with key."
  (interactive)
  (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
         (notes-file  (concat sync0-obsidian-directory  bibkey ".md")))
    (if (file-exists-p notes-file)
        (find-file notes-file)
      (message "No markdown notes file found for entry %s" bibkey))))

(defun sync0-bibtex-print-pdf (&optional pdf command)
  "Print the pdf provided in the argument. Generalized for
interactive use and use in pipes to output to the printer."
  (interactive)
  (cond ((and pdf
              command)
         (if (file-exists-p pdf)
             (shell-command (concat command pdf))
           (message "File %s does not exist" pdf)))
        (pdf (let ((command (sync0-print-define-command)))
               (if (file-exists-p pdf)
                   (shell-command (concat command pdf))
                 (message "File %s does not exist" pdf))))
        (command (let* ((bibkey (sync0-bibtex-completion-choose-key))
                        (pdf (car (bibtex-completion-find-pdf bibkey))))
                   (if (file-exists-p pdf)
                       (shell-command (concat command pdf))
                     (message "No attachment found for entry %s" bibkey))))
        (t (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
                  (pdf (car (bibtex-completion-find-pdf bibkey)))
                  (command (sync0-print-define-command))) 
             (if (file-exists-p pdf)
                 (shell-command (concat command pdf))
               (message "No attachment found for entry %s" bibkey))))))

(defun sync0-bibtex-crop-pdf (&optional in-pdf in-cropbox)
  "Define the cropbox for pdf attached to a bibtex entry. This
function does not crop the pdf; it is a helper function do define
the cropbox. When called interactively, this functions calls
ivy-bibtex to search for the pdf attached to a bibtex entry."
  (interactive)
  (let* ((bibkey (unless in-pdf (sync0-bibtex-completion-choose-key)))
         (pdf (if bibkey
                  (car (bibtex-completion-find-pdf bibkey))
                in-pdf))
         (cropbox (if in-cropbox
                      in-cropbox
                    (sync0-pdf-define-cropbox pdf))) 
         (output (concat sync0-pdfs-folder "temp.pdf"))
         (command (concat "gs -o " output " -sDEVICE=pdfwrite" cropbox " /PAGES pdfmark\" -f " pdf)))
    (if (file-exists-p pdf)
        (progn 
          (shell-command command)
          (rename-file output pdf t)
          (message "Crop box has been redefined for %s" pdf))
      (message "No pdf found for %s" pdf))))

;; (defun sync0-bibtex-add-field ()
;;   (interactive)
;;   (sync0-nullify-variable-list (append 
;;                                 sync0-bibtex-entry-definitions-list
;;                                 sync0-bibtex-entry-extraction-fields-list
;;                                 sync0-bibtex-entry-initial-fields-list))
;;   (let* ((field (completing-read "Choose Bibtex field: " sync0-bibtex-fields))
;;          (entry (save-excursion (bibtex-beginning-of-entry)
;; 			        (bibtex-parse-entry)))
;;          (bibkey (cdr (assoc "=key=" entry))))
;;     (sync0-bibtex-completion-load-entry bibkey)
;;     (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
;;     (funcall (cadr (assoc field sync0-bibtex-fields-functions)))))

(defun sync0-bibtex-add-field ()
  (interactive)
  (sync0-nullify-variable-list (append 
                                sync0-bibtex-entry-definitions-list
                                sync0-bibtex-entry-extraction-fields-list
                                sync0-bibtex-entry-initial-fields-list))
  (let* ((field (completing-read "Choose Bibtex field: " sync0-bibtex-fields))
         (entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry))))
    (sync0-bibtex-completion-load-entry bibkey)
    (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
    (bibtex-beginning-of-entry)
    (bibtex-make-field (list field "Whatever string"
                             (eval (intern (concat "sync0-bibtex-entry-" field))) nil) t)))

(defun sync0-bibtex-copy-pdf-to-path (&optional in-path bibkey)
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (if bibkey 
      (sync0-bibtex-completion-load-entry bibkey)
    (sync0-bibtex-completion-load-entry))
  (let ((file (sync0-bibtex-choose-attachment)))
    (if (file-exists-p file)
        (let* ((target-path (or in-path
                                (read-string "Où envoyer ce pdf ? (finir en /) ")))
               (command (concat "cp "
                                file
                                " \""
                                target-path
                                sync0-bibtex-entry-lastname
                                " "
                                sync0-bibtex-entry-date-fixed
                                " "
                                sync0-bibtex-entry-title-compatible
                                ".pdf\"")))
          (shell-command command)
          (message "PDF for %s moved to target location" sync0-bibtex-entry-key))
      (message "No PDF found for %s" sync0-bibtex-entry-key))))

(defun sync0-bibtex-archive-entry (&optional bibkey)
  "Choose an entry to send to the archived bibliography. This
function fails when the entry is at the top of the buffer becase
the function 1- fails to compute."
  (interactive)
  (when bibkey
    (re-search-forward (concat "{" bibkey ",") nil t 1))
  (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
         (end (save-excursion (bibtex-end-of-entry))))
    (append-to-file beginning end sync0-bibtex-archived-bibliography)
    (delete-region beginning end)))

;; (defun sync0-bibtex-extract-toc-from-pdf (&optional bibkey)
;;   "Add toc to pdf using cpdf command line"
;;   (interactive)
;;   (if bibkey 
;;       (sync0-bibtex-completion-load-entry bibkey)
;;     (sync0-bibtex-completion-load-entry))
;;   (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
;;          (end (save-excursion (bibtex-end-of-entry))))

(defun sync0-bibtex-add-toc-to-pdf (&optional refkey)
  "Add toc to pdf using cpdf command line"
  (interactive)
  (let* ((bibkey (or refkey 
                     (sync0-bibtex-completion-choose-key t t)))
         (file (car (bibtex-completion-find-pdf bibkey)))
         ;; necessary to prevent malfunction due to same input
         ;; and output file
         (output (concat sync0-pdfs-folder "temp.pdf"))
         (toc-file (concat sync0-bibtex-tocs-directory bibkey ".txt"))
         ;; (malformation (when (yes-or-no-p "Malformed? ")
         ;;                 "-gs /usr/bin/gs -gs-malformed "))
         ;; (command (concat "cpdf " (unless (sync0-null-p malformation) malformation) "-utf8 -add-bookmarks " toc-file " " file " -o " output)
         (command (concat "cpdf -utf8 -add-bookmarks " toc-file " " file " -o " output)))
    (if (and (file-exists-p toc-file)
             (file-exists-p file))
        (progn
          (shell-command command)
          (rename-file output file t))
      (message "Conditions not satisfied by entry %s to attach corresponding toc." bibkey))))

(major-mode-hydra-define bibtex-mode nil 
  ("Entries"
   (("c" sync0-bibtex-clean-entry "Clean entry")
    ("E" sync0-bibtex-define-entry "Capture entry")
    ("e" (sync0-bibtex-define-entry t) "Quick capture")
    ("u" sync0-bibtex-update-key "Update next key")
    ("N" sync0-bibtex-create-note-from-entry "Create md from entry")
    ("R" (sync0-bibtex-create-note-from-entry t) "Rewrite md from entry")
    ("a" sync0-bibtex-add-field "Add field")
    ("k" bu-make-field-keywords "Add keyword"))
   "PDF editing"
   (("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
    ("p" sync0-bibtex-print-pdf "Print att. from entry")
    ("C" sync0-bibtex-crop-pdf "Crop attached pdf")
    ("T" sync0-bibtex-add-toc-to-pdf "Add TOC to pdf")
    ("d" sync0-bibtex-download-pdf "Download pdf from url"))
    ;; ("t" sync0-bibtex-extract-subpdf "Extract subpdf")
   "Visit"
   (("v" ivy-bibtex "Visit entry")
    ("o" sync0-bibtex-open-pdf-at-point "Open in pdfview")
    ;; ("o" sync0-org-ref-open-pdf-at-point "Open in pdfview")
    ("z" (sync0-bibtex-open-pdf-at-point t) "Open in zathura")
    ("w" sync0-bibtex-open-url "Open url")
   ("n" sync0-bibtex-open-notes "Open annotations"))
   "Bibliographies"
   (("A" sync0-bibtex-archive-entry "Archive entry")
    ;; ("r" (sync0-bibtex-update-completion-files sync0-bibtex-completion-variables-list) "Refresh completion vars")
    ("V" sync0-visit-bibliography-in-buffer "Visit bibfile"))))

(provide 'sync0-bibtex-functions)
