;; -*- lexical-binding: t -*-

(require 'sync0-print)
(require 'sync0-pdf)

(defvar sync0-bibtex-archived-bibliography "/home/sync0/Dropbox/bibliographies/archived.bib"
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

(defvar sync0-bibtex-entry-shorthand nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-related nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-relatedtype nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

;; (defvar sync0-bibtex-entry-creation-entry nil
;;   "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-crossref-entry nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-use-extraction-page-numbers nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-extract nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-title nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-eventtitle nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-edition nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-subtitle nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-title-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-title-compatible nil
  "Dummy variable used for title of files to be copied to a usb or other media.")

(defvar sync0-bibtex-entry-eventdate nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-date nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-date-tag nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-date-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-origdate nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-author nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

;; (defvar sync0-bibtex-entry-author-tag nil
;;   "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-author-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-lastname nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-author-tag nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-journaltitle nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-booktitle nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-booksubtitle nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-crossref nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-chapter nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-volume nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-number nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-series nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-publisher nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-location nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-pages nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-note nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-doi nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-url nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-urldate nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-language nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-langid nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-medium nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-library nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-institution nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-file nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-keywords nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-type nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

;; (defvar sync0-bibtex-entry-type-downcase nil
;;   "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-key nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-creation-date nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-command nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")


(defvar sync0-bibtex-entry-initial-date nil)
(defvar sync0-bibtex-entry-initial-origdate nil)
(defvar sync0-bibtex-entry-initial-author nil)
(defvar sync0-bibtex-entry-initial-language nil)

(defvar sync0-bibtex-entry-definitions-list
  '(sync0-bibtex-entry-title
    sync0-bibtex-entry-subtitle
    sync0-bibtex-entry-eventtitle
    sync0-bibtex-entry-date
    sync0-bibtex-entry-origdate
    sync0-bibtex-entry-eventdate
    sync0-bibtex-entry-author
    sync0-bibtex-entry-journaltitle
    sync0-bibtex-entry-edition
    sync0-bibtex-entry-booktitle
    sync0-bibtex-entry-booksubtitle
    sync0-bibtex-entry-crossref
    sync0-bibtex-entry-chapter
    sync0-bibtex-entry-volume
    sync0-bibtex-entry-number
    sync0-bibtex-entry-series
    sync0-bibtex-entry-publisher
    sync0-bibtex-entry-location
    sync0-bibtex-entry-pages
    sync0-bibtex-entry-note
    sync0-bibtex-entry-doi
    sync0-bibtex-entry-url
    sync0-bibtex-entry-creation-date
    sync0-bibtex-entry-language
    sync0-bibtex-entry-language
    sync0-bibtex-entry-medium
    sync0-bibtex-entry-institution
    sync0-bibtex-entry-library
    sync0-bibtex-entry-related
    sync0-bibtex-entry-relatedtype
    sync0-bibtex-entry-file
    sync0-bibtex-entry-shorthand
    sync0-bibtex-entry-keywords))

(defvar sync0-bibtex-entry-initial-fields-list 
  '(sync0-bibtex-entry-initial-date 
    sync0-bibtex-entry-initial-origdate
    sync0-bibtex-entry-initial-author
    sync0-bibtex-entry-initial-language))

(defvar sync0-bibtex-entry-extraction-fields-list 
  '(sync0-bibtex-entry-extract
    sync0-bibtex-entry-crossref-entry
    ;; sync0-bibtex-entry-creation-entry
    sync0-bibtex-entry-use-extraction-page-numbers))

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
                      ;; (when sync0-bibtex-entry-derivation
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
                              (bibtex-completion-get-value "language" sync0-bibtex-entry-crossref-entry))))
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


;; (defvar sync0-bibtex-entry-functions
;;   '(("Article" (lambda ()
;;                  (setq sync0-bibtex-entry-journaltitle
;;                        (completing-read "Titre du journal : " sync0-bibtex-completion-journaltitle))
;;                  (setq sync0-bibtex-entry-volume
;;                        (read-string "Tome : "))
;;                  (setq sync0-bibtex-entry-number
;;                        (read-string "Numero : "))
;;                  (setq sync0-bibtex-entry-pages
;;                        (read-string "Pages (ex. : 90-180) : "))
;;                  (setq sync0-bibtex-entry-parent
;;                        sync0-bibtex-entry-journaltitle)
;;                  (setq sync0-bibtex-entry-doi
;;                        (read-string "DOI : " nil nil nil t))
;;                  (setq sync0-bibtex-entry-url
;;                        (when (sync0-null-p sync0-bibtex-entry-doi) 
;;                          (read-string "Url : " nil nil nil t)))))
;;     ("Book" (lambda ()
;;               (setq sync0-bibtex-entry-volume
;;                     (read-string "Tome : "))
;;               (sync0-bibtex-entry-define-publisher)
;;               (sync0-bibtex-entry-define-location)
;;               (setq sync0-bibtex-entry-series
;;                     (read-string "Series : "))
;;               (setq sync0-bibtex-entry-parent
;;                     sync0-bibtex-entry-series)
;;               (sync0-bibtex-entry-define-edition)
;;               (sync0-bibtex-entry-define-medium)
;;               (sync0-bibtex-entry-define-library)
;;               (setq sync0-bibtex-entry-url
;;                     (read-string "Url : " nil nil nil t))))
;;     ("Collection" (lambda ()
;;                     (setq sync0-bibtex-entry-volume
;;                           (read-string "Tome : "))
;;                     (sync0-bibtex-entry-define-publisher)
;;                     (sync0-bibtex-entry-define-edition)
;;                     (sync0-bibtex-entry-define-location)
;;                     (setq sync0-bibtex-entry-series
;;                           (read-string "Series : "))
;;                     (setq sync0-bibtex-entry-parent
;;                           sync0-bibtex-entry-series)
;;                     (sync0-bibtex-entry-define-medium)
;;                     (sync0-bibtex-entry-define-library)
;;                     (setq sync0-bibtex-entry-url
;;                           (read-string "Url : " nil nil nil t))))
;;     ("InBook" (lambda ()
;;                 (setq sync0-bibtex-entry-extract
;;                       (yes-or-no-p "Extract  PDF from existing entry?"))
;;                 (setq sync0-bibtex-entry-use-extraction-page-numbers
;;                       (when sync0-bibtex-entry-extract
;;                         (yes-or-no-p "Use page numbers from extraction?")))
;;                 (setq sync0-bibtex-entry-pages
;;                       (read-string "Pages (ex. : 90-180) : "))
;;                 (sync0-bibtex-entry-define-booktitle)
;;                 (sync0-bibtex-entry-define-edition)
;;                 (setq sync0-bibtex-entry-chapter
;;                       (read-string "Chapter : "))
;;                 (setq sync0-bibtex-entry-parent
;;                       (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
;;                           sync0-bibtex-entry-booktitle
;;                         (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
;;                 (unless sync0-bibtex-entry-crossref 
;;                   (progn
;;                     (setq sync0-bibtex-entry-series
;;                           (read-string "Series : "))
;;                     (sync0-bibtex-entry-define-medium)
;;                     (sync0-bibtex-entry-define-library)
;;                     (sync0-bibtex-entry-define-publisher)
;;                     (sync0-bibtex-entry-define-location)))))
;;     ("InCollection" (lambda ()
;;                       (setq sync0-bibtex-entry-extract
;;                             (yes-or-no-p "Extract  PDF from existing entry?"))
;;                       (setq sync0-bibtex-entry-use-extraction-page-numbers
;;                             (when sync0-bibtex-entry-extract
;;                               (yes-or-no-p "Use page numbers from extraction?")))
;;                       (setq sync0-bibtex-entry-pages
;;                             (read-string "Pages (ex. : 90-180) : "))
;;                       (sync0-bibtex-entry-define-booktitle)
;;                       (setq sync0-bibtex-entry-chapter
;;                             (read-string "Chapter : "))
;;                       (setq sync0-bibtex-entry-parent
;;                             (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
;;                                 sync0-bibtex-entry-booktitle
;;                               (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
;;                       (unless sync0-bibtex-entry-crossref 
;;                         (progn
;;                           (setq sync0-bibtex-entry-series
;;                                 (read-string "Series : "))
;;                           (sync0-bibtex-entry-define-medium)
;;                           (sync0-bibtex-entry-define-library)
;;                           (sync0-bibtex-entry-define-publisher)
;;                           (sync0-bibtex-entry-define-location)))))
;;     ("InProceedings" (lambda ()
;;                        (setq sync0-bibtex-entry-extract
;;                              (yes-or-no-p "Extract  PDF from existing entry?"))
;;                        (setq sync0-bibtex-entry-use-extraction-page-numbers
;;                              (when sync0-bibtex-entry-extract
;;                                (yes-or-no-p "Use page numbers from extraction?")))
;;                        (setq sync0-bibtex-entry-pages
;;                              (read-string "Pages (ex. : 90-180) : "))
;;                        (sync0-bibtex-entry-define-booktitle)
;;                        (setq sync0-bibtex-entry-chapter
;;                              (read-string "Chapter : "))
;;                        (setq sync0-bibtex-entry-parent
;;                              (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
;;                                  sync0-bibtex-entry-booktitle
;;                                (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
;;                        (unless sync0-bibtex-entry-crossref 
;;                          (progn
;;                            (setq sync0-bibtex-entry-eventtitle
;;                                  (read-string "Event title : "))
;;                            (setq sync0-bibtex-entry-eventdate
;;                                  (read-string "Event date (ex. 1990-12-28) : "))
;;                            (setq sync0-bibtex-entry-series
;;                                  (read-string "Series : "))
;;                            (sync0-bibtex-entry-define-medium)
;;                            (sync0-bibtex-entry-define-library)
;;                            (sync0-bibtex-entry-define-publisher)
;;                            (sync0-bibtex-entry-define-location)))))
;;     ("Manual" (lambda ()
;;                 (setq sync0-bibtex-entry-institution
;;                       (read-string "Institution : "))
;;                 (setq sync0-bibtex-entry-number
;;                       (read-string "Numero : "))
;;                 (setq sync0-bibtex-entry-volume
;;                       (read-string "Tome : "))
;;                 (sync0-bibtex-entry-define-publisher)
;;                 (sync0-bibtex-entry-define-location)
;;                 (setq sync0-bibtex-entry-series
;;                       (read-string "Series : "))
;;                 (setq sync0-bibtex-entry-parent
;;                       sync0-bibtex-entry-series)
;;                 (sync0-bibtex-entry-define-edition)
;;                 (sync0-bibtex-entry-define-medium)
;;                 (sync0-bibtex-entry-define-library)
;;                 (setq sync0-bibtex-entry-url
;;                       (read-string "Url : " nil nil nil t))
;;                 (sync0-bibtex-entry-define-note)))
;;     ("Online" (lambda ()
;;                 (setq sync0-bibtex-entry-institution
;;                       (read-string "Institution : "))
;;                 (sync0-bibtex-entry-define-publisher)
;;                 (setq sync0-bibtex-entry-series
;;                       (read-string "Series : "))
;;                 (setq sync0-bibtex-entry-parent
;;                       sync0-bibtex-entry-series)
;;                 (sync0-bibtex-entry-define-medium)
;;                 (sync0-bibtex-entry-define-library)
;;                 (setq sync0-bibtex-entry-url
;;                       (read-string "Url : " nil nil nil t))
;;                 (sync0-bibtex-entry-define-note)))
;;     ("Report" (lambda ()
;;                 (setq sync0-bibtex-entry-institution
;;                       (read-string "Institution : "))
;;                 (setq sync0-bibtex-entry-number
;;                       (read-string "Numero : "))
;;                 (setq sync0-bibtex-entry-volume
;;                       (read-string "Tome : "))
;;                 (setq sync0-bibtex-entry-series
;;                       (read-string "Series : "))
;;                 (setq sync0-bibtex-entry-parent
;;                       sync0-bibtex-entry-series)
;;                 (sync0-bibtex-entry-define-publisher)
;;                 (sync0-bibtex-entry-define-location)
;;                 (sync0-bibtex-entry-define-medium)
;;                 (sync0-bibtex-entry-define-library)
;;                 (setq sync0-bibtex-entry-url
;;                       (read-string "Url : " nil nil nil t))
;;                 (sync0-bibtex-entry-define-note)))
;;     ("Unpublished" (lambda ()
;;                      (setq sync0-bibtex-entry-number
;;                            (read-string "Numero : "))
;;                      (setq sync0-bibtex-entry-institution
;;                            (read-string "Institution : "))
;;                      (setq sync0-bibtex-entry-series
;;                            (read-string "Series : "))
;;                      (setq sync0-bibtex-entry-parent
;;                            sync0-bibtex-entry-series)
;;                      (sync0-bibtex-entry-define-publisher)
;;                      (sync0-bibtex-entry-define-location)
;;                      (sync0-bibtex-entry-define-medium)
;;                      (sync0-bibtex-entry-define-library)
;;                      (setq sync0-bibtex-entry-url
;;                            (read-string "Url : " nil nil nil t))
;;                 (sync0-bibtex-entry-define-note)))
;;     ("Misc" (lambda ()
;;                      ;; (setq sync0-bibtex-entry-number
;;                      ;;       (read-string "Numero : "))
;;                      (setq sync0-bibtex-entry-institution
;;                            (read-string "Institution : "))
;;                      ;; (setq sync0-bibtex-entry-series
;;                      ;;       (read-string "Series : "))
;;                      (setq sync0-bibtex-entry-parent
;;                            (read-string "Parent ? "))
;;                      ;;       sync0-bibtex-entry-series)
;;                      ;; (sync0-bibtex-entry-define-publisher)
;;                      ;; (sync0-bibtex-entry-define-location)
;;                      (sync0-bibtex-entry-define-medium)
;;                      (sync0-bibtex-entry-define-library)
;;                      (setq sync0-bibtex-entry-url
;;                            (read-string "Url : " nil nil nil t))
;;                 (sync0-bibtex-entry-define-note)))
;;     ("Thesis" (lambda ()
;;                 (setq sync0-bibtex-entry-number
;;                       (read-string "Numero : "))
;;                 (setq sync0-bibtex-entry-institution
;;                       (read-string "Institution : "))
;;                 (sync0-bibtex-entry-define-publisher)
;;                 (sync0-bibtex-entry-define-location)
;;                 (sync0-bibtex-entry-define-medium)
;;                 (sync0-bibtex-entry-define-library)
;;                 (setq sync0-bibtex-entry-url
;;                       (read-string "Url : " nil nil nil t))
;;                 (sync0-bibtex-entry-define-note)))
;;     ("Proceedings" (lambda ()
;;                      (setq sync0-bibtex-entry-volume
;;                            (read-string "Tome : "))
;;                      (sync0-bibtex-entry-define-publisher)
;;                      (sync0-bibtex-entry-define-location)
;;                      (setq sync0-bibtex-entry-series
;;                            (read-string "Series : "))
;;                      (setq sync0-bibtex-entry-parent
;;                            sync0-bibtex-entry-series)
;;                      (sync0-bibtex-entry-define-medium)
;;                      (sync0-bibtex-entry-define-library)
;;                      (setq sync0-bibtex-entry-eventtitle
;;                            (read-string "Event title : "))
;;                      (setq sync0-bibtex-entry-eventdate
;;                            (read-string "Event date (ex. 1990-12-28) : "))
;;                      (setq sync0-bibtex-entry-url
;;                            (read-string "Url : " nil nil nil t))))))

    (defun sync0-bibtex-completion-load-entry (&optional bibkey quick)
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
                       (if quick
                           sync0-bibtex-base-fields
                         (append (cdr (assoc type sync0-bibtex-type-fields)) sync0-bibtex-base-fields)))))
        (setq sync0-bibtex-entry-key
              (or bibkey 
                  (format-time-string "%Y%m%d%H%M%S")))
        (setq sync0-bibtex-entry-creation-date
              (format-time-string "%Y-%m-%d")) 
        (setq sync0-bibtex-entry-type type)
        (dolist (element fields)
          (if entry
              (when-let ((value (bibtex-completion-get-value element entry)))
                (set (intern (concat "sync0-bibtex-entry-" element)) value))
            (funcall (cadr (assoc element sync0-bibtex-entry-functions))))))
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
                        (unless (sync0-null-p sync0-bibtex-entry-volume) (concat ", T." sync0-bibtex-entry-volume)))
              (concat sync0-bibtex-entry-title
                      (unless (sync0-null-p sync0-bibtex-entry-volume) (concat ", T." sync0-bibtex-entry-volume)) " : " sync0-bibtex-entry-subtitle)))
      (setq sync0-bibtex-entry-title-compatible
            (replace-regexp-in-string "[[:blank:]]*:[[:blank:]]*" "_" sync0-bibtex-entry-title-fixed))
      ;; parent
      (setq sync0-bibtex-entry-parent
            (cond ((string= sync0-bibtex-entry-type "Article")
                   sync0-bibtex-entry-journaltitle)
                  ((or (string= sync0-bibtex-entry-type "InBook")
                       (string= sync0-bibtex-entry-type "InCollection")
                       (string= sync0-bibtex-entry-type "InProceedings"))
                   (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
                       sync0-bibtex-entry-booktitle
                     (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
                  (t sync0-bibtex-entry-series)))) 

(defun sync0-bibtex-entry-extract-pdf (filename &optional beg end)
  "Extract pdf from crossref defined by
   sync0-bibtex-entry-crossref. Before activating,
   sync0-bibtex-entry-extract must be set to t. Filename is not
   automatically calculated because this function is not
   standalone; it is intended to be part of pipes. Therefore,
   filename is the only mandatory argument."
  ;; (when (and  sync0-bibtex-entry-crossref
  ;;             sync0-bibtex-entry-extract)
    (if (and beg end)
        (let* ((origfile (car (bibtex-completion-find-pdf sync0-bibtex-entry-crossref)))
               (command (unless (null origfile)
                          (concat
                           "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="
                           beg
                           " -dLastPage="
                           end
                           " -sOutputFile="
                           sync0-pdfs-folder filename ".pdf " origfile))))
          (shell-command command))
      (let* ((beg (read-string "First page for extraction: "))
             (end (read-string "Last page for extraction: "))
             (origfile (car (bibtex-completion-find-pdf sync0-bibtex-entry-crossref)))
             (command (unless (null origfile)
                        (concat
                         "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="
                         beg
                         " -dLastPage="
                         end
                         " -sOutputFile="
                         sync0-pdfs-folder filename ".pdf " origfile))))
        (shell-command command))))

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

    (defun sync0-bibtex-define-entry (&optional quick)
      "Create new BibLaTeX entry in the default bibliography. When
   optional quick is non-nil, only capture the minimal fields
   required to create a new entry."
      (interactive)
      ;; Before calculating any values, reset all values in my
      ;; master list of variable (set them to nil).
      (setq sync0-bibtex-entry-derivation (yes-or-no-p "Derive entry?"))
      (sync0-bibtex-completion-load-entry nil quick)
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

(defvar sync0-bibtex-fields-functions
  '(("file" (lambda ()
              (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
              (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
              (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			                    (bibtex-parse-entry)))
                     (bibkey (cdr (assoc "=key=" entry)))
                     (new-path (concat sync0-pdfs-folder bibkey ".pdf"))
                     (my-list (list "file" "Attachment path" new-path nil)))
                (bibtex-beginning-of-entry)
                (if  (assoc "file" entry)
                    (message "Field file already presente for %s entry" entry)
                    (bibtex-make-field my-list t)))))
    ("keywords" (lambda ()
                  (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
                  (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
                  (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
                  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			                        (bibtex-parse-entry)))
                         (bibkey (cdr (assoc "=key=" entry)))
                         (type  (cdr (assoc "=type=" entry)))
                         (crossref (when-let (x (cdr (assoc "crossref" entry)))
                                     (string-trim x "{" "}"))))
                    (setq sync0-bibtex-entry-type type)
                    (setq sync0-bibtex-entry-crossref crossref)
                    (setq sync0-bibtex-entry-volume
                          (when-let (x (cdr (assoc "volume" entry)))
                                     (string-trim x "{" "}")))
                    (setq sync0-bibtex-entry-edition
                          (when-let (x (cdr (assoc "edition" entry)))
                                     (string-trim x "{" "}")))
                    (sync0-bibtex-entry-define-author bibkey)
                    (sync0-bibtex-entry-define-title bibkey)
                    (sync0-bibtex-entry-define-date bibkey)
                    (sync0-bibtex-entry-define-language bibkey)
                    (sync0-bibtex-entry-define-related bibkey)
                    (sync0-bibtex-entry-define-keywords)
                    (bibtex-beginning-of-entry)
                    (bibtex-make-field (list "keywords" "Entry keywords" sync0-bibtex-entry-keywords nil) t))))
    ("related" (lambda ()
                (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
                (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
                (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
                (bibtex-beginning-of-entry)
                (sync0-bibtex-entry-define-related)
                (bibtex-make-field (list "related" "Entry related" sync0-bibtex-entry-related nil) t)
                (bibtex-make-field (list "relatedtype" "Entry related" sync0-bibtex-entry-relatedtype nil) t)))
    ("author" (lambda ()
                (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
                (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
                (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
                (bibtex-beginning-of-entry)
                (sync0-bibtex-entry-define-author)
                (bibtex-make-field (list "author" "Entry author" sync0-bibtex-entry-author nil) t)))))

(defun sync0-bibtex-entry-define-note (&optional bibkey)
  "Define the note for new BibLaTeX entry."
  (setq sync0-bibtex-entry-note
        (if bibkey
            (let ((entry (bibtex-completion-get-entry bibkey)))
              (bibtex-completion-get-value "note" entry))
          (completing-read "Quelle note ou addenda ? : "
                           sync0-bibtex-completion-note))))

(defun sync0-bibtex-entry-define-language (&optional bibkey)
  "Define the language for new BibLaTeX entry."
  (setq sync0-bibtex-entry-language
        (if bibkey
            (let ((entry (bibtex-completion-get-entry bibkey)))
              (bibtex-completion-get-value "language" entry))
          (completing-read "Choose language : "
                           sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))))

(defun sync0-bibtex-entry-define-keywords (&optional no-extra bibkey)
  "Define the keywords for new BibLaTeX entry."
  (setq sync0-bibtex-entry-keywords
        (if bibkey
            (let ((entry (bibtex-completion-get-entry bibkey)))
              (bibtex-completion-get-value "keywords" entry))
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
                  (unless no-extra
                    (when-let* ((extra-keywords (completing-read-multiple "Input extra keywords: " 
                                    sync0-bibtex-completion-keywords))
                                (keywords-string
                                 (unless (sync0-null-p extra-keywords)
                                 (sync0-show-elements-of-list extra-keywords ", ")))) 
                   ;; (when-let ((extra-keywords (read-string "Input extra keywords: ")))
                        (concat ", " keywords-string)))))))

(defun sync0-bibtex-entry-define-booktitle (&optional bibkey)
  "Define the booktitle for new BibLaTeX entry."
  (if bibkey 
      (let* ((entry (bibtex-completion-get-entry bibkey))
             (booktitle (bibtex-completion-get-value "booktitle" entry))
             (booksubtitle (bibtex-completion-get-value "booktitle" entry)))
        (setq sync0-bibtex-entry-booktitle booktitle)
        (setq sync0-bibtex-entry-subbooktitle booksubtitle))
    (progn 
      (setq sync0-bibtex-entry-booktitle
            (if sync0-bibtex-entry-crossref 
                (bibtex-completion-get-value "title" sync0-bibtex-entry-crossref-entry)
              (read-string "Book title : ")))
      (setq sync0-bibtex-entry-booksubtitle
            (if sync0-bibtex-entry-crossref 
                (bibtex-completion-get-value "subtitle" sync0-bibtex-entry-crossref-entry)
              (read-string "Book subtitle : "))))))

(defun sync0-bibtex-entry-define-library ()
  "Define the library for new BibLaTeX entry."
  (setq sync0-bibtex-entry-library
        (completing-read "Choose location to trace back: "
                         sync0-bibtex-completion-library nil nil nil)))

(defun sync0-bibtex-entry-define-location ()
  "Define the media for new BibLaTeX entry."
  (setq sync0-bibtex-entry-location
        (completing-read "Location : " sync0-bibtex-completion-location)))

(defun sync0-bibtex-entry-define-publisher ()
  "Define the publisher for new BibLaTeX entry."
  (setq sync0-bibtex-entry-publisher
        (completing-read "Maison d'edition : " sync0-bibtex-completion-publisher)))

(defun sync0-bibtex-entry-define-medium ()
  "Define the media for new BibLaTeX entry."
  (setq sync0-bibtex-entry-medium
        (string-trim
         (prin1-to-string
          (completing-read-multiple "Quel support ?"
                                    sync0-bibtex-completion-medium)) "(\"" "\")")))

(defun sync0-bibtex-entry-define-author (&optional bibkey)
  "Define the author for new BibLaTeX entry."
  (setq sync0-bibtex-entry-author
        (if bibkey 
            (let ((entry (bibtex-completion-get-entry bibkey)))
              (if (or (string= (downcase  sync0-bibtex-entry-type) "collection")
                      (string= (downcase sync0-bibtex-entry-type) "proceedings"))
                  (bibtex-completion-get-value "editor" entry)
                (bibtex-completion-get-value "author" entry)))
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
            result)))
  ;; (completing-read "Auteur : " sync0-bibtex-completion-author
  ;;                  nil nil sync0-bibtex-entry-initial-author)))
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
                     (downcase (replace-regexp-in-string " " "-" raw))))))))

(defun sync0-bibtex-entry-define-title (&optional bibkey)
  "Define the title for new BibLaTeX entry."
  (if bibkey
      (let ((entry (bibtex-completion-get-entry bibkey)))
        (setq sync0-bibtex-entry-title
              (bibtex-completion-get-value "title" entry))
        (setq sync0-bibtex-entry-subtitle
              (if-let* ((crossref (bibtex-completion-get-value "crossref" entry))
                        (crossref-entry (bibtex-completion-get-entry crossref)))
                  (when (string= 
                         (bibtex-completion-get-value "subtitle" entry)
                         (bibtex-completion-get-value "subtitle" crossref-entry))
                    nil)
                (bibtex-completion-get-value "subtitle" entry))))
    (progn (setq sync0-bibtex-entry-title
                 (completing-read "Input title of new BibLaTeX entry: " sync0-bibtex-completion-title))
           (setq sync0-bibtex-entry-subtitle
                 (read-string "Sous-titre du texte : " nil nil nil t))))
  (when-let* ((entry (bibtex-completion-get-entry bibkey))
              (crossref (bibtex-completion-get-value "crossref" entry))
              (crossref-entry (bibtex-completion-get-entry crossref)))
                  (when (string= 
                         (bibtex-completion-get-value "volume" entry)
                         (bibtex-completion-get-value "volume" crossref-entry))
        (setq sync0-bibtex-entry-volume nil)))
  (setq sync0-bibtex-entry-title-fixed
        (if (sync0-null-p sync0-bibtex-entry-subtitle)
            (concat sync0-bibtex-entry-title (unless (sync0-null-p sync0-bibtex-entry-volume) (concat ", T." sync0-bibtex-entry-volume)))
          (concat sync0-bibtex-entry-title (unless (sync0-null-p sync0-bibtex-entry-volume) (concat ", T." sync0-bibtex-entry-volume)) " : " sync0-bibtex-entry-subtitle)))
  (setq sync0-bibtex-entry-title-compatible
        (replace-regexp-in-string "[[:blank:]]*:[[:blank:]]*" "_" sync0-bibtex-entry-title-fixed)))

(defun sync0-bibtex-entry-define-related (&optional bibkey)
  "Define the author for new BibLaTeX entry."
  (if bibkey 
      (let ((entry (bibtex-completion-get-entry bibkey)))
        (setq sync0-bibtex-entry-related
              (bibtex-completion-get-value "related" entry))
        (setq sync0-bibtex-entry-relatedtype
              (bibtex-completion-get-value "relatedtype" entry)))
    (progn
        (setq sync0-bibtex-entry-related (sync0-bibtex-completion-choose-key nil t))
        (setq sync0-bibtex-entry-relatedtype
              (completing-read "Type de relation : " '("multivolume" "origpubas" "reviewof" "reprintof" "reprintas" "reprintfrom" "translationas" "translationfrom" "translationof"))))))

(defun sync0-bibtex-entry-define-edition (&optional bibkey)
  "Define the author for new BibLaTeX entry."
  (setq sync0-bibtex-entry-edition
        (if bibkey 
            (let ((entry (bibtex-completion-get-entry bibkey)))
              (bibtex-completion-get-value "edition" entry))
          (read-string "Édition : "))))

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

      ;; ("booktitle" sync0-bibtex-entry-booktitle sync0-bibtex-completion-booktitle "~/.emacs.d/sync0-vars/bibtex-completion-booktitle.txt")
      ;; ("medium" sync0-bibtex-entry-medium sync0-bibtex-completion-medium "~/.emacs.d/sync0-vars/bibtex-completion-medium.txt")

(defun sync0-bibtex-update-vars (seqlists)
  "Update variables used for completion based on the information
   provided by the new entry."
  (dolist (element seqlists)
    ;; Check whether the item to be added is empty.
    (unless (sync0-null-p  (cadr element))
    ;; Check whether the item to be added is already present.
      (unless (member (eval (cadr element))  (eval (caddr element)))
    ;; Send the element to the list.
        (push (cadr element) (caddr element))
    ;; Send the element to the file.
        (append-to-file (concat (eval (cadr element)) "\n") nil (cadddr element))))))

(defun sync0-bibtex-update-completion-files (seqlists)
  (interactive)
  (dolist (element seqlists)
    (let ((current-elements)
          (bib-elements (delete-duplicates 
                         (cond ((string= (car element) "author")
                                (let ((my-list (bibtex-completion-candidates))
                                      (aggregate))
                                  (dolist (element my-list aggregate)
                                    (when-let ((author  (cdr (assoc  "author" element))))
                                      (cond ((string-match " and " author)
                                             ;; create a list with parts 
                                             (append (split-string author " and ") aggregate))
                                            ;; check when author is an organization
                                            ((string-match "^{" author)
                                             (push  (substring author 1 -1) aggregate))
                                            ;; other cases
                                            (t (push author aggregate)))))))
                               ((string= (car element) "keywords")
                                (let ((my-list (bibtex-completion-candidates))
                                      (aggregate))
                                  (dolist (element my-list aggregate)
                                    (when-let ((keywords  (cdr (assoc  "keywords" element))))
                                      (if (string-match ", " keywords)
                                          ;; create a list with parts 
                                          (append (split-string keywords ", ") aggregate)
                                        ;; other cases
                                           (push keywords aggregate))))))
                               (t (mapcar #'(lambda (x) (cdr (assoc (car element) x)))
                                          (bibtex-completion-candidates)))))))
      (with-temp-buffer
        (insert-file-contents (cadddr element))
        (goto-char (point-min))
        ;; (keep-lines "contexts" (point-min) (point-max)) 
        (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
          (push  (match-string 1) current-elements)))
      (with-temp-file (cadddr element)
        (if (string= (car element) "keywords")
            (set (caddr element) (delete-dups (append current-elements bib-elements bu-keywords-values)))
          (set (caddr element) (delete-dups (append current-elements bib-elements))))
        (sync0-insert-elements-of-list (eval (caddr element)))))))

(defun sync0-bibtex-entry-append-to-bibliography (bibkey)
  "Append new BibLaTeX entry to default bibliography file.
   Beware, this function only constructs and appends the entry
   to the bib file; the values of the entry must have been defined
   elsewhere. For the sake of speed, this function does not
   perform any sanity checks on duplicate entries, and thus a
   unique correct entry is assumed to be supplied as mandatory
   argument bibkey."
  (let* ((definitions (mapcar #'eval sync0-bibtex-entry-definitions-list))
         (bibtex-fields (if (or (string= sync0-bibtex-entry-type "Collection")
                                (string= sync0-bibtex-entry-type "Proceedings"))
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

(defun sync0-bibtex-entry-define-date (&optional bibkey)
  "Define the dates for new BibLaTeX entry."
  (setq sync0-bibtex-entry-creation-date
        (format-time-string "%Y-%m-%d")) 
  (if bibkey 
      (let ((entry (bibtex-completion-get-entry bibkey)))
        (setq sync0-bibtex-entry-date
              (bibtex-completion-get-value "date" entry))
        (setq sync0-bibtex-entry-origdate
              (bibtex-completion-get-value "origdate" entry)))
    (setq sync0-bibtex-entry-date
          (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-date))
    (setq sync0-bibtex-entry-origdate
          (read-string "Origdate (ex. 1890-18-12) : " sync0-bibtex-entry-initial-origdate)))
  (setq sync0-bibtex-entry-date-tag
        (unless (sync0-null-p sync0-bibtex-entry-date)
        (replace-regexp-in-string "-" "/" sync0-bibtex-entry-date)))
  (setq sync0-bibtex-entry-date-fixed
        (if (or (sync0-null-p sync0-bibtex-entry-origdate)
                (string= sync0-bibtex-entry-date sync0-bibtex-entry-origdate))
            (concat "(" sync0-bibtex-entry-date ")")
          (concat "(" sync0-bibtex-entry-origdate ") (" sync0-bibtex-entry-date ")"))))

(defun sync0-bibtex-entry-inform-new-entry ()
  "Inform the user about a new entry that has been just created."
  (if (sync0-null-p sync0-bibtex-entry-author)
      (message "Entry %s %s has been defined with key %s" sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)
    (message "Entry %s %s %s has been defined with key %s" sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)))

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
                                "created: " sync0-bibtex-entry-creation-date "\n"
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
                                "# " (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed (unless (sync0-null-p sync0-bibtex-entry-edition) (concat " (" sync0-bibtex-entry-edition "e)")) "\n\n"))
         (obsidian-rest (concat  "## Description\n\n" 
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

;; (defun sync0-bibtex-completion-choose-key (&optional pointer)
;;   "Choose key with completion. When optional pointer is t,
;;    preselect the entry at point."
;;   (let* ((entry (when (or pointer
;;                           (string= major-mode "bibtex-mode"))
;;                   (save-excursion (bibtex-beginning-of-entry)
;; 			          (bibtex-parse-entry))))
;;          (preselect (when entry
;;                       (cdr (assoc "=key=" entry))))
;;          (candidates (bibtex-completion-candidates))
;;          (selection (ivy-read "Bibliography candidates: "
;;                               candidates
;;                               :preselect preselect
;;                               :caller 'ivy-bibtex
;;                               :history 'ivy-bibtex-history)))
;;     (cdr (assoc "=key=" (cdr (assoc selection candidates))))))

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

;; (defun sync0-bibtex-completion-load-entry (&optional pointer bibkey)
;;   "Load entry vars based on key on point or with completion. When optional pointer is t,
;;    preselect the entry at point."
;;   (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
;;   (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
;;   (if bibkey
;;       (let ((entry (bibtex-completion-get-entry bibkey)))
;;         (setq sync0-bibtex-entry-type 
;;               (cdr (assoc "=type=" entry)))
;;         ;; (setq sync0-bibtex-entry-type-downcase
;;         ;;       (downcase sync0-bibtex-entry-type))
;;         (setq sync0-bibtex-entry-key bibkey)
;;         (setq sync0-bibtex-entry-file
;;               (car (bibtex-completion-find-pdf  bibkey)))
;;         (sync0-bibtex-entry-define-author bibkey)
;;         (sync0-bibtex-entry-define-title bibkey)
;;         (sync0-bibtex-entry-define-date bibkey))
;;     (let* ((entry (when (or pointer
;;                             (string= major-mode "bibtex-mode"))
;;                     (save-excursion (bibtex-beginning-of-entry)
;; 			            (bibtex-parse-entry))))
;;            (preselect (when (or pointer
;;                                 (string= major-mode "bibtex-mode"))
;;                         (cdr (assoc "=key=" entry))))
;;            (candidates (bibtex-completion-candidates))
;;            (selection (ivy-read "Bibliography candidates: "
;;                                 candidates
;;                                 :preselect preselect
;;                                 :caller 'ivy-bibtex
;;                                 :history 'ivy-bibtex-history))
;;            (bibkey (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
;;       ;; (setq sync0-bibtex-entry-crossref 
;;       ;;       (cdr (assoc "crossref" entry)))
;;       ;; (setq sync0-bibtex-entry-crossref-entry
;;       ;;       (when sync0-bibtex-entry-crossref
;;       ;;         (bibtex-completion-get-entry sync0-bibtex-entry-crossref)))
;;       ;; Nullify initial entry fields since they are unnecessary
;;       ;; (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
;;       ;; General settings
;;       (setq sync0-bibtex-entry-type 
;;             (cdr (assoc "=type=" entry)))
;;       ;; (setq sync0-bibtex-entry-type-downcase
;;       ;;       (downcase sync0-bibtex-entry-type))
;;       (setq sync0-bibtex-entry-key bibkey)
;;       (setq sync0-bibtex-entry-file
;;             (car (bibtex-completion-find-pdf  bibkey)))
;;       (sync0-bibtex-entry-define-author bibkey)
;;       (sync0-bibtex-entry-define-title bibkey)
;;       (sync0-bibtex-entry-define-date bibkey))))

(defun sync0-bibtex-download-pdf (&optional creation)
  (interactive)
  (if creation
      (if (y-or-n-p "Call the buchaneers? ")
          (sync0-pdf-download-from-url sync0-bibtex-entry-url sync0-bibtex-entry-file t)
        (sync0-pdf-download-from-url sync0-bibtex-entry-url sync0-bibtex-entry-file))
    (when-let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
                   (entry (bibtex-completion-get-entry bibkey))
                   (url (bibtex-completion-get-value "url" entry))
                   (pdf (bibtex-completion-get-value "file" entry)))
      (if (y-or-n-p "Call the buchaneers? ")
          (sync0-pdf-download-from-url url pdf t)
        (sync0-pdf-download-from-url url pdf)))))

;; (defun sync0-bibtex-define-entry (&optional quick)
;;   "Create new BibLaTeX entry in the default bibliography. When
;;    optional quick is non-nil, only capture the minimal fields
;;    required to create a new entry."
;;   (interactive)
;;   ;; Before calculating any values, reset all values in my
;;   ;; master list of variable (set them to nil).
;;   (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
;;   (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
;;   (let* ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
;;          (derivation (when (or (string= type "InCollection")
;;                                (string= type "InBook")
;;                                (string= type "InProceedings"))
;;                        (yes-or-no-p "Derive entry?"))))
;;     (setq sync0-bibtex-entry-crossref 
;;           (when derivation
;;             (sync0-bibtex-completion-choose-key t t)))
;;     (setq sync0-bibtex-entry-crossref-entry
;;           (when sync0-bibtex-entry-crossref
;;             (bibtex-completion-get-entry sync0-bibtex-entry-crossref)))
;;     (if derivation
;;         (setq sync0-bibtex-entry-initial-date
;;               (bibtex-completion-get-value "date" sync0-bibtex-entry-crossref-entry)
;;               sync0-bibtex-entry-initial-origdate
;;               (bibtex-completion-get-value "origdate" sync0-bibtex-entry-crossref-entry)
;;               sync0-bibtex-entry-initial-author
;;               (if (or (string= type "Collection")
;;                       (string= type "InCollection")
;;                       (string= type "Proceedings")
;;                       (string= type "InProceedings"))
;;                   (bibtex-completion-get-value "editor" sync0-bibtex-entry-crossref-entry)
;;                 (bibtex-completion-get-value "author" sync0-bibtex-entry-crossref-entry))
;;               sync0-bibtex-entry-initial-language
;;               (bibtex-completion-get-value "language" sync0-bibtex-entry-crossref-entry))
;;     ;; Nullify initial entry fields since they are unnecessary
;;     (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list))
;;     ;; General settings
;;     (setq sync0-bibtex-entry-type type)
;;     ;; (setq sync0-bibtex-entry-type-downcase
;;     ;;       (downcase type))
;;     (setq sync0-bibtex-entry-key
;;           (format-time-string "%Y%m%d%H%M%S"))
;;     (setq sync0-bibtex-entry-file
;;           (unless (string= sync0-bibtex-entry-type "Online")
;;             (concat "/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf")))
;;     (sync0-bibtex-entry-define-author)
;;     (sync0-bibtex-entry-define-title)
;;     (sync0-bibtex-entry-define-date)
;;     (sync0-bibtex-entry-define-language)
;;     ;; Type-specific settings
;;     (unless quick
;;       (funcall (cadr (assoc type sync0-bibtex-entry-functions)))
;;       ;; delete unused entries
;;       (when  sync0-bibtex-entry-extract
;;         (if sync0-bibtex-entry-use-extraction-page-numbers
;;             (let ((beg (progn
;;                          (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
;;                          (match-string 1 sync0-bibtex-entry-pages)))
;;                   (end (progn
;;                          (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
;;                          (match-string 1 sync0-bibtex-entry-pages))))
;;               (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end))
;;           (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key))))
;;     (sync0-bibtex-entry-define-keywords)
;;     ;; Insert entry in default bibliography file
;;     (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
;;     ;; Reset these two values to prevent unwanted extractions
;;     (unless (sync0-null-p sync0-bibtex-entry-url)
;;       (when (yes-or-no-p "Download the attached pdf? ")
;;         (sync0-bibtex-download-pdf t)))))

    ;; (sync0-bibtex-update-vars sync0-bibtex-completion-variables-list)

    ;; (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)

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
         (new-path (concat sync0-pdfs-folder new-key ".pdf"))
         (entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (old-key (cdr (assoc "=key=" entry)))
         (attachment (when-let (x (cdr (assoc "file" entry)))
                       (string-trim x "{" "}")))
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
      (rename-file attachment new-path t))
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
  (let    ((bibkey (if refkey 
                       refkey
                     (sync0-bibtex-completion-choose-key t t))))
    (sync0-bibtex-completion-load-entry bibkey)
    (unless no-extract
      (when (or (string= sync0-bibtex-entry-type "InCollection")
                (string= sync0-bibtex-entry-type "InBook")
                (string= sync0-bibtex-entry-type "InProceedings"))
        (sync0-bibtex-extract-pdf)))
    (if rewrite
        (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key t)
      (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key))))

;; (defun sync0-bibtex-create-note-from-entry (&optional rewrite refkey no-extract)
;;   "Create a new Obsidian markdown note from an existing BibLaTeX
;;    entry in the default bibliography file. When optional rewrite
;;    is t, do not create a new file but simply rewrite an existing
;;    entry with the data of the corresponding bibtex entry in the
;;    default .bib file. When optional no-extract is true, do not
;;    attempt to extract a sub-pdf from its crossref (this feature
;;    is only useful when calling this function in loops to prevent
;;    undesired behavior)."
;;   (interactive)
;;   (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
;;   (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
;;   ;; Nullify initial entry fields since they are unnecessary
;;   (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
;;   (let*    ((bibkey (if refkey 
;;                         refkey
;;              (sync0-bibtex-completion-choose-key t t)))
;;             (entry (bibtex-completion-get-entry bibkey))
;;             (type (bibtex-completion-get-value "=type=" entry)))
;;     ;; (setq sync0-bibtex-entry-creation-entry entry)
;;     (setq sync0-bibtex-entry-crossref 
;;              (bibtex-completion-get-value "crossref" entry))
;;     (setq sync0-bibtex-entry-crossref-entry
;;           (when sync0-bibtex-entry-crossref
;;             (bibtex-completion-get-entry sync0-bibtex-entry-crossref)))
;;     ;; General settings
;;     (setq sync0-bibtex-entry-key bibkey)
;;     (setq sync0-bibtex-entry-type type)
;;     (setq sync0-bibtex-entry-volume
;;             (bibtex-completion-get-value "volume" entry))
;;     (setq sync0-bibtex-entry-edition
;;             (bibtex-completion-get-value "edition" entry))
;;     (sync0-bibtex-entry-define-author sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-define-edition sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-define-title sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-define-date sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-define-language sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-define-related sync0-bibtex-entry-key)
;;     (sync0-bibtex-entry-define-keywords nil sync0-bibtex-entry-key)
;;     ;; Specific settings
;;     (setq sync0-bibtex-entry-parent
;;           (cond ((string= type "article")
;;                  (bibtex-completion-get-value "journaltitle" entry))
;;                 ((or (string= type "inbook")
;;                      (string= type "incollection")
;;                      (string= type "inproceedings"))
;;                  (if bibkey
;;                      (sync0-bibtex-entry-define-booktitle bibkey)
;;                    (sync0-bibtex-entry-define-booktitle))
;;                  (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
;;                      sync0-bibtex-entry-booktitle
;;                    (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
;;                 (t (bibtex-completion-get-value "series" entry))))
;;     (setq sync0-bibtex-entry-pages
;;           (bibtex-completion-get-value "pages" entry))
;;     (unless no-extract 
;;       (when sync0-bibtex-entry-pages
;;         (setq sync0-bibtex-entry-extract
;;               (yes-or-no-p "Extract  PDF from existing entry?")))
;;       (when (and sync0-bibtex-entry-pages
;;                  sync0-bibtex-entry-extract)
;;         (setq sync0-bibtex-entry-use-extraction-page-numbers
;;               (yes-or-no-p "Use page numbers from extraction?"))
;;         (if sync0-bibtex-entry-use-extraction-page-numbers
;;             (let ((beg (progn
;;                          (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
;;                          (match-string 1 sync0-bibtex-entry-pages)))
;;                   (end (progn
;;                          (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
;;                          (match-string 1 sync0-bibtex-entry-pages))))
;;               (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end))
;;           (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key))))
;;     (if rewrite
;;         (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key t)
;;       (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key))))
    ;; Reset these two values to prevent unwanted extractions
    ;; (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)

;; (when (file-exists-p pdf-path)
;;   (rename-file pdf-path new-path)))))

(defun sync0-bibtex-extract-multiple-entries-from-pdf (seqlists)
  "Extract pdfs and create bibtex entries and markdown entries
   for each extranction. This function requires an input 'seqlists'
   that is a list composed of n lists of the form: (name,
   first-page, last-page). Likewise, for this to work, it is
   necessary that the pages of the pdf match the page numbers of the
   actual book. Otherwise, the entries will have wrong data."
  ;; Set all fields to nil 
  (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
  (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
            (entry (bibtex-completion-get-entry bibkey))
            (type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
            (extra-keyword (completing-read "Extra keywords to add : " sync0-bibtex-completion-keywords))
            (input-file (car (bibtex-completion-find-pdf bibkey)))
            (counter 1)
            (obsidian-master-note (concat sync0-obsidian-directory bibkey ".md")))
    (setq sync0-bibtex-entry-crossref bibkey)
    ;; Warning: Under normal circumstances these two should not be the
    ;; same. After all, crossref-entry refers to the entry of the
    ;; crossref of the target entry whereas creation-entry refers to
    ;; an already existing entry which will be used as a basis for
    ;; creating something more. The problem with this function is that
    ;; some of the fields of the crossref entry should not be passed
    ;; unto the new entries, but these fields cannot be derived from
    ;; an existing entry since no entries have yet been created.
    (setq sync0-bibtex-entry-crossref-entry entry)
    ;; (setq sync0-bibtex-entry-creation-entry entry)
    (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
    ;; General settings for all notes to be created
    (setq sync0-bibtex-entry-type type)
    ;; (setq sync0-bibtex-entry-type-downcase
    ;;       (downcase type))
    (sync0-bibtex-entry-define-author)
    (sync0-bibtex-entry-define-date bibkey)
    (sync0-bibtex-entry-define-language bibkey)
    (setq sync0-bibtex-entry-parent
          (bibtex-completion-get-value "title" entry))
    ;; Begin loop; specific settings for each note are to be defined
    ;; inside the loop.
    (dolist (elt seqlists)
      (setq counter (1+ counter))
      (let* ((raw-filename (+ (string-to-number (format-time-string "%Y%m%d%H%M%S")) counter))
             (filename (number-to-string raw-filename))
             (file-pdf (concat "/home/sync0/Documents/pdfs/" filename ".pdf"))
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
        (sync0-bibtex-entry-define-keywords t)
        (unless (sync0-null-p extra-keyword)
          (setq sync0-bibtex-entry-keywords
                (concat sync0-bibtex-entry-keywords ", " extra-keyword)))
        (setq sync0-bibtex-entry-extract t)
        (setq sync0-bibtex-entry-use-extraction-page-numbers t)
        (setq sync0-bibtex-entry-pages pages)
        (setq sync0-bibtex-entry-shorthand
              (concat title ", p. " pages-fixed))
        (setq sync0-bibtex-entry-file
              (unless (string= sync0-bibtex-entry-type "Online")
                (concat "/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf")))
        ;; Beginning of loop actions
        ;; First, extract entry
        (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end)
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
        ;; Third, create an obsidian markdown note for the entry.
        (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
        ;; Fourth, add a markdown link in the obsidian master note
        ;; (ie., the note corresponding to the file used as the source
        ;; for extraction).
        (append-to-file obsidian-reference nil obsidian-master-note)))))
    ;; End of loop
    ;; (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)


(defun sync0-bibtex-add-shorthand ()
  "Create a new Obsidian markdown note from an existing BibLaTeX
   entry in the default bibliography file."
  (interactive)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
            (entry (bibtex-completion-get-entry bibkey))
            (type (bibtex-completion-get-value "=type=" entry))
            (title (bibtex-completion-get-value "title" entry))
            (pages (bibtex-completion-get-value "pages" entry))
            (pages-fixed (replace-regexp-in-string "-" "--" pages))
            (shorthand (concat "  shorthand         = {" title ", p. " pages-fixed "},\n")))
    ;; (search-forward bibkey nil t 1)
    (forward-line)
    (insert shorthand)))

(defun sync0-bibtex-extract-subpdf ()
  "Extract pdf from an existing pdf."
  ;; Set all fields to nil 
  (interactive)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
            (input-file (car (bibtex-completion-find-pdf bibkey)))
            (output-file (concat sync0-pdfs-folder bibkey "a.pdf"))
            (range (read-string "Page range: "))
            (command (concat "pdftk " input-file " cat " range " output " output-file)))
    (if (file-exists-p input-file)
        (shell-command command)
      (message "No PDF found for %s" bibkey))))

(defun sync0-bibtex-open-pdf-at-point (&optional zathura)
  "Open the pdf for bibtex key under point if it exists. If
   optional zathura is t, use zathura to open the pdf."
  (interactive)
  (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
         (file (car (bibtex-completion-find-pdf bibkey))))
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

;; (defun sync0-bibtex-derive-entry ()
;;   "Derive a new entry from an existing entry." 
;;   (interactive)
;;   (sync0-nullify-variable-list sync0-bibtex-entry-definitions-list)
;;   (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)
;;   ;; Nullify initial entry fields since they are unnecessary
;;   (sync0-nullify-variable-list sync0-bibtex-entry-initial-fields-list)
;;   ;; First, choose the bibkey of the derivator
;;   (let* ((derive (yes-or-no-p "Derive information from existing BibTeX entry?"))
;;          (filename (if derive
;;                        (sync0-bibtex-completion-choose-key t t)
;;                      (format-time-string "%Y%m%d%H%M%S")))
;;          (filename-entry (when derive
;;           (bibtex-completion-get-entry filename)))
;;          (bibkey (if derive
;;                    (bibtex-completion-get-value "crossref" filename-entry)
;;           (sync0-bibtex-completion-choose-key t t)))
;;          (entry (bibtex-completion-get-entry bibkey))
;;          (type (if derive
;;                    (bibtex-completion-get-value "=type=" filename-entry)
;;                  (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))))
;;     (if derive
;;         (setq sync0-bibtex-entry-crossref 
;;               (bibtex-completion-get-value "crossref" filename-entry))
;;       (setq sync0-bibtex-entry-crossref bibkey))
;;     (setq sync0-bibtex-entry-crossref-entry
;;           (bibtex-completion-get-entry sync0-bibtex-entry-crossref))
;;     ;; General settings
;;     (setq sync0-bibtex-entry-type type)
;;     ;; (setq sync0-bibtex-entry-type-downcase
;;     ;;       (downcase type))
;;     (setq sync0-bibtex-entry-key filename)
;;     (setq sync0-bibtex-entry-parent
;;           (bibtex-completion-get-value "title" sync0-bibtex-entry-crossref-entry))
;;     (setq sync0-bibtex-entry-file
;;           (unless (string= sync0-bibtex-entry-type "Online")
;;             (concat "/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf")))
;;     (if derive
;;         (progn 
;;           (sync0-bibtex-entry-define-author filename)
;;           (sync0-bibtex-entry-define-date filename)
;;           (sync0-bibtex-entry-define-language filename)
;;           (sync0-bibtex-entry-define-title filename)
;;           (setq sync0-bibtex-entry-pages
;;                 (bibtex-completion-get-value "pages" filename-entry)))
;;       (progn 
;;         (sync0-bibtex-entry-define-author)
;;         (sync0-bibtex-entry-define-date)
;;         (sync0-bibtex-entry-define-language)
;;         (sync0-bibtex-entry-define-title)
;;         (setq sync0-bibtex-entry-pages
;;               (read-string "Pages (ex. : 90-180) : "))))
;;     (sync0-bibtex-entry-define-keywords)
;;     (setq sync0-bibtex-entry-extract t)
;;     (setq sync0-bibtex-entry-use-extraction-page-numbers
;;           (yes-or-no-p "Use page numbers from extraction?"))
;;       (if sync0-bibtex-entry-use-extraction-page-numbers
;;           (let ((beg (progn
;;                        (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
;;                        (match-string 1 sync0-bibtex-entry-pages)))
;;                 (end (progn
;;                        (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
;;                        (match-string 1 sync0-bibtex-entry-pages))))
;;             (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end))
;;         (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key))
;;       (unless derive 
;;         (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key))
;;       (unless derive 
;;       (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key))))
    ;; Reset these two values to prevent unwanted extractions
      ;; (sync0-nullify-variable-list sync0-bibtex-entry-extraction-fields-list)

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

;; (defun sync0-bibtex-crop-pdf (&optional input)
;;   "Define the cropbox for pdf attached to a bibtex entry. This
;; function does not crop the pdf; it is a helper function do define
;; the cropbox. When called interactively, this functions calls
;; ivy-bibtex to search for the pdf attached to a bibtex entry."
;;   (interactive)
;;   (let* ((bibkey (unless input (sync0-bibtex-completion-choose-key)))
;;          (pdf (if bibkey
;;                   (car (bibtex-completion-find-pdf bibkey))
;;                 input))
;;          (cropbox (sync0-pdf-define-cropbox pdf)) 
;;          (output (concat sync0-pdfs-folder "temp.pdf"))
;;          (command (concat "gs -o " output " -sDEVICE=pdfwrite" cropbox " /PAGES pdfmark\" -f " pdf)))
;;     (if (file-exists-p pdf)
;;         (progn 
;;           (shell-command command)
;;           (rename-file output pdf t)
;;           (message "Crop box has been redefined for %s" pdf))
;;       (message "No pdf found for %s" pdf))))

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

(defun sync0-bibtex-add-field ()
  (interactive)
  (let ((field
         (completing-read "Choose Bibtex field: " sync0-bibtex-fields)))
    (funcall (cadr (assoc field sync0-bibtex-fields-functions)))))

(defun sync0-bibtex-copy-pdf-to-path (&optional in-path bibkey)
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (if bibkey 
      (progn 
        (sync0-bibtex-completion-load-entry bibkey)
        (if (file-exists-p sync0-bibtex-entry-file)
            (let* ((target-path
                    (if in-path
                        in-path
                      (read-string "Où envoyer ce pdf ? (finir en /) ")))
                   (command (concat "cp "
                                    sync0-bibtex-entry-file
                                    " \""
                                    target-path
                                    sync0-bibtex-entry-lastname
                                    " "
                                    sync0-bibtex-entry-date-fixed
                                    " "
                                    sync0-bibtex-entry-title-compatible
                                    ".pdf\"")))
              (shell-command command)
              ;; (message "%s" command)
              (message "PDF for %s moved to target location" sync0-bibtex-entry-key))
          (message "No PDF found for %s" sync0-bibtex-entry-key)))
    (progn 
      (sync0-bibtex-completion-load-entry)
      (if (file-exists-p sync0-bibtex-entry-file)
          (let* ((target-path
                  (if in-path
                      in-path
                    (read-string "Où envoyer ce pdf ? (finir en /) ")))
                 (command (concat "cp "
                                  sync0-bibtex-entry-file
                                  " \""
                                  target-path
                                  sync0-bibtex-entry-lastname
                                  " "
                                  sync0-bibtex-entry-date-fixed
                                  " "
                                  sync0-bibtex-entry-title-compatible
                                  ".pdf\"")))
            (shell-command command)
            ;; (message "%s" command)
            (message "PDF for %s moved to target location" sync0-bibtex-entry-key))
        (message "No PDF found for %s" sync0-bibtex-entry-key)))))

(defun sync0-bibtex-archive-entry (&optional bibkey)
  "Choose an entry to send to the archived bibliography. This
function fails when the entry is at the top of the buffer becase
the function 1- fails to compute."
  (interactive)
  (when bibkey
    (re-search-forward (concat "{" bibkey ",") nil t 1))
  (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
         (end (save-excursion (bibtex-end-of-entry))))
    ;; (entry-text (kill-region beginning end))
    (append-to-file beginning end sync0-bibtex-archived-bibliography)
    (delete-region beginning end)))

(major-mode-hydra-define bibtex-mode nil 
  ("Entries"
   (("c" sync0-bibtex-clean-entry "Clean entry")
    ("E" sync0-bibtex-define-entry "Capture entry")
    ("e" (sync0-bibtex-define-entry t) "Quick capture")
    ("u" sync0-bibtex-update-key "Update next key")
    ("N" sync0-bibtex-create-note-from-entry "Create md from entry")
    ("R" (sync0-bibtex-create-note-from-entry t) "Rewrite md from entry")
    ("k" bu-make-field-keywords "Add keyword"))
   "PDFs"
   (("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
    ;; ("x" sync0-bibtex-derive-entry "Extract from entry")
    ("p" sync0-bibtex-print-pdf "Print att. from entry")
    ("C" sync0-bibtex-crop-pdf "Crop attached pdf")
    ("d" sync0-bibtex-download-pdf "Download pdf from url")
    ("t" sync0-bibtex-extract-subpdf "Extract subpdf"))
   "Fields"
   (("a" sync0-bibtex-add-field "Add field")
    ("s" sync0-bibtex-add-shorthand "Create shorthand"))
   "Visit"
   (("v" ivy-bibtex "Visit entry")
    ("o" sync0-org-ref-open-pdf-at-point "Open in pdfview")
    ("z" (sync0-bibtex-open-pdf-at-point t) "Open in zathura")
    ("w" sync0-bibtex-open-url "Open url")
   ("n" sync0-bibtex-open-notes "Open annotations"))
   "Bibliographies"
   (("r" (sync0-bibtex-update-completion-files sync0-bibtex-completion-variables-list) "Refresh completion vars")
    ("A" sync0-bibtex-archive-entry "Archive entry")
    ("V" sync0-visit-bibliography-in-buffer "Visit bibfile"))))

(provide 'sync0-bibtex-functions)
