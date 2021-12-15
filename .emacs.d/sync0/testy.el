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

(defvar sync0-bibtex-entry-subtitle nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-title-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

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

(defvar sync0-bibtex-entry-author-fixed nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-lastname nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-lastname-tag nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-journal nil
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

(defvar sync0-bibtex-entry-addendum nil
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

(defvar sync0-bibtex-entry-attachment nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-keywords nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-type nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-type-downcase nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-key nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-creation-date nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

(defvar sync0-bibtex-entry-command nil
  "Dummy variable used for capturing new BibLaTeX entries with custom functions.")

;; (defun sync0-bibtex-entry-definitions ()
;;   "Create list of all necessary variables to create a bibtex entries"
;;   (list sync0-bibtex-entry-title
;;         sync0-bibtex-entry-subtitle
;;         sync0-bibtex-entry-eventtitle
;;         sync0-bibtex-entry-date
;;         sync0-bibtex-entry-origdate
;;         sync0-bibtex-entry-eventdate
;;         sync0-bibtex-entry-author
;;         sync0-bibtex-entry-journal
;;         sync0-bibtex-entry-booktitle
;;         sync0-bibtex-entry-booksubtitle
;;         sync0-bibtex-entry-crossref
;;         sync0-bibtex-entry-chapter
;;         sync0-bibtex-entry-volume
;;         sync0-bibtex-entry-number
;;         sync0-bibtex-entry-series
;;         sync0-bibtex-entry-publisher
;;         sync0-bibtex-entry-location
;;         sync0-bibtex-entry-pages
;;         sync0-bibtex-entry-addendum
;;         sync0-bibtex-entry-doi
;;         sync0-bibtex-entry-url
;;         sync0-bibtex-entry-creation-date
;;         sync0-bibtex-entry-language
;;         sync0-bibtex-entry-language
;;         sync0-bibtex-entry-medium
;;         sync0-bibtex-entry-institution
;;         sync0-bibtex-entry-library
;;         sync0-bibtex-entry-attachment
;;         sync0-bibtex-entry-keywords))

(defvar sync0-bibtex-entry-definitions
  (list 'sync0-bibtex-entry-title
        'sync0-bibtex-entry-subtitle
        'sync0-bibtex-entry-eventtitle
        'sync0-bibtex-entry-date
        'sync0-bibtex-entry-origdate
        'sync0-bibtex-entry-eventdate
        'sync0-bibtex-entry-author
        'sync0-bibtex-entry-journal
        'sync0-bibtex-entry-booktitle
        'sync0-bibtex-entry-booksubtitle
        'sync0-bibtex-entry-crossref
        'sync0-bibtex-entry-chapter
        'sync0-bibtex-entry-volume
        'sync0-bibtex-entry-number
        'sync0-bibtex-entry-series
        'sync0-bibtex-entry-publisher
        'sync0-bibtex-entry-location
        'sync0-bibtex-entry-pages
        'sync0-bibtex-entry-addendum
        'sync0-bibtex-entry-doi
        'sync0-bibtex-entry-url
        'sync0-bibtex-entry-creation-date
        'sync0-bibtex-entry-language
        'sync0-bibtex-entry-language
        'sync0-bibtex-entry-medium
        'sync0-bibtex-entry-institution
        'sync0-bibtex-entry-library
        'sync0-bibtex-entry-attachment
        'sync0-bibtex-entry-keywords))

;; (defun sync0-bibtex-define-entry-crossref ()
;;   "Define the title for new BibLaTeX entry."
;;   (if-let ((derivation (yes-or-no-p "Derive entry?")))
;;   (setq sync0-bibtex-entry-crossref
;;         (let* ((candidates (bibtex-completion-candidates))
;;                (selection (ivy-read "Crossref : "
;;                                     candidates
;;                                     :caller 'ivy-bibtex
;;                                     :history 'ivy-bibtex-history)))
;;           (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
;;   (setq sync0-bibtex-entry-crossref nil)))

;; (defun sync0-bibtex-define-entry-date ()
;;   "Define the title for new BibLaTeX entry."
;;   ;; (when sync0-bibtex-define-entry-crossref
;;   (let* ((initial-date (sync0-org-ref-get-citation-date sync0-bibtex-entry-crossref))
;;           (initial-origdate (sync0-org-ref-get-citation-origdate sync0-bibtex-entry-crossref)))
;;   (setq sync0-bibtex-entry-date
;; (read-string "Date (ex. 1890-18-12) : " initial-date))
;;   (setq sync0-bibtex-entry-origdate
;; (read-string "Date (ex. 1890-18-12) : " initial-origdate))
;;   ;; (setq sync0-bibtex-entry-origdate
;;   ;;         (sync0-org-ref-get-citation-origdate sync0-bibtex-entry-crossref))
;;   (setq sync0-bibtex-entry-date-tag
;;           (replace-regexp-in-string "-" "/" sync0-bibtex-entry-date))
;;   (setq sync0-bibtex-entry-date-fixed
;;         (if (or (string= sync0-bibtex-entry-origdate "")
;;                 (string= sync0-bibtex-entry-date sync0-bibtex-entry-origdate))
;;             (concat "(" sync0-bibtex-entry-date ")")
;;           (concat "(" sync0-bibtex-entry-origdate ") (" sync0-bibtex-entry-date ")")))))

;; (defun sync0-bibtex-define-entry-type ()
;;     "Define the type for new BibLaTeX entry."
;;   (setq sync0-bibtex-entry-type
;;         (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
;;   (setq sync0-bibtex-entry-type-downcase
;;          (downcase sync0-bibtex-entry-type)))

;; (defun sync0-bibtex-define-entry-key ()
;;   "Define the type for new BibLaTeX entry."
;;   (setq sync0-bibtex-entry-key
;;         (format-time-string "%Y%m%d%H%M%S")))

;; (defun sync0-bibtex-define-entry-creation-date ()
;;   "Define the type for new BibLaTeX entry."
;;   (setq sync0-bibtex-entry-creation-date
;;         (format-time-string "%Y-%m-%d"))) 

;; (defun sync0-bibtex-define-entry-title ()
;;   "Define the title for new BibLaTeX entry."
;;   (setq sync0-bibtex-entry-title
;;         (let* ((candidates (bibtex-completion-candidates))
;;                (selection (ivy-read "Input title : "
;;                                     candidates
;;                                     :caller 'ivy-bibtex
;;                                     :require-match nil
;;                                     :history 'ivy-bibtex-history)))
;;           (if (string-match-p "[0-9]\\{14\\}$" selection)
;;               (cdr (assoc "title" (cdr (assoc selection candidates))))
;;             selection)))
;;   (setq sync0-bibtex-entry-subtitle
;;         (read-string "Sous-titre du texte : " nil nil nil t))
;;   (setq sync0-bibtex-entry-title-fixed
;;         (if (string= sync0-bibtex-entry-subtitle "")
;;             sync0-bibtex-entry-title
;;           (concat sync0-bibtex-entry-title " : " sync0-bibtex-entry-subtitle))))

(defvar sync0-bibtex-entry-functions
  '(("Article" (lambda ()
                 (setq sync0-bibtex-entry-journal
                       (completing-read "Titre du journal : " sync0-bibtex-journals))
                 (setq sync0-bibtex-entry-volume
                       (read-string "Tome : "))
                 (setq sync0-bibtex-entry-number
                       (read-string "Numero : "))
                 (setq sync0-bibtex-entry-pages
                       (read-string "Pages (ex. : 90-180) : "))
                 (setq sync0-bibtex-entry-parent
                       sync0-bibtex-entry-journal)
                 (setq sync0-bibtex-entry-doi
                       (read-string "DOI : " nil nil nil t))
                 (setq sync0-bibtex-entry-url
                       (unless (string= sync0-bibtex-entry-doi "") 
                         (read-string "Url : " nil nil nil t)))))
        ("Book" (lambda ()
                  (setq sync0-bibtex-entry-volume
                        (read-string "Tome : "))
                  (sync0-bibtex-entry-define-publisher)
                  (sync0-bibtex-entry-define-location)
                  (setq sync0-bibtex-entry-series
                        (read-string "Series : "))
                  (setq sync0-bibtex-entry-parent
                        sync0-bibtex-entry-series)
                   (sync0-bibtex-entry-define-media)
                   (sync0-bibtex-entry-define-library)
                  (setq sync0-bibtex-entry-url
                        (read-string "Url : " nil nil nil t))))
        ("Collection" (lambda ()
                   (setq sync0-bibtex-entry-volume
                         (read-string "Tome : "))
                  (sync0-bibtex-entry-define-publisher)
                  (sync0-bibtex-entry-define-location)
                  (setq sync0-bibtex-entry-series
                           (read-string "Series : "))
                   (setq sync0-bibtex-entry-parent
                         sync0-bibtex-entry-series)
                   (sync0-bibtex-entry-define-media)
                   (sync0-bibtex-entry-define-library)
                   (setq sync0-bibtex-entry-url
                           (read-string "Url : " nil nil nil t))))
        ("InBook" (lambda ()
                    (setq sync0-bibtex-entry-extract
                          (yes-or-no-p "Extract  PDF from existing entry?"))
                    (setq sync0-bibtex-entry-use-extraction-page-numbers
                          (when sync0-bibtex-entry-extract
                            (yes-or-no-p "Use page numbers from extraction?")))
                    (setq sync0-bibtex-entry-pages
                               (read-string "Pages (ex. : 90-180) : "))
                    (sync0-bibtex-entry-define-booktitle)
                    (setq sync0-bibtex-entry-chapter
                          (read-string "Chapter : "))
                    (setq sync0-bibtex-entry-parent
                          (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
                              sync0-bibtex-entry-booktitle
                            (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
                    (unless sync0-bibtex-entry-crossref 
                      (progn
                        (setq sync0-bibtex-entry-series
                              (read-string "Series : "))
                        (sync0-bibtex-entry-define-media)
                        (sync0-bibtex-entry-define-library)
                        (sync0-bibtex-entry-define-publisher)
                        (sync0-bibtex-entry-define-location)))))
        ("InCollection" (lambda ()
                    (setq sync0-bibtex-entry-extract
                          (yes-or-no-p "Extract  PDF from existing entry?"))
                    (setq sync0-bibtex-entry-use-extraction-page-numbers
                          (when sync0-bibtex-entry-extract
                            (yes-or-no-p "Use page numbers from extraction?")))
                    (setq sync0-bibtex-entry-pages
                               (read-string "Pages (ex. : 90-180) : "))
                    (sync0-bibtex-entry-define-booktitle)
                    (setq sync0-bibtex-entry-chapter
                          (read-string "Chapter : "))
                    (setq sync0-bibtex-entry-parent
                          (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
                              sync0-bibtex-entry-booktitle
                            (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
                    (unless sync0-bibtex-entry-crossref 
                      (progn
                        (setq sync0-bibtex-entry-series
                              (read-string "Series : "))
                        (sync0-bibtex-entry-define-media)
                        (sync0-bibtex-entry-define-library)
                        (sync0-bibtex-entry-define-publisher)
                        (sync0-bibtex-entry-define-location)))))
        ("InProceedings" (lambda ()
                    (setq sync0-bibtex-entry-extract
                          (yes-or-no-p "Extract  PDF from existing entry?"))
                    (setq sync0-bibtex-entry-use-extraction-page-numbers
                          (when sync0-bibtex-entry-extract
                            (yes-or-no-p "Use page numbers from extraction?")))
                    (setq sync0-bibtex-entry-pages
                               (read-string "Pages (ex. : 90-180) : "))
                    (sync0-bibtex-entry-define-booktitle)
                    (setq sync0-bibtex-entry-chapter
                          (read-string "Chapter : "))
                    (setq sync0-bibtex-entry-parent
                          (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
                              sync0-bibtex-entry-booktitle
                            (concat sync0-bibtex-entry-booktitle " : " sync0-bibtex-entry-booksubtitle)))
                    (unless sync0-bibtex-entry-crossref 
                      (progn
                        (setq sync0-bibtex-entry-eventtitle
                              (read-string "Event title : "))
                        (setq sync0-bibtex-entry-eventdate
                              (read-string "Event date (ex. 1990-12-28) : "))
                        (setq sync0-bibtex-entry-series
                              (read-string "Series : "))
                        (sync0-bibtex-entry-define-media)
                        (sync0-bibtex-entry-define-library)
                        (sync0-bibtex-entry-define-publisher)
                        (sync0-bibtex-entry-define-location)))))
        ("Manual" (lambda ()
                    (setq sync0-bibtex-entry-institution
                          (read-string "Institution : "))
                    (setq sync0-bibtex-entry-number
                          (read-string "Numero : "))
                    (setq sync0-bibtex-entry-volume
                          (read-string "Tome : "))
                    (sync0-bibtex-entry-define-publisher)
                    (sync0-bibtex-entry-define-location)
                    (setq sync0-bibtex-entry-series
                          (read-string "Series : "))
                    (setq sync0-bibtex-entry-parent
                          sync0-bibtex-entry-series)
                    (sync0-bibtex-entry-define-media)
                    (sync0-bibtex-entry-define-library)
                    (setq sync0-bibtex-entry-url
                          (read-string "Url : " nil nil nil t))
                    (setq sync0-bibtex-entry-addendum
                          (read-string "Addendum (ex. Box, Folder, etc.) : "))))
        ("Online" (lambda ()
                    (setq sync0-bibtex-entry-institution
                          (read-string "Institution : "))
                    (sync0-bibtex-entry-define-publisher)
                    (setq sync0-bibtex-entry-series
                          (read-string "Series : "))
                    (setq sync0-bibtex-entry-parent
                          sync0-bibtex-entry-series)
                    (sync0-bibtex-entry-define-media)
                    (sync0-bibtex-entry-define-library)
                    (setq sync0-bibtex-entry-url
                          (read-string "Url : " nil nil nil t))
                    (setq sync0-bibtex-entry-addendum
                          (read-string "Addendum (ex. Box, Folder, etc.) : "))))
        ("Report" (lambda ()
                    (setq sync0-bibtex-entry-institution
                          (read-string "Institution : "))
                    (setq sync0-bibtex-entry-number
                          (read-string "Numero : "))
                   (setq sync0-bibtex-entry-volume
                         (read-string "Tome : "))
                   (setq sync0-bibtex-entry-series
                           (read-string "Series : "))
                   (setq sync0-bibtex-entry-parent
                         sync0-bibtex-entry-series)
                    (sync0-bibtex-entry-define-publisher)
                    (sync0-bibtex-entry-define-location)
                    (sync0-bibtex-entry-define-media)
                    (sync0-bibtex-entry-define-library)
                   (setq sync0-bibtex-entry-url
                           (read-string "Url : " nil nil nil t))
                    (setq sync0-bibtex-entry-addendum
                          (read-string "Addendum (ex. Box, Folder, etc.) : "))))
        ("Unpublished" (lambda ()
                    (setq sync0-bibtex-entry-number
                          (read-string "Numero : "))
                    (setq sync0-bibtex-entry-institution
                          (read-string "Institution : "))
                   (setq sync0-bibtex-entry-series
                           (read-string "Series : "))
                   (setq sync0-bibtex-entry-parent
                         sync0-bibtex-entry-series)
                    (sync0-bibtex-entry-define-publisher)
                    (sync0-bibtex-entry-define-location)
                    (sync0-bibtex-entry-define-media)
                    (sync0-bibtex-entry-define-library)
                   (setq sync0-bibtex-entry-url
                           (read-string "Url : " nil nil nil t))
                    (setq sync0-bibtex-entry-addendum
                          (read-string "Addendum (ex. Box, Folder, etc.) : "))))
        ("Thesis" (lambda ()
                    (setq sync0-bibtex-entry-number
                          (read-string "Numero : "))
                    (setq sync0-bibtex-entry-institution
                          (read-string "Institution : "))
                    (sync0-bibtex-entry-define-publisher)
                    (sync0-bibtex-entry-define-location)
                    (sync0-bibtex-entry-define-media)
                    (sync0-bibtex-entry-define-library)
                   (setq sync0-bibtex-entry-url
                           (read-string "Url : " nil nil nil t))
                    (setq sync0-bibtex-entry-addendum
                          (read-string "Addendum (ex. Box, Folder, etc.) : "))))
        ("Proceedings" (lambda ()
                   (setq sync0-bibtex-entry-volume
                         (read-string "Tome : "))
                  (sync0-bibtex-entry-define-publisher)
                  (sync0-bibtex-entry-define-location)
                  (setq sync0-bibtex-entry-series
                        (read-string "Series : "))
                  (setq sync0-bibtex-entry-parent
                        sync0-bibtex-entry-series)
                  (sync0-bibtex-entry-define-media)
                  (sync0-bibtex-entry-define-library)
                  (setq sync0-bibtex-entry-eventtitle
                        (read-string "Event title : "))
                  (setq sync0-bibtex-entry-eventdate
                        (read-string "Event date (ex. 1990-12-28) : "))
                  (setq sync0-bibtex-entry-url
                        (read-string "Url : " nil nil nil t))))))

         (defun sync0-bibtex-entry-define-language ()
           "Define the language for new BibLaTeX entry."
           (setq sync0-bibtex-entry-language
                 (completing-read "Choose language : "
                                  sync0-bibtex-languages nil nil initial-language)))

         (defun sync0-bibtex-entry-define-keywords ()
           "Define the keywords for new BibLaTeX entry."
           (setq sync0-bibtex-entry-keywords
                 (concat sync0-bibtex-entry-type-downcase ", "
                         (unless (sync0-null-p sync0-bibtex-entry-date)
                           (concat sync0-bibtex-entry-date ", "))
                         (unless (sync0-null-p sync0-bibtex-entry-origdate)
                           (concat sync0-bibtex-entry-origdate ", "))
                         (unless (sync0-null-p sync0-bibtex-entry-crossref)
                           (concat sync0-bibtex-entry-crossref ", "))
                         (unless (sync0-null-p sync0-bibtex-entry-author)
                           sync0-bibtex-entry-author))))

         (defun sync0-bibtex-entry-define-booktitle ()
           "Define the booktitle for new BibLaTeX entry."
           (setq sync0-bibtex-entry-booktitle
                 (if sync0-bibtex-entry-crossref 
                     (bibtex-completion-get-value "title" sync0-bibtex-entry-crossref-entry)
                   (read-string "Book title : ")))
           (setq sync0-bibtex-entry-subbooktitle
                 (if sync0-bibtex-entry-crossref 
                     (bibtex-completion-get-value "subtitle" sync0-bibtex-entry-crossref-entry)
                   (read-string "Book subtitle : "))))

        (defun sync0-bibtex-entry-define-library ()
          "Define the library for new BibLaTeX entry."
          (setq sync0-bibtex-entry-library
                (completing-read "Choose location to trace back: "
                                 sync0-bibtex-traces nil nil nil)))

        (defun sync0-bibtex-entry-define-location ()
          "Define the media for new BibLaTeX entry."
          (setq sync0-bibtex-entry-location
                (completing-read "Location : " sync0-bibtex-locations)))

        (defun sync0-bibtex-entry-define-publisher ()
          "Define the publisher for new BibLaTeX entry."
          (setq sync0-bibtex-entry-publisher
                (completing-read "Maison d'edition : " sync0-bibtex-publishers)))


        (defun sync0-bibtex-entry-define-media ()
          "Define the media for new BibLaTeX entry."
          (setq sync0-bibtex-entry-medium
                (string-trim
                 (prin1-to-string
                  (completing-read-multiple "Quel support ?"
                                            sync0-bibtex-media)) "(\"" "\")")))

        (defun sync0-bibtex-entry-define-author ()
          "Define the author for new BibLaTeX entry."
          (setq sync0-bibtex-entry-author
                (completing-read "Auteur : " sync0-bibtex-authors
                                 nil nil sync0-bibtex-entry-initial-author))
          (setq sync0-bibtex-entry-author-fixed
                (unless (string= sync0-bibtex-entry-author "nil")
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
                (cond ((string-match " and " sync0-bibtex-entry-author)
                       ;; create a list with parts 
                       (let* ((author-list  (split-string sync0-bibtex-entry-author " and "))
                              (last-names (let (x)
                                            (dolist  (element author-list x)
                                              (setq x (concat x
                                                              (progn
                                                                (string-match "\\([[:graph:]]+\\),"   element)
                                                                (match-string 1 element))
                                                              ", "))))))
                         (substring last-names 0 -2)))
                      ((string-match "^{" sync0-bibtex-entry-author)
                       (string-match "{\\([[:print:]]+\\)}" sync0-bibtex-entry-author)
                       (match-string 1 sync0-bibtex-entry-author))
                      (t (nth 0 (split-string sync0-bibtex-entry-author ", ")))))
          (setq sync0-bibtex-entry-lastname-tag
                (let ((raw (cond ((string-match " and " sync0-bibtex-entry-author)
                                  ;; create a list with parts 
                                  (let* ((author-list  (split-string author " and "))
                                         (last-names (let (x)
                                                       (dolist  (element author-list x)
                                                         (setq x (concat x
                                                                         (progn
                                                                           (string-match "\\([[:graph:]]+\\),"   element)
                                                                           (match-string 1 element))
                                                                         ", author/"))))))
                                    (substring last-names 0 -7)))
                                 ((string-match "^{" author)
                                  (string-match "{\\([[:print:]]+\\)}" author)
                                  (match-string 1 author))
                                 (t (nth 0 (split-string author ", "))))))
                  (replace-regexp-in-string "[[:space:]]+" "_" (downcase raw)))))

        (defun sync0-bibtex-entry-define-title ()
          "Define the title for new BibLaTeX entry."
            (setq sync0-bibtex-entry-title
                  (let* ((candidates (bibtex-completion-candidates))
                         (selection (ivy-read "Input title : "
                                              candidates
                                              :caller 'ivy-bibtex
                                              :require-match nil
                                              :history 'ivy-bibtex-history)))
                    (if (string-match-p "[0-9]\\{14\\}$" selection)
                        (cdr (assoc "title" (cdr (assoc selection candidates))))
                      selection)))
            (setq sync0-bibtex-entry-subtitle
                  (read-string "Sous-titre du texte : " nil nil nil t))
            (setq sync0-bibtex-entry-title-fixed
                  (if (string= sync0-bibtex-entry-subtitle "")
                      sync0-bibtex-entry-title
                    (concat sync0-bibtex-entry-title " : " sync0-bibtex-entry-subtitle))))

        (defun sync0-bibtex-update-vars ()
          "Define the title for new BibLaTeX entry."
          (unless (member sync0-bibtex-entry-author sync0-bibtex-authors)
            (add-to-list 'sync0-bibtex-authors sync0-bibtex-entry-author)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-authors.txt"
              (sync0-insert-elements-of-list sync0-bibtex-authors)))
          (unless (member sync0-bibtex-entry-booktitle sync0-bibtex-booktitles)
            (add-to-list 'sync0-bibtex-booktitles sync0-bibtex-entry-booktitle)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-booktitles.txt"
              (sync0-insert-elements-of-list sync0-bibtex-booktitles)))
          (unless (member sync0-bibtex-entry-location sync0-bibtex-locations)
            (add-to-list 'sync0-bibtex-locations sync0-bibtex-entry-location)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-locations.txt"
              (sync0-insert-elements-of-list sync0-bibtex-locations)))
          (unless (member sync0-bibtex-entry-library sync0-bibtex-traces)
            (add-to-list 'sync0-bibtex-traces sync0-bibtex-entry-library)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-traces.txt"
              (sync0-insert-elements-of-list sync0-bibtex-traces)))
          (unless (member sync0-bibtex-entry-medium sync0-bibtex-media)
            (add-to-list 'sync0-bibtex-media sync0-bibtex-entry-medium)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-media.txt"
              (sync0-insert-elements-of-list sync0-bibtex-media)))
          (unless (member sync0-bibtex-entry-publisher sync0-bibtex-publishers)
            (add-to-list 'sync0-bibtex-publishers sync0-bibtex-entry-location)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-publishers.txt"
              (sync0-insert-elements-of-list sync0-bibtex-publishers)))
          (unless (member sync0-bibtex-entry-institution sync0-bibtex-institutions)
            (add-to-list 'sync0-bibtex-institutions sync0-bibtex-entry-institution)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-institutions.txt"
              (sync0-insert-elements-of-list sync0-bibtex-institutions)))
          (unless (member sync0-bibtex-entry-journal sync0-bibtex-journals)
            (add-to-list 'sync0-bibtex-journals sync0-bibtex-entry-journal)
            (with-temp-file "~/.emacs.d/sync0-vars/bibtex-journals.txt"
              (sync0-insert-elements-of-list sync0-bibtex-journals))))

        (defun sync0-bibtex-entry-append-to-bibliography (bibkey)
          "Define the title for new BibLaTeX entry."
          (let* ((definitions (mapcar #'eval sync0-bibtex-entry-definitions))
                 (bibtex-fields (if (or (string= type "Collection")
                                        (string= type "Proceedings"))
                             (cl-substitute "editor" "author" sync0-bibtex-fields :test #'string=)
                           sync0-bibtex-fields))
                 (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields definitions))
                 ;; define the bibtex entries
                 (entries
                  (let (x)
                    (dolist (element fields x) 
                      (unless (sync0-null-p (cadr element))
                        (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
                 (bibtex-entry (concat "\n@" type "{" bibkey ",\n" entries "}\n")))
            (if (string= sync0-default-bibliography (buffer-file-name))
                (progn
                  (goto-char (point-max))
                  (insert bibtex-entry))
              (with-temp-buffer 
                (insert bibtex-entry)
                (append-to-file (point-min) (point-max) sync0-default-bibliography)))))

        (defun sync0-bibtex-entry-define-date ()
          "Define the dates for new BibLaTeX entry."
            (setq sync0-bibtex-entry-creation-date
                  (format-time-string "%Y-%m-%d")) 
            (setq sync0-bibtex-entry-date
                  (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-date))
            (setq sync0-bibtex-entry-origdate
                  (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-origdate))
            (setq sync0-bibtex-entry-date-tag
                  (replace-regexp-in-string "-" "/" sync0-bibtex-entry-date))
            (setq sync0-bibtex-entry-date-fixed
                  (if (or (string= sync0-bibtex-entry-origdate "")
                          (string= sync0-bibtex-entry-date sync0-bibtex-entry-origdate))
                      (concat "(" sync0-bibtex-entry-date ")")
                    (concat "(" sync0-bibtex-entry-origdate ") (" sync0-bibtex-entry-date ")"))))

        (defun sync0-bibtex-entry-create-obsidian-note-from-entry (bibkey)
          "Define the dates for new BibLaTeX entry."
          (let ((obsidian-file (concat sync0-obsidian-directory bibkey ".md")) 
                (obsidian-entry (concat "---\n"
                                        "zettel_type: reference\n"
                                        "id: " bibkey "\n"
                                        "citekey: " bibkey "\n"
                                        "created: " sync0-bibtex-entry-creation-date "\n"
                                        "biblatex_type: " sync0-bibtex-entry-type-downcase  "\n"
                                        "title: \"" sync0-bibtex-entry-title "\"\n"
                                        (unless (sync0-null-p sync0-bibtex-entry-subtitle)
                                          (concat "subtitle: \"" sync0-bibtex-entry-subtitle "\"\n"))
                                        (unless (sync0-null-p sync0-bibtex-entry-author)
                                          (concat "author: [" sync0-bibtex-entry-author-fixed "]\n"))
                                        (unless (sync0-null-p sync0-bibtex-entry-crossref)
                                          (concat "crossref: " sync0-bibtex-entry-crossref "\n"))
                                        (unless (sync0-null-p sync0-bibtex-entry-parent)
                                          (concat "parent: \"" sync0-bibtex-entry-parent "\"\n"))
                                        (if (sync0-null-p sync0-bibtex-entry-subtitle)
                                            (concat "aliases: [\"" (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed "\"]\n")
                                          (concat "aliases: [\"" (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed "\", "
                                                  "\"" (unless (sync0-null-p sync0-bibtex-entry-author) sync0-bibtex-entry-lastname) " " sync0-bibtex-entry-date-fixed  " " sync0-bibtex-entry-title "\"]\n"))
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
                                        "tags: [reference/" sync0-bibtex-entry-type-downcase ", bibkey/" bibkey ", date/" bibkeydate-tag (unless (string= sync0-bibtex-entry-author "nil") (concat ", author/" sync0-bibtex-entry-lastname-tag)) ", language/" sync0-bibtex-entry-language "]\n"
                                        "---\n" 
                                        "# " (unless (sync0-null-p sync0-bibtex-entry-author) (concat sync0-bibtex-entry-lastname " ")) sync0-bibtex-entry-date-fixed " " sync0-bibtex-entry-title-fixed "\n\n"     
                                        "## Description\n\n" 
                                        "## Progrès de la lecture\n\n" 
                                        "## Annotations\n\n")))
            (with-temp-buffer 
              (insert obsidian-entry)
              (write-file obsidian-file))))

        (defun sync0-bibtex-entry-extract-pdf (filename &optional beg end)
          "Extract pdf from crossref defined by
           sync0-bibtex-entry-crossref. Before activating,
           sync0-bibtex-entry-extract must be set to t. Filename
           is not automatically calculated because this function
           is not standalone; it is intended to be part of
           pipes. Therefore, filename is the only mandatory argument."
          (when (and  sync0-bibtex-entry-crossref
                      sync0-bibtex-entry-extract)
            (let* ((beg (unless beg
                          (read-string "First page for extraction: ")))
                   (end (unless end
                          (read-string "Last page for extraction: ")))
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

        (defun sync0-bibtex-completion-choose-key ()
          "Choose key with completion."
          (let* ((candidates (bibtex-completion-candidates))
                 (selection (ivy-read "Crossref : "
                                      candidates
                                      :caller 'ivy-bibtex
                                      :history 'ivy-bibtex-history)))
            (cdr (assoc "=key=" (cdr (assoc selection candidates))))))

        (defun sync0-bibtex-define-entry ()
          "Define the title for new BibLaTeX entry."
          (interactive)
          ;; Before calculating any values, reset all values in my
          ;; master list of variable (set them to nil).
          (mapcar #'(lambda (a) (set a nil)) sync0-bibtex-entry-definitions)
          (let* ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
                 (derivation (when (or (string= type "InCollection")
                                       (string= type "InBook")
                                       (string= type "InProceedings"))
                               (yes-or-no-p "Derive entry?"))))
            (setq sync0-bibtex-entry-crossref 
                  (when derivation
                    (sync0-bibtex-completion-choose-key)))
            (setq sync0-bibtex-entry-crossref-entry
                  (when sync0-bibtex-entry-crossref
                    (bibtex-completion-get-entry sync0-bibtex-entry-crossref)))
            (if derivation
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
                      (bibtex-completion-get-value "language" sync0-bibtex-entry-crossref-entry))
              (setq sync0-bibtex-entry-initial-date nil
                    sync0-bibtex-entry-initial-origdate nil
                    sync0-bibtex-entry-initial-author nil
                    sync0-bibtex-entry-initial-language nil))
            ;; General settings
            (setq sync0-bibtex-entry-type type)
            (setq sync0-bibtex-entry-type-downcase
                  (downcase type))
            (setq sync0-bibtex-entry-key
                  (format-time-string "%Y%m%d%H%M%S"))
            (setq sync0-bibtex-entry-attachment
                  (unless (string= sync0-bibtex-entry-type "Online")
                    (concat "/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf")))
            (sync0-bibtex-entry-define-author)
            (sync0-bibtex-entry-define-title)
            (sync0-bibtex-entry-define-date)
            (sync0-bibtex-entry-define-language)
            (sync0-bibtex-entry-define-keywords)
            ;; Type-specific settings
            (funcall (cadr (assoc type sync0-bibtex-entry-functions)))
            ;; delete unused entries
            (if sync0-bibtex-entry-use-extraction-page-numbers
                (let ((beg (progn
                             (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
                             (match-string 1 sync0-bibtex-entry-pages)))
                      (end (progn
                             (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
                             (match-string 1 sync0-bibtex-entry-pages))))
                  (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end))
              (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key))
            (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
            (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
            ;; Reset these two values to prevent unwanted extractions
            (setq sync0-bibtex-entry-extract nil
                  sync0-bibtex-entry-crossref-entry nil
                  sync0-bibtex-entry-use-extraction-page-numbers nil)
            ;; Insert entry in default bibliography file
            (sync0-bibtex-update-vars)))


;; (defun sync0-bibtex-define-entry ()
;;   "Define the title for new BibLaTeX entry."
;;   (let* ((derivation (yes-or-no-p "Derive entry?"))
;;          (crossref (when derivation
;;                      (let* ((candidates (bibtex-completion-candidates))
;;                             (selection (ivy-read "Crossref : "
;;                                                  candidates
;;                                                  :caller 'ivy-bibtex
;;                                                  :history 'ivy-bibtex-history)))
;;                        (cdr (assoc "=key=" (cdr (assoc selection candidates)))))))
;;          (initial-date (when derivation
;;                          (sync0-org-ref-get-citation-date crossref)))
;;          (initial-origdate (when derivation
;;                              (sync0-org-ref-get-citation-origdate crossref)))
;;          (initial-author (when derivation
;;                            (sync0-org-ref-get-citation-author crossref)))
;;          (initial-language (when  derivation
;;                              (sync0-org-ref-get-citation-language crossref))))





;;     (setq sync0-bibtex-entry-crossref crossref)
;;     (setq sync0-bibtex-entry-type
;;           (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
;;     (setq sync0-bibtex-entry-type-downcase
;;           (downcase sync0-bibtex-entry-type)))
;;   (setq sync0-bibtex-entry-key
;;         (format-time-string "%Y%m%d%H%M%S"))
;;   (setq sync0-bibtex-entry-attachment
;;         (unless (string= sync0-bibtex-entry-type "Online")
;;           (concat "/home/sync0/Documents/pdfs/" sync0-bibtex-entry-key ".pdf")))
;;   (setq sync0-bibtex-entry-creation-date
;;         (format-time-string "%Y-%m-%d")) 
;;   (setq sync0-bibtex-entry-title
;;         (let* ((candidates (bibtex-completion-candidates))
;;                (selection (ivy-read "Input title : "
;;                                     candidates
;;                                     :caller 'ivy-bibtex
;;                                     :require-match nil
;;                                     :history 'ivy-bibtex-history)))
;;           (if (string-match-p "[0-9]\\{14\\}$" selection)
;;               (cdr (assoc "title" (cdr (assoc selection candidates))))
;;             selection)))
;;   (setq sync0-bibtex-entry-subtitle
;;         (read-string "Sous-titre du texte : " nil nil nil t))
;;   (setq sync0-bibtex-entry-title-fixed
;;         (if (string= sync0-bibtex-entry-subtitle "")
;;             sync0-bibtex-entry-title
;;           (concat sync0-bibtex-entry-title " : " sync0-bibtex-entry-subtitle)))
;;   (setq sync0-bibtex-entry-date
;;         (read-string "Date (ex. 1890-18-12) : " initial-date))
;;   (setq sync0-bibtex-entry-origdate
;;         (read-string "Date (ex. 1890-18-12) : " initial-origdate))
;;   (setq sync0-bibtex-entry-date-tag
;;         (replace-regexp-in-string "-" "/" sync0-bibtex-entry-date))
;;   (setq sync0-bibtex-entry-date-fixed
;;         (if (or (string= sync0-bibtex-entry-origdate "")
;;                 (string= sync0-bibtex-entry-date sync0-bibtex-entry-origdate))
;;             (concat "(" sync0-bibtex-entry-date ")")
;;           (concat "(" sync0-bibtex-entry-origdate ") (" sync0-bibtex-entry-date ")"))))
;; (setq sync0-bibtex-entry-author
;;       (completing-read "Auteur : " sync0-bibtex-authors
;;                        nil nil initial-author))
;; (setq sync0-bibtex-entry-author-fixed
;;       (unless (string= sync0-bibtex-entry-author "nil")
;;         (cond ((string-match " and " sync0-bibtex-entry-author)
;;                ;; create a list with parts 
;;                (let* ((author-list  (split-string sync0-bibtex-entry-author " and "))
;;                       (names (let (x)
;;                                (dolist  (element author-list x)
;;                                  (setq x (concat x element "\", \""))))))
;;                  (concat "\"" (substring names 0 -2))))
;;               ;; check when author is an organization
;;               ((string-match "^{" sync0-bibtex-entry-author)
;;                (concat "\"" (substring sync0-bibtex-entry-author 1 -1) "\""))
;;               ;; other cases
;;               (t (concat "\"" sync0-bibtex-entry-author "\"")))))
;; (setq sync0-bibtex-entry-lastname
;;       (cond ((string-match " and " sync0-bibtex-entry-author)
;;              ;; create a list with parts 
;;              (let* ((author-list  (split-string sync0-bibtex-entry-author " and "))
;;                     (last-names (let (x)
;;                                   (dolist  (element author-list x)
;;                                     (setq x (concat x
;;                                                     (progn
;;                                                       (string-match "\\([[:graph:]]+\\),"   element)
;;                                                       (match-string 1 element))
;;                                                     ", "))))))
;;                (substring last-names 0 -2)))
;;             ((string-match "^{" sync0-bibtex-entry-author)
;;              (string-match "{\\([[:print:]]+\\)}" sync0-bibtex-entry-author)
;;              (match-string 1 sync0-bibtex-entry-author))
;;             (t (nth 0 (split-string sync0-bibtex-entry-author ", ")))))
;; (setq sync0-bibtex-entry-lastname-tag
;;       (let ((raw (cond ((string-match " and " author)
;;                         ;; create a list with parts 
;;                         (let* ((author-list  (split-string author " and "))
;;                                (last-names (let (x)
;;                                              (dolist  (element author-list x)
;;                                                (setq x (concat x
;;                                                                (progn
;;                                                                  (string-match "\\([[:graph:]]+\\),"   element)
;;                                                                  (match-string 1 element))
;;                                                                ", author/"))))))
;;                           (substring last-names 0 -7)))
;;                        ((string-match "^{" author)
;;                         (string-match "{\\([[:print:]]+\\)}" author)
;;                         (match-string 1 author))
;;                        (t (nth 0 (split-string author ", "))))))
;;         (replace-regexp-in-string "[[:space:]]+" "_" (downcase raw))))
;; (setq sync0-bibtex-entry-language
;;       (completing-read "Choose language : "
;;                        sync0-bibtex-languages nil nil initial-language))


;; ;; Next : create functions for defining author strings

(defun sync0-bibtex-update-completion-files (seqlists)
  (interactive)
  (dolist (element seqlists)
    (let ((current-elements)
          (final-list)
          (bib-elements (mapcar #'(lambda (x) (cdr (assoc (car element) x)))
                                  (bibtex-completion-candidates))))
      (with-temp-buffer
        (insert-file-contents (cadddr element))
        (goto-char (point-min))
        ;; (keep-lines "contexts" (point-min) (point-max)) 
        (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
          (push  (match-string 1) current-elements)))
      (with-temp-file (cadddr element)
        (setq final-list (delete-dups (append current-elements bib-elements)))
        (sync0-insert-elements-of-list final-list)))))

  (setq foo
    '(("booktitle" (sync0-bibtex-entry-booktitle sync0-bibtex-booktitles "~/.emacs.d/sync0-vars/bibtex-booktitles.txt"))
      ("publisher" (sync0-bibtex-entry-publisher sync0-bibtex-publishers "~/.emacs.d/sync0-vars/bibtex-publishers.txt"))
      ("journaltitle" (sync0-bibtex-entry-journal sync0-bibtex-journals "~/.emacs.d/sync0-vars/bibtex-journals.txt"))
      ("location" (sync0-bibtex-entry-location sync0-bibtex-locations "~/.emacs.d/sync0-vars/bibtex-locations.txt"))
      ("author" (sync0-bibtex-entry-author sync0-bibtex-authors "~/.emacs.d/sync0-vars/bibtex-authors.txt"))
      ("library" (sync0-bibtex-entry-library sync0-bibtex-traces "~/.emacs.d/sync0-vars/bibtex-trace.txt"))
      ("medium" (sync0-bibtex-entry-medium sync0-bibtex-media "~/.emacs.d/sync0-vars/bibtex-media.txt"))
      ("institution" (sync0-bibtex-entry-institution sync0-bibtex-institutions "~/.emacs.d/sync0-vars/bibtex-institutions.txt"))
      ("language" (sync0-bibtex-entry-language sync0-bibtex-languages  "~/.emacs.d/sync0-vars/languages.txt"))))

      (setq test '("author" (sync0-bibtex-entry-author sync0-bibtex-authors "~/.emacs.d/sync0-vars/bibtex-authors.txt")))


(defun sync0-bibtex-update-vars (seqlists)
  "Update variables used for completion based on the information
   provided by the new entry."
  (dolist (element seqlists)
    (unless (member (eval (cadr element))  (eval (caddr element)))
      (push (cadr element) (caddr element))
      (append-to-file (concat (eval (cadr element)) "\n") nil (cadddr element))))))


(defun sync0-bibtex-extract-multiple-entries-from-pdf (seqlists)
  "Extract pdfs and create bibtex entries and markdown entries
for each extranction. This function requires an input 'seqlists'
that is a list composed of n lists of the form: (name,
first-page, last-page). Likewise, for this to work, it is
necessary that the pages of the pdf match the page numbers of the
actual book. Otherwise, the entries will have wrong data."
  ;; Set all fields to nil 
  (sync0-nullify-variables sync0-bibtex-entry-definitions-list)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key))
            (entry (bibtex-completion-get-entry bibkey))
            (type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
            (input-file (car (bibtex-completion-find-pdf bibkey)))
            (obsidian-master-note (concat sync0-obsidian-directory bibkey ".md")))
    ;; (setq sync0-bibtex-entry-creation-entry entry)
    (setq sync0-bibtex-entry-crossref bibkey)
    (setq sync0-bibtex-entry-crossref-entry entry)
    (sync0-nullify-variables sync0-bibtex-entry-initial-fields-list))
  ;; General settings for all notes to be created
  (setq sync0-bibtex-entry-type type)
  (setq sync0-bibtex-entry-type-downcase
        (downcase type))
  (sync0-bibtex-entry-define-author bibkey)
  (sync0-bibtex-entry-define-date bibkey)
  (sync0-bibtex-entry-define-language bibkey)
  (setq sync0-bibtex-entry-parent
        (bibtex-completion-get-value "title" entry))
  ;; Begin loop; specific settings for each note are to be defined
  ;; inside the loop.
  (dolist (elt seqlists)
    (let* ((raw-filename (1+ (string-to-number (format-time-string "%Y%m%d%H%M%S"))))
           (filename (number-to-string raw-filename))
           (file-pdf (concat "/home/sync0/Documents/pdfs/" filename ".pdf"))
           (title (nth 0 elt))
           (beg (number-to-string (nth 1 elt)))
           (end (number-to-string (nth 2 elt)))
           (pages (concat beg "-" end))
           (obsidian-reference
            (concat "\n- [" sync0-bibtex-entry-lastname " " sync0-bibtex-entry-date-fixed " " title "](" filename ".md)\n")))
      (setq sync0-bibtex-entry-key filename)
      (setq sync0-bibtex-entry-title title)
      (setq sync0-bibtex-entry-subtitle nil)
      (setq sync0-bibtex-entry-title-fixed sync0-bibtex-entry-title)
      (sync0-bibtex-entry-define-keywords)
      (setq sync0-bibtex-entry-extract t)
      (setq sync0-bibtex-entry-use-extraction-page-numbers t)
      (setq sync0-bibtex-entry-pages pages)
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
      (append-to-file obsidian-reference nil obsidian-master-note)))
      ;; End of loop
      (sync0-nullify-variables sync0-bibtex-entry-extraction-fields-list))












    






    (setq sync0-bibtex-entry-pages
          (bibtex-completion-get-value "pages" sync0-bibtex-entry-creation-entry))
    (unless (sync0-null-p sync0-bibtex-entry-pages)
      (setq sync0-bibtex-entry-extract
            (yes-or-no-p "Extract  PDF from existing entry?"))
      (setq sync0-bibtex-entry-use-extraction-page-numbers
            (when sync0-bibtex-entry-extract
              (yes-or-no-p "Use page numbers from extraction?")))
      (if sync0-bibtex-entry-use-extraction-page-numbers
          (let ((beg (progn
                       (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
                       (match-string 1 sync0-bibtex-entry-pages)))
                (end (progn
                       (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
                       (match-string 1 sync0-bibtex-entry-pages))))
            (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key beg end))
        (sync0-bibtex-entry-extract-pdf sync0-bibtex-entry-key)))
    (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
    ;; Reset these two values to prevent unwanted extractions
    (setq sync0-bibtex-entry-extract nil
          sync0-bibtex-entry-crossref-entry nil
          sync0-bibtex-entry-creation-entry nil
          sync0-bibtex-entry-use-extraction-page-numbers nil)))










         ;; (origdate (bibtex-completion-get-value "origdate" entry))
         ;; (date (bibtex-completion-get-value "date" entry))
         ;; (date-fixed (cond ((and (null date)
         ;;                         (null origdate)) "")
         ;;                   ((null origdate)
         ;;                    (concat "(" date ")"))
         ;;                   (t (concat "(" origdate ") (" date ")"))))
         ;; (date-tag
         ;;  (replace-regexp-in-string "-" "/" date))



         ;; (author (completing-read "Auteur : " sync0-bibtex-completion-author
         ;;                          nil nil nil))
         ;; (author-fixed (unless (string= author "nil")
         ;;                 (cond ((string-match " and " author)
         ;;                        ;; create a list with parts 
         ;;                        (let* ((author-list  (split-string author " and "))
         ;;                               (names (let (x)
         ;;                                        (dolist  (element author-list x)
         ;;                                          (setq x (concat x element "\", \""))))))
         ;;                          (concat "\"" (substring names 0 -2))))
         ;;                       ;; check when author is an organization
         ;;                       ((string-match "^{" author)
         ;;                        (concat "\"" (substring author 1 -1) "\""))
         ;;                       ;; other cases
         ;;                       (t (concat "\"" author "\"")))))
         ;; (lastname (cond ((string-match " and " author)
         ;;                  ;; create a list with parts 
         ;;                  (let* ((author-list  (split-string author " and "))
         ;;                         (last-names (let (x)
         ;;                                       (dolist  (element author-list x)
         ;;                                         (setq x (concat x
         ;;                                                         (progn
         ;;                                                           (string-match "\\([[:graph:]]+\\),"   element)
         ;;                                                           (match-string 1 element))
         ;;                                                         ", "))))))
         ;;                    (substring last-names 0 -2)))
         ;;                 ((string-match "^{" author)
         ;;                  (string-match "{\\([[:print:]]+\\)}" author)
         ;;                  (match-string 1 author))
         ;;                 (t (nth 0 (split-string author ", ")))))
         ;; (lastname-raw (cond ((string-match " and " author)
         ;;                      ;; create a list with parts 
         ;;                      (let* ((author-list  (split-string author " and "))
         ;;                             (last-names (let (x)
         ;;                                           (dolist  (element author-list x)
         ;;                                             (setq x (concat x
         ;;                                                             (progn
         ;;                                                               (string-match "\\([[:graph:]]+\\),"   element)
         ;;                                                               (match-string 1 element))
         ;;                                                             ", author/"))))))
         ;;                        ;; (substring last-names 0 -2)))
         ;;                        (substring last-names 0 -7)))
         ;;                     ((string-match "^{" author)
         ;;                      (string-match "{\\([[:print:]]+\\)}" author)
         ;;                      (match-string 1 author))
         ;;                     (t (nth 0 (split-string author ", ")))))
         ;; (author-tag-obsidian
         ;;  (replace-regexp-in-string "[[:space:]]+" "_" (downcase lastname-raw)))
         ;; (language (bibtex-completion-get-value "language" entry))
         ;; (langid language)
         ;; (crossref bibkey)) 


    ;; End of definition before loop
    ;; Loop for extracting the pdf using ghostcritp


(defun sync0-bibtex-extract-subpdf ()
  "Extract pdf from an existing pdf."
  ;; Set all fields to nil 
(interactive)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key))
            (input-file (car (bibtex-completion-find-pdf bibkey)))
            (output-file (concat sync0-pdfs-folder bibkey "a.pdf"))
            (range (read-string "Page range: "))
            (command (concat "pdftk " input-file " cat" range " output " output-file)))
(shell-command command)))

(defun sync0-bibtex-extract-subpdf ()
  "Extract pdf from an existing pdf."
  ;; Set all fields to nil 
(interactive)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key))
            (input-file (car (bibtex-completion-find-pdf bibkey)))
            (output-file (concat sync0-pdfs-folder bibkey "a.pdf"))
            (range (read-string "Page range: "))
            (command (concat "pdftk " input-file " cat" range " output " output-file)))
(shell-command command)))

(defun foo ()
  "Choose key with completion."
(interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			       (bibtex-parse-entry)))
         (preselect (cdr (assoc "=key=" entry))))
(message "%s" preselect)))
