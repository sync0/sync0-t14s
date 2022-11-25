;; -*- lexical-binding: t -*-

(defvar sync0-bibtex-fields
  '("title" "subtitle" "origtitle" "eventtitle" "date" "origdate" "eventdate" "author" "editor" "translator" "recipient" "introduction" "journaltitle" "edition" "booktitle" "booksubtitle" "crossref" "chapter" "volume" "volumes" "number" "series" "publisher" "location" "pages" "note" "doi" "url" "urldate" "language" "langid" "origlanguage" "medium" "institution" "library" "related" "relatedtype" "relatedstring" "file" "created" "password" "shorttitle" "doctype" "shorthand" "description" "keywords" "foreword" "afterword" "editortype" "pagetotal" "verba" "cote" "project" "site" "version" "people" "country")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-string-fields
  '("subtitle" "eventtitle" "eventdate" "edition" "chapter" "volume" "volumes" "number" "pages" "pagetotal" "doi" "password" "shorttitle" "shorthand" "description" "verba" "cote" "version")
  "List of Bibtex entry fields that use read-string for being defined")

(defvar sync0-bibtex-string-multiple-fields
  '("medium" "doctype" "project" "country")
  "List of Bibtex entry fields that use completing-read-multiple for being defined")

(defvar sync0-bibtex-date-fields
  '("date" "origdate" "eventdate" "urldate" "created")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-people-fields
  '("author" "editor" "people" "recipient" "translator" "introduction" "foreword" "afterword")
  "List of Bibtex entry fields")

  ;; '("author" "editor" "editorone" "people" "recipient" "translator" "introduction" "foreword" "afterword")

(defvar sync0-bibtex-full-fields
  '("title" "subtitle" "date" "origdate" "author" "editor" "journaltitle" "booktitle" "booksubtitle" "translator" "crossref"  "eventdate" "eventtitle" "venue" "volume" "number" "chapter" "edition" "pages" "publisher" "location" "pages" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-entry-types
  '("Article" "MvBook" "Book" "InBook" "InCollection" "MvCollection" "Collection" "Unpublished" "Thesis" "MvProceedings" "Proceedings" "InProceedings" "Online" "Report" "Manual" "Misc")
  "List of Bibtex entry types")

(defvar sync0-bibtex-entry-types-correction '("mvbook" "inbook" "incollection" "mvcollection" "mvproceedings" "inproceedings")
  "Entry types used to correct case problems when extracting entries from Obsidian into BibLaTex.")

(defvar sync0-bibtex-crossref-types
  '("InBook" "InCollection" "InProceedings")
  "List of Bibtex entry types")

;; (defvar sync0-bibtex-quick-fields
;;   '("title" "subtitle" "date" "author" "editor" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
;;   "List of Bibtex entry fields")

(setq sync0-bibtex-obsidian-reference-template-top
      (concat  "\n## Description\n\n" 
               "## Progrès de la lecture\n\n"
               "## Annotations\n\n"
               "```dataview\n"
               "TABLE WITHOUT ID\n"
               "link(file.name, title) AS \"Title\", created AS \"Created\"\n"
               ;; "title AS \"Titre\"\n"
               "FROM #permanent/annotation AND #bibkey/"))

(setq sync0-bibtex-obsidian-reference-template-bottom
      (concat "\n"
              "SORT created DESC\n"
              "```\n\n"))

(setq sync0-bibtex-quick-fields
      '("title"
        "subtitle"
        "date"
        "created"
        ;; "author"
        ;; "url"
        "file"))

(defvar sync0-bibtex-extract-fields
  '("title" "date" "author" "crossref" "pages" "language" "langid" "file" "keywords")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-completion-fields
  '("publisher" "journaltitle" "location" "title" "author" "keywords" "note" "library" "series" "medium" "institution" "language" "doctype" "project" "site" "country")
  "List of Bibtex entry completion fields")

;; Set completion vars and create an alist of variables used to define their initial values to be used in completion.

;; (let ((prefix "sync0-bibtex-completion-")
;;       x)
;;   (dolist (element sync0-bibtex-completion-fields x)
;;     (let ((my-var (intern (concat prefix element))))
;;       (set my-var nil)
;;       (push my-var x))))

  ;; (setq sync0-bibtex-completions-collection (nreverse x))

(let ((prefix "sync0-bibtex-completion-")
      (file-prefix (concat sync0-vars-dir "bibtex-completion-"))
      x)
  (dolist (element sync0-bibtex-completion-fields x)
    (let* ((my-var (intern (concat prefix element)))
           (file (concat file-prefix element ".txt"))
           (cell (cons my-var file)))
      (set my-var nil)
      (push cell x)))
  (setq sync0-bibtex-completion-variables-alist x))

;; Create certain helper variables used for calculating various things
;; for all the variables that correspond to the objects in the var
;; sync0-bibtex-people-fields

(defvar sync0-bibtex-entry-helper-fields-list nil
  "Bibtex dummy fields used for calculation purposes of values for
a Bibtex entry.")

;; use this to define all the dummy fields required for creating
;; bibtex entries. these do not appear in the bibtex entry but are
;; used for calculating certain things that appear on notes or the
;; entry.
(let ((prefix "sync0-bibtex-entry-"))
  (dolist (element '("type-downcase" "crossref-entry" "title-fixed" "title-aliases" "editor-over-author" "title-compatible" "lastname" "related-tag" "doctype-fixed" "doctype-tag" "key" "medium-fixed" "project-fixed" "file-old" "country-tag"))
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var sync0-bibtex-entry-helper-fields-list))))

(defvar sync0-bibtex-entry-people-helper-fields-list nil
  "Bibtex dummy fields used for calculation purposes of values for
a Bibtex entry.")

(defvar sync0-bibtex-entry-date-helper-fields-list nil
  "Bibtex dummy fields used for calculation purposes of values for
a Bibtex entry.")

;; Define helper fields for people and date fields. 
(let ((prefix "sync0-bibtex-entry-")
      (suffix-fixed "-fixed") 
      (suffix-tag "-tag"))
  (dolist (element sync0-bibtex-people-fields)
    (let ((fixed-var (intern (concat prefix element suffix-fixed)))
          (tag-var (intern (concat prefix element suffix-tag))))
      (set fixed-var nil)
      (set tag-var nil)
      (push fixed-var sync0-bibtex-entry-helper-fields-list)
      (push fixed-var sync0-bibtex-entry-people-helper-fields-list)
      (push tag-var sync0-bibtex-entry-helper-fields-list)
      (push tag-var sync0-bibtex-entry-people-helper-fields-list)))
  (dolist (element sync0-bibtex-date-fields)
    (let ((fixed-var (intern (concat prefix element suffix-fixed)))
          (tag-var (intern (concat prefix element suffix-tag))))
      (set fixed-var nil)
      (set tag-var nil)
      (push fixed-var sync0-bibtex-entry-helper-fields-list)
      (push fixed-var sync0-bibtex-entry-date-helper-fields-list)
      (push tag-var sync0-bibtex-entry-helper-fields-list)
      (push tag-var sync0-bibtex-entry-date-helper-fields-list))))

;; (defvar sync0-alpha "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz"
;;   "Possible alphabetic characters that can appear in BibLaTeX keys.
;; Use format of base58 encoding.")


(defvar sync0-alpha "abcdefghijkmnpqrstuvwxyz"
  "Possible alphabetic characters that can appear in BibLaTeX keys.
Use format of base58 encoding.")

(defvar sync0-bibtex-keys nil
  "List of all the keys used in of all of the bibliography files")

(defvar sync0-bibtex-today-keys nil
  "Variable used to keep track of bibtex keys created today to
  prevent creating duplicated keys. Previously, I was using keys
  in the YYYYMMDDHHmmss format, but these were unnecessarily
  long; thus, I decided to simplify this format but only for
  those things related to bibtex files. Every day, I dispose of
  672 different keys.")

(defvar sync0-bibtex-archived-bibliography (concat sync0-bibtex-bibliobraphy-directory "archived.bib")
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-archive-directory "/home/sync0/Pictures/archives/"
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-keys-file "/home/sync0/.emacs.d/sync0-vars/bibtex-completion-key.txt"
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-keys-backup-file "/home/sync0/Gdrive/vars/bibtex-completion-key.txt"
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-tocs-directory "/home/sync0/Documents/tocs/"
  "Bibliography to store entries that are not needed at the moment for whatever reason.")

(defvar sync0-bibtex-author-separator "_"
  "Separating character used for distinguishing lastname from
  name. Beware, this variable should only bet set to
  non-alphabetic characters or any other character that could
  appear in names.")

(defvar sync0-bibtex-entry-author-or-editor-p nil
  "Variable to determine whether there is any person or institution
to fill this bibtex field. This variable is used for calculating
titles and the like.")

(defvar sync0-bibtex-entry-date-or-origdate-p nil
  "Variable to determine whether there is any person or institution
to fill this bibtex field. This variable is used for calculating
titles and the like.")

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

;; use this to define all the dummy fields required for creating
;; crossref entries
(let ((prefix "sync0-bibtex-entry-crossref-")
      x)
  (dolist (element sync0-bibtex-fields x)
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var x)))
  (setq sync0-bibtex-entry-crossref-definitions-list (nreverse x)))

;; Define dummy variables for initial fields
(let ((prefix "sync0-bibtex-entry-initial-")
      x)
  (dolist (element '("date" "origdate" "author" "language") x)
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var x)))
  (setq sync0-bibtex-entry-initial-fields-list x))

(setq sync0-bibtex-entry-crossref-crosscheck-fields-list '("subtitle" "volume" "edition" "shorttitle")) 

(defvar sync0-bibtex-obsidian-fields-list 
  '(("description" "\"" "\"" sync0-bibtex-entry-description)
    ("doctype" "[" "]" sync0-bibtex-entry-doctype-fixed)
    ("title" "\"" "\"" sync0-bibtex-entry-title)
    ("eventtitle" "\"" "\"" sync0-bibtex-entry-eventtitle)
    ("subtitle" "\"" "\"" sync0-bibtex-entry-subtitle)
    ("crossref" "" "" sync0-bibtex-entry-crossref)
    ("parent" "\"" "\"" sync0-bibtex-entry-parent)
    ("related" "[" "]" sync0-bibtex-entry-related)
    ("relatedtype" "" "" sync0-bibtex-entry-relatedtype)
    ("edition" "" "" sync0-bibtex-entry-edition)
    ("pagetotal" "" "" sync0-bibtex-entry-pagetotal)
    ("url" "\"" "\"" sync0-bibtex-entry-url)
    ("medium" "[" "]" sync0-bibtex-entry-medium-fixed)
    ("country" "[" "]" sync0-bibtex-entry-country)
    ("project" "[" "]" sync0-bibtex-entry-project-fixed)
    ("cote" "\"" "\"" sync0-bibtex-entry-cote)
    ("language" "" "" sync0-bibtex-entry-language)
    ("library" "[\"" "\"]" sync0-bibtex-entry-library))
  "Variable that commands how certain bibtex fields of an entry are
to appear in Obsidian markdown notes corresponding to it.")

;; Add author and date helper fields to variable
;; sync0-bibtex-obsidian-fields-list
(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element "-fixed")))
         (my-list-elem (list element "[" "]" my-var)))  
    (push my-list-elem sync0-bibtex-obsidian-fields-list)))
(dolist (element sync0-bibtex-date-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (my-list-elem (list element "" "" my-var)))  
    (push my-list-elem sync0-bibtex-obsidian-fields-list)))

(defvar sync0-bibtex-tag-fields-list
  '(("type" "reference/" sync0-bibtex-entry-type-downcase)
    ;; ("date" "" sync0-bibtex-entry-date-tag)
    ;; ("origdate" "" sync0-bibtex-entry-origdate-tag)
    ;; ("eventdate" "" sync0-bibtex-entry-eventdate-tag)
    ;; ("urldate" "" sync0-bibtex-entry-urldate-tag)
    ("crossref" "crossref/" sync0-bibtex-entry-crossref)
    ;; ("created" "" sync0-bibtex-entry-created-tag)
    ("doctype" "" sync0-bibtex-entry-doctype-tag)
    ("language" "language/" sync0-bibtex-entry-language)
    ("country" "country/" sync0-bibtex-entry-country-tag)
    ("edition" "edition/" sync0-bibtex-entry-edition)
    ("related" "related/" sync0-bibtex-entry-related-tag)
    ("project" "project/" sync0-bibtex-entry-project)
    ("relatedtype" "relatedtype/" sync0-bibtex-entry-relatedtype))
    ;; ("translator" "translator/" sync0-bibtex-entry-translator-tag)
    ;; ("introduction" "introduction/" sync0-bibtex-entry-introduction-tag)
    ;; ("recipient" "recipient/" sync0-bibtex-entry-recipient-tag)
    ;; ("people" "people/" sync0-bibtex-entry-people-tag)
    ;; ("editor" "editor/" sync0-bibtex-entry-editor-tag)
    ;; ("author" "author/" sync0-bibtex-entry-author-tag)
  "Variable that commands how certain bibtex fields of an entry are
to appear in tags of Obsidian markdown notes corresponding to
it.")

(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element "-tag")))
         (tag-prefix (concat element "/"))
         (my-list-elem (list element tag-prefix my-var)))  
    (push my-list-elem sync0-bibtex-tag-fields-list)))
(dolist (element sync0-bibtex-date-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element "-tag")))
         (my-list-elem (list element "" my-var)))  
    (push my-list-elem sync0-bibtex-tag-fields-list)))

(defvar sync0-bibtex-entry-extraction-fields-list 
  '(sync0-bibtex-entry-crossref-entry))

(defvar sync0-bibtex-entry-creation nil
  "Variable to define whether a field is being defined at entry creation or interactively from other contexts.")

(defvar sync0-bibtex-lastname-abbrev-string "et al."
  "String used to abbreviate long lists of lastnames")

(defvar sync0-bibtex-maximum-lastnames 2
  "Maximum no. of lastnames to appear in titles")

(setq sync0-bibtex-base-fields
      '("title"
        "subtitle"
        "date"
        "created"
        ;; "author"
        ;; "url"
        "language"
        "file"))
;; "keywords"

;; I decided to calculate keywords directly to prevent certain
;; problems that arise due to it needing some helper variables to be
;; calculated correctly

(setq sync0-bibtex-type-fields
      '(("Article" "author"
         "journaltitle"
         "volume"
         "number"
         ;; "series"
         "pages"
         ;; "doi"
         "url"
         "urldate")
        ("MvBook" "author"
         "volumes"
         ;; "origdate"
         ;; "edition"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("Book" "author"
         "publisher"
         ;; "origdate"
         ;; "edition"
         ;; "volume"
         ;; "series"
         "location")
         ;; "medium"
         ;; "library"
        ("MvCollection" "editor"
         ;; "origdate"
         ;; "edition"
         "volumes"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("Collection" "editor"
         ;; "origdate"
         ;; "edition"
         ;; "volume"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("InBook" "author"
         "crossref"
         ;; "origdate"
         ;; "edition"
         ;; "volume"
         ;; "series"
         ;; "publisher"
         ;; "location"
         "chapter"
         "booktitle"
         "booksubtitle"
         "pages")
         ;; "medium"
         ;; "library"
        ("InCollection" "author"
         "crossref"
         "editor"
         ;; "origdate"
         ;; "edition"
         ;; "volume"
         ;; "series"
         ;; "publisher"
         ;; "location"
         "chapter"
         "booktitle"
         "booksubtitle"
         "pages")
         ;; "medium"
         ;; "library"
        ("InProceedings" "author"
         "crossref"
         "editor"
         "eventtitle"
         "eventdate"
         "origdate"
         ;; "edition"
         ;; "volume"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "chapter"
         "booktitle"
         "booksubtitle"
         "pages")
         ;; "medium"
         ;; "library"
        ("Manual" "author"
         "institution"
         "origdate"
         "edition"
         "volume")
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "library"
         ;; "note"
        ("Online" "author"
         "institution"
         "url"
         "urldate")
         ;; "series"
         ;; "publisher"
         ;; "medium"
         ;; "library"
         ;; "note"
        ("Report" "author"
         "institution"
         ;; "volume"
         ;; "number"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "note"
         "library")
        ("Unpublished" "author"
         "doctype"
         "institution"
         ;; "volume"
         ;; "number"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "note"
         "library")
        ("Misc" "author"
         "doctype"
         "institution"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "library"
         "note")
        ("Thesis" "author"
         "institution"
         ;; "number"
         ;; "publisher"
         ;; "note"
         ;; "location"
         ;; "medium"
         "library")
        ("MvProceedings" "editor"
         "institution"
         "eventtitle"
         "eventdate"
         "volume"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "library"
         ;; "note"
         "number")
        ("Proceedings" "editor"
         "institution"
         "eventtitle"
         "eventdate"
         "volume"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "library"
         ;; "note"
         "number")))

(provide 'sync0-bibtex-vars)
