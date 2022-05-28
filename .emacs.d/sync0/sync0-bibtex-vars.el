;; -*- lexical-binding: t -*-

(defvar sync0-bibtex-fields
  '("title" "subtitle" "origtitle" "eventtitle" "date" "origdate" "eventdate" "author" "editor" "translator" "recipient" "introduction" "journaltitle" "edition" "booktitle" "booksubtitle" "crossref" "chapter" "volume" "volumes" "number" "series" "publisher" "location" "pages" "note" "doi" "url" "urldate" "language" "langid" "origlanguage" "medium" "institution" "library" "related" "relatedtype" "relatedstring" "file" "created" "password" "shorttitle" "doctype" "shorthand" "description" "keywords" "foreword" "afterword")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-full-fields
  '("title" "subtitle" "date" "origdate" "author" "editor" "journaltitle" "booktitle" "booksubtitle" "translator" "crossref"  "eventdate" "eventtitle" "venue" "volume" "number" "chapter" "edition" "pages" "publisher" "location" "pages" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-entry-types
  '("Article" "MvBook" "Book" "InBook" "InCollection" "MvCollection" "Collection" "Unpublished" "Thesis" "MvProceedings" "Proceedings" "InProceedings" "Online" "Report" "Manual" "Misc")
  "List of Bibtex entry types")

(defvar sync0-bibtex-crossref-types
  '("InBook" "InCollection" "InProceedings")
  "List of Bibtex entry types")

(defvar sync0-bibtex-quick-fields
  '("title" "subtitle" "date" "author" "editor" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-extract-fields
  '("title" "date" "author" "crossref" "pages" "language" "langid" "file" "keywords")
  "List of Bibtex entry fields")

;; Set completion vars
(let ((prefix "sync0-bibtex-completion-")
      x)
  (dolist (element '("publisher" "journaltitle" "location" "title" "author" "keywords" "note" "library" "series" "medium" "institution" "language" "doctype") x)
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var x)))
  (setq sync0-bibtex-completions-collection (nreverse x)))

(defvar sync0-bibtex-completion-variables-alist
  '((sync0-bibtex-completion-publisher . "~/.emacs.d/sync0-vars/bibtex-completion-publisher.txt")
    (sync0-bibtex-completion-journaltitle . "~/.emacs.d/sync0-vars/bibtex-completion-journaltitle.txt")
    (sync0-bibtex-completion-location . "~/.emacs.d/sync0-vars/bibtex-completion-location.txt")
    (sync0-bibtex-completion-title . "~/.emacs.d/sync0-vars/bibtex-completion-title.txt")
    (sync0-bibtex-completion-author .  "~/.emacs.d/sync0-vars/bibtex-completion-author.txt")
    (sync0-bibtex-completion-keywords .  "~/.emacs.d/sync0-vars/bibtex-completion-keywords.txt")
    (sync0-bibtex-completion-note .  "~/.emacs.d/sync0-vars/bibtex-completion-note.txt")
    (sync0-bibtex-completion-doctype .  "~/.emacs.d/sync0-vars/bibtex-completion-doctype.txt")
    (sync0-bibtex-completion-library .  "~/.emacs.d/sync0-vars/bibtex-completion-library.txt")
    (sync0-bibtex-completion-series .  "~/.emacs.d/sync0-vars/bibtex-completion-series.txt")
    (sync0-bibtex-completion-medium .  "~/.emacs.d/sync0-vars/bibtex-completion-medium.txt")
    (sync0-bibtex-completion-institution .  "~/.emacs.d/sync0-vars/bibtex-completion-institution.txt")
    (sync0-bibtex-completion-language .  "~/.emacs.d/sync0-vars/bibtex-completion-language.txt"))
  "Alist of variables used to define their initial values to be used in completion.")

(defvar sync0-alpha "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz"
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

(setq sync0-bibtex-obsidian-fields-list '(("description" "\"" "\"" sync0-bibtex-entry-description)
                                          ("doctype" "\"" "\"" sync0-bibtex-entry-doctype)
                                          ("author" "[" "]" sync0-bibtex-entry-author-fixed)
                                          ("editor" "[" "]" sync0-bibtex-entry-editor-fixed)
                                          ("translator" "[" "]" sync0-bibtex-entry-translator-fixed)
                                          ("introduction" "[" "]" sync0-bibtex-entry-introduction-fixed)
                                          ("recipient" "[" "]" sync0-bibtex-entry-recipient-fixed)
                                          ("title" "\"" "\"" sync0-bibtex-entry-title)
                                          ("subtitle" "\"" "\"" sync0-bibtex-entry-subtitle)
                                          ("crossref" "" "" sync0-bibtex-entry-crossref)
                                          ("parent" "\"" "\"" sync0-bibtex-entry-parent)
                                          ("related" "[" "]" sync0-bibtex-entry-related)
                                          ("relatedtype" "" "" sync0-bibtex-entry-relatedtype)
                                          ("edition" "" "" sync0-bibtex-entry-edition)
                                          ("url" "\"" "\"" sync0-bibtex-entry-url)
                                          ("origdate" "" "" sync0-bibtex-entry-origdate)
                                          ("date" "" "" sync0-bibtex-entry-date)
                                          ("medium" "[" "]" sync0-bibtex-entry-medium-fixed)
                                          ("language" "" "" sync0-bibtex-entry-language)
                                          ("library" "[\"" "\"]" sync0-bibtex-entry-library)))

(setq sync0-bibtex-tag-fields-list '(("type" "reference/" sync0-bibtex-entry-type-downcase)
                                     ("date" "" sync0-bibtex-entry-date-tag)
                                     ("origdate" "origdate/" sync0-bibtex-entry-origdate)
                                     ("crossref" "crossref/" sync0-bibtex-entry-crossref)
                                     ("created" "created/" sync0-bibtex-entry-created-fixed)
                                     ("doctype" "doctype/" sync0-bibtex-entry-doctype)
                                     ("language" "language/" sync0-bibtex-entry-language)
                                     ("edition" "edition/" sync0-bibtex-entry-edition)
                                     ("related" "" sync0-bibtex-entry-related-tag)
                                     ("relatedtype" "relatedtype/" sync0-bibtex-entry-relatedtype)
                                     ("translator" "translator/" sync0-bibtex-entry-translator-tag)
                                     ("introduction" "introduction/" sync0-bibtex-entry-introduction-tag)
                                     ("recipient" "recipient/" sync0-bibtex-entry-recipient-tag)
                                     ("editor" "editor/" sync0-bibtex-entry-editor-tag)
                                     ("author" "author/" sync0-bibtex-entry-author-tag)))

;; use this to define all the dummy fields required for creating
;; bibtex entries. these do not appear in the bibtex entry but are
;; used for calculating certain things that appear on notes or the
;; entry.
(let ((prefix "sync0-bibtex-entry-")
      x)
  (dolist (element '("type-downcase" "crossref-entry" "title-fixed" "title-aliases" "editor-over-author" "title-compatible" "date-tag" "date-fixed" "author-fixed" "translator-fixed" "introduction-fixed" "editor-fixed" "recipient-fixed" "lastname" "author-tag" "afterword-tag" "afterword-fixed" "foreword-tag" "foreword-fixed" "recipient-tag" "editor-tag" "translator-tag" "introduction-tag" "related-tag" "key" "medium-fixed" "created-fixed" "file-old") x)
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var x)))
  (setq sync0-bibtex-entry-helper-fields-list x))

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
        "author"
        ;; "url"
        "language"
        "file"))
;; "keywords"

;; I decided to calculate keywords directly to prevent certain
;; problems that arise due to it needing some helper variables to be
;; calculated correctly

(setq sync0-bibtex-type-fields
      '(("Article" "journaltitle"
         "volume"
         "number"
         ;; "series"
         "pages"
         ;; "doi"
         "url")
        ("MvBook" "origdate"
         ;; "edition"
         "volumes"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("Book" "origdate"
         "edition"
         ;; "volume"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("MvCollection" "editor"
         "origdate"
         ;; "edition"
         "volumes"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("Collection" "editor"
         "origdate"
         ;; "edition"
         ;; "volume"
         ;; "series"
         "publisher"
         "location")
         ;; "medium"
         ;; "library"
        ("InBook" "crossref"
         "origdate"
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
        ("InCollection" "crossref"
         "editor"
         "origdate"
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
        ("InProceedings" "crossref"
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
        ("Manual" "institution"
         "origdate"
         "edition"
         "volume")
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "library"
         ;; "note"
        ("Online" "institution"
         "url")
         ;; "series"
         ;; "publisher"
         ;; "medium"
         ;; "library"
         ;; "note"
        ("Report" "institution"
         ;; "volume"
         ;; "number"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "note"
         "library")
        ("Unpublished" "doctype"
         "institution"
         ;; "volume"
         ;; "number"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "note"
         "library")
        ("Misc" "doctype"
         "institution"
         ;; "series"
         ;; "publisher"
         ;; "location"
         ;; "medium"
         ;; "library"
         "note")
        ("Thesis" "institution"
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
