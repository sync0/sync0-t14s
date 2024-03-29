;; -*- lexical-binding: t -*-
(defvar sync0-bibtex-timeday
  (format-time-string "%y%-j")
  "Timeday for newly created bibkeys and such")

(defvar sync0-bibtex-default-key-length
  2 
  "Default length for biblatex keys")

(defvar sync0-bibtex-temp-pdf-copy-new-path-and-filename
  nil 
  "Default pdf copy path for pdfs to be copied")

(defvar sync0-bibtex-helper-pdf-copy-filepath
  "/home/sync0/Gdrive/goodreads/copy_list.txt"
  "Default text file name and path for pdfs to be copied")

(defvar sync0-bibtex-completion-variables-alist nil 
  "Dotlist of variables ")

(defvar sync0-bibtex-entry-fields-alist nil
  "Dotlist of bibtex fields with corresponding values for most
recently created entry. This variable is useless. It's used only
for diagnostics purposes only.")

(defvar sync0-bibtex-fields
  '("titleaddon" "title" "subtitle" "origtitle" "eventtitle" "date" "origdate" "eventdate" "author" "editor" "translator" "recipient" "introduction" "journaltitle" "edition" "booktitle" "booksubtitle" "crossref" "chapter" "volume" "volumes" "number" "series" "publisher" "location" "pages" "note" "doi" "url" "urldate" "language" "langid" "origlanguage" "medium" "institution" "library" "related" "relatedtype" "relatedstring" "file" "created" "password" "shorttitle" "doctype" "shorthand" "description" "keywords" "foreword" "afterword" "editortype" "pagetotal" "verba" "cote" "project" "site" "version" "people" "country" "lecture" "seminar" "theme" "currency" "value" "recommender" "podcast" "visibility" "source" "year" "status" "alive" "expages" "century" "aliases" "scanstatus" "issuetitle" "priority" "scheduled" "deadline" "supervisor" "lastseen" "seen" "amount" "revised" "mention" "format" "jury" "reviewer" "filetype" "extension" "mentioned" "bookloan")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-automatic-fields
  '("file" "created" "keywords" "year" "status" "century" "lastseen")
  "List of Bibtex entry fields that are set based on previous
information and that although can be set by the user, are usually
calculated automatically at entry for new biblatex entries.")

(defvar sync0-bibtex-unique-fields
  '("titleaddon" "title" "subtitle" "origtitle" "eventtitle" "date" "origdate" "eventdate" "journaltitle" "edition" "booktitle" "booksubtitle" "crossref" "chapter" "volume" "volumes" "number" "series" "publisher" "pages" "note" "doi" "url" "urldate" "language" "langid" "origlanguage" "institution" "library" "relatedtype" "relatedstring" "password" "shorttitle" "shorthand" "description" "editortype" "pagetotal" "verba" "cote" "site" "version" "lecture" "seminar" "currency" "value" "podcast" "visibility" "source" "year" "status" "alive" "expages" "century" "scanstatus" "issuetitle" "priority" "scheduled" "deadline" "format" "extension" "created" "bookloan")
  "List of Bibtex fields that only take one value. No multiple values allowed.")

(defvar sync0-bibtex-string-fields
  '("eventdate" "edition" "chapter" "volume" "volumes" "number" "expages" "pages" "doi" "password" "shorttitle" "shorthand" "description" "verba" "cote" "version" "url" "value" "aliases" "issuetitle" "amount")
  "List of Bibtex entry fields that use read-string without
accompanying completion variable for being defined. The lambda functions for their
definition are automatically calculated and added to the variable
sync0-bibtex-entry-functions.")

(defvar sync0-bibtex-string-multiple-fields
  '("medium" "doctype" "project" "country" "theme" "keywords" "seen" "mention" "filetype" "mentioned")
  "List of Bibtex entry fields that use completing-read-multiple
for being defined. The only exception is the field keywords
because it requires a special treatment.")

(defvar sync0-bibtex-date-fields
  '("date" "origdate" "eventdate" "urldate" "created" "year" "deadline" "scheduled" "lastseen" "revised")
  "List of Bibtex entry fields for dates. These functions are manually")

(defvar sync0-bibtex-people-fields
  '("author" "editor" "people" "recipient" "translator" "introduction" "foreword" "afterword" "recommender" "supervisor" "jury" "reviewer")
  "List of Bibtex entry fields")

;; (defvar sync0-bibtex-full-fields
;;   '("title" "subtitle" "date" "origdate" "author" "editor" "journaltitle" "booktitle" "booksubtitle" "translator" "crossref"  "eventdate" "eventtitle" "venue" "volume" "number" "chapter" "edition" "pages" "publisher" "location" "pages" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
;;   "List of Bibtex entry fields")

(defvar sync0-bibtex-entry-types
  '("Article" "MvBook" "Book" "InBook" "InCollection" "MvCollection" "Collection" "Unpublished" "Thesis" "MvProceedings" "Proceedings" "InProceedings" "Online" "Report" "Manual" "Misc")
  "List of Bibtex entry types")

(defvar sync0-bibtex-entry-types-correction '("mvbook" "inbook" "incollection" "mvcollection" "mvproceedings" "inproceedings")
  "Entry types used to correct case problems when extracting entries from Obsidian into BibLaTex.")

(defvar sync0-bibtex-crossref-types
  '("InBook" "InCollection" "InProceedings")
  "List of Bibtex entry types that use the crossref field")

(defvar sync0-bibtex-entry-editor-types
  '("MvCollection" "Collection" "InCollection" "InProceedings" "Proceedings")
  "List of Bibtex entry types that use the editor field")

(defvar sync0-bibtex-attachment-programs
  '("zathura" "okular" "gwenview" "nil" "vlc")
  "List of software to open attachments")

;; (defvar sync0-bibtex-quick-fields
;;   '("title" "subtitle" "date" "author" "editor" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
;;   "List of Bibtex entry fields")

;; (setq sync0-bibtex-obsidian-reference-template-top
;;       (concat  "\n## Description\n\n" 
;;                "## Progrès de la lecture\n\n"
;;                "## Notes\n\n"
;;                "## Annotations\n\n"
;;                "```dataview\n"
;;                "TABLE WITHOUT ID\n"
;;                "link(file.name, title) AS \"Title\", created AS \"Created\"\n"
;;                ;; "title AS \"Titre\"\n"
;;                "FROM #permanent AND #bibkey/"))

;; (setq sync0-bibtex-obsidian-reference-template-bottom
;;       (concat "\n"
;;               "SORT created DESC\n"
;;               "```\n\n"
;;               "## Relations\n\n"))

(setq sync0-bibtex-obsidian-reference-template-top
      (concat  "\n## Description {.noexport}\n\n" 
               "\n### Motivation\n\n" 
               "\n### Aperçu\n\n" 
               "### Progrès de la lecture\n\n"
               "### Annotations\n\n"
               "```dataview\n"
               "TABLE WITHOUT ID\n"
               "link(file.name, title) AS \"Title\", created AS \"Created\"\n"
               "FROM #permanent AND #bibkey/"))

(setq sync0-bibtex-obsidian-reference-template-bottom
      (concat "\n"
              "SORT created DESC\n"
              "```\n\n"
              "### Relations\n\n"
              "### Index\n\n"
              "## Notes\n\n"))

(setq sync0-bibtex-quick-fields
      '("title"
        "subtitle"
        "date"
        "year"
        "century"
        "created"
        "status"
        ;; "author"
        ;; "url"
        "file"))

(defvar sync0-bibtex-extract-fields
  '("title" "date" "author" "crossref" "pages" "language" "langid" "file" "keywords")
  "List of Bibtex entry fields")

(defvar sync0-bibtex-completion-single-fields
  '("publisher" "journaltitle" "location" "titleaddon" "title" "eventtitle" "note" "library" "series" "institution" "language" "site" "relatedtype" "editortype" "lecture" "seminar" "podcast" "visibility" "source" "status" "alive" "scanstatus" "priority" "currency" "format" "extension" "bookloan")
   "List of biblatex fields that are set with the completing-read
function---as opposed to those defined with
completing-read-multiple, which appear in
sync0-bibtex-string-multiple-fields.")

(defvar sync0-bibtex-completion-fields
  (append sync0-bibtex-completion-single-fields
          sync0-bibtex-string-multiple-fields
          (list "author"))
  "List of Bibtex entry completion fields. This variable does not
hold all the bibtlatex fields that take completion, but rather
those variables that should provide the template for other
similar biblatex fields that may employ the same base completion
file in the sync0-vars directory. This varialbe is important for
the creation of sync0-bibtex-completion-variables-alist, which is
used to set up the initial values of the completion vars to be
used by my biblatex functions.")

;; Set completion vars and create an alist of variables used to define
;; their initial values to be used in completion.
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

  ;; (setq sync0-bibtex-completions-collection (nreverse x))

(defvar sync0-bibtex-completion-variables-list nil
  "List of variables used for updating completion files.")

;; Configure people variables and add them to var
;; sync0-bibtex-completion-variables-list
(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (comp-file (concat sync0-vars-dir "bibtex-completion-author.txt"))
         (my-list-elem (list element my-var 'sync0-bibtex-completion-author comp-file)))  
    (unless (file-exists-p comp-file)
      (make-empty-file comp-file))
    (push my-list-elem sync0-bibtex-completion-variables-list)))

(let* ((exceptions (list "author"))
       (x (cl-set-difference sync0-bibtex-completion-fields exceptions :test 'string=)))
  ;; Remove languages to prevent overwriting the function it currently has
  (dolist (element x)
    (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
           (comp-var (intern (concat "sync0-bibtex-completion-" element)))
           (comp-file (concat sync0-vars-dir "bibtex-completion-" element ".txt"))
           (my-list-elem (list element my-var comp-var comp-file)))  
      (unless (file-exists-p comp-file)
        (make-empty-file comp-file))
      (push my-list-elem sync0-bibtex-completion-variables-list))))

;; Check existance of completion files and create those that are
;; missing.
;; CREATE THIS FUNC

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
(let* ((prefix "sync0-bibtex-entry-")
       (no-key-list (remove "keywords" sync0-bibtex-string-multiple-fields))
       (mult-var-tag (mapcar (lambda (x) (concat x "-tag")) no-key-list))
       (mult-var-fix (mapcar (lambda (x) (concat x "-fixed")) no-key-list))
       (raw-list (list "type-downcase" "crossref-entry" "title-fixed" "title-aliases" "editor-over-author" "title-compatible" "lastname" "related-tag" "key" "file-old" "journaltitle-tag" "library-tag"))
       (full-list (append mult-var-tag mult-var-fix raw-list)))
  (dolist (element full-list) 
    (let ((my-var (intern (concat prefix element))))
      (set my-var nil)
      (push my-var sync0-bibtex-entry-helper-fields-list))))

;; Define helper fields for people and date fields. 
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

(defvar sync0-bibtex-today-keys-file
  "/home/sync0/.emacs.d/sync0-vars/sync0-bibtex-today-keys.txt"
  "File to store today's unique strings.")

(defun sync0-bibtex-load-today-keys ()
  "Load today's unique strings from file."
  (when (file-readable-p sync0-bibtex-today-keys-file)
    (with-temp-buffer
      (insert-file-contents sync0-bibtex-today-keys-file)
      (setq sync0-bibtex-today-keys (split-string (buffer-string) "\n" t)))))

(defun sync0-bibtex-save-today-keys ()
  "Save today's unique strings to file."
  (with-temp-file sync0-bibtex-today-keys-file
    (insert (mapconcat 'identity sync0-bibtex-today-keys "\n"))))

(defun sync0-bibtex-clear-today-keys ()
  "Clear the contents of sync0-bibtex-today-keys.txt, removing entries not created today."
  (with-temp-buffer
    (insert-file-contents sync0-bibtex-today-keys-file)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line-start (line-beginning-position)))
        (if (looking-at-p (concat "^" sync0-bibtex-timeday))
            (forward-line)
          (delete-region line-start (1+ (line-end-position))))))
    (write-region (point-min) (point-max) sync0-bibtex-today-keys-file nil 'no-message)))

(sync0-bibtex-load-today-keys)
(sync0-bibtex-clear-today-keys)

;; Your function to add unique strings to sync0-bibtex-today-keys goes here

;; Call save-sync0-bibtex-today-keys when Emacs is about to exit
(add-hook 'kill-emacs-hook #'sync0-bibtex-save-today-keys)

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
    ("author" "[" "]" sync0-bibtex-entry-author-fixed)
    ("editor" "[" "]" sync0-bibtex-entry-editor-fixed)
    ("seen" "[" "]" sync0-bibtex-entry-seen)
    ("people" "[" "]" sync0-bibtex-entry-people-fixed)
    ("title" "\"" "\"" sync0-bibtex-entry-title)
    ("eventtitle" "\"" "\"" sync0-bibtex-entry-eventtitle)
    ("date" "" "" sync0-bibtex-entry-date)
    ("bookloan" "" "" sync0-bibtex-entry-bookloan)
    ("amount" "" "" sync0-bibtex-entry-amount)
    ("currency" "" "" sync0-bibtex-entry-currency)
    ("lastseen" "" "" sync0-bibtex-entry-lastseen)
    ("scheduled" "" "" sync0-bibtex-entry-scheduled)
    ("deadline" "" "" sync0-bibtex-entry-deadline)
    ("year" "" "" sync0-bibtex-entry-year)
    ("priority" "" "" sync0-bibtex-entry-priority)
    ("status" "" "" sync0-bibtex-entry-status)
    ("scanstatus" "" "" sync0-bibtex-entry-scanstatus)
    ("century" "" "" sync0-bibtex-entry-century)
    ("origdate" "" "" sync0-bibtex-entry-origdate)
    ("eventtitle" "\"" "\"" sync0-bibtex-entry-eventtitle)
    ("location" "\"" "\"" sync0-bibtex-entry-location)
    ("subtitle" "\"" "\"" sync0-bibtex-entry-subtitle)
    ("crossref" "" "" sync0-bibtex-entry-crossref)
    ("parent" "\"" "\"" sync0-bibtex-entry-parent)
    ("journaltitle" "\"" "\"" sync0-bibtex-entry-journaltitle)
    ("volume" "" "" sync0-bibtex-entry-volume)
    ("number" "" "" sync0-bibtex-entry-number)
    ("library" "\"" "\"" sync0-bibtex-entry-library)
    ("related" "[" "]" sync0-bibtex-entry-related)
    ("relatedtype" "" "" sync0-bibtex-entry-relatedtype)
    ("pages" "" "" sync0-bibtex-entry-pages)
    ("edition" "" "" sync0-bibtex-entry-edition)
    ("pagetotal" "" "" sync0-bibtex-entry-pagetotal)
    ("url" "\"" "\"" sync0-bibtex-entry-url)
    ("mention" "[" "]" sync0-bibtex-entry-mention)
    ("medium" "[" "]" sync0-bibtex-entry-medium-fixed)
    ;; ("country" "[" "]" sync0-bibtex-entry-country-fixed)
    ("project" "[" "]" sync0-bibtex-entry-project-fixed)
    ("cote" "\"" "\"" sync0-bibtex-entry-cote)
    ("language" "" "" sync0-bibtex-entry-language))
  "Variable that commands how certain bibtex fields of an entry are
to appear in Obsidian markdown notes corresponding to it. It is
very to set this variable explicitly to avoid mishandlings by
Obsidian of produced markdown (corrupt YAML frontmatters).")

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
    ("journaltitle" "journaltitle/" sync0-bibtex-entry-journaltitle-tag)
;;    ("publication" "publication/" sync0-bibtex-entry-publication)
    ("crossref" "crossref/" sync0-bibtex-entry-crossref)
    ("doctype" "" sync0-bibtex-entry-doctype-tag)
    ("visibility" "visibility/" sync0-bibtex-entry-visibility)
    ("source" "century/" sync0-bibtex-entry-century)
    ("century" "source/" sync0-bibtex-entry-source)
    ("bookloan" "bookloan/" sync0-bibtex-entry-bookloan)
    ("theme" "" sync0-bibtex-entry-theme-tag)
    ("status" "status/" sync0-bibtex-entry-status)
    ("scanstatus" "scanstatus/" sync0-bibtex-entry-scanstatus)
    ("priority" "priority/" sync0-bibtex-entry-priority)
    ("country" "" sync0-bibtex-entry-country-tag)
    ("mention" "" sync0-bibtex-entry-mention-tag)
    ("mentioned" "" sync0-bibtex-entry-mentioned-tag)
    ("seen" "" sync0-bibtex-entry-seen-tag)
    ("library" "library/" sync0-bibtex-entry-library-tag)
    ("language" "language/" sync0-bibtex-entry-language)
    ("edition" "edition/" sync0-bibtex-entry-edition)
    ("related" "related/" sync0-bibtex-entry-related-tag)
    ("project" "" sync0-bibtex-entry-project-tag)
    ("relatedtype" "relatedtype/" sync0-bibtex-entry-relatedtype))
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
        "year"
        "century"
        "created"
        ;; "author"
        ;; "url"
        "language"
        "theme"
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
         "pagetotal"
         ;; "doi"
         ;; "urldate"
         "url")
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
         "pages"
         "pagetotal")
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
         "pages"
         "pagetotal")
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
         "pages"
         "pagetotal")
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
