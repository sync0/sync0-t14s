;; -*- lexical-binding: t -*-

;; (defmacro defvar* (vars initial-value)
;;  `(progn
;;     ,@(loop for var in vars
;;             do (check-type var symbol)
;;             collect `(defvar ,var ,initial-value))))

(require 'unidecode)
(require 'bibtex-completion)
(require 'sync0-print)
(require 'sync0-pdf)
(require 'sync0-yaml)
(require 'xah-replace-pairs)

(sync0-set-variable-from-files sync0-bibtex-completion-variables-alist)

(with-temp-buffer
  (insert-file-contents sync0-bibtex-keys-file)
  (goto-char (point-min))
  (let (x)
    ;; (keep-lines "contexts" (point-min) (point-max)) 
    (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
      (push (match-string-no-properties 1) x))
    (setq sync0-bibtex-keys x)))

(defun sync0-bibtex-populate-keys ()
  "Load all bibtex keys on variable sync0-bibtex-keys."
  (interactive)
  ;; Load variable sync0-bibtex-keys extracting the keys from the
  ;; bibtex-completion-candidates
  (setq sync0-bibtex-keys (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                                  (bibtex-completion-candidates)))
  ;; Write contents of sync0-bitex-keys to file
  (with-temp-file sync0-bibtex-keys-file
    (sync0-insert-elements-of-list sync0-bibtex-keys))
  ;; Rewrite file in Gdrive to allow for manipulation in iOS
  (copy-file sync0-bibtex-keys-file sync0-bibtex-keys-backup-file t))

(defun sync0-random-alnum ()
  "From a defined alphabet variable (sync0-alpha), take one
character at random."
  (let* ((i (% (abs (random)) (length sync0-alpha))))
    (substring sync0-alpha i (1+ i))))

(defun sync0-bibtex-buffer-p ()
  "Check whether current buffer is visiting a bibtex file."
  (string= (file-name-extension (buffer-file-name)) "bib"))

(defun sync0-bibtex-duplicate-entry-key-p (key &optional daily)
  (if daily
      (member key sync0-bibtex-today-keys)
    (progn 
      (sync0-bibtex-populate-keys)
      (member key sync0-bibtex-keys))))

(defun sync0-bibtex-add-key-to-keyvars (key) 
  (setq sync0-bibtex-keys (cons key sync0-bibtex-keys))
  (setq sync0-bibtex-today-keys (cons key sync0-bibtex-today-keys)))

(defun sync0-bibtex-entry-key-define (&optional daily)
  "Create new bibtex key following a pre-defined rule. In this
case, keys are outputed in the YYMMDDxx format, in which the last
two characters are produced using the sync0-random-alnum function
to produce random characters."
  (let ((x (concat (format-time-string "%y%j")
                   (sync0-random-alnum)
                   (sync0-random-alnum))))
    (if (sync0-bibtex-duplicate-entry-key-p x daily)
        ;; Call recursively the function again
            (sync0-bibtex-entry-key-define daily)
      (progn 
        (sync0-bibtex-add-key-to-keyvars x)
        (format "%s" x)))))

(defun sync0-bibtex-entry-define-keys-list (times) 
  "Create as many new keys as requested. They are put on a list"
  (let (values)
    (dotimes (i times)
      (push (sync0-bibtex-entry-key-define) values))
    values))

(defun sync0-bibtex-abbreviate-lastnames (string)
  "For very long lists of last names, cut it up with a chosen
abbreviation defined in sync0-bibtex-lastname-abbrev-string. This
function does not take lists but strings as argument, which is
less efficient but less confusing as well, due to the fact that
bibtex does not take lists but strings as arguments."
  (let ((x  (1+ (how-many-str ", " string)))
        (appearance (string-match ", " string)))
    ;; Change the string according to the maximum number of lastnames
    ;; defined
    (if (> x sync0-bibtex-maximum-lastnames)
      (concat (substring string 0 appearance) " "
              sync0-bibtex-lastname-abbrev-string)
      string)))

(defmacro sync0-bibtex-quotify-var (var)
  "Put the values of var with quotes."
  `(unless (sync0-null-p ,var)
     ;; first remove the sync0-bibtex-entry- prefix to use later
     (set (intern (concat ,(symbol-name var) "-fixed"))
          (if (string-match ", " ,var)
              ;; create a list with parts 
              (let* ((name-parts  (split-string ,var ", "))
                     (name-string (let (x)
                                    (dolist  (element name-parts x)
                                      (setq x (concat x element "\", \""))))))
                (concat "\"" (substring name-string 0 -3)))
            ;; other cases
            (concat "\"" ,var "\"")))
     (set (intern (concat ,(symbol-name var) "-tag"))
          (let* ((var-end (when (string-match "sync0-bibtex-entry-\\([[:alnum:]]+\\)" ,(symbol-name var))
                            (match-string-no-properties 1 ,(symbol-name var))))
                 (tag-prefix (concat var-end "/")))
            (if (string-match ", " ,var)
                (sync0-add-prefix-to-list-convert-to-string ,var ", " tag-prefix)
              (concat tag-prefix ,var))))))

(defmacro sync0-bibtex-fix-names (var)
  "Put the names of var in the right order for calculation of
different things. The order is 'Last name, First name'. Works for
variables that contain author names. It sets a variable of the
form sync0-bibtex-entry-author-fixed to allow other functions to
access it."
  `(unless (sync0-null-p ,var)
     ;; first remove the sync0-bibtex-entry- prefix to use later
     (set (intern (concat ,(symbol-name var) "-fixed"))
          (cond ((string-match " and " ,var)
                 ;; create a list with parts 
                 (let* ((name-parts  (split-string ,var " and "))
                        (name-string (let (x)
                                       (dolist  (element name-parts x)
                                         (setq x (concat x element "\", \""))))))
                   (concat "\"" (substring name-string 0 -3))))
                ;; check when author is an organization
                ((string-match "^{" ,var)
                 (concat "\"" (substring ,var 1 -1) "\""))
                ;; other cases
                (t (concat "\"" ,var "\""))))
     (set (intern (concat ,(symbol-name var) "-lastname"))
          (cond ((string-match " and " ,var)
                 ;; create a list with parts 
                 (let* ((author-list  (split-string ,var " and "))
                        (last-names (let (x)
                                      (dolist  (element author-list x)
                                        (setq x (concat x
                                                        (progn
                                                          (string-match "\\([[:print:]]+\\),"   element)
                                                          (match-string 1 element))
                                                        ", "))))))
                   (substring last-names 0 -2)))
                ((string-match "^{" ,var)
                 (string-match "{\\([[:print:]]+\\)}" ,var)
                 (match-string 1 ,var))
                (t (nth 0 (split-string ,var ", ")))))
     (set (intern (concat ,(symbol-name var) "-tag"))
          (cond ((string-match " and " ,var)
                 (let* ((no-comma (replace-regexp-in-string ", " "_" (downcase ,var)))
                        (person (substring  ,(symbol-name var) 19))
                        (person-string (concat ", " person "/"))
                        (no-space (replace-regexp-in-string "[[:space:]]+" "-" no-comma)))
                   (replace-regexp-in-string "-and-" person-string no-space)))
                ((string-match "^{" ,var)
                 (string-match "{\\([[:print:]]+\\)}" ,var)
                 (downcase (match-string 1 ,var)))
                (t (let ((raw (replace-regexp-in-string ", " "_" ,var)))
                     (downcase (replace-regexp-in-string " " "-" raw))))))))

(defmacro sync0-bibtex-normalize-name-string (var completion-string)
  "Get values from completing-read-multiple and organize them into
a string that can be understood to paste as a value in a BibLaTeX file."
  `(let* ((x)
          (crm-separator  "[ 	]*;[ 	]*")
          (initial-input  (completing-read-multiple ,completion-string
                                                    sync0-bibtex-completion-author nil nil sync0-bibtex-entry-initial-author)))
     ;; (set (intern (concat ,(symbol-name var) "-length")) (length initial-input))
     (setq x (sync0-show-elements-of-list initial-input " and "))
     (when (string-match sync0-bibtex-author-separator x)
       (setq x
             (replace-regexp-in-string sync0-bibtex-author-separator ", " x)))
     (when (string-match sync0-bibtex-author-lastname-separator x)
       (setq x
             (replace-regexp-in-string sync0-bibtex-author-lastname-separator " " x)))
     (set (intern ,(symbol-name var)) x)))

(defun sync0-bibtex-normalize-case (entrytype)
  "Normalize case of bibtex type to prevent crazy unwanted
results. This is done to make the bibtex files be in accord with
the format of Jabref and other software that uses the newer
biblatex standard instead of bibtex."
  (if (or (string= entrytype "inbook")
          (string= entrytype "incollection") 
          (string= entrytype "inproceedings")
          (string= entrytype "mvbook")
          (string= entrytype "mvcollection")
          (string= entrytype "mvproceedings"))
      (let ((one (upcase-initials (substring entrytype 0 2)))
            (two (upcase-initials (substring entrytype 2 nil))))
        (concat one two))
    (upcase-initials entrytype)))

;; (defun sync0-bibtex-add-missing-braces (string)
;;   "Function to correct an error in bibtex-completion-get-value from
;; bibtex-completion package, in which the latex forms \textbf{...}
;; are retrieved without the last matching brace, which causes a lot
;; of crazy problems"
;;   (if (and (string-match "\\textit{[[:graph:] ]+}" string)
;;            (not (string-match "\\textit{[[:graph:] ]+[^}]" string)))
;;       string
;;     (if (string-match "\\textit{[[:graph:] ]+[^}]$" string)
;;         (concat string "}")
;;       string)))

;; (defun sync0-bibtex-add-missing-braces (string)
;;   "Function to correct an error in bibtex-completion-get-value from
;; bibtex-completion package, in which the latex forms \textbf{...}
;; are retrieved without the last matching brace, which causes a lot
;; of crazy problems"
;;   (if (and (string-match "\\textit{[[:graph:] ]+}" string)
;;            (not (string-match "\\textit{[[:graph:] ]+[^}]" string)))
;;       string
;;     (concat string "}")))

(defun sync0-bibtex-add-missing-braces (string)
  "Function to correct an error in bibtex-completion-get-value from
bibtex-completion package, in which the latex forms \textbf{...}
are retrieved without the last matching brace, which causes a lot
of crazy problems"
  (if (string-match "\\textit{[[:graph:] ]+[^}]" string)
      (concat string "}")
    string))

;; (defun sync0-bibtex-fix-obsidian-chars (arg)
;;   "Remove or replace characters that cause trouble in obsdian
;; markdown."
;;   (unless (sync0-null-p arg)
;;     (setq arg 
;;           (xah-replace-regexp-pairs-in-string
;;            (sync0-bibtex-add-missing-braces arg)
;;            [["\\textit{\\([[:graph:] ]+\\)}" "*\\1*"]
;;             ["\textit{\\([[:graph:] ]+\\)}" "*\\1*"]]))
;;     (xah-replace-pairs-in-string
;;      arg
;;      [["\\*" "*"]])))

;; (defun sync0-bibtex-fix-obsidian-chars (arg)
;;   "Remove or replace characters that cause trouble in obsdian
;; markdown."
;;   (unless (sync0-null-p arg)
;;     (setq arg 
;;           (xah-replace-pairs-in-string-recursive
;;            (sync0-bibtex-add-missing-braces arg)
;;            [["\\textit{" "*"]
;;             ["}" "*"]]))
;;     (xah-replace-pairs-in-string-recursive
;;      arg
;;      [["\\*" "*"]])))

(defun sync0-bibtex-fix-obsidian-chars (arg)
  "Remove or replace characters that cause trouble in obsdian
markdown."
  (unless (sync0-null-p arg)
    (xah-replace-pairs-in-string-recursive
     (sync0-bibtex-add-missing-braces arg)
     [["\\textit{" "*"]
     ["\\&" "&"]
      ["}" "*"]])))

(defvar sync0-bibtex-entry-functions
      '(("title" (lambda ()
                   (setq sync0-bibtex-entry-title
                         (completing-read "Title of BibLaTeX entry: " sync0-bibtex-completion-title))))
        ("origtitle" (lambda ()
                       (setq sync0-bibtex-entry-origtitle
                             (completing-read "Origtitle of entry: " sync0-bibtex-completion-title))))
        ("date" (lambda ()
                  (setq sync0-bibtex-entry-date
                        (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-date))))
        ("created" (lambda ()
                     (setq sync0-bibtex-entry-created-tag
                           (format-time-string "%Y/%m/%d"))
                     (setq sync0-bibtex-entry-created
                           (format-time-string "%Y-%m-%d"))))
        ("origdate" (lambda ()
                      (setq sync0-bibtex-entry-origdate
                            (read-string "Origdate (ex. 1890-18-12) : " sync0-bibtex-entry-initial-origdate))))
        ("editortype" (lambda ()
                         (setq sync0-bibtex-entry-editortype
                               (completing-read "Type d'éditeur : "
                                                '("editor" "compiler" "founder" "continuator" "redactor" "reviser" "collaborator" "organizer")))))
        ("series" (lambda ()
                    (setq sync0-bibtex-entry-series
                          (completing-read "Series : " sync0-bibtex-completion-series))))
        ("publisher" (lambda ()
                       (setq sync0-bibtex-entry-publisher
                             (completing-read "Maison d'edition : " sync0-bibtex-completion-publisher))))
        ("url" (lambda ()
                 (unless sync0-bibtex-entry-urldate
                   (setq sync0-bibtex-entry-urldate 
                           (format-time-string "%Y-%m-%d")))
                 (setq sync0-bibtex-entry-url
                       (read-string "Url : " nil nil nil t))))
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
        ("library" (lambda ()
                     (setq sync0-bibtex-entry-library
                           (completing-read "Choose location to trace back: "
                                            sync0-bibtex-completion-library nil nil nil))))
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
        ("journaltitle" (lambda ()
                          (setq sync0-bibtex-entry-journaltitle
                                (completing-read "Titre du journal : " sync0-bibtex-completion-journaltitle))))
        ("site" (lambda ()
                      (setq sync0-bibtex-entry-site
                            (completing-read "Site : " sync0-bibtex-completion-site))))
        ("location" (lambda ()
                      (setq sync0-bibtex-entry-location
                            (completing-read "Location : " sync0-bibtex-completion-location))))
        ("note" (lambda ()
                  (setq sync0-bibtex-entry-note
                        (completing-read "Quelle note ou addenda ? : "
                                         sync0-bibtex-completion-note))))
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
        ("institution" (lambda ()
                         (setq sync0-bibtex-entry-institution
                               (completing-read "Institution : " sync0-bibtex-completion-institution))))
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
        ("relatedtype" (lambda ()
                         (setq sync0-bibtex-entry-relatedtype
                               (completing-read "Type de relation : "
                                                '("multivolume" "origpubas" "reviewof" "reprintof" "reprintas" "reprintfrom" "translationas" "translationfrom" "translationof")))))
        ("keywords" (lambda ()
                      ;; Requires package unidecode for conversion to
                      ;; ASCII. See:
                      ;; https://github.com/sindikat/unidecode
                      (setq sync0-bibtex-entry-keywords
                            (unidecode
                             (let* ((actual-fields-list
                                     (let (x)
                                       (dolist (element sync0-bibtex-tag-fields-list x) 
                                         (unless (sync0-null-p (symbol-value (caddr element)))
                                           (setq x (cons (concat (cadr element)  (eval (caddr element))) x))))))
                                    ;; (push (concat (cadr element)  (eval (caddr element))) x)
                                    (extra-keywords (completing-read-multiple "Input extra keywords: " 
                                                                              sync0-bibtex-completion-keywords))
                                    (master-list (if (sync0-null-p extra-keywords)
                                                     actual-fields-list
                                                   (cl-union actual-fields-list extra-keywords))))
                               (sync0-show-elements-of-list master-list ", ")))))))
      "List of lists in which the car of each list is bibtex-field and
the cdr is a lambda function with all the actions that should be
carried to calculate the value it will take in a BibLaTeX entry.")

(dolist (element sync0-bibtex-string-multiple-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (comp-var (intern (concat "sync0-bibtex-completion-" element)))
         (help-string (concat (upcase-initials element) ": "))
         (my-base-func  (list  'completing-read-multiple help-string comp-var)) 
         (my-list-string (list 'sync0-show-elements-of-list my-base-func ", "))
         (my-macro  (list 'setq my-var  my-list-string)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))

(dolist (element sync0-bibtex-string-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (help-string (concat (upcase-initials element) ": "))
         (my-base-func  (list  'read-string help-string nil nil nil t)) 
         (my-macro  (list 'setq my-var  my-base-func)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))

(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (help-string (concat (upcase-initials element) "(s): "))
         (my-macro  (list  'sync0-bibtex-normalize-name-string my-var help-string)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))


(defun sync0-bibtex-correct-crossref-fields ()
  "Correct crossref fields to prevent errors generated by the way
bibtex-completion handles crossreferences."
  (setq sync0-bibtex-entry-crossref-entry
        (bibtex-completion-get-entry sync0-bibtex-entry-crossref))
  ;; when crossref is present, load crossref fields
  (let (x)
    (dolist (element sync0-bibtex-fields x)
      (let ((value (bibtex-completion-get-value element sync0-bibtex-entry-crossref-entry))
            (var (concat "sync0-bibtex-entry-crossref-" element)))
        ;; set the variable to the value
        (set (intern var) value)
        ;; create cons of the form  ("sync0-bibtex-entry-title" . "The good old days") for an alist
        (push (cons var value) x)
        ;; added this last function to see if I can get completion to work
        (sync0-bibtex-update-var element)))
    (setq sync0-bibtex-entry-crossref-fields-alist x))
  ;; Check and fix problems with crossref-fields
  (dolist (element sync0-bibtex-entry-crossref-crosscheck-fields-list)
    (let* ((entry-element (concat "sync0-bibtex-entry-" element))
           (entry-cons (assoc entry-element sync0-bibtex-entry-fields-alist))
           (entry-value (cdr entry-cons))
           (crossref-element (concat "sync0-bibtex-entry-crossref-" element))
           (crossref-cons (assoc crossref-element sync0-bibtex-entry-crossref-fields-alist))
           (crossref-value (cdr crossref-cons)))
      (when (string= entry-value crossref-value)
        (set (intern entry-element) nil)))))

(defun sync0-bibtex-set-date-extra-fields ()
  "Calculate date fields to be used in titles and obsidian notes.
Set fields that do not appear on the biblatex entry as such (they
are not present in the .bib file), but that are necessary for
other functions."
  ;; this function needs correction to deal with composite dates of
  ;; the type 2022/2933
  (cond  ((and (sync0-null-p sync0-bibtex-entry-origdate)
               (sync0-null-p sync0-bibtex-entry-eventdate)
               (sync0-null-p sync0-bibtex-entry-date))
          (setq sync0-bibtex-entry-date-or-origdate-p nil))
         ((equal (or sync0-bibtex-entry-origdate
                     sync0-bibtex-entry-eventdate)
                 sync0-bibtex-entry-date)
          (setq sync0-bibtex-entry-date-or-origdate-p "equal"))
         ((and (sync0-null-p sync0-bibtex-entry-origdate)
               (sync0-null-p sync0-bibtex-entry-eventdate)
               sync0-bibtex-entry-date)
          (setq sync0-bibtex-entry-date-or-origdate-p "date"))
         ((and (sync0-null-p sync0-bibtex-entry-origdate)
               sync0-bibtex-entry-eventdate
               sync0-bibtex-entry-date)
          (setq sync0-bibtex-entry-date-or-origdate-p "eventdate"))
         ((and (sync0-null-p sync0-bibtex-entry-origdate)
               (sync0-null-p sync0-bibtex-entry-date)
               sync0-bibtex-entry-eventdate)
          (setq sync0-bibtex-entry-date-or-origdate-p "eventdate-nodate"))
         ((and (sync0-null-p sync0-bibtex-entry-date)
               (sync0-null-p sync0-bibtex-entry-eventdate)
               sync0-bibtex-entry-origdate)
          (setq sync0-bibtex-entry-date-or-origdate-p "origdate-nodate"))
         (t (setq sync0-bibtex-entry-date-or-origdate-p "origdate")))
  ;; calculate tag date fields for use in keywords
  (dolist (element sync0-bibtex-date-fields)
    (when-let* ((var (intern (concat "sync0-bibtex-entry-" element)))
                (value (symbol-value var))
                (var-tag (intern (concat "sync0-bibtex-entry-" element "-tag")))
                (dates (unless (sync0-null-p value) 
                         (sync0-string-split-with-sep-and-list value "/"))))
      (set var-tag (let (x)
                     (dolist (date dates x)
                       (push (concat element "/" (replace-regexp-in-string "-" "/" date)) x))
                     (sync0-show-elements-of-list x ", ")))))
  (setq sync0-bibtex-entry-date-fixed
        (cond ((and (null sync0-bibtex-entry-author-or-editor-p)
                    (null sync0-bibtex-entry-date-or-origdate-p))
               " ")
              ((and sync0-bibtex-entry-author-or-editor-p
                    (null sync0-bibtex-entry-date-or-origdate-p))
               ", ")
              ((or (equal sync0-bibtex-entry-date-or-origdate-p "equal")
                   (equal sync0-bibtex-entry-date-or-origdate-p "date"))
               (concat "(" sync0-bibtex-entry-date ")"))
              ((equal sync0-bibtex-entry-date-or-origdate-p "eventdate")
               (concat "(" sync0-bibtex-entry-eventdate ") (" sync0-bibtex-entry-date ")"))
              ((equal sync0-bibtex-entry-date-or-origdate-p "eventdate-nodate")
               (concat "(" sync0-bibtex-entry-eventdate ")"))
              ((equal sync0-bibtex-entry-date-or-origdate-p "origdate-nodate")
               (concat "(" sync0-bibtex-entry-origdate ")"))
              (t (concat "(" sync0-bibtex-entry-origdate ") (" sync0-bibtex-entry-date ")")))))

(defun sync0-bibtex-entry-select-draft-prefix ()
  (unless (sync0-null-p sync0-bibtex-entry-doctype)
    (when (string-match "draft" sync0-bibtex-entry-doctype)
      (cond ((equal sync0-bibtex-entry-language "french")
             "Ébauche : ")
            ((equal sync0-bibtex-entry-language "english")
             "Draft: ")
            ((equal sync0-bibtex-entry-language "spanish")
             "Esbozo: ")
            ((equal sync0-bibtex-entry-language "portuguese")
             "Esboço: ")
            (t "Draft: ")))))

;; (defun sync0-bibtex-entry-select-eventtitle-prefix ()
;;   (unless (sync0-null-p sync0-bibtex-entry-eventtitle)
;;     (let ((text (sync0-bibtex-fix-obsidian-chars  sync0-bibtex-entry-eventtitle)))
;;       (if (equal sync0-bibtex-entry-language "french")
;;           (concat text " : ")
;;         (concat text ": ")))))

(defun sync0-bibtex-entry-select-eventtitle-prefix ()
  (unless (sync0-null-p sync0-bibtex-entry-eventtitle)
    (concat "(" (sync0-bibtex-fix-obsidian-chars sync0-bibtex-entry-eventtitle) ") ")))

(defun sync0-bibtex-entry-select-title-separator ()
  (if (equal sync0-bibtex-entry-language "french")
      (setq sync0-bibtex-entry-separator " : ")
    (setq sync0-bibtex-entry-separator ": ")))

(defun sync0-bibtex-entry-select-chapter-prefix ()
  (unless (sync0-null-p sync0-bibtex-entry-chapter)
    (cond ((equal sync0-bibtex-entry-language "french")
           (concat "Ch. " sync0-bibtex-entry-chapter sync0-bibtex-entry-separator))
          ((equal sync0-bibtex-entry-language "english")
           (concat "Ch. " sync0-bibtex-entry-chapter sync0-bibtex-entry-separator))
          ((equal sync0-bibtex-entry-language "spanish")
           (concat "Cap. " sync0-bibtex-entry-chapter sync0-bibtex-entry-separator))
          ((equal sync0-bibtex-entry-language "portuguese")
           (concat "Cap. " sync0-bibtex-entry-chapter sync0-bibtex-entry-separator))
          (t (concat "Ch. " sync0-bibtex-entry-chapter sync0-bibtex-entry-separator)))))

(defun sync0-bibtex-entry-select-volume-postfix ()
  (unless (or (sync0-null-p sync0-bibtex-entry-volume)
              (string= sync0-bibtex-entry-type "Article"))
    (cond ((equal sync0-bibtex-entry-language "french")
           (concat ", T. " sync0-bibtex-entry-volume))
          ((equal sync0-bibtex-entry-language "english")
           (concat ", Vol. " sync0-bibtex-entry-volume))
          ((equal sync0-bibtex-entry-language "spanish")
           (concat ", T. " sync0-bibtex-entry-volume))
          ((equal sync0-bibtex-entry-language "portuguese")
           (concat ", T. " sync0-bibtex-entry-volume))
          (t (concat ", Vol. " sync0-bibtex-entry-volume)))))

(defun sync0-bibtex-entry-set-title-shape ()
  (setq sync0-bibtex-entry-title-shape
        (cond ((and sync0-bibtex-entry-author-or-editor-p 
                    sync0-bibtex-entry-date-or-origdate-p)
               "author-date-title")
              ((and (null sync0-bibtex-entry-author-or-editor-p)
                    sync0-bibtex-entry-date-or-origdate-p)
               "date-title")
              ((and sync0-bibtex-entry-author-or-editor-p
                    (null sync0-bibtex-entry-date-or-origdate-p))
               "author-title")
              (t "title"))))

(defun sync0-bibtex-set-title-extra-fields ()
  "Set fields that do not appear on the biblatex entry as
such (they are not present in the .bib file), but that are
necessary for other functions. The difference between title-fixed
and title-aliases is that when a work has a subtitle, it should
calculate a shorter title, which here is the title-aliases."
  (sync0-bibtex-entry-select-title-separator)
  (sync0-bibtex-entry-set-title-shape)
  (setq sync0-bibtex-entry-title-aliases
        (concat (sync0-bibtex-entry-select-draft-prefix)
                (sync0-bibtex-entry-select-chapter-prefix)
                (sync0-bibtex-entry-select-eventtitle-prefix)
                (sync0-bibtex-fix-obsidian-chars sync0-bibtex-entry-title)
                (sync0-bibtex-entry-select-volume-postfix)))
  (setq sync0-bibtex-entry-title-fixed
        (if (sync0-null-p sync0-bibtex-entry-subtitle)
            sync0-bibtex-entry-title-aliases
          (concat sync0-bibtex-entry-title-aliases sync0-bibtex-entry-separator
                  (sync0-bibtex-fix-obsidian-chars
                   sync0-bibtex-entry-subtitle))))
  (setq sync0-bibtex-entry-title-compatible
        (replace-regexp-in-string "[[:blank:]]*:[[:blank:]]*" "_" sync0-bibtex-entry-title-fixed))
  (setq sync0-bibtex-entry-obsidian-title
         (cond ((equal sync0-bibtex-entry-title-shape "author-date-title")
                (concat sync0-bibtex-entry-lastname
                        " "
                        sync0-bibtex-entry-date-fixed
                        " "
                        sync0-bibtex-entry-title-fixed))
               ((equal sync0-bibtex-entry-title-shape "date-title")
                (concat sync0-bibtex-entry-date-fixed
                        " "
                        sync0-bibtex-entry-title-fixed))
               ((equal sync0-bibtex-entry-title-shape "author-title")
                (concat sync0-bibtex-entry-lastname
                        ", "
                        sync0-bibtex-entry-title-fixed))
               (t sync0-bibtex-entry-title-fixed))))

(defun sync0-bibtex-set-author-or-editor-extra-fields ()
  "Set fields that do not appear on the biblatex entry as
such (they are not present in the .bib file), but that are
necessary for other functions. This function is essential to
calculate the variables used to calculate titles in files and
Obsidian aliases."
  (cond ((and (sync0-null-p sync0-bibtex-entry-author)
              (sync0-null-p sync0-bibtex-entry-editor))
         (setq sync0-bibtex-entry-editor-over-author nil)
         (setq sync0-bibtex-entry-author-or-editor-p nil)
         (setq sync0-bibtex-entry-lastname nil))
        ((and (sync0-null-p sync0-bibtex-entry-author)
              (not (sync0-null-p sync0-bibtex-entry-editor)))
         (setq sync0-bibtex-entry-editor-over-author t)
         (setq sync0-bibtex-entry-author-or-editor-p t)
         (sync0-bibtex-fix-names sync0-bibtex-entry-editor)
         (setq sync0-bibtex-entry-lastname
               (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-editor-lastname)))
        ((and (not (sync0-null-p sync0-bibtex-entry-author))
              (sync0-null-p sync0-bibtex-entry-editor))
         (setq sync0-bibtex-entry-editor-over-author nil)
         (setq sync0-bibtex-entry-author-or-editor-p t)
         (sync0-bibtex-fix-names sync0-bibtex-entry-author)
         (setq sync0-bibtex-entry-lastname
               (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-author-lastname)))
        (t (setq sync0-bibtex-entry-author-or-editor-p t)
           (setq sync0-bibtex-entry-editor-over-author nil)
           (sync0-bibtex-fix-names sync0-bibtex-entry-editor)
           (sync0-bibtex-fix-names sync0-bibtex-entry-author)
           (setq sync0-bibtex-entry-lastname
               (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-author-lastname)))))

(defun sync0-bibtex-nullify-all-variables ()
  "Set all bibtex variables required to do operations to nil to
prevent undesired results."
  (sync0-nullify-variable-list (append sync0-bibtex-entry-helper-fields-list
                                       sync0-bibtex-entry-definitions-list
                                       sync0-bibtex-entry-crossref-definitions-list
                                       sync0-bibtex-entry-extraction-fields-list
                                       sync0-bibtex-entry-initial-fields-list)))

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
      ;;; Configure the author dummy variables avoiding conflicts with
      ;;; the editor field. This part is also necessary to calculate
      ;;; the name of the person used in calculating entries. 
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
    ;; (when sync0-bibtex-entry-doctype
    (unless (sync0-null-p sync0-bibtex-entry-doctype)
      (sync0-bibtex-quotify-var sync0-bibtex-entry-doctype))
    ;; Calculate the people fields
    ;; (let* ((to-remove (list "author" "editor" "translator"))
    ;;        (my-list (cl-set-difference sync0-bibtex-people-fields to-remove :test 'string=)))
    ;;   (dolist (element my-list)
    ;;     (let* ((my-var-string (concat "sync0-bibtex-entry-" element))
    ;;            (my-var (intern my-var-string))) 
    ;;       (unless (sync0-null-p my-var)
    ;;         (sync0-bibtex-fix-names my-var)))))
    (let* ((to-remove (list "author" "editor" "translator"))
           (my-list (cl-set-difference sync0-bibtex-people-fields to-remove :test 'string=)))
      (dolist (element my-list)
        (let ((my-var (intern (concat "sync0-bibtex-entry-" element))))
          (eval `(sync0-bibtex-fix-names ,my-var)))))
    ;; Calculate the translator field
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

(defun sync0-bibtex-choose-attachment (&optional bibkey extension)
  "REWRITE! Function used to select an attachment to be read by
some other programs. This function is inteded to be used in
pipes and not standalone. This function requires package
bibtex-completion to be loaded; otherwise, fails."
  (interactive)
  (let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (predicate (when extension
                      (lambda (x) (string-match (concat ".+\\." extension) x))))
         (attach-list  (bibtex-completion-find-pdf refkey)))
    (cond ((> (length attach-list) 1)
           (completing-read "Choose an attachment to open: " attach-list predicate))
          ((equal (length attach-list) 1)
           (car attach-list))
          (t (error "File field for entry %s is empty." refkey)))))

  (defun sync0-bibtex-add-key-to-pdf (&optional bibkey)
    "Add bibkey to (only) the first page of the pdf on the top left
corner using cpdf. This function only works on pdfs. This
function is to be used only in pipes."
    (interactive)
    (when-let* ((refkey (or bibkey
                            (sync0-bibtex-completion-choose-key t t)))
                (file (sync0-bibtex-choose-attachment refkey))
                (message-function (lambda ()
                                    (message "No pdf found for entry %s." refkey)))
                (output
                 (concat sync0-zettelkasten-attachments-directory refkey "temp.pdf"))
                (command
                 (concat "cpdf -utf8 -add-text \"" refkey "\" -font \"Courier-Bold\" -topleft 20 -font-size 16 " file " 1 -o " output)))
      (if (and (file-exists-p file)
               (equal (file-name-extension file) "pdf")) 
          (progn
            (shell-command command)
            (if (file-exists-p output)
                (rename-file output file t)
              (message "cpdf failed to create the pdf; investigate the error.")))
        (funcall message-function))))

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
                              (sync0-bibtex-choose-attachment sync0-bibtex-entry-crossref "pdf")))
                  (file (when origfile
                          (concat sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key ".pdf " origfile)))
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
                     (shell-command command)))))
        (sync0-bibtex-add-key-to-pdf sync0-bibtex-entry-key))))

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
      ("series" sync0-bibtex-entry-series sync0-bibtex-completion-series "~/.emacs.d/sync0-vars/bibtex-completion-series.txt")
      ("title" sync0-bibtex-entry-title sync0-bibtex-completion-title "~/.emacs.d/sync0-vars/bibtex-completion-title.txt")
      ("doctype" sync0-bibtex-entry-doctype sync0-bibtex-completion-doctype "~/.emacs.d/sync0-vars/bibtex-completion-doctype.txt")
      ("note" sync0-bibtex-entry-note sync0-bibtex-completion-note "~/.emacs.d/sync0-vars/bibtex-completion-note.txt")
      ("library" sync0-bibtex-entry-library sync0-bibtex-completion-library "~/.emacs.d/sync0-vars/bibtex-completion-library.txt")
      ("institution" sync0-bibtex-entry-institution sync0-bibtex-completion-institution "~/.emacs.d/sync0-vars/bibtex-completion-institution.txt")
      ("country" sync0-bibtex-entry-country sync0-bibtex-completion-country "~/.emacs.d/sync0-vars/bibtex-completion-country.txt")
      ("keywords" sync0-bibtex-entry-keywords sync0-bibtex-completion-keywords "~/.emacs.d/sync0-vars/bibtex-completion-keywords.txt")
      ("language" sync0-bibtex-entry-language sync0-bibtex-completion-language  "~/.emacs.d/sync0-vars/bibtex-completion-language.txt"))
    "List of variables used for updating completion files.")

;; Configure people variables and add them to var sync0-bibtex-completion-variables-list
(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (my-list-elem (list element my-var 'sync0-bibtex-completion-author "~/.emacs.d/sync0-vars/bibtex-completion-author.txt")))  
    (push my-list-elem sync0-bibtex-completion-variables-list)))

  (defun sync0-bibtex-update-var (field)
    "Update variables used for completion based on the information
   provided by the new entry."
    (when-let* ((my-list (assoc field sync0-bibtex-completion-variables-list))
                (new-object  (eval (cadr my-list)))
                (completion-var  (caddr my-list))
                (completion-list  (symbol-value completion-var))
                (completion-file   (cadddr my-list)))
      (cond ((member field sync0-bibtex-people-fields) 
             (let (x)
               (cond ((string-match " and " new-object)
                      ;; create a list with parts 
                      (setq x (append (split-string new-object " and ") x)))
                     ;; check when author is an organization
                     ((string-match "^{" new-object)
                      (push  (substring new-object 1 -1) x))
                     ;; other cases
                     (t (push new-object x)))
               (dolist (element x)
                 ;; Check whether the item to be added is already present.
                 (unless (member  element   completion-list)
                   ;; Send the element to the list.
                   (push element completion-list)
                   ;; Update the variable with the bigger list
                   (set completion-var completion-list)
                   ;; Send the element to the file.
                   (append-to-file (concat element "\n") nil completion-file)))))
            ((or (string= field "keywords")
                 (string= field "doctype"))
             (let (x)
               (if (string-match ", " new-object)
                   ;; create a list with parts 
                   (setq x (append (split-string new-object ", ") x))
                 ;; other cases
                 (push new-object x))
               (dolist (element x)
                 (unless (member  element completion-list)
                   ;; Send the element to the list.
                   (push element completion-list)
                   ;; Update the variable with the bigger list
                   (set completion-var completion-list)
                   ;; Send the element to the file.
                   (append-to-file (concat element "\n") nil completion-file)))))
            (t (unless (member  new-object  completion-list)
                 ;; Send the element to the list.
                 (push new-object completion-list)
                 ;; Update the variable with the bigger list
                 (set completion-var completion-list)
                 ;; Send the element to the file.
                 (append-to-file (concat new-object "\n") nil completion-file))))))

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

  ;; (defun sync0-bibtex-entry-append-to-bibliography (bibkey &optional bibfile)
  ;;   "Append new BibLaTeX entry to default bibliography file.
  ;;  Beware, this function only constructs and appends the entry
  ;;  to the bib file; the values of the entry must have been defined
  ;;  elsewhere. For the sake of speed, this function does not
  ;;  perform any sanity checks on duplicate entries, and thus a
  ;;  unique correct entry is assumed to be supplied as mandatory
  ;;  argument bibkey."
  ;;   (let* ((definitions (mapcar #'eval sync0-bibtex-entry-definitions-list))
  ;;          (bibtex-fields sync0-bibtex-fields)
  ;;          (bibliography-file (or bibfile sync0-bibtex-default-bibliography))
  ;;          (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields definitions))
  ;;          ;; define the bibtex entries
  ;;          (entries
  ;;           (let (x)
  ;;             (dolist (element fields x) 
  ;;               (unless (sync0-null-p (cadr element))
  ;;                 (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
  ;;          (bibtex-entry (concat "@" sync0-bibtex-entry-type "{" bibkey ",\n" entries "}\n")))
  ;;     (progn
  ;;       (append-to-file bibtex-entry nil bibliography-file)
  ;;       (sync0-bibtex-entry-inform-new-entry))))

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
           (bibliography-file (cond (bibfile bibfile) 
                                    ((and (sync0-bibtex-buffer-p)
                                          (yes-or-no-p "Use current bibliography file?"))
                                     (buffer-file-name))
                                    (t (completing-read "Which bibliography file to append to ? "
                                                        sync0-bibtex-bibliographies))))
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

  (defun sync0-bibtex-entry-calculate-obsidian-title-aliases ()
    "Calculate titles list to be used in obsidian notes."
    (let* ((title (concat "*" sync0-bibtex-entry-title-fixed "*"))
           (altertitle (concat "*" sync0-bibtex-entry-title-aliases "*"))
           (shorttitle (when sync0-bibtex-entry-shorttitle
                          (concat (sync0-bibtex-entry-select-draft-prefix)
                                  "*"
                                (sync0-bibtex-fix-obsidian-chars
                                  sync0-bibtex-entry-shorttitle)
                                  "*")))
           (shorthand (when sync0-bibtex-entry-shorthand
                        (concat (sync0-bibtex-entry-select-draft-prefix)
                                "*"
                                (sync0-bibtex-fix-obsidian-chars
                                 sync0-bibtex-entry-shorthand)
                                "*")))
           (shorttitle-date (when (and sync0-bibtex-entry-date-or-origdate-p
                                       sync0-bibtex-entry-shorttitle)
                              (concat shorttitle " " sync0-bibtex-entry-date-fixed)))
           ;; (shorthand-date (when (and sync0-bibtex-entry-date-or-origdate-p
           ;;                             sync0-bibtex-entry-shorthand)
           ;;                    (concat shorthand " " sync0-bibtex-entry-date-fixed)))
           (title-date (when sync0-bibtex-entry-date-or-origdate-p
                         (concat title " " sync0-bibtex-entry-date-fixed)))
           (date-title (when sync0-bibtex-entry-date-or-origdate-p
                         (concat sync0-bibtex-entry-date-fixed " " title)))
           (date-shorttitle (when (and sync0-bibtex-entry-date-or-origdate-p
                                       sync0-bibtex-entry-shorttitle)
                         (concat sync0-bibtex-entry-date-fixed " " shorttitle)))
           ;; (date-shorthand (when (and sync0-bibtex-entry-date-or-origdate-p
           ;;                             sync0-bibtex-entry-shorthand)
           ;;               (concat sync0-bibtex-entry-date-fixed " " shorthand)))
           (altertitle-date (when sync0-bibtex-entry-date-or-origdate-p
                              (concat altertitle " " sync0-bibtex-entry-date-fixed)))
           (date-altertitle (when sync0-bibtex-entry-date-or-origdate-p
                              (concat sync0-bibtex-entry-date-fixed " " altertitle)))
           (altertitle-edition (unless (sync0-null-p  sync0-bibtex-entry-edition)
                                 (concat altertitle " (" sync0-bibtex-entry-edition "e)")))
           (title-edition (unless (sync0-null-p  sync0-bibtex-entry-edition)
                            (concat title " (" sync0-bibtex-entry-edition "e)")))
           (shorttitle-edition (when (and shorttitle
                                          sync0-bibtex-entry-edition)
                                 (concat shorttitle " (" sync0-bibtex-entry-edition "e)")))
           ;; (shorthand-edition (when (and shorthand
           ;;                                sync0-bibtex-entry-edition)
           ;;                       (concat shorthand " (" sync0-bibtex-entry-edition "e)")))
           (author-title-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                             (not (sync0-null-p sync0-bibtex-entry-edition)))
                                   (concat sync0-bibtex-entry-lastname ", " title-edition)))
           (author-shorttitle-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                                  sync0-bibtex-entry-shorttitle
                                             (not (sync0-null-p sync0-bibtex-entry-edition)))
                                   (concat sync0-bibtex-entry-lastname ", " shorttitle-edition)))
           ;; (author-shorthand-edition (when (and  sync0-bibtex-entry-author-or-editor-p
           ;;                                        sync0-bibtex-entry-shorthand
           ;;                                   (not (sync0-null-p sync0-bibtex-entry-edition)))
           ;;                         (concat sync0-bibtex-entry-lastname ", " shorthand-edition)))
           (author-altertitle-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                                  (not (sync0-null-p sync0-bibtex-entry-edition)))
                                        (concat sync0-bibtex-entry-lastname ", " altertitle-edition)))
           (author-title-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-date-or-origdate-p)
                                (concat sync0-bibtex-entry-lastname " " date-title)))
           (author-shorttitle-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                               sync0-bibtex-entry-shorttitle
                                          sync0-bibtex-entry-date-or-origdate-p)
                                (concat sync0-bibtex-entry-lastname " " date-shorttitle)))
           ;; (author-shorthand-date (when (and  sync0-bibtex-entry-author-or-editor-p
           ;;                                     sync0-bibtex-entry-shorthand
           ;;                                sync0-bibtex-entry-date-or-origdate-p)
           ;;                      (concat sync0-bibtex-entry-lastname " " date-shorthand)))
           (author-altertitle-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                               sync0-bibtex-entry-date-or-origdate-p)
                                     (concat sync0-bibtex-entry-lastname " " date-altertitle)))
           (author-title (when (and  sync0-bibtex-entry-author-or-editor-p
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " title)))
           (author-shorttitle (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-shorttitle
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " shorttitle)))
           (author-shorthand (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-shorthand
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " shorthand)))
           (author-altertitle (when (and  sync0-bibtex-entry-author-or-editor-p
                                          (or (null sync0-bibtex-entry-date-or-origdate-p)
                                              (null sync0-bibtex-entry-edition)))
                                (concat sync0-bibtex-entry-lastname ", " altertitle)))
           (title-to-use (list title
                               altertitle
                               shorttitle
                               shorthand
                               title-date
                               shorttitle-date
                               ;; shorthand-date
                               date-title
                               date-shorttitle
                               altertitle-date
                               date-altertitle
                               altertitle-edition
                               title-edition
                               shorttitle-edition
                               ;; shorthand-edition
                               author-title-edition
                               author-shorttitle-edition
                               ;; author-shorthand-edition
                               author-altertitle-edition
                               author-title-date
                               author-shorttitle-date
                               ;; author-shorthand-date
                               author-altertitle-date
                               author-title
                               author-shorttitle
                               author-shorthand
                               author-altertitle))
           (purged-title-list (if (equal sync0-bibtex-entry-title-shape "title")
                                  (cl-remove nil title-to-use)
                                (cl-remove nil (cdddr title-to-use))))
           (title-list (cl-remove-duplicates purged-title-list :test #'equal)))
       (sync0-show-elements-of-list title-list "\", \"")))

  (defun sync0-bibtex-entry-create-obsidian-note-from-entry (bibkey &optional rewrite)
    "Create new markdown note for corresponding bibkey in default
obsidian vault. This function in not intended for interactive
use, but as a function to be included in pipes. When optional
rewrite is true, this function rewrites the YAML frontmatter of
the note, instead of attempting to create a new note."
    (let* ((obsidian-file (concat sync0-zettelkasten-directory bibkey ".md")) 
           (title-list-string (sync0-bibtex-entry-calculate-obsidian-title-aliases))
           (obsidian-fields-string (let (x)
                                     (dolist (element sync0-bibtex-obsidian-fields-list x)
                                       (let ((field (car element))
                                             (opener (cadr element))
                                             (closer (caddr element))
                                             (variable (cadddr element)))
                                          ;; (value (sync0-bibtex-fix-obsidian-chars (eval variable)))
                                         ;; (when-let (value   (eval variable))
                                         (when-let (value (sync0-bibtex-fix-obsidian-chars (eval variable)))
                                           (push (concat field ": " opener value closer) x))))
                                     (sync0-show-elements-of-list x "\n")))
           (obsidian-yaml (concat "---\n"
                                  "zettel_type: reference\n"
                                  "id: " bibkey "\n"
                                  "citekey: " bibkey "\n"
                                  "biblatex_type: " (downcase sync0-bibtex-entry-type)  "\n"
                                  obsidian-fields-string
                                  ;; (if (sync0-null-p sync0-bibtex-entry-created)
                                  ;;     (concat "\ncreated: " (format-time-string "%Y-%m-%d") "\n")
                                  ;;   (concat "\ncreated: " sync0-bibtex-entry-created "\n"))
                                  (concat "aliases: [\"" title-list-string "\"]\n")
                                  "tags: [bibkey/" bibkey ", " sync0-bibtex-entry-keywords "]\n"
                                  "---\n"
                                  (concat "# " sync0-bibtex-entry-obsidian-title "\n")))
           (obsidian-rest (concat
                           sync0-bibtex-obsidian-reference-template-top
                           bibkey
                           sync0-bibtex-obsidian-reference-template-bottom))
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
          (setq x (concat (cdr (assoc "=key=" (cdr (assoc selection candidates)))) ", " x)))
        (if (equal counter 1)
            (format "%s" x)
          (format "%s" (substring x 2 -2))))))

  (defun sync0-bibtex-download-pdf (&optional creation)
    (interactive)
    (if creation
        (if (y-or-n-p "Call the buchaneers? ")
            (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF") t)
          (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF")))
      (when-let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
                     (entry (bibtex-completion-get-entry bibkey))
                     (url (bibtex-completion-get-value "url" entry))
                     (pdf (sync0-bibtex-choose-attachment bibkey "pdf")))
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

(defun sync0-bibtex-entry-key-redefine (&optional suggester)
  "Recreate new bibtex key following a pre-defined rule."
  (if (yes-or-no-p "Create automatic key?")
      (sync0-bibtex-entry-key-define t)
    (let ((x (read-string "New key : " suggester)))
      (while (member x sync0-bibtex-keys) 
        (message "Invalid key. %s is already present in database." x)
        (setq x (read-string "New key : " suggester)))
      (sync0-bibtex-add-key-to-keyvars)
      (format "%s" x))))

  ;; (defun sync0-bibtex-update-key-in-bibfile (oldkey newkey bibfile)
  ;;   "Change bibtex key at point with a key using the format provided
  ;; by org-roam files"
  ;;   (with-temp-file bibfile
  ;;     (insert-file-contents bibfile)
  ;;     (goto-char (point-min))
  ;;     (while (re-search-forward oldkey nil t)
  ;;       (replace-match newkey))))

  (defun sync0-bibtex-update-key-in-buffer (oldkey newkey)
    "Change bibtex key at point with a key using the format provided
by org-roam files"
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward oldkey nil t)
        (replace-match newkey))))

  (defun sync0-bibtex-update-key ()
    "Change bibtex key at point with a key using the format
provided by org-roam files. Function intended to be used on an
entry under point in a .bib file"
    (interactive)
    (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			          (bibtex-parse-entry)))
           (type (cdr (assoc "=type=" entry)))
           (old-key (cdr (assoc "=key=" entry)))
           (suggest-key (if (string-match "[[:digit:]]\{8\}" old-key)
                            (concat (substring old-key 2 4)
                                    (sync0-random-alnum)
                                    (sync0-random-alnum)
                                    (sync0-random-alnum))
                          (concat (substring old-key 0 2)
                                  (sync0-random-alnum)
                                  (sync0-random-alnum)
                                  (sync0-random-alnum))))
           (new-key (sync0-bibtex-entry-key-redefine suggest-key))
           (beg (save-excursion (bibtex-beginning-of-entry)))
           (end (save-excursion (bibtex-end-of-entry)))
           (attach-list  (bibtex-completion-find-pdf old-key))
           (old-archive-image  (concat sync0-bibtex-archive-directory old-key ".jpg"))
           (new-archive-image  (concat sync0-bibtex-archive-directory new-key ".jpg"))
           (old-attachment (when-let (x (cdr (assoc "file" entry)))
                             (string-trim x "{" "}")))
           (new-attachment (when old-attachment
                             (replace-regexp-in-string old-key new-key old-attachment)))
           (path-list (list "file" "Attachment path" new-attachment nil)))
;;; end of definitions
;;; Go to beginning of entry
      (sync0-bibtex-completion-load-entry old-key)
      (bibtex-beginning-of-entry)
      (when (sync0-null-p sync0-bibtex-entry-keywords)
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
        (save-excursion 
          (bibtex-make-field (list "keywords" "Whatever string" sync0-bibtex-entry-keywords nil) t)))
;;; Update journal fields
      (save-excursion
        (when (re-search-forward "\\(journal\\)[[:blank:]]+=" end t 1)
          (replace-match "journaltitle" nil nil nil 1)))
;;; Update year fields
      (save-excursion
        (when (re-search-forward "\\(year\\)[[:blank:]]+=" end t 1)
          (replace-match "date" nil nil nil 1)))
      (kill-whole-line 1)
      (insert (concat "@" type "{" new-key ",\n"))
      (when (file-exists-p old-archive-image)
        (rename-file old-archive-image new-archive-image))
      (when (and attach-list
                 (yes-or-no-p "Rename existing attachments? "))
        (dolist (element attach-list)
          (let ((new-path (replace-regexp-in-string old-key new-key element)))
            (when (file-exists-p element)
              (rename-file element new-path)))))
      (when attach-list 
        (bibtex-make-field path-list t))
      ;; Replace all instances of the old key in the current buffer.
      (sync0-bibtex-update-key-in-buffer old-key new-key)
      (message "Key for entry %s has been replaced with key %s" old-key new-key)))

  (defun sync0-bibtex-clean-entry ()
    "Change bibtex key at point with a key using the format provided
by org-roam files"
    (interactive)
    (let* ((new-key (sync0-bibtex-entry-key-define))
           (directory sync0-zettelkasten-attachments-directory)
           (new-path (concat directory new-key ".pdf"))
           (regex "^@\\([[A-z]+\\){[[:alnum:]]+,")
           (beg (save-excursion (bibtex-beginning-of-entry)))
           (end (save-excursion (bibtex-end-of-entry)))
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

(defun sync0-bibtex-define-multiple-entries (num)
  "Create new BibLaTeX entries..."
  (interactive "nHow many new entries to create? ")
  (sync0-bibtex-nullify-all-variables)
  (let*    ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
            ;; Specify which fields to load
            (fields (append (cdr (assoc type sync0-bibtex-type-fields))
                            (remove "file" sync0-bibtex-base-fields)))
            ;; (add-notes (yes-or-no-p "Create Obsidian notes for the newly created entries?"))
            (bibfile (if (and (sync0-bibtex-buffer-p)
                              (yes-or-no-p "Use current bibliography file?"))
                         (buffer-file-name)
                       (completing-read "Which bibliography file to append to ? "
                                        sync0-bibtex-bibliographies)))
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
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography filename bibfile)
        ;; Third, create an obsidian markdown note for the entry.
        (sync0-bibtex-entry-create-obsidian-note-from-entry filename)))))

  (defun sync0-bibtex-extract-multiple-entries-from-pdf (seqlists)
    "Extract pdfs and create bibtex entries and markdown entries
   for each extranction. This function requires an input 'seqlists'
   that is a list composed of n lists of the form: (name,
   first-page, last-page). Likewise, for this to work, it is
   necessary that the pages of the pdf match the page numbers of the
   actual book. Otherwise, the entries will have wrong data."
    (sync0-bibtex-completion-load-entry (sync0-bibtex-completion-choose-key t t))
    (let*    ((bibkey sync0-bibtex-entry-key)
              (add-notes (yes-or-no-p "Create Obsidian notes for the newly created entries?"))
              (no-of-entries (length seqlists))
              (keylist (sync0-bibtex-entry-define-keys-list no-of-entries))
              (bibfile (if (and (sync0-bibtex-buffer-p)
                                (yes-or-no-p "Use current bibliography file?"))
                           (buffer-file-name)
                         (completing-read "Which bibliography file to append to ? "
                                          sync0-bibtex-bibliographies)))
              (keywords-list (split-string sync0-bibtex-entry-keywords ", "))
              (extra-keywords (completing-read-multiple "Input extra keywords: " 
                                                        sync0-bibtex-completion-keywords))
              (master-list (if (sync0-null-p extra-keywords)
                               keywords-list
                             (cl-union keywords-list extra-keywords)))
              (input-file (sync0-bibtex-choose-attachment bibkey "pdf")))
      (setq sync0-bibtex-entry-keywords (sync0-show-elements-of-list master-list ", "))
      (setq sync0-bibtex-entry-type-crossref bibkey)
      (setq sync0-bibtex-entry-type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
      (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
      (setq sync0-bibtex-entry-parent sync0-bibtex-entry-title-fixed)
      ;; Begin loop; specific settings for each note are to be defined
      ;; inside the loop.
      (dotimes (i no-of-entries)
        (let* ((triple (elt seqlists i))
               (filename (elt keylist i))
               (title (nth 0 triple))
               (beg (number-to-string (nth 1 triple)))
               (end (number-to-string (nth 2 triple)))
               (pages (if (string= beg end)
                          beg
                        (concat beg "-" end)))
               (pages-fixed (replace-regexp-in-string "-" "--" pages)))
          (setq sync0-bibtex-entry-key filename)
          (setq sync0-bibtex-entry-title title)
          (setq sync0-bibtex-entry-subtitle nil)
          (setq sync0-bibtex-entry-title-fixed sync0-bibtex-entry-title)
          (setq sync0-bibtex-entry-pages pages)
          (setq sync0-bibtex-entry-shorthand
                (concat title ", p. " pages-fixed))
          (setq sync0-bibtex-entry-file
                (concat ":" sync0-zettelkasten-attachments-directory filename ".pdf:PDF"))
          ;; Beginning of loop actions
          ;; First, extract entry
          (sync0-bibtex-crossref-extract-pdf beg end)
          ;; Second, append entry to default bibliography file.
          (sync0-bibtex-entry-append-to-bibliography filename bibfile)
          ;; Third, create an obsidian markdown note for the entry.
          (sync0-bibtex-entry-create-obsidian-note-from-entry filename)))))

  (defun sync0-bibtex-extract-subpdf ()
    "Extract pdf from an existing pdf."
    ;; Set all fields to nil 
    (interactive)
    (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
              (input-file (sync0-bibtex-choose-attachment bibkey "pdf"))
              (postfix (read-string "What postfix to identify extracted pdf? : "))
              (output-file (concat sync0-zettelkasten-attachments-directory bibkey "_" postfix ".pdf"))
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
           ;; (entry (bibtex-completion-get-entry  bibkey))
           ;; (raw-file (bibtex-completion-get-value "file" entry))
           ;; (file (sync0-bibtex-choose-attachment raw-file)))
           ;; (file (let ((attachments  (bibtex-completion-find-pdf bibkey)))
           ;;         (if (equal (length attachments) 1)
           ;;             (car attachments)
           ;;           (completing-read "Which attachment to open? " attachments))))
           (file (sync0-bibtex-choose-attachment bibkey)))
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
           (notes-file  (concat sync0-zettelkasten-directory  bibkey ".md")))
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
                          (attachments  (bibtex-completion-find-pdf bibkey))
                          (pdf (if (equal (length attachments) 1)
                                   (car attachments)
                                 (completing-read "Which attachment to open? " attachments))))
                     (if (file-exists-p pdf)
                         (shell-command (concat command pdf))
                       (message "No attachment found for entry %s" bibkey))))
          (t (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
                    (attachments  (bibtex-completion-find-pdf bibkey))
                    (pdf (if (equal (length attachments) 1)
                             (car attachments)
                           (completing-read "Which attachment to open? " attachments)))
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
           (output (concat sync0-zettelkasten-attachments-directory "temp.pdf"))
           (command (concat "gs -o " output " -sDEVICE=pdfwrite" cropbox " /PAGES pdfmark\" -f " pdf)))
      (if (file-exists-p pdf)
          (progn 
            (shell-command command)
            (rename-file output pdf t)
            (message "Crop box has been redefined for %s" pdf))
        (message "No pdf found for %s" pdf))))

  (defun sync0-bibtex-add-field ()
    (interactive)
    (setq sync0-bibtex-entry-creation nil)
    (sync0-bibtex-nullify-all-variables)
    (let* ((field (completing-read "Choose Bibtex field: " sync0-bibtex-fields))
           (entry (save-excursion (bibtex-beginning-of-entry)
			          (bibtex-parse-entry)))
           (bibkey (cdr (assoc "=key=" entry))))
      ;; (setq sync0-bibtex-entry-key bibkey)
      (sync0-bibtex-completion-load-entry bibkey)
      (unless (sync0-null-p  sync0-bibtex-entry-file)
        (setq sync0-bibtex-entry-file-old t))
      (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
      (bibtex-beginning-of-entry)
      (bibtex-make-field (list field "Whatever string"
                               (eval (intern (concat "sync0-bibtex-entry-" field))) nil) t)))

  (defun sync0-bibtex-copy-pdf-to-path (&optional in-path bibkey)
    "Copy attached pdf to path and change the title to make it
readable."
    (interactive)
    (when-let* ((refkey (or bibkey
                            (sync0-bibtex-completion-choose-key t t)))
                (file (sync0-bibtex-choose-attachment refkey))
                (extension  (file-name-extension file t)))
      (sync0-bibtex-completion-load-entry refkey)
      (if (file-exists-p file)
          (let* ((target-path (or in-path
                                  (read-string "Où envoyer ce fichier ? (finir en /) ")))
                 (command (concat "cp "
                                  file
                                  " \""
                                  target-path
                                  sync0-bibtex-entry-lastname
                                  "_"
                                  (or sync0-bibtex-entry-date-fixed
                                      "")
                                  "_"
                                  sync0-bibtex-entry-title-compatible
                                  extension
                                  "\"")))
            (shell-command command)
            (message "PDF for %s moved to target location" sync0-bibtex-entry-key))
        (message "No PDF found for %s" sync0-bibtex-entry-key))))

  (defun sync0-bibtex-archive-entry (&optional bibkey)
    "Choose an entry to send to the archived bibliography. This
function fails when the entry is at the top of the buffer becase
the function 1- fails to compute."
    (interactive)
    (when bibkey
      (bibtex-search-entry bibkey))
    ;; (re-search-forward (concat "{" bibkey ",") nil t 1)
    (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
           (end (save-excursion (bibtex-end-of-entry))))
      (append-to-file beginning end sync0-bibtex-archived-bibliography)
      (delete-region beginning end)))

(defun sync0-bibtex-move-entry-to-bibfile (&optional bibkey bibfile)
  "Choose an entry to send to the archived bibliography. This
function fails when the entry is at the top of the buffer becase
the function 1- fails to compute."
  (interactive)
  (when-let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (bibdestiny (or bibfile
                         (completing-read "Which bibliography file to send to ? "
                                          sync0-bibtex-bibliographies nil t))))
      (bibtex-search-entry refkey)
      (let ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
            (end (save-excursion (bibtex-end-of-entry))))
      (append-to-file beginning end bibdestiny)
      (delete-region beginning end))))

  (defun sync0-bibtex-delete-entry (&optional bibkey)
    "Choose an entry to permanently delete. Remeber: The deleted
entry could be recovered if previously commited on a
version-controlled file. The attachments are sent to the system
trash, as defined by the desktop environments (KDE, Gnome,
etc.)."
    (interactive)
    (let ((refkey (or bibkey
                      (sync0-bibtex-completion-choose-key t t))))
      (bibtex-search-entry refkey)
      (sync0-bibtex-completion-load-entry refkey)
      (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
             (attachments (bibtex-completion-find-pdf refkey))
             (num-of-attachments (length attachments))
             (trash-message (concat "Send to trash entry "
                                    sync0-bibtex-entry-lastname
                                    (or sync0-bibtex-entry-date-fixed
                                        " ")
                                    sync0-bibtex-entry-title-compatible
                                    "?"))
             (trash-attach-message (format "Entry has %s attachments. Do you want to send these to trash?" attachments))
             (end (save-excursion (bibtex-end-of-entry))))
        (when (yes-or-no-p trash-message)
          (delete-region beginning end))
        (when (yes-or-no-p trash-attach-message)
          (mapc #'move-file-to-trash attachments)))))

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
           (output (concat sync0-zettelkasten-attachments-directory "temp.pdf"))
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

  (defun sync0-bibtex-define-entries-from-bibkey (&optional bibkey)
    "Creates copies of target entry but with different keys. This
function also creates markdown notes for the created entries."
    (interactive)
    (sync0-bibtex-completion-load-entry (or bibkey
                                            (sync0-bibtex-completion-choose-key t t)))
    (let* ((howmany (string-to-number (read-string "How many entries to create? ")))
           (add-notes (yes-or-no-p "Create Obsidian notes for the newly created entries?"))
           (keylist (sync0-bibtex-entry-define-keys-list howmany))
           (bibfile (if (and (sync0-bibtex-buffer-p)
                             (yes-or-no-p "Use current bibliography file for new entry?"))
                        (buffer-file-name)
                      (completing-read "Which bibliography file to append to ? "
                                       sync0-bibtex-bibliographies nil t)))
           (keywords-list (split-string sync0-bibtex-entry-keywords ", "))
           (extra-keywords (completing-read-multiple "Input extra keywords: " 
                                                     sync0-bibtex-completion-keywords))
           (master-list (if (sync0-null-p extra-keywords)
                            keywords-list
                          (cl-union keywords-list extra-keywords))))
      (setq sync0-bibtex-entry-keywords (sync0-show-elements-of-list master-list ", "))
      ;; Beginning of loop actions
      (dolist (key keylist)
        (setq sync0-bibtex-entry-key key)
        (unless (sync0-null-p sync0-bibtex-entry-file)
          (setq sync0-bibtex-entry-file
                (concat ":" sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key ".pdf:PDF")))
        ;; Second, append entry to default bibliography file.
        (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key bibfile)
        ;; Third, create an obsidian markdown note for the entry.
        (unless (null add-notes)
          (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)))
      ;; Fourth, add a markdown link in the obsidian master note
      ;; (ie., the note corresponding to the file used as the source
      ;; for extraction).
      ;; (append-to-file obsidian-reference nil obsidian-master-note)
      (unless (sync0-null-p sync0-bibtex-entry-crossref)
        (bibtex-search-entry sync0-bibtex-entry-crossref)
        ;; (bibtex-beginning-of-entry)
        (bibtex-make-field (list "relatedtype" "Whatever string" "multivolume" nil) t)
        (bibtex-make-field (list "related" "Whatever string" (sync0-show-elements-of-list keylist ", ") nil) t))))

  (defun sync0-bibtex-yank-citation-from-bibkey (&optional bibkey)
    "Add bibkey citation kill ring."
    (interactive)
    (let ((refkey (if bibkey
                      bibkey
                    (sync0-bibtex-completion-choose-key t t))))
      (sync0-bibtex-completion-load-entry refkey)
      (when-let ((citation (concat sync0-bibtex-entry-lastname
                                   (or sync0-bibtex-entry-date-fixed
                                       " ")
                                   sync0-bibtex-entry-title-compatible)))
        (kill-new citation)
        (message "%s copied to kill ring." citation))))

  (defun sync0-bibtex-yank-citation-from-bibkey (&optional bibkey)
    "Add bibkey citation kill ring."
    (interactive)
    (let ((refkey (if bibkey
                      bibkey
                    (sync0-bibtex-completion-choose-key t t))))
      (sync0-bibtex-completion-load-entry refkey)
      (when-let ((citation (concat sync0-bibtex-entry-lastname
                                   (or sync0-bibtex-entry-date-fixed
                                       " ")
                                   sync0-bibtex-entry-title-compatible)))
        (kill-new citation)
        (message "%s copied to kill ring." citation))))

  (defun sync0-bibtex-convert-jpg-to-pdf (&optional bibkey)
    "Copy attached pdf to path and change the title to make it
readable."
    (interactive)
    (let* ((refkey (if bibkey
                       bibkey
                     (sync0-bibtex-completion-choose-key t t)))
           (file (sync0-bibtex-choose-attachment refkey))
           (image (concat sync0-bibtex-archive-directory refkey ".jpg"))
           ;; (extension (file-name-extension file))
           (command (concat "convert " image " -auto-orient " file)))
      ;; (sync0-bibtex-completion-load-entry refkey)
      (if (and (file-exists-p file)
               (file-exists-p image))
          (shell-command command)
        (message "Conversion for entry %s failed." refkey))))

  (defun sync0-bibtex-arrange-pdf (&optional bibkey)
    "Use PDF Arranger on target pdf."
    (interactive)
    (let* ((refkey (or bibkey
                       (sync0-bibtex-completion-choose-key t t)))
           (file (sync0-bibtex-choose-attachment refkey))
           (extension (file-name-extension file))
           (command (concat "pdfarranger " file)))
      (if (and (file-exists-p file)
               (equal extension "pdf"))
          (shell-command command)
        (message "Problem opening file for entry %s failed." refkey))))

(defun sync0-bibtex-transplant-obsidian-ref-into-biblatex ()
  "Create new BibLaTeX entry in the default bibliography. When
   optional quick is non-nil, only capture the minimal fields
   required to create a new entry."
  (interactive)
  ;; Before calculating any values, reset all values in my
  ;; master list of variable (set them to nil).
  ;; (setq sync0-bibtex-entry-derivation (yes-or-no-p "Derive entry?"))
  (setq sync0-bibtex-entry-creation t)
  (setq sync0-bibtex-entry-file-old nil)
  (setq sync0-bibtex-entry-keywords nil)
  ;; (sync0-bibtex-completion-load-entry nil quick)
  (let* ((obsidian-id (read-string "Which Obsidian file to use as base for new entry?"))
         (obsidian-file (concat sync0-zettelkasten-directory obsidian-id ".md"))
         (obsidian-file-string (f-read-text obsidian-file))
         x)
    (if (file-exists-p obsidian-file)
        (progn
          (when (sync0-bibtex-duplicate-entry-key-p obsidian-id)
            (setq obsidian-id (sync0-bibtex-entry-key-define t)))
          (setq sync0-bibtex-entry-key obsidian-id)  
          (setq sync0-bibtex-entry-keywords (sync0-yaml-get-property "tags" obsidian-file-string))
          (setq sync0-bibtex-entry-author (sync0-yaml-get-property "author" obsidian-file-string))
          (setq sync0-bibtex-entry-date (sync0-yaml-get-property "date" obsidian-file-string))
          (setq sync0-bibtex-entry-origdate (sync0-yaml-get-property "origdate" obsidian-file-string))
          (setq sync0-bibtex-entry-publisher (sync0-yaml-get-property "publisher" obsidian-file-string))
          (setq sync0-bibtex-entry-doctype (sync0-yaml-get-property "doctype" obsidian-file-string))
          (setq sync0-bibtex-entry-title (sync0-yaml-get-property "title" obsidian-file-string))
          (setq sync0-bibtex-entry-journaltitle (sync0-yaml-get-property "journaltitle" obsidian-file-string))
          (setq sync0-bibtex-entry-language (sync0-yaml-get-property "language" obsidian-file-string))
          (setq sync0-bibtex-entry-langid sync0-bibtex-entry-language)
          (setq sync0-bibtex-entry-type-downcase (sync0-yaml-get-property "biblatex_type" obsidian-file-string))
          (setq sync0-bibtex-entry-type (if (member sync0-bibtex-entry-type-downcase sync0-bibtex-entry-types-correction)
                                            (let (beg (upcase-initials (substring sync0-bibtex-entry-type-downcase 0 3)))
                                              (end (upcase-initials (substring sync0-bibtex-entry-type-downcase 4 nil)))
                                              (concat beg end))
                                          (upcase-initials sync0-bibtex-entry-type-downcase)))
          (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key))
      (message "No Obsidian file found for %s" obsidian-id))))

  (major-mode-hydra-define bibtex-mode nil 
    ("Entries"
     (("c" sync0-bibtex-clean-entry "Clean entry")
      ("E" sync0-bibtex-define-entry "Capture entry")
      ("e" (sync0-bibtex-define-entry t) "Quick capture")
      ("m" sync0-bibtex-define-multiple-entries "Multiple entries")
      ("t" sync0-bibtex-transplant-obsidian-ref-into-biblatex "Get from Obsidian")
      ("u" sync0-bibtex-update-key "Update key")
      ("f" sync0-bibtex-define-entries-from-bibkey "Capture many entries from key")
      ("M" sync0-bibtex-move-entry-to-bibfile "Move entry to bibfile")
      ("D" sync0-bibtex-delete-entry "Delete entry")
      ("A" sync0-bibtex-archive-entry "Archive entry"))
     "PDF editing"
     (("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
      ("p" sync0-bibtex-print-pdf "Print att. from entry")
      ;; This does note work for some reason
      ;; ("x" sync0-bibtex-arrange-pdf "Arrange pdf")
      ("C" sync0-bibtex-crop-pdf "Crop attached pdf")
      ("T" sync0-bibtex-add-toc-to-pdf "Add TOC to pdf")
      ("K" sync0-bibtex-add-key-to-pdf "Add key to pdf")
      ;; ("s" sync0-bibtex-extract-subpdf "Extract subpdf")
      ("d" sync0-bibtex-download-pdf "Download pdf from url"))
     "Visit"
     (("o" sync0-bibtex-open-pdf-at-point "Open in pdfview")
      ("z" (sync0-bibtex-open-pdf-at-point t) "Open in zathura")
      ;; ("o" sync0-org-ref-open-pdf-at-point "Open in pdfview")
      ("w" sync0-bibtex-open-url "Open url")
      ("n" sync0-bibtex-open-notes "Open annotations"))
     "Bibliographies"
     (("s" ivy-bibtex "Search entry")
      ("S" ivy-bibtex-with-local-bibliography "Search entry locally")
      ("b" sync0-bibtex-recalc-bibliographies "Recalc bibliographies")
      ("B" sync0-bibtex-recalc-master-bibliography "Recalc master bib file")
      ("r" sync0-bibtex-populate-keys "Populate keys")
      ("v" sync0-bibtex-visit-bibliography "Visit bibfile"))
     "Etc"
     ;; ("r" (sync0-bibtex-update-completion-files sync0-bibtex-completion-variables-list) "Refresh completion vars")
     (("a" sync0-bibtex-add-field "Add field")
      ("i" sync0-bibtex-convert-jpg-to-pdf "Convert jpg to pdf")
      ("k" bu-make-field-keywords "Add keyword")
      ("y" sync0-bibtex-yank-citation-from-bibkey "Yank citation.")
      ("N" sync0-bibtex-create-note-from-entry "Create md note for entry")
      ("R" (sync0-bibtex-create-note-from-entry t) "Rewrite md for entry"))))

  (provide 'sync0-bibtex-functions)
