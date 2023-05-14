(require 'xah-replace-pairs)

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
    (concat "(" sync0-bibtex-entry-eventtitle ") ")))

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
              (string= sync0-bibtex-entry-type "Article")
              (string= sync0-bibtex-entry-type "InCollection"))
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
        (let ((base (replace-regexp-in-string "[[:blank:]]*:[[:blank:]]*" "_" sync0-bibtex-entry-title-fixed)))
          (if (string= sync0-bibtex-entry-type-downcase "article")
              (concat base "_(" sync0-bibtex-entry-journaltitle ")")
            base)))
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

;; (defun sync0-bibtex-obsidian-keyword-tagify (my-string)
;;   "Make my-string compatible with the tags of obsidian by
;; downcasing and removing whitespace from tags to be included as
;; keywords in biblatex entries and obsidian markdown files."
;;   (let ((x (if (string-match-p ",[[:space:]]" my-string)
;;                my-string
;;              (let ((nospace (replace-regexp-in-string "[[:space:]]+" "_" my-string)))
;;                (downcase nospace)))))
;;     (xah-replace-pairs-in-string-recursive
;;      x
;;      [["d'" ""]
;;       ["l'" ""]
;;       ["l’" ""]
;;       ["." ""]
;;       ["_/&_" "_"]
;;       ["_//&_" "_"]
;;       [" /& " "_"]
;;       [" //& " "_"]
;;       [", " "_"]
;;       ["d’" ""]])))

;; (defun sync0-bibtex-obsidian-keyword-cleanup (keyword-string)
;;   (xah-replace-pairs-in-string-recursive
;;    keyword-string 
;;    [["d'" ""]
;;     ["l'" ""]
;;     ;; ["-de-" ""]
;;     ["l’" ""]
;;     ["." ""]
;;     ;; ["&" "_"]
;;     ["\&" ""]
;;     ["\\&" ""]
;;     ["\\_" "_"]
;;     ["\_" "_"]
;;     ["__" "_"]
;;     ;; [" /& " "_"]
;;     ;; [" & " "_"]
;;     ;; [" //& " "_"]
;;     ;; [", " "_"]
;;     [",_" "_"]
;;     ["d’" ""]]))

(defun sync0-bibtex-obsidian-keyword-cleanup (keyword-string)
  "Corrections for the whole keyword string."
  (let ((x (replace-regexp-in-string "[^,]\\([[:space:]]\\)" "_" keyword-string t nil 1)))
    (xah-replace-pairs-in-string-recursive
     x
     [["/de-" "/"]])))

;; (defun sync0-bibtex-correct-journaltitle-keywords ()
;;   "Corrections for the whole keyword string."
;;   (when (or (string= sync0-bibtex-entry-type-downcase "article")
;;             (string= sync0-bibtex-entry-type-downcase "collection"))
;;     (unless (null sync0-bibtex-entry-journaltitle)
;;       (setq sync0-bibtex-entry-journaltitle-tag
;;             (xah-replace-pairs-in-string
;;              sync0-bibtex-entry-journaltitle
;;              [[" de " "_"]
;;               [" la " "_"]
;;               ["la " ""]
;;               ["le " ""]
;;               ["les " ""]
;;               ["the " ""]
;;               [" et " "_"]
;;               [" of " "_"]
;;               [" the " "_"]
;;               [" les " "_"]
;;               [" and " "_"]
;;               [" du " "_"]
;;               [" des " "_"]
;;               [" da " "_"]
;;               [" do " "_"]
;;               [" du " "_"]])))))

(defun sync0-bibtex-correct-keywords (field)
  "Create corrected var-tag for field to appear in keyword string."
  (when-let* ((var (intern (concat "sync0-bibtex-entry-" field)))
              (value (symbol-value var))
              (var-tag (intern (concat "sync0-bibtex-entry-" field "-tag"))))
    (unless (null var)
      (set var-tag
            (xah-replace-pairs-in-string
             value
             [[" de " "_"]
              [" la " "_"]
              ["la " ""]
              ["le " ""]
              ["les " ""]
              ["the " ""]
              [" et " "_"]
              [" of " "_"]
              [" the " "_"]
              [" les " "_"]
              [" and " "_"]
              [" du " "_"]
              [" des " "_"]
              [" da " "_"]
              [" do " "_"]
              [" du " "_"]])))))

  ;; (let* ((nospace (replace-regexp-in-string "[^,][[:space:]]+" "_" my-string))
  ;;        (x (downcase nospace)))

(defun sync0-bibtex-obsidian-keyword-tagify (my-string)
  "Corrections for individual fields used in keywords. Make
my-string compatible with the tags of obsidian by downcasing and
removing whitespace from tags to be included as keywords in
biblatex entries and obsidian markdown files."
  (let ((x (downcase my-string)))
    (xah-replace-pairs-in-string-recursive
     x
     [["d'" ""]
      ["l'" ""]
      ;; ["-de-" ""]
      ["l’" ""]
      ;; [" de " "-"]
      ;; ["-de-" "-"]
      ;; ["de-" ""]
      ["." ""]
      ;; ["&" "_"]
      ["\&" ""]
      ["\\&" ""]
      ["\\_" "_"]
      ["\_" "_"]
      ["__" "_"]
      ;; [" /& " "_"]
      ;; [" & " "_"]
      ;; [" //& " "_"]
      ;; [", " "_"]
      [",_" "_"]
      ["d’" ""]])))

(defun sync0-bibtex-correct-smartquotes (my-string)
  "Make my-string compatible with the tags of obsidian by
downcasing and removing whitespace from tags to be included as
keywords in biblatex entries and obsidian markdown files."
    (xah-replace-pairs-in-string
     my-string
     [["'" "’"]
     ["--" "–"]
     ["---" "—"]]))

(defun sync0-bibtex-obsidian-tagify (sep my-string)
  "Make my-string compatible with the tags of obsidian by
downcasing and removing whitespace from tags to be included as
keywords in biblatex entries and obsidian markdown files."
  (let* ((nospace (s-trim sep))
         (regex (concat "[^" nospace "]\\([[:space:]]+\\)"))
         (nospace (replace-regexp-in-string regex "_" my-string nil t 1)))
    (downcase nospace)))

(defmacro sync0-bibtex-tagify-var (var)
  "Put the values of var with quotes. This function prepares two
helper variables for biblatex fields that have multiple
completion options such as medium and doctype (keywords is
special, thus it should not be subjected to this functions).
Specifically, the helper variables that end in -tag and -fixed,
which are used for various functions, are created with this
function."
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
            (sync0-bibtex-obsidian-tagify ", "
                                          (if (string-match ", " ,var)
                                              (sync0-add-prefix-to-list-convert-to-string ,var ", " tag-prefix)
                                            (concat tag-prefix ,var)))))))

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
          (let ((x (cond ((string-match " and " ,var)
                          (let* ((no-comma (replace-regexp-in-string ", " "_" (downcase ,var)))
                                 (person (substring  ,(symbol-name var) 19))
                                 (person-string (concat ", " person "/"))
                                 (no-space (replace-regexp-in-string "[[:space:]]+" "-" no-comma)))
                            (replace-regexp-in-string "-and-" person-string no-space)))
                         ((string-match "^{" ,var)
                          (string-match "{\\([[:print:]]+\\)}" ,var)
                          (downcase (match-string 1 ,var)))
                         (t (let ((raw (replace-regexp-in-string ", " "_" ,var)))
                              (downcase (replace-regexp-in-string " " "-" raw)))))))
            (xah-replace-pairs-in-string
             x
             [["-de-" "–"]])))))

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

(defun sync0-bibtex-add-missing-braces (string)
  "Function to correct an error in bibtex-completion-get-value from
bibtex-completion package, in which the latex forms \textbf{...}
are retrieved without the last matching brace, which causes a lot
of crazy problems"
  (if (string-match "\\textit{[[:graph:] ]+[^}]" string)
      (concat string "}")
    string))

(defun sync0-bibtex-fix-obsidian-chars (arg)
  "Remove or replace characters that cause trouble in obsdian
markdown."
  (unless (sync0-null-p arg)
    (xah-replace-pairs-in-string-recursive
     (sync0-bibtex-add-missing-braces arg)
     [["\\textit{" " « "]
      ;; ["d'" ""]
      ;; ["d’" ""]
      ["\\&" "&"]
      ["\&" "&"]
      ["}" " » "]])))

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

(provide 'sync0-bibtex-corrections)
