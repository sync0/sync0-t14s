(require 'sync0-corrections)

(defun sync0-bibtex-corrections-add-quotes-name (person)
  "Add quotes to person string."
  (when (stringp person)
          (cond ((string-match " and " person)
                 ;; create a list with parts 
                 (let* ((name-parts  (split-string person " and "))
                        (name-string (let (x)
                                       (dolist  (element name-parts x)
                                         (setq x (concat x element "\", \""))))))
                   (concat "\"" (substring name-string 0 -3))))
                ;; check when author is an organization
                ((string-match "^{" person)
                 (concat "\"" (substring person 1 -1) "\""))
                ;; other cases
                (t (concat "\"" person "\"")))))

(defun sync0-bibtex-corrections-reverse-name (person)
  "Correct the order of names from Last, First to First Last as
they are defined in the order of the completion bib variable.
This function takes a string as argument; otherwise fails."
  (when (stringp person)
    (let* ((author-list (split-string person ", "))
           (last-name (nth 0 author-list))
           (first-name (nth 1 author-list)))
      (concat first-name " " last-name))))

(defun sync0-bibtex-entry-select-draft-prefix ()
  (unless (sync0-null-p sync0-bibtex-entry-doctype)
    (cond ((sync0-string-member "draft" sync0-bibtex-entry-doctype)
	   (cond ((equal sync0-bibtex-entry-language "french")
		  "Ébauche : ")
		 ((equal sync0-bibtex-entry-language "english")
		  "Draft: ")
		 ((equal sync0-bibtex-entry-language "spanish")
		  "Esbozo: ")
		 ((equal sync0-bibtex-entry-language "portuguese")
		  "Esboço: ")
		 (t "Draft: ")))
	  ((sync0-string-member "proposal" sync0-bibtex-entry-doctype)
	   (cond ((equal sync0-bibtex-entry-language "french")
		  "Proposition : ")
		 ((equal sync0-bibtex-entry-language "english")
		  "Proposal: ")
		 ((equal sync0-bibtex-entry-language "spanish")
		  "Proposición: ")
		 ((equal sync0-bibtex-entry-language "portuguese")
		  "Proposta: ")
		 (t "Proposal: ")))
	  (t ""))))

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
              sync0-bibtex-entry-editor)
         (setq sync0-bibtex-entry-editor-over-author t)
         (setq sync0-bibtex-entry-author-or-editor-p t)
         ;; (sync0-bibtex-fix-names sync0-bibtex-entry-editor)
         (setq sync0-bibtex-entry-lastname
               (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-editor-lastname)))
        ((and (sync0-null-p sync0-bibtex-entry-editor)
               sync0-bibtex-entry-author)
         (setq sync0-bibtex-entry-editor-over-author nil)
         (setq sync0-bibtex-entry-author-or-editor-p t)
         ;; (sync0-bibtex-fix-names sync0-bibtex-entry-author)
         (setq sync0-bibtex-entry-lastname
               (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-author-lastname)))
        (t (setq sync0-bibtex-entry-author-or-editor-p t)
           (setq sync0-bibtex-entry-editor-over-author nil)
           ;; (sync0-bibtex-fix-names sync0-bibtex-entry-editor)
           ;; (sync0-bibtex-fix-names sync0-bibtex-entry-author)
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

(defun sync0-bibtex-filesystem-cleanup (stringy)
  "Correct characters forbidden in system filenames."
    (xah-replace-pairs-in-string
    stringy 
     [["/" "--"]]))

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
  (when (stringp my-string)
    (let ((x (downcase my-string)))
      (xah-replace-pairs-in-string-recursive
       x
       [["d'" ""]
        ["l'" ""]
        ;; ["-de-" ""]
        ["l’" ""]
        ["{" ""]
        ["}" ""]
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
        ["d’" ""]]))))

(defun sync0-bibtex-correct-smartquotes (my-string)
  "Make my-string compatible with the tags of obsidian by
downcasing and removing whitespace from tags to be included as
keywords in biblatex entries and obsidian markdown files."
  (when (stringp my-string)
    (xah-replace-pairs-in-string
     my-string
     [["'" "’"]
      ["--" "–"]
      ["---" "—"]])))

(defun sync0-bibtex-obsidian-tagify (sep my-string)
  "Make my-string compatible with the tags of obsidian by
downcasing and removing whitespace from tags to be included as
keywords in biblatex entries and obsidian markdown files."
  (when (stringp my-string)
    (let* ((nospace (s-trim sep))
           (regex (concat "[^" nospace "]\\([[:space:]]+\\)"))
           (nospace (replace-regexp-in-string regex "_" my-string nil t 1)))
      (downcase nospace))))

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
          (sync0-bibtex-corrections-add-quotes-name ,var))
          ;; (cond ((string-match " and " ,var)
          ;;        ;; create a list with parts 
          ;;        (let* ((name-parts  (split-string ,var " and "))
          ;;               (name-string (let (x)
          ;;                              (dolist  (element name-parts x)
          ;;                                (setq x (concat x element "\", \""))))))
          ;;          (concat "\"" (substring name-string 0 -3))))
          ;;       ;; check when author is an organization
          ;;       ((string-match "^{" ,var)
          ;;        (concat "\"" (substring ,var 1 -1) "\""))
          ;;       ;; other cases
          ;;       (t (concat "\"" ,var "\""))))
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
          (let* ((downcased (downcase ,var))
                 (x (cond ((string-match " and " ,var)
                           (let* ((no-comma (replace-regexp-in-string ", " "_" downcased))
                                  (person (substring  ,(symbol-name var) 19))
                                  (person-string (concat ", " person "/"))
                                  (no-space (replace-regexp-in-string "[[:space:]]+" "-" no-comma)))
                             (replace-regexp-in-string "-and-" person-string no-space)))
                          ((not (string-match "," ,var))
                           (xah-replace-pairs-in-string
                            downcased
                            [[" de la " "_"]
                             [" de " "_"]
                             [" la " "_"]
                             [" des " "_"]
                             ["-" "_"]]))
                          (t (xah-replace-pairs-in-string
                              downcased
                              [[" de la " "-"]
                               [" de " "-"]
                               [" la " "-"]
                               [" des " "-"]
                               [", " "_"]
                               [" " "-"]])))))
            (xah-replace-pairs-in-string
             x
             [["-de-" "–"]
             ["-la-" "–"]])))))

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

(defun sync0-bibtex-correct-entry-fields ()
  "Collection of functions with corrections necessary for correct formatting of entries."
    ;; Calculate the people fields; the reason for excluding author,
    ;; editor and translator fields is that these are necessary for
    ;; the cumbersome calculation of titles. 
    (dolist (element sync0-bibtex-people-fields)
      (let ((my-var (intern (concat "sync0-bibtex-entry-" element))))
        (eval `(sync0-bibtex-fix-names ,my-var))))
    ;; Calculate the translator field. This is calculated separately
    ;; to allow for some flexibility in including the translator
    ;; field, when present, in Obsidian markdown notes.
    (unless (sync0-null-p sync0-bibtex-entry-translator)
      ;; (sync0-bibtex-fix-names sync0-bibtex-entry-translator)
      (setq sync0-bibtex-entry-translator-lastname
            (sync0-bibtex-abbreviate-lastnames sync0-bibtex-entry-translator-lastname))
      (setq sync0-bibtex-entry-lastname
            (concat sync0-bibtex-entry-lastname " (" sync0-bibtex-entry-translator-lastname ")")))
    ;; Set author for title calculation
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
    ;; (sync0-bibtex-correct-journaltitle-keywords)
    ;; Define certain variables that end with "-tag" in order to have them be calculated and included in keywords (biblatex entries)
    (sync0-bibtex-correct-keywords "journaltitle")
    (sync0-bibtex-correct-keywords "library")
    (sync0-bibtex-correct-keywords "institution")
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
;; Correct problem with certain date tags that take multiple entries
    (when (bound-and-true-p sync0-bibtex-entry-seen-tag)
      (setq sync0-bibtex-entry-seen-tag 
            (xah-replace-pairs-in-string
             sync0-bibtex-entry-seen-tag 
             [["-" "/"]])))
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
                (concat "\"" sync0-bibtex-entry-project "\"")))))

(defun sync0-bibtex-corrections-format-bibtex-to-biblatex ()
  "Format bibtex keys in buffer to follow my BibLaTeX conventions."
  (let ((regex "^@\\([[A-z]+\\){[[:alnum:]]+,$"))
    (while (re-search-forward regex nil t)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry)))
             (type (cdr (assoc "=type=" entry)))
             (type-lowcase (downcase type))
             (author-full (when-let ((person (cdr (assoc "author" entry))))
                            (unless (string-match "^[[:print:]]+, " person)
                              (substring person 1 -1))))
             (year (when (assoc "year" entry)
                         (cdr (assoc "year" entry))))
             (author-list 
              (when author-full
                (split-string author-full " ")))
             (author-first (when author-list
                             (string-trim-whitespace (car author-list))))
             (author-last (when author-list
                            (string-trim-whitespace (cadr author-list))))
             (author (when (and author-first author-last)
                       (concat author-last ", " author-first)))
             (date (unless (assoc "date" entry)
		     (when year
                       (substring year 1 -1))))
	     (year-or-date (or year date))
	     (century (when year-or-date
			(format "%s" (1+ (string-to-number year-or-date)))))
             (journaltitle (when (string= type-lowcase "article")
                             (unless (assoc "journaltitle" entry)
                               (sync0-bibtex-get-field-value-at-point "journal" entry))))
             (new-key (if (string-match "^[[:digit:]]+[[:alpha:]]+" bibkey)
                          bibkey
                        (sync0-bibtex-entry-key-define)))
             (created (unless  (assoc "created" entry)
                        (format-time-string "%Y-%m-%d")))
             (new-path (unless (assoc "file" entry)
                         (concat ":" sync0-zettelkasten-attachments-directory new-key ".pdf:PDF")))
             (regex-journal "^[[:blank:]]+journal[[:blank:]]+=")
             (beg (save-excursion (bibtex-beginning-of-entry)))
             (end (save-excursion (bibtex-end-of-entry)))
             ;; (language (unless (assoc "language" entry)
             ;;             (completing-read "Choose language : "
             ;;                              sync0-bibtex-completion-language)))
             (language (unless (assoc "language" entry)
                         "french"))
             (title-full (sync0-bibtex-get-field-value-at-point "title" entry))
             (title-list 
              (when title-full
                (split-string title-full ":")))
             (title (when title-list
                      (string-trim-whitespace (car title-list))))
             (subtitle (when (> (length title-list) 1)
                         (string-trim-whitespace (cadr title-list))))
             (status (unless  (assoc "status" entry)
                       "fetch"))
             (type-string (concat "@" (upcase-initials type) "{" new-key ",\n")))
        (bibtex-beginning-of-entry)
        (re-search-forward regex end t 1) 
        (kill-whole-line 1)
        (insert type-string)
        ;; remove the journal field 
        (when journaltitle
          (re-search-forward regex-journal end t 1) 
          (kill-whole-line 1))
        (sync0-bibtex-create-field-at-entry "author" author t)
        (sync0-bibtex-create-field-at-entry "title" title t)
        (when subtitle
          (sync0-bibtex-create-field-at-entry "subtitle" subtitle))
        (sync0-bibtex-create-field-at-entry "journaltitle" journaltitle)
	(when date
          (sync0-bibtex-create-field-at-entry "date" date)
	  (unless year
            (sync0-bibtex-create-field-at-entry "year" date)))
        (sync0-bibtex-create-field-at-entry "created" created)
        (sync0-bibtex-create-field-at-entry "file" new-path)
        (sync0-bibtex-create-field-at-entry "language" language)
        (sync0-bibtex-create-field-at-entry "langid" language)
        (sync0-bibtex-create-field-at-entry "status" status)
        (bibtex-fill-entry)))))



(defun sync0-bibtex-corrections-ensure-bibtex-year-field ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "@\\(article\\|book\\|incollection\\|misc\\|conference\\){\\([^,]+\\)," nil t)
    (save-excursion
      (unless (re-search-forward "year\\s-*=" (save-excursion (bibtex-end-of-entry)) t)
        (message "Warning: Entry %s has no year field." (match-string 2))))))

;; (defun sync0-convert-online-to-misc ()
;;   "Convert BibLaTeX @online entries to Natbib-compatible @misc entries."
;;   (interactive)
;;   (goto-char (point-min))
;;   (while (re-search-forward "@online" nil t)
;;     (replace-match "@misc")))

;; (defun sync0-bibtex-convert-biblatex-to-natbib ()
;;   "Convert BibLaTeX fields to Natbib-compatible fields in the current buffer."
;;   (interactive)
;;   ;; Convert all BibTeX entry types in the current buffer to lowercase.
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "@\\([A-Za-z]+\\){" nil t)
;;       (replace-match (concat "@" (downcase (match-string 1)) "{") t t)))
;;   (message "BibTeX entry types converted to lowercase.")
;;   ;; Convert all BibTeX entry types in the current buffer to lowercase.
;;   (goto-char (point-min))
;;   (save-excursion
;;     (while (re-search-forward "\\(journaltitle\\|origdate\\|location\\|booktitle\\|\\[.*\\]\\)" nil t)
;;       (replace-match
;;        (cond
;; 	((string= (match-string 1) "journaltitle") "journal")
;; 	((string= (match-string 1) "origdate") "origyear")
;; 	((string= (match-string 1) "location") "address")
;; 	((string= (match-string 1) "booktitle") "book")
;; 	(t (match-string 1))) nil nil)))
;; ;;   "Delete lines containing specified BibTeX fields from the current buffer.
;; ;; The fields to be removed are hard-coded in the function."
;;   (let ((fields '("url" "urldate" "file" "abstract" "langid" "people" "theme"
;;                   "verba" "expages" "century" "verbb" "verbc" "source"
;;                   "project" "country" "library" "pagetotal" "related"
;;                   "relatedtype" "shorttitle" "shorthand" "cote" "format"
;;                   "doctype" "aliases" "status" "priority" "foreword"
;; 		  "introduction" "created"))
;;         (case-fold-search t)) ; Case-insensitive search
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (not (eobp))
;;         (let ((line (thing-at-point 'line t)))
;;           (when (cl-some (lambda (field)
;;                            (string-match (concat "\\b" (regexp-quote field) "\\b") line))
;;                          fields)
;;             (delete-region (line-beginning-position) (line-end-position))
;;             (forward-line -1))) ; Move back one line after deletion
;;         (forward-line 1)))) ; Move to the next line
;;   (message "Specified fields removed from the buffer."))

(defun sync0-convert-utf8-to-latex-accents ()
  "Convert accented UTF-8 characters in the buffer to LaTeX-compatible escape sequences using `xah-replace-pairs-in-region`."
  (interactive)
  (let ((accent-pairs
         [["á" "\\'{a}"]
          ["à" "\\`{a}"]
          ["â" "\\^{a}"]
          ["ä" "\\\"{a}"]
          ["ã" "\\~{a}"]
          ["å" "\\aa{}"]
          ["é" "\\'{e}"]
          ["è" "\\`{e}"]
          ["ê" "\\^{e}"]
          ["ë" "\\\"{e}"]
          ["í" "\\'{i}"]
          ["ì" "\\`{i}"]
          ["î" "\\^{i}"]
          ["ï" "\\\"{i}"]
          ["ó" "\\'{o}"]
          ["ò" "\\`{o}"]
          ["ô" "\\^{o}"]
          ["ö" "\\\"{o}"]
          ["õ" "\\~{o}"]
          ["ú" "\\'{u}"]
          ["ù" "\\`{u}"]
          ["û" "\\^{u}"]
          ["ü" "\\\"{u}"]
          ["ñ" "\\~{n}"]
          ["ç" "\\c{c}"]
          ["Á" "\\'{A}"]
          ["À" "\\`{A}"]
          ["Â" "\\^{A}"]
          ["Ä" "\\\"{A}"]
          ["Ã" "\\~{A}"]
          ["Å" "\\AA{}"]
          ["É" "\\'{E}"]
          ["È" "\\`{E}"]
          ["Ê" "\\^{E}"]
          ["Ë" "\\\"{E}"]
          ["Í" "\\'{I}"]
          ["Ì" "\\`{I}"]
          ["Î" "\\^{I}"]
          ["Ï" "\\\"{I}"]
          ["Ó" "\\'{O}"]
          ["Ò" "\\`{O}"]
          ["Ô" "\\^{O}"]
          ["Ö" "\\\"{O}"]
          ["Õ" "\\~{O}"]
          ["Ú" "\\'{U}"]
          ["Ù" "\\`{U}"]
          ["Û" "\\^{U}"]
          ["’" "'"]
          ["Ü" "\\\"{U}"]
          ["Ñ" "\\~{N}"]
          ["«" "\\guillemotleft"]
          ["»" "\\guillemotright"]
          ["Ç" "\\c{C}"]
          ;; German special characters
          ["ß" "\\ss{}"]
          ["ø" "\\o{}"]
          ["Ø" "\\O{}"]]))
    (xah-replace-pairs-region (point-min) (point-max) accent-pairs)))

(defun sync0-bibtex-convert-biblatex-to-natbib ()
  "Convert BibLaTeX fields to Natbib-compatible fields and remove specified fields in the current buffer.
This function performs the following operations:
1. Converts all BibTeX entry types to lowercase.
2. Replaces specific BibTeX fields with their Natbib-compatible counterparts.
3. Deletes lines containing specified BibTeX fields."
  (interactive)
  
  ;; Convert BibTeX entry types to lowercase
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "@\\([A-Za-z]+\\){" nil t)
      (replace-match (concat "@" (downcase (match-string 1)) "{") t t)))

  ;; Replace specific BibTeX fields with Natbib-compatible fields
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(journaltitle\\|origdate\\|location\\|booktitle\\|\\[.*\\]\\)" nil t)
      (replace-match
       (cond
        ((string= (match-string 1) "journaltitle") "journal")
        ((string= (match-string 1) "origdate") "origyear")
        ((string= (match-string 1) "location") "address")
        ((string= (match-string 1) "booktitle") "book")
        (t (match-string 1))) nil nil)))

  ;; Format date field appropriately
  (let ((case-fold-search t)) ; Case-insensitive search
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*date\\s-*=[^}]*" nil t)
        (let* ((date-field (thing-at-point 'line t))
               (date-value (if (string-match "\\s-*=\\s-*{\\([^}]*\\)}" date-field)
                               (match-string 1 date-field)
                             ""))
               (parts (split-string date-value "-"))
               (year (if (>= (length parts) 1) (nth 0 parts) ""))
               (month (if (>= (length parts) 2) (nth 1 parts) ""))
               (day (if (>= (length parts) 3) (nth 2 parts) "")))
          ;; Remove the old date field
          (delete-region (line-beginning-position) (line-end-position))
          (forward-line -1)
          ;; Add new year, month, and day fields
          (insert (format "  year = {%s},\n" year))
          (when (and month (not (string-empty-p month)))
            (insert (format "  month = {%s},\n" month)))
          (when (and day (not (string-empty-p day)))
            (insert (format "  day = {%s},\n" day)))
          ;; Add remaining fields
          (insert (thing-at-point 'line t))))))
  (message "Date fields reformatted to Natbib-compatible format.")

  ;; Delete lines containing specified BibTeX fields
  (let ((fields '("url" "urldate" "file" "abstract" "langid" "people" "theme"
                  "verba" "expages" "century" "verbb" "verbc" "source"
                  "project" "country" "library" "pagetotal" "related"
                  "relatedtype" "shorttitle" "shorthand" "cote" "format"
                  "doctype" "aliases" "status" "priority" "foreword"
                  "introduction" "created"))
        (case-fold-search t)) ; Case-insensitive search
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (thing-at-point 'line t)))
          (when (cl-some (lambda (field)
                           (string-match (concat "\\b" (regexp-quote field) "\\b") line))
                         fields)
            (delete-region (line-beginning-position) (line-end-position))
            (forward-line -1))) ; Move back one line after deletion
        (forward-line 1)))) ; Move to the next line

  (message "BibLaTeX fields converted to Natbib-compatible fields and specified fields removed from the buffer."))

(defun sync0-bibtex-format-date-to-natbib ()
  "Format the 'date' field to 'year', 'month', and 'day' in the current BibTeX buffer."
  (interactive)
  (let ((case-fold-search t)) ; Case-insensitive search
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*date\\s-*=[^}]*" nil t)
        (let* ((date-field (thing-at-point 'line t))
               (date-value (if (string-match "\\s-*=\\s-*{\\([^}]*\\)}" date-field)
                               (match-string 1 date-field)
                             ""))
               (parts (split-string date-value "-"))
               (year (if (>= (length parts) 1) (nth 0 parts) ""))
               (month (if (>= (length parts) 2) (nth 1 parts) ""))
               (day (if (>= (length parts) 3) (nth 2 parts) "")))
          ;; Remove the old date field
          (delete-region (line-beginning-position) (line-end-position))
          (forward-line -1)
          ;; Add new year, month, and day fields
          (insert (format "  year = {%s},\n" year))
          (when (and month (not (string-empty-p month)))
            (insert (format "  month = {%s},\n" month)))
          (when (and day (not (string-empty-p day)))
            (insert (format "  day = {%s},\n" day)))
          ;; Add remaining fields
          (insert (thing-at-point 'line t))))))
  (message "Date fields reformatted to Natbib-compatible format."))

(setq my-bib-alist
      '(("anf" . "/home/sync0/Gdrive/bibpubs/24263gu_anf.bib")
        ("add" . "/home/sync0/Gdrive/bibpubs/24263gu_add.bib")
        ("adhv" . "/home/sync0/Gdrive/bibpubs/24263gu_adhv.bib")
        ("aml" . "/home/sync0/Gdrive/bibpubs/24263gu_aml.bib")))

(defun my-modify-citations ()
  "Modify citation commands in the current buffer based on my-bib-alist."
  (interactive)
  (let ((bib-key-alist '()))  ;; Temporary alist to store bibkey and labels
    ;; Build the bib-key-alist from my-bib-alist
    (dolist (bibfile my-bib-alist)
      (let ((label (car bibfile))
            (path (cdr bibfile)))
        ;; Read the bib file and extract citation keys
        (with-temp-buffer
          (insert-file-contents path)
          (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
            (let ((bibkey (match-string 1)))
              ;; Add the bibkey and its corresponding label to the alist
              (push (cons bibkey label) bib-key-alist))))))
    
    ;; Regex to match different citation commands
    (let ((citation-regexp "\\\\cite\\(t\\|p\\|alp\\|alt\\|author\\|year\\|text\\)*\\(\\[.*?\\]\\)?{\\([^}]+\\)}"))
      
      ;; Define a local function to modify citations recursively
      (defun modify-citation ()
        (let ((pos (point)))
          (while (re-search-forward citation-regexp nil t)
            (let ((cmd (match-string 0))
                  (options (match-string 2))
                  (key (match-string 3)))
              ;; Find the corresponding label for the bibkey
              (let ((label (cdr (assoc key bib-key-alist))))
                (when label
                  ;; Modify the citation command to include the label
                  (replace-match (concat "\\\\cite" label options "{" key "}") t))
              ;; Check for nested commands and apply this function recursively
              (when (looking-back "\\\\citetext{")
                (modify-citation)))))))
      
      ;; Perform the modification in the current buffer
      (save-excursion
        (goto-char (point-min))
        (modify-citation)))))

(provide 'sync0-bibtex-corrections)

