(require 'sync0-macros)
(require 'bibtex-utils)
(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-corrections)
(require 'sync0-bibtex-utils)
(require 'sync0-citar)
(require 'sync0-projects)

(defun sync0-bibtex-nullify-all-variables ()
  "Set all bibtex variables required to do operations to nil to
prevent undesired results."
  (sync0-nullify-variable-list (append sync0-bibtex-entry-helper-fields-list
                                       sync0-bibtex-entry-definitions-list
                                       sync0-bibtex-entry-crossref-definitions-list
                                       sync0-bibtex-entry-extraction-fields-list
                                       sync0-bibtex-entry-initial-fields-list)))

(defvar sync0-bibtex-entry-functions
  '(("origtitle" (lambda ()
                   (setq sync0-bibtex-entry-origtitle
                         (sync0-bibtex-correct-smartquotes 
                          (completing-read "Origtitle of entry: " sync0-bibtex-completion-title)))))
    ("date" (lambda ()
              (setq sync0-bibtex-entry-date
                    (read-string "Date (ex. 1890-18-12) : " sync0-bibtex-entry-initial-date))))
    ("scheduled" (lambda ()
              (setq sync0-bibtex-entry-scheduled
                    (read-string "Scheduled : " (format-time-string "%Y-%m-%d")))))
    ("deadline" (lambda ()
              (setq sync0-bibtex-entry-deadline
                    (read-string "Deadline : " (format-time-string "%Y-%m-%d")))))
    ("year" (lambda ()
              (unless (sync0-null-p sync0-bibtex-entry-date)
                (setq sync0-bibtex-entry-year (substring-no-properties sync0-bibtex-entry-date 0 4)))))
    ("century" (lambda ()
                 (unless (sync0-null-p sync0-bibtex-entry-date)
		   (setq sync0-bibtex-entry-century
                         (let ((base-string (substring-no-properties sync0-bibtex-entry-date 0 2)))
                           (format "%s" (1+ (string-to-number base-string))))))))
    ("created" (lambda ()
                 (setq sync0-bibtex-entry-created-tag
                       (format-time-string "%Y/%m/%d"))
                 (setq sync0-bibtex-entry-created
                       (format-time-string "%Y-%m-%d"))))
    ("lastseen" (lambda ()
                 (setq sync0-bibtex-entry-lastseen-tag
                       (format-time-string "%Y/%m/%d"))
                 (setq sync0-bibtex-entry-lastseen
                       (format-time-string "%Y-%m-%d"))))
    ("origdate" (lambda ()
                  (setq sync0-bibtex-entry-origdate
                        (read-string "Origdate (ex. 1890-18-12) : " sync0-bibtex-entry-initial-origdate))))
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
    ("pagetotal" (lambda ()
                   (setq sync0-bibtex-entry-pagetotal
                         (if (sync0-null-p sync0-bibtex-entry-pages)
                             (read-string "Pagetotal: ")
                           (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" sync0-bibtex-entry-pages)
                               (when-let* ((beg (match-string 1 sync0-bibtex-entry-pages))
                                           (end (match-string 2 sync0-bibtex-entry-pages))
                                           (result (1+ (- (string-to-number end) (string-to-number beg))))) 
                                 (number-to-string result))
                             "1")))))
    ("origlanguage" (lambda ()
                      (setq sync0-bibtex-entry-origlanguage
                            (completing-read "Choose original language : "
                                             sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))))
    ("languages" (lambda ()
                (let ((x (completing-read-multiple "Languages: " sync0-bibtex-completion-language)))
                  (setq sync0-bibtex-entry-languages (sync0-show-elements-of-list x ", ")))))
    ("mention" (lambda ()
                 (setq sync0-bibtex-entry-mention (sync0-bibtex-choose-key))))
    ("mentioned" (lambda ()
                 (setq sync0-bibtex-entry-mentioned (sync0-bibtex-choose-key))))
    ("file" (lambda ()
              (if sync0-bibtex-entry-creation
                  (let* ((extension (or sync0-bibtex-entry-extension "pdf"))
                         (coda (concat "." extension ":" (upcase extension))))
                    (setq sync0-bibtex-entry-file
                          (concat ":" sync0-zkn-attachments-dir sync0-bibtex-entry-key coda)))
                (if sync0-bibtex-entry-file-old
                    (setq sync0-bibtex-entry-file
                          (let* ((attachments  (citar-get-value "file" sync0-bibtex-entry-key))
                                 (new-extension (completing-read "Choose extension to add: " sync0-bibtex-completion-extension))
                                 (new-extension-caps (upcase new-extension))
                                 (new-attach (concat ":" sync0-zkn-attachments-dir sync0-bibtex-entry-key "." new-extension ":" new-extension-caps)))
                          (concat attachments ";" new-attach)))
                  (setq sync0-bibtex-entry-file
                        (let* ((extension (or sync0-bibtex-entry-extension
                                              (completing-read "Choose extension to add: " sync0-bibtex-completion-extension)))
                               (extension-upcase (upcase extension)))
                          (concat ":" sync0-zkn-attachments-dir sync0-bibtex-entry-key "." extension ":" extension-upcase)))))))
    ("subtitle" (lambda ()
                   (setq sync0-bibtex-entry-subtitle
                         (sync0-bibtex-correct-smartquotes 
                         (completing-read "Subtitle : " sync0-bibtex-completion-title)))))
    ("origpublisher" (lambda ()
                   (setq sync0-bibtex-entry-origpublisher
                         (sync0-bibtex-correct-smartquotes 
                         (completing-read "Origpublisher : " sync0-bibtex-completion-publisher)))))
    ("booktitle" (lambda ()
                   (setq sync0-bibtex-entry-booktitle
                         (sync0-bibtex-correct-smartquotes 
                          (if (and sync0-bibtex-entry-crossref 
                                   sync0-bibtex-entry-crossref-entry)
                              (citar-get-value "title" sync0-bibtex-entry-crossref-entry)
                            (completing-read "Booktitle : " sync0-bibtex-completion-title))))))
    ("booksubtitle" (lambda ()
                      (setq sync0-bibtex-entry-booksubtitle
                            (sync0-bibtex-correct-smartquotes 
                             (if sync0-bibtex-entry-crossref 
                                 (citar-get-value "subtitle" sync0-bibtex-entry-crossref-entry)
                               (completing-read "Booksubtitle : " sync0-bibtex-completion-title))))))
    ("crossref" (lambda ()
                  (when (yes-or-no-p "Load crossref? ")
                    (setq sync0-bibtex-entry-crossref 
                          (sync0-bibtex-choose-key "Crossref: "))
                    (setq sync0-bibtex-entry-crossref-entry
                          (citar-get-entry sync0-bibtex-entry-crossref))
                    (setq sync0-bibtex-entry-initial-date
                          (citar-get-value "date" sync0-bibtex-entry-crossref-entry)
                          sync0-bibtex-entry-initial-origdate
                          (citar-get-value "origdate" sync0-bibtex-entry-crossref-entry)
                          sync0-bibtex-entry-initial-author
                          (if (member sync0-bibtex-entry-type sync0-bibtex-entry-editor-types)
                              (citar-get-value "editor" sync0-bibtex-entry-crossref-entry)
                            (citar-get-value "author" sync0-bibtex-entry-crossref-entry))
                          sync0-bibtex-entry-initial-language
                          (citar-get-value "language" sync0-bibtex-entry-crossref-entry)))))
    ("related" (lambda ()
                 (setq sync0-bibtex-entry-related (sync0-bibtex-choose-key))))
    ;; ("seen" (lambda ()
    ;;           (if sync0-bibtex-entry-creation
    ;;               (progn
    ;;                 (setq sync0-bibtex-entry-seen-tag
    ;;                       (format-time-string "%Y/%m/%d"))
    ;;                 (setq sync0-bibtex-entry-seen
    ;;                       (format-time-string "%Y-%m-%d")))
    ;;               (if (bound-and-true-p sync0-bibtex-entry-seen)
    ;;                   (let* ((x (completing-read-multiple "Seen: " sync0-bibtex-completion-seen))
    ;;                         (y (concat sync0-bibtex-entry-seen ", " x)))
    ;;                 (setq sync0-bibtex-entry-seen-tag (replace-regexp-in-string "-" "/" y))
    ;;                 (setq sync0-bibtex-entry-seen y))
    ;;               (progn
    ;;                 (setq sync0-bibtex-entry-seen-tag
    ;;                       (format-time-string "%Y/%m/%d"))
    ;;                 (setq sync0-bibtex-entry-seen
    ;;                       (format-time-string "%Y-%m-%d")))))))
    ("seen" (lambda ()
              (if (or sync0-bibtex-entry-creation
                      (not (bound-and-true-p sync0-bibtex-entry-seen)))
                  (setq sync0-bibtex-entry-seen
                        (format-time-string "%Y-%m-%d"))
                (let ((x (completing-read-multiple "Seen: " sync0-bibtex-completion-seen)))
                  (setq sync0-bibtex-entry-seen (concat sync0-bibtex-entry-seen ", " x))))))
    ("project" (lambda ()
		 (setq sync0-bibtex-entry-project
		       (sync0-show-elements-of-list
			(let ((translated-projects (mapcar (lambda (proj)
							     (or (cdr (assoc proj sync0-projects-alist)) proj))
							   sync0-bibtex-completion-project)))
			  (sync0-completing-read-projects translated-projects))
			", "))))
    ("keywords" (lambda ()
                  ;; Requires package unidecode for conversion to
                  ;; ASCII. See:
                  ;; https://github.com/sindikat/unidecode
                  (setq sync0-bibtex-entry-keywords
                      (sync0-bibtex-obsidian-keyword-cleanup
                        (unidecode (let (x)
                                     (dolist (element sync0-bibtex-tag-fields-list x) 
                                       (unless (sync0-null-p (symbol-value (caddr element)))
                                         (setq x (cons (concat (cadr element)  (sync0-bibtex-obsidian-keyword-tagify (eval (caddr element)))) x))))
                                     (sync0-show-elements-of-list x ", "))))))))
  "List of lists in which the car of each list is bibtex-field and
the cdr is a lambda function with all the actions that should be
carried to calculate the value it will take in a BibLaTeX entry.")

;; Set entry functions for biblatex fields that require completion of multiple
;; things.
(let ((x (cl-set-difference sync0-bibtex-string-multiple-fields '("keywords" "seen" "mention" "mentioned" "project") :test #'string=)))
  ;; Remove keywords to prevent overwriting the function it currently has
  (dolist (element x)
    (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
           (comp-var (intern (concat "sync0-bibtex-completion-" element)))
           (help-string (concat (upcase-initials element) ": "))
           (my-base-func  (list  'completing-read-multiple help-string comp-var)) 
           (my-list-string (list 'sync0-show-elements-of-list my-base-func ", "))
           (my-macro  (list 'setq my-var  my-list-string)) 
           (my-func (list 'lambda () my-macro))
           (my-cons (cons element (list my-func))))
      (push my-cons sync0-bibtex-entry-functions))))

;; Set functions for biblatex fields that are defined with read-string
(dolist (element sync0-bibtex-string-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (help-string (concat (upcase-initials element) ": "))
         (my-base-func  (list  'read-string help-string nil nil nil t)) 
         (my-macro  (list 'setq my-var  my-base-func)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))

;; Set functions for biblatex fields that require completion of people
(dolist (element sync0-bibtex-people-fields)
  (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
         (help-string (concat (upcase-initials element) "(s): "))
         (my-macro  (list  'sync0-bibtex-normalize-name-string my-var help-string)) 
         (my-func (list 'lambda () my-macro))
         (my-cons (cons element (list my-func))))
    (push my-cons sync0-bibtex-entry-functions)))

;; Set functions for biblatex fields that require completion of single
;; things.
(let* ((exceptions (list "language"))
       (x (cl-set-difference sync0-bibtex-completion-single-fields exceptions :test #'string=)))
  ;; Remove languages to prevent overwriting the function it currently has
  (dolist (element x)
    (let* ((my-var (intern (concat "sync0-bibtex-entry-" element)))
           (comp-var (intern (concat "sync0-bibtex-completion-" element)))
           (help-string (concat (upcase-initials element) ": "))
           (my-base-func  (list 'completing-read help-string comp-var)) 
           (correc-func  (list 'sync0-bibtex-correct-smartquotes my-base-func)) 
           (my-macro  (list 'setq my-var correc-func)) 
           (my-func (list 'lambda () my-macro))
           (my-cons (cons element (list my-func))))
      (push my-cons sync0-bibtex-entry-functions))))

(defun sync0-bibtex-entry-set-dummy-vars (entry fields)
  "Set values for BibLaTeX FIELDS using lambda functions defined in
sync0-bibtex-entry-functions. FIELDS must be a list of valid
BibLaTeX fields (e.g., those in sync0-bibtex-fields). ENTRY must
be specified in the format expected by
bibtex-completion (helm-bibtex) package."
  (unless (listp fields)
    (error "FIELDS must be a list of BibLaTeX field names"))
  (let ((field-alist nil)) ;; Collect results
    (dolist (field fields field-alist)
      (let* ((field-variable (concat "sync0-bibtex-entry-" field))
             (field-symbol (intern field-variable))
             (field-function (cadr (assoc field sync0-bibtex-entry-functions)))
             (field-value (if entry
                              (citar-get-value field entry)
                            (when field-function
                              (condition-case err
                                  (progn
                                    ;; Calculate the value and related variables
                                    (funcall field-function)
                                    ;; Retrieve the calculated value
                                    (symbol-value field-symbol))
                                (error
                                 (message "Error processing field '%s': %s" field err)))))))
        ;; Update variable and collect result
        (when field-value
          (set field-symbol field-value)
          (push (cons field-variable field-value) field-alist)
          (sync0-bibtex-update-var field))))
  ;; Optionally set this globally if required
  (setq sync0-bibtex-entry-fields-alist field-alist)))

(defun sync0-bibtex-entry-calculate-bibfield (bibfield)
  "Call corresponding function for bibfield from functions defined
in the variable sync0-bibtex-entry-functions to get the value for
bibfield. The value will be set to the corresponding
sync0-variables. For this function to run correctly, the
sync0-entry-variables must have been cleaned beforehand using
sync0-bibtex-nullify-all-variables functions. This function's
output is the value calculated by the called function."
  (if (member bibfield sync0-bibtex-fields)
      (funcall (cadr (assoc bibfield sync0-bibtex-entry-functions)))
    (error "%s is not part of BibLaTeX fields defined in sync0-bibtex-fields" bibfield)))

(defun sync0-bibtex-load-entry-at-point () 
  (interactive)
  (sync0-bibtex-nullify-all-variables)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
                                (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (type (cdr (assoc "=type=" entry)))
         (excluded-keys '("=key=" "=type="))
         (curated-alist (mapcar (lambda (pair)
                                  (let ((key (car pair))
                                        (value (cdr pair)))
                                    (cons (format "%s" key)
                                          (when value
                                            (format "%s"
                                                    (replace-regexp-in-string "[{}]" "" value))))))
                                entry))
         (final-alist (cl-remove-if (lambda (pair)
                                      (member (car pair) excluded-keys))
                                    curated-alist)))
    ;; Set global variables
    (setq sync0-bibtex-entry-fields (mapcar #'car final-alist)
          sync0-bibtex-entry-key bibkey
          sync0-bibtex-entry-type type
          sync0-bibtex-entry-alist final-alist)
    ;; Create dynamic variables for each field
    (dolist (field final-alist)
      (let* ((field-name (car field))
             (field-variable (concat "sync0-bibtex-entry-" field-name))
             (field-symbol (intern field-variable))
             (field-value (cdr field)))
        (when field-value
          (set field-symbol field-value))))
    (sync0-bibtex-entry-constitute-bibentry bibkey)
    ;; Return final-alist for debugging or further use
    final-alist))

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
  (let* ((entry (when bibkey (citar-get-entry bibkey)))
         (type (if entry
                   (sync0-bibtex-normalize-case
                    (citar-get-value "=type="  entry))
                 (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types)))
         ;; Specify which fields to load
         (fields (cond (bibkey
                        sync0-bibtex-fields)
                       (quick
                        (if (string= type (or "Collection" "MvCollection" "Proceedings"))
                          (cons "editor" sync0-bibtex-base-fields)
                          (cons "author" sync0-bibtex-base-fields)))
                       (t (append
                           (cdr (assoc type sync0-bibtex-type-fields))
                           sync0-bibtex-base-fields)))))
    (setq sync0-bibtex-entry-key (or bibkey 
                                     (sync0-bibtex-entry-key-define t)))
    (setq sync0-bibtex-entry-type type)
    ;; the following field is used to avoid inconsistensies in
    ;; case when using the type for boolean operations
    (setq sync0-bibtex-entry-type-downcase (downcase type))
    (setq sync0-bibtex-entry-author-or-editor-priority
	  (when sync0-bibtex-entry-author-or-editor-p
            (if (string= type (or "Collection" "MvCollection" "Proceedings"))
		"editor"
	      "author")))
    ;; Call the appropriate functions to define the biblatex fields.
    ;; Collect the result in an alist of the form:
    ;; ("sync0-bibtex-entry-title" . "The good old days"). Finally,
    ;; collect such conses into the list
    ;; sync0-bibtex-entry-fields-alist, which will be used by other
    ;; functions.
    (sync0-bibtex-entry-set-dummy-vars entry fields)
    ;; After loading (or defining) the fields for the biblatex entry,
    ;; it is necessary to define these helper functions. Otherwise,
    ;; errors occur when calling functions to extract pdfs, creating
    ;; notes, etc.
    ;; 
    ;; Correct fields to prevent conflicts with the crossref fields
    ;; present in an entry.
    (when sync0-bibtex-entry-crossref
      (sync0-bibtex-correct-crossref-fields))
      ;;; The following three functions are used to calculate the titles
      ;;; (and their accompanying dummy variables) of the Obsidian
      ;;; markdown notes and others.
       ;;; Configure the author dummy
       ;;; variables avoiding conflicts with the editor field. This
       ;;; part is also necessary to calculate the name of the person
       ;;; used in calculating entries.
          ;; Corrections
      (sync0-bibtex-correct-entry-fields)
      ;; Set parent extra field
      ;; (setq sync0-bibtex-entry-parent
      ;;       (cond ((string= sync0-bibtex-entry-type-downcase "article")
      ;;              sync0-bibtex-entry-journaltitle)
      ;;             ((or (string= sync0-bibtex-entry-type-downcase "inbook")
      ;;                  (string= sync0-bibtex-entry-type-downcase "incollection")
      ;;                  (string= sync0-bibtex-entry-type-downcase "inproceedings"))
      ;;              (if (sync0-null-p sync0-bibtex-entry-booksubtitle)
      ;;                  sync0-bibtex-entry-booktitle
      ;;                (concat sync0-bibtex-entry-booktitle sync0-bibtex-entry-separator sync0-bibtex-entry-booksubtitle)))
      ;;             (t sync0-bibtex-entry-series)))
      ;; keywors have to be calculated last in order to prevent empty
      ;; fields when defining the keywords
      (if bibkey
          (citar-get-value "keywords" entry)
        (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions))))
      (sync0-bibtex-entry-constitute-bibentry sync0-bibtex-entry-key)))

(provide 'sync0-bibtex-entry-functions)
