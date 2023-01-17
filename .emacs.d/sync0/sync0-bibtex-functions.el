;; -*- lexical-binding: t -*-

;; (defmacro defvar* (vars initial-value)
;;  `(progn
;;     ,@(loop for var in vars
;;             do (check-type var symbol)
;;             collect `(defvar ,var ,initial-value))))

(require 'unidecode)
(require 'bibtex-completion)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-entry-functions)
(require 'sync0-print)
(require 'sync0-pdf)
(require 'sync0-yaml)
(require 'sync0-pandoc)
(require 'sync0-bibtex-var-functions)
(require 'sync0-bibtex-extraction)
(require 'sync0-bibtex-diagnostics)
(require 'sync0-bibtex-corrections)
(require 'sync0-bibtex-obsidian)
(require 'xah-replace-pairs)

(defun sync0-bibtex-buffer-p ()
  "Check whether current buffer is visiting a bibtex file."
  (string= (file-name-extension (buffer-file-name)) "bib"))

(defun sync0-bibtex-choose-attachment (&optional bibkey extension)
  "REWRITE! Function used to select an attachment to be read by
some other programs. This function is inteded to be used in
pipes and not standalone. This function requires package
bibtex-completion to be loaded; otherwise, fails."
  (interactive)
  (let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (predicate (when extension
                      (if (string-match-p "^\\." extension)
                          (lambda (x) (string-match extension x))
                        (lambda (x) (string-match (concat ".+\\." extension) x)))))
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
    (when (and (or (string= sync0-bibtex-entry-type-downcase "incollection")
                   (string= sync0-bibtex-entry-type-downcase "inbook")
                   (string= sync0-bibtex-entry-type-downcase "inproceedings"))
               (yes-or-no-p "Extract  PDF from existing entry?"))
      (sync0-bibtex-extract-pdf-from-crossref))
    ;; Insert entry in default bibliography file
    (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key)
    (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)
    ;; the function below does not work; throws an error listp that I
    ;; have not been able to solve (sync0-bibtex-update-vars
    ;; sync0-bibtex-completion-variables-list)
    (unless (sync0-null-p sync0-bibtex-entry-url)
      (when (yes-or-no-p "Download the attached pdf? ")
        (sync0-bibtex-download-pdf sync0-bibtex-entry-key t))))

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

(defun sync0-bibtex-download-pdf (&optional refkey creation)
  (interactive)
  (if creation
      (sync0-pdf-download-from-url sync0-bibtex-entry-url (string-trim sync0-bibtex-entry-file ":" ":PDF"))
    (when-let*    ((bibkey (or refkey 
                               (sync0-bibtex-completion-choose-key t t)))
                   (entry (bibtex-completion-get-entry bibkey))
                   (url (bibtex-completion-get-value "url" entry))
                   (pdf (concat sync0-zettelkasten-attachments-directory bibkey ".pdf")))
      (sync0-pdf-download-from-url url pdf))))


  ;; (defun sync0-bibtex-update-key-in-bibfile (oldkey newkey bibfile)
  ;;   "Change bibtex key at point with a key using the format provided
  ;; by org-roam files"
  ;;   (with-temp-file bibfile
  ;;     (insert-file-contents bibfile)
  ;;     (goto-char (point-min))
  ;;     (while (re-search-forward oldkey nil t)
  ;;       (replace-match newkey))))

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
      ;; (save-excursion
      ;;   (when (re-search-forward "\\(year\\)[[:blank:]]+=" end t 1)
      ;;     (replace-match "date" nil nil nil 1)))
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

  (defun sync0-bibtex-create-note-from-entry (&optional rewrite refkey)
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
      ;; (unless no-extract
      ;;   (when (or (string= sync0-bibtex-entry-type-downcase "incollection")
      ;;             (string= sync0-bibtex-entry-type-downcase "inbook")
      ;;             (string= sync0-bibtex-entry-type-downcase "inproceedings"))
      ;;     (sync0-bibtex-extract-pdf-from-crossref)))
      (if rewrite
          (sync0-bibtex-entry-create-obsidian-note-from-entry bibkey t)
        (sync0-bibtex-entry-create-obsidian-note-from-entry bibkey))))

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

  (defun sync0-bibtex-open-crossref-pdf (&optional zathura)
    "Open the pdf for bibtex key under point if it exists. If
   optional zathura is t, use zathura to open the pdf."
    (interactive)
    (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
           (file (sync0-bibtex-choose-attachment sync0-bibtex-entry-crossref)))
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
             (trash-attach-message (format "Entry %s has %s attachments. Do you want to send these to trash?" refkey attachments))
             (end (save-excursion (bibtex-end-of-entry))))
        (when (yes-or-no-p trash-message)
          (delete-region beginning end))
        (when (yes-or-no-p trash-attach-message)
          (mapc #'move-file-to-trash attachments)))))

(defun sync0-bibtex-delete-attachments (&optional bibkey)
  "Choose an entry to permanently delete. Remeber: The deleted
entry could be recovered if previously commited on a
version-controlled file. The attachments are sent to the system
trash, as defined by the desktop environments (KDE, Gnome,
etc.)."
  (interactive)
  (let ((refkey (or bibkey
                    (sync0-bibtex-completion-choose-key t t))))
    (sync0-bibtex-completion-load-entry refkey)
    (let* ((attachments (bibtex-completion-find-pdf refkey))
           (num-of-attachments (length attachments))
           (trash-attach-message
            (format "Entry %s has %s attachments. Do you want to send these to trash?" refkey num-of-attachments)))
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
           (keywords-list (when sync0-bibtex-entry-keywords
                                 (split-string sync0-bibtex-entry-keywords ", "))))
           ;; (extra-keywords (completing-read-multiple "Input extra keywords: " 
           ;;                                           sync0-bibtex-completion-keywords))
           ;; (master-list (if (sync0-null-p extra-keywords)
           ;;                  keywords-list
           ;;                (cl-union keywords-list extra-keywords))))
      (setq sync0-bibtex-entry-keywords (sync0-show-elements-of-list keywords-list ", "))
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
          (sync0-bibtex-entry-create-obsidian-note-from-entry sync0-bibtex-entry-key)))))
      ;; Fourth, add a markdown link in the obsidian master note
      ;; (ie., the note corresponding to the file used as the source
      ;; for extraction).
      ;; (append-to-file obsidian-reference nil obsidian-master-note)
      ;; (unless (sync0-null-p sync0-bibtex-entry-crossref)
      ;;   (bibtex-search-entry sync0-bibtex-entry-crossref)
      ;;   ;; (bibtex-beginning-of-entry)
      ;;   (bibtex-make-field (list "relatedtype" "Whatever string" "multivolume" nil) t)
      ;;   (bibtex-make-field (list "related" "Whatever string" (sync0-show-elements-of-list keylist ", ") nil) t))

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

(defun sync0-add-field-theme (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting 
the empty string will quit the prompt. If the keyword is not already
present in the buffer, it will be added to the local variable
bu-keywords-values. Note that if you use ido-ubiquitous, the value of
  `ido-ubiquitous-enable-old-style-default' is temporarily set to t within
the body of this command."
  (interactive "P")
  (let ((elist (save-excursion (bibtex-beginning-of-entry)
			       (bibtex-parse-entry)))
        (ido-ubiquitous-enable-old-style-default t)
	append)
    (if (assoc "theme" elist)
	(progn (setq append t)
	       (bibtex-beginning-of-entry)
	       (goto-char 
		(car (last (or (bibtex-search-forward-field "theme" t)
                               (progn (setq append nil)
                                      (bibtex-search-forward-field "OPTtheme" t)))))))
      (bibtex-make-field "theme" t nil))
    (skip-chars-backward "}\n")
    (unless arg
      (let ((cnt 0)
            k)
	(while (and (setq k (completing-read 
                             "Theme (RET to quit): " sync0-bibtex-completion-theme nil))
		    (not (equal k "")))
	  (when append (insert ", ")
                (setq append nil))
	  (setq cnt (1+ cnt))
	  (insert (format "%s%s" (if (> cnt 1) ", " "") k))
          ;; (add-to-list 'sync0-bibtex-completion-theme k)
          )))))

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
      ("A" sync0-bibtex-archive-entry "Archive entry")
      ("1" sync0-bibtex-file-exists-p "Check file exists"))
     "PDF editing"
     (("x" sync0-bibtex-extract-from-crossref "Extract from crossref")
      ("X" sync0-bibtex-delete-attachments "Delete attachments")
      ("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
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
      ("Z" (sync0-bibtex-open-crossref-pdf t) "Open in zathura")
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
      ("k" sync0-add-field-theme "Add theme")
      ("y" sync0-bibtex-yank-citation-from-bibkey "Yank citation.")
      ("N" sync0-bibtex-create-note-from-entry "Create md note for entry")
      ("R" (sync0-bibtex-create-note-from-entry t) "Rewrite md for entry"))))

  (provide 'sync0-bibtex-functions)
