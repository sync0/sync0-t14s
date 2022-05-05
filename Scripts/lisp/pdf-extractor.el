(defun sync0-pdf-page-extractor ()
  "Extract as a separate pdf the pages within the page rage
specified by beg and end"
  (interactive)
  (let* ((beg (read-string "Première page : "))
         (end (read-string "Dernière page : "))
         (candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates)))
         (selection (ivy-read "Crossref : "
                              candidates
                              :preselect preselect
                              :caller 'ivy-bibtex
                              :history 'ivy-bibtex-history))
         (chosen-key (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
         (file (car (bibtex-completion-find-pdf chosen-key)))
       ;;  (entry (bibtex-completion-get-entry chosen-key))
         ;; (file (bibtex-completion-get-value "file" entry))
         (command (concat "pdfpextr " beg " " end " " sync0-pdfs-folder chosen-key ".pdf")))
    (insert command)))
    ;; (shell-command command)))

            (call-process "zathura" nil 0 nil pdf-file)
        (message "No PDF found for %s" key)


;; version 1 (works)
(defun sync0-pdf-page-extractor ()
  "Extract as a separate pdf the pages within the page rage
specified by beg and end"
  (interactive)
  (let* ((beg (read-string "Première page : "))
         (end (read-string "Dernière page : "))
         (results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (file (car (bibtex-completion-find-pdf key)))
         (command (concat
                   "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=" beg " -dLastPage=" end " -sOutputFile=" sync0-pdfs-folder key "_" beg "-" end".pdf " file)))
    (if (file-exists-p file)
        (shell-command command)
      (message "No PDF found for %s" key))))

;;: version 2
(defun sync0-pdf-page-extractor ()
  "Extract as a separate pdf the pages within the page rage
specified by beg and end"
  (interactive)
  (let* ((beg (read-string "Première page : "))
         (end (read-string "Dernière page : "))
         (candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates)))
         (selection (ivy-read "Crossref : "
                              candidates
                              :preselect preselect
                              :caller 'ivy-bibtex
                              :history 'ivy-bibtex-history))
         (chosen-key (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
         (file (car (bibtex-completion-find-pdf chosen-key)))
         (command (concat
                   "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=" beg " -dLastPage=" end " -sOutputFile=" sync0-pdfs-folder key "_" beg "-" end".pdf " file)))
    (if (file-exists-p file)
        (shell-command command)
      (message "No PDF found for %s" key))))


    ;; (defun sync0-org-ref-open-pdf-at-point-zathura ()
    ;;   "Open the pdf for bibtex key under point if it exists."
    ;;   (interactive)
    ;;   (let* ((results (org-ref-get-bibtex-key-and-file))
    ;;          (key (car results))
    ;;          (pdf-file (car (bibtex-completion-find-pdf key))))
    ;;     (if (file-exists-p pdf-file)
    ;;         (call-process "zathura" nil 0 nil pdf-file)
    ;;     (message "No PDF found for %s" key))))


          ((equal property "PROJECT_TITLE")
           (let* ((x (completing-read-multiple "Quel project ?"
                                               sync0-zettelkasten-projects))
                  (y 
                   (if (> (length x) 1)
                       (string-trim
                        (prin1-to-string x)
                        "(" ")")
                     (concat "\""   (car x) "\""))))
             (org-set-property property y)))



;; the extractor right now extracts the pdf from the input pdf,
;; but we must be able to create a zettel from it and also add it
;; to the bibtex bibliography. This will greatly reduce the human
;; input needed to manage the bibliography


(defun sync0-org-ref-get-citation-date (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "date" (bibtex-parse-entry t))))))

(defun sync0-org-ref-get-citation-language (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "language" (bibtex-parse-entry t))))))

(defun sync0-org-ref-get-citation-author (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "author" (bibtex-parse-entry t))))))



(defun sync0-pdf-extract-and-derive ()
  "Extract as a separate pdf the pages within the page rage
specified by beg and end"
  (interactive)
;; first, define the things to use the extractor 
  (let* (

      (beg (read-string "Première page : "))
      (end (read-string "Dernière page : "))
      (results (org-ref-get-bibtex-key-and-file))
      (key (car results))
      (file (car (bibtex-completion-find-pdf key)))

      ;; prepare the bibtex entry
      (type (completing-read "BibLaTex entry type: " sync0-biblatex-entry-types))
      (id (org-id-new))
      (filename (format-time-string "%Y%m%d%H%M%S"))
      (creation (format-time-string "%Y-%m-%d")) 
      (initial-date (sync0-org-ref-get-citation-date key))
      (date (read-string "Date (ex. 1890-18-12) : " initial-date))
      (initial-author (sync0-org-ref-get-citation-author key))
      (author (completing-read "Auteur : "
                                 (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                                        (bibtex-completion-candidates))) nil nil initial-author))
      ;; check whether there are multiple authors
      (author-fixed (cond ((string-match " and " author)
                           ;; create a list with parts 
                           (let* ((author-list  (split-string author " and "))
                                  (names (let (x)
                                           (dolist  (element author-list x)
                                             (setq x (concat x
                                                             (progn
                                                               (string-match ", \\([[:graph:]]+\\)$"   element)
                                                               (match-string 1 element))
                                                             " "
                                                             (progn
                                                               (string-match "\\([[:graph:]]+\\),"   element)
                                                               (match-string 1 element))
                                                             ", "))))))
                             (substring names 0 -2)))
                          ;; check when author is an organization
                          ((string-match "^{" author)
                           (string-match "{\\([[:print:]]+\\)}" author)
                           (match-string 1 author))
                          ;; other cases
                          (t (let* ((author-list (split-string author ", "))
                                    (last-name (nth 0 author-list))
                                    (first-name (nth 1 author-list)))
                               (concat first-name " " last-name)))))
      (initial-language (sync0-org-ref-get-citation-language key))
      (language (completing-read "Langue : " sync0-biblatex-languages nil nil initial-language))
      (langid language) 
      (pages (read-string "Pages (ex. : 90-180) : "))
      (title (read-string "Titre du texte : " nil nil nil t))
      (subtitle (read-string "Sous-titre du texte : " nil nil nil t))
      (derived-file (concat "/home/sync0/Documents/pdfs/" filename ".pdf"))
      ;; (buffer (buffer-file-name))     
           (biblatex-definitions (list 
                                  title
                                  subtitle
                                  date
                                  ;; origdate
                                  author
                                  ;; journal
                                  ;; booktitle
                                  ;; booksubtitle
                                  crossref
                                  ;; volume
                                  ;; number
                                  ;; publisher
                                  ;; location
                                  pages
                                  ;; addendum
                                  ;; url
                                  ;; urldate
                                  language
                                  langid
                                  derived-file))
           (fields (mapcar* #'(lambda (x y) (list x y)) sync0-biblatex-fields biblatex-definitions))
           ;; define the biblatex entries
           (entries
            (let (x)
              (dolist (element fields x) 
                (unless (or (null (cadr element))
                            (equal (cadr element) ""))
                  (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
           ;; select target bibliography file (.bib)
           (bib-file (completing-read "Fichier BibLaTeX : "
                                      (f-files "~/Dropbox/bibliographies" (lambda (k) (string-match-p ".bib" k)))))
           ;; create string of new biblatex entry
           (biblatex-entry (concat "\n@" type "{" filename "," "\n" entries "\n}\n")))

      (append-to-file biblatex-entry nil bib-file)
      ;; define certain varibles to construct the path with another function
      (setq  sync0-zettel-filename filename)
      (setq sync0-zettel-path (concat sync0-zettelkasten-directory "reference"))
      (concat
       ":PROPERTIES:\n"
       ":ID:      " id "\n"
       ":ROAM_REFS: cite:" filename "\n"
       (unless (or (null crossref)
                  (equal crossref ""))
       (concat ":CROSSREF: cite:" crossref "\n"))
       ":BIBLATEX_TYPE: " type "\n"
       (when (equal type "article") (concat ":JOURNAL_TITLE: \"" journal "\"\n"))
       (when (equal type "online") (concat ":WEBSITE: " url "\n"))
       ":AUTHOR: \"" author-fixed "\"\n"
       ":CREATED: " creation "\n"
       ":LAST_MODIFIED: " creation "\n"
       ":ZETTEL_TYPE: reference\n"
       ":LANGUAGE: " language "\n"
       ":DATE: \"" date "\"\n" 
       (unless (equal origdate "") (concat ":ORIG_DATE: \"" origdate "\"\n"))
       ":END:\n"
       "#+TITLE: " title "\n"
       (unless (equal subtitle "") (concat "#+SUBTITLE: " subtitle "\n"))
       "#+AUTHOR: " author-fixed "\n"
       "#+FILETAGS: :" filename ":" type ":" date ":\n"
       "#+INTERLEAVE_PDF: " derived-file "\n\n"))




(command (concat
    "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage=" beg " -dLastPage=" end " -sOutputFile=" sync0-pdfs-folder key "_" beg "-" end".pdf " file)))

        (if (file-exists-p file)
            ;; (insert command)
      (shell-command command)
        (message "No PDF found for %s" key))



  (defun sync0-org-capture-reference ()
    (let* ((type (if (equal (org-capture-get :key) "w")
                     "online"
                   (completing-read "Choose BibLaTex entry type: " sync0-biblatex-entry-types)))
           (id (org-id-new))
           (filename (format-time-string "%Y%m%d%H%M%S"))
           (creation (format-time-string "%Y-%m-%d")) 
           (derivation (yes-or-no-p "Derive entry?"))
           (crossref (unless (null derivation)
                       (let* ((candidates (bibtex-completion-candidates))
                              (key (bibtex-completion-key-at-point))
                              (preselect (and key
                                              (cl-position-if (lambda (cand)
                                                                (member (cons "=key=" key)
                                                                        (cdr cand)))
                                                              candidates)))
                              (selection (ivy-read "Crossref : "
                                                   candidates
                                                   :preselect preselect
                                                   :caller 'ivy-bibtex
                                                   :history 'ivy-bibtex-history)))
                         (cdr (assoc "=key=" (cdr (assoc selection candidates)))))))
           (initial-date (unless (null derivation)
                           (sync0-org-ref-get-citation-date crossref)))
           (date (read-string "Date (ex. 1890-18-12) : " initial-date))
           (initial-author (unless (null derivation)
                             (sync0-org-ref-get-citation-author crossref)))
           ;; (date (read-string "Date (ex. 1890-18-12) : "))
           (origdate (read-string "Origdate (ex. 1890-18-12) : "))
           (author (if (equal type "collection")
                       (completing-read "Editeur : "
                                        (delete-dups (mapcar #'(lambda (x) (cdr (assoc "editor" x)))
                                                               (bibtex-completion-candidates))))
                     (if (null derivation)
                         (completing-read "Auteur : "
                                          (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                                                 (bibtex-completion-candidates))))
                       (completing-read "Auteur : "
                                        (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                                               (bibtex-completion-candidates)))
                                        nil nil initial-author))))
           ;; check whether there are multiple authors
           (author-fixed (cond ((string-match " and " author)
                                ;; create a list with parts 
                                (let* ((author-list  (split-string author " and "))
                                       (names (let (x)
                                                (dolist  (element author-list x)
                                                  (setq x (concat x
                                                                  (progn
                                                                    (string-match ", \\([[:graph:]]+\\)$"   element)
                                                                    (match-string 1 element))
                                                                  " "
                                                                  (progn
                                                                    (string-match "\\([[:graph:]]+\\),"   element)
                                                                    (match-string 1 element))
                                                                  ", "))))))
                                  (substring names 0 -2)))
                               ;; check when author is an organization
                               ((string-match "^{" author)
                                (string-match "{\\([[:print:]]+\\)}" author)
                                (match-string 1 author))
                               ;; other cases
                               (t (let* ((author-list (split-string author ", "))
                                         (last-name (nth 0 author-list))
                                         (first-name (nth 1 author-list)))
                                    (concat first-name " " last-name)))))
           (initial-language (unless (null derivation)
                               (sync0-org-ref-get-citation-language key)))
           (language (if (null initial-language)
                         (completing-read "Choose language : " sync0-biblatex-languages)
                       (completing-read "Choose language : "
                                        sync0-biblatex-languages nil nil initial-language)))
           (langid language) 
           (journal (when (equal type "article")
                      (completing-read "Titre du journal : "
                                       (delete-dups (mapcar #'(lambda (x) (cdr (assoc "journaltitle" x)))
                                                              (bibtex-completion-candidates))))))
           (volume
            (unless (null derivation)
              (when (or (equal type "article")
                        (equal type "book")
                        (equal type "inbook")
                        (equal type "collection")
                        (equal type "incollection"))
                (read-string "Tome : "))))
           (number (when (equal type "article")
                     (read-string "Numero : ")))
           (publisher (when (or (equal type "book")
                                (equal type "collection"))
                        (completing-read "Maison d'edition : "
                                         (delete-dups (mapcar #'(lambda (x) (cdr (assoc "publisher" x)))
                                                              (bibtex-completion-candidates))))))
           (location (when (equal type "book")
                       (completing-read "Location : "
                                        (delete-dups (mapcar #'(lambda (x) (cdr (assoc "location" x)))
                                                             (bibtex-completion-candidates))))))
           (pages (when (or (equal type "article")
                            (equal type "incollection")
                            (equal type "inbook"))
                    (read-string "Pages (ex. : 90-180) : ")))
           (booktitle (when (and (or (equal type "incollection")
                                     (equal type "inbook"))
                                     (null derivation))
                        (completing-read "Titre du livre (booktitle) : "
                                         (delete-dups (mapcar #'(lambda (x) (cdr (assoc "booktitle" x)))
                                                              (bibtex-completion-candidates))))))
           (booksubtitle (when (and (or (equal type "incollection")
                                        (equal type "inbook"))
                                    (null crossref))
                           (read-string "Soustitre du livre (booksubtitle)  : ")))
           (addendum (when (equal type "unpublished")
                       (read-string "Addendum (ex. Box, Folder, etc.) : ")))
           (url (when (equal type "online")
                  (read-string "Url : " nil nil nil t)))
           (urldate (when (equal type "online") creation))
           (title (read-string "Titre du texte : " nil nil nil t))
           (subtitle (read-string "Sous-titre du texte : " nil nil nil t))
           (file (concat "/home/sync0/Documents/pdfs/" filename ".pdf"))
           (buffer (buffer-file-name))     
           ;; in case of extraction define these
           (beg (unless (null derivation)
            (read-string "First page for extraction: ")))
           (end (unless (null derivation)
            (read-string "Last page for extraction: ")))
           (origfile (unless (null derivation)
            (car (bibtex-completion-find-pdf crossref))))
           (command (unless (null origfile)
                      (concat
                       "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="
                       beg
                       " -dLastPage="
                       end
                       " -sOutputFile="
                       sync0-pdfs-folder filename ".pdf " origfile)))
           ;; define list of conses whose first element is a biblatex category and
           ;; the second element is its value, as a string, when previously defined
           ;; by this fucntion
           (biblatex-definitions (list 
                                  title
                                  subtitle
                                  date
                                  origdate
                                  author
                                  journal
                                  booktitle
                                  booksubtitle
                                  crossref
                                  volume
                                  number
                                  publisher
                                  location
                                  pages
                                  addendum
                                  url
                                  urldate
                                  language
                                  langid
                                  file))
           (fields (mapcar* #'(lambda (x y) (list x y)) sync0-biblatex-fields biblatex-definitions))
           ;; define the biblatex entries
           (entries
            (let (x)
              (dolist (element fields x) 
                (unless (or (null (cadr element))
                            (equal (cadr element) ""))
                  (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
           ;; select target bibliography file (.bib)
           (bib-file (completing-read "Fichier BibLaTeX : "
                                      (f-files "~/Dropbox/bibliographies" (lambda (k) (string-match-p ".bib" k)))))
           ;; create string of new biblatex entry
           (biblatex-entry (concat "\n@" type "{" filename "," "\n" entries "\n}\n")))
      ;; add biblatex entry to target bibliography file
      (append-to-file biblatex-entry nil bib-file)
      ;; define certain varibles to construct the path with another function
      (setq  sync0-zettel-filename filename)
      (setq sync0-zettel-path (concat sync0-zettelkasten-directory "reference"))
      ;; in case of derivation, perform extraction
      (unless (null command)
        (shell-command command))
      ;; define the body of the reference zettel
      (concat
       ":PROPERTIES:\n"
       ":ID:      " id "\n"
       ":ROAM_REFS: cite:" filename "\n"
       (unless (or (null crossref)
                  (equal crossref ""))
       (concat ":CROSSREF: cite:" crossref "\n"))
       ":BIBLATEX_TYPE: " type "\n"
       (when (equal type "article") (concat ":JOURNAL_TITLE: \"" journal "\"\n"))
       (when (equal type "online") (concat ":WEBSITE: " url "\n"))
       ":AUTHOR: \"" author-fixed "\"\n"
       ":CREATED: " creation "\n"
       ":LAST_MODIFIED: " creation "\n"
       ":ZETTEL_TYPE: reference\n"
       ":LANGUAGE: " language "\n"
       ":DATE: \"" date "\"\n" 
       (unless (equal origdate "") (concat ":ORIG_DATE: \"" origdate "\"\n"))
       ":END:\n"
       "#+TITLE: " title "\n"
       (unless (equal subtitle "") (concat "#+SUBTITLE: " subtitle "\n"))
       "#+AUTHOR: " author-fixed "\n"
       "#+FILETAGS: :" filename ":" type ":" date ":\n"
       "#+INTERLEAVE_PDF: " file "\n\n" 
       "Origin: [[id:"
       (sync0-org-get-id buffer)
       "]["
       (sync0-org-get-file-title-keyword buffer)
       "]]\n\n%?")))
