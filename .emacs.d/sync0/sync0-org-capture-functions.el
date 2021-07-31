(defvar sync0-zettel-filename
  "Dummy variable for Zettel file names")

(defvar sync0-zettel-path
  "Dummy variable for Zettel paths")

;; The following two functions are necessary to replicate the functionality of org-roam into org-capture.
;; https://emacs.stackexchange.com/questions/27620/orgmode-capturing-original-document-title
(defun sync0-org-get-file-title-keyword (file)
  (let (title)
    (when file
      (with-current-buffer
          (get-file-buffer file)
        (pcase (org-collect-keywords '("TITLE"))
          (`(("TITLE" . ,val))
           (setq title (car val)))))
      title)))

;; Adapted from: 
;; https://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun sync0-org-get-keyword (KEYWORD)
  "get the value from a line like
                                                    this #+KEYWORD: value in a file."
  (let ((case-fold-search t)
        (re (format "^#\\+%s:[ \t]+\\([^\t\n]+\\)" KEYWORD)))
    (when  (save-excursion
             (or (re-search-forward re nil t)
                 (re-search-backward re nil t)))
      (match-string-no-properties 1))))

(defun sync0-org-get-previous-heading-or-title (file)
  (let (title)
    (when file
      (with-current-buffer
          (get-file-buffer file)
        (if (re-search-backward "^\\*+[ \t]+" nil t)
            (setq title (nth 4 (org-heading-components)))
          (pcase (org-collect-keywords '("TITLE"))
            (`(("TITLE" . ,val))
             (setq title (car val))))))
      title)))

(defun sync0-org-get-author-keyword (file)
  (let (author)
    (when file
      (with-current-buffer
          (get-file-buffer file)
        (pcase (org-collect-keywords '("AUTHOR"))
          (`(("AUTHOR" . ,val))
           (setq author (car val)))))
      author)))

(defun sync0-org-get-id (file)
  (interactive)
  (when file
    (with-current-buffer
        (get-file-buffer file)
      (org-entry-get 1 "ID"))))

(defun sync0-org-capture-get-language (file)
  (interactive)
  (when file
    (with-current-buffer
        (get-file-buffer file)
      (org-entry-get 1 "LANGUAGE"))))

(defun sync0-org-capture-zettel-path ()
  "Output the path where the new zettel will be created"
  (let* ((key (org-capture-get :key)))
    ;; Add this zettel to the productivy chart
    (with-current-buffer
        (find-file-noselect
         (concat sync0-zettelkasten-directory 
                 (format-time-string "chart/%Y%m.org")))
      (goto-char (point-min))
      (let* ((date (format-time-string "%Y/%m/%d"))
             (entry (concat "\n| " date " | 1 | 0 |"))
             (second-exist
              (concat "^| "
                      date                    
                      " |\\([[:blank:]]+\\)|[[:blank:]]+[[:digit:]]+ |$"))
             (previous-value
              (concat "^| " date " |[[:blank:]]+\\([[:digit:]]+\\) |[[:blank:]]+[[:digit:]]+ |$")))
        (cond ((re-search-forward previous-value nil t 1)
               (let* ((old-value (string-to-number
                                  (match-string-no-properties 1)))
                      (new-value (number-to-string
                                  (1+ old-value))))
                 (replace-match new-value nil nil nil 1)))
              ((re-search-forward second-exist nil t 1)
               (replace-match " 1 " nil nil nil 1))
              (t (progn
                   (goto-char (point-max))
                   (insert entry)))))
      ;; second part
      (goto-char (point-min))
      (let* ((word
              (cond ((or (equal key "r") 
                         (equal key "w"))
                     "Références")
                    ((or (equal key "p") 
                         (equal key "t"))
                     "Projets")
                    ((equal key "f") 
                     "Fiches")
                    ((equal key "a") 
                     "Annotations")
                    (t 
                     "Zettel")))
             (regex (concat "^| " word " [[:blank:]]+|[[:blank:]]+\\([[:digit:]]+\\) |")))
        (if (re-search-forward regex nil t 1)
            (let* ((old-value (string-to-number
                               (match-string-no-properties 1)))
                   (new-value (number-to-string
                               (1+ old-value))))
              (replace-match new-value nil nil nil 1))
          (goto-char (point-max))
          (insert (concat "| " word "   |        1 |       0 |")))))
    ;; output the file name and path
    (concat sync0-zettel-path "/" sync0-zettel-filename ".org")))

(defun sync0-org-capture-permanent-body ()
  ;; Determine the filter for title completion candidates
  ;; i.e., do not complete with all files
  (let* ((orig-buffer
          (buffer-name (org-capture-get :original-buffer)))
         (id (org-id-new))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (type "permanent")
         (path (concat sync0-zettelkasten-directory type))
         (title (completing-read "Titre du Zettel : "
                                 (org-roam--get-titles)
                                 nil nil nil nil nil t))
         (buffer (buffer-file-name))     
         (creation  (format-time-string "%Y-%m-%d")))
    ;; filename
    (setq  sync0-zettel-filename filename)
    (setq  sync0-zettel-path path)
    ;; define string of zettel
    (concat
     ":PROPERTIES:\n"
     ":ID:      " id "\n"
     ":CREATED: " creation "\n"
     ":LAST_MODIFIED: " creation "\n"
     ":ZETTEL_TYPE: "  type "\n"
     ":END:\n"
     "#+TITLE: " title "\n"
     ;; add roam tags according to zettel type
     "#+FILETAGS:\n\n"
     "Origin: [[id:" (sync0-org-get-id buffer)
     "]["
     (sync0-org-get-file-title-keyword buffer)
     "]]\n\n%?")))

(defun sync0-org-capture-zettel-body ()
  ;; Determine the filter for title completion candidates
  ;; i.e., do not complete with all files
  (let* ((orig-buffer
          (buffer-name (org-capture-get :original-buffer)))
         (id (org-id-new))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (type 
          (completing-read "Type de Zettel : "
          sync0-zettelkasten-zettel-types))
         (path (concat sync0-zettelkasten-directory 
                       (cond ((equal type "project")  "project")
                             ((equal type "todo") "todo")
                             ((equal type "inbox") "inbox")
                             (t "permanent"))))
         (title (completing-read "Titre du Zettel : "
                                 (org-roam--get-titles)
                                 nil nil nil nil nil t))
         (alias (let ((x
                       (read-string "Alias (comma separated): "
                                    nil nil nil t)))
                  (if (string-match-p "," x)

                      (string-trim
                       (prin1-to-string
                        (split-string-and-unquote x ","))
                       "(" ")")
                    (concat "\"" x "\""))))
         (subtitle (unless (equal type "fiche")
                     (read-string "Sous-titre : " nil nil nil t)))
         (buffer (buffer-file-name))     
         (project (unless (or 
                           (equal type "annotation")
                           (equal type "fiche"))
                    (string-trim
                     (prin1-to-string
                      (completing-read-multiple "Quel projet ?"
                                                sync0-zettelkasten-projects)) "(\"" "\")")))
         (fiche-type (when (equal type "fiche")
                       (completing-read "Quel type de fiche ?"
                                        sync0-zettelkasten-fiche-types)))
         (func (unless (or (equal type "fiche")
                           (equal type "annotation"))
                 (string-trim
                  (prin1-to-string
                   (completing-read-multiple "Quel fonction ?"
                                             sync0-zettelkasten-zettel-functions)) "(" ")")))
         (annotation-author
          (when (equal type "annotation")
            (with-current-buffer orig-buffer
                 (string-trim
                  (org-entry-get 1 "AUTHOR")
                  "\"" "\""))))
         (annotation-key
          (when (equal type "annotation")
            (with-current-buffer orig-buffer
              (substring (org-entry-get 1 "ROAM_REFS") 5 nil))))
         (annotation-language
          (when (equal type "annotation")
            (with-current-buffer orig-buffer
              (org-entry-get 1 "LANGUAGE"))))
         (annotation-intro
          (when (equal type "annotation")
            (cond ((equal annotation-language "french")
                    (format "Dans la page X de "))
                   ((equal annotation-language "spanish")
                    (format "En la página X de "))
                   ((equal annotation-language "portuguese")
                    (format "Na  página X de "))
                   (t (format "On page X of ")))))
         (creation  (format-time-string "%Y-%m-%d")))
    ;; filename
    (setq  sync0-zettel-filename filename)
    (setq  sync0-zettel-path path)
;; to loop over these, it is necessary to learn to use macros
    (unless (member project sync0-zettelkasten-projects)
      (add-to-list 'sync0-zettelkasten-projects project)
      (with-temp-file "~/.emacs.d/sync0-vars/projects.txt"
        (sync0-insert-elements-of-list sync0-zettelkasten-projects)))
    (unless (member func sync0-zettelkasten-zettel-functions)
      (add-to-list 'sync0-zettelkasten-zettel-functions func)
      (with-temp-file "~/.emacs.d/sync0-vars/zettel-functions.txt"
        (sync0-insert-elements-of-list sync0-zettelkasten-zettel-functions)))
    (unless (member fiche-type sync0-zettelkasten-fiche-types)
      (add-to-list 'sync0-zettelkasten-fiche-types fiche-type)
      (with-temp-file "~/.emacs.d/sync0-vars/fiche-types.txt"
        (sync0-insert-elements-of-list sync0-zettelkasten-fiche-types)))
    ;; define string of zettel
    (concat
     ":PROPERTIES:\n"
     ":ID:      " id "\n"
     (unless (or (null alias)
                 (equal alias "\"\""))
       (concat ":ROAM_ALIASES: "  alias "\n"))
     ":CREATED: " creation "\n"
     ":LAST_MODIFIED: " creation "\n"
     (if (equal type "project")
         ":ZETTEL_TYPE: \"project\"\n"
       (concat ":ZETTEL_TYPE: "  type "\n"))
     (when (equal type "annotation")
       (concat ":ANNOTATION_REFS: cite:" annotation-key "\n"))
     (unless (or (null func)
                 (equal func "nil")
                 (equal func ""))
       (concat ":ZETTEL_FUNCTION: " func "\n"))
     (when (equal type "fiche")
       (concat ":FICHE_TYPE: " fiche-type "\n"))
     (unless (or (null project)
                 (equal project "nil"))
       (concat ":PROJECT_TITLE: \"" project "\"\n"))
     ":END:\n"
     "#+TITLE: " title "\n"
     (unless (or (null subtitle)
                 (equal subtitle ""))
       (concat "#+SUBTITLE: " subtitle "\n"))
     (when (equal type "todo")
       (concat "#+CATEGORY: " (upcase project) "\n"))
     ;; add roam tags according to zettel type
     "#+FILETAGS: :" type 
     (cond ((equal type "annotation")
            (concat ":" annotation-key ":" sync0-current-month-downcase
                    (format-time-string ":%Y:\n")))
           ((equal type "project")
            (concat
             (when (equal type "todo") ":todo")
             ":" project ":" sync0-current-month-downcase ":"
             (format-time-string ":%Y:\n")))
           (t ":\n"))
     "\n"
     "Origin: [[id:" (sync0-org-get-id buffer)
     "]["
     (sync0-org-get-file-title-keyword buffer)
     "]]\n\n"
     (if (equal type "annotation")
         (concat
          annotation-intro
          "[[id:"
          (sync0-org-get-id buffer)
          "]["
          (sync0-org-get-previous-heading-or-title buffer)
          "]], "
          annotation-author
          " %?")
       "%?"))))

(defun sync0-org-capture-quick-reference ()
  (let* ((type (completing-read "Choose BibLaTex entry type: " sync0-biblatex-entry-types))
         (id (org-id-new))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (title (completing-read "Titre du texte : "
                                 (org-roam--get-titles)
                                 nil nil nil nil nil t))
         (subtitle (read-string "Sous-titre du texte : " nil nil nil t))
         (date (read-string "Date (ex. 1890-18-12) : "))
         (author (completing-read "Auteur : " sync0-bibtex-authors
                                    nil nil nil))
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
         (lastname (cond ((string-match " and " author)
                          ;; create a list with parts 
                          (let* ((author-list  (split-string author " and "))
                                 (last-names (let (x)
                                               (dolist  (element author-list x)
                                                 (setq x (concat x
                                                                 (progn
                                                                   (string-match "\\([[:graph:]]+\\),"   element)
                                                                  (downcase (match-string 1 element)))
                                                                 ":"))))))
                            (substring last-names 0 -2)))
                         ((string-match "^{" author)
                          (string-match "{\\([[:print:]]+\\)}" author)
                          (downcase (match-string 1 author)))
                         (t (downcase (nth 0 (split-string author ", "))))))
         (language (completing-read "Choose language : "
                                      sync0-bibtex-languages nil nil nil))
         (langid language) 
         (addendum (read-string "Addendum (ex. Box, Folder, etc.) : "))
         (url (read-string "Url : " nil nil nil t))
         (urldate (unless (equal url "") creation))
         (file (concat "/home/sync0/Documents/pdfs/" filename ".pdf"))
         (buffer (buffer-file-name))     
         (biblatex-definitions (list 
                                title
                                subtitle
                                date
                                author
                                addendum
                                url
                                urldate
                                language
                                langid
                                file))
         (biblatex-fields (if (equal type "collection")
                              (cl-substitute "editor" "author" sync0-biblatex-quick-fields)
                            sync0-biblatex-quick-fields))
         (fields (mapcar* #'(lambda (x y) (list x y)) biblatex-fields biblatex-definitions))
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
    ;; add biblatex entry to target bibliography file.
    ;; The entry has to be added this way to prevent a bug
    ;; that happens with bibtex-completion: unless the entry
    ;; is added in a different buffer, the template in org-capture
    ;; won't expand because the last output is not the template but
    ;; a bibtex-completion message abour bibliography reloading
    (with-current-buffer
        (find-file-noselect bib-file)
      (goto-char (point-max))
      (insert biblatex-entry))
    ;; (append-to-file biblatex-entry nil bib-file)
    ;; define certain varibles to construct the path with another function
    (setq sync0-zettel-filename filename)
    (setq sync0-zettel-path (concat sync0-zettelkasten-directory "permanent"))
(unless (member author sync0-bibtex-authors)
    (add-to-list 'sync0-bibtex-authors author)
    (with-temp-file "~/.emacs.d/sync0-vars/bibtex-authors.txt"
      (sync0-insert-elements-of-list sync0-bibtex-authors)))
    ;; define the body of the reference zettel
    (concat
     ":PROPERTIES:\n"
     ":ID:      " id "\n"
     ":ROAM_REFS: cite:" filename "\n"
     ":BIBLATEX_TYPE: " type "\n"
     (when (equal type "online") (concat ":WEBSITE: " url "\n"))
     ":AUTHOR: \"" author-fixed "\"\n"
     ":CREATED: " creation "\n"
     ":LAST_MODIFIED: " creation "\n"
     ":ZETTEL_TYPE: reference\n"
     ":LANGUAGE: " language "\n"
     ":DATE: \"" date "\"\n" 
     ":END:\n"
     "#+TITLE: " title "\n"
     (unless (equal subtitle "") (concat "#+SUBTITLE: " subtitle "\n"))
     "#+AUTHOR: " author-fixed "\n"
     "#+FILETAGS: :" filename ":" date ":" lastname ":\n"
     "#+INTERLEAVE_PDF: " file "\n\n" 
     "Origin: [[id:"
     (sync0-org-get-id buffer)
     "]["
     (sync0-org-get-file-title-keyword buffer)
     "]]\n\n%?")))

(defun sync0-org-capture-reference ()
  (let* ((type (if (equal (org-capture-get :key) "w")
                   "online"
                 (completing-read "Choose BibLaTex entry type: " sync0-biblatex-entry-types)))
         (id (org-id-new))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (title (completing-read "Titre du texte : "
                                 (org-roam--get-titles)
                                 nil nil nil nil nil t))
         (subtitle (read-string "Sous-titre du texte : " nil nil nil t))
         (derivation (yes-or-no-p "Derive entry?"))
         (crossref (when derivation
                     (let* ((candidates (bibtex-completion-candidates))
                            (selection (ivy-read "Crossref : "
                                                 candidates
                                                 :caller 'ivy-bibtex
                                                 :history 'ivy-bibtex-history)))
                       (cdr (assoc "=key=" (cdr (assoc selection candidates)))))))
         (initial-date (unless (null derivation)
                         (sync0-org-ref-get-citation-date crossref)))
         (date (read-string "Date (ex. 1890-18-12) : " initial-date))
         (initial-author (unless (null derivation)
                           (sync0-org-ref-get-citation-author crossref)))
         (origdate (read-string "Origdate (ex. 1890-18-12) : "))
         (author (completing-read "Auteur : " sync0-bibtex-authors
                                    nil nil initial-author))
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
         (lastname (cond ((string-match " and " author)
                          ;; create a list with parts 
                          (let* ((author-list  (split-string author " and "))
                                 (last-names (let (x)
                                               (dolist  (element author-list x)
                                                 (setq x (concat x
                                                                 (progn
                                                                   (string-match "\\([[:graph:]]+\\),"   element)
                                                                  (downcase (match-string 1 element)))
                                                                 ":"))))))
                            (substring last-names 0 -2)))
                         ((string-match "^{" author)
                          (string-match "{\\([[:print:]]+\\)}" author)
                          (downcase (match-string 1 author)))
                         (t (downcase (nth 0 (split-string author ", "))))))
         (initial-language (unless (null derivation)
                             (sync0-org-ref-get-citation-language crossref)))
         (language (completing-read "Choose language : "
                                      sync0-bibtex-languages nil nil initial-language))
         (langid language) 
         (journal (when (or (equal type "article")
                        (equal type "incollection"))
                    (completing-read "Titre du journal : " sync0-bibtex-journals)))
         (volume
            (when (or (equal type "article")
                      (equal type "book")
                      (equal type "inbook")
                      (equal type "collection")
                      (equal type "incollection"))
              (read-string "Tome : ")))
         (number (when (equal type "article")
                   (read-string "Numero : ")))
         (publisher (unless (or (equal type "unpublished")
                                 (equal type "article"))
                      (completing-read "Maison d'edition : " sync0-bibtex-publishers)))
         (location (when (or (equal type "book")
                             (equal type "collection"))
                     (completing-read "Location : " sync0-bibtex-locations)))
         (pages (when (or (equal type "article")
                          (equal type "incollection")
                          (equal type "inbook"))
                  (read-string "Pages (ex. : 90-180) : ")))
         (booktitle (when (and (or (equal type "inbook")
                                   (equal type "incollection"))
                               (null derivation))
                      (completing-read "Book title: " sync0-bibtex-booktitles)))
         (booksubtitle (when (and (or (equal type "inbook")
                                      (equal type "incollection"))
                                  (null derivation))
                         (read-string "Book subtitle: " nil nil nil t)))
         (parent
          (cond ((and (null derivation)
                      (equal type "article"))
                 journal)
                ((not (null booktitle))
                 booktitle)
                ((not (null derivation))
                 (sync0-org-ref-get-citation-title crossref))
                (t nil)))
         (addendum (when (equal type "unpublished")
                     (read-string "Addendum (ex. Box, Folder, etc.) : ")))
         (url (read-string "Url : " nil nil nil t))
         (urldate (unless (equal url "") creation))
         (file (concat "/home/sync0/Documents/pdfs/" filename ".pdf"))
         (buffer (buffer-file-name))     
         ;; in case of extraction define these
         (extraction (unless (null derivation)
                       (yes-or-no-p "Extract  PDF from entry?")))
         (beg (unless (and (null derivation)
                           (null extraction))
                (read-string "First page for extraction: ")))
         (end (unless (and (null derivation)
                           (null extraction))
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
         (biblatex-fields (if (equal type "collection")
                              (cl-substitute "editor" "author" sync0-biblatex-fields)
                            sync0-biblatex-fields))
         (fields (mapcar* #'(lambda (x y) (list x y)) biblatex-fields biblatex-definitions))
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
    ;; add biblatex entry to target bibliography file.
    ;; The entry has to be added this way to prevent a bug
    ;; that happens with bibtex-completion: unless the entry
    ;; is added in a different buffer, the template in org-capture
    ;; won't expand because the last output is not the template but
    ;; a bibtex-completion message abour bibliography reloading
    (with-current-buffer
        (find-file-noselect bib-file)
      (goto-char (point-max))
      (insert biblatex-entry))
    ;; (append-to-file biblatex-entry nil bib-file)
    ;; define certain varibles to construct the path with another function
    (setq sync0-zettel-filename filename)
    (setq sync0-zettel-path (concat sync0-zettelkasten-directory "permanent"))
(unless (member author sync0-bibtex-authors)
    (add-to-list 'sync0-bibtex-authors author)
    (with-temp-file "~/.emacs.d/sync0-vars/bibtex-authors.txt"
      (sync0-insert-elements-of-list sync0-bibtex-authors)))
(unless (member booktitle sync0-bibtex-booktitles)
    (add-to-list 'sync0-bibtex-booktitles booktitle)
    (with-temp-file "~/.emacs.d/sync0-vars/bibtex-booktitles.txt"
      (sync0-insert-elements-of-list sync0-bibtex-booktitles)))
(unless (member location sync0-bibtex-locations)
    (add-to-list 'sync0-bibtex-locations location)
    (with-temp-file "~/.emacs.d/sync0-vars/bibtex-locations.txt"
      (sync0-insert-elements-of-list sync0-bibtex-locations)))
(unless (member publisher sync0-bibtex-publishers)
    (add-to-list 'sync0-bibtex-publishers location)
    (with-temp-file "~/.emacs.d/sync0-vars/bibtex-publishers.txt"
      (sync0-insert-elements-of-list sync0-bibtex-publishers)))
(unless (member journal sync0-bibtex-journals)
    (add-to-list 'sync0-bibtex-journals journal)
    (with-temp-file "~/.emacs.d/sync0-vars/bibtex-journals.txt"
      (sync0-insert-elements-of-list sync0-bibtex-journals)))
    ;; (sync0-update-list author sync0-bibtex-authors "bibtex-authors")
    ;; (sync0-update-list booktitle sync0-bibtex-booktitles "bibtex-booktitles")
    ;; (sync0-update-list location sync0-bibtex-locations "bibtex-locations")
    ;; (sync0-update-list publisher sync0-bibtex-publishers "bibtex-publishers")
    ;; (sync0-update-list journal sync0-bibtex-journals "bibtex-journals")
    ;; in case of derivation, perform extraction
    (unless  (null command)
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
     (unless (or (null parent)
                 (equal parent ""))
       (concat ":PARENT: \"" parent "\"\n"))
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
     "#+FILETAGS: :" filename ":" date ":" lastname ":\n"
     "#+INTERLEAVE_PDF: " file "\n\n" 
     "Origin: [[id:"
     (sync0-org-get-id buffer)
     "]["
     (sync0-org-get-file-title-keyword buffer)
     "]]\n\n%?")))

;; (when (equal type "online") "%:initial%?")

(defun sync0-org-references-fetch-title-and-subtitle ()
  (if (equal sync0-reference-subtitle "")
      (format "%s" sync0-reference-title) 
    (format "%s_%s" sync0-reference-title sync0-reference-subtitle))) 

;; Taken from https://github.com/abo-abo/hydra/wiki/mu4e
(defun sync0-org-capture-mu4e ()
  (interactive)
  "Capture a TODO item via email."
  (org-capture nil "o"))

(provide 'sync0-org-capture-functions)
