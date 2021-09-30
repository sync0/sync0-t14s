(defun sync0-bibtex-capture-reference ()
  (interactive)
  (let* ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
         (type-downcase (downcase type))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (title (let* ((candidates (bibtex-completion-candidates))
                       (selection (ivy-read "Choose BibTeX key to extract from : "
                                            candidates
                                            :caller 'ivy-bibtex
                                            :require-match nil
                                            :history 'ivy-bibtex-history)))
                  (if (string-match-p "[0-9]\\{14\\}$" selection)
                      (cdr (assoc "title" (cdr (assoc selection candidates))))
                    selection)))
         (subtitle (read-string "Sous-titre du texte : " nil nil nil t))
         (title-fixed (if (equal subtitle "")
                          title
                        (concat title " : " subtitle)))
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
         (date-fixed (if (or (equal origdate "")
                             (equal date origdate))
                         (concat "(" date ")")
                       (concat "(" origdate ") (" date ")")))
         (author (completing-read "Auteur : " sync0-bibtex-authors
                                  nil nil initial-author))
     (author-fixed (cond ((string-match " and " author)
                          ;; create a list with parts 
                          (let* ((author-list  (split-string author " and "))
                                 (names (let (x)
                                          (dolist  (element author-list x)
                                            (setq x (concat x element "\",\""))))))
                            (concat "\"" (substring names 0 -2))))
                         ;; check when author is an organization
                         ((string-match "^{" author)
                          (concat "\"" (substring author 1 -1) "\""))
                         ;; other cases
                         (t (concat "\"" author "\""))))
         (lastname (cond ((string-match " and " author)
                          ;; create a list with parts 
                          (let* ((author-list  (split-string author " and "))
                                 (last-names (let (x)
                                               (dolist  (element author-list x)
                                                 (setq x (concat x
                                                                 (progn
                                                                   (string-match "\\([[:graph:]]+\\),"   element)
                                                                   (match-string 1 element))
                                                                 ":"))))))
                            (substring last-names 0 -2)))
                         ((string-match "^{" author)
                          (string-match "{\\([[:print:]]+\\)}" author)
                          (match-string 1 author))
                         (t (nth 0 (split-string author ", ")))))
         (lastname-downcase
          (replace-regexp-in-string "[[:space:]]+" "_" (downcase lastname)))
         (initial-language (unless (null derivation)
                             (sync0-org-ref-get-citation-language crossref)))
         (language (completing-read "Choose language : "
                                    sync0-bibtex-languages nil nil initial-language))
         (langid language) 
         (journal (when (or (equal type "Article")
                            (equal type "InCollection"))
                    (completing-read "Titre du journal : " sync0-bibtex-journals)))
         (volume
          (when (or (equal type "Article")
                    (equal type "Book")
                    (equal type "InBook")
                    (equal type "Collection")
                    (equal type "InCollection"))
            (read-string "Tome : ")))
         (number (when (equal type "Article")
                   (read-string "Numero : ")))
         (publisher (unless (or (equal type "Unpublished")
                                (equal type "Article"))
                      (completing-read "Maison d'edition : " sync0-bibtex-publishers)))
         (location (when (or (equal type "Book")
                             (equal type "Collection"))
                     (completing-read "Location : " sync0-bibtex-locations)))
         (pages (when (or (equal type "Article")
                          (equal type "InCollection")
                          (equal type "InBook"))
                  (read-string "Pages (ex. : 90-180) : ")))
         (booktitle (when (and (or (equal type "InBook")
                                   (equal type "InCollection"))
                               (null derivation))
                      (completing-read "Book title: " sync0-bibtex-booktitles)))
         (booksubtitle (when (and (or (equal type "InBook")
                                      (equal type "InCollection"))
                                  (null derivation))
                         (read-string "Book subtitle: " nil nil nil t)))
         (parent
          (cond ((and (null derivation)
                      (equal type "Article"))
                 journal)
                ((not (null booktitle))
                 booktitle)
                ((not (null derivation))
                 (sync0-org-ref-get-citation-title crossref))
                (t nil)))
         (medium (string-trim
                  (prin1-to-string
                   (completing-read-multiple "Quel support ?"
                                             sync0-bibtex-media)) "(\"" "\")"))
         (library (completing-read "Choose location to trace back: "
                                   sync0-bibtex-traces nil nil nil))
         (addendum (when (equal type "Unpublished")
                     (read-string "Addendum (ex. Box, Folder, etc.) : ")))
         (url (read-string "Url : " nil nil nil t))
         (urldate (unless (equal url "") creation))
         (file-pdf (unless (equal type "Online")
                     (concat "/home/sync0/Documents/pdfs/" filename ".pdf")))
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
         ;; define list of conses whose first element is a bibtex category and
         ;; the second element is its value, as a string, when previously defined
         ;; by this fucntion
         (bibtex-definitions (list 
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
                              medium
                              library
                              file-pdf))
         (bibtex-fields (if (equal type "Collection")
                            (cl-substitute "editor" "author" sync0-bibtex-fields)
                          sync0-bibtex-fields))
         (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields bibtex-definitions))
         ;; define the bibtex entries
         (entries
          (let (x)
            (dolist (element fields x) 
              (unless (or (null (cadr element))
                          (equal (cadr element) ""))
                (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
         ;; select target bibliography file (.bib)
         (obsidian-file (concat sync0-obsidian-directory filename ".md")) 
         (obsidian-entry (concat "---\n"
                                 "zettel_type: reference\n"
                                 "citekey: " filename "\n"
                                 "created: " (format-time-string "%Y-%m-%d") "\n"
                                 "biblatex_type: " type-downcase "\n"
                                 "title: \"" title "\"\n"
                                 (unless (equal subtitle "")
                                   (concat "subtitle: \"" subtitle "\"\n"))
                                 "author: [" author-fixed "]\n"
                                 (unless (or (null crossref)
                                          (equal crossref ""))
                                   (concat "crossref: " crossref "\n"))
                                 (unless (or (null parent)
                                          (equal parent ""))
                                   (concat "parent: \"" parent "\"\n"))
                                 "aliases: [\"" lastname " " date-fixed  " " title-fixed "\"]\n"
                                 (unless (or (null url)
                                          (equal url ""))
                                   (concat "url: \"" url "\"\n"))
                                 (unless (or (null origdate)
                                          (equal origdate ""))
                                   (concat "origdate: " origdate "\n"))
                                 "date: " date "\n"
                                 (unless (or (null medium)
                                          (equal medium ""))
                                   (concat "medium: [\"" medium "\"]\n"))
                                 "library: \"" library "\"\n"
                                 "tags: [reference/" type-downcase ",bibkey/" filename ",date/" date "," lastname-downcase "]\n"
                                 "---\n" 
                                 "# " lastname " " date-fixed " " title-fixed "\n\n" 
                                 "## Description\n\n" 
                                 "## Progrès de la lecture\n\n" 
                                 "## Annotations\n\n"))
         (bib-file sync0-default-bibliography)
         ;; (bib-file (completing-read "Fichier BibLaTeX : "
         ;;                            (f-files "~/Dropbox/bibliographies" (lambda (k) (string-match-p ".bib" k)))))
         ;; create string of new biblatex entry
         (bibtex-entry (concat "\n@" type "{" filename "," "\n" entries "\n}\n")))
    ;; add biblatex entry to target bibliography file.
    ;; The entry has to be added this way to prevent a bug
    ;; that happens with biblatex-completion: unless the entry
    ;; is added in a different buffer, the template in org-capture
    ;; won't expand because the last output is not the template but
    ;; a biblatex-completion message abour bibliography reloading
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
    (unless (member library sync0-bibtex-traces)
      (add-to-list 'sync0-bibtex-traces library)
      (with-temp-file "~/.emacs.d/sync0-vars/bibtex-traces.txt"
        (sync0-insert-elements-of-list sync0-bibtex-traces)))
    (unless (member medium sync0-bibtex-media)
      (add-to-list 'sync0-bibtex-media medium)
      (with-temp-file "~/.emacs.d/sync0-vars/bibtex-media.txt"
        (sync0-insert-elements-of-list sync0-bibtex-media)))
    (unless (member publisher sync0-bibtex-publishers)
      (add-to-list 'sync0-bibtex-publishers location)
      (with-temp-file "~/.emacs.d/sync0-vars/bibtex-publishers.txt"
        (sync0-insert-elements-of-list sync0-bibtex-publishers)))
    (unless (member journal sync0-bibtex-journals)
      (add-to-list 'sync0-bibtex-journals journal)
      (with-temp-file "~/.emacs.d/sync0-vars/bibtex-journals.txt"
        (sync0-insert-elements-of-list sync0-bibtex-journals)))
    ;; in case of derivation, perform extraction
    (unless  (null command)
      (shell-command command))
    ;; define the body of the reference zettel
    (with-temp-buffer 
      (insert obsidian-entry)
      (write-file obsidian-file))
    (goto-char (point-max))
    (insert bibtex-entry)))

(defun sync0-bibtex-capture-quick-reference ()
  (interactive)
  (let* ((type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
         (type-downcase (downcase type))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (creation (format-time-string "%Y-%m-%d")) 
         (title (let* ((candidates (bibtex-completion-candidates))
                       (selection (ivy-read "Choose BibTeX key to extract from : "
                                            candidates
                                            :caller 'ivy-bibtex
                                            :require-match nil
                                            :history 'ivy-bibtex-history)))
                  (if (string-match-p "[0-9]\\{14\\}$" selection)
                      (cdr (assoc "title" (cdr (assoc selection candidates))))
                    selection)))
         (subtitle (read-string "Sous-titre du texte : " nil nil nil t))
         (title-fixed (if (equal subtitle "")
                          title
                        (concat title " : " subtitle)))
         (date (read-string "Date (ex. 1890-18-12) : "))
         (author (completing-read "Auteur : " sync0-bibtex-authors
                                  nil nil nil))
     (author-fixed (cond ((string-match " and " author)
                          ;; create a list with parts 
                          (let* ((author-list  (split-string author " and "))
                                 (names (let (x)
                                          (dolist  (element author-list x)
                                            (setq x (concat x element "\",\""))))))
                            (concat "\"" (substring names 0 -2))))
                         ;; check when author is an organization
                         ((string-match "^{" author)
                          (concat "\"" (substring author 1 -1) "\""))
                         ;; other cases
                         (t (concat "\"" author "\""))))
         (lastname (cond ((string-match " and " author)
                          ;; create a list with parts 
                          (let* ((author-list  (split-string author " and "))
                                 (last-names (let (x)
                                               (dolist  (element author-list x)
                                                 (setq x (concat x
                                                                 (progn
                                                                   (string-match "\\([[:graph:]]+\\),"   element)
                                                                   (match-string 1 element))
                                                                 ":"))))))
                            (substring last-names 0 -2)))
                         ((string-match "^{" author)
                          (string-match "{\\([[:print:]]+\\)}" author)
                          (match-string 1 author))
                         (t (nth 0 (split-string author ", ")))))
         (lastname-downcase
          (replace-regexp-in-string "[[:space:]]+" "_" (downcase lastname)))
         (language (completing-read "Choose language : "
                                    sync0-bibtex-languages nil nil nil))
         (langid language) 
         ;; (medium (string-trim
         ;;          (prin1-to-string
         ;;           (completing-read-multiple "Quel support ?"
         ;;                                     sync0-bibtex-media)) "(" ")"))
         (library (completing-read "Choose location to trace back: "
                                      sync0-bibtex-traces nil nil nil))
         (addendum (read-string "Addendum (ex. Box, Folder, etc.) : "))
         (url (read-string "Url : " nil nil nil t))
         (urldate (unless (equal url "") creation))
         (file-pdf (unless (equal type "Online")
                     (concat "/home/sync0/Documents/pdfs/" filename ".pdf")))
         (bibtex-definitions (list 
                              title
                              subtitle
                              date
                              author
                              addendum
                              url
                              urldate
                              language
                              langid
                              library
                              file-pdf))
         (bibtex-fields (if (equal type "Collection")
                            (cl-substitute "editor" "author" sync0-bibtex-quick-fields)
                          sync0-bibtex-quick-fields))
         (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields bibtex-definitions))
         ;; define the bibtex entries
         (entries
          (let (x)
            (dolist (element fields x) 
              (unless (or (null (cadr element))
                          (equal (cadr element) ""))
                (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
         ;; select target bibliography file (.bib)
         (bib-file sync0-default-bibliography) 
         ;; (bib-file (completing-read "Fichier Bibtex : "
         ;;                            (f-files "~/Dropbox/bibliographies" (lambda (k) (string-match-p ".bib" k)))))
         ;; create string of new bibtex entry
         (obsidian-file (concat sync0-obsidian-directory filename ".md")) 
         (obsidian-entry (concat "---\n"
                                 "zettel_type: reference\n"
                                 "biblatex_type: " type-downcase "\n"
                                 "citekey: " filename "\n"
                                 "created: " (format-time-string "%Y-%m-%d") "\n"
                                 "title: \"" title "\"\n"
                                 (unless (equal subtitle "")
                                   (concat "subtitle: \"" subtitle "\"\n"))
                                 "author: [" author-fixed "]\n"
                                 "aliases: [\""  lastname " " date  " " title-fixed "\"]\n"
                                 (unless (or (null url)
                                          (equal url ""))
                                   (concat "url: \"" url "\"\n"))
                                 "date: (" date ")\n"
                                 "library: \"" library "\"\n"
                                 "tags: [reference/" type-downcase ",bibkey/" filename ",date/" date "," lastname-downcase "]\n"
                                 "---\n" 
                                 "# " lastname " (" date ") " title-fixed "\n\n" 
                                 "## Description\n\n" 
                                 "## Progrès de la lecture\n\n" 
                                 "## Annotations\n\n"))
         (bibtex-entry (concat "\n@" type "{" filename "," "\n" entries "\n}\n")))
    ;; add biblatex entry to target bibliography file.
    ;; The entry has to be added this way to prevent a bug
    ;; that happens with biblatex-completion: unless the entry
    ;; is added in a different buffer, the template in org-capture
    ;; won't expand because the last output is not the template but
    ;; a biblatex-completion message abour bibliography reloading
    ;; (append-to-file bibtex-entry nil bib-file)
    ;; define certain varibles to construct the path with another function
    (unless (member author sync0-bibtex-authors)
      (add-to-list 'sync0-bibtex-authors author)
      (with-temp-file "~/.emacs.d/sync0-vars/bibtex-authors.txt"
        (sync0-insert-elements-of-list sync0-bibtex-authors)))
    (with-temp-buffer 
      (insert obsidian-entry)
      (write-file obsidian-file))
    (goto-char (point-max))
    (insert bibtex-entry)))

(defun sync0-bibtex-pick-crossref ()
"Use bibtex-completion to pick a crossref bibtex key."
  (interactive)
  (let* ((candidates (bibtex-completion-candidates))
         (selection (ivy-read "Crossref : "
                              candidates
                              :caller 'ivy-bibtex
                              :history 'ivy-bibtex-history)))
    (cdr (assoc "=key=" (cdr (assoc selection candidates))))))

(defun sync0-bibtex-update-authors-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates
                          (append
                           (mapcar #'(lambda (x) (cdr (assoc "editor" x)))
                                   (bibtex-completion-candidates))
                           (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                   (bibtex-completion-candidates))))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-authors.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-authors.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-booktitles-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates 
                          (mapcar #'(lambda (x) (cdr (assoc "booktitle" x)))
                                  (bibtex-completion-candidates)))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-booktitles.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-booktitles.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-locations-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates 
                          (mapcar #'(lambda (x) (cdr (assoc "location" x)))
                                  (bibtex-completion-candidates)))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-locations.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-locations.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-traces-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates 
                          (mapcar #'(lambda (x) (cdr (assoc "library" x)))
                                  (bibtex-completion-candidates)))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-trace.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-trace.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-media-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates 
                          (mapcar #'(lambda (x) (cdr (assoc "medium" x)))
                                  (bibtex-completion-candidates)))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-media.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-media.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-publishers-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates 
                          (mapcar #'(lambda (x) (cdr (assoc "publisher" x)))
                                  (bibtex-completion-candidates)))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-publishers.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-publishers.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-journals-list ()
  "Update the contents of file bibtex-authors.txt from the date in
our main bibliography file"
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (delete-duplicates 
                          (mapcar #'(lambda (x) (cdr (assoc "journaltitle" x)))
                                  (bibtex-completion-candidates)))))
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-journals.txt")
      (goto-char (point-min))
      ;; (keep-lines "contexts" (point-min) (point-max)) 
      (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
        (push  (match-string 1) current-authors)))
    (with-temp-file
        "~/.emacs.d/sync0-vars/bibtex-journals.txt"
      (setq final-list (delete-dups (append current-authors bibtex-authors)))
      (sync0-insert-elements-of-list final-list))))

(defun sync0-bibtex-update-sync0-variables ()
  "Update the helper bibtex variables from the content in the
bibliography file."
  (interactive)
  (progn 
    (sync0-bibtex-update-authors-list)
    (sync0-bibtex-update-booktitles-list)
    (sync0-bibtex-update-locations-list)
    (sync0-bibtex-update-media-list)
    (sync0-bibtex-update-traces-list)
    (sync0-bibtex-update-publishers-list)
    (sync0-set-variable-from-files sync0-bibtex-variables-list)))

(defun sync0-bibtex-new-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new (yes-or-no-p "Use existing key for new entry? "))
         (new-key (if new
                    (read-string "Input existing key: ")
  (format-time-string "%Y%m%d%H%M%S"))))
    (setq sync0-bibtex-new-key new-key)
    (insert new-key)))

(defun sync0-bibtex-create-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S")))
    (insert new-key)))

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

(defun sync0-bibtex-update-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S"))
         (directory "/home/sync0/Documents/pdfs/")
         (new-path (concat directory new-key ".pdf"))
         (pdf-path
          (save-excursion
            (when  (re-search-forward "file = {\\(.+\\)}," nil t 1)
              (match-string-no-properties 1)))))
    (when-let ((type
                (when (re-search-forward "\\(^@[[:lower:]]+{\\)[[:digit:]]+," nil t 1)
                  (match-string-no-properties 1))))
      (kill-whole-line 1)
      (insert (concat type new-key ",\n")))
    (when (re-search-forward "file = {" nil t 1)
      (kill-whole-line 1)
      (insert (concat "file = {" new-path "},\n")))))

;; (when (file-exists-p pdf-path)
;;   (rename-file pdf-path new-path)))))

(defhydra sync0-hydra-bibtex-functions (:color amaranth :hint nil :exit t)
  "
     ^Refs^              ^PDFs^             ^Notes^              
     ^-----------------------------------------------------
     Key _u_pdate        Pdf _o_pen         Open _n_otes
     _E_ntry insert      Open in _z_athura
     Quick _e_ntry       Copy to _p_ath  
     ^----------------------------------------------------
     ^Bibliographies^ 
     ^---------------------------------------------------
     Bibfile _v_isit 
                                                                     
     _q_uit
          "
  ("E" sync0-bibtex-capture-reference)
  ("e" sync0-bibtex-capture-quick-reference)
  ("u" sync0-bibtex-update-key)
  ("p" sync0-org-ref-copy-pdf-to-path)
  ("n" sync0-org-ref-open-notes)
  ("v" sync0-visit-bibliography-in-buffer)
  ("o" sync0-org-ref-open-pdf-at-point)
  ("z" sync0-org-ref-open-pdf-at-point-zathura)
  ("q" nil :color blue))

;; (evil-leader/set-key
;;   "O" 'org-open-at-point
;;   "#" 'sync0-org-open-other-frame)
;; "O" 'sync0-overview-tree-window
;; "o" 'sync0-overview-jump-to-overview
;; "I" 'org-insert-link
;; "z" 'sync0-org-tree-to-indirect-buffer
;; "z" 'sync0-hydra-org-functions/body

(evil-leader/set-key-for-mode 'bibtex-mode "z" 'sync0-hydra-bibtex-functions/body)


(provide 'sync0-bibtex-functions)
