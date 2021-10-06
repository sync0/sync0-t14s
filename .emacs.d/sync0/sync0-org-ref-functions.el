
(defun sync0-org-ref-search-bibkey-in-buffer ()
  "Check the existence of a bibtex in current buffer."
  (interactive)
  (let ((regex-one "cite:\\([[:digit:]]+\\)")
        (regex-two "@book{\\([[:digit:]]+\\),"))
    (cond ((equal major-mode 'org-mode)
;;; problem with finding headlines
           ;; (outline-up-heading)
           ;; (org-element-property )
      ;; ((elem (org-entry-get (point) "EXPORT_TITLE")))
           (org-with-point-at 1 
             (re-search-forward regex-one nil t 1)
             (match-string-no-properties 1)))
          ((equal major-mode 'bibtex-mode)
           (re-search-forward regex-two nil t 1)
           (match-string-no-properties 1))
          (t (re-search-forward regex-one nil t 1)
             (match-string-no-properties 1)))))

(defun sync0-org-ref-pdf-exist-p ()
  "Check the existence of the pdf for bibtex key under point."
  (interactive)
  (let* ((key-at-point (org-ref-get-bibtex-key-under-cursor))
         (bibkey (if  (equal key-at-point "")
                     (let* ((candidates (bibtex-completion-candidates))
                            (selection (ivy-read "Choose BibTeX key to extract from : "
                                                 candidates
                                                 :caller 'ivy-bibtex
                                                 :history 'ivy-bibtex-history)))
                       (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
                   key-at-point))            
         (pdf-file (car (bibtex-completion-find-pdf bibkey))))
    (if (file-exists-p pdf-file)
      (message "PDF found in library for %s" bibkey)
      (message "No PDF found for %s" bibkey))))

(defun sync0-org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p file)
        (org-open-file file))
    (message "No PDF found for %s" key)))

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

(defun sync0-org-ref-get-citation-title (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "title" (bibtex-parse-entry t))))))

;; (defun sync0-org-ref-get-citation-author (key)
;;   "Get the year of an entry with KEY.  Return year as a string."
;;   (let* ((results (org-ref-get-bibtex-key-and-file key))
;;          (bibfile (cdr results)))
;;     (with-temp-buffer
;;       (insert-file-contents bibfile)
;;       (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
;;       (bibtex-search-entry key nil 0)
;;       (prog1 (reftex-get-bib-field "author" (bibtex-parse-entry t))))))

(defun sync0-org-ref-get-citation-author (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((entry (bibtex-completion-get-entry key))
         (type (cdr (assoc "=type=" entry))))
    (if (or (equal type "collection")
            (equal type "Collection"))
        (cdr (assoc "editor" entry))
      (cdr (assoc "author" entry)))))

(setq sync0-bibtex-current-author "John Doe")
(setq sync0-bibtex-current-zettel-type "John Doe")
(setq sync0-bibtex-current-language "John Doe")
(setq sync0-bibtex-current-date "John Doe")
;; (setq sync0-bibtex-current-origdate "John Doe")

;; (let* (
;;        (key-at-point (org-ref-get-bibtex-key-under-cursor))
;;          (bibkey (if  (equal key-at-point "")
;;                      (let* ((candidates (bibtex-completion-candidates))
;;                             (selection (ivy-read "Choose BibTeX key to extract from : "
;;                                                  candidates
;;                                                  :caller 'ivy-bibtex
;;                                                  :history 'ivy-bibtex-history)))
;;                        (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
;;                    key-at-point))            


(defun sync0-bibtex-fix-author-string (author)
  "Update org properties according to info. in the bibtex entry."
  (cond ((string-match " and " author)
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

(defun sync0-bibtex-extract-lastname (author)
  "Update org properties according to info. in the bibtex entry."
  (cond ((string-match " and " author)
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
        (t  (nth 0 (split-string author ", ")))))

(defun sync0-org-ref-update-notes-file ()
  "Update org properties according to info. in the bibtex entry."
  (interactive)
  (let* ((key-at-point (org-ref-get-bibtex-key-under-cursor))
         (bibkey (if  (equal key-at-point "")
                     (let* ((candidates (bibtex-completion-candidates))
                            (selection (ivy-read "Choose BibTeX key to extract from : "
                                                 candidates
                                                 :caller 'ivy-bibtex
                                                 :history 'ivy-bibtex-history)))
                       (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
                   key-at-point))            
         (results (org-ref-get-bibtex-key-and-file bibkey))
         (bibfile (cdr results))
         (today (format-time-string "%Y-%m-%d"))
         (regex (concat "@[[:alpha:]]+{" bibkey ","))
         (epub-file (concat "file = {/home/sync0/Dropbox/pdfs/" bibkey ".epub},")))
    (with-temp-file bibfile
      (insert-file-contents bibfile)
      (unless (re-search-forward epub-file (point-max) t)
        (re-search-forward regex (point-max) t)
        (next-line)
        (insert (concat "  " epub-file "\n"))))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry bibkey nil 0)
      (let ((fields (bibtex-parse-entry t)))
        (setq sync0-bibtex-current-zettel-type
              (reftex-get-bib-field "=type=" fields))
        (if (equal sync0-bibtex-current-zettel-type "collection")
            (setq sync0-bibtex-current-author
                  (reftex-get-bib-field "editor" fields))
          (setq sync0-bibtex-current-author
                (reftex-get-bib-field "author" fields)))
        (setq sync0-bibtex-current-language
              (reftex-get-bib-field "language" fields))
        (setq sync0-bibtex-current-medium
            (sync0-split-string-with-separator (reftex-get-bib-field "medium" fields) ","))
        (setq sync0-bibtex-current-crossref
              (reftex-get-bib-field "crossref" fields))
        (setq sync0-bibtex-current-title
              (reftex-get-bib-field "title" fields))
        (setq sync0-bibtex-current-trace
              (reftex-get-bib-field "trace" fields))
        (setq sync0-bibtex-current-origdate
              (reftex-get-bib-field "origdate" fields))
        (setq sync0-bibtex-current-subtitle
              (reftex-get-bib-field "subtitle" fields))
        (setq sync0-bibtex-current-date
              (reftex-get-bib-field "date" fields))))
    (org-with-point-at 1
      (org-set-property "AUTHOR" (concat "\""
                                         (sync0-bibtex-fix-author-string
                                          sync0-bibtex-current-author)"\""))
      (org-set-property "BIBLATEX_TYPE" sync0-bibtex-current-zettel-type)
      (unless (equal sync0-bibtex-current-medium  "")
      (org-set-property "MEDIUM" sync0-bibtex-current-medium))
      (unless (equal sync0-bibtex-current-trace  "")
      (org-set-property "TRACE" sync0-bibtex-current-trace))
      (unless (equal sync0-bibtex-current-crossref  "")
      (org-set-property "CROSSREF" (concat "cite:" sync0-bibtex-current-crossref)))
      (org-set-property "DATE" (concat "\"" sync0-bibtex-current-date "\""))
      (unless (equal sync0-bibtex-current-origdate  "")
      (org-set-property "ORIG_DATE" (concat "\"" sync0-bibtex-current-origdate "\"")))
      (org-set-property "LANGUAGE" sync0-bibtex-current-language)
       (when  (re-search-forward "^#\\+TITLE:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+TITLE: " sync0-bibtex-current-title "\n")))
       (when  (re-search-forward "^#\\+SUBTITLE:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+SUBTITLE: " sync0-bibtex-current-subtitle "\n")))
       (when  (re-search-forward "^#\\+FILETAGS:" (point-max) t)
         (next-line)
         (unless (re-search-forward "^#\\+DATE:" (point-max) t)
           (insert (concat "#+DATE:" today "\n")))
         (unless (re-search-forward "^#\\+EXPORT_FILE_NAME:" (point-max) t)
           (insert (concat "#+EXPORT_FILE_NAME: /home/sync0/Dropbox/pdfs/" bibkey ".epub\n")))
         (unless (re-search-forward "^#\\+LANGUAGE:" (point-max) t)
           (insert  "#+LANGUAGE: fr\n"))
         (unless (re-search-forward "^#\\+UID:" (point-max) t)
           (insert (concat "#+UID: " bibkey "\n")))
         (unless (re-search-forward "^#\\+OPTIONS:" (point-max) t)
           (insert "#+OPTIONS: tex:dvipng broken-links:t todo:nil pri:nil\n")))
      )))

(setq org-ref-notes-function
      (lambda (thekey)
        (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
          (bibtex-completion-edit-notes
           (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

(defun sync0-visit-bibliography-in-buffer ()
  (interactive)
  (let ((bib-file
         (completing-read "Fichier Biblatex : "
                          (f-files "~/Dropbox/bibliographies" (lambda (k) (string-match-p ".bib" k))))))
    (find-file
     (expand-file-name bib-file))))

;; (defun sync0-org-ref-open-pdf-at-point-zathura ()
;;   "Open the pdf for bibtex key under point if it exists."
;;   (interactive)
;;   (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
;;                         (selection (ivy-read "Choose BibTeX key to extract from : "
;;                                              candidates
;;                                              :caller 'ivy-bibtex
;;                                              :history 'ivy-bibtex-history)))
;;                    (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
;;          (pdf-file (car (bibtex-completion-find-pdf bibkey))))
;;     (if (file-exists-p pdf-file)
;;         (call-process "zathura" nil 0 nil pdf-file)
;;       (message "No PDF found for %s" key))))

(defun sync0-org-ref-open-pdf-at-point-zathura ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
                        ;; (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
                        (key-at-point (org-ref-get-bibtex-key-under-cursor))
                        (preselect (and key-at-point
                                        (cl-position-if (lambda (cand)
                                                          (member (cons "=key=" key-at-point)
                                                                  (cdr cand)))
                                                        candidates)))
                        (selection (ivy-read "Choose BibTeX key to extract from : "
                                             candidates
                                             :preselect preselect
                                             :caller 'ivy-bibtex
                                             :history 'ivy-bibtex-history)))
                   (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
         (pdf-file (car (bibtex-completion-find-pdf bibkey))))
    (if (file-exists-p pdf-file)
        (call-process "zathura" nil 0 nil pdf-file)
      (message "No PDF found for %s" key))))


(defun sync0-org-ref-create-headline ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
                        ;; (key-at-point (org-ref-get-bibtex-key-under-cursor))
                        ;; (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
                        ;; (preselect (and key-at-point
                        ;;                 (cl-position-if (lambda (cand)
                        ;;                                   (member (cons "=key=" key-at-point)
                        ;;                                           (cdr cand)))
                        ;;                                 candidates)))
                        (selection (ivy-read "Choose BibTeX key to extract from : "
                                             candidates
                                             ;; :preselect preselect
                                             :caller 'ivy-bibtex
                                             :history 'ivy-bibtex-history)))
                   (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
         (entry (bibtex-completion-get-entry bibkey))
         (type (bibtex-completion-get-value "=type=" entry))
         (author (if (equal type "collection")
                     (bibtex-completion-get-value "editor" entry)
                   (bibtex-completion-get-value "author" entry)))
         (name (when author
                        (sync0-bibtex-fix-author-string author)))
         (lastname (when author
                            (sync0-bibtex-extract-lastname author)))
         (crossref (when (member type sync0-biblatex-crossref-types)
                     (bibtex-completion-get-value "crossref" entry)))
         (crossref-link (when crossref  
                          (let*    ((crossref-file (expand-file-name (concat "~/Dropbox/org/permanent/" crossref ".org")))
                                    (crossref-id (sync0-org-get-id crossref-file))
                                    (crossref-title (sync0-org-get-title-keyword crossref-file)))
                            (concat "[[id:" crossref-id "][" crossref-title "]]\n"))))
         (title (bibtex-completion-get-value "title" entry))
         (date (bibtex-completion-get-value "date" entry))
         (origdate (bibtex-completion-get-value "origdate" entry))
         (notes-file (expand-file-name (concat "~/Dropbox/org/permanent/" bibkey ".org")))
         (notes-id (when (file-exists-p notes-file)
                     (sync0-org-get-id notes-file)))
         (notes-title (when (file-exists-p notes-file)
                        (sync0-org-get-title-keyword notes-file)))
         (notes-link (when (file-exists-p notes-file)
                       (concat "[[id:" notes-id "][" notes-title "]]")))
         (headline (concat "* "
                           lastname
                           (unless (or (equal origdate "")
                                       (null origdate))
                             (concat " [" origdate "]"))
                           " ("
                           date
                           ") "
                           title
                           "\n:PROPERTIES:\n:EXPORT_TITLE: cite:"
                           bibkey
                           "\n:END:\n:INFO:\n:AUTHOR: "
                           "\"" name "\"\n"
                           (unless (or (equal crossref "")
                                       (null crossref))
                             (concat ":CROSSREF: cite:"
                                     crossref
                                     "\n:PARENT: "
                                     crossref-link
                                     "\n"
                                     ))
                           ":NOTES: "
                           (when (file-exists-p notes-file) notes-link)
                           "\n:END:\n")))
    (insert headline)))

;; (defun sync0-org-ref-create-headline ()
;;   "Open the pdf for bibtex key under point if it exists."
;;   (interactive)
;;   (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
;;                         ;; (key-at-point (org-ref-get-bibtex-key-under-cursor))
;;                         ;; (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
;;                         ;; (preselect (and key-at-point
;;                         ;;                 (cl-position-if (lambda (cand)
;;                         ;;                                   (member (cons "=key=" key-at-point)
;;                         ;;                                           (cdr cand)))
;;                         ;;                                 candidates)))
;;                         (selection (ivy-read "Choose BibTeX key to extract from : "
;;                                              candidates
;;                                              ;; :preselect preselect
;;                                              :caller 'ivy-bibtex
;;                                              :history 'ivy-bibtex-history)))
;;                    (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
;;          (results (org-ref-get-bibtex-key-and-file bibkey))
;;          (bibfile (cdr results))
;;          (file (expand-file-name (concat "~/Dropbox/org/permanent/" bibkey ".org")))
;;          (id (sync0-org-get-id file))
;;          (title (sync0-org-get-title-keyword file))
;;          (link   (concat "[[id:" id "][" title "]]\n")))
;;     (save-excursion
;;       (with-temp-buffer
;;         (insert-file-contents bibfile)
;;         (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
;;         (bibtex-search-entry bibkey nil 0)
;;         (let ((fields (bibtex-parse-entry t)))
;;           (setq sync0-bibtex-current-type
;;                 (reftex-get-bib-field "=type=" fields))
;;           (if (equal sync0-bibtex-current-type "collection")
;;               (setq sync0-bibtex-current-author-name
;;                     (reftex-get-bib-field "editor"
;;                                           fields))
;;             (setq sync0-bibtex-current-author-name
;;                   (reftex-get-bib-field "author"
;;                                         fields)))
;;           (when (member sync0-bibtex-current-type sync0-biblatex-crossref-types)
;;             (setq sync0-bibtex-current-crossref
;;                   (reftex-get-bib-field "crossref"
;;                                         fields))
;;             (unless (or (equal sync0-bibtex-current-crossref "")
;;                         (null sync0-bibtex-current-crossref))
;;               (let*    ((crossref-file (expand-file-name (concat "~/Dropbox/org/permanent/" sync0-bibtex-current-crossref ".org")))
;;                         (crossref-id (sync0-org-get-id crossref-file))
;;                         (crossref-title (sync0-org-get-title-keyword crossref-file)))
;;                 (setq sync0-bibtex-current-crossref-link
;;                       (concat "[[id:" crossref-id "][" crossref-title "]]\n")))))
;;           (setq sync0-bibtex-current-author
;;                 (sync0-bibtex-fix-author-string sync0-bibtex-current-author-name))
;;           (setq sync0-bibtex-current-author-lastname
;;                 (sync0-bibtex-extract-lastname sync0-bibtex-current-author-name))
;;           (setq sync0-bibtex-current-title
;;                 (reftex-get-bib-field "title" fields))
;;           (setq sync0-bibtex-current-origdate
;;                 (reftex-get-bib-field "origdate" fields))
;;           (setq sync0-bibtex-current-date
;;                 (reftex-get-bib-field "date" fields)))))
;;     (insert (concat 
;;              "* "
;;              sync0-bibtex-current-author-lastname
;;              (unless (or (equal sync0-bibtex-current-origdate "")
;;                          (null sync0-bibtex-current-origdate))
;;                (concat " ["
;;                        sync0-bibtex-current-origdate
;;                        "]"))
;;              " ("
;;              sync0-bibtex-current-date
;;              ") "
;;              sync0-bibtex-current-title
;;              "\n:PROPERTIES:\n:EXPORT_TITLE: cite:"
;;              bibkey
;;              "\n:END:\n:INFO:\n:AUTHOR: "
;;              "\"" sync0-bibtex-current-author "\"\n"
;;              (unless (or (equal sync0-bibtex-current-crossref "")
;;                          (null sync0-bibtex-current-crossref))
;;                (concat ":CROSSREF: cite:"
;;                        sync0-bibtex-current-crossref
;;                        "\n:PARENT: "
;;                        sync0-bibtex-current-crossref-link
;;                        "\n"
;;                        ))
;;              ":NOTES: " link "\n:END:\n"))))


(defun sync0-org-ref-copy-pdf-to-path ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
                        ;; (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
                        (key-at-point (org-ref-get-bibtex-key-under-cursor))
                        (preselect (and key-at-point
                                        (cl-position-if (lambda (cand)
                                                          (member (cons "=key=" key-at-point)
                                                                  (cdr cand)))
                                                        candidates)))
                        (selection (ivy-read "Choose BibTeX key to extract from : "
                                             candidates
                                             :preselect preselect
                                             :caller 'ivy-bibtex
                                             :history 'ivy-bibtex-history)))
                   (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
         (results (org-ref-get-bibtex-key-and-file bibkey))
         (bibfile (cdr results))
         (target-path (read-string "Où envoyer ce pdf ? ")))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry bibkey nil 0)
      (let ((fields (bibtex-parse-entry t)))
        (setq sync0-bibtex-current-author
              (replace-regexp-in-string "[[:space:]]+" "\\\\ " 
                                        (sync0-bibtex-extract-lastname
                                         (reftex-get-bib-field "author"
                                                               fields))))
        (setq sync0-bibtex-current-title
              (replace-regexp-in-string "[[:space:]]+" "\\\\ " 
                                        (reftex-get-bib-field "title" fields)))
        (setq sync0-bibtex-current-file
            (reftex-get-bib-field "file" fields))
      (setq sync0-bibtex-current-date
            (reftex-get-bib-field "date" fields))))
  (if (file-exists-p sync0-bibtex-current-file)
      (let ((command (concat "cp "
                             sync0-bibtex-current-file
                             " "
                             target-path
                             sync0-bibtex-current-author
                             "_"
                             sync0-bibtex-current-date
                             "_"
                             sync0-bibtex-current-title
                             ".pdf")))
        (shell-command command)
        (message "PDF for %s moved to target location" bibkey))
    (message "No PDF found for %s" bibkey))))

(defun sync0-org-ref-open-notes ()
  "Open the notes for bibtex key under point in a cite link in a
buffer. Can also be called with key."
  (interactive)
  (let ((bibkey (let* ((candidates (bibtex-completion-candidates))
                       (key-at-point (org-ref-get-bibtex-key-under-cursor))
                       ;; (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
                       (preselect (and key-at-point
                                       (cl-position-if (lambda (cand)
                                                         (member (cons "=key=" key-at-point)
                                                                 (cdr cand)))
                                                       candidates)))
                       (selection (ivy-read "Choose BibTeX key to extract from : "
                                            candidates
                                            :preselect preselect
                                            :caller 'ivy-bibtex
                                            :history 'ivy-bibtex-history)))
                  (cdr (assoc "=key=" (cdr (assoc selection candidates)))))))
    (funcall org-ref-notes-function bibkey)))

(defun sync0-org-insert-link-to-notes-from-bibkey ()
  "Insert org-mode link to buffer"
  (interactive)
  (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
                        ;; (key-at-point (org-ref-get-bibtex-key-under-cursor))
                        ;; (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
                        ;; (preselect (and key-at-point
                        ;;                 (cl-position-if (lambda (cand)
                        ;;                                   (member (cons "=key=" key-at-point)
                        ;;                                           (cdr cand)))
                        ;;                                 candidates)))
                        (selection (ivy-read "Choose BibTeX key to extract from : "
                                             candidates
                                             ;; :preselect preselect
                                             :caller 'ivy-bibtex
                                             :history 'ivy-bibtex-history)))
                   (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
         (file (expand-file-name (concat "~/Dropbox/org/permanent/" bibkey ".org")))
         (id (sync0-org-get-id file))
         (title (sync0-org-get-title-keyword file)))
    (insert
     (concat "[[id:" id "][" title "]]\n"))))

(evil-leader/set-key
  "C" 'org-ref-ivy-insert-cite-link)

(provide 'sync0-org-ref-functions)
