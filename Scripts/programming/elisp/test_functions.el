
(defun sync0-ivy-bibtex-extractor ()
  (interactive)
 (let*   ((pre-entry   (ivy-completing-read "Select from list: " (bibtex-completion-candidates)))
           (key   (progn (string-match "[[:blank:]]\\([[:graph:]]+$\\)" pre-entry)
                  (match-string 1 pre-entry)))
         (entry (bibtex-completion-get-entry1 key))
         (entity (ivy-completing-read "Choose one: " '("=key=" "title" "author" "journal" "date" "editor")))
         (extraction (bibtex-completion-get-value entity entry)))
       (insert  extraction)))






(defun foo (arg)
  (interactive
   (list
    (ivy-completing-read "Choose one: " (bibtex-completion-candidates))))
  (let* ((biby (list arg))
         (key (car biby))
         (entry (bibtex-completion-get-entry1 key))
         (title (bibtex-completion-get-value "title" entry)))
       (message "%s" title)))


(defun foo ()
  (interactive)
 (let*   ((pre-entry   (ivy-completing-read "Select from list: " (bibtex-completion-candidates)))
           (key   (progn (string-match "[[:blank:]]\\([[:graph:]]+$\\)" pre-entry)
                  (match-string 1 pre-entry)))
         (entry (bibtex-completion-get-entry1 key))
         (title (bibtex-completion-get-value "title" entry)))
       (message "%s" title)))


((=key= . yuhas2016) (=type= . online) (author . Yuhas, Alan) (title . Would you bet against sex robots?) (date . 2016-02-13) (url . https://www.theguardian.com/technology/2016/feb/13/artificial-intelligence-ai-unemployment-jobs-moshe-vardi) (subtitle . AI `could leave half of world unemployed') (language . english) (organization . The Guardian) (urldate . 2019-06-02) (langid . english) (langidopts . variant=american) (keywords . yuhas-alan))




















(defun foo ()
  (interactive)
 (let*   ((biby   (ivy-completing-read "Select from list: " (bibtex-completion-candidates)))
          (beast  (list    biby))
          (key    (progn (re-search-backward "[[:blank:]]\\([[:graph:]]+\\)" nil nil 1)
                  (match-string 1))))
       (message "%s" key)))


        (re-search-forward "#\\+CREATOR: \\([[:graph:]]+\\)" nil t 1)
      (match-string 1)

        (let ((bibtex-key (re-search-backward "[[:blank:]][[:graph:]]+" nil nil 1)))

2016-02-13 Would you bet against sex robots? Yuhas, Alan online yuhas2016


(defun foo ()
  (interactive)
 (let*   ((biby (list (ivy-completing-read "Select from list: " (bibtex-completion-candidates)))))
       (message "%s" biby)))



(defun foo ()
  (interactive)
 (let*   ((biby (list (ivy-completing-read "Select from list: " (bibtex-completion-candidates))))
          (key (last  biby)))
       (message "%s" key)))


          (entry (bibtex-completion-get-entry1 key))
          (title (bibtex-completion-get-value "title" entry)))

       (message "%s" title)))



;; 
(bibtex-completion-get-entry1 "frobert2011")


(defun sync0-org-ref-open-pdf-at-point ()


  (defun sync0-bibtex-insert-title ()
    (interactive)

(bibtex-completion-candidates)



      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (car(bibtex-completion-find-pdf key))))


(defun foo (arg)
  (interactive
   (list
    (ivy-completing-read "Choose one: " (bibtex-completion-candidates))))
  (message "%s" arg))


(defun foo (list)
  (interactive)
  (let ((arg (ivy-completing-read "Select from list: " list)))
  (message "%s" arg)))

(defun foo (arg)
  (interactive
   (list
    (ivy-completing-read "Choose one: " (bibtex-completion-candidates))))
  (let* ((key (last (list arg))))
       (message "%s" key)))

(last '(2016-02-13 Would you bet against sex robots? Yuhas, Alan online yuhas2016))




            







      (insert title))

  (message "this is %s" arg))

(let* ((entry (bibtex-completion-get-entry1 entry-key))
       (crossref (bibtex-completion-get-value "title" entry)))
      (insert title))


  (defun sync0-bibtex-insert-title ()
    (interactive)
    ;; (bibtex-completion-init)
    (let* ((candidates (bibtex-completion-candidates))
             (key (car results))
           (key (bibtex-completion-key-at-point)))
      (insert title)))

(defun foo (list)
  (interactive)
  (let ((arg (ivy-completing-read "Select from list: " list)

              (completing-read "Complete a foo: " (list org-ref-get-bibtex-keys))


    (defun sync0-org-ref-open-pdf-at-point ()
      "Open the pdf for bibtex key under point if it exists."
      (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (car(bibtex-completion-find-pdf key))))
        (if (file-exists-p pdf-file)
            (org-open-file pdf-file))
        (message "No PDF found for %s" key)))


    (bibtex-completion-get-entry 'frobert2011)

    (org-ref-reftex-get-bib-field "title" "frobert2011")
    (org-ref-reftex-get-bib-field "title" "frobert2011")

    (defun org-ref-reftex-get-bib-field (field entry &optional format)

    (defun org-ref-reftex-get-bib-field (field entry &optional format)
  "Get FIELD from a bibtex ENTRY in optional FORMAT.
Similar to `reftex-get-bib-field', but removes enclosing braces
and quotes in FIELD in the bibtex ENTRY."
  (let ((result))
    (setq result (reftex-get-bib-field field entry format))
    (when (and (not (string= result "")) (string= "{" (substring result 0 1)))
      (setq result (substring result 1 -1)))
    (when (and (not (string= result "")) (string= "\"" (substring result 0 1)))
      (setq result (substring result 1 -1)))
    result))

    (reftex-get-bib-field (list "title" "frobert2011"))

    (reftex-get-bib-field  "title" (list 'frobert2011))

(defun reftex-get-bib-field (fieldname entry &optional format)
  "Extract the field FIELDNAME from ENTRY.
If FORMAT is non-nil `format' entry accordingly."
  (let ((cell (assoc fieldname entry)))
    (if cell
        (if format
            (format format (cdr cell))
          (cdr cell))
      "")))




      (defun sync0-org-ref-open-pdf-at-point ()
      "Open the pdf for bibtex key under point if it exists."
      (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (car (bibtex-completion-find-pdf key))))
        (if (file-exists-p pdf-file)
            (org-open-file pdf-file))
        (message "No PDF found for %s" key)))



    
(defun bibtex-completion-get-entry (entry-key)
  "Given a BibTeX key this function scans all bibliographies listed in `bibtex-completion-bibliography' and returns an alist of the record with that key.
Fields from crossreferenced entries are appended to the requested entry."
  (let* ((entry (bibtex-completion-get-entry1 entry-key))
         (crossref (bibtex-completion-get-value "crossref" entry))
         (crossref (when crossref (bibtex-completion-get-entry1 crossref))))
    (bibtex-completion-remove-duplicated-fields (append entry crossref))))

(defun bibtex-completion-get-entry1 (entry-key &optional do-not-find-pdf)
  (let ((bib (bibtex-completion-normalize-bibliography 'bibtex)))
    (with-temp-buffer
      (mapc #'insert-file-contents bib)
      (goto-char (point-min))
      (if (re-search-forward (concat "^[ \t]*@\\(" parsebib--bibtex-identifier
                                     "\\)[[:space:]]*[\(\{][[:space:]]*"
                                     (regexp-quote entry-key) "[[:space:]]*,")
                             nil t)
          (let ((entry-type (match-string 1)))
            (reverse (bibtex-completion-prepare-entry
                      (parsebib-read-entry entry-type (point) bibtex-completion-string-hash-table) nil do-not-find-pdf)))
        (progn
          (display-warning :warning (concat "Bibtex-completion couldn't find entry with key \"" entry-key "\"."))
          nil)))))



(defun bibtex-completion-get-value ( entry &optional default)

  
