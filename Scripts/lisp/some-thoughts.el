(defun foo ()
(interactive)
(when-let* ((info default-directory)
            (fixed (split-string-and-unquote info "/"))
            (rest (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":")))
  (when (and (member "projects" rest)
              (not (member "project" tags)))
      (org-with-point-at 1
        (re-search-forward "^#\\+FILETAGS:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+FILETAGS: :project"  tags-line "\n\n"))))))

(defun foo ()
(interactive)
(when-let* ((info default-directory)
            (fixed (split-string-and-unquote info "/"))
            (rest (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
            (tag (when (member  "projects" rest) (cadr rest)))
            (tags-line (cadar (org-collect-keywords '("FILETAGS")))))
  (unless (null tag)
      (org-with-point-at 1
        (org-set-property "PROJECT_TITLE" tag)  
        (re-search-forward "^#\\+FILETAGS:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+FILETAGS: :" tag tags-line "\n\n"))))))

(defun foo ()
(interactive)
(let* ((info default-directory)
            (fixed (split-string-and-unquote info "/"))
            (rest (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
            (tag (when (member  "projects" rest) (cadr rest)))
            (tags-line (cadar (org-collect-keywords '("FILETAGS")))))
  (unless (null tag)
      (org-with-point-at 1
        (org-set-property "PROJECT_TITLE" tag)  
        (re-search-forward "^#\\+FILETAGS:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+FILETAGS: :" tag tags-line "\n\n"))))))

(defun foo ()
(interactive)
(when-let* ((info default-directory)
            (fixed (split-string-and-unquote info "/"))
            (rest (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
            (last-tag (when (member  "projects" rest) (cadr rest)))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":")))
  (unless (null last-tag)
      (org-with-point-at 1
        (org-set-property "PROJECT_TITLE" last-tag)  
        (re-search-forward "^#\\+FILETAGS:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+FILETAGS: :" last-tag tags-line "\n\n"))))))

(defun foo ()
"replace the reference for numbers"
(interactive)
(when-let* ((info default-directory)
            (fixed (split-string-and-unquote info "/"))
            (rest (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
            (last-tag (when (member  "reference" rest) (cadr rest)))
            (tags-line (cadar (org-collect-keywords '("INTERLEAVE_PDF"))))
            (tags (split-string-and-unquote tags-line ":")))
  (unless (null last-tag)
      (org-with-point-at 1
        (org-set-property "PROJECT_TITLE" last-tag)  
        (re-search-forward "^#\\+FILETAGS:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+FILETAGS: :" last-tag tags-line "\n\n"))))))


;;;;
;;;; Change one tag for another
(defun foo ()
(interactive)
(when-let*  ((old (read-string "old tag: "))
             (new (read-string "new tag: "))
             (info (cadar (org-collect-keywords '("FILETAGS"))))
              (fixed (replace-regexp-in-string old new  info)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))

(dolist (f (org-roam--list-all-files))
  (with-current-buffer (find-file-noselect f)
    (foo)))
;;;;


(defun foo ()
(interactive)
(when-let*  ((info (cadar (org-collect-keywords '("FILETAGS"))))
              (fixed (replace-regexp-in-string "projects" "project"  info)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))

(defun foo ()
(interactive)
(let*  ((info (cadar (org-collect-keywords '("FILETAGS"))))
              (fixed (replace-regexp-in-string "annotations" "annotation"  info)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))


(defun foo ()
(interactive)
 (when-let*    ((type (org-entry-get 1 "ZETTEL_TYPE"))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":"))
            (first-tag (car tags)))
      (when (equal type "annotation")
      (org-with-point-at 1
        (org-set-property "ANNOTATION_REFS" first-tag)))))

(defun foo ()
(interactive)
 (let*    ((type (org-entry-get 1 "ZETTEL_TYPE"))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":"))
            (first-tag (car tags)))
      (when (equal type "annotation")
      (org-with-point-at 1
        (org-set-property "ANNOTATION_REFS" first-tag)))))


 (let*    ((type (org-entry-get 1 "ZETTEL_TYPE"))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":"))
            (first-tag (car tags)))
      (when (equal type "annotation")
      (org-with-point-at 1
        (org-set-property "ANNOTATION_REFS" first-tag))))



(defun foo ()
(interactive)
(when-let* ((type (org-entry-get 1 "PROJECT_TITLE"))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":")))
  (unless (or (null type)
              (member type tags))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
               (kill-whole-line 1)
               (insert (concat "#+FILETAGS: :" type tags-line "\n\n")))))))

(defun foo ()
(interactive)
(when-let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/"))
         (tags-line    (org-with-point-at 1 (re-search-forward "^#\\+FILETAGS: :\\(.+\\):" (point-max) t)
                            (match-string 1)))
         (tags (split-string-and-unquote tags-line ":"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
(last-tag (when (equal (car matcher) "archives") (cadr matcher))))
(org-with-point-at 1
        (unless (null  last-tag)
        (org-set-property "DOCUMENT_TYPE" last-tag)))))

(defun foo ()
(interactive)
(when-let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/"))
         (tags-line    (org-with-point-at 1 (re-search-forward "^#\\+FILETAGS: :\\(.+\\):" (point-max) t)
                            (match-string 1)))
         (tags (split-string-and-unquote tags-line ":"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
(last-tag (when (equal (car matcher) "permanent") (cadr matcher))))
(org-with-point-at 1
        (unless (null  last-tag)
        (org-set-property "PROJECT_TITLE" last-tag)))))


(defun foo ()
(interactive)
(when-let* ((fiche-type (org-entry-get 1 "PROJECT_TITLE"))
            (zettel-type (org-entry-get 1 "ZETTEL_TYPE"))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":")))
  ;; (unless  (member fiche-type tags)
  ;;     (org-with-point-at 1
  ;;       (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
  ;;              (kill-whole-line 1)
  ;;              (insert (concat "#+FILETAGS: :" fiche-type tags-line "\n\n")))))
  (unless (member zettel-type tags)
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
               (kill-whole-line 1)
               (insert (concat "#+FILETAGS: :" zettel-type tags-line "\n\n")))))))

(defun foo ()
(interactive)
(let* ((fiche-type (org-entry-get 1 "FICHE_TYPE"))
            (zettel-type (org-entry-get 1 "ZETTEL_TYPE"))
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":")))
  (unless  (member fiche-type tags)
      (org-with-point-at 1
        (when (re-search-forward "^#\\+FILETAGS:" (point-max) t)
               (kill-whole-line 1)
               (insert (concat "#+FILETAGS: :" fiche-type tags-line "\n\n")))))
  (unless (member zettel-type tags)
      (org-with-point-at 1
        (when (re-search-forward "^#\\+FILETAGS:" (point-max) t)
               (kill-whole-line 1)
               (insert (concat "#+FILETAGS: :" zettel-type tags-line "\n\n")))))))


(defun foo ()
(interactive)
(when-let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/"))
         (tags-line    (org-with-point-at 1 (re-search-forward "^#\\+FILETAGS: :\\(.+\\):" (point-max) t)
                            (match-string 1)))
         (tags (split-string-and-unquote tags-line ":"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal)))
(org-with-point-at 1
 (when (member "oeuvres" matcher) 
        (org-set-property "FICHE_TYPE" "reference")))))

(defun foo ()
(interactive)
(when-let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/"))
         (tags-line    (org-with-point-at 1 (re-search-forward "^#\\+FILETAGS: :\\(.+\\):" (point-max) t)
                            (match-string 1)))
         (tags (split-string-and-unquote tags-line ":"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal))
(last-tag (when (equal (car matcher) "fiches") (cadr matcher))))
(org-with-point-at 1
        (unless (null  last-tag)
        (org-set-property "FICHE_TYPE" last-tag)))))


(defun foo ()
(interactive)
(let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/"))
         (tags-line    (progn (re-search-forward "^#\\+FILETAGS: :\\(.+\\):" (point-max) t))
                            (match-string 1))
         (tags (split-string-and-unquote tags-line ":"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))

(defun foo ()
(interactive)
(let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/")))
      (org-with-point-at 1
        (when (member "fiches" fixed)
        (org-set-property "ZETTEL_TYPE" "fiche")))))

(defun foo ()
(when-let*  ((info default-directory)
             (fixed (split-string-and-unquote info "/")))
      (org-with-point-at 1
        (when (member "bibliographies" fixed)
        (org-set-property "ZETTEL_TYPE" "bibliography")))))

(defun foo ()
(when-let*  ((info default-directory)
             (fixed (split-string-and-unquote info "/")))
      (org-with-point-at 1
        (when (member "archives" fixed)
        (org-set-property "ZETTEL_TYPE" "archive")))))

(defun foo ()
(when-let*  ((info default-directory)
             (fixed (split-string-and-unquote info "/")))
      (org-with-point-at 1
        (when (member "references" fixed)
        (org-set-property "ZETTEL_TYPE" "reference")))))

(defun foo ()
(when-let*  ((info default-directory)
             (fixed (split-string-and-unquote info "/")))
      (org-with-point-at 1
        (when (member "projects" fixed)
        (org-set-property "ZETTEL_TYPE" "project")))))

(defun foo ()
(when-let*  ((info default-directory)
             (fixed (split-string-and-unquote info "/")))
      (org-with-point-at 1
        (when (member "permanent" fixed)
        (org-set-property "ZETTEL_TYPE" "permanent")))))


(defun foo ()
(when-let*  ((info (org-entry-get 1 "JOURNAL_TITLE")))
      (org-with-point-at 1
        (org-set-property "JOURNAL_TITLE" (concat "\"" info "\"")))))
        ;;(while (re-search-forward "^#\\+AUTHOR:" (point-max) t)

(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("JOURNAL_TITLE")))))
      (org-with-point-at 1
        (org-set-property "JOURNAL_TITLE" (concat "\"" info "\"")))))
        ;;(while (re-search-forward "^#\\+AUTHOR:" (point-max) t)
          
(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("JOURNAL_TITLE")))))
      (org-with-point-at 1
        (org-set-property "JOURNAL_TITLE" info)
        (while (re-search-forward "^#\\+JOURNAL_TITLE:" (point-max) t)
          (kill-whole-line 1)))))

(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("BOOK_TITLE")))))
      (org-with-point-at 1
        (org-set-property "BOOK_TITLE" info)
        (while (re-search-forward "^#\\+BOOK_TITLE:" (point-max) t)
          (kill-whole-line 1)))))


(defun foo ()
(interactive)
      (org-with-point-at 1
      ;;  (org-set-property "CREATED" info)
        (when (or (re-search-forward "^#\\+ROAM_KEY:#\\+CREATED:" (point-max) t)
               (re-search-forward "^#\\+ROAM_ALIAS:#\\+CREATED:" (point-max) t))
          (replace-match "#+CREATED:"))))

(defun foo ()
(interactive)
      (org-with-point-at 1
      ;;  (org-set-property "CREATED" info)
        (when (re-search-forward ":ROAM_REFS: \\(cite:\\)" (point-max) t)
          (replace-match "" nil nil nil 1))))


(defun foo ()
(interactive)
(let*  ((info (cadar (org-collect-keywords '("FILETAGS"))))
             (fixed (replace-regexp-in-string "\\(\"\\)" ""  info nil nil 1)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))


(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("FILETAGS"))))
             (fixed (replace-regexp-in-string "\\(\"\\)" ""  info nil nil 1)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))



(when-let* ((aliases (mapcan #'split-string-and-unquote
                               (cdar (org-collect-keywords '("CREATED"))))))    


(setq tester "/home/sync0/Dropbox/org/permanent/doctorat/")

(setq tester "/home/sync0/Dropbox/org/fiches/concepts/")


(let*  ((info tester)
         (fixed (split-string-and-unquote info "/"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal)))
matcher)



;; the funciton to use to put back the tags where they belong from the file path

(defun foo ()
(interactive)
(let*  ((info default-directory)
         (fixed (split-string-and-unquote info "/"))
         (tags-line    (progn (re-search-forward "^#\\+FILETAGS: :\\(.+\\):" (point-max) t))
                            (match-string 1))
         (tags (split-string-and-unquote tags-line ":"))
         (matcher (cl-set-difference fixed '("home" "sync0" "Dropbox"  "org")  :test #'equal)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))



#+FILETAGS: :adddd:people:sf:

#+ROAM_ALIAS:#+CREATED:

#+FILETAGS: philosophy kantians people kuehn_manfred notetaking



(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("FILETAGS"))))
             (fixed (replace-regexp-in-string "\"" ""  info nil nil 1)))
      (org-with-point-at 1
        (while (re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " fixed "\n\n"))))))
          

(let*  ((info (cadar (org-collect-keywords '("FILETAGS"))))
             (fixed (replace-regexp-in-string "\"[A-z]+\\(:\\)[A-z]+\"" "_"  info nil nil 1)))
fixed)




(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("CREATED")))))
      (org-with-point-at 1
        (org-set-property "CREATED" info)
        (while (re-search-forward "^#\\+ROAM_ALIAS:#\\+CREATED:" (point-max) t)
          (kill-whole-line 1)))))

(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("CREATED")))))
      (org-with-point-at 1
        (org-set-property "CREATED" info)
        (while (re-search-forward "^#\\+CREATED:" (point-max) t)
          (kill-whole-line 1)))))


(defun foo ()
(when-let*  ((info (cadar (org-collect-keywords '("DATE")))))
      (org-with-point-at 1
        (org-set-property "LAST_MODIFIED" info)
        (while (re-search-forward "^#\\+DATE:" (point-max) t)
          (kill-whole-line 1)))))


(defun foo2 ()
(interactive)
  (let* ((info (cadar (org-collect-keywords '("DATE"))))
         (fixed (replace-regexp-in-string "/" "-"  info))
        (case-fold-search t))
      (org-with-point-at 1
        (org-set-property "LAST_MODIFIED" fixed)
        (while (re-search-forward "^#\\+DATE:" (point-max) t)
          (kill-whole-line 1)))))


(when-let* ((aliases (mapcan #'split-string-and-unquote
                               (cdar (org-collect-keywords '("roam_alias"))))))    
    (let ((case-fold-search t))


    (let ()
      (org-with-point-at 1
         (org-set-property "ROAM_ALIASES" (combine-and-quote-strings aliases))
        (while (re-search-forward "^#\\+roam_alias:" (point-max) t)
          (kill-line 1)))))


 (org-with-point-at 1
    (let ((case-fold-search t))
      (while (search-forward "#+roam_tags:" nil t)
        (replace-match "#+filetags:" nil t))))
  (save-buffer))


(defun org-roam--extract-titles-alias ()
  "Return the aliases from the current buffer.
Reads from the \"roam_alias\" property."
  (let* ((prop (org-roam--extract-global-props '("ROAM_ALIASES")))
         (aliases (or (cdr (assoc "ROAM_ALIASES" prop))
                      "")))
    (condition-case nil
        (split-string-and-unquote aliases)
      (error
       (progn
         (lwarn '(org-roam) :error
                "Failed to parse aliases for buffer: %s. Skipping"
                (or org-roam-file-name
                    (buffer-file-name)))
         nil)))))


(setq sync0-counter 0)

(defun sync0-new-name ()
"Create a new filename from a time string"
(interactive)
(let ((value (1+ sync0-counter))
       (base (string-to-number (format-time-string "%Y%m%d%H%M%S"))))
(setq sync0-counter value)
(number-to-string (+ base value))))


(setq sync0-biblios (f-files "/home/sync0/Dropbox/bibliographies/"))

(defun foo-single ()
"replace the filename for numbers in current filename and in its
corresponding bibtex entry"
(interactive)
(let* ((filename (sync0-new-name))
        (inter-path (org-collect-keywords '("INTERLEAVE_PDF")))
        (directory "/home/sync0/Documents/pdfs/")
        (path (concat directory filename ".pdf"))
        (pdf-path
         (when inter-path
             (cadar (org-collect-keywords '("INTERLEAVE_PDF")))))
        (raw-ref (org-entry-get 1 "ROAM_REFS"))
        (ref (if (string-match-p "\"[[:graph:]]+\"" raw-ref)
             (string-trim raw-ref "\"" "\"")
                raw-ref))
        (old-location (concat default-directory ref ".org"))
        (new-location (concat default-directory filename ".org")))
;; do this on the file
;; first, rename it
(rename-file old-location new-location)
    (with-current-buffer
        (find-file-noselect new-location)
      (org-with-point-at 1
        (org-set-property "ROAM_REFS" filename)  
        (while (search-forward ref (point-max) t)
          (replace-match filename))
       (when  (re-search-forward "^#\\+INTERLEAVE_PDF:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+INTERLEAVE_PDF: " path "\n\n")))))
(dolist (biblio sync0-biblios)
    (with-current-buffer
        (find-file-noselect biblio)
        (goto-char (point-min))
        (when (search-forward ref (point-max) t)
          (replace-match filename)
       ;; (when  (re-search-forward "^file = " (point-max) t)
       ;;  (kill-whole-line 1)
       ;;  (insert (concat "#+INTERLEAVE_PDF: " path "\n\n")))
        (if pdf-path
            (progn
              (search-forward pdf-path (point-max) t)
       (replace-match path)
(when (file-exists-p pdf-path)
(rename-file pdf-path path)))
(progn 
(next-line)
(beginning-of-line)
        (insert (concat "file = {" path "},\n")))))))))
          

(defun foo ()
(interactive)
      (org-with-point-at 1
      ;;  (org-set-property "CREATED" info)
        (when (re-search-forward "^#\\+ROAM_ALIAS:#\\+roam_key:" (point-max) t)
          (replace-match ""))))


;; this code applies a function (looped) named "foo" to all files
;; in my zettelkasten sub directory "references"
(dolist (f
(f-files sync0-zettelkasten-directory
         (lambda (k) (string-match-p "reference\\/[A-z]+" k)) t))
  (with-current-buffer (find-file-noselect f)
    (foo-single)))


(dolist (f (org-roam--list-all-files))
  (with-current-buffer (find-file-noselect f)
    (foo)))
