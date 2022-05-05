(defun foo-single ()
"replace the filename for numbers in current filename and in its
corresponding bibtex entry"
(interactive)
(when-let* ((current-prop (org-entry-get 1 "ZETTEL_TYPE"))
            (raw-prop (string-trim current-prop "\"" "\"")))
;; do this on the file
;; first, rename it
      (org-with-point-at 1
        (org-set-property "ZETTEL_TYPE" raw-prop))))  

(defun foo-single ()
"replace the filename for numbers in current filename and in its
corresponding bibtex entry"
(interactive)
(when-let* ((current-prop (org-entry-get 1 "BIBLATEX_TYPE"))
            (raw-prop (string-trim current-prop "\"" "\"")))
;; do this on the file
;; first, rename it
      (org-with-point-at 1
        (org-set-property "BIBLATEX_TYPE" raw-prop))))  

(defun foo-single ()
"replace the filename for numbers in current filename and in its
corresponding bibtex entry"
(interactive)
(when-let* ((current-prop (org-entry-get 1 "FICHE_TYPE"))
            (raw-prop (string-trim current-prop "\"" "\"")))
;; do this on the file
;; first, rename it
      (org-with-point-at 1
        (org-set-property "FICHE_TYPE" raw-prop))))  

(defun foo-single ()
"replace the filename for numbers in current filename and in its
corresponding bibtex entry"
(interactive)
(when-let* ((current-prop (org-entry-get 1 "ZETTEL_FUNCTION"))
            (raw-prop (string-trim current-prop "\"" "\"")))
;; do this on the file
;; first, rename it
      (org-with-point-at 1
        (org-set-property "ZETTEL_FUNCTION" raw-prop))))  


(dolist (f (org-roam--list-all-files))
  (with-current-buffer (find-file-noselect f)
    (foo-single)))
