(defun foo-single ()

"replace the filename for numbers in current filename and in its
corresponding bibtex entry"
(interactive)
(when-let* ((raw-ref (org-entry-get 1 "CROSSREF"))
            (ref (string-trim raw-ref "\"" "\""))
            (new-ref (concat "cite:" ref)))
;; do this on the file
;; first, rename it
      (org-with-point-at 1
        (org-set-property "CROSSREF" new-ref))))  

    (dolist (f
(f-files sync0-zettelkasten-directory
         (lambda (k) (string-match-p "permanent" k)) t))
  (with-current-buffer (find-file-noselect f)
    (foo-single)))

    (dolist (f
(f-files sync0-zettelkasten-directory
         (lambda (k) (string-match-p "permanent" k)) t))
  (with-current-buffer (find-file-noselect f)
    (sync0-obsidian-reference-migrate)))


;; (let ((test "path/like/this"))
;;               (when (string-match "/" test)
;; (message "say yes")))

 
