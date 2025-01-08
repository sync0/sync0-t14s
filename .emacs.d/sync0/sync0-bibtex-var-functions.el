(require 'sync0-bibtex-vars)

(defun sync0-bibtex-update-var (field)
  "Update completion variables based on the information provided by a BibTeX field."
  (when-let* ((my-list (assoc field sync0-bibtex-completion-variables-list))
              (new-object (eval (cadr my-list)))
              (completion-var (caddr my-list))
              (completion-list (symbol-value completion-var))
              (completion-file (cadddr my-list)))
    (when new-object
      (let ((elements (cond
                       ((member field sync0-bibtex-people-fields)
                        (if (string-match " and " new-object)
                            (string-split new-object " and ")
                          (list (string-trim new-object "{" "}"))))
                       ((member field sync0-bibtex-string-multiple-fields)
                        (if (string-match ", " new-object)
                            (string-split new-object ", ")
                          (list new-object)))
                       (t (list new-object)))))
        (dolist (element elements)
          (unless (member element completion-list)
            (setq completion-list (cons element completion-list))
            (set completion-var completion-list)
            (append-to-file (concat element "\n") nil completion-file)))))))

;; (defun sync0-bibtex-recalc-master-bibliography ()
;;   "Recalculate the master bibliography file based on other bibliography files.
;; The master bibliography file is determined by `sync0-bibtex-master-bibliography'.
;; The list of other bibliography files is specified by `sync0-bibtex-bibliographies'.
;; Any bibliography file listed in `sync0-bibtex-bibliographies' but not `sync0-bibtex-sick-bibliography' will be appended to the master bibliography file.

;; This function also replaces smart quotes with their corresponding simple equivalents."
;;   (interactive)
;;   (condition-case err
;;       (let ((master-bibliography sync0-bibtex-master-bibliography)
;;             (bibliographies (remove sync0-bibtex-sick-bibliography sync0-bibtex-bibliographies)))
;;         (with-temp-file master-bibliography
;;           (dolist (biblio bibliographies)
;;             (insert-file-contents biblio)
;;             (goto-char (point-max))
;;             (insert "\n")))
;;         (goto-char (point-min))
;;         (message "Master bibliography file has been recalculated."))
;;     (file-error
;;      (message "Error: Unable to recalculate master bibliography. %s" (error-message-string err)))))

(defun sync0-bibtex-recalc-master-bibliography ()
  "Recalculate the master bibliography file based on other bibliography files.
The master bibliography file is determined by `sync0-bibtex-master-bibliography`.
The list of other bibliography files is specified by `sync0-bibtex-bibliographies`.
Any bibliography file listed in `sync0-bibtex-bibliographies` but not `sync0-bibtex-sick-bibliography` will be appended to the master bibliography file.

This function also replaces smart quotes with their corresponding simple equivalents and sorts the final bibliography entries."
  (interactive)
  (condition-case err
      (let ((master-bibliography sync0-bibtex-master-bibliography)
            (bibliographies (remove sync0-bibtex-sick-bibliography sync0-bibtex-bibliographies))
            (temp-file (make-temp-file "bibtex-temp-" nil ".bib")))
        ;; Step 1: Combine the bibliography files
        (with-temp-file temp-file
          (dolist (biblio bibliographies)
            (insert-file-contents biblio)
            (goto-char (point-max))
            (insert "\n")))
        
        ;; Step 2: Sort the combined entries using bibtex-mode
        (with-temp-buffer
          (insert-file-contents temp-file)
          (bibtex-sort-buffer) ;; Sort entries by citekey using bibtex-mode
          (write-file master-bibliography)) ;; Write sorted content to the master bibliography file
        
        ;; Step 3: Clean up the temporary file
        (delete-file temp-file)

        (message "Master bibliography file has been recalculated and sorted."))
    (file-error
     (message "Error: Unable to recalculate master bibliography. %s" (error-message-string err)))))

(defun sync0-bibtex-visit-bibliography ()
  (interactive)
  (let ((bib-file
         (completing-read "Fichier Biblatex : " sync0-bibtex-bibliographies)))
    (find-file
     (expand-file-name bib-file))))

(provide 'sync0-bibtex-var-functions)
