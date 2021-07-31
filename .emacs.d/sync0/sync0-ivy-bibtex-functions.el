(defun sync0-update-bibtex-authors ()
  (interactive)
  (let ((current-authors)
        (final-list)
        (bibtex-authors  (append
                (mapcar #'(lambda (x) (cdr (assoc "editor" x)))
                          (bibtex-completion-candidates))
                (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                          (bibtex-completion-candidates)))))
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


(defun sync0-bibtex-completion-reverse-author (key)
  "Reverse the author used for "
  (let* ((string-key (number-to-string key))
         (author (sync0-org-ref-get-citation-author string-key)))
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
             (concat first-name " " last-name))))))

(defun sync0-pdf-page-extractor ()
  "Extract as a separate pdf the pages within the page rage
  specified by beg and end"
  (interactive)
  (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
                        (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
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
         (input-file (car (bibtex-completion-find-pdf bibkey)))
         (beg (read-string "Première page : "))
         (end (read-string "Dernière page : "))
         (filename
          (if (yes-or-no-p "Derive pdf file name from existing BibTeX entry?")
              (let* ((candidates (bibtex-completion-candidates))
                     (selection (ivy-read "Crossref : "
                                          candidates
                                          :caller 'ivy-bibtex
                                          :history 'ivy-bibtex-history)))
                (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
            (concat bibkey "_" beg "-" end)))
         (command (concat
                   "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="
                   beg
                   " -dLastPage="
                   end
                   " -sOutputFile="
                   sync0-pdfs-folder
                   filename
                   ".pdf "
                   input-file)))
    (if (file-exists-p input-file)
        (shell-command command)
      (message "No PDF found for %s" bibkey))))

(defun sync0-bibtex-completion-journaltitle ()
  (completing-read "Journal title : "
                   (delete-dups (mapcar #'(lambda (x) (cdr (assoc "journaltitle" x)))
                                        (bibtex-completion-candidates)))))

(defun sync0-bibtex-completion-author ()
  (completing-read "Auteur : "
                   (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                        (bibtex-completion-candidates)))))

(defun sync0-ivy-bibtex-extractor ()
  "Extract the target field from BibTeX entry"
  (interactive)
  (let* ((bibkey (let* ((candidates (bibtex-completion-candidates))
                        (key-at-point (sync0-org-ref-search-bibkey-in-buffer))
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
         (entry (bibtex-completion-get-entry bibkey))
         (bibtex-field
          (completing-read "Choose one: "
                           '("=key=" "title" "author" "journaltitle" "date" "editor" "booktitle"))))
    (insert (bibtex-completion-get-value bibtex-field entry))))

(provide 'sync0-ivy-bibtex-functions)
