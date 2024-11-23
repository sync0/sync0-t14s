
(defvar sync0-ivy-bibtex-cache nil
  "Cache variable for sync0-ivy-bibtex.")

(defvar sync0-bibtex-completion-cache-key nil
  "Cache variable to store the current BibTeX key for preselection.")

(defvar sync0-bibtex-completion-cache-pages nil
  "Cache variable to store the current BibTeX pages for preselection.")

(defvar sync0-bibtex-completion-cache-citation nil
  "Cache variable to store the last used BibTeX citation.")

(defun sync0-ivy-bibtex-update-cache ()
  "Force update sync0-ivy-bibtex-cache to reflect changes."
  (interactive)
  (bibtex-completion-init)
  (setq sync0-ivy-bibtex-cache (bibtex-completion-candidates)))

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

(provide 'sync0-ivy-bibtex)
