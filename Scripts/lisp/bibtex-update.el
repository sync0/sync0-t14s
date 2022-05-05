;; 
;;
;;   Man should not be ready to show that he can live like a
;;   badly-fed animal. He should decline to live like that, and
;;   should either steal or go on the rates, which is considered by
;;   many to be a form of stealing. As for begging, it is safer to
;;   beg than to take, but it is finer to take than to beg. No: a
;;   poor man who is ungrateful, unthrifty, discontented, and
;;   rebellious, is probably a real personality, and has much in him.
;;   He is at any rate a healthy protest. As for the virtuous poor,
;;   one can pity them, of course, but one cannot possibly admire
;;   them. They have made private terms with the enemy, and sold
;;   their birthright for very bad pottage.
;;
;;   Oscar Wilde
;;   The Soul of Man under Socialism (1891)
;; 
    (sync0-update-list "bloggger" sync0-zettelkasten-projects "projects")



create a function taht updates the names of th authors in the bibtex file and in the so that they are both syncronized


    (sync0-update-list nil sync0-bibtex-locations "bibtex-locations")



(defun sync0-insert-bibtex-field (field file)
  (interactive)
  (let ((bibtex (delete-dups (mapcar #'(lambda (x) (cdr (assoc field x)))
                          (bibtex-completion-candidates)))))
    (with-temp-file
        (concat "~/.emacs.d/sync0-vars/" file ".txt")
      (sync0-insert-elements-of-list bibtex))))


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


         (completing-read "Auteur : " sync0-bibtex-authors)
         

