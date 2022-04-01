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

;; (defhydra sync0-hydra-ivy-bibtex-functions (:color blue :hint nil)
;;   "
;; ^BibLaTeX^           ^Etc.^    
;; ^--------------------------------------
;; Open _n_otes         Open _d_eft
;; _E_xtract field      _Q_uote (display)     
;; PDF _o_pen           _F_oreign quote       
;; PDF in _z_athura
;; _V_isit corr. PDF
;; _C_opy pdf
;; _B_ib files
;; Open _i_vy bibtex

;; _q_uit
;; "
;;   ("B" sync0-visit-bibliography-in-buffer)
;;   ("C" sync0-org-ref-copy-pdf-to-path)
;;   ("d" deft)
;;   ("i" ivy-bibtex)
;;   ("n" sync0-ivy-bibtex-open-notes)
;;   ("E" sync0-ivy-bibtex-extractor)
;;   ("o" sync0-org-ref-open-pdf-at-point)
;;   ("F" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_foreign_displayquote"))))
;;   ("Q" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_displayquote"))))
;;   ("V" sync0-org-open-corresponding-pdf)
;;   ("z" sync0-org-ref-open-pdf-at-point-zathura)
;;   ("q" nil :color blue))

;; (evil-leader/set-key
;;   "i" 'sync0-hydra-ivy-bibtex-functions/body)

;; This action is for ivy-bibtex/bibtex-completion
;; (defun bibtex-completion-print-pdf-list (keys)
;;   "Print the PDFs of the entries with the given KEYS where available."
;;   (let ((command (sync0-print-define-command)))
;;     (dolist (key keys)
;;       (let ((pdf (car (bibtex-completion-find-pdf key))))
;;         (if pdf
;;             (sync0-bibtex-print-pdf pdf command)
;;           (message "No PDF(s) found for this entry: %s" key))))))

(defun bibtex-completion-print-pdf-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((command (sync0-print-define-command)))
    (dolist (key keys)
      (when-let ((pdf (car (bibtex-completion-find-pdf key))))
        (sync0-bibtex-print-pdf pdf command)))))

(defun bibtex-completion-crop-pdf-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (when-let*    ((bibkey (sync0-bibtex-completion-choose-key))
                 (entry (bibtex-completion-get-entry bibkey))
                 (sample (bibtex-completion-get-value "file" entry))
                 (cropbox (sync0-pdf-define-cropbox sample)))
    (dolist (key keys)
      (when-let ((pdf (car (bibtex-completion-find-pdf key))))
        (sync0-bibtex-crop-pdf pdf cropbox)))))

(defun bibtex-completion-copy-pdf-to-path-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((path (read-string "Où envoyer ce pdf ? (finir en /) ")))
    (dolist (key keys)
      (when-let ((pdf (car (bibtex-completion-find-pdf key))))
        (sync0-bibtex-copy-pdf-to-path path key)))))

(defun bibtex-completion-rewrite-notes-from-biblatex-data-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (dolist (key keys)
    (when (file-exists-p (concat sync0-obsidian-directory key ".md"))
      (sync0-bibtex-create-note-from-entry t key t)))) 

(defun bibtex-completion-archive-entries-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (dolist (key keys)
    (when (string-match ".+\\.bib"   (buffer-file-name))
      (sync0-bibtex-archive-entry key))))

(defun bibtex-completion-add-key-to-pdf-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
    (dolist (key keys)
        (sync0-bibtex-add-key-to-pdf key)))

;; Before being able to call custom functions from ivy-bibtex, these
;; have to be manually added to ivy-bibtex. 

(ivy-bibtex-ivify-action bibtex-completion-print-pdf-list ivy-bibtex-print-pdf-list)

(ivy-bibtex-ivify-action bibtex-completion-crop-pdf-list ivy-bibtex-crop-pdf-list)

(ivy-bibtex-ivify-action bibtex-completion-copy-pdf-to-path-list ivy-bibtex-copy-pdf-to-path-list)

(ivy-bibtex-ivify-action bibtex-completion-rewrite-notes-from-biblatex-data-list ivy-bibtex-rewrite-notes-from-biblatex-data-list)

(ivy-bibtex-ivify-action bibtex-completion-archive-entries-list ivy-bibtex-archive-entries-list)

(ivy-bibtex-ivify-action bibtex-completion-add-key-to-pdf-list ivy-bibtex-add-key-to-pdf-list)

;; This is the way to add actions to ivy-bibtex wituhout overwriting
;; those already defined.
(ivy-add-actions
 'ivy-bibtex
 '(("z" ivy-bibtex-print-pdf-list "Print attachments with default printer" ivy-bibtex-print-pdf-list)
   ("Z" ivy-bibtex-copy-pdf-to-path-list "Copy attached pdf to target path" ivy-bibtex-copy-pdf-to-path-list)
   ("Y" ivy-bibtex-rewrite-notes-from-biblatex-data-list "Rewrite note metadata from Biblatex entry" ivy-bibtex-rewrite-notes-from-biblatex-data-list)
   ("w" ivy-bibtex-archive-entries-list "Archive Biblatex entries" ivy-bibtex-archive-entries-list)
   ("K" ivy-bibtex-add-key-to-pdf-list "Add bibkeys to pdfs" ivy-bibtex-add-key-to-pdf-list)
   ("x" ivy-bibtex-crop-pdf-list "Crop attachments using model cropbox" ivy-bibtex-crop-pdf-list)))

(defun sync0-ivy-bibtex-with-local-bibliography ()
  ""
  (interactive)
  (let ((current-buffer (buffer-file-name)))
    (if (string-match ".+\\.bib" current-buffer)
        (ivy-bibtex-with-local-bibliography)
      (let* ((files (f-files "/home/sync0/Dropbox/bibliographies/" (lambda (x) (string-match ".+\\.bib" x))))
             (selection (completing-read "Choose bibliography file to navigate: " files)))
        (find-file selection)
        (ivy-bibtex-with-local-bibliography)))))

  (evil-leader/set-key "V" 'sync0-ivy-bibtex-with-local-bibliography)

(provide 'sync0-ivy-bibtex-functions)
