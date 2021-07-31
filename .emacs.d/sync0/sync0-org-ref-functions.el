
;; (defun sync0-org-ref-search-bibkey-in-buffer (buffer)
;;   "Check the existence of a bibtex in current buffer."
;; (interactive)
;; ;; (equal major-mode 'org-mode)
;; (let ((regex "cite:\\([[:digit:]]+\\)"))
;; (cond ((and (equal major-mode 'org-mode)
;;     (search (thing-at-point t) regex))
;; (equal major-mode 'org-mode)
;;     (org-with-point-at 1 
;;       (re-search-forward "cite:\\([[:digit:]]+\\)" nil t 1)
;;       (match-string-no-properties 1))

(defun sync0-org-ref-search-bibkey-in-buffer ()
  "Check the existence of a bibtex in current buffer."
  (interactive)
  (let ((regex-one "cite:\\([[:digit:]]+\\)")
        (regex-two "@book{\\([[:digit:]]+\\),"))
    (cond ((equal major-mode 'org-mode)
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

(defun sync0-org-ref-get-citation-author (key)
  "Get the year of an entry with KEY.  Return year as a string."
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field "author" (bibtex-parse-entry t))))))

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
         (bibfile (cdr results)))
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
        (setq sync0-bibtex-current-title
              (reftex-get-bib-field "title" fields))
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
      (org-set-property "DATE" (concat "\"" sync0-bibtex-current-date "\""))
      (org-set-property "ORIG_DATE" (concat "\"" sync0-bibtex-current-origdate "\""))
      (org-set-property "LANGUAGE" sync0-bibtex-current-language)
       (when  (re-search-forward "^#\\+TITLE:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+TITLE: " sync0-bibtex-current-title "\n")))
       (when  (re-search-forward "^#\\+SUBTITLE:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+SUBTITLE: " sync0-bibtex-current-subtitle "\n")))
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
         (pdf-file (car (bibtex-completion-find-pdf bibkey))))
    (if (file-exists-p pdf-file)
        (call-process "zathura" nil 0 nil pdf-file)
      (message "No PDF found for %s" key))))

(defun sync0-org-ref-copy-pdf-to-path ()
  "Open the pdf for bibtex key under point if it exists."
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
                  (cdr (assoc "=key=" (cdr (assoc selection candidates)))))))
    (funcall org-ref-notes-function bibkey)))

;; (defhydra sync0-hydra-research-functions (:color amaranth :hint nil :exit t)
;; "
;; ^Bibtex functions^   ^References^          ^Roam^                ^Etc^
;; ^------------------------------------------------------------------------------
;; Orb _i_nsert           _I_nsert footnote   Find _f_ile        Replace smart quotes
;; Orb _a_ctions          _Q_uote (Csquotes)  Open _r_oam buffer 
;; Entry _n_otes          _F_oreign quote     Open _d_eft       
;; Bibtex _e_ntry         Insert _c_itation   _B_uild cache    
;; Open _b_ibliography    _E_xtract field     Show _g_raph   
;; Open _p_df             _U_pdate notes file _S_et property
;; Open in _z_athura      ^ ^                 
;; _C_opy pdf to location

;; _q_uit
;; "
;;   ("s" org-store-link)
;;   ("i" orb-insert)
;;   ("a" orb-note-actions)
;;   ("d" deft)
;;   ("r" org-roam-buffer-toggle)
;;   ("Q" replace-smart-quotes)
;;   ("B" org-roam-db-sync)
;;   ("f" org-roam-node-find)
;;   ("g" org-roam-graph)
;;   ("l" org-roam-node-insert)
;;   ("c" org-ref-ivy-insert-cite-link)
;;   ("C" sync0-org-ref-copy-pdf-to-path)
;;   ("S" sync0-zettelkasten-set-property)
;;   ("n" sync0-org-ref-open-notes)
;;   ("U" sync0-org-ref-update-notes-file)
;;   ("e" org-ref-open-citation-at-point)
;;   ("E" sync0-ivy-bibtex-extractor)
;;   ("b" sync0-visit-bibliography-in-buffer)
;;   ("p" sync0-org-ref-open-pdf-at-point)
;;   ("z" sync0-org-ref-open-pdf-at-point-zathura)
;;   ("I" org-footnote-new)
;;   ("Q" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_displayquote"))))
;;   ("F" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_foreign_displayquote"))))
;;   ("q" nil :color blue))

;; (evil-leader/set-key
;;   "C" 'org-ref-ivy-insert-cite-link
;;   "R" 'sync0-hydra-research-functions/body)

(evil-leader/set-key
  "C" 'org-ref-ivy-insert-cite-link)

(provide 'sync0-org-ref-functions)
