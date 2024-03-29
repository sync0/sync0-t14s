(require 'sync0-bibtex-extraction)
(require 'sync0-functions)
(require 'sync0-bibtex-python)

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


(defun bibtex-completion-print-pdf-list (&optional keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (interactive)
  (let ((command (sync0-print-define-command))
        (processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
        ;; Loop
        (dolist (bibkey processed-keys)
          (when-let ((pdf (sync0-bibtex-choose-attachment bibkey "pdf")))
            (sync0-bibtex-print-pdf pdf command)))
      (if (sync0-bibtex-buffer-p)
          (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			                (bibtex-parse-entry)))
                 (bibkey (cdr (assoc "=key=" entry)))
                 (pdf (sync0-bibtex-choose-attachment bibkey "pdf")))
            ;; Error control
            ;; (message "%s" command)
            (sync0-bibtex-print-pdf pdf command))
        (let ((pdf (sync0-bibtex-choose-attachment processed-keys "pdf")))
          (sync0-bibtex-print-pdf pdf command))))))

(defun bibtex-completion-crop-pdf-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (when-let*    ((bibkey (sync0-bibtex-completion-choose-key))
                 (entry (bibtex-completion-get-entry bibkey))
                 (sample (bibtex-completion-get-value "file" entry))
                 (cropbox (sync0-pdf-define-cropbox sample)))
    (dolist (key keys)
      (when-let ((pdf (car (bibtex-completion-find-pdf key))))
        (sync0-bibtex-crop-pdf pdf cropbox)))))

;; This function depends on the shell command trash-cli that can be found in YAY or in Github
;; https://github.com/andreafrancia/trash-cli

(defun bibtex-completion-copy-pdf-to-path-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let* ((processed-keys (sync0-process-bibkeys keys))
	 (path (sync0-correct-string-with-corrector
		(read-string "Where should I send these PDFs? : " sync0-goodreads-directory) "/" t))
         (compress
	  (when (listp processed-keys)
	    (yes-or-no-p "Compress selected files as one ZIP? ")))
        (paths))
    (if (file-accessible-directory-p path)
	(if (listp processed-keys)
            (progn 
              (dolist (key processed-keys paths)
		(when-let ((pdf (car (bibtex-completion-find-pdf key))))
		  (sync0-bibtex-copy-pdf-to-path path key)
		  (push sync0-bibtex-temp-pdf-copy-new-path-and-filename paths)))
	      (unless compress
		(message "PDFs succesfully copied to path %s" path))
              (when compress
		(let* ((zip-path path)
                       ;; (zip-path (sync0-correct-string-with-corrector
                       ;;            (read-string "Where to save ZIP file? : " sync0-goodreads-directory) "/" t))
                       (zip-name (read-string "Name of ZIP file? : " (format-time-string "%Y-%m-%d-%H-%M")))
                       ;; junk paths (-j option) is necessary to prevent the creation of unecessary folders in the created Zip file
                       (command (concat "zip --junk-paths " zip-path zip-name ".zip -@ < " sync0-bibtex-helper-pdf-copy-filepath)))
		  (sync0-erase-file-contents sync0-bibtex-helper-pdf-copy-filepath (sync0-show-elements-of-list paths "\n"))
		  (shell-command command)
		  ;; avoid ; too dangerous and unreliable
		  ;; (shell-command (concat "xargs rm < " sync0-bibtex-helper-pdf-copy-filepath))
		  ;; Prevent problem with spaces in path : need to use double quotes around the path to prevent this;
		  ;; however, remember that this quotes interfere with the zip command, thus they should only
		  ;; be used to prevent problems with the trash-put command
		  (mapc #'(lambda  (x) (shell-command (concat "trash-put \"" x "\"")))  paths)
		  (message "ZIP file %s with PDFs copied in path %s" zip-name zip-path))))
	  (progn 
	    (sync0-bibtex-copy-pdf-to-path path processed-keys)
	    (message "PDF for %s succesfully copied to path %s" processed-keys path)))
      (message "Path/directory %s is unaccesable or does not exist." path))))

;; (defun sync0-bibtex-python-search-pdf-define-shell-command (venv script-path search-term pdf alias separator &optional summary-only)
;;   "Run shell commmand of python search for keyword in PDF and output as a string the product."
;;   (let ((command (if summary-only 
;;                 (format "source %s && python3 %s %s %s %s --summary_only" venv script-path search-term separator pdf alias)
;;                (format "source %s && python3 %s %s %s %s" venv script-path search-term pdf alias))))
;;     (shell-command-to-string command)))

(defun sync0-bibtex-python-search-pdf-define-shell-command (venv script-path search-term pdf alias separator)
  "Run shell commmand of python search for keyword in PDF and output as a string the product."
  (let ((command (format "source %s && python3 %s %s %s %s %s" venv script-path search-term separator pdf alias)))
    (shell-command-to-string command)))

(defun bibtex-completion-search-pdf-with-python (&optional keys)
  "Yank a PDF file based on the specified type and languages."
  (interactive)
  (let* ((processed-keys (sync0-process-bibkeys keys))
	 (script-path sync0-bibtex-python-pdf-searcher-path)
         (venv sync0-bibtex-python-pdf-searcher-venv-path)
         (search-term (read-string "String or regex to search: "))
         ;; (separator (cond ((string-match "[[:graph:]]+;[[:space:]]*" search-term)
         ;;                ";")
         ;;                  ((string-match "[[:graph:]]+,[[:space:]]*" search-term)
         ;;                   ",")
         ;;            (t "nil")))
         ;; (separator (read-string "Separator: " ","))
         ;; (summary-only (yes-or-no-p "Summary only? "))
         ;; (summary-only "t")
         ;; (separators (list "," ";" "|" "nil"))
         ;; (separator (if (string-match "[[:graph:]]+,[[:space:]]*" search-term)
         ;;                ","
         ;;              (completing-read "Separator" separators nil t "nil")))
         (separator (if (string-match "[[:graph:]]+,[[:space:]]*[[:graph:]]+" search-term)
			","
                      "nil")))
    (if (listp processed-keys)
        ;; Loop
        (let (x)
          (dolist (key processed-keys x)
            (when-let* ((pdf (car (bibtex-completion-find-pdf key)))
                        (alias (progn 
                                  (sync0-bibtex-completion-load-entry key)
                                  (concat "\"" (sync0-bibtex-corrections-format-yank-citation key) "\"")))
                        ;; (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator summary-only))
                        (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator)))
             (setq x (concat output "\n--------------------------------\n" x))))
          (message "%s" x))
      (if (sync0-bibtex-buffer-p)
	  (when-let* ((entry (save-excursion (bibtex-beginning-of-entry)
					     (bibtex-parse-entry)))
		      (bibkey (cdr (assoc "=key=" entry)))
		      (pdf (car (bibtex-completion-find-pdf bibkey)))
		      (alias (progn 
			       (sync0-bibtex-completion-load-entry bibkey)
			       (concat "\"" (sync0-bibtex-corrections-format-yank-citation bibkey) "\"")))
		      ;; (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator summary-only))
		      (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator)))
            (message "%s" output))
	(when-let* ((bibkey processed-keys)
		    (pdf (car (bibtex-completion-find-pdf bibkey)))
                    (alias (progn 
                             (sync0-bibtex-completion-load-entry bibkey)
                             (concat "\"" (sync0-bibtex-corrections-format-yank-citation bibkey) "\"")))
                    ;; (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator summary-only))
                    (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator)))
          (message "%s" output))))))

(defun bibtex-completion-rewrite-notes-from-biblatex-data-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (if (file-exists-p (concat sync0-zettelkasten-references-directory key ".md"))
	      (sync0-bibtex-create-note-from-entry t key)
            (sync0-bibtex-create-note-from-entry nil key))) 
      (if (file-exists-p (concat sync0-zettelkasten-references-directory processed-keys ".md"))
          (sync0-bibtex-create-note-from-entry t processed-keys)
        (sync0-bibtex-create-note-from-entry nil processed-keys)))))

;; (defun sync0-process-bibkeys (keys)
;;   "Process the KEYS argument into a list of strings."
;;   (cond
;;     ;; Case 1: List of strings
;;     ((listp keys)
;;      (mapcar #'symbol-name keys))
;;     ;; Case 2: List with a single string element
;;     ((stringp keys)
;;      (list keys))
;;     ;; Case 3: String
;;     (t
;;      (list keys))))

;; (defun bibtex-completion-recalc-tags (keys)
;;   "Recalculate tags for entries with given KEYS."
;;   (let ((processed-keys (sync0-process-bibkeys keys)))
;;     (dolist (key processed-keys)
;;       (sync0-bibtex-recalc-tags-and-mdnote key))))

(defun bibtex-completion-recalc-tags (keys)
  "Recalculate tags for entries with given KEYS."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
        (dolist (key processed-keys)
          (sync0-bibtex-recalc-tags-and-mdnote key))
      (sync0-bibtex-recalc-tags-and-mdnote processed-keys))))

;; (defun bibtex-completion-recalc-tags (keys)
;;   "Print the PDFs of the entries with the given KEYS where available."
;;   (when keys
;;       (if (cdr keys)
;; 	  (dolist (key keys)
;; 	    (sync0-bibtex-recalc-tags-and-mdnote key))
;;         (sync0-bibtex-recalc-tags-and-mdnote (car keys)))))

(defun bibtex-completion-archive-entries-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (when (string-match ".+\\.bib"   (buffer-file-name))
	    (sync0-bibtex-archive-entry key)))
      (sync0-bibtex-archive-entry processed-keys))))

(defun bibtex-completion-move-entries-to-bibfile-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys))
	(bibfile (completing-read "Which bibliography file to send to ? "
				  (directory-files sync0-bibtex-bibliobraphy-directory t ".+\\.bib") nil t)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (sync0-bibtex-move-entry-to-bibfile key bibfile))
      (sync0-bibtex-move-entry-to-bibfile processed-keys bibfile))))

(defun bibtex-completion-add-key-to-pdf-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (sync0-bibtex-add-key-to-pdf key))
      (sync0-bibtex-add-key-to-pdf processed-keys))))

(defun bibtex-completion-open-url (&optional keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (interactive)
  (if keys
      (let ((processed-keys (sync0-process-bibkeys keys)))
	(if (listp processed-keys)
	    (dolist (key processed-keys)
	      (sync0-bibtex-open-url key))
	  (sync0-bibtex-open-url processed-keys)))
    (when (sync0-bibtex-buffer-p)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry))))
      (sync0-bibtex-open-url bibkey)))))

(defun bibtex-completion-download-from-youtube (&optional keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (interactive)
  (if keys
      (let ((processed-keys (sync0-process-bibkeys keys)))
	(if (listp processed-keys)
	    (dolist (key processed-keys)
	      (sync0-bibtex-download-from-youtube key))
          (sync0-bibtex-download-from-youtube processed-keys)))
    (when (sync0-bibtex-buffer-p)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry))))
      (sync0-bibtex-download-from-youtube bibkey)))))

(defun bibtex-completion-concatenate-pdf-list (keys)
  "Concatenate pdfs corresponding to keys"
  (let* ((output (concat sync0-zettelkasten-attachments-directory "temp.pdf"))
         (raw-command (concat "gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=" output " "))
         (pdf (if (yes-or-no-p "Choose key from bibliographies?")
                  (let ((bibref (sync0-bibtex-completion-choose-key t)))
                    (concat sync0-zettelkasten-attachments-directory bibref ".pdf"))
                (read-string "Input full path with name of output pdf: ")))
         x
         command)
    (dolist (key keys)
      (let  ((file (sync0-bibtex-choose-attachment key "pdf")))
        (push file x)))
    (if (null x)
        (message "One or more bibkeys have no attached pdfs.")
      (progn 
        (shell-command (concat raw-command (sync0-show-elements-of-list x " ")))
        (rename-file output pdf t)
            (message "Concatenated pdf has been created at " pdf)))))

(defun bibtex-completion-file-exists-p (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((extension (completing-read "Choose extension to check: " bibtex-completion-pdf-extension))
	(processed-keys (sync0-process-bibkeys keys))
        messages)
    (if (listp processed-keys)
	(progn (dolist (key processed-keys messages)
		 (let ((my-message
			(sync0-bibtex-file-exists-p key extension t)))
		   (push my-message messages)))
	       (message-or-box (sync0-show-elements-of-list messages "\n")))
      (sync0-bibtex-file-exists-p processed-keys extension t))))

(defun bibtex-completion-download-pdf-from-url (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (sync0-bibtex-download-pdf key))
      (sync0-bibtex-download-pdf processed-keys))))

(defun bibtex-completion-extract-pdf-from-crossref (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (sync0-bibtex-extract-from-crossref key t))
      (sync0-bibtex-extract-from-crossref processed-keys t))))

  (defun bibtex-completion-delete-entry (keys)
    "Print the PDFs of the entries with the given KEYS where available."
    (let ((processed-keys (sync0-process-bibkeys keys)))
      (if (listp processed-keys)
	  (dolist (key processed-keys)
            (sync0-bibtex-delete-entry key)))
      (sync0-bibtex-delete-entry processed-keys)))

  (defun bibtex-completion-open-pdf-external (keys)
    "Print the PDFs of the entries with the given KEYS where available."
    (let ((processed-keys (sync0-process-bibkeys keys)))
      (if (listp processed-keys)
	  (dolist (key processed-keys)
            (sync0-bibtex-open-pdf key))
        (sync0-bibtex-open-pdf processed-keys))))

;; (defun bibtex-completion-string-keys-with-sep (keys)
;;   "Produce strin of concatenated KEYS with separator."
;;   (let ((separator ", ")
;;         (x ""))
;;     (dolist (key keys x)
;;       (setq x (concat key separator x)))
;;     (string-trim-right x ", ")))

;; (defun bibtex-completion-string-keys-with-sep (keys)
;;   "Produce strin of concatenated KEYS with separator."
;;   (if (> (length keys) 1)
;;       (sync0-show-elements-of-list keys ", ")
;;     keys))

;; (defun bibtex-completion-string-keys-with-sep (keys)
;;   "Produce strin of concatenated KEYS with separator."
;;   (let ((separator ", ")
;;         (x ""))
;;     (dolist (key keys x)
;;       (setq x (concat key separator x)))
;;     x))

(defun bibtex-completion-add-field-and-recalc-mdnote (keys)
  "Add biblatex field to an entry and recalc both tags and corresponding Md note."
  (let* ((processed-keys (sync0-process-bibkeys keys))
	 (field (completing-read "Choose Bibtex field: " (remove "keywords" sync0-bibtex-fields)))
         (separator (cond ((member field sync0-bibtex-people-fields)
                           " and ")
                          ((string= field "file")
                           ";")
                          (t ", ")))
         (unique-p (member field sync0-bibtex-unique-fields))
         (delay-calc (string= field "file"))
         (assigned-value (unless delay-calc
                           (progn (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
                                  (eval (intern (concat "sync0-bibtex-entry-" field))))))
         (assigned-values (when (and assigned-value
                                     (null unique-p))
                            (split-string assigned-value separator)))
         (multiple-new-p (when assigned-values
                           (> (length assigned-values) 1))))
    (if (listp processed-keys)
	;; Loop
	(dolist (key processed-keys)
	  (sync0-bibtex-add-field-and-recalc-keywords-and-mdnote key field unique-p multiple-new-p separator assigned-value assigned-values))
      (sync0-bibtex-add-field-and-recalc-keywords-and-mdnote processed-keys field unique-p multiple-new-p separator assigned-value assigned-values))))

(defun bibtex-convert-pdf-to-txt (&optional keys)
  "Summarize a PDF file based on the specified type and languages."
  (interactive)
  (if keys
      ;; Loop
      (dolist (key keys)
        (let* ((pdf-file (sync0-bibtex-choose-attachment key ".pdf"))
               (command (format "pdftotext -layout -enc UTF-8 -nopgbrk %s" pdf-file)))
          (shell-command command)))
    (when (sync0-bibtex-buffer-p)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry)))
        (pdf-file (sync0-bibtex-choose-attachment bibkey ".pdf"))
        (command (format "pdftotext -layout -enc UTF-8 -nopgbrk %s" pdf-file)))
      (shell-command command)))))

(defun bibtex-completion-yank-citations-from-bibkeys (&optional keys)
  "Yank a PDF file based on the specified type and languages."
  (interactive)
  (if keys
      (let ((processed-keys (sync0-process-bibkeys keys))
	    x)
	(if (listp processed-keys)
	    (progn (dolist (key processed-keys x)
		     (when-let ((citation (sync0-bibtex-corrections-format-yank-citation key)))
		       (setq x (concat citation "\n" x))))
		   (kill-new x)
		   (message "%s copied to kill ring." x))
	  (when-let ((citation (sync0-bibtex-corrections-format-yank-citation processed-keys)))
	    (kill-new citation)
	    (message "%s copied to kill ring." citation))))
    (when (sync0-bibtex-buffer-p)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry))))
        (when-let ((citation (sync0-bibtex-corrections-format-yank-citation bibkey)))
          (kill-new citation)
          (message "%s copied to kill ring." citation))))))

;; Before being able to call custom functions from ivy-bibtex, these
;; have to be manually added to ivy-bibtex. 

(ivy-bibtex-ivify-action bibtex-completion-yank-citations-from-bibkeys ivy-bibtex-yank-citations-from-bibkeys)

(ivy-bibtex-ivify-action bibtex-completion-print-pdf-list ivy-bibtex-print-pdf-list)

(ivy-bibtex-ivify-action bibtex-completion-crop-pdf-list ivy-bibtex-crop-pdf-list)

(ivy-bibtex-ivify-action bibtex-completion-copy-pdf-to-path-list ivy-bibtex-copy-pdf-to-path-list)

(ivy-bibtex-ivify-action bibtex-completion-rewrite-notes-from-biblatex-data-list ivy-bibtex-rewrite-notes-from-biblatex-data-list)

(ivy-bibtex-ivify-action bibtex-completion-archive-entries-list ivy-bibtex-archive-entries-list)

(ivy-bibtex-ivify-action bibtex-completion-move-entries-to-bibfile-list ivy-bibtex-move-entries-to-bibfile-list)

(ivy-bibtex-ivify-action bibtex-completion-add-key-to-pdf-list ivy-bibtex-add-key-to-pdf-list)

(ivy-bibtex-ivify-action bibtex-completion-concatenate-pdf-list ivy-bibtex-concatenate-pdf-list)

(ivy-bibtex-ivify-action bibtex-completion-file-exists-p ivy-bibtex-file-exists-p)

(ivy-bibtex-ivify-action bibtex-completion-download-pdf-from-url ivy-bibtex-download-pdf-from-url)

(ivy-bibtex-ivify-action bibtex-completion-extract-pdf-from-crossref ivy-bibtex-extract-pdf-from-crossref)

(ivy-bibtex-ivify-action bibtex-completion-add-field-and-recalc-mdnote ivy-bibtex-add-field-and-recalc-mdnote)

(ivy-bibtex-ivify-action bibtex-completion-delete-entry ivy-bibtex-delete-entry)

(ivy-bibtex-ivify-action bibtex-completion-open-url ivy-bibtex-open-url)

(ivy-bibtex-ivify-action bibtex-completion-string-keys-with-sep ivy-bibtex-string-keys-with-sep)

(ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)

(ivy-bibtex-ivify-action bibtex-completion-recalc-tags ivy-bibtex-recalc-tags)

(ivy-bibtex-ivify-action bibtex-completion-recalc-tags ivy-bibtex-recalc-tags)

(ivy-bibtex-ivify-action bibtex-convert-pdf-to-txt ivy-bibtex-convert-pdf-to-txt)

(ivy-bibtex-ivify-action bibtex-completion-search-pdf-with-python ivy-bibtex-search-pdf-with-python)

(ivy-bibtex-ivify-action bibtex-completion-download-from-youtube ivy-bibtex-download-from-youtube)

 ;; '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)" ivy-bibtex-open-pdf)
 ;;   ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser" ivy-bibtex-open-url-or-doi)
 ;;   ("c" ivy-bibtex-insert-citation "Insert citation" ivy-bibtex-insert-citation)
 ;;   ("r" ivy-bibtex-insert-reference "Insert reference" ivy-bibtex-insert-reference)
 ;;   ("k" ivy-bibtex-insert-key "Insert BibTeX key" ivy-bibtex-insert-key)
 ;;   ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry" ivy-bibtex-insert-bibtex)
 ;;   ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email" ivy-bibtex-add-PDF-attachment)
 ;;   ("e" ivy-bibtex-edit-notes "Edit notes" ivy-bibtex-edit-notes)
 ;;   ("s" ivy-bibtex-show-entry "Show entry" ivy-bibtex-show-entry)
 ;;   ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library" ivy-bibtex-add-pdf-to-library)
 ;;   ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options")))


;; This is the way to add actions to ivy-bibtex wituhout overwriting
;; those already defined.

(defvar sync0-ivy-bibtex-actions 
 '(("P" ivy-bibtex-print-pdf-list "Print PDF with default printer" ivy-bibtex-print-pdf-list)
   ("C" ivy-bibtex-copy-pdf-to-path-list "Copy PDF to target path" ivy-bibtex-copy-pdf-to-path-list)
   ("1" ivy-bibtex-convert-pdf-to-txt "Convert to TXT" ivy-bibtex-convert-pdf-to-txt)
   ("y" ivy-bibtex-yank-citations-from-bibkeys "Yank citations from keys" ivy-bibtex-yank-citations-from-bibkeys)
   ("Y" ivy-bibtex-rewrite-notes-from-biblatex-data-list "Rewrite mdnote metadata" ivy-bibtex-rewrite-notes-from-biblatex-data-list)
   ;; ("A" ivy-bibtex-archive-entries-list "Archive entry" ivy-bibtex-archive-entries-list)
   ;; ("p" ivy-bibtex-open-pdf-external "Open PDF" ivy-bibtex-open-pdf-external)
   ("p" ivy-bibtex-search-pdf-with-python "Search PDF (Python)" ivy-bibtex-search-pdf-with-python)
   ("w" ivy-bibtex-open-url "Open URL" ivy-bibtex-open-url)
   ("M" ivy-bibtex-move-entries-to-bibfile-list "Move entry to bibfile" ivy-bibtex-move-entries-to-bibfile-list)
   ;; ("S" ivy-bibtex-string-keys-with-sep "Produce string of keys" ivy-bibtex-string-keys-with-sep)
   ("K" ivy-bibtex-add-key-to-pdf-list "Mark bibkey onto PDF" ivy-bibtex-add-key-to-pdf-list)
   ("j" ivy-bibtex-concatenate-pdf-list "Concatenate PDFs" ivy-bibtex-concatenate-pdf-list)
   ("f" ivy-bibtex-file-exists-p "Check existence of attachment" ivy-bibtex-file-exists-p)
   ("d" ivy-bibtex-download-pdf-from-url "Download attachement from URL" ivy-bibtex-download-pdf-from-url)
   ("D" ivy-bibtex-delete-entry "Delete entry" ivy-bibtex-delete-entry)
   ("v" ivy-bibtex-download-from-youtube "Download Youtube video" ivy-bibtex-download-from-youtube)
   ("E" ivy-bibtex-extract-pdf-from-crossref "Extract PDF from crossref" ivy-bibtex-extract-pdf-from-crossref)
   ("a" ivy-bibtex-add-field-and-recalc-mdnote "Add field and recalc mdnote" ivy-bibtex-add-field-and-recalc-mdnote)
   ("T" ivy-bibtex-recalc-tags "Recalc tags and mdnote" ivy-bibtex-recalc-tags)
   ("x" ivy-bibtex-crop-pdf-list "Crop PDF with cropbox" ivy-bibtex-crop-pdf-list))
 "List of my custom ivy-bibtex actions")

(ivy-add-actions
 'ivy-bibtex
 sync0-ivy-bibtex-actions)

(defun sync0-ivy-bibtex-with-local-bibliography ()
  "Run ivy-bibtex with a local bibliography instead of global bib files"
  (interactive)
  (let ((current-buffer (buffer-file-name)))
    (if (string-match ".+\\.bib" current-buffer)
        (ivy-bibtex-with-local-bibliography)
      (let* ((files (f-files "/home/sync0/Gdrive/bibliographies/" (lambda (x) (string-match ".+\\.bib" x))))
             (selection (completing-read "Choose bibliography file to navigate: " files)))
        (find-file selection)
        (ivy-bibtex-with-local-bibliography)))))

  (evil-leader/set-key "V" 'sync0-ivy-bibtex-with-local-bibliography)

(provide 'sync0-ivy-bibtex-functions)
