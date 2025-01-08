(require 'sync0-functions)
(require 'sync0-bibtex-vars)
(require 'bibtex-utils)
(require 'sync0-citar)
(require 'sync0-bibtex-extraction)
(require 'sync0-bibtex-actions)
(require 'sync0-bibtex-create-functions)
(require 'sync0-bibtex-corrections)
(require 'sync0-bibtex-python)
(require 'sync0-bibtex-sql)

(defun sync0-bibtex-format-citation ()
  "Insert a cached or new markdown citation at point, querying the database."
  (let ((cached-citation sync0-bibtex-cache-citation))
    (if (and cached-citation
             (y-or-n-p (format "Use cached citation: %s? " cached-citation)))
        ;; Use the cached citation directly, with optional edit
        (let ((insert-cit (read-string "Edit citation (if needed): " cached-citation)))
          (setq sync0-bibtex-cache-citation insert-cit))
      ;; Else, query the database and allow selection
      (let ((key (sync0-bibtex-choose-key))
      ;; (key (sync0-bibtex-db-query-entry))
            (pages (read-string "Pages: " sync0-bibtex-cache-pages)))
        (setq sync0-bibtex-cache-key key)
        (setq sync0-bibtex-cache-pages pages)
        (setq sync0-bibtex-cache-citation
              (format "@%s, %s" key pages))))))

(defun sync0-bibtex-insert-citation ()
  "Insert a cached or new markdown citation at point, querying the database."
  (interactive)
  (let ((citation (sync0-bibtex-format-citation)))
    (if (derived-mode-p 'org-mode)
	(insert (concat " [cite:" citation "]"))
      (insert (concat " [" citation "]")))))

(defun sync0-bibtex-clipboard-to-org-quote-block ()
  "Convert clipboard content into an Org-mode quote block with unified lines."
  (interactive)
  (let* ((clipboard-content (current-kill 0))
         (formatted-content (when clipboard-content
			      (with-temp-buffer
				(insert clipboard-content)
				(let ((fill-column (point-max)))
				  (fill-region (point-min) (point-max)))
				(buffer-string))))
	 (citation (sync0-bibtex-format-citation)))
    (if (and clipboard-content
	     (yes-or-no-p "Format clipboard contents as org quote block? "))
	(insert (format "#+BEGIN_QUOTE\n%s [cite:%s].\n#+END_QUOTE\n" formatted-content citation))
      (insert (concat " [cite:" citation "]")))))

;; (defun sync0-bibtex-insert-bibnote-link ()
;;   "Insert a link to the bibnotes of a given bibliographic entry."
;;   (interactive)
;;   (let* ((key (sync0-bibtex-choose-key))
;; 	 (formatted-citation (sync0-bibtex-corrections-format-insert-citation key))
;;          (link (if (derived-mode-p 'org-mode)
;; 	  (format "[[id:%s][%s]]" key formatted-citation)
;; 	  (format "[%s](%s.md)" formatted-citation key))))
;;     (insert link)))

(defun sync0-bibtex-insert-bibnote-link ()
  "Insert a link to the bibnotes of a given bibliographic entry.
By default, uses `sync0-bibtex-default-citation-style`. If the user
opts out, allows selection of a different style."
  (interactive)
  (let* ((key (sync0-bibtex-choose-key))
         (style (if (yes-or-no-p
                     (format "Use default citation style '%s'? " sync0-bibtex-citation-style))
                    sync0-bibtex-citation-style
                  (completing-read "Choose citation style: "
                                   '("author-title" "title-date" "date-title" "author-date-title")
                                   nil t nil nil sync0-bibtex-citation-style)))
         (formatted-citation (sync0-bibtex-corrections-format-insert-citation key style))
         (link (if (derived-mode-p 'org-mode)
                   (format "[[id:%s][%s]]" key formatted-citation)
                 (format "[%s](%s.md)" formatted-citation key))))
    (insert link)))

(defun sync0-bibtex-open-attachment ()
  "Insert a markdown link with a citation at point."
  (interactive)
  (let* ((bibkey (sync0-bibtex-choose-key))
         (file (sync0-bibtex-choose-attachment bibkey))
	 (extension (file-name-extension file))
	 (program (if (assoc extension sync0-default-file-associations)
                      (cdr (assoc extension sync0-default-file-associations))
		    (completing-read "Which software to open attachment with? " sync0-bibtex-attachment-programs))))
    (cond ((and (sync0-null-p program)
                (file-exists-p file))
             (org-open-file file))
            ((file-exists-p file)
             (call-process program nil 0 nil file))
            (t (message "No attachment found for key %s" bibkey)))))

(defun sync0-bibtex-open-notes ()
  "Insert a markdown link with a citation at point."
  (interactive)
  (let* ((bibkey (sync0-bibtex-choose-key))
         (file (concat sync0-zkn-references-dir bibkey ".org")))
    (if (file-exists-p file)
	(find-file file)
      (message "No bibnotes found for key %s" bibkey))))

(defun sync0-bibtex-completion-journaltitle ()
  (completing-read "Journal title : "
                   (delete-dups (mapcar #'(lambda (x) (cdr (assoc "journaltitle" x)))
                                        (bibtex-completion-candidates)))))

(defun sync0-bibtex-completion-author ()
  (completing-read "Auteur : "
                   (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                        (bibtex-completion-candidates)))))

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
  (when-let*    ((bibkey (sync0-bibtex-choose-key))
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
	 (path (read-directory-name "Where should I send these PDFs? : " sync0-goodreads-dir))
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
                       ;;            (read-string "Where to save ZIP file? : " sync0-goodreads-dir) "/" t))
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
	(when-let* ((bibkey processed-keys)
		    (pdf (car (bibtex-completion-find-pdf bibkey)))
                    (alias (progn 
                             (sync0-bibtex-completion-load-entry bibkey)
                             (concat "\"" (sync0-bibtex-corrections-format-yank-citation bibkey) "\"")))
                    ;; (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator summary-only))
                    (output (sync0-bibtex-python-search-pdf-define-shell-command venv script-path search-term pdf alias separator)))
          (message "%s" output)))))

(defun bibtex-completion-rewrite-notes-from-biblatex-data-list (keys)
  "Print the PDFs of the entries with the given KEYS where available."
  (let ((processed-keys (sync0-process-bibkeys keys)))
    (if (listp processed-keys)
	(dolist (key processed-keys)
	  (if (file-exists-p (concat sync0-zkn-references-dir key ".org"))
	      (sync0-bibtex-create-note-from-entry t key)
            (sync0-bibtex-create-note-from-entry nil key))) 
      (if (file-exists-p (concat sync0-zkn-references-dir processed-keys ".org"))
          (sync0-bibtex-create-note-from-entry t processed-keys)
        (sync0-bibtex-create-note-from-entry nil processed-keys)))))

;; (defun bibtex-completion-recalc-tags (keys)
;;   "Recalculate tags for entries with given KEYS."
;;   (let ((processed-keys (sync0-process-bibkeys keys)))
;;     (if (listp processed-keys)
;;         (dolist (key processed-keys)
;;           (sync0-bibtex-recalc-tags-and-mdnote key))
;;       (sync0-bibtex-recalc-tags-and-mdnote processed-keys))))

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
				  (directory-files sync0-bibtex-bibliobraphy-dir t ".+\\.bib") nil t)))
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
      (let ((processed-keys (sync0-process-bibkeys keys))
	    (video-only (yes-or-no-p "Download videos for selected key? (Otherwise, audio only)")))
	(if (listp processed-keys)
	    (dolist (key processed-keys)
	      (sync0-bibtex-download-from-youtube key video-only))
          (sync0-bibtex-download-from-youtube processed-keys video-only)))
    (when (sync0-bibtex-buffer-p)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry)))
             (title (cdr (assoc "title" entry)))
	     (media-type-message (format "Download video for %s (%s)? (Otherwise, audio only)" title bibkey))
             (video-only (yes-or-no-p media-type-message)))
      (sync0-bibtex-download-from-youtube bibkey video-only)))))

(defun bibtex-completion-concatenate-pdf-list (keys)
  "Concatenate pdfs corresponding to keys"
  (let* ((output (concat sync0-zkn-attachments-dir "temp.pdf"))
         (raw-command (concat "gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=" output " "))
         (pdf (if (yes-or-no-p "Choose key from bibliographies?")
                  (let ((bibref (sync0-bibtex-choose-key)))
                    (concat sync0-zkn-attachments-dir bibref ".pdf"))
                (read-file-name "Input full path with name of output pdf: ")))
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

;; (defun bibtex-completion-add-field-and-recalc-mdnote (keys)
;;   "Add biblatex field to an entry and recalc both tags and corresponding Md note."
;;   (let* ((processed-keys (sync0-process-bibkeys keys))
;; 	 (field (completing-read "Choose Bibtex field: " (remove "keywords" sync0-bibtex-fields)))
;;          (separator (cond ((member field sync0-bibtex-people-fields)
;;                            " and ")
;;                           ((string= field "file")
;;                            ";")
;;                           (t ", ")))
;;          (unique-p (member field sync0-bibtex-unique-fields))
;;          (delay-calc (string= field "file"))
;;          (assigned-value (unless delay-calc
;;                            (progn (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
;;                                   (eval (intern (concat "sync0-bibtex-entry-" field))))))
;;          (assigned-values (when (and assigned-value
;;                                      (null unique-p))
;;                             (split-string assigned-value separator)))
;;          (multiple-new-p (when assigned-values
;;                            (> (length assigned-values) 1))))
;;     (if (listp processed-keys)
;; 	;; Loop
;; 	(dolist (key processed-keys)
;; 	  (sync0-bibtex-add-field-and-recalc-keywords-and-mdnote key field unique-p multiple-new-p separator assigned-value assigned-values))
;;       (sync0-bibtex-add-field-and-recalc-keywords-and-mdnote processed-keys field unique-p multiple-new-p separator assigned-value assigned-values))))

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

(defun bibtex-completion-ocr-pdf-files (&optional keys)
  "OCR the PDF files referenced by a list of BibLaTeX KEYS by calling an external shell script.
If KEYS are provided, OCR the PDFs associated with those keys.
If no KEYS are provided, OCR the PDF linked to the current BibTeX entry.
Optionally pass LANGUAGE to specify the OCR language(s)."
  (interactive)
  (let ((command-base "bash /home/sync0/Scripts/shell/ocr_script.sh"))
    (if keys
        (let ((processed-keys (sync0-process-bibkeys keys)))
          (if (listp processed-keys)
              (dolist (key processed-keys)
                (let* ((pdf-file (sync0-bibtex-choose-attachment key ".pdf"))
                       (entry (bibtex-completion-get-entry key))
                       (languages (sync0-bibtex-ocr-language-chooser entry))
                       (command (format "%s %s %s" command-base (shell-quote-argument languages) (shell-quote-argument pdf-file))))
                  (if (and pdf-file (file-readable-p pdf-file))
                      (progn
                        (async-shell-command command)
                        (message "OCR produced for %s" key))
                    (message "No readable PDF found for key: %s" key))))
            (let* ((pdf-file (sync0-bibtex-choose-attachment processed-keys ".pdf"))
                   (entry (bibtex-completion-get-entry processed-keys))
                   (languages (sync0-bibtex-ocr-language-chooser entry))
                   (command (format "%s %s %s" command-base (shell-quote-argument languages) (shell-quote-argument pdf-file))))
              (if (and pdf-file (file-readable-p pdf-file))
                  (progn
                    (async-shell-command command)
                    (message "OCR produced for %s" processed-keys))
                (message "No readable PDF found for key: %s" processed-keys)))))
    ;; No keys provided, OCR the PDF from the current BibTeX entry
    (when (sync0-bibtex-buffer-p)
      (let* ((entry (save-excursion (bibtex-beginning-of-entry)
                                    (bibtex-parse-entry)))
             (bibkey (cdr (assoc "=key=" entry)))
             (entry (bibtex-completion-get-entry bibkey))
             (languages (sync0-bibtex-ocr-language-chooser entry))
             (pdf-file (sync0-bibtex-choose-attachment bibkey ".pdf"))
             (command (format "%s %s %s" command-base (shell-quote-argument languages) (shell-quote-argument pdf-file))))
        (if (and pdf-file (file-readable-p pdf-file))
            (progn
              (async-shell-command command)
              (message "OCR produced for %s" bibkey))
          (message "No readable PDF found for key: %s" bibkey)))))))

;; (defun sync0-consult-bibtex-with-local-bibliography (&optional arg)
;;   "Search BibTeX entries with local bibliography using consult.
;; With a prefix ARG the cache is invalidated and the bibliography reread."
;;   (interactive "P")
;;   (let* ((local-bib (bibtex-completion-find-local-bibliography))
;;          (bibtex-completion-bibliography
;;           (if local-bib
;;               (list local-bib)
;;             (let* ((files (f-files "/home/sync0/Gdrive/bibliographies/" (lambda (x) (string-match ".+\\.bib" x))))
;;                    (selection (consult--read
;;                               "Choose bibliography file: "
;;                               files
;;                               :require-match t)))
;;               (find-file selection)
;;               (list selection)))))
;;     (consult-bibtex arg bibtex-completion-bibliography)))

;; (defvar sync0-consult-bibtex-action-map
;;   '(
;;     ("Show entry" . bibtex-completion-show-entry)
;;     ("Yank citations" . bibtex-completion-yank-citations-from-bibkeys)
;;     ("Print PDF list" . bibtex-completion-print-pdf-list)
;;     ("Crop PDF list" . bibtex-completion-crop-pdf-list)
;;     ("Copy PDFs to path" . bibtex-completion-copy-pdf-to-path-list)
;;     ("Rewrite notes" . bibtex-completion-rewrite-notes-from-biblatex-data-list)
;;     ("Archive entries" . bibtex-completion-archive-entries-list)
;;     ("Move entries to bibfile" . bibtex-completion-move-entries-to-bibfile-list)
;;     ("Add key to PDFs" . bibtex-completion-add-key-to-pdf-list)
;;     ("Concatenate PDFs" . bibtex-completion-concatenate-pdf-list)
;;     ("Check file existence" . bibtex-completion-file-exists-p)
;;     ("Download PDF from URL" . bibtex-completion-download-pdf-from-url)
;;     ("Extract PDF from CrossRef" . bibtex-completion-extract-pdf-from-crossref)
;;     ;; ("Add field and recalc mdnote" . bibtex-completion-add-field-and-recalc-mdnote)
;;     ("Delete entry" . bibtex-completion-delete-entry)
;;     ("Open URL" . bibtex-completion-open-url)
;;     ("String keys with separator" . bibtex-completion-string-keys-with-sep)
;;     ("Open PDF externally" . bibtex-completion-open-pdf-external)
;;     ;; ("Recalculate tags" . bibtex-completion-recalc-tags)
;;     ("Convert PDF to text" . bibtex-convert-pdf-to-txt)
;;     ("Search PDF with Python" . bibtex-completion-search-pdf-with-python)
;;     ("Download from YouTube" . bibtex-completion-download-from-youtube)
;;     ("OCR PDF files" . bibtex-completion-ocr-pdf-files))
;;   "Mapping of action names to BibTeX-related commands.")


(defvar sync0-citar-action-map
  '(
;;     ("Show entry" . bibtex-completion-show-entry)
    ("Yank citations" . bibtex-completion-yank-citations-from-bibkeys)
    ("Print PDF list" . bibtex-completion-print-pdf-list)
    ("Crop PDF list" . bibtex-completion-crop-pdf-list)
    ("Copy PDFs to path" . bibtex-completion-copy-pdf-to-path-list)
;;     ("Rewrite notes" . bibtex-completion-rewrite-notes-from-biblatex-data-list)
    ("Archive entries" . bibtex-completion-archive-entries-list)
;;     ("Move entries to bibfile" . bibtex-completion-move-entries-to-bibfile-list)
;;     ("Add key to PDFs" . bibtex-completion-add-key-to-pdf-list)
;;     ("Concatenate PDFs" . bibtex-completion-concatenate-pdf-list)
;;     ("Check file existence" . bibtex-completion-file-exists-p)
    ("Download PDF from URL" . bibtex-completion-download-pdf-from-url)
    ("Extract PDF from CrossRef" . bibtex-completion-extract-pdf-from-crossref)
    ;; ("Add field and recalc mdnote" . bibtex-completion-add-field-and-recalc-mdnote)
    ("Delete entry" . bibtex-completion-delete-entry)
    ("Open URL" . bibtex-completion-open-url)
;;     ("String keys with separator" . bibtex-completion-string-keys-with-sep)
    ("Open PDF externally" . bibtex-completion-open-pdf-external)
    ("Typeset bibnotes (no multiple)" . sync0-bibtex-typeset-bibnotes)
    ;; ("Recalculate tags" . bibtex-completion-recalc-tags)
;;     ("Convert PDF to text" . bibtex-convert-pdf-to-txt)
    ("Search PDF with Python" . bibtex-completion-search-pdf-with-python)
    ("Download from YouTube" . bibtex-completion-download-from-youtube)
    ("OCR PDF files" . bibtex-completion-ocr-pdf-files))
  "Mapping of action names to BibTeX-related commands.")

(defun sync0-citar-perform-action (&optional keys)
  "Prompt user to choose an action from `sync0-citar-action-map` and then apply the action to the selected keys."
  (interactive)
  (let* ((chosen-keys (or keys (sync0-bibtex-choose-key)))  ; Choose keys if not provided
	 (action-names (mapcar 'car sync0-citar-action-map))
	 (chosen-action (completing-read "Choose action: " action-names nil t))
         (action-func (cdr (assoc chosen-action sync0-citar-action-map))))
          (if action-func
              (funcall action-func chosen-keys)  ; Call the selected action with the keys
            (message "No action found for %s" chosen-action))))

(provide 'sync0-ivy-bibtex-functions)
