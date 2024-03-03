(require 'sync0-bibtex-entry-functions)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-corrections)
(require 'sync0-obsidian)
(require 'sync0-bibtex-obsidian)
(require 'sync0-yaml)

(defun sync0-bibtex-add-key-to-pdf (&optional bibkey)
  "Add bibkey to (only) the first page of the pdf on the top left
corner using cpdf. This function only works on pdfs. This
function is to be used only in pipes."
  (interactive)
  (when-let* ((refkey (or bibkey
                          (sync0-bibtex-completion-choose-key t t)))
              (file (sync0-bibtex-choose-attachment refkey))
              (message-function (lambda ()
                                  (message "No pdf found for entry %s." refkey)))
              (output
               (concat sync0-zettelkasten-attachments-directory refkey "temp.pdf"))
              (command
               (concat "cpdf -utf8 -add-text \"" refkey "\" -font \"Courier-Bold\" -topleft 20 -font-size 16 " file " 1 -o " output)))
    (if (and (file-exists-p file)
             (equal (file-name-extension file) "pdf")) 
        (progn
          (shell-command command)
          (if (file-exists-p output)
              (rename-file output file t)
            (message "cpdf failed to create the pdf; investigate the error.")))
      (funcall message-function))))

(defun sync0-bibtex-arrange-pdf (&optional bibkey)
  "Use PDF Arranger on target pdf."
  (interactive)
  (let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (file (sync0-bibtex-choose-attachment refkey))
         (extension (file-name-extension file))
         (command (concat "pdfarranger " file)))
    (if (and (file-exists-p file)
             (equal extension "pdf"))
        (shell-command command)
      (message "Problem opening file for entry %s failed." refkey))))

;; (defun sync0-bibtex-download-pdf (&optional refkey creation)
;;   (interactive)
;;   (if creation
;;       (let ((file (string-trim sync0-bibtex-entry-file ":" ":PDF"))
;;             (url sync0-bibtex-entry-url))
;;         (sync0-pdf-download-from-url url file))
;;     (let*    ((bibkey (or refkey 
;;                           (sync0-bibtex-completion-choose-key t t)))
;;               (entry (bibtex-completion-get-entry bibkey))
;;               (url (sync0-bibtex-completion-get-value "url" entry))
;;               (doi (sync0-bibtex-completion-get-value "doi" entry))
;;               (file (concat sync0-zettelkasten-attachments-directory bibkey ".pdf")))
;;       (if (yes-or-no-p "Call the pirates?")
;;           (if doi
;;               (scihub doi file)
;;             (scihub url file))
;;         (if (file-exists-p file)
;;             (message "Cannot download. Attachment exists for key %s" bibkey)
;;           (sync0-pdf-download-from-url url file))))))

(defun sync0-bibtex-download-pdf (&optional refkey creation)
  (interactive)
  (if creation
      (when-let* ((extension (when (string-match ":[[:upper:]]+$" sync0-bibtex-entry-file)
                                    (match-string 0 sync0-bibtex-entry-file)))
                  (file (string-trim sync0-bibtex-entry-file ":" extension))
                  (url sync0-bibtex-entry-url))
        (sync0-pdf-download-from-url url file))
    (let*    ((bibkey (or refkey 
                          (sync0-bibtex-completion-choose-key t t)))
              (entry (bibtex-completion-get-entry bibkey))
              (url (sync0-bibtex-completion-get-value "url" entry))
              (doi (sync0-bibtex-completion-get-value "doi" entry))
              (extension (concat "." (or sync0-bibtex-entry-extension "pdf")))
              (file (concat sync0-zettelkasten-attachments-directory bibkey extension)))
        (if (file-exists-p file)
            (message "Cannot download. Attachment exists for key %s" bibkey)
          (sync0-pdf-download-from-url url file)))))

(defun sync0-bibtex-update-key ()
  "Change bibtex key at point with a key using the format
provided by org-roam files. Function intended to be used on an
entry under point in a .bib file"
  (interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (type (cdr (assoc "=type=" entry)))
         (old-key (cdr (assoc "=key=" entry)))
         (suggest-key (if (string-match "[[:digit:]]\{8\}" old-key)
                          (concat (substring old-key 2 4)
                                  (sync0-random-alnum)
                                  (sync0-random-alnum)
                                  (sync0-random-alnum))
                        (concat (substring old-key 0 2)
                                (sync0-random-alnum)
                                (sync0-random-alnum)
                                (sync0-random-alnum))))
         (new-key (sync0-bibtex-entry-key-redefine suggest-key))
         (beg (save-excursion (bibtex-beginning-of-entry)))
         (end (save-excursion (bibtex-end-of-entry)))
         (attach-list  (bibtex-completion-find-pdf old-key))
         (old-archive-image  (concat sync0-bibtex-archive-directory old-key ".jpg"))
         (new-archive-image  (concat sync0-bibtex-archive-directory new-key ".jpg"))
         (old-attachment (when-let (x (cdr (assoc "file" entry)))
                           (string-trim x "{" "}")))
         (new-attachment (when old-attachment
                           (replace-regexp-in-string old-key new-key old-attachment)))
         (path-list (list "file" "Attachment path" new-attachment nil)))
;;; end of definitions
;;; Go to beginning of entry
    (sync0-bibtex-completion-load-entry old-key)
    (bibtex-beginning-of-entry)
    (when (sync0-null-p sync0-bibtex-entry-keywords)
      (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
      (save-excursion 
        (bibtex-make-field (list "keywords" "Whatever string" sync0-bibtex-entry-keywords nil) t)))
;;; Update journal fields
    (save-excursion
      (when (re-search-forward "\\(journal\\)[[:blank:]]+=" end t 1)
        (replace-match "journaltitle" nil nil nil 1)))
;;; Update year fields
    ;; (save-excursion
    ;;   (when (re-search-forward "\\(year\\)[[:blank:]]+=" end t 1)
    ;;     (replace-match "date" nil nil nil 1)))
    (kill-whole-line 1)
    (insert (concat "@" type "{" new-key ",\n"))
    (when (file-exists-p old-archive-image)
      (rename-file old-archive-image new-archive-image))
    (when (and attach-list
               (yes-or-no-p "Rename existing attachments? "))
      (dolist (element attach-list)
        (let ((new-path (replace-regexp-in-string old-key new-key element)))
          (when (file-exists-p element)
            (rename-file element new-path)))))
    (when attach-list 
      (bibtex-make-field path-list t))
    ;; Replace all instances of the old key in the current buffer.
    (sync0-bibtex-update-key-in-buffer old-key new-key)
    (message "Key for entry %s has been replaced with key %s" old-key new-key)))


(defun sync0-bibtex-extract-subpdf ()
  "Extract pdf from an existing pdf."
  ;; Set all fields to nil 
  (interactive)
  (let*    ((bibkey (sync0-bibtex-completion-choose-key t t))
            (input-file (sync0-bibtex-choose-attachment bibkey "pdf"))
            (postfix (read-string "What postfix to identify extracted pdf? : "))
            (output-file (concat sync0-zettelkasten-attachments-directory bibkey "_" postfix ".pdf"))
            (range (read-string "Page range: "))
            (command (concat "pdftk " input-file " cat " range " output " output-file)))
    (if (file-exists-p input-file)
        (shell-command command)
      (message "No PDF found for %s" bibkey))))

(defun sync0-bibtex-open-pdf (&optional bibkey)
  "Open the pdf for bibtex key under point if it exists. If
   optional zathura is t, use zathura to open the pdf."
  (interactive)
  (let* ((bibkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (file (sync0-bibtex-choose-attachment bibkey))
         (program
          (completing-read "Which softare to open attachment ?" sync0-bibtex-attachment-programs)))
    (cond ((and (sync0-null-p program)
                (file-exists-p file))
           (org-open-file file))
          ((file-exists-p file)
           (call-process program nil 0 nil file))
          (t (message "No PDF found for %s" bibkey)))))

(defun sync0-bibtex-open-pdf-at-point (&optional op-crossref)
  "Open the pdf for bibtex key under point if it exists. If
   optional zathura is t, use zathura to open the pdf."
  (interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (crossref (when op-crossref
                   (substring (cdr (assoc "crossref" entry)) 1 -1)))
         (file (sync0-bibtex-choose-attachment bibkey))
	 (extension (file-name-extension file))
	 (program (if (assoc extension sync0-default-file-associations)
                      (cdr (assoc extension sync0-default-file-associations))
		    (completing-read "Which software to open attachment with? " sync0-bibtex-attachment-programs)))
         (crossref-file (when op-crossref
                          (sync0-bibtex-choose-attachment crossref))))
    (if op-crossref 
        (cond ((and (sync0-null-p program)
                    (file-exists-p crossref-file))
               (org-open-file crossref-file))
              ((file-exists-p crossref-file)
               (call-process program nil 0 nil crossref-file))
              (t (message "No attachment found for key %s" crossref)))
      (cond ((and (sync0-null-p program)
                  (file-exists-p file))
             (org-open-file file))
            ((file-exists-p file)
             (call-process program nil 0 nil file))
            (t (message "No attachment found for key %s" bibkey))))))

(defun sync0-bibtex-open-url (&optional bibkey)
  "Open the url for bibtex key under point if it exists."
  (interactive)
  (let* ((bibkey (or bibkey 
                     (sync0-bibtex-completion-choose-key t t)))
         (entry (bibtex-completion-get-entry bibkey))
         (url  (sync0-bibtex-completion-get-value "url" entry)))
    (if (not (sync0-null-p url))
        (browse-url url) ;; Replace with your desired function
      (message "No url found for %s" bibkey))))

(defun sync0-bibtex-download-from-youtube (&optional bibkey)
  "Open the URL for BibTeX key under point if it exists."
  (interactive)
  (let* ((bibkey (or bibkey 
                     (sync0-bibtex-completion-choose-key t t)))
         (entry (bibtex-completion-get-entry bibkey))
         (url  (sync0-bibtex-completion-get-value "url" entry))
         (output-directory sync0-zettelkasten-attachments-directory)
         (output-file (concat (file-name-as-directory output-directory) bibkey ".%(ext)s"))
         ;; Use yt-dlp; it has to be installed from AUR
         (command (concat "yt-dlp --write-sub --write-auto-sub --sub-lang \"en.*\" -o " (shell-quote-argument output-file) " -f \"bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best\" " (shell-quote-argument url))))
    (if (not (sync0-null-p url))
        (progn
          (message "Downloading video for %s" bibkey)
          (shell-command command)
          (message "Video downloaded successfully for %s" bibkey))
      (message "No URL found for %s" bibkey))))

(defun sync0-bibtex-open-notes-at-point ()
  "Open the notes for bibtex key under point in a cite link in a
buffer. Can also be called with key."
  (interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (notes-file  (concat sync0-zettelkasten-references-directory  bibkey ".md")))
    (if (file-exists-p notes-file)
        (find-file notes-file)
      (message "No mdnotes found for key %s" bibkey))))

(defun sync0-bibtex-print-pdf (&optional pdf command)
  "Print the pdf provided in the argument. Generalized for
interactive use and use in pipes to output to the printer."
  (interactive)
  (cond ((and pdf
              command)
         (if (file-exists-p pdf)
             (shell-command (concat command pdf))
           (message "File %s does not exist" pdf)))
        (pdf (let ((command (sync0-print-define-command)))
               (if (file-exists-p pdf)
                   (shell-command (concat command pdf))
                 (message "File %s does not exist" pdf))))
        (command (let* ((bibkey (sync0-bibtex-completion-choose-key))
                        (pdf (sync0-bibtex-choose-attachment bibkey "pdf")))
                        ;; (pdf (if (equal (length attachments) 1)
                        ;;          (car attachments)
                        ;;        (completing-read "Which attachment to open? " attachments)))
                   (if (file-exists-p pdf)
                       (shell-command (concat command pdf))
                     (message "No attachment found for entry %s" bibkey))))
        (t (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
                   (pdf (sync0-bibtex-choose-attachment bibkey "pdf"))
                  (command (sync0-print-define-command))) 
             (if (file-exists-p pdf)
                 (shell-command (concat command pdf))
               (message "No attachment found for entry %s" bibkey))))))

  (defun sync0-bibtex-crop-pdf (&optional in-pdf in-cropbox)
    "Define the cropbox for pdf attached to a bibtex entry. This
function does not crop the pdf; it is a helper function do define
the cropbox. When called interactively, this functions calls
ivy-bibtex to search for the pdf attached to a bibtex entry."
    (interactive)
    (let* ((bibkey (unless in-pdf (sync0-bibtex-completion-choose-key)))
           (pdf (if bibkey
                    (car (bibtex-completion-find-pdf bibkey))
                  in-pdf))
           (cropbox (if in-cropbox
                        in-cropbox
                      (sync0-pdf-define-cropbox pdf))) 
           (output (concat sync0-zettelkasten-attachments-directory "temp.pdf"))
           (command (concat "gs -o " output " -sDEVICE=pdfwrite" cropbox " /PAGES pdfmark\" -f " pdf)))
      (if (file-exists-p pdf)
          (progn 
            (shell-command command)
            (rename-file output pdf t)
            (message "Crop box has been redefined for %s" pdf))
        (message "No pdf found for %s" pdf))))

;; Experimental!!! very hacky and could be improved but works so far
(defun sync0-bibtex-add-field-at-point (&optional reload-mdnote)
  "Add field to single field to bibkey at point. With optional
reload-mdnote, it recalculates keywords and the corresponding
notes file in vault to reflect metadata changes."
  (interactive)
  (setq sync0-bibtex-entry-creation nil)
  (sync0-bibtex-nullify-all-variables)
  (let* ((field (completing-read "Choose Bibtex field: " (remove "keywords" sync0-bibtex-fields)))
         (entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (unique-p (member field sync0-bibtex-unique-fields))
         ;; (multiple-p (member field sync0-bibtex-string-multiple-fields))
         (separator (cond ((member field sync0-bibtex-people-fields)
                           " and ")
                          ((string= field "file")
                           ";")
                          (t ", "))))
    ;; load the variables 
    (sync0-bibtex-completion-load-entry bibkey)
    (unless (sync0-null-p  sync0-bibtex-entry-file)
      (setq sync0-bibtex-entry-file-old t))
    ;; call new value
    (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
    (let* ((keywords-p (assoc "keywords" entry))
           ;; (file-p (when (string= field "file")
           ;;               t))
           (assigned-value (eval (intern (concat "sync0-bibtex-entry-" field))))
           (assigned-values (when (and assigned-value
                                       (null unique-p))
                           (split-string assigned-value separator)))
           (old-value (when (assoc field entry)
                        (substring (cdr (assoc field entry)) 1 -1)))
           (old-values (unless unique-p
                         (when old-value
                           (split-string old-value separator))))
           (multiple-new-p (when assigned-values
                                 (> (length assigned-values) 1)))
           (already-present-p (unless multiple-new-p 
                                (if unique-p
                                    (string= old-value assigned-value)
                                  (member assigned-value old-values))))
           (new-value (cond ((and multiple-new-p
                                  (null unique-p))
                             (let ((new-list (cl-union old-values assigned-values :test #'string=)))
                               (sync0-show-elements-of-list new-list separator)))
                            ((and old-value
                                  (null unique-p))
                             (concat old-value separator assigned-value))
                            (t assigned-value))))
      (if already-present-p
          (error "%s already present or assigned for %s in %s " assigned-value bibkey field)
        (progn 
          ;; position the cursor at beg of entry
          (bibtex-beginning-of-entry)
          (sync0-bibtex-create-field-at-entry field new-value old-value)
          ;; save newly created field
          (when reload-mdnote
            (save-buffer)
            ;; reload entry
            (sync0-bibtex-nullify-all-variables)
            (sync0-bibtex-completion-load-entry bibkey)
            ;; recalc tags
            (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
            (sync0-bibtex-create-field-at-entry "keywords" sync0-bibtex-entry-keywords keywords-p)
            ;; save newly created keywords
            (save-buffer)
            (sync0-bibtex-create-note-from-entry t bibkey)))))))

(defun sync0-bibtex-clean-entry ()
  "Change bibtex key at point with a key using the
format provided by org-roam files"
  (interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (type (cdr (assoc "=type=" entry)))
         (type-lowcase (downcase type))
         (author-full (when-let ((person (cdr (assoc "author" entry))))
                        (unless (string-match "^[[:print:]]+, " person)
                        (substring person 1 -1))))
         (author-list 
          (when author-full
            (split-string author-full " ")))
         (author-first (when author-list
                  (string-trim-whitespace (car author-list))))
         (author-last (when author-list
                  (string-trim-whitespace (cadr author-list))))
         (author (when (and author-first author-last)
                   (concat author-last ", " author-first)))
         (date (unless (assoc "date" entry)
                 (substring (cdr (assoc "year" entry)) 1 -1)))
         (journaltitle (when (string= type-lowcase "article")
                         (unless (assoc "journaltitle" entry)
                           (sync0-bibtex-get-field-value-at-point "journal" entry))))
          ;; "^[0-9]+[a-z]\{2\}"
         (new-key (if (string-match "[[:digit:]]+[[:alpha:]]+" bibkey)
                      bibkey
                    (sync0-bibtex-entry-key-define)))
         (created (unless  (assoc "created" entry)
                    (format-time-string "%Y-%m-%d")))
         (new-path (unless (assoc "file" entry)
                     (concat ":" sync0-zettelkasten-attachments-directory new-key ".pdf:PDF")))
         (regex "^@\\([[A-z]+\\){[[:alnum:]]+,$")
         (regex-journal "^[[:blank:]]+journal[[:blank:]]+=")
         (beg (save-excursion (bibtex-beginning-of-entry)))
         (end (save-excursion (bibtex-end-of-entry)))
         (language (unless (assoc "language" entry)
                     (completing-read "Choose language : "
                                      sync0-bibtex-completion-language)))
         (title-full (sync0-bibtex-get-field-value-at-point "title" entry))
         (title-list 
          (when title-full
            (split-string title-full ":")))
         (title (when title-list
                  (string-trim-whitespace (car title-list))))
         (subtitle (when (> (length title-list) 1)
                     (string-trim-whitespace (cadr title-list))))
         (status (unless  (assoc "status" entry)
                   "inspect"))
         (type-string (concat "@" (upcase-initials type) "{" new-key ",\n")))
    (bibtex-beginning-of-entry)
    (re-search-forward regex end t 1) 
    (kill-whole-line 1)
    (insert type-string)
    ;; remove the journal field 
    (when journaltitle
      (re-search-forward regex-journal end t 1) 
      (kill-whole-line 1))
    (sync0-bibtex-create-field-at-entry "author" author t)
    (sync0-bibtex-create-field-at-entry "title" title t)
    (when subtitle
      (sync0-bibtex-create-field-at-entry "subtitle" subtitle))
    (sync0-bibtex-create-field-at-entry "journaltitle" journaltitle)
    (sync0-bibtex-create-field-at-entry "date" date)
    (sync0-bibtex-create-field-at-entry "created" created)
    (sync0-bibtex-create-field-at-entry "file" new-path)
    (sync0-bibtex-create-field-at-entry "language" language)
    (sync0-bibtex-create-field-at-entry "langid" language)
    (sync0-bibtex-create-field-at-entry "status" status)
    (bibtex-fill-entry)))

  (defun sync0-bibtex-create-note-from-entry (&optional rewrite refkey)
    "Create a new Obsidian markdown note from an existing BibLaTeX
   entry in the default bibliography file. When optional rewrite
   is t, do not create a new file but simply rewrite an existing
   entry with the data of the corresponding bibtex entry in the
   default .bib file. When optional no-extract is true, do not
   attempt to extract a sub-pdf from its crossref (this feature
   is only useful when calling this function in loops to prevent
   undesired behavior)."
    (interactive)
    (let    ((bibkey (or refkey 
                         (sync0-bibtex-completion-choose-key t t))))
      (sync0-bibtex-completion-load-entry bibkey)
      (if rewrite
          (sync0-bibtex-entry-create-obsidian-note-from-entry bibkey t)
        (sync0-bibtex-entry-create-obsidian-note-from-entry bibkey))))

  (defun sync0-bibtex-create-note-at-point (&optional rewrite)
    "Create a new Obsidian markdown note from an existing BibLaTeX
   entry in the default bibliography file. When optional rewrite
   is t, do not create a new file but simply rewrite an existing
   entry with the data of the corresponding bibtex entry in the
   default .bib file. When optional no-extract is true, do not
   attempt to extract a sub-pdf from its crossref (this feature
   is only useful when calling this function in loops to prevent
   undesired behavior)."
    (interactive)
    (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			          (bibtex-parse-entry)))
           (bibkey (cdr (assoc "=key=" entry))))
      (sync0-bibtex-completion-load-entry bibkey)
      ;; (unless no-extract
      ;;   (when (or (string= sync0-bibtex-entry-type-downcase "incollection")
      ;;             (string= sync0-bibtex-entry-type-downcase "inbook")
      ;;             (string= sync0-bibtex-entry-type-downcase "inproceedings"))
      ;;     (sync0-bibtex-extract-pdf-from-crossref)))
      (if rewrite
          (sync0-bibtex-entry-create-obsidian-note-from-entry bibkey t)
        (sync0-bibtex-entry-create-obsidian-note-from-entry bibkey))))



(defun sync0-add-field-theme (&optional arg)
  "Make a keywords field.
If ARG is nil, ask for each keyword and offer completion over
keywords that are already available in the buffer.  Inserting 
the empty string will quit the prompt. If the keyword is not already
present in the buffer, it will be added to the local variable
bu-keywords-values. Note that if you use ido-ubiquitous, the value of
  `ido-ubiquitous-enable-old-style-default' is temporarily set to t within
the body of this command."
  (interactive "P")
  (let ((elist (save-excursion (bibtex-beginning-of-entry)
			       (bibtex-parse-entry)))
        (ido-ubiquitous-enable-old-style-default t)
	append
        new-themes)
    (if (assoc "theme" elist)
	(progn (setq append t)
	       (bibtex-beginning-of-entry)
	       (goto-char 
		(car (last (or (bibtex-search-forward-field "theme" t)
                               (progn (setq append nil)
                                      (bibtex-search-forward-field "OPTtheme" t)))))))
      (bibtex-make-field "theme" t nil))
    (skip-chars-backward "}\n")
    (unless arg
      (let ((cnt 0)
            k)
	(while (and (setq k (completing-read 
                             "Theme (RET to quit): " sync0-bibtex-completion-theme nil))
		    (not (or (equal k "")
		             (equal k "nil"))))
	  (when append (insert ", ")
                (setq append nil))
	  (setq cnt (1+ cnt))
          (unless (member k sync0-bibtex-completion-theme) 
            (push k new-themes))
	  (insert (format "%s%s" (if (> cnt 1) ", " "") k)))))
    (unless (null new-themes)
      (setq sync0-bibtex-entry-theme
            (sync0-show-elements-of-list new-themes ", "))
      (sync0-bibtex-update-var "theme"))))


(defun sync0-bibtex-recalc-tags-and-mdnote-at-point ()
  (interactive)
  (setq sync0-bibtex-entry-creation nil)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (prev-keywords-p (assoc "keywords" entry))
         (bibkey (cdr (assoc "=key=" entry))))
    ;; load the variables 
    (sync0-bibtex-completion-load-entry bibkey)
    ;; (setq sync0-bibtex-entry-key bibkey)
    ;; call new value
    (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
    (sync0-bibtex-create-field-at-entry "keywords" sync0-bibtex-entry-keywords prev-keywords-p)
    (save-buffer)
    (sync0-bibtex-create-note-from-entry t bibkey)))

(defun sync0-bibtex-recalc-tags-and-mdnote (refkey)
  (interactive)
  (setq sync0-bibtex-entry-creation nil)
  (let* ((bibkey (or refkey
                     (sync0-bibtex-completion-choose-key t t)))
         (bib-file (sync0-bibtex-find-key-in-bibfiles bibkey)))
    ;; load the variables 
    (sync0-bibtex-completion-load-entry bibkey)
    ;; call new value
    (let ((old-value sync0-bibtex-entry-keywords)
          (new-keys (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))))
      (if (string= new-keys old-value)
          (message "Keywords already calculated for bibkey %s" bibkey)
        (unless (null bib-file)
          ;; position the cursor at beg of entry
          ;; problem bib search function
          (find-file bib-file)
          (goto-char (point-min))
          (re-search-forward (concat "^@[[:alpha:]]+{" bibkey ",") nil t 1)
          (bibtex-beginning-of-entry)
          (sync0-bibtex-create-field-at-entry "keywords" new-keys old-value)
          ;; save newly created field
          (save-buffer)
          (sync0-bibtex-create-note-from-entry t bibkey))))))

(defun sync0-bibtex-find-key-in-bibfiles (key)
    "Search key in bibfile. Output the full path of the bibliography
file where it was found."
    (let (x)
      (catch 'break
        (dolist (bib-file sync0-bibtex-bibliographies)
          (with-temp-buffer
            (insert-file-contents bib-file)
            (goto-char (point-min))
            (when (re-search-forward (concat "^@[[:alpha:]]+{" key ",") nil t 1)
              (setq x bib-file)
              (throw 'break t)))))
      x))

;; Attention! Do not call this function directly without ivy-bibtex
(defun sync0-bibtex-add-field-and-recalc-keywords-and-mdnote (bibkey field unique-p multiple-new-p separator assigned-value assigned-values) 
  (interactive)
  (setq sync0-bibtex-entry-creation nil)
  (sync0-bibtex-nullify-all-variables)
  ;; load the variables 
  (sync0-bibtex-completion-load-entry bibkey)
  (let* ((old-value (eval (intern (concat "sync0-bibtex-entry-" field))))
         (old-values (unless unique-p
                       (when old-value
                         (split-string old-value separator))))
         (corrected-value (if (null assigned-value)
                              (progn (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
                                     (eval (intern (concat "sync0-bibtex-entry-" field))))
                            assigned-value))
         (already-present-p (unless multiple-new-p 
                              (if unique-p
                                  (string= old-value corrected-value)
                                (member corrected-value old-values))))
         (keywords-p sync0-bibtex-entry-keywords)
         (new-value (cond ((and multiple-new-p
                                (null unique-p))
                           (let ((new-list (cl-union old-values assigned-values :test #'string=)))
                             (sync0-show-elements-of-list new-list separator)))
                          ((and old-value
                                (null unique-p))
                           (concat old-value separator corrected-value))
                          (t corrected-value)))
         (bib-file (sync0-bibtex-find-key-in-bibfiles bibkey)))
    (if already-present-p
        (message "%s already present or assigned for %s in %s " corrected-value bibkey field)
      (unless (null bib-file)
        ;; position the cursor at beg of entry
        ;; problem bib search function
        (find-file bib-file)
        (goto-char (point-min))
        (re-search-forward (concat "^@[[:alpha:]]+{" bibkey ",") nil t 1)
        (bibtex-beginning-of-entry)
        (sync0-bibtex-create-field-at-entry field new-value old-value)
        ;; save newly created field
        (save-buffer)
        ;; reload entry
        (sync0-bibtex-nullify-all-variables)
        (sync0-bibtex-completion-load-entry bibkey)
        ;; recalc tags ; is this necessar?
        ;; (funcall (cadr (assoc "keywords" sync0-bibtex-entry-functions)))
        (sync0-bibtex-create-field-at-entry "keywords" sync0-bibtex-entry-keywords keywords-p)
        ;; save newly created keywords
        (save-buffer)
        (sync0-bibtex-create-note-from-entry t bibkey)))))

  (provide 'sync0-bibtex-actions)
