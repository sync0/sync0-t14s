;; -*- lexical-binding: t -*-

;; (defmacro defvar* (vars initial-value)
;;  `(progn
;;     ,@(loop for var in vars
;;             do (check-type var symbol)
;;             collect `(defvar ,var ,initial-value))))

(require 'cl-lib)
(require 'unidecode)
(require 'bibtex-completion)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-entry-functions)
(require 'sync0-print)
(require 'sync0-pdf)
(require 'sync0-yaml)
(require 'sync0-pandoc)
(require 'doi-utils)
(require 'sync0-bibtex-var-functions)
(require 'sync0-bibtex-extraction)
(require 'sync0-bibtex-diagnostics)
(require 'sync0-bibtex-corrections)
(require 'sync0-bibtex-obsidian)
(require 'sync0-bibtex-pandoc)
(require 'sync0-bibtex-python)
(require 'sync0-bibtex-create-functions)
(require 'sync0-bibtex-actions)
(require 'xah-replace-pairs)

  ;; (defun sync0-bibtex-entry-define-related (&optional bibkey)
  ;;   "Define the author for new BibLaTeX entry."
  ;;   (if bibkey 
  ;;       (let ((entry (bibtex-completion-get-entry bibkey)))
  ;;         (setq sync0-bibtex-entry-related
  ;;               (bibtex-completion-get-value "related" entry))
  ;;         (setq sync0-bibtex-entry-relatedtype
  ;;               (bibtex-completion-get-value "relatedtype" entry)))
  ;;     (progn
  ;;         (setq sync0-bibtex-entry-related (sync0-bibtex-completion-choose-key nil t))
  ;;         (setq sync0-bibtex-entry-relatedtype
  ;;               (completing-read "Type de relation : " '("multivolume" "origpubas" "reviewof" "reprintof" "reprintas" "reprintfrom" "translationas" "translationfrom" "translationof"))))))

  ;; (defun sync0-bibtex-entry-append-to-bibliography (bibkey &optional bibfile)
  ;;   "Append new BibLaTeX entry to default bibliography file.
  ;;  Beware, this function only constructs and appends the entry
  ;;  to the bib file; the values of the entry must have been defined
  ;;  elsewhere. For the sake of speed, this function does not
  ;;  perform any sanity checks on duplicate entries, and thus a
  ;;  unique correct entry is assumed to be supplied as mandatory
  ;;  argument bibkey."
  ;;   (let* ((definitions (mapcar #'eval sync0-bibtex-entry-definitions-list))
  ;;          (bibtex-fields sync0-bibtex-fields)
  ;;          (bibliography-file (or bibfile sync0-bibtex-default-bibliography))
  ;;          (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields definitions))
  ;;          ;; define the bibtex entries
  ;;          (entries
  ;;           (let (x)
  ;;             (dolist (element fields x) 
  ;;               (unless (sync0-null-p (cadr element))
  ;;                 (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
  ;;          (bibtex-entry (concat "@" sync0-bibtex-entry-type "{" bibkey ",\n" entries "}\n")))
  ;;     (progn
  ;;       (append-to-file bibtex-entry nil bibliography-file)
  ;;       (sync0-bibtex-entry-inform-new-entry))))


  ;; (defun sync0-bibtex-update-key-in-bibfile (oldkey newkey bibfile)
  ;;   "Change bibtex key at point with a key using the format provided
  ;; by org-roam files"
  ;;   (with-temp-file bibfile
  ;;     (insert-file-contents bibfile)
  ;;     (goto-char (point-min))
  ;;     (while (re-search-forward oldkey nil t)
  ;;       (replace-match newkey))))


;;   (defun sync0-bibtex-open-notes-at-point ()
;;     "Open the notes for bibtex key under point in a cite link in a
;; buffer. Can also be called with key."
;;     (interactive)
;;     (let* ((bibkey (sync0-bibtex-completion-choose-key t t))
;;            (notes-file  (concat sync0-zettelkasten-references-directory  bibkey ".md")))
;;       (if (file-exists-p notes-file)
;;           (find-file notes-file)
;;         (message "No markdown notes file found for entry %s" bibkey))))


;; (defun sync0-bibtex-add-field ()
;;   (interactive)
;;   (setq sync0-bibtex-entry-creation nil)
;;   (sync0-bibtex-nullify-all-variables)
;;   (let* ((field (completing-read "Choose Bibtex field: " sync0-bibtex-fields))
;;          (entry (save-excursion (bibtex-beginning-of-entry)
;; 			        (bibtex-parse-entry)))
;;          (bibkey (cdr (assoc "=key=" entry))))
;;     ;; load the variables 
;;     (sync0-bibtex-completion-load-entry bibkey)
;;     (unless (sync0-null-p  sync0-bibtex-entry-file)
;;       (setq sync0-bibtex-entry-file-old t))
;;     ;; (setq sync0-bibtex-entry-key bibkey)
;;     ;; call new value
;;     (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
;;     (let* ((keywords-p (when (string= field "keywords")
;;                          t))
;;            (prev-keywords-p (when keywords-p
;;                               (assoc "keywords" entry)))
;;            ;; (file-p (when (string= field "file")
;;            ;;               t))
;;            (old-value (when (assoc field entry)
;;                         (unless keywords-p
;;                       (substring (cdr (assoc field entry)) 1 -1))))
;;            (assigned-value (eval (intern (concat "sync0-bibtex-entry-" field))))
;;            (separator (when old-value
;;                         (cond ((member field sync0-bibtex-people-fields)
;;                                 " and ")
;;                                ((string= field "file")
;;                                          ";")
;;                                (t ", "))))
;;            (new-value (if old-value
;;                           (concat old-value separator assigned-value)
;;                         assigned-value))
;;            (bib-list (list field "Whatever string" new-value nil)))
;;       (bibtex-beginning-of-entry)
;;       (when (or old-value
;;                 (and prev-keywords-p
;;                      keywords-p)) 
;;         (save-excursion
;;           (re-search-forward (concat "[[:space:]]+" field "[[:space:]]+="))
;;           (bibtex-kill-field nil t)))
;;       (bibtex-make-field bib-list t))))

(defun sync0-bibtex-copy-attachment-at-point (&optional in-path bibkey)
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (file (sync0-bibtex-choose-attachment bibkey))
         (extension  (file-name-extension file t))
         (path (or in-path
                   (read-string "Où envoyer ce fichier ? (finir en /) " sync0-goodreads-directory))))
    (sync0-bibtex-completion-load-entry bibkey)
    (if (and (file-exists-p file)
             (file-accessible-directory-p path))
        (let* ((filetitle (sync0-bibtex-define-attachment-copy-filename bibkey path))
               (command (concat "cp "
                                file
                                " \""
                                path
                                filetitle
                                extension
                                "\"")))
          (shell-command command)
          (message "PDF for %s moved to target location" bibkey))
      (message "No PDF found for %s" bibkey))))

(defun sync0-bibtex-copy-pdf-to-path (&optional in-path bibkey)
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (file (sync0-bibtex-choose-attachment refkey))
         (path (or in-path
                   (read-string "Où envoyer ce fichier ? (finir en /) " sync0-goodreads-directory)))
         (extension  (file-name-extension file t)))
    (sync0-bibtex-completion-load-entry refkey)
    (if (and (file-exists-p file)
             (file-accessible-directory-p path))
        (let* ((filetitle (sync0-bibtex-define-attachment-copy-filename refkey path))
               (command (concat "cp "
                                file
                                " \""
                                path
                                filetitle
                                extension
                                "\"")))
          (shell-command command)
          (message "PDF for %s moved to target location" refkey))
      (message "No PDF found for %s" refkey))))

  (defun sync0-bibtex-archive-entry (&optional bibkey)
    "Choose an entry to send to the archived bibliography. This
function fails when the entry is at the top of the buffer becase
the function 1- fails to compute."
    (interactive)
    (when bibkey
      (bibtex-search-entry bibkey))
    ;; (re-search-forward (concat "{" bibkey ",") nil t 1)
    (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
           (end (save-excursion (bibtex-end-of-entry))))
      (append-to-file beginning end sync0-bibtex-archived-bibliography)
      (delete-region beginning end)))

(defun sync0-bibtex-move-entry-to-bibfile (&optional bibkey bibfile)
  "Choose an entry to send to the archived bibliography. This
function fails when the entry is at the top of the buffer becase
the function 1- fails to compute."
  (interactive)
  (when-let* ((refkey (or bibkey
                     (sync0-bibtex-completion-choose-key t t)))
         (bibdestiny (or bibfile
                         (completing-read "Which bibliography file to send to ? "
                                          sync0-bibtex-bibliographies nil t))))
      (bibtex-search-entry refkey)
      (let ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
            (end (save-excursion (bibtex-end-of-entry))))
      (append-to-file beginning end bibdestiny)
      (delete-region beginning end))))

(defun sync0-bibtex-delete-entry (&optional bibkey)
  "Choose an entry to permanently delete. Remeber: The deleted
entry could be recovered if previously commited on a
version-controlled file. The attachments are sent to the system
trash, as defined by the desktop environments (KDE, Gnome,
etc.)."
  (interactive)
  (let ((refkey (or bibkey
                    (sync0-bibtex-completion-choose-key t t))))
    (bibtex-search-entry refkey)
    (sync0-bibtex-completion-load-entry refkey)
    (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
           (attachments (bibtex-completion-find-pdf refkey))
           (num-of-attachments (length attachments))
           (trash-message (concat "Send to trash entry "
                                  sync0-bibtex-entry-lastname
                                  (or sync0-bibtex-entry-date-fixed
                                      " ")
                                  sync0-bibtex-entry-title-compatible
                                  "?"))
           (trash-attach-message (format "Entry %s has %s attachments. Do you want to send these to trash?" refkey attachments))
           (end (save-excursion (bibtex-end-of-entry))))
      (when (yes-or-no-p trash-message)
        (delete-region beginning end))
      (when (yes-or-no-p trash-attach-message)
        (mapc #'move-file-to-trash attachments)))))

(defun sync0-bibtex-delete-attachments (&optional bibkey)
  "Choose an entry to permanently delete. Remeber: The deleted
entry could be recovered if previously commited on a
version-controlled file. The attachments are sent to the system
trash, as defined by the desktop environments (KDE, Gnome,
etc.)."
  (interactive)
  (let ((refkey (or bibkey
                    (sync0-bibtex-completion-choose-key t t))))
    (sync0-bibtex-completion-load-entry refkey)
    (let* ((attachments (bibtex-completion-find-pdf refkey))
           (num-of-attachments (length attachments))
           (trash-attach-message
            (format "Entry %s has %s attachments. Do you want to send these to trash?" refkey num-of-attachments)))
      (when (yes-or-no-p trash-attach-message)
        (mapc #'move-file-to-trash attachments)))))

  ;; (defun sync0-bibtex-extract-toc-from-pdf (&optional bibkey)
  ;;   "Add toc to pdf using cpdf command line"
  ;;   (interactive)
  ;;   (if bibkey 
  ;;       (sync0-bibtex-completion-load-entry bibkey)
  ;;     (sync0-bibtex-completion-load-entry))
  ;;   (let* ((beginning (save-excursion (1- (bibtex-beginning-of-entry))))
  ;;          (end (save-excursion (bibtex-end-of-entry))))

  (defun sync0-bibtex-add-toc-to-pdf (&optional refkey)
    "Add toc to pdf using cpdf command line"
    (interactive)
    (let* ((bibkey (or refkey 
                       (sync0-bibtex-completion-choose-key t t)))
           (file (sync0-bibtex-choose-attachment bibkey))
           ;; necessary to prevent malfunction due to same input
           ;; and output file
           (output (concat sync0-zettelkasten-attachments-directory "temp.pdf"))
           (toc-file (concat sync0-bibtex-tocs-directory bibkey ".txt"))
           ;; (malformation (when (yes-or-no-p "Malformed? ")
           ;;                 "-gs /usr/bin/gs -gs-malformed "))
           ;; (command (concat "cpdf " (unless (sync0-null-p malformation) malformation) "-utf8 -add-bookmarks " toc-file " " file " -o " output)
           (command (concat "cpdf -utf8 -add-bookmarks " toc-file " " file " -o " output)))
      (if (and (file-exists-p toc-file)
               (file-exists-p file))
          (progn
            (shell-command command)
            (rename-file output file t))
        (message "Conditions not satisfied by entry %s to attach corresponding toc." bibkey))))

  (defun sync0-bibtex-yank-citation-from-bibkey (&optional bibkey)
    "Add bibkey citation kill ring."
    (interactive)
    (let ((refkey (if bibkey
                      bibkey
                    (sync0-bibtex-completion-choose-key t t))))
      (sync0-bibtex-completion-load-entry refkey)
      (when-let ((citation (concat sync0-bibtex-entry-lastname
                                   (or sync0-bibtex-entry-date-fixed
                                       " ")
                                   sync0-bibtex-entry-title-compatible)))
        (kill-new citation)
        (message "%s copied to kill ring." citation))))

  (defun sync0-bibtex-yank-citation-from-bibkey (&optional bibkey)
    "Add bibkey citation kill ring."
    (interactive)
    (let ((refkey (if bibkey
                      bibkey
                    (sync0-bibtex-completion-choose-key t t))))
      (sync0-bibtex-completion-load-entry refkey)
      (when-let ((citation (concat sync0-bibtex-entry-lastname
                                   (or sync0-bibtex-entry-date-fixed
                                       " ")
                                   sync0-bibtex-entry-title-compatible)))
        (kill-new citation)
        (message "%s copied to kill ring." citation))))

  (defun sync0-bibtex-convert-jpg-to-pdf (&optional bibkey)
    "Copy attached pdf to path and change the title to make it
readable."
    (interactive)
    (let* ((refkey (if bibkey
                       bibkey
                     (sync0-bibtex-completion-choose-key t t)))
           (file (sync0-bibtex-choose-attachment refkey))
           (image (concat sync0-bibtex-archive-directory refkey ".jpg"))
           ;; (extension (file-name-extension file))
           (command (concat "convert " image " -auto-orient " file)))
      ;; (sync0-bibtex-completion-load-entry refkey)
      (if (and (file-exists-p file)
               (file-exists-p image))
          (shell-command command)
        (message "Conversion for entry %s failed." refkey))))

;; (defun sync0-bibtex-recalc-keywords ()
;;   (interactive)
;;   (setq sync0-bibtex-entry-creation nil)
;;   (sync0-bibtex-nullify-all-variables)
;;   (let* ((field "keywords")
;;          (entry (save-excursion (bibtex-beginning-of-entry)
;; 			        (bibtex-parse-entry)))
;;          (bibkey (cdr (assoc "=key=" entry))))
;;     ;; load the variables 
;;     (sync0-bibtex-completion-load-entry bibkey)
;;     ;; (setq sync0-bibtex-entry-key bibkey)
;;     ;; call new value
;;     (funcall (cadr (assoc field sync0-bibtex-entry-functions)))
;;     (let* ((assigned-value (eval (intern (concat "sync0-bibtex-entry-" field))))
;;            (bib-list (list field "Whatever string" assigned-value nil)))
;;       (bibtex-beginning-of-entry)
;;       (save-excursion
;;         (re-search-forward (concat "[[:space:]]+" field "[[:space:]]+="))
;;         (bibtex-kill-field nil t))
;;       (bibtex-make-field bib-list t))
;;     (save-buffer)))

  (major-mode-hydra-define bibtex-mode nil 
    ("Entries"
     (("c" sync0-bibtex-clean-entry "Clean this entry")
      ("E" sync0-bibtex-define-entry "New entry")
      ("e" (sync0-bibtex-define-entry t) "Quick new entry")
      ("d" doi-utils-add-bibtex-entry-from-doi "Entry from DOI")
      ;; ("m" sync0-bibtex-define-multiple-entries "Define entries")
      ("t" sync0-bibtex-transplant-obsidian-ref-into-biblatex "Entry from mdnote")
      ("u" sync0-bibtex-update-key "Update key")
      ("n" sync0-bibtex-open-notes-at-point "Open notes")
      ("M" sync0-bibtex-define-similar-type-entries "Define X similar entries")
      ("f" sync0-bibtex-define-entries-from-bibkey "Define multiple from this entry")
      ("g" sync0-bibtex-python-bibentry-from-gallica "Define entry from Gallica"))
      ;; ("w" sync0-bibtex-open-url "Open url")
      ;; ("M" sync0-bibtex-move-entry-to-bibfile "Move entry to bibfile")
      ;; ("D" sync0-bibtex-delete-entry "Delete entry")
      ;; ("A" sync0-bibtex-archive-entry "Archive entry")
      ;; ("1" sync0-bibtex-file-exists-p "Check file exists")
     "PDF editing"
     (("X" sync0-bibtex-delete-attachments "Delete attachments")
      ("P" sync0-pandoc-export-epub-to-pdf "EPUB to PDF")
      ("C" sync0-bibtex-copy-attachment-at-point "Copy attachment")
      ;; ("C" sync0-bibtex-crop-pdf "Crop attached PDF")
      ("T" sync0-bibtex-recalc-tags-and-mdnote-at-point "Recalc keywords at point")
      ("o" sync0-bibtex-open-pdf-at-point "Show PDF")
      ("O" (sync0-bibtex-open-pdf-at-point t) "Show crossref PDF"))
      ;; ("x" sync0-bibtex-extract-from-crossref "Extract from crossref")
      ;; ("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
      ;; ("p" sync0-bibtex-print-pdf "Print att. from entry")
      ;; This does note work for some reason
      ;; ("x" sync0-bibtex-arrange-pdf "Arrange pdf")
      ;; ("T" sync0-bibtex-add-toc-to-pdf "Add TOC to PDF")
      ;; ("K" sync0-bibtex-add-key-to-pdf "Add key to PDF")
      ;; ("s" sync0-bibtex-extract-subpdf "Extract subpdf")
      ;; ("d" sync0-bibtex-download-pdf "Download pdf from url")
     ;; "Visit"
      ;; ("o" sync0-org-ref-open-pdf-at-point "Open in pdfview")
      ;; ("n" sync0-bibtex-open-notes "Open annotations")
     "Bibliographies"
     (("S" ivy-bibtex "Search entry")
      ("s" ivy-bibtex-with-local-bibliography "Search entry locally")
      ("b" sync0-bibtex-recalc-bibliographies "Recalc bibliographies")
      ("B" sync0-bibtex-recalc-master-bibliography "Recalc master bib file")
      ("r" sync0-bibtex-populate-keys "Populate keys")
      ("v" sync0-bibtex-visit-bibliography "Visit bibfile"))
     "Etc"
     ;; ("r" (sync0-bibtex-update-completion-files sync0-bibtex-completion-variables-list) "Refresh completion vars")
     (("a" sync0-bibtex-add-field-at-point "Add field")
      ("A" (sync0-bibtex-add-field-at-point t) "Add field and recalc")
      ("i" sync0-bibtex-convert-jpg-to-pdf "Convert jpg to pdf")
      ("k" sync0-add-field-theme "Add theme")
      ("1" bibtex-convert-pdf-to-txt "Convert to TXT")
      ;; ("2" sync0-bibtex-python-summarize-txt "Summarize TXT")
      ("y" sync0-bibtex-yank-citation-from-bibkey "Yank citation.")
      ("N" sync0-bibtex-create-note-at-point "Create mdnote")
      ("R" (sync0-bibtex-create-note-at-point t) "Rewrite mdnote"))))

  (provide 'sync0-bibtex-functions)
