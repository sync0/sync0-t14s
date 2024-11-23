;; -*- lexical-binding: t -*-
(require 'cl-lib)
;; (require 'scihub)
(require 'unidecode)
(require 'bibtex-utils)
(require 'bibtex-completion)
(require 'sync0-bibtex-fields)
(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-var-functions)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-utils)
(require 'sync0-ivy-bibtex)
(require 'sync0-bibtex-extraction)
(require 'sync0-bibtex-entry-functions)
(require 'sync0-print)
(require 'sync0-pdf)
(require 'sync0-yaml)
(require 'sync0-pandoc)
(require 'doi-utils)
(require 'sync0-bibtex-extraction)
(require 'sync0-bibtex-diagnostics)
(require 'sync0-bibtex-corrections)
(require 'sync0-bibtex-obsidian)
(require 'sync0-bibtex-pandoc)
(require 'sync0-bibtex-python)
(require 'sync0-bibtex-create-functions)
(require 'sync0-bibtex-actions)
(require 'sync0-bibtex-url)
(require 'xah-replace-pairs)

(defun sync0-bibtex-recalc-master-bibliography ()
  "Recalculate the master bibliography file based on other bibliography files.
The master bibliography file is determined by `sync0-bibtex-master-bibliography'.
The list of other bibliography files is specified by `sync0-bibtex-bibliographies'.
Any bibliography file listed in `sync0-bibtex-bibliographies' but not `sync0-bibtex-sick-bibliography' will be appended to the master bibliography file.

This function also replaces smart quotes with their corresponding simple equivalents."
  (interactive)
  (condition-case err
      (let ((master-bibliography sync0-bibtex-master-bibliography)
            (bibliographies (remove sync0-bibtex-sick-bibliography sync0-bibtex-bibliographies)))
        (with-temp-file master-bibliography
          (dolist (biblio bibliographies)
            (insert-file-contents biblio)
            (goto-char (point-max))
            (insert "\n")))
        (goto-char (point-min))
        (message "Master bibliography file has been recalculated."))
    (file-error
     (message "Error: Unable to recalculate master bibliography. %s" (error-message-string err)))))

(defun sync0-bibtex-visit-bibliography ()
  (interactive)
  (let ((bib-file
         (completing-read "Fichier Biblatex : " sync0-bibtex-bibliographies)))
    (find-file
     (expand-file-name bib-file))))

(defun sync0-bibtex-copy-attachment-at-point (&optional in-path bibkey)
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (let* ((entry (save-excursion (bibtex-beginning-of-entry)
			        (bibtex-parse-entry)))
         (bibkey (cdr (assoc "=key=" entry)))
         (file (sync0-bibtex-choose-attachment bibkey))
         (extension  (file-name-extension file t))
         (path (sync0-validate-path (or in-path
                                        (read-directory-name "Où envoyer ce fichier ?" sync0-goodreads-directory)))))
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
                   (read-directory-name "Où envoyer ce fichier ? " sync0-goodreads-directory)))
         (extension  (file-name-extension file t)))
    (sync0-bibtex-completion-load-entry refkey)
    (if (and (file-exists-p file)
             (file-accessible-directory-p path))
        (let* ((filetitle (sync0-bibtex-define-attachment-copy-filename refkey path))
               (file-and-path (concat path filetitle extension))
               (file-and-path-quotes (concat "\"" file-and-path "\""))
               (command (concat "cp "
                                file
                                " "
                                file-and-path-quotes)))
          (shell-command command)
          (setq sync0-bibtex-temp-pdf-copy-new-path-and-filename file-and-path)
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
    ;; ("d" doi-utils-add-bibtex-entry-from-doi "Entry from DOI")
    ;; ("m" sync0-bibtex-define-multiple-entries "Define entries")
    ("d" sync0-bibtex-derive-entries-from-collection "Derive from collection")
    ("t" sync0-bibtex-transplant-obsidian-ref-into-biblatex "Entry from mdnote")
    ("u" sync0-bibtex-update-key "Update key")
    ("n" sync0-bibtex-open-notes-at-point "Open notes")
    ("M" sync0-bibtex-define-similar-type-entries "Define X similar entries")
    ("f" sync0-bibtex-define-entries-from-bibkey "Define multiple from this entry")
    ;; ("g" sync0-bibtex-python-bibentry-from-gallica "Define entry from Gallica")
    ("W" sync0-bibtex-python-bibentry-from-webscrapper "Define from webscrapper")
    ("Z" sync0-bibtex-bibentry-from-anystyle "Define from AnyStyle")
    ("V" bibtex-completion-download-from-youtube "Download Youtube video")
    ("2" sync0-bibtex-duplicate-attachment-from-bibkey "Duplicate attachment")
    ("D" sync0-bibtex-python-bibentry-from-doi-or-isbn "Define from DOI or ISBN"))
   ;; ("w" sync0-bibtex-open-url "Open url")
   ;; ("M" sync0-bibtex-move-entry-to-bibfile "Move entry to bibfile")
   ;; ("D" sync0-bibtex-delete-entry "Delete entry")
   ;; ("A" sync0-bibtex-archive-entry "Archive entry")
   ;; ("1" sync0-bibtex-file-exists-p "Check file exists")
   "PDF editing & more"
   (("X" sync0-bibtex-delete-attachments "Delete attachments")
    ;; ("P" sync0-pandoc-export-epub-to-pdf "EPUB to PDF")
    ("P" bibtex-completion-search-pdf-with-python "Search PDF (Python)")
    ("C" sync0-bibtex-copy-attachment-at-point "Copy attachment")
    ;; ("C" sync0-bibtex-crop-pdf "Crop attached PDF")
    ("3" bibtex-completion-ocr-pdf-files "OCR pdf")
    ("T" sync0-bibtex-recalc-tags-and-mdnote-at-point "Recalc keywords at point")
    ("o" sync0-bibtex-open-pdf-at-point "Show PDF")
    ("O" (sync0-bibtex-open-pdf-at-point t) "Show crossref PDF")
    ;; ("x" sync0-bibtex-extract-from-crossref "Extract from crossref")
    ;; ("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
    ("p" bibtex-completion-print-pdf-list "Print att. from entry"))
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
   (("U" sync0-ivy-bibtex-update-cache "Update BibTeX keys cache")
    ("S" sync0-consult-bibtex-multi-select "Search entry")
    ("s" sync0-consult-bibtex-with-local-bibliography "Search entry locally")
    ("b" sync0-bibtex-recalc-bibliographies "Recalc bibliographies")
    ("B" sync0-bibtex-recalc-master-bibliography "Recalc master bib file")
    ("r" sync0-bibtex-populate-keys "Populate keys")
    ("v" sync0-bibtex-visit-bibliography "Visit bibfile"))
   "Etc"
   ;; ("r" (sync0-bibtex-update-completion-files sync0-bibtex-completion-variables-list) "Refresh completion vars")
   (("a" sync0-bibtex-add-field-at-point "Add field")
    ("A" (sync0-bibtex-add-field-at-point t) "Add field and recalc")
    ("I" sync0-bibtex-convert-jpg-to-pdf "Convert jpg to pdf")
    ("i" bibtex-completion-open-url "Open URL")
    ("k" sync0-add-field-theme "Add theme")
    ("w" sync0-search-in-catalogs "Search in catalogs")
    ("1" bibtex-convert-pdf-to-txt "Convert to TXT")
    ;; ("2" sync0-bibtex-python-summarize-txt "Summarize TXT")
    ("y" bibtex-completion-yank-citations-from-bibkeys "Yank citation.")
    ("N" sync0-bibtex-create-note-at-point "Create mdnote")
    ("R" (sync0-bibtex-create-note-at-point t) "Rewrite mdnote"))))

(provide 'sync0-bibtex-functions)
