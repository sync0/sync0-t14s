  (defun sync0-bibtex-extract-pdf-from-crossref (&optional beg end noconfirm)
    "Extract pdf from crossref defined by
   sync0-bibtex-entry-crossref. Be careful! Filename is not
   automatically calculated because this function is not
   standalone; it is intended to be part of pipes. Therefore,
   sync0-bibtex-entry-key, which will be used to name the
   extracted pdf must have been set correctly previously by 
   function."
      (when-let* ((origfile (when sync0-bibtex-entry-crossref
                              (sync0-bibtex-choose-attachment sync0-bibtex-entry-crossref "pdf")))
                  (file (concat sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key ".pdf"))
                  (command-file (concat file " " origfile))
                  (message-function (lambda ()
                                      (message "Pdf for entry %s already present in default pdf folder." sync0-bibtex-entry-key)))
                  (base-command "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="))
        (cond ((and beg end)
               (let ((command (concat
                               base-command
                               beg
                               " -dLastPage="
                               end
                               " -sOutputFile="
                               command-file)))
                 (if (file-exists-p file)
                     (funcall message-function)
                   (shell-command command))))
              (sync0-bibtex-entry-expages
               (let* ((beg (progn
                             (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-expages)
                             (match-string 1 sync0-bibtex-entry-expages)))
                      (end (progn
                             (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-expages)
                             (match-string 1 sync0-bibtex-entry-expages)))
                      (command (concat
                                base-command
                                beg
                                " -dLastPage="
                                end
                                " -sOutputFile="
                                command-file)))
                 (if (file-exists-p file)
                     (funcall message-function)
                   (shell-command command))))
              ((and sync0-bibtex-entry-pages
                    (unless noconfirm
                      (yes-or-no-p "Use page numbers for extraction?")))
               (let* ((beg (progn
                             (string-match "\\([[:digit:]]+\\)-[[:digit:]]+"  sync0-bibtex-entry-pages)
                             (match-string 1 sync0-bibtex-entry-pages)))
                      (end (progn
                             (string-match "[[:digit:]]+-\\([[:digit:]]+\\)" sync0-bibtex-entry-pages)
                             (match-string 1 sync0-bibtex-entry-pages)))
                      (command (concat
                                base-command
                                beg
                                " -dLastPage="
                                end
                                " -sOutputFile="
                                command-file)))
                 (if (file-exists-p file)
                     (funcall message-function)
                   (shell-command command))))
              (t (let* ((beg (read-string "First page for extraction: "))
                        (end (read-string "Last page for extraction: "))
                        (command (concat
                                  base-command
                                  beg
                                  " -dLastPage="
                                  end
                                  " -sOutputFile="
                                  command-file)))
                   (if (file-exists-p file)
                       (funcall message-function)
                     (shell-command command)))))
        (sync0-bibtex-add-key-to-pdf sync0-bibtex-entry-key)))

  (defun sync0-bibtex-extract-multiple-entries-from-pdf (seqlists)
    "Extract pdfs and create bibtex entries and markdown entries
   for each extranction. This function requires an input 'seqlists'
   that is a list composed of n lists of the form: (name,
   first-page, last-page). Likewise, for this to work, it is
   necessary that the pages of the pdf match the page numbers of the
   actual book. Otherwise, the entries will have wrong data."
    (sync0-bibtex-completion-load-entry (sync0-bibtex-completion-choose-key t t))
    (let*    ((bibkey sync0-bibtex-entry-key)
              (add-notes (yes-or-no-p "Create Obsidian notes for the newly created entries?"))
              (no-of-entries (length seqlists))
              (keylist (sync0-bibtex-entry-define-keys-list no-of-entries))
              (bibfile (if (and (sync0-bibtex-buffer-p)
                                (yes-or-no-p "Use current bibliography file?"))
                           (buffer-file-name)
                         (completing-read "Which bibliography file to append to ? "
                                          sync0-bibtex-bibliographies)))
              (keywords-list (split-string sync0-bibtex-entry-keywords ", "))
              (extra-keywords (completing-read-multiple "Input extra keywords: " 
                                                        sync0-bibtex-completion-keywords))
              (master-list (if (sync0-null-p extra-keywords)
                               keywords-list
                             (cl-union keywords-list extra-keywords)))
              (input-file (sync0-bibtex-choose-attachment bibkey "pdf")))
      (setq sync0-bibtex-entry-keywords (sync0-show-elements-of-list master-list ", "))
      (setq sync0-bibtex-entry-type-crossref bibkey)
      (setq sync0-bibtex-entry-type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
      (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
      (setq sync0-bibtex-entry-parent sync0-bibtex-entry-title-fixed)
      ;; Begin loop; specific settings for each note are to be defined
      ;; inside the loop.
      (dotimes (i no-of-entries)
        (let* ((triple (elt seqlists i))
               (filename (elt keylist i))
               (title (nth 0 triple))
               (beg (number-to-string (nth 1 triple)))
               (end (number-to-string (nth 2 triple)))
               (pages (if (string= beg end)
                          beg
                        (concat beg "-" end)))
               (pages-fixed (replace-regexp-in-string "-" "--" pages)))
          (setq sync0-bibtex-entry-key filename)
          (setq sync0-bibtex-entry-title title)
          (setq sync0-bibtex-entry-subtitle nil)
          (setq sync0-bibtex-entry-title-fixed sync0-bibtex-entry-title)
          (setq sync0-bibtex-entry-pages pages)
          (setq sync0-bibtex-entry-shorthand
                (concat title ", p. " pages-fixed))
          (setq sync0-bibtex-entry-file
                (concat ":" sync0-zettelkasten-attachments-directory filename ".pdf:PDF"))
          ;; Beginning of loop actions
          ;; First, extract entry
          (sync0-bibtext-extract-pdf-from-crossref beg end)
          ;; Second, append entry to default bibliography file.
          (sync0-bibtex-entry-append-to-bibliography filename bibfile)
          ;; Third, create an obsidian markdown note for the entry.
          (sync0-bibtex-entry-create-obsidian-note-from-entry filename)))))

(defun sync0-bibtex-extract-from-crossref (&optional refkey noconfirm)
  "Create a new Obsidian markdown note from an existing BibLaTeX
   undesired behavior)."
  (interactive)
  (let    ((bibkey (or refkey 
                       (sync0-bibtex-completion-choose-key t t))))
    (sync0-bibtex-completion-load-entry bibkey)
    (if noconfirm
        (sync0-bibtex-extract-pdf-from-crossref nil nil t)
      (sync0-bibtex-extract-pdf-from-crossref))))

(provide 'sync0-bibtex-extraction)
