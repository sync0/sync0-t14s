(require 'sync0-bibtex-var-functions)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-entry-functions)

(defun sync0-bibtex-extract-page-matcher (&optional stringy back)
  (let ((range (or stringy
                   (read-string "Pages to extract: ")))
        (regex (if back
                   "-\\([[:digit:]]+\\)$"
                 "^\\([[:digit:]]+\\)-")))
    (if (string-match regex range)
        (match-string 1 range)
      range)))

(defun sync0-bibtex-extract-pdf-from-crossref (crossref-file &optional noconfirm)
  "Extract pdf from crossref defined by
   sync0-bibtex-entry-crossref. Be careful! Filename is not
   automatically calculated because this function is not
   standalone; it is intended to be part of pipes. Therefore,
   sync0-bibtex-entry-key, which will be used to name the
   extracted pdf must have been set correctly previously by 
   function."
  (unless (null crossref-file)
    (when-let* ((file (concat sync0-zettelkasten-attachments-directory sync0-bibtex-entry-key ".pdf"))
                (command-file (concat file " " crossref-file))
                (message-function (lambda ()
                                    (message "Pdf for entry %s already present in default pdf folder." sync0-bibtex-entry-key)))
                (base-command "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER -dFirstPage="))
      (cond ((and (null sync0-bibtex-entry-expages)
                  (null sync0-bibtex-entry-pages))
             (let* ((range (read-string "Pages to extract: "))
                    (beg (sync0-bibtex-extract-page-matcher range))
                    (end (sync0-bibtex-extract-page-matcher range t))
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
            (sync0-bibtex-entry-expages
             (let* ((beg (sync0-bibtex-extract-page-matcher sync0-bibtex-entry-expages))
                    (end (sync0-bibtex-extract-page-matcher sync0-bibtex-entry-expages t))
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
                  (or noconfirm
                    (yes-or-no-p "Use page numbers for extraction?")))
             (let* ((beg (sync0-bibtex-extract-page-matcher sync0-bibtex-entry-pages))
                    (end (sync0-bibtex-extract-page-matcher sync0-bibtex-entry-pages t))
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
      (sync0-bibtex-add-key-to-pdf sync0-bibtex-entry-key))))

(defun sync0-bibtex-derive-pdf-from-crossref (&optional no-confirm from-pages)
  (when (and sync0-bibtex-entry-crossref
             (member sync0-bibtex-entry-type sync0-bibtex-crossref-types)
             (or no-confirm
                 (yes-or-no-p "Extract  PDF from crossref?")))
    (let ((crossref-file (sync0-bibtex-choose-attachment sync0-bibtex-entry-crossref "pdf")))
      (sync0-bibtex-extract-pdf-from-crossref crossref-file from-pages))))

(defun sync0-bibtex-extract-from-crossref (&optional refkey noconfirm)
  "Create a new Obsidian markdown note from an existing BibLaTeX
   undesired behavior)."
  (interactive)
  (let    ((bibkey (or refkey 
                       (sync0-bibtex-completion-choose-key t t))))
    (sync0-bibtex-completion-load-entry bibkey)
    (let ((crossref-file (sync0-bibtex-choose-attachment sync0-bibtex-entry-crossref "pdf")))
      (if noconfirm
          (sync0-bibtex-extract-pdf-from-crossref crossref-file t)
        (sync0-bibtex-extract-pdf-from-crossref crossref-file)))))

(provide 'sync0-bibtex-extraction)
