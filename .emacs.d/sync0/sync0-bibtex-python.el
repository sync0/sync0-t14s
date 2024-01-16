(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-corrections)

;; (defun sync0-bibtex-python-format-from-gallica (url-or-list)
;;   "Run the Python script with each URL from the URL-LIST."
;;   (interactive)
;;   (let ((script-path 
;;         (mycommand (lambda ()
;;                    (shell-command (format "python3 %s %s %s" script-path url bibkey)))))
;;     (if (listp url-or-list)
;;         ;; If url-or-list is a list, loop through each URL
;;         (dolist (url url-or-list)
;;           mycommand)
;;       ;; If url-or-list is not a list, assume it's a single URL
;;       (let ((url (read-string "Enter a URL: "))
;;             (bibkey (sync0-bibtex-entry-key-define)))
;; mycommand))))

(defvar sync0-bibtex-python-summarizer-script-path
  "/home/sync0/Scripts/python/summarizer/summarize4.py"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-summarizer-venv-path
  "/home/sync0/Scripts/python/summarizer/myenv/bin/activate"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-webscrapper-script-path
  "/home/sync0/Scripts/python/web_scrapper/scrapper.py"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-webscrapper-venv-path
"/home/sync0/Scripts/python/web_scrapper/myenv/bin/activate"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-pdf-to-txt-script-path
  "/home/sync0/Scripts/python/summarizer/pdf-to-txt.py"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-pdf-to-txt-venv-path
"/home/sync0/Scripts/python/summarizer/myenv/bin/activate"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-doi-or-isbn-venv-path
"/home/sync0/Scripts/python/doi_biblatex/.venv/bin/activate"
  "Path of my current python commands for doi or bib to isbn script")

(defvar sync0-bibtex-python-doi-to-bibtex-script-path
  "/home/sync0/Scripts/python/doi_biblatex/isbn_to_bibtex.py"
  "Path of my current python webscrapper script")

(defvar sync0-bibtex-python-google-books-to-bibtex-script-path
  "/home/sync0/Scripts/python/web_scrapper/google-books_scrapper.py"
  "Path of my google python webscrapper script")

(defvar sync0-bibtex-python-webscrapper-scripts-list
  '(("HAL scrapper" . "/home/sync0/Scripts/python/web_scrapper/hal_scrapper.py")
    ("BnF (Gallica)" . "/home/sync0/Scripts/python/web_scrapper/bnf_scrapper.py")
    ;; ("Webcat" . "/home/sync0/Scripts/python/worldcat_scrapper")
    ("Theses.fr" . "/home/sync0/Scripts/python/web_scrapper/theses_scrapper.py"))
  "Paths of python webscrapper scripts")

(defun sync0-url-clean-google-books-url (url)
  "Clean Google Books URL by removing everything after the first '&'."
  (if (string-match "&" url)
    (concat "\"" (substring url 0 (match-beginning 0)) "\"")
    (concat "\"" url "\"")))

(defun sync0-bibtex-python-determine-webscrapper-script (url)
  "Analyze the url and determine the right web scrapper python
script to use"
  (cond ((string-match-p "hal.science" url)
         "/home/sync0/Scripts/python/web_scrapper/hal_scrapper.py")
        ((string-match-p "catalogue.bnf" url)
         "/home/sync0/Scripts/python/web_scrapper/bnf_scrapper.py")
        ((string-match-p "books.google" url)
         "/home/sync0/Scripts/python/web_scrapper/google-books_scrapper.py")
        ((string-match-p "theses.fr" url)
         "/home/sync0/Scripts/python/web_scrapper/theses_scrapper.py")))

(defun sync0-bibtex-python-determine-doi-or-isbn (doi-or-isbn)
  "Analyze the string and determine the right python command
to use"
  (if (string-match-p "[0-9]+\\.[0-9]+" doi-or-isbn)
         "doi2bib"
         "isbn_meta"))

;; (defun sync0-bibtex-python-bibentry-from-webscrapper (&optional url-or-list)
;;   "Run the Python script chosen interactiveley with each URL in the
;; the URL-LIST. The only accepted URLs are those from the BnF
;; Catalogue The Python script creates biblatex entries on defined
;; bibfile."
;;   (interactive)
;;   (let ((bibfile (sync0-bibtex-entry-choose-bibliography-file))
;;         (venv sync0-bibtex-python-webscrapper-venv-path)
;;         (url (or url-or-list
;;                  (read-string "Enter a URL: "))))
;;     (if (listp url)
;;         ;; If url-or-list is a list, loop through each URL
;;         (dolist (daurl url-or-list)
;;           (when-let ((bibkey (sync0-bibtex-entry-key-define))
;;                 (script-path (sync0-bibtex-python-determine-webscrapper-script daurl)))
;;             (shell-command (format "source %s && python3 %s %s %s %s" venv script-path daurl bibkey bibfile))))
;;       ;; If url-or-list is not a list, assume it's a single URL
;;       (when-let ((bibkey (sync0-bibtex-entry-key-define))
;;             (script-path (sync0-bibtex-python-determine-webscrapper-script url)))
;;         (shell-command (format "source %s && python3 %s %s %s %s" venv script-path url bibkey bibfile))))))

(defun sync0-bibtex-python-bibentry-from-webscrapper (&optional url-or-list)
  "Run the Python script chosen interactiveley with each URL in the
the URL-LIST. The only accepted URLs are those from the BnF
Catalogue The Python script creates biblatex entries on defined
bibfile."
  (interactive)
  (let ((bibfile (sync0-bibtex-entry-choose-bibliography-file))
        (venv sync0-bibtex-python-webscrapper-venv-path)
        (url (sync0-define-list-interactively "Enter a URL: " "Add another URL? " url-or-list)))
    ;; If url-or-list is a list, loop through each URL
    (dolist (daurl url)
      (when-let* ((bibkey (sync0-bibtex-entry-key-define))
                  (script-path (sync0-bibtex-python-determine-webscrapper-script daurl))
                  (clean-url (if (string= script-path sync0-bibtex-python-google-books-to-bibtex-script-path)
                                 (sync0-url-clean-google-books-url daurl)
                               (concat "\"" daurl "\""))))
        (shell-command (format "source %s && python3 %s %s %s %s" venv script-path clean-url bibkey bibfile))))))

;; (defun sync0-bibtex-python-convert-pdf-to-txt (&optional setbibkey)
;;   "Summarize a PDF file based on the specified type and languages."
;;   (interactive)
;;   (let* ((bibkey (or setbibkey
;;           (sync0-bibtex-completion-choose-key t t)))
;;          (entry (bibtex-completion-get-entry bibkey))
;;          (pdf-file (sync0-bibtex-choose-attachment bibkey))
;;          (script-file sync0-bibtex-python-pdf-to-txt-script-path)
;;          (venv sync0-bibtex-python-pdf-to-txt-venv-path))
;;     (shell-command (format "source %s && python %s %s"
;;                            venv
;;                            script-file
;;                            pdf-file))))

(defun sync0-bibtex-python-summarize-txt (&optional setbibkey)
  "Summarize a PDF file based on the specified type and languages."
  (interactive)
  (let* ((bibkey (or setbibkey
          (sync0-bibtex-completion-choose-key t t)))
         (entry (bibtex-completion-get-entry bibkey))
         (file (sync0-bibtex-choose-attachment bibkey))
         (script-file sync0-bibtex-python-summarizer-script-path)
         (venv sync0-bibtex-python-summarizer-venv-path))
    (if (sync0-file-has-extension-p file "txt")
    (shell-command (format "source %s && python %s %s"
                           venv
                           script-file
                           file))
    (message "%s is not a TXT file. Unable to summarize."))))

;; (defun sync0-bibtex-python-bibentry-from-doi-or-isbn (&optional doi-or-list)
;;   "Run the Python command chosen interactiveley with each DOI or ISBN number in the
;; the URL-LIST."
;;   (interactive)
;;   (let ((bibfile (sync0-bibtex-entry-choose-bibliography-file))
;;         (venv sync0-bibtex-python-doi-or-isbn-venv-path)
;;         (script-path sync0-bibtex-python-doi-to-bibtex-script-path)
;;         (doi (sync0-define-list-interactively doi-or-list))
;;     (if (listp doi)
;;         ;; If oi-or-list is a list, loop through each URL
;;       (with-temp-buffer
;;         (dolist (element doi-or-list)
;;               (when-let ((python-command (sync0-bibtex-python-determine-doi-or-isbn element)))
;;                 (forward-line)
;;                 (if (string= python-command "doi2bib")
;;                     (insert (shell-command-to-string (format "source %s && %s %s" venv python-command element)))
;;                   (insert (shell-command-to-string (format "source %s && python3 %s %s" venv script-path element))))
;;                (bibtex-fill-entry)))
;;         (goto-char (point-min))
;;         (sync0-bibtex-corrections-format-bibtex-to-biblatex)
;;         (append-to-file nil nil bibfile))
;;       (with-temp-buffer
;;               (when-let ((python-command (sync0-bibtex-python-determine-doi-or-isbn doi)))
;;                 (forward-line)
;;                 (if (string= python-command "doi2bib")
;;                     (insert (shell-command-to-string (format "source %s && %s %s" venv python-command doi)))
;;                   (insert (shell-command-to-string (format "source %s && python3 %s %s" venv script-path doi))))
;;                 (bibtex-fill-entry))
;;                     (goto-char (point-min))
;;         (sync0-bibtex-corrections-format-bibtex-to-biblatex)
;;         (append-to-file nil nil bibfile)))))

(defun sync0-bibtex-python-bibentry-from-doi-or-isbn (&optional doi-or-list)
  "Run the Python command chosen interactiveley with each DOI or ISBN number in the
the URL-LIST."
  (interactive)
  (let ((bibfile (sync0-bibtex-entry-choose-bibliography-file))
        (venv sync0-bibtex-python-doi-or-isbn-venv-path)
        (script-path sync0-bibtex-python-doi-to-bibtex-script-path)
        (doi (sync0-define-list-interactively "Enter DOI or ISBN: " "Add another DOI or ISBN? " doi-or-list)))
        ;; If oi-or-list is a list, loop through each URL
      (with-temp-buffer
        (dolist (element doi)
              (when-let ((python-command (sync0-bibtex-python-determine-doi-or-isbn element)))
                (forward-line)
                (if (string= python-command "doi2bib")
                    (insert (shell-command-to-string (format "source %s && %s %s" venv python-command element)))
                  (insert (shell-command-to-string (format "source %s && python3 %s %s" venv script-path element))))
               (bibtex-fill-entry)))
        (goto-char (point-min))
        (sync0-bibtex-corrections-format-bibtex-to-biblatex)
        (append-to-file nil nil bibfile))))

(provide 'sync0-bibtex-python)
