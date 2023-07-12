(require 'sync0-bibtex-key-functions)

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

(defun sync0-bibtex-python-bibentry-from-gallica (&optional url-or-list)
  "Run the a Python script with each URL in the the URL-LIST. The
only accepted URLs are those from the BnF Catalogue The Python
script creates biblatex entries on defined bibfile."
  (interactive)
  (let ((script-path sync0-bibtex-python-webscrapper-script-path)
        (bibfile (sync0-bibtex-entry-choose-bibliography-file))
        (venv sync0-bibtex-python-webscrapper-venv-path)
        (url (or url-or-list
                 (read-string "Enter a URL: "))))
    (if (listp url)
        ;; If url-or-list is a list, loop through each URL
        (dolist (daurl url-or-list)
          (let ((bibkey (sync0-bibtex-entry-key-define)))
            (shell-command (format "source %s && python3 %s %s %s %s" venv script-path daurl bibkey bibfile))))
      ;; If url-or-list is not a list, assume it's a single URL
      (let ((bibkey (sync0-bibtex-entry-key-define)))
        (shell-command (format "source %s && python3 %s %s %s %s" venv script-path url bibkey bibfile))))))

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

(provide 'sync0-bibtex-python)
