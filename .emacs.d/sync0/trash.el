(defun create-langy-variable ()
  (interactive)
  (let ((file-url "https://gist.githubusercontent.com/koseki/e9715224163690df190e/raw/8687e882987016d0e4e26f4c390f44cf25886cc0/ISO639-1.txt")
        (langy '()))
    (with-current-buffer (url-retrieve-synchronously file-url)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (string-match "\\([a-z]+\\)[[:blank:]]+\\(.*\\)" line)
            (setq langy (cons (cons (downcase (match-string 2 line)) (match-string 1 line)) langy))))
        (forward-line)))
    (setq langy (reverse langy))
    langy))

(setq langy (create-langy-variable))

(defun sync0-summarize-pdf (pdf-file input-lang output-lang)
  "Summarize a PDF file based on the specified type and languages."
  (interactive
   (let* (
          ;; (cabinet-path sync0-zettelkasten-attachments-directory)  ; Replace with the actual path to the "cabinet" folder
          (bibkey (sync0-bibtex-completion-choose-key t t))
          (entry (bibtex-completion-get-entry bibkey))
          (pdf-file (sync0-bibtex-choose-attachment bibkey))
          ;; (type (completing-read "Enter the type (book, article, short text, chapter): "
          ;;                        '("book" "article" "short text" "chapter")))
          (input-file-lang (bibtex-completion-get-value "language" entry))
          (input-lang (cdr (assoc input-file-lang langy)))
          (output-file-lang (if (yes-or-no-p "Use same language for summary?")
                                input-file-lang
                              (completing-read "Enter the input language code: " langy)))
          (output-lang (cdr (assoc input-file-lang langy))))
     (list pdf-file input-lang output-lang)))
  (let* ((script-file "/home/sync0/Scripts/python/summarizer/summarize4.py")  ; Replace with the actual path to your Python script
         (python-env "/home/sync0/Scripts/python/summarizer/myenv/bin/activate")  ; Replace with the name or path of your desired Python environment
         (default-directory "/home/sync0/"))  ; Replace with the path to the home folder
    (setenv "PDF_FILE" pdf-file)
    ;; (setenv "PDF_TYPE" type)
    (setenv "INPUT_LANG" input-lang)
    (setenv "OUTPUT_LANG" output-lang)
    ;; (setenv "SUMMARY_LENGTH" (number-to-string (/ (read-number "Enter the summary length (percentage): ") 100.0)))
    (shell-command (format "source %s && python %s"
                           python-env
                           script-file))))



         (output-file "/home/sync0/Gdrive/bibliographies/courcelle-seneuil_unverified.bib")
  (let* ((input-file "/home/sync0/Gdrive/test2.txt")


         
(defun generate-biblatex-entries ()
  (interactive)
  (setq filename "/home/sync0/Gdrive/test2.txt")  ;; Update the filename if necessary
  (setq entries '())  ;; Initialize an empty list to store the entries

  ;; Read the file line by line
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (while (not (eobp))
      (setq line (buffer-substring (line-beginning-position) (line-end-position)))
      (setq entry (format "@InCollection{\n  %s,\n  %s\n}\n\n" (substring line 2 22) (substring line 26)))
      (setq entries (cons entry entries)))
    (setq entries (reverse entries)))

  ;; Write the entries to a new file
  (setq output-filename "/home/sync0/Gdrive/bibliographies/courcelle-seneuil_unverified.bib")
  (with-temp-file output-filename
    (insert (format "%% Biblatex entries generated from %s\n\n" filename))
    (insert (format "@String{century = {19}}\n"))
    (insert (format "@String{created = {2023-07-10}}\n"))
    (insert (format "@String{author = {Courcelle-Seneuil, Jean-Gustave}}\n"))
    (insert (format "@String{journaltitle = {Journal des économistes}}\n"))
    (insert (format "@String{theme = {19th_century, political_economy}}\n"))
    (insert (format "@String{source = {primary}}\n\n"))
    (dolist (entry entries)
      (insert entry)))
  





  (message "Biblatex entries generated successfully. File saved as %s" output-filename))

(defun generate-biblatex-entries ()
  (let* ((input-file "/home/sync0/Gdrive/test2.txt")
         (output-file "/home/sync0/Gdrive/bibliographies/courcelle-seneuil_unverified.bib")
         (entries '())
         (random-characters "abcdefghijkmnpqrstuvwxyz")
         (counter 0))
    
    (with-temp-buffer
      (insert-file-contents input-file)
      (goto-char (point-min))
      
      (while (not (eobp))
        (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
               (date-string (substring line 2 12))
               (title (substring line 26))
               (bibkey (concat "23191"
                               (string (aref random-characters (random (length random-characters))))
                               (string (aref random-characters (random (length random-characters)))))))
          (push (format "@InCollection{%s,
  created           = {2023-07-10},
  author            = {Courcelle-Seneuil, Jean-Gustave},
  journaltitle      = {Journal des économistes},
  theme             = {19th_century, political_economy},
  source            = {primary},
  century           = {19},
  date              = {%s},
  title             = {%s},
}" bibkey date-string title) entries)
          (forward-line)
          (setq counter (1+ counter))))
      
      (setq entries (nreverse entries))
      
      (with-temp-file output-file
        (insert (mapconcat 'identity entries "\n\n")))
      
      (message "Generated %d biblatex entries in %s" counter output-file))))

(generate-biblatex-entries)

(defun generate-biblatex-entries ()
  (let* ((input-file "/home/sync0/Gdrive/test5.txt")
         (output-file "/home/sync0/Gdrive/bibliographies/trash.bib")
         (entries '())
         (random-characters "abcdefghijkmnpqrstuvwxyz")
         (counter 0))
    
    (with-temp-buffer
      (insert-file-contents input-file)
      (goto-char (point-min))
      
      (while (not (eobp))
        (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
               (date (when (string-match "^- (\\([0-9/]+\\)) " line)
                         (match-string 1 line)))
               (date-string (replace-regexp-in-string "/" "-" date))
               (year (substring date-string 0 4))
               ;; this has to be corrected
               ;; (title-volume-pages (substring line 26))
               (journal (when (string-match "[0-9]+) \\([[:graph:][:blank:]]+\\) : [[:upper:]]" line)
                         (match-string 1 line)))
               ;; (title-volume-pages (when (string-match ": \\([[:upper:]][[:graph:][:blank:]]+\\)$" line)
               ;;           (match-string 1 line)))
               ;; (title (substring title-volume-pages 0 (string-match "/\\|, p." title-volume-pages)))
                ;; (title (when (string-match " : \\([[:graph:][:blank:]]+\\)/\\|, p." line)
                ;;          (match-string 1 line)))
               (title (when (string-match " : \\([[:upper:]][[:graph:][:blank:]]+\\),?[[:graph:][:blank:]]*$" line)
                         (match-string 1 line)))
               (review-p (when title
                             (string-match "^Compte rendu " title)))
               ;; this has to be corrected
               (volume (when (string-match "T\\. \\([0-9]+\\)[,: ]" line)
                         (match-string 1 line)))
               ;; (number (when (string-match "N\\. \\([0-9]+\\)[,:]" title-volume-pages)
               ;;           (match-string 1 title-volume-pages)))
               (number (when (string-match ", N\\. \\([0-9]+\\)[ :]" line)
                         (match-string 1 line)))
               ;; this has to be corrected
               (pages (when (string-match "p\\. \\([0-9-]+\\)" line)
                        (match-string 1 line)))
               (bibkey (concat "23192"
                               (string (aref random-characters (random (length random-characters))))
                               (string (aref random-characters (random (length random-characters)))))))

          (push (format "@InCollection{%s,
  title             = {%s},
  created           = {2023-07-10},
  author            = {Courcelle-Seneuil, Jean-Gustave},
  journaltitle      = {%s},
  theme             = {19th_century, political_economy},
  source            = {primary},
  status            = {unverified},
  language          = {french},
  langid            = {french},
  century           = {19},
  date              = {%s},
  year              = {%s},
  file              = {:/home/sync0/Gdrive/cabinet/%s.pdf:PDF},
  %s
  %s
  %s
  %s
}" bibkey title journal date-string year bibkey
             (when review-p (concat "doctype            = {review},"))
             (when volume (concat "volume            = {" volume "},"))
             (when number (concat "number            = {" number "},"))
             (when pages (concat "pages             = {" pages "},"))) entries)

          (forward-line)
          (setq counter (1+ counter))))
      
      (setq entries (nreverse entries))
      
      (with-temp-file output-file
        (insert (mapconcat 'identity entries "\n\n")))
      
      (message "Generated %d biblatex entries in %s" counter output-file))))

(setq dummy-alist nil)




(cdr (cons "234sm" "1889-07/1889-09"))

(assoc "234sm" dummy-alist)

(defun foo ()
  (interactive)
  (let ((input-file "/home/sync0/Gdrive/bibliographies/jde.bib")
        keys)
    (with-temp-buffer
      (insert-file-contents input-file)
      (goto-char (point-min))
   ;; loop to collect values   
      (while (not (eobp))
        (bibtex-next-entry)
        (when-let* ((entry (save-excursion (bibtex-beginning-of-entry)
			              (bibtex-parse-entry)))
               (bibkey (cdr (assoc "=key=" entry)))
               (type (cdr (assoc "=type=" entry)))
               (date (substring (cdr (assoc "date" entry)) 1 -1)))
          (when (string= type "Collection")
            (push (cons bibkey date) keys)))
          (or (bibtex-next-entry)
           (forward-line))))
    (setq dummy-alist keys)
    (message "%s" keys)))




    (message "%s"  (sync0-show-elements-of-list keys "\n"))))



