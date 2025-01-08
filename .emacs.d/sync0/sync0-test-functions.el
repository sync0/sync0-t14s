(defun sync0-obsidian-import-reference ()
  "Import target markdown note into our bibtex file."
  (interactive)
  (let* ((key (read-string "Input existing key: "))
         (contents 
          (with-temp-buffer 
            (insert-file-contents (concat sync0-obsidian-dir key ".md"))
            (buffer-string)))
         (end (string-match  "^# .+\n" contents))
         (yaml (substring contents 0 end))
         (author (when
                     (or (string-match  "^author: \\[\"\\(.+\\)\"\\]\n" yaml)
                     (string-match  "^editor: \\[\"\\(.+\\)\"\\]\n" yaml))
                   (match-string 1 yaml)))
         (translator (when
                         (string-match  "^translator: \\[\"\\(.+\\)\"\\]\n" yaml)
                       (match-string 1 yaml)))
         (type (when
                   (string-match  "^biblatex_type: \\(.+\\)\n" yaml)
                 (match-string 1 yaml)))
         (url (when
                  (string-match  "^url: \\(.+\\)\n" yaml)
                (match-string 1 yaml)))
         (pages (when
                    (string-match  "^pages: \\(.+\\)\n" yaml)
                  (match-string 1 yaml)))
         (chapter (when
                    (string-match  "^pages: \\(.+\\)\n" yaml)
                  (match-string 1 yaml)))
         (edition (when
                    (string-match  "^edition: \\(.+\\)\n" yaml)
                  (match-string 1 yaml)))
         (language (when
                       (string-match  "^language: \\(.+\\)\n" yaml)
                     (match-string 1 yaml)))
         (langid language)
         (journaltitle (when
                           (string-match  "^journaltitle: \"?\\(.+\\)\"?\n" yaml)
                         (match-string 1 yaml)))
         (number (when
                     (string-match  "^number: \\(.+\\)\n" yaml)
                   (match-string 1 yaml)))
         (volume (when
                     (string-match  "^volume: \\(.+\\)\n" yaml)
                   (match-string 1 yaml)))
         (crossref (when
                       (string-match  "^crossref: \\(.+\\)\n" yaml)
                     (match-string 1 yaml)))
         (eventdate (when
                        (string-match  "^eventdate: \\(.+\\)\n" yaml)
                      (match-string 1 yaml)))
         (venue (when
                    (string-match  "^venue: \"?\\(.+\\)\"?\n" yaml)
                  (match-string 1 yaml)))
         (eventtitle (when
                         (string-match  "^eventtitle: \"?\\(.+\\)\"?\n" yaml)
                       (match-string 1 yaml)))
         (booktitle (when
                    (string-match  "^booktitle: \"?\\(.+\\)\"?\n" yaml)
                  (match-string 1 yaml)))
         (booksubtitle (when
                    (string-match  "^booksubtitle: \"?\\(.+\\)\"?\n" yaml)
                  (match-string 1 yaml)))
         (title (when
                    (string-match  "^title: \"?\\(.+\\)\"?\n" yaml)
                  (match-string 1 yaml)))
         (subtitle (when
                       (string-match  "^subtitle: \"?\\(.+\\)\"?\n" yaml)
                     (match-string 1 yaml)))
         (date (when
                   (string-match  "^date: \\(.+\\)\n" yaml)
                 (match-string 1 yaml)))
         (urldate (if
                      (string-match  "^urldate: \\(.+\\)\n" yaml)
                      (match-string 1 yaml)
                    (format-time-string "%Y-%m-%d")))
         (addendum (when
                       (string-match  "^addendum: \"?\\(.+\\)\"?\n" yaml)
                     (match-string 1 yaml)))
         (library (when
                      (string-match  "^library: \"?\\(.+\\)\"?\n" yaml)
                    (match-string 1 yaml)))
         (location (when
                       (string-match  "^location: \"?\\(.+\\)\"?\n" yaml)
                     (match-string 1 yaml)))
         (publisher (when
                        (string-match  "^publisher: \"?\\(.+\\)\"?\n" yaml)
                      (match-string 1 yaml)))
         (file-pdf (concat "/home/sync0/Documents/pdfs/" key ".pdf"))
         (origdate (when
                       (string-match  "^origdate: \\(.+\\)\n" yaml)
                     (match-string 1 yaml)))
         (bibtex-definitions (list 
                              title
                              subtitle
                              date
                              origdate
                              author
                              journaltitle
                              booktitle
                              booksubtitle
                              translator
                              crossref
                              eventdate
                              eventtitle
                              venue
                              volume
                              number
                              chapter
                              edition
                              pages
                              publisher
                              location
                              pages
                              addendum
                              url
                              urldate
                              language
                              langid
                              library
                              file-pdf))
         (bibtex-fields (if (equal type "collection")
                            (cl-substitute "editor" "author" sync0-bibtex-full-fields)
                          sync0-bibtex-full-fields))
         (fields (mapcar* #'(lambda (x y) (list x y)) bibtex-fields bibtex-definitions))
         (entries
          (let (x)
            (dolist (element fields x) 
              (unless (or (null (cadr element))
                          (equal (cadr element) ""))
                (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
         (bib-file sync0-default-bibliography)
         (bib-contents
          (with-temp-buffer 
            (insert-file-contents bib-file)
            (buffer-string)))
         (bibtex-entry (concat "\n@" (upcase-initials type) "{" key "," "\n" entries "\n}\n")))
    (if (string-match key bib-contents)
        (error "%s key already present in bibliography." key)
      (progn 
        (with-temp-file bib-file
          (insert-file-contents bib-file)
          (goto-char (point-max))
          (insert bibtex-entry))
        (message "File %s has been transcoded into bibliography." key)))))

;; (defun sync0-obsidian-zettel-migrate ()
;; "Convert the text open in the present buffer from Org-roam format
;; to Obsian markdown using my custom Zettel format."
;; (interactive)
;; (when (equal major-mode "org-mode")
;; (let* 
;; (title keywoard)
;; (filetags)
;; (creation its a property)
;; (roam-aliases )
;; (zettel-type)

;; (obsidian-file (concat sync0-obsidian-dir filename ".md")) 
;; (obsidian-entry (concat "---\n"
;;                         "citekey: " filename "\n"
;;                         "biblatex_type: " type "\n"
;;                         "title: " title "\n"
;;                         "subtitle: " subtitle "\n"
;;                         "authors: [" author-fixed "]\n"
;;                         "parent:\n" 
;;                         "aliases: [" lastname " (" date  ") " title " : " subtitle "]\n"
;;                         "url: " url "\n"
;;                         "origdate:\n"
;;                         "date: " date "\n"
;;                         "media:\n"
;;                         "trace:\n"
;;                         "tags: [references," type "," filename "," lastname "]\n"
;;                         "---\n" 
;;                         "# " lastname " (" date ") " title " : " subtitle "\n" 
;;                         "## Progrès de la lecture\n" 
;;                         "## Annotations\n"))


;;                             (cdar (org-collect-keywords '("roam_key"))))))



;;  (when-let*    ((type (org-entry-get 1 "ZETTEL_TYPE"))
;;                 (roam-aliases (org-entry-get 1 "ROAM_ALIASES"))
;; (let*    ((zettel-type (org-entry-get 1 "ZETTEL_TYPE"))



;; (defun sync0-obsidian-reference-migrate ()
;;   "Convert the text open in the present buffer from Org-roam format
;; to Obsian markdown using my custom Zettel format."
;;   (interactive)
;;   (when (equal (org-entry-get 1 "ZETTEL_TYPE") "reference")
;;     (let*    ((zettel-type "reference")
;;               (title        (cadar (org-collect-keywords '("TITLE"))))
;;               (subtitle (if (org-collect-keywords '("SUBTITLE"))
;;                             (cadar (org-collect-keywords '("SUBTITLE")))
;;                           title))
;;               (title-fixed (if (equal title subtitle)
;;                                title
;;                              (concat title " : " subtitle)))
;;               (tags-raw (org-collect-keywords '("FILETAGS")))
;;               (tags-line (unless (null tags-raw)
;;                (substring (cadar tags-raw) 1 -1)))
;;               (tags (unless (null tags-raw)
;;                        (when (string-match ":" tags-line)
;;                       (replace-regexp-in-string ":" "," tags-line))))
;;               (author (cadar (org-collect-keywords '("AUTHOR"))))
;;               (citekey (substring (org-entry-get 1 "ROAM_REFS") 5 nil))
;;               ;; (creation (org-entry-get 1 "CREATED"))
;;               (date (org-entry-get 1 "DATE"))
;;               (origdate (if (org-entry-get 1 "ORIG_DATE")
;;                             (org-entry-get 1 "ORIG_DATE")
;;                           date))
;;               (date-fixed (if (equal date origdate)
;;                               (concat "(" date ")")
;;                             (concat "(" origdate ") (" date ")")))
;;               (biblatex-type (when (org-entry-get 1 "BIBLATEX_TYPE")
;;                                (org-entry-get 1 "BIBLATEX_TYPE")))
;;               (url (when (org-entry-get 1 "WEBSITE")
;;                                (org-entry-get 1 "WEBSITE")))
;;               (langauge (when (org-entry-get 1 "LANGUAGE")
;;                           (org-entry-get 1 "LANGUAGE")))
;;               ;; (creation-fixed (if (string-match "/" creation)
;;               ;;                     (replace-regexp-in-string "/" "-" creation)
;;               ;;                   creation))
;;               (obsidian-file (concat sync0-obsidian-dir citekey ".md")) 
;;               (obsidian-entry (concat "---\n"
;;                                       "zettel_type: reference\n"
;;                                       "citekey: " citekey "\n"
;;                                       "biblatex_type: " biblatex-type "\n"
;;                                       "title: " title "\n"
;;                                       (unless (equal title subtitle)
;;                                         (concat "subtitle: " subtitle "\n"))
;;                                       "authors: [" author "]\n"
;;                                       "parent:\n" 
;;                                       "aliases: [" author " " date-fixed  " " title-fixed "]\n"
;;                                       "url: " url "\n"
;;                                       (unless (equal date origdate)
;;                                         (concat "origdate: " origdate "\n"))
;;                                       "date: " date "\n"
;;                                       "media:\n"
;;                                       "trace:\n"
;;                                       "tags: [references,"
;;                                       (unless (null biblatex-type)
;;                                        (concat biblatex-type ","))
;;                                       citekey ","
;;                                       (unless (null date)
;;                                        (concat date ","))
;;                                       (unless (equal date origdate)
;;                                        (concat origdate ","))
;;                                       (unless (null tags-raw)
;;                                        tags) 
;;                                       "]\n"
;;                                       "---\n" 
;;                                       "# " author " " date-fixed " " title-fixed "\n"
;;                                       "## Progrès de la lecture\n" 
;;                                       "## Annotations\n")))
;; (unless (file-exists-p obsidian-file)
;;       (with-temp-buffer 
;;         (insert obsidian-entry)
;;         (write-file obsidian-file))))))

;; (let* ((origtags (when (re-search-forward "^tags: \\[\\([A-z0-9-_@]+\\)\\]" nil t 1)

(defun sync0-obsidian-delete-duplicate-tags ()
  "Delete duplicate tags in YAML preamble."
  (interactive)
  (when-let* ((origtags (when (re-search-forward "^tags: \\[\\(.+\\)\\]" nil t 1)               
                     (match-string-no-properties 1)))
         (tags-list (split-string-and-unquote origtags ","))
         (new-tags-list (delete-duplicates tags-list :test #'string-equal))
         (new-tags-line
          (let (x)
            (dolist (element new-tags-list x)
              (setq x (concat element  "," x)))))
         (corrected-tags (substring new-tags-line 0 -1))
         (new-tags-string (concat "tags: [" corrected-tags "]\n")))
    (goto-char (point-min))
    (re-search-forward "^tags: .+" nil t 1)               
    (kill-whole-line 1)
    (insert new-tags-string)))

(defun sync0-obsidian-correct-bibkeys ()
  "Delete duplicate tags in YAML preamble."
  (interactive)
  (when-let* ((tags (when (re-search-forward "^tags: \\[\\(.+\\)\\]" nil t 1)               
                      (match-string-no-properties 1)))
              (key (when (string-match "[\\[,]r?\\([0-9]\\{14\\}\\)[,\\]]" tags)
                     (match-string-no-properties 1 tags)))
              (new-key (concat "bibkey/" key))
              (new-tags (replace-regexp-in-string key new-key tags))
              (tags-string (concat "tags: [" new-tags "]\n")))
    (goto-char (point-min))
    (re-search-forward "^tags: .+" nil t 1)               
    (kill-whole-line 1)
    (insert tags-string)))

(defun sync0-obsidian-correct-biblatex ()
  "Delete duplicate tags in YAML preamble."
  (interactive)
  (when-let* ((tags (when (re-search-forward "^tags: \\[\\(.+\\)\\]" nil t 1)               
                      (match-string-no-properties 1)))
              (key (when (string-match "[,\\[]\\(thesis\\)[,\\]]" tags)
                     (match-string-no-properties 1 tags)))
              (new-key (concat "reference/" key))
              (new-tags (replace-regexp-in-string key new-key tags))
              (tags-string (concat "tags: [" new-tags "]\n")))
    (goto-char (point-min))
    (re-search-forward "^tags: .+" nil t 1)               
    (kill-whole-line 1)
    (insert tags-string)))

;; (defun sync0-obsidian-correct-tags ()
;;   "Delete duplicate tags in YAML preamble."
;;   (interactive)
;;     (when-let* ((tags (when (re-search-forward "^authors: \\[\\(.+\\)\\]" nil t 1)               
;;                         (match-string-no-properties 1)))
;;                 (new-tags (replace-regexp-in-string "fiche" "fiche/people" tags)))
;;       (goto-char (point-min))
;;       (re-search-forward "^authors: .+" nil t 1)               
;;       (kill-whole-line 1)))
    ;; (insert (concat "aliases: [" new-aliases "]\n"))))

(defun sync0-obsidian-correct-authors ()
  "Have authors in the YAML preamble have correct formating."
  (interactive)
  (when-let* ((authors (when (re-search-forward "^authors: \\[\\(.+\\)\\]" nil t 1)               
                         (match-string-no-properties 1))))
    (goto-char (point-min))
    (re-search-forward "^authors: .+" nil t 1)               
    (kill-whole-line 1)
    (insert (concat "author: [" authors "]\n"))))

(defun sync0-obsidian-correct-author-field ()
  "Have authors in the YAML preamble have correct formating."
  (interactive)
  (when (re-search-forward "^zettel_type: reference" nil t 1)
    (when-let* ((buffer (buffer-file-name))
           (key (when (string-match "\\([0-9]+\\).md$" buffer)
                  (match-string-no-properties 1 buffer)))
           (author  (when (stringp key)
                      (sync0-org-ref-get-citation-author key)))
           (author-fixed (when (stringp author)
            (cond ((string-match " and " author)
                                ;; create a list with parts 
                                (let* ((author-list  (split-string author " and "))
                                       (names (let (x)
                                                (dolist  (element author-list x)
                                                  (setq x (concat x element "\",\""))))))
                                  (concat "\"" (substring names 0 -2))))
                               ;; check when author is an organization
                               ((string-match "^{" author)
                                (concat "\"" (substring author 1 -1) "\""))
                               ;; other cases
                               (t (concat "\"" author "\"")))))
           (new-author (when (stringp author-fixed)
            (concat "author: [" author-fixed "]\n"))))
      (goto-char (point-min))
      (re-search-forward "^author: .+" nil t 1)               
      (kill-whole-line 1)
      (insert new-author))))

(defun sync0-obsidian-correct-aliases ()
  "Delete duplicate tags in YAML preamble."
  (interactive)
  (when-let* ((aliases (when (re-search-forward "^aliases: \\[\\(.+\\)\\]" nil t 1)               
                         (match-string-no-properties 1)))
              (new-aliases (replace-regexp-in-string ":" "-" aliases)))
    (goto-char (point-min))
    (re-search-forward "^aliases: .+" nil t 1)               
    (kill-whole-line 1)
    (insert (concat "aliases: [" new-aliases "]\n"))))

(defun sync0-obsidian-correct-date ()
  "Delete duplicate tags in YAML preamble."
  (interactive)
  (when-let* ((date (when (re-search-forward "^origdate: \\(.+\\)" nil t 1)               
                         (match-string-no-properties 1)))
              (new-date (replace-regexp-in-string "\"" "" date)))
    (goto-char (point-min))
    (re-search-forward "^origdate: .+" nil t 1)               
    (kill-whole-line 1)
    (insert (concat "origdate: " new-date "\n"))))

;; (defun sync0-obsidian-correct-aliases ()
;;   "Delete duplicate tags in YAML preamble."
;;   (interactive)
;;   (when-let* ((aliases (when (re-search-forward "^# \\(.+\\)" nil t 1)               
;;                          (match-string-no-properties 1)))
;;               (new-aliases (replace-regexp-in-string "(\"" "(" aliases)))
;;     (goto-char (point-min))
;;     (re-search-forward "^#: .+" nil t 1)               
;;     (kill-whole-line 1)
;;     (insert (concat "title: " new-aliases "\n"))))


(defun sync0-obsidian-correct-creation ()
  "Delete duplicate tags in YAML preamble."
  (interactive)
  (when-let* ((date (when (re-search-forward "^created: \\([0-9-/]+\\)" nil t 1)               
                         (match-string-no-properties 1)))
              (new-date (replace-regexp-in-string "/" "-" date)))
    (goto-char (point-min))
    (re-search-forward "^created: .+" nil t 1)               
    (kill-whole-line 1)
    (insert (concat "created: " new-date "\n"))))

;; (defun sync0-obsidian-correct-tags ()
;;   "Delete duplicate tags in YAML preamble."
;;   (interactive)
;;   (when-let* ((tags (when (re-search-forward "^title: \\[\\(.+\\)\\]" nil t 1)               
;;                          (match-string-no-properties 1)))
;;               (new-tags (replace-regexp-in-string "\"" "" tags)))
;;     (goto-char (point-min))
;;     (re-search-forward "^title: .+" nil t 1)               
;;     (kill-whole-line 1)
;;     (insert (concat "title: [" new-tags "]\n"))))


               
  ;;       (insert obsidian-entry)
  ;;       (write-file obsidian-file)))


  ;; (with-current-buffer (find-file-noselect f)
  ;;   (sync0-obsidian-reference-migrate)))

;;     (dolist (f
;; (f-files sync0-obsidian-dir
;;          (lambda (k) (string-match-p ".md" k)) t))
;;   (with-current-buffer (find-file-noselect f)
;;     (sync0-obsidian-correct-aliases)))

;; (defun test ()
;; (interactive)
;;     (re-search-forward "^tags: \\[\\(.+\\)\\]$" nil t 1))


;;  (setq sync0-test-dir (concat (getenv "HOME") "/Dropbox/test/"))

(defun sync0-org-roam-replace-id-with-markdown-links ()
  (interactive)
  (let* ((current-path (buffer-file-name))
         (current-file (when (string-match ".+\\([0-9]+\\)\\.org$" current-path)
                         (match-string-no-properties 1 current-path)))
         (obsidian-file (concat sync0-test-dir current-file ".org")))  
    (with-temp-buffer 
      (insert-file-contents-literally current-path)
      (while (re-search-forward org-link-bracket-re nil t)
        (let* ((mdata (match-data))
               (path (match-string 1))
               (desc (match-string 2)))
          (when (string-prefix-p "file:" path)
            (setq path (expand-file-name (substring path 5)))
            (when-let* ((node-path (caar (org-roam-db-query
                                         [:select [nodes:file] :from nodes])))
                       (file-title (when (string-match ".+/\\([0-9]+\\)\\.org$" node-path)
                                     (match-string-no-properties 1))))
              (set-match-data mdata)
              (replace-match (concat "[" desc "](" file-title ".md)"))))))
      (if (file-exists-p obsidian-file)
          (message "File %s already present in Obsidian vault!" current-file)
        (write-file obsidian-file)))))

(defun sync0-org-roam-copy-to-markdown ()
  (interactive)
  (let* ((current-path (buffer-file-name))
         (current-file (when (string-match ".+\\([0-9]+\\)\\.org$" current-path)
                         (match-string-no-properties 1 current-path)))
         (obsidian-file (concat sync0-test-dir current-file ".md")) 
         (command
          (concat "pandoc -s -f org -t markdown+yaml_metadata_block " current-path  " -o " obsidian-file)))
    (shell-command command)))

;; (defun test ()
;;   (interactive)
;;   (while (re-search-forward org-link-bracket-re nil t)
;;     (let* ((mdata (match-data))
;;            (orgid (substring (match-string-no-properties 1) 3 nil)) 
;;            (desc (match-string 2)))
;;       (when-let* ((path
;;                    (caar
;;                     (org-roam-db-query [:select [file]
;;                                                 :from nodes
;;                                                 :where (= id $s1)]
;;                                        orgid)))
;;                   (filename (when (string-match "^.+/\\([0-9]+\\)\\.org$" path)
;;                               (match-string-no-properties 1 path))))
;;         (set-match-data mdata)
;;         (replace-match
;;          (concat "[" desc "](" filename ".md)"))))))

(defun sync0-obsidian-migrate-project ()
  "Convert the text open in the present buffer from Org-roam format
to Obsian markdown using my custom Zettel format."
  (interactive)
  (unless (equal (org-entry-get 1 "ZETTEL_TYPE") "reference")
    (let*    ((zettel-type (org-entry-get 1 "ZETTEL_TYPE"))
              (title        (cadar (org-collect-keywords '("TITLE"))))
              (end (progn
                     (re-search-forward "^#\\+FILETAGS: .+$" nil t)
                     (match-end 0)))
              (contents (buffer-substring-no-properties (1+ end) (point-max)))
              (tags-raw (org-collect-keywords '("FILETAGS")))
              (tags-line (when tags-raw
                           (substring (cadar tags-raw) 1 -1)))
              (tags (when tags-line
                      (string-match ":" tags-line)
                      (replace-regexp-in-string ":" "," tags-line)))
              (creation (org-entry-get 1 "CREATED"))
              (project (org-entry-get 1 "PROJECT_TITLE"))
              (project-fixed (when project (substring project 1 -1)))
              (current-path (buffer-file-name))
              (current-file (when (string-match "^.+/\\([0-9]+\\)\\.org$" current-path)
                              (match-string-no-properties 1 current-path)))
              (obsidian-file (concat sync0-obsidian-dir current-file ".md")) 
              (obsidian-entry (concat "---\n"
                                      "zettel_type: " zettel-type "\n"
                                      "created: " creation "\n"
                                      "title: " title "\n"
                                      "aliases: [" title "]\n"
                                      (unless (null project)
                                        (concat "project: " project-fixed "\n")) 
                                      "tags: [" zettel-type
                                      (unless (null tags)
                                        (concat "," tags)) 
                                      "]\n"
                                      "---\n" 
                                      "# " title "\n")))
      (unless (file-exists-p obsidian-file)
        (with-temp-buffer 
          (insert obsidian-entry)
          (insert contents)
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let* ((mdata (match-data))
                   (raw-orgid (match-string-no-properties 1))
                   (orgid (substring raw-orgid 3 nil)) 
                   (desc (match-string 2)))
         (unless (string-prefix-p "file:" raw-orgid)
              (when-let* ((path
                           (caar
                            (org-roam-db-query [:select [file]
                                                        :from nodes
                                                        :where (= id $s1)]
                                               orgid)))
                          (filename (when (string-match "^.+/\\([0-9]+\\)\\.org$" path)
                                      (match-string-no-properties 1 path))))
                (set-match-data mdata)
                (replace-match
                 (concat "[" desc "](" filename ".md)"))))))
          (write-file obsidian-file))))))




(defun sync0-obsidian-migrate-zettel ()
  "Convert the text open in the present buffer from Org-roam format
to Obsian markdown using my custom Zettel format."
  (interactive)
  (unless (equal (org-entry-get 1 "ZETTEL_TYPE") "reference")
    (let*    ((zettel-type (org-entry-get 1 "ZETTEL_TYPE"))
              (title        (cadar (org-collect-keywords '("TITLE"))))
              (end (progn
                     (re-search-forward "^#\\+FILETAGS: .+$" nil t)
                     (match-end 0)))
              (contents (buffer-substring-no-properties (1+ end) (point-max)))
              (tags-raw (org-collect-keywords '("FILETAGS")))
              (tags-line (when tags-raw
                           (substring (cadar tags-raw) 1 -1)))
              (tags (when tags-line
                      (string-match ":" tags-line)
                      (replace-regexp-in-string ":" "," tags-line)))
              (creation (org-entry-get 1 "CREATED"))
              (current-path (buffer-file-name))
              (current-file (when (string-match "^.+/\\([0-9]+\\)\\.org$" current-path)
                              (match-string-no-properties 1 current-path)))
              (obsidian-file (concat sync0-obsidian-dir current-file ".md")) 
              (obsidian-entry (concat "---\n"
                                      "zettel_type: " zettel-type "\n"
                                      "created: " creation "\n"
                                      "title: " title "\n"
                                      "aliases: [" title "]\n"
                                      "tags: [" zettel-type
                                      (unless (null tags)
                                        (concat "," tags)) 
                                      "]\n"
                                      "---\n" 
                                      "# " title "\n")))
      (unless (file-exists-p obsidian-file)
        (with-temp-buffer 
          (insert obsidian-entry)
          (insert contents)
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let* ((mdata (match-data))
                   (raw-orgid (match-string-no-properties 1))
                   (orgid (substring raw-orgid 3 nil)) 
                   (desc (match-string 2)))
         (unless (string-prefix-p "file:" raw-orgid)
              (when-let* ((path
                           (caar
                            (org-roam-db-query [:select [file]
                                                        :from nodes
                                                        :where (= id $s1)]
                                               orgid)))
                          (filename (when (string-match "^.+/\\([0-9]+\\)\\.org$" path)
                                      (match-string-no-properties 1 path))))
                (set-match-data mdata)
                (replace-match
                 (concat "[" desc "](" filename ".md)"))))))
          (write-file obsidian-file))))))
 
(defun sync0-obsidian-migrate-reference ()
  "Convert the text open in the present buffer from Org-roam format
to Obsian markdown using my custom Zettel format."
  (interactive)
  (when (equal (org-entry-get 1 "ZETTEL_TYPE") "reference")
    (let*    ((zettel-type "reference")
              (title        (cadar (org-collect-keywords '("TITLE"))))
              (subtitle (if (org-collect-keywords '("SUBTITLE"))
                            (cadar (org-collect-keywords '("SUBTITLE")))
                          title))
              (title-fixed (if (equal title subtitle)
                               title
                             (concat title " : " subtitle)))
              (end (progn
                     (re-search-forward "^#\\+FILETAGS: .+$" nil t)
                     (match-end 0)))
              (contents (buffer-substring-no-properties (1+ end) (point-max)))
              (tags-raw (org-collect-keywords '("FILETAGS")))
              (tags-line (unless (null tags-raw)
                           (substring (cadar tags-raw) 1 -1)))
              (tags (unless (null tags-raw)
                      (when (string-match ":" tags-line)
                        (replace-regexp-in-string ":" "," tags-line))))
              (author (cadar (org-collect-keywords '("AUTHOR"))))
              (citekey (substring (org-entry-get 1 "ROAM_REFS") 5 nil))
              ;; (creation (org-entry-get 1 "CREATED"))
              (date (org-entry-get 1 "DATE"))
              (origdate (if (org-entry-get 1 "ORIG_DATE")
                            (org-entry-get 1 "ORIG_DATE")
                          date))
              (date-fixed (if (equal date origdate)
                              (concat "(" date ")")
                            (concat "(" origdate ") (" date ")")))
              (biblatex-type (when (org-entry-get 1 "BIBLATEX_TYPE")
                               (org-entry-get 1 "BIBLATEX_TYPE")))
              (url (when (org-entry-get 1 "WEBSITE")
                     (org-entry-get 1 "WEBSITE")))
              (language (when (org-entry-get 1 "LANGUAGE")
                          (org-entry-get 1 "LANGUAGE")))
              (obsidian-file (concat sync0-obsidian-dir citekey ".md")) 
              (obsidian-entry (concat "---\n"
                                      "zettel_type: reference\n"
                                      "citekey: " citekey "\n"
                                      "biblatex_type: " biblatex-type "\n"
                                      "title: " title "\n"
                                      (unless (equal title subtitle)
                                        (concat "subtitle: " subtitle "\n"))
                                      "authors: [" author "]\n"
                                      "parent:\n" 
                                      "aliases: [" author " " date-fixed  " " title-fixed "]\n"
                                      "url: " url "\n"
                                      (unless (equal date origdate)
                                        (concat "origdate: " origdate "\n"))
                                      "date: " date "\n"
                                      "language: " language "\n"
                                      "media:\n"
                                      "trace:\n"
                                      "tags: ["
                                      (unless (null tags-raw)
                                        tags) 
                                      "]\n"
                                      "---\n" 
                                      "# " author " " date-fixed " " title-fixed "\n"
                                      "## Description\n" 
                                      "## Progrès de la lecture\n" 
                                      "## Annotations\n")))
      (if (file-exists-p obsidian-file)
          (message "Error: %s.md file already present in Obsidian vault." citekey)
        (with-temp-buffer 
          (insert obsidian-entry)
          (goto-char (point-max))
          (insert contents)
          (goto-char (point-min))
          (while (re-search-forward org-link-bracket-re nil t)
            (let* ((mdata (match-data))
                   (raw-orgid (match-string-no-properties 1))
                   (orgid (substring raw-orgid 3 nil)) 
                   (desc (match-string 2)))
              (unless (string-prefix-p "file:" raw-orgid)
                (when-let* ((path
                             (caar
                              (org-roam-db-query [:select [file]
                                                          :from nodes
                                                          :where (= id $s1)]
                                                 orgid)))
                            (filename (when (string-match "^.+/\\([0-9]+\\)\\.org$" path)
                                        (match-string-no-properties 1 path))))
                  (set-match-data mdata)
                  (replace-match
                   (concat "[" desc "](" filename ".md)"))))))
          (write-file obsidian-file))))))

(defun sync0-obsidian-migrate-org-links-to-markdown ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward org-link-bracket-re nil t)
    (let* ((mdata (match-data))
           (raw-orgid (match-string-no-properties 1))
           (orgid (substring raw-orgid 3 nil)) 
           (desc (match-string 2))
           (filename (when (string-match "^.+/\\([0-9]+\\)\\.org$" raw-orgid)
                       (match-string-no-properties 1 raw-orgid))))
      (set-match-data mdata)
      (replace-match
       (concat "[" desc "](" filename ".md)")))))

(defun sync0-obsidian-migrate-org-images-to-markdown ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward org-link-bracket-re nil t)
    (let* ((mdata (match-data))
           (raw-orgid (match-string-no-properties 1))
           ;; (orgid (substring raw-orgid 3 nil)) 
           (desc (match-string 2))
           (filename (when (string-match "^.+/\\([[:alnum:]_-]+\\)\\.png$" raw-orgid)
                       (match-string-no-properties 1 raw-orgid))))
      (set-match-data mdata)
      (replace-match
       (concat "![" desc "](" filename ".png)")))))

;; (defun sync0-obsidian-migrate-reference ()
;;   "Convert the text open in the present buffer from Org-roam format
;; to Obsian markdown using my custom Zettel format."
;;   (interactive)
;;   (when (equal (org-entry-get 1 "ZETTEL_TYPE") "reference")
;;     (let*    ((end (progn
;;                      (re-search-forward "^#\\+FILETAGS: .+$" nil t)
;;                      (match-end 0)))
;;               (contents (buffer-substring-no-properties (1+ end) (point-max)))
;;               (current-path (buffer-file-name))
;;               (current-file (when (string-match "^.+/\\([0-9]+\\)\\.org$" current-path)
;;                               (match-string-no-properties 1 current-path)))
;;               (obsidian-file (concat sync0-obsidian-dir current-file ".md"))) 
;;       (when (file-exists-p obsidian-file)
;;         (with-temp-buffer 
;;           (insert-file-contents obsidian-file)
;;           (goto-char (point-max))
;;           (insert contents)
;;           (goto-char (point-min))
;;           (while (re-search-forward org-link-bracket-re nil t)
;;             (let* ((mdata (match-data))
;;                    (raw-orgid (match-string-no-properties 1))
;;                    (orgid (substring raw-orgid 3 nil)) 
;;                    (desc (match-string 2)))
;;          (unless (string-prefix-p "file:" raw-orgid)
;;               (when-let* ((path
;;                            (caar
;;                             (org-roam-db-query [:select [file]
;;                                                         :from nodes
;;                                                         :where (= id $s1)]
;;                                                orgid)))
;;                           (filename (when (string-match "^.+/\\([0-9]+\\)\\.org$" path)
;;                                       (match-string-no-properties 1 path))))
;;                 (set-match-data mdata)
;;                 (replace-match
;;                  (concat "[" desc "](" filename ".md)"))))))
;;           (write-file obsidian-file))))))


(provide 'sync0-test-functions)

;; Step 3: Replace all file links with id links where possible
;; (defun org-roam-replace-file-links-with-id ()
;;   (org-with-point-at 1
;;     (while (re-search-forward org-link-bracket-re nil t)
;;       (let* ((mdata (match-data))
;;              (path (match-string 1))
;;              (desc (match-string 2)))
;;         (when (string-prefix-p "file:" path)
;;           (setq path (expand-file-name (substring path 5)))
;;           (when-let ((node-id (caar (org-roam-db-query [:select [id] :from nodes
;;                                                         :where (= file $s1)
;;                                                         :and (= level 0)] path)) ))
;;             (set-match-data mdata)
;;             (replace-match (org-link-make-string (concat "id:" node-id) desc))))))))

;; (dolist (f (org-roam--list-all-files))
;;   (with-current-buffer (find-file-noselect f)
;;     (org-roam-replace-links-with-id)))


;; (dolist (f (f-files sync0-obsidian-dir
;;                     (lambda (k) (string-match-p ".md" k)) t))
;;   (with-current-buffer (find-file-noselect f)
;;     (sync0-obsidian-migrate-org-images-to-markdown)))

;; (dolist (f (f-files sync0-obsidian-dir
;;                     (lambda (k) (string-match-p ".md" k)) t))
;;   (with-current-buffer (find-file-noselect f)
;;     (sync0-obsidian-correct-biblatex)))

;; (dolist (f (f-files sync0-obsidian-dir
;;                     (lambda (k) (string-match-p ".md" k)) t))
;;   (with-current-buffer (find-file-noselect f)
;;     (sync0-obsidian-correct-bibkeys)))

;; (dolist (f (f-files sync0-zkn-dir
;;                     (lambda (k) (string-match-p "permanent" k)) t))
;;   (with-current-buffer (find-file-noselect f)
;;     (sync0-obsidian-migrate-reference)))

(defun org+-tangle-table-at-point ()
  "Tangle Org table at point.
The table is ignored if it is not preceeded by a line like:
#+ATTR_TANGLE: DATAFILE
or
#+ATTR_TANGLE: (\"DATAFILE\" \"TYPE\")"
  (interactive)
  (unless (org-at-table-p)
    (user-error "Not at Org table"))
  (let* (type
         (data (org-element-context))
         (parent (org-element-property :parent data))
         file)
    (while parent
      (setq data parent)
      (setq parent (org-element-property :parent data)))
    (when (and (setq prop (org-element-property :attr_tangle data))
               (setq file (read prop)))
      (cond
       ((symbolp file)
        (setq file (symbol-name file)))
       ((stringp file)) ;; keep it that way
       ((consp file)
        (setq type (nth 1 file)
              file (car file)))
       (t
        (user-error "Unexpected format of table to be tangled")))
      (org-table-export file (or type org-table-export-default-format))
      t)))

(defun org+-tangle-tables (fun &optional arg target-file lang)
  "Tangle Org tables in current buffer.
For around advice with `org-babel-tangle' as FUN.
See `org-babel-tangle' for the args ARG, TARGET-FILE and LANG.

Currently, only one table per file is possible."
  (cond
   ((equal arg '(4)) ;; at point
    (if (org-at-table-p)
        (org+-tangle-table-at-point)
      (funcall fun arg target-file lang)))
   ((equal arg '(16))
    (funcall fun arg target-file lang))
   (t
    (funcall fun arg target-file lang)
    (let ((table-count 0))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^[ \t\r]*|" nil t)
         (goto-char (match-beginning 0))
         (when (org+-tangle-table-at-point)
           (cl-incf table-count))
         (goto-char (org-table-end)))
       )
      (message "Tangled %d tables from %s." table-count (buffer-name))))))

(advice-add 'org-babel-tangle :around #'org+-tangle-tables)

(provide 'sync0-test-funcitions)
