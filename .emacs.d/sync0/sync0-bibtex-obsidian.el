(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-corrections)
(require 'sync0-zettelkasten)

;; (require 'sync0-yaml)

;; (defun sync0-bibtex-transplant-obsidian-ref-into-biblatex ()
;;   "Create new BibLaTeX entry in the default bibliography. When
;;    optional quick is non-nil, only capture the minimal fields
;;    required to create a new entry."
;;   (interactive)
;;   ;; Before calculating any values, reset all values in my
;;   ;; master list of variable (set them to nil).
;;   ;; (setq sync0-bibtex-entry-derivation (yes-or-no-p "Derive entry?"))
;;   (setq sync0-bibtex-entry-creation t)
;;   (setq sync0-bibtex-entry-file-old nil)
;;   (setq sync0-bibtex-entry-keywords nil)
;;   ;; (sync0-bibtex-completion-load-entry nil quick)
;;   (let* ((obsidian-id (read-string "Which Obsidian file to use as base for new entry?"))
;;          (obsidian-file (concat sync0-zkn-references-dir obsidian-id ".org"))
;;          (obsidian-file-string (f-read-text obsidian-file))
;;          x)
;;     (if (file-exists-p obsidian-file)
;;         (progn
;;           (when (sync0-bibtex-duplicate-entry-key-p obsidian-id)
;;             (setq obsidian-id (sync0-bibtex-entry-key-define t)))
;;           (setq sync0-bibtex-entry-key obsidian-id)  
;;           (setq sync0-bibtex-entry-keywords (sync0-yaml-get-property "tags" obsidian-file-string))
;;           (setq sync0-bibtex-entry-author (sync0-yaml-get-property "author" obsidian-file-string))
;;           (setq sync0-bibtex-entry-date (sync0-yaml-get-property "date" obsidian-file-string))
;;           (setq sync0-bibtex-entry-origdate (sync0-yaml-get-property "origdate" obsidian-file-string))
;;           (setq sync0-bibtex-entry-publisher (sync0-yaml-get-property "publisher" obsidian-file-string))
;;           (setq sync0-bibtex-entry-doctype (sync0-yaml-get-property "doctype" obsidian-file-string))
;;           (setq sync0-bibtex-entry-title (sync0-yaml-get-property "title" obsidian-file-string))
;;           (setq sync0-bibtex-entry-journaltitle (sync0-yaml-get-property "journaltitle" obsidian-file-string))
;;           (setq sync0-bibtex-entry-language (sync0-yaml-get-property "language" obsidian-file-string))
;;           (setq sync0-bibtex-entry-langid sync0-bibtex-entry-language)
;;           (setq sync0-bibtex-entry-type-downcase (sync0-yaml-get-property "biblatex_type" obsidian-file-string))
;;           (setq sync0-bibtex-entry-type (if (member sync0-bibtex-entry-type-downcase sync0-bibtex-entry-types-correction)
;;                                             (let (beg (upcase-initials (substring sync0-bibtex-entry-type-downcase 0 3)))
;;                                               (end (upcase-initials (substring sync0-bibtex-entry-type-downcase 4 nil)))
;;                                               (concat beg end))
;;                                           (upcase-initials sync0-bibtex-entry-type-downcase)))
;;           (sync0-bibtex-entry-append-to-bibliography sync0-bibtex-entry-key))
;;       (message "No Obsidian file found for %s" obsidian-id))))

  (defun sync0-bibtex-entry-inform-new-note (&optional rewrite)
    "Inform the user about a new entry that has been just created."
    (cond ((and rewrite
                (sync0-null-p sync0-bibtex-entry-author))
           (message "Note for %s %s with key %s has been updated." sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key))
          (rewrite 
           (message "Note for %s %s %s with key %s has been updated." sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key))
          (t (if (sync0-null-p sync0-bibtex-entry-author)
                 (message "New note created for %s %s with key %s" sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)
               (message "New note created for %s %s %s with key %s" sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)))))

  (defun sync0-bibtex-entry-calculate-obsidian-title-aliases ()
    "Produce a list of titles to be used in obsidian note aliases."
    (let* ((title (concat "/" sync0-bibtex-entry-title-fixed "/"))
           (altertitle (concat "/" sync0-bibtex-entry-title-aliases "/"))
           (title-date (when sync0-bibtex-entry-date-or-origdate-p
                         (concat title " " sync0-bibtex-entry-date-fixed)))
           (title-journal-date (when (and  sync0-bibtex-entry-date-or-origdate-p
                                           sync0-bibtex-entry-journaltitle)
                         (concat title " (" sync0-bibtex-entry-journaltitle  ") " sync0-bibtex-entry-date-fixed)))
           (date-title (when sync0-bibtex-entry-date-or-origdate-p
                         (concat sync0-bibtex-entry-date-fixed " " title)))
           (date-title-journal (when (and sync0-bibtex-entry-date-or-origdate-p
                                           sync0-bibtex-entry-journaltitle)
                         (concat sync0-bibtex-entry-date-fixed " (" sync0-bibtex-entry-journaltitle ") ")))
           (title-edition (unless (sync0-null-p  sync0-bibtex-entry-edition)
                            (concat title " (" sync0-bibtex-entry-edition "e)")))
           (author-title-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                             (not (sync0-null-p sync0-bibtex-entry-edition)))
                                   (concat sync0-bibtex-entry-lastname ", " title-edition)))
           (author-title-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-date-or-origdate-p)
                                (concat sync0-bibtex-entry-lastname " " date-title)))
           (author-title (when (and  sync0-bibtex-entry-author-or-editor-p
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " title)))
           (author-altertitle (when (and  sync0-bibtex-entry-author-or-editor-p
                                          (or (null sync0-bibtex-entry-date-or-origdate-p)
                                              (null sync0-bibtex-entry-edition)))
                                (concat sync0-bibtex-entry-lastname ", " altertitle)))
           (author-date (unless (or (string= sync0-bibtex-entry-type-downcase "incollection")
                                    (string= sync0-bibtex-entry-type-downcase "inproceedings")
                                    (string= sync0-bibtex-entry-type-downcase "inbook"))
                          (when (and  sync0-bibtex-entry-author-or-editor-p
                                      sync0-bibtex-entry-date-or-origdate-p)
                            (if (and sync0-bibtex-entry-volume 
                                     (not (string= sync0-bibtex-entry-type-downcase "article")))
                                (concat sync0-bibtex-entry-lastname " " sync0-bibtex-entry-date-fixed ", T. " sync0-bibtex-entry-volume)
                              (concat sync0-bibtex-entry-lastname " " sync0-bibtex-entry-date-fixed)))))
           (title-to-use (list title
                               altertitle
                               title-date
                               title-journal-date
                               date-title
                               date-title-journal
                               title-edition
                               author-date
                               author-title-edition
                               author-title-date
                               author-title
                               author-altertitle
			       ))
           (purged-title-list (if (equal sync0-bibtex-entry-title-shape "title")
                                  (cl-remove nil title-to-use)
                                (cl-remove nil (cdddr title-to-use))))
           (title-corrected (mapcar 'sync0-bibtex-fix-obsidian-chars purged-title-list)))
           (cl-remove-duplicates title-corrected :test #'equal)))

  (defun sync0-bibtex-entry-create-obsidian-note-from-entry (bibkey &optional rewrite)
    "Create new markdown note for corresponding bibkey in default
obsidian vault. This function in not intended for interactive
use, but as a function to be included in pipes. When optional
rewrite is true, this function rewrites the YAML frontmatter of
the note, instead of attempting to create a new note."
    (let* ((obsidian-file (concat sync0-zkn-references-dir bibkey ".org")) 
           (title-automated-list (sync0-bibtex-entry-calculate-obsidian-title-aliases))
	   (my-aliases (when sync0-bibtex-entry-aliases
			 (if (string-match-p "\", \"" sync0-bibtex-entry-aliases)
                             (mapcar (lambda (alias) (replace-regexp-in-string "^\"?\\(.*?\\)\"?$" "\\1" alias))
				     (split-string sync0-bibtex-entry-aliases "\", \""))
			   (list (replace-regexp-in-string "^\"\\(.*?\\)\"$" "\\1" sync0-bibtex-entry-aliases)))))
	   (title-list (if my-aliases
			   (append my-aliases title-automated-list)
			 title-automated-list))
	   (title-list-fixed-as-list  (mapcar (lambda (x) (concat "\"" x "\"")) title-list))
           (title-list-fixed (concat "  - " (sync0-show-elements-of-list title-list-fixed-as-list "\n  - ")))
           (obsidian-fields-string (let (x)
                                     (dolist (element sync0-bibtex-obsidian-fields-list x)
                                       (let ((field (car element))
                                             (opener (cadr element))
                                             (closer (caddr element))
                                             (variable (cadddr element)))
                                         (when-let ((value (sync0-bibtex-fix-obsidian-chars (eval variable))))
                                           (push (concat field ": " opener value closer) x))))
                                     (delete-duplicates x :test #'string=)
                                     (sync0-show-elements-of-list x "\n")))
           (keywords-corrected
            (let ((x (sync0-string-split-with-sep-and-list sync0-bibtex-entry-keywords ", ")))
              (concat "  - " (sync0-show-elements-of-list x "\n  - "))))
           (obsidian-yaml (concat ":PROPERTIES:\n"
                                  ":ZTYPE: reference\n"
                                  "id: " bibkey "\n"
                                  "ROAM_REFS: @" bibkey "\n"
                                  "BIBTYPE: " (downcase sync0-bibtex-entry-type)  "\n"
                                  obsidian-fields-string
                                  (concat "\n:ROAM_ALIASES: " title-list-fixed "\n")
                                  "tags:\n  - bibkey/" bibkey "\n" keywords-corrected "\n"
                                  ":END:\n"
                                  ;; (concat "# " sync0-bibtex-entry-obsidian-title "\n")
				  (when sync0-bibtex-entry-author-or-editor-p)
				  (if sync0-bibtex-entry-p)

                                  (concat "#+AUTHOR: "  "\n"))
                                  (concat "#+DATE: " sync0-bibtex-entry-key "\n")
                                  (concat "#+TITLE: " sync0-bibtex-entry-key "\n")
                                  (concat "#+SUBTITLE: " sync0-bibtex-entry-key "\n")
				  ))
           (obsidian-rest (concat
                           sync0-bibtex-obsidian-reference-template-top
                           bibkey
                           sync0-bibtex-obsidian-reference-template-bottom))
           (obsidian-entry (concat obsidian-yaml obsidian-rest)))
      (cond ((and rewrite
                  (file-exists-p obsidian-file))
             (with-temp-file  obsidian-file
               (insert-file-contents obsidian-file)
               (goto-char (point-min))
               (let* ((regex "^# .+\n")
                      (end (re-search-forward regex nil t 1)))
                 (kill-region (point-min) end)
                 (insert obsidian-yaml)))
             (sync0-bibtex-entry-inform-new-note t))
            ((file-exists-p obsidian-file)
             (message "Error: %s.org already present in Obsidian vault." bibkey))
            (t (with-temp-buffer 
                 (insert obsidian-entry)
                 (write-file obsidian-file))
               (sync0-bibtex-entry-inform-new-note)))))

(provide 'sync0-bibtex-obsidian)
