(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-entry-functions)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-corrections)
(require 'sync0-zettelkasten)

(defun sync0-bibtex-notes-inform-new (&optional rewrite)
  "Inform the user about a new entry that has been just created."
  (cond ((and rewrite
              (sync0-null-p sync0-bibtex-entry-author))
         (message "Note for %s %s with key %s has been updated." sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key))
        (rewrite 
         (message "Note for %s %s %s with key %s has been updated." sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key))
        (t (if (sync0-null-p sync0-bibtex-entry-author)
               (message "New note created for %s %s with key %s" sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)
             (message "New note created for %s %s %s with key %s" sync0-bibtex-entry-lastname sync0-bibtex-entry-date-fixed sync0-bibtex-entry-title-fixed sync0-bibtex-entry-key)))))

(defun sync0-bibtex-notes-calculate-title-aliases ()
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
                             author-altertitle))
         (purged-title-list (if (equal sync0-bibtex-entry-title-shape "title")
                                (cl-remove nil title-to-use)
                              (cl-remove nil (cdddr title-to-use))))
         (title-corrected (mapcar 'sync0-bibtex-fix-obsidian-chars purged-title-list)))
    (cl-remove-duplicates title-corrected :test #'equal)))

;; (defun sync0-bibtex-note-format-note ()
;;   "Create new markdown note for corresponding bibkey in default
;; obsidian vault. This function in not intended for interactive
;; use, but as a function to be included in pipes. When optional
;; rewrite is true, this function rewrites the YAML frontmatter of
;; the note, instead of attempting to create a new note."
;;   (sync0-bibtex-completion-load-entry sync0-bibtex-entry-key)
;;   (let* ((title-list (sync0-bibtex-notes-calculate-title-aliases))
;; 	 (title-list-fixed-as-list  (mapcar (lambda (x) (concat "\"" x "\"")) title-list))
;;          (title-list-fixed (sync0-show-elements-of-list title-list-fixed-as-list " "))
;; 	 (obsidian-yaml (concat ":PROPERTIES:\n"
;; 				 ":ZTYPE: reference\n"
;; 				 ":ID: " sync0-bibtex-entry-key "\n"
;; 				 ":ROAM_REFS: @" sync0-bibtex-entry-key "\n"
;; 				 ":BIBTYPE: " sync0-bibtex-entry-type-downcase "\n"
;; 				 ":ROAM_ALIASES: " title-list-fixed "\n"
;; 				 ":END:\n"
;; 				 (when sync0-bibtex-entry-author-or-editor-p
;; 				   (if (string= sync0-bibtex-entry-author-or-editor-priority "author")
;; 				       (concat "#+AUTHOR: " sync0-bibtex-entry-author " \n")
;; 				     (concat "#+AUTHOR: " sync0-bibtex-entry-editor "\n")))
;; 				 (when sync0-bibtex-entry-date-or-origdate-p
;; 				   (if sync0-bibtex-entry-origdate
;; 				       (concat 
;; 					"#+DATE: " sync0-bibtex-entry-date "\n"
;; 					"#+ORIGDATE: " sync0-bibtex-entry-origdate "\n")
;; 				     (concat "#+DATE: " sync0-bibtex-entry-date "\n")))
;; 				 "#+TITLE: " sync0-bibtex-entry-title "\n"
;; 				 (when sync0-bibtex-entry-subtitle
;; 				   "#+SUBTITLE: " sync0-bibtex-entry-subtitle "\n"))))
;; 	 (concat obsidian-yaml sync0-bibtex-notes-template)))

(defun sync0-bibtex-note-format-note ()
  "Create new markdown note for corresponding bibkey in default
obsidian vault. This function in not intended for interactive
use, but as a function to be included in pipes. When optional
rewrite is true, this function rewrites the YAML frontmatter of
the note, instead of attempting to create a new note."
  (sync0-bibtex-completion-load-entry sync0-bibtex-entry-key)
  (let* ((title
          (cond
           ;; If both author/editor and date are present
           ((and sync0-bibtex-entry-author-or-editor-p sync0-bibtex-entry-date-or-origdate-p)
            (concat sync0-bibtex-entry-lastname ", " sync0-bibtex-entry-title-fixed
                    " " sync0-bibtex-entry-date-fixed))
           ;; If only author/editor is present
           (sync0-bibtex-entry-author-or-editor-p
            (concat sync0-bibtex-entry-lastname ", " sync0-bibtex-entry-title-fixed))
           ;; If only date is present
           (sync0-bibtex-entry-date-or-origdate-p
            (concat sync0-bibtex-entry-title-fixed " " sync0-bibtex-entry-date-fixed))
           ;; If neither is present
           (t
            sync0-bibtex-entry-title-fixed)))
          (org-properties-drawer (concat ":PROPERTIES:\n"
				 ":ZTYPE: reference\n"
				 ":BIBTYPE: " sync0-bibtex-entry-type-downcase "\n"
				 ":ID: " sync0-bibtex-entry-key "\n"
				 ":ROAM_REFS: @" sync0-bibtex-entry-key "\n"
				 (when sync0-bibtex-entry-doctype
				   (concat ":DOCTYPE: " sync0-bibtex-entry-doctype "\n"))
				 (when sync0-bibtex-entry-date
				   (concat ":DATE: " sync0-bibtex-entry-origdate "\n"))
				 (when sync0-bibtex-entry-origdate
				   (concat ":ORIGDATE: " sync0-bibtex-entry-origdate "\n"))
				 (when sync0-bibtex-entry-edition
				   (concat ":EDITION: " sync0-bibtex-entry-edition "\n"))
				 (when sync0-bibtex-entry-journaltitle
				   (concat ":JOURNALTITLE: " sync0-bibtex-entry-journaltitle "\n"))
				 (when sync0-bibtex-entry-crossref
				   (concat ":CROSSREF: " sync0-bibtex-entry-crossref "\n"))
				 (when sync0-bibtex-entry-related
				   (concat ":RELATED: " sync0-bibtex-entry-related "\n"))
				 (when sync0-bibtex-entry-relatedtype
				   (concat ":RELATEDTYPE: " sync0-bibtex-entry-relatedtype "\n"))
				 ":END:\n"))
;; 	  (org-keywords (concat "#+TITLE: " title "\n"
;; 				 (when sync0-bibtex-entry-subtitle
;; 				   (concat "#+SUBTITLE: " sync0-bibtex-entry-subtitle "\n"))))
	  (org-keywords (concat "#+TITLE: " title "\n")))
	 (concat org-properties-drawer org-keywords sync0-bibtex-notes-template)))

;; (defun sync0-bibtex-note-add-aliases (bibkey)
;;   "Create new markdown note for corresponding bibkey in default
;; obsidian vault. This function in not intended for interactive
;; use, but as a function to be included in pipes. When optional
;; rewrite is true, this function rewrites the YAML frontmatter of
;; the note, instead of attempting to create a new note."
;;   (sync0-bibtex-completion-load-entry bibkey)
;;   (let* ((title-list (sync0-bibtex-entry-calculate-obsidian-title-aliases))
;; 	 (title-list-fixed-as-list  (mapcar (lambda (x) (concat "\"" x "\"")) title-list))
;;          (title-list-fixed (concat (sync0-show-elements-of-list title-list-fixed-as-list " "))))
;;     (org-set-property "ROAM_ALIASES" title-list-fixed)))

(provide 'sync0-bibtex-notes)
