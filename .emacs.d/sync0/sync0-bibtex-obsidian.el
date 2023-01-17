(require 'sync0-bibtex-corrections)

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
    "Calculate titles list to be used in obsidian notes."
    (let* ((title (concat "*" sync0-bibtex-entry-title-fixed "*"))
           (altertitle (concat "*" sync0-bibtex-entry-title-aliases "*"))
           (shorttitle (when sync0-bibtex-entry-shorttitle
                          (concat (sync0-bibtex-entry-select-draft-prefix)
                                  "*"
                                (sync0-bibtex-fix-obsidian-chars
                                  sync0-bibtex-entry-shorttitle)
                                  "*")))
           (shorthand (when sync0-bibtex-entry-shorthand
                        (concat (sync0-bibtex-entry-select-draft-prefix)
                                "*"
                                (sync0-bibtex-fix-obsidian-chars
                                 sync0-bibtex-entry-shorthand)
                                "*")))
           (shorttitle-date (when (and sync0-bibtex-entry-date-or-origdate-p
                                       sync0-bibtex-entry-shorttitle)
                              (concat shorttitle " " sync0-bibtex-entry-date-fixed)))
           ;; (shorthand-date (when (and sync0-bibtex-entry-date-or-origdate-p
           ;;                             sync0-bibtex-entry-shorthand)
           ;;                    (concat shorthand " " sync0-bibtex-entry-date-fixed)))
           (title-date (when sync0-bibtex-entry-date-or-origdate-p
                         (concat title " " sync0-bibtex-entry-date-fixed)))
           (date-title (when sync0-bibtex-entry-date-or-origdate-p
                         (concat sync0-bibtex-entry-date-fixed " " title)))
           (date-shorttitle (when (and sync0-bibtex-entry-date-or-origdate-p
                                       sync0-bibtex-entry-shorttitle)
                         (concat sync0-bibtex-entry-date-fixed " " shorttitle)))
           ;; (date-shorthand (when (and sync0-bibtex-entry-date-or-origdate-p
           ;;                             sync0-bibtex-entry-shorthand)
           ;;               (concat sync0-bibtex-entry-date-fixed " " shorthand)))
           (altertitle-date (when sync0-bibtex-entry-date-or-origdate-p
                              (concat altertitle " " sync0-bibtex-entry-date-fixed)))
           (date-altertitle (when sync0-bibtex-entry-date-or-origdate-p
                              (concat sync0-bibtex-entry-date-fixed " " altertitle)))
           (altertitle-edition (unless (sync0-null-p  sync0-bibtex-entry-edition)
                                 (concat altertitle " (" sync0-bibtex-entry-edition "e)")))
           (title-edition (unless (sync0-null-p  sync0-bibtex-entry-edition)
                            (concat title " (" sync0-bibtex-entry-edition "e)")))
           (shorttitle-edition (when (and shorttitle
                                          sync0-bibtex-entry-edition)
                                 (concat shorttitle " (" sync0-bibtex-entry-edition "e)")))
           ;; (shorthand-edition (when (and shorthand
           ;;                                sync0-bibtex-entry-edition)
           ;;                       (concat shorthand " (" sync0-bibtex-entry-edition "e)")))
           (author-title-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                             (not (sync0-null-p sync0-bibtex-entry-edition)))
                                   (concat sync0-bibtex-entry-lastname ", " title-edition)))
           (author-shorttitle-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                                  sync0-bibtex-entry-shorttitle
                                             (not (sync0-null-p sync0-bibtex-entry-edition)))
                                   (concat sync0-bibtex-entry-lastname ", " shorttitle-edition)))
           ;; (author-shorthand-edition (when (and  sync0-bibtex-entry-author-or-editor-p
           ;;                                        sync0-bibtex-entry-shorthand
           ;;                                   (not (sync0-null-p sync0-bibtex-entry-edition)))
           ;;                         (concat sync0-bibtex-entry-lastname ", " shorthand-edition)))
           (author-altertitle-edition (when (and  sync0-bibtex-entry-author-or-editor-p
                                                  (not (sync0-null-p sync0-bibtex-entry-edition)))
                                        (concat sync0-bibtex-entry-lastname ", " altertitle-edition)))
           (author-title-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-date-or-origdate-p)
                                (concat sync0-bibtex-entry-lastname " " date-title)))
           (author-shorttitle-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                               sync0-bibtex-entry-shorttitle
                                          sync0-bibtex-entry-date-or-origdate-p)
                                (concat sync0-bibtex-entry-lastname " " date-shorttitle)))
           ;; (author-shorthand-date (when (and  sync0-bibtex-entry-author-or-editor-p
           ;;                                     sync0-bibtex-entry-shorthand
           ;;                                sync0-bibtex-entry-date-or-origdate-p)
           ;;                      (concat sync0-bibtex-entry-lastname " " date-shorthand)))
           (author-altertitle-date (when (and  sync0-bibtex-entry-author-or-editor-p
                                               sync0-bibtex-entry-date-or-origdate-p)
                                     (concat sync0-bibtex-entry-lastname " " date-altertitle)))
           (author-title (when (and  sync0-bibtex-entry-author-or-editor-p
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " title)))
           (author-shorttitle (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-shorttitle
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " shorttitle)))
           (author-shorthand (when (and  sync0-bibtex-entry-author-or-editor-p
                                          sync0-bibtex-entry-shorthand
                                     (or (null sync0-bibtex-entry-date-or-origdate-p)
                                         (null sync0-bibtex-entry-edition)))
                           (concat sync0-bibtex-entry-lastname ", " shorthand)))
           (author-altertitle (when (and  sync0-bibtex-entry-author-or-editor-p
                                          (or (null sync0-bibtex-entry-date-or-origdate-p)
                                              (null sync0-bibtex-entry-edition)))
                                (concat sync0-bibtex-entry-lastname ", " altertitle)))
           (title-to-use (list title
                               altertitle
                               shorttitle
                               shorthand
                               title-date
                               shorttitle-date
                               ;; shorthand-date
                               date-title
                               date-shorttitle
                               altertitle-date
                               date-altertitle
                               altertitle-edition
                               title-edition
                               shorttitle-edition
                               ;; shorthand-edition
                               author-title-edition
                               author-shorttitle-edition
                               ;; author-shorthand-edition
                               author-altertitle-edition
                               author-title-date
                               author-shorttitle-date
                               ;; author-shorthand-date
                               author-altertitle-date
                               author-title
                               author-shorttitle
                               author-shorthand
                               author-altertitle))
           (purged-title-list (if (equal sync0-bibtex-entry-title-shape "title")
                                  (cl-remove nil title-to-use)
                                (cl-remove nil (cdddr title-to-use))))
           (title-list (cl-remove-duplicates purged-title-list :test #'equal)))
       (sync0-show-elements-of-list title-list "\", \"")))

  (defun sync0-bibtex-entry-create-obsidian-note-from-entry (bibkey &optional rewrite)
    "Create new markdown note for corresponding bibkey in default
obsidian vault. This function in not intended for interactive
use, but as a function to be included in pipes. When optional
rewrite is true, this function rewrites the YAML frontmatter of
the note, instead of attempting to create a new note."
    (let* ((obsidian-file (concat sync0-zettelkasten-directory bibkey ".md")) 
           (title-automated-list (sync0-bibtex-entry-calculate-obsidian-title-aliases))
           (title-list-string (if sync0-bibtex-entry-aliases 
                                  (concat sync0-bibtex-entry-aliases ", " title-automated-list)
                                title-automated-list))
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
           (obsidian-yaml (concat "---\n"
                                  "zettel_type: reference\n"
                                  "id: " bibkey "\n"
                                  "citekey: " bibkey "\n"
                                  "biblatex_type: " (downcase sync0-bibtex-entry-type)  "\n"
                                  obsidian-fields-string
                                  (concat "\naliases: [\"" title-list-string "\"]\n")
                                  "tags: [bibkey/" bibkey ", " sync0-bibtex-entry-keywords "]\n"
                                  "---\n"
                                  (concat "# " sync0-bibtex-entry-obsidian-title "\n")))
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
             (message "Error: %s.md already present in Obsidian vault." bibkey))
            (t (with-temp-buffer 
                 (insert obsidian-entry)
                 (write-file obsidian-file))
               (sync0-bibtex-entry-inform-new-note)))))

(provide 'sync0-bibtex-obsidian)
