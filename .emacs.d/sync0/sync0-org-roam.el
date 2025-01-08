(use-package org-roam
  :after org
  :custom
  (org-roam-directory sync0-zkn-dir)
  (org-roam-file-extensions '("org" "md")) ; enable Org-roam for a markdown extension
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n f" . org-roam-node-find)
  ;;        ("C-c n g" . org-roam-graph)
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        ("C-c n c" . org-roam-capture)
  ;;        ;; Dailies
  ;;        ("C-c n j" . org-roam-dailies-capture-today))
;;   (org-roam-node-display-template "${title:*} [${file}]")
  :config
  (require 'sync0-zkn-functions)
  (require 'sync0-bibtex-notes)
  (require 'sync0-yaml)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-node-display-template "${title:*} [${file}]")

  (defun sync0-org-roam-generate-file-path (directory)
    "Generate a full file path in DIRECTORY using `sync0-zkn-define-unique-filename-list`.
Appends '.org' extension to the filename."
    (let ((filename (sync0-zkn-define-unique-filename-list directory)))
      (expand-file-name (concat filename ".org") directory)))

;; (defun sync0-org-roam-node-insert-filtered ()
;;   "Insert a reference to a literature or non-literature note in Org-roam.
;; Prompts the user to choose between a literature note or a non-literature note.
;; Filters nodes based on the directory `sync0-zkn-references-dir`."
;;   (interactive)
;;   (let ((references-dir (expand-file-name sync0-zkn-references-dir))
;;         (is-literature (yes-or-no-p "Insert reference to a literature note? ")))
;;     (org-roam-node-insert
;;      (lambda (node)
;;        (let ((file-path (org-roam-node-file node)))
;;          (if is-literature
;;              (string-prefix-p references-dir file-path) ;; Include only literature notes
;;            (not (string-prefix-p references-dir file-path)))))))) 

(defun sync0-org-roam-node-insert-filtered ()
  "Insert a reference to a literature or non-literature note in Org-roam.
Prompts the user to choose between a literature note or a non-literature note.
Filters nodes based on the directory `sync0-zkn-references-dir`."
  (interactive)
  (let ((references-dir (expand-file-name sync0-zkn-references-dir)))
    (org-roam-node-insert
     (lambda (node)
       (let ((file-path (org-roam-node-file node)))
           (not (string-prefix-p references-dir file-path)))))))

(defun sync0-org-roam-node-find-non-literature ()
  "Find and open an Org-roam non-literature node by its title or alias.
Filters out nodes located in the directory `sync0-zkn-references-dir`."
  (interactive)
  (let ((references-dir (expand-file-name sync0-zkn-references-dir)))
    (org-roam-node-find
     nil ;; Do not open in another window
     nil ;; No initial input
     (lambda (node)
       (not (string-prefix-p references-dir (org-roam-node-file node))))))) 

(defun sync0-markdown-cleanup-file-links ()
  "Strip Markdown file links from the current buffer, leaving only the link text.
Pandoc citation links (e.g., [@citation-key]) are spared."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]*\\))" nil t)
      (let ((link (match-string 2)))
        ;; Skip links that are Pandoc citations
        (unless (string-match-p "^@" link)
          (replace-match "\\1"))))))

;; (defun sync0-markdown-to-org-links (org-id)
;;   "Convert Markdown links in the current buffer to Org-mode links using ORG-ID."
;;   (goto-char (point-min))
;;   (while (re-search-forward "\[\([^]]+\)\](\([^)]+\))" nil t)
;;     (let* ((link-text (match-string 1))
;;            (link-target (match-string 2))
;;            (org-link (if (string-match-p "^[0-9a-f]+$" (file-name-base link-target))
;;                          (format "[[id:%s][%s]]" (file-name-base link-target) link-text)
;;                        (format "[[%s][%s]]" link-target link-text))))
;;       (replace-match org-link))))

(defun sync0-markdown-to-org-links ()
  "Convert Markdown file links in the current buffer to Org-mode links using Org IDs.
Markdown links are assumed to have the format [text](filename.md),
and the corresponding Org ID is derived from the filename without the .md extension."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]*\\.md\\))" nil t)
      (let ((link-text (match-string 1))
            (link-target (match-string 2)))
        ;; Convert Markdown file links to Org ID links
        (replace-match (format "[[id:%s][%s]]"
                               (file-name-base link-target)
                               link-text))))))

(defun sync0-markdown-to-org-properties ()
  "Transform YAML front matter in the current buffer into an Org properties drawer."
  (interactive)
  (let ((yaml-data (sync0-parse-yaml-front-matter)))
    (if (not yaml-data)
        (message "No valid YAML front matter found in the buffer.")
      (save-excursion
        (goto-char (point-min))
        ;; Remove YAML front matter from the buffer
        (when (re-search-forward "^---\n" nil t)
          (let ((start (match-beginning 0)))
            (if (re-search-forward "^---\n" nil t)
                (delete-region start (point))
              (message "Invalid YAML front matter format."))))

        ;; Insert properties drawer
        (goto-char (point-min))
        (insert ":PROPERTIES:\n")
        
        ;; Process each key-value pair
        (maphash (lambda (key value)
                   (cond
                    ;; Rename "tags" to "filetags" and remove brackets (handles list or string)
                    ((string= (symbol-name key) "tags")
                     (insert (format ":FILETAGS: %s\n"
                                     (if (listp value)
                                         (mapconcat 'identity value " ")
                                       value))))
                    ;; Rename "aliases" to "roam_aliases" and replace braces with double quotes
                    ((string= (symbol-name key) "aliases")
                     (insert (format ":ROAM_ALIASES: \"%s\"\n"
                                     (if (vectorp value)
                                         (mapconcat 'identity (append value nil) " ")
                                       value))))
                    ;; Remove the "key" property
                    ((string= (symbol-name key) "key")
                     nil)
                    ;; Rename "zettel_type" to "ztype"
                    ((string= (symbol-name key) "zettel_type")
                     (insert (format ":ZTYPE: %s\n" value)))
                    ;; Rename "zettel_subtype" to "zstype"
                    ((string= (symbol-name key) "zettel_subtype")
                     (insert (format ":ZSTYPE: %s\n" value)))
                    ;; Remove the "title" property
                    ((string= (symbol-name key) "title")
                     nil)
                    ;; For other properties, insert as is
                    (t
                     (insert (format ":%s: %s\n" (upcase (symbol-name key)) value)))))
                 yaml-data)
        
        (insert ":END:\n"))
      (message "YAML front matter converted to Org properties drawer."))))

(defun sync0-markdown-to-org-quote-block ()
  "Convert Markdown blockquote lines in the current buffer into Org-mode quote blocks.
For every Markdown quote ('>') line found, it wraps the content into:
#+begin_quote
<content>
#+end_quote"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
            (rx line-start 
                (group-n 1 ">") 
                (zero-or-more space) 
                (group-n 2 (zero-or-more not-newline)))
            nil t)
      (let ((quote (match-string 2)))
        ;; Replace match with Org quote block
        (replace-match (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" quote) t t)))))

(defun sync0-markdown-to-org-citations ()
  "Convert Pandoc citations in the current buffer to Org-mode citation format.
Replaces:
[@citekey, page] -> [cite:@citekey, page]
[@citekey1 ; @citekey2] -> [cite:@citekey1 ; @citekey2]"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
            (rx "[" "@" (group-n 1 (one-or-more not-newline)) "]")
            nil t)
      (let ((citation (match-string 1)))
        (replace-match (format "[cite:@%s]" citation) t t)))))

(defun sync0-markdown-to-org-footnotes ()
  "Convert Markdown footnotes in the current buffer to Org-mode footnotes.
Handles both inline references (e.g., [^1]) and definitions (e.g., [^1]: Footnote text)."
  (interactive)
  (save-excursion
    ;; Convert inline footnote references [^1] -> Org footnote refs [fn:1]
    (goto-char (point-min))
    (while (re-search-forward (rx "[^" (group-n 1 (one-or-more digit)) "]") nil t)
      ;; Ensure we don't match definitions by checking the next character is not ":"
      (unless (looking-at ":")
        (replace-match "[fn:\\1]" t nil)))
    ;; Convert footnote definitions [^1]: -> Org footnote definitions [fn:1]:
    (goto-char (point-min))
    (while (re-search-forward (rx "[^" (group-n 1 (one-or-more digit)) "]:" (zero-or-more space)) nil t)
      (replace-match "[fn:\\1] " t nil))))

(defun sync0-markdown-to-org-emphasis ()
  "Convert Markdown emphasis (*text*) to Org-mode emphasis (/text/) in the current buffer.
Ensures proper handling of visual lines and avoids conflicts with other Markdown syntax."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward 
            (rx (group-n 1 (not (any "*")))
                "*" 
                (group-n 2 (one-or-more (not (any "*\n"))))
                "*" 
                (group-n 3 (not (any "*"))))
            nil t)
      ;; Replace *text* with /text/
      (replace-match "\\1/\\2/\\3" t nil))))

(defun sync0-markdown-to-org-headlines ()
  "Convert Markdown headings in the current buffer to Org-mode style headlines.
Level 1 Markdown headings (#) are removed. All other headings are reduced by one level."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+\\)\\s-+" nil t)
      (let ((heading-level (length (match-string 1))))
        (if (= heading-level 1)
            ;; Remove level 1 Markdown headings
            (replace-match "" t t)
          ;; Convert other headings to Org-mode style
          (replace-match (concat (make-string (1- heading-level) ?*) " ") t t)))))
  (message "Converted Markdown headings to Org-mode style."))

(defun sync0-markdown-to-org-current-buffer (&optional buffer)
  "Run all Markdown-to-Org conversion functions on the specified BUFFER.
If BUFFER is nil, defaults to the current buffer. The functions applied are:
1. Cleaning up Markdown file links.
2. Converting Markdown blockquotes to Org-mode quote blocks.
3. Converting Pandoc citations to Org-mode citation format."
  (interactive)
  (let ((target-buffer (or buffer (current-buffer))))
    (with-current-buffer target-buffer
;;       (sync0-markdown-cleanup-file-links)
      (sync0-markdown-to-org-headlines)
      (sync0-markdown-to-org-links)
      (sync0-markdown-to-org-quote-block)
      (sync0-markdown-to-org-footnotes)
      (sync0-markdown-to-org-emphasis)
      (sync0-markdown-to-org-citations))))

(defun sync0-org-roam-orgify-current-markdown-buffer (&optional buffer)
  "Convert the specified Markdown BUFFER to Org format, replacing the original file.
If BUFFER is nil, defaults to the current buffer. Performs the following operations:
1. Converts Markdown links to Org-mode links using org-id.
2. Transforms YAML frontmatter to an Org properties block.
3. Converts Markdown blockquotes to Org-mode quote blocks.
4. Converts Pandoc-style citations to Org-mode citation format.
5. Converts Markdown footnotes and emphasis to Org-mode equivalents.
6. Deletes the original Markdown file and creates a new Org file with the same name, 
   setting the :ID: property to the file name."
  (interactive)
  (let* ((target-buffer (or buffer (current-buffer)))
         (markdown-file (buffer-file-name target-buffer))
         (org-file (concat (file-name-sans-extension markdown-file) ".org"))
         (org-id (file-name-base markdown-file))
         title)
    (with-current-buffer target-buffer
      (setq title (sync0-yaml-get-property "title"))
      ;; Convert Markdown headlines to org style headlines
      (sync0-markdown-to-org-headlines)
      ;; Convert Markdown links to Org links with org-id
      (sync0-markdown-to-org-links)
      ;; Transform YAML frontmatter to Org properties block
      (sync0-markdown-to-org-properties)
      ;; Perform additional Markdown to Org conversions
      (sync0-markdown-to-org-quote-block)
      (sync0-markdown-to-org-citations)
      (sync0-markdown-to-org-footnotes)
      (sync0-markdown-to-org-emphasis)
      ;; Save the Org file
      (write-file org-file))
      ;; Delete the original Markdown file
      (delete-file markdown-file)
      ;; Add #+TITLE and :ID: property to the new Org file
      (with-temp-buffer
        (org-mode)
        (insert-file-contents org-file)
	(org-with-point-at 1
	  (org-set-property "ID" org-id)
	  (org-roam-set-keyword "TITLE" (or title org-id)))
        (write-file org-file))))

(defun sync0-org-roam-set-directional-link ()
  "Choose a property ('up', 'down', 'left', 'right'), select an Org-roam node,
   and set the chosen property with the Org ID of the selected node."
  (interactive)
  ;; Step 1: Prompt user to select one of the four properties.
  (let* ((properties '("up" "down" "left" "right" "origin"))
         (selected-property (completing-read "Choose property: " properties nil t))
         ;; Step 2: Query Org-roam database for a node using org-roam-node-read.
         (node (org-roam-node-read))
         (node-id (org-roam-node-id node)))
    ;; Step 3: Insert or update the selected property with the Org ID.
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (format "^:%%s*:.*$" selected-property) nil t)
          ;; If property already exists, replace its value.
          (replace-match (format ":%s: %s" (upcase selected-property) node-id))
        ;; If property does not exist, insert it.
        (insert (format ":%s: %s\n" (upcase selected-property) node-id))))
    (message "Set '%s' to Org ID: %s" selected-property node-id)))

(defun sync0-org-roam-find-linked-file ()
  "Search for 'up', 'down', 'left', 'right', or 'origin' properties in the current Org file.
   Present the user with a list of associated files or titles to choose from, and open the selected file.
   Assumes all property values are Org IDs."
  (interactive)
  (let ((properties '("up" "down" "left" "right" "origin"))
        links)
    ;; Step 1: Search for properties and collect their Org ID values.
    (save-excursion
      (goto-char (point-min))
      (dolist (property properties)
	(let ((value (org-entry-get 1 (upcase property))))
          (push (cons property value) links))))

    ;; Step 2: Resolve Org IDs to filenames or mark them as unresolved.
    (setq links (delq nil
                      (mapcar (lambda (entry)
                                (let* ((property (car entry))
                                       (org-id (cdr entry))
                                       (file (car (org-roam-id-find org-id))) ;; Resolve ID to file
                                       (title-or-filename
					(when file
					  (cond 
                                           ((and (string-match "\\.org$" file)
						 (file-exists-p file))
					    (sync0-org-get-keyword (sync0-get-or-create-buffer-for-file file) "TITLE"))
                                           ((and (string-match "\\.md$" file)
						 (file-exists-p file))
					    (car (sync0-db-extract-title-and-aliases file)))
                                           (t
                                            ;; For non-Org files, just use filename.
                                            (file-name-nondirectory file))))))
				       (when title-or-filename 
                                  (cons (format "%s: %s" (upcase property) title-or-filename) file))))
                              links)))
(if links
        (let* ((choices (mapcar #'car links))
               (selection (completing-read "Choose a file: " choices nil t))
               (file (cdr (assoc selection links))))
          (if file
              (find-file file)
            (message "Selected ID could not be resolved.")))
      (message "No valid links found."))))

(defun sync0-org-roam-set-extra-properties ()
  "Set additional properties for new Org-roam nodes based on non-nil variables."
  (org-set-property "CREATED" (format-time-string "%Y-%m-%d"))
;;   (org-set-property "ROAM_REFS" sync0-org-roam-zettel-filename)
  ;;   (when sync0-org-roam-zettel-author
  ;;     (org-set-property "AUTHOR" sync0-org-roam-zettel-author))
  (when sync0-org-roam-zettel-title
    (org-set-property "SUBTITLE" sync0-org-roam-zettel-subtitle))
  (when sync0-org-roam-zettel-date
    (org-set-property "DATE" sync0-org-roam-zettel-date))
  (when sync0-org-roam-zettel-left
    (org-set-property "LEFT" sync0-org-roam-zettel-left))
  (when sync0-org-roam-zettel-up
    (org-set-property "UP" sync0-org-roam-zettel-up))
  (when sync0-org-roam-zettel-right
    (org-set-property "RIGHT" sync0-org-roam-zettel-right))
  (when sync0-org-roam-zettel-down
    (org-set-property "DOWN" sync0-org-roam-zettel-down))
  (when sync0-org-roam-zettel-version
    (org-set-property "VERSION" sync0-org-roam-zettel-version))
  (when sync0-org-roam-zettel-last-version
    (org-set-property "LAST_VERSION" sync0-org-roam-zettel-last-version))
  (when sync0-org-roam-zettel-project
    (org-set-property "PROJECT" sync0-org-roam-zettel-project))
  (when sync0-org-roam-zettel-origin
    (org-set-property "ORIGIN" sync0-org-roam-zettel-origin))
  ;; (when sync0-org-roam-zettel-parent
  ;;   (org-set-property "PARENT" sync0-org-roam-zettel-parent))
  (when sync0-org-roam-zettel-aliases
    (org-set-property "ROAM_ALIASES" sync0-org-roam-zettel-aliases))
  ;;   (when sync0-org-roam-zettel-filename
  ;;     (org-set-property "FILENAME" sync0-org-roam-zettel-filename))
  (when sync0-org-roam-zettel-type
    (org-set-property "ZTYPE" sync0-org-roam-zettel-type))
  (when sync0-org-roam-zettel-subtype
    (org-set-property "ZSTYPE" sync0-org-roam-zettel-subtype))
  (when sync0-org-roam-zettel-doctype
    (org-set-property "DOCTYPE" sync0-org-roam-zettel-doctype))
  ;; Reset all the variables after setting the properties
  (sync0-org-roam-reset-zettel-properties))

(add-hook 'org-roam-capture-new-node-hook #'sync0-org-roam-set-extra-properties)

(defun sync0-org-roam-format-zettel (quickcapture &optional type subtype)
  "Create a new zettel note in Obsidian and update the cache. This
is the main function to create Zettels."
  (interactive)
  (let* ((orig-buffer
          (buffer-name (org-capture-get :original-buffer)))
	 (origin (when (derived-mode-p 'org-mode)
		   (sync0-org-get-property orig-buffer "ID"))) 
	 (type (or type
		   (completing-read "Choose Zettel type: " sync0-zkn-zettel-types)))
         (subtype (unless quickcapture
		    (or subtype
		     (completing-read "Choose Zettel subtype: " (cdr (assoc type sync0-zkn-zettel-types-alist)))))) 
         (doctype (when (string= subtype "draft")
		    (completing-read "Choose Doc. type: " sync0-zkn-doctypes)))
         (directory (sync0-zkn-get-dir type subtype))  ;; Get the directory based on type and subtype
         (filename (sync0-zkn-generate-unique-filename directory))
         (obsidian-file (expand-file-name (concat filename ".org") directory)))
    (setq sync0-org-roam-zettel-filename filename)
    (setq sync0-org-roam-zettel-type type)
    (when subtype
      (setq sync0-org-roam-zettel-subtype subtype))
    (setq sync0-org-roam-zettel-origin origin)
    (setq sync0-org-roam-zettel-doctype doctype)
    ;; full path should be the last thing for this functino to work nnice with org-roam-capture
    (setq sync0-org-roam-zettel-path obsidian-file)))

(defun sync0-org-get-property (buffer org-property)
  (with-current-buffer buffer 
    (org-entry-get 1 org-property)))

(defun sync0-org-get-keyword (buffer keyword)
  (with-current-buffer buffer 
    (cadar (org-collect-keywords `(,keyword)))))

(defun sync0-org-buffer-contents-clean (buffer)
  "Get the contents of BUFFER without properties drawers and top-level keywords.
BUFFER is the buffer to process."
  (with-current-buffer buffer
    (save-restriction
      (widen) ;; Ensure we work on the entire buffer
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (insert content)
          ;; Remove top-level keywords
          (goto-char (point-min))
          (while (re-search-forward "^#\\+\\w+:.*$" nil t)
            (replace-match ""))
          ;; Remove properties drawers
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" nil t)
            (replace-match ""))
          ;; Return the cleaned-up content
          (buffer-string))))))

(defun sync0-org-roam-format-draft ()
  "Create a new zettel note in Obsidian and update the cache. This
is the main function to create Zettels."
  (interactive)
  (let* ((orig-buffer
          (buffer-name (org-capture-get :original-buffer)))
	 (type "writing")
	 (subtype "draft")
         (doctype (sync0-org-get-property orig-buffer "DOCTYPE")) 
         (prevdraft (sync0-org-get-property orig-buffer "ID")) 
         (project (sync0-org-get-property orig-buffer "PROJECT")) 
         (version (sync0-org-get-property orig-buffer "VERSION")) 
         (lastversion (sync0-org-get-property orig-buffer "LAST_VERSION"))  
         (title (sync0-org-get-keyword orig-buffer "TITLE"))  
         (archive (sync0-org-get-keyword orig-buffer "ARCHIVE"))  
         (author (sync0-org-get-keyword orig-buffer "AUTHOR"))  
         (date (sync0-org-get-keyword orig-buffer "DATE"))  
         (language (sync0-org-get-keyword orig-buffer "LANGUAGE"))  
         (directory (sync0-zkn-get-dir type subtype))  ;; Get the directory based on type and subtype
         (filename (sync0-zkn-generate-unique-filename directory))
         (contents (sync0-org-buffer-contents-clean orig-buffer))
         (obsidian-file (expand-file-name (concat filename ".org") directory)))
    (setq sync0-org-roam-zettel-filename filename)
    (setq sync0-org-roam-zettel-type type)
    (setq sync0-org-roam-zettel-left prevdraft)
    (setq sync0-org-roam-zettel-contents contents)
    (setq sync0-org-roam-zettel-subtype subtype)
    (setq sync0-org-roam-zettel-doctype doctype)
    (setq sync0-org-roam-zettel-archive archive)
    (setq sync0-org-roam-zettel-author author)
    (setq sync0-org-roam-zettel-date date)
    (setq sync0-org-roam-zettel-language language)
    (setq sync0-org-roam-zettel-project project)
    (setq sync0-org-roam-zettel-version  version)
    (setq sync0-org-roam-zettel-last-version  lastversion)
    (setq sync0-org-roam-zettel-title title)
    (setq sync0-org-roam-zettel-aliases (format "\"%s, %s\"" title lastversion))
    ;; full path should be the last thing for this functino to work nnice with org-roam-capture
    (setq sync0-org-roam-zettel-path obsidian-file)))

(defun sync0-org-roam-generate-zettel-template ()
  "Generate the template for Org-roam Zettel notes."
  (concat "#+TITLE: ${title}\n\n" sync0-zkn-org-main-coda))

(defun sync0-org-roam-generate-draft-template ()
  "Generate the template for Org-roam Zettel notes."
  (concat "#+TITLE: " sync0-org-roam-zettel-title "\n"
	  "#+AUTHOR: " sync0-org-roam-zettel-author "\n"
	  "#+DATE: " sync0-org-roam-zettel-date "\n"
	  "#+LANGUAGE: " sync0-org-roam-zettel-language "\n"
	  "#+ARCHIVE: " sync0-org-roam-zettel-archive "\n\n" sync0-org-roam-zettel-contents))

(setq org-roam-capture-templates
      '(("m" "Main" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel t "main"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("z" "Zettel" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("s" "Structure" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "main" "structure"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("f" "Fiche" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel t "fiche"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("p" "People" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "fiche" "people"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("l" "Project (note)" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "project" "note"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("k" "Keyword" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "fiche" "keyword"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)

        ("b" "Bibliography" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "main" "bibliography"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)

        ("c" "Collage" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "writing" "collage"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("d" "Dashboard" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "index" "dashboard"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("o" "Oeuvre" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-zettel nil "fiche" "oeuvre"))
                            (lambda () (sync0-org-roam-generate-zettel-template)))
         :unnarrowed t)
        ("r" "Reference" plain
         "%?"
         :target (file+head "%(concat sync0-zkn-references-dir \"${citar-citekey}.org\")"
                            (lambda () (sync0-bibtex-note-format-note)))
         :unnarrowed t)
        ("n" "New draft" plain
         "%?"
         :target (file+head (lambda () (sync0-org-roam-format-draft))
                            (lambda () (sync0-org-roam-generate-draft-template)))
         :unnarrowed t)))
	 )

(use-package md-roam
  :straight (md-roam :type git :host github :repo "nobiot/md-roam")
  :after org-roam
  :custom
  (md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
  :config
  (require 'sync0-zkn)

  (setq md-roam-regex-id
	"\\(^id:[ \t]*\\)\\(.*\\)")

(setq sync0-md-roam-regex-id
	"\\(^key:[ \t]*\\)\\(.*\\)")

  (setq md-roam-regex-aliases
	;; Assumed to be case insensitive
	"\\(^.*aliases:[ \t]*\\)\\(.*\\)")

(defun md-roam-get-id ()
  "Extract the key or id from the YAML frontmatter in the current buffer.

This function checks for the presence of `key:` first using `sync0-md-roam-regex-id`.
If `key:` is not found, it falls back to looking for `id:` using `md-roam-regex-id`.

It assumes:
 (1) Current buffer is a markdown file (but does not check it).
 (2) YAML frontmatter is located at the top of the file.
 (3) The formats are either:
     - `key: <string>`
     - `id: <string>`."

  (let ((frontmatter (md-roam-get-yaml-front-matter)))
    (cond
     ;; Check for `key:` first
     ((and frontmatter
           (string-match sync0-md-roam-regex-id frontmatter))
      (match-string-no-properties 2 frontmatter))
     ;; Fallback to `id:` if `key:` is not found
     ((and frontmatter
           (string-match md-roam-regex-id frontmatter))
      (match-string-no-properties 2 frontmatter))
     ;; Return nil if neither `key:` nor `id:` is found
     (t nil))))

(defun md-roam-db-insert-aliases (file-path)
  "Insert aliases from the YAML front matter of FILE-PATH into Org-roam cache.
The aliases must be defined in the front matter under the key `aliases`.

FILE-PATH is the path to the Markdown file being processed."
  (let ((node-id (md-roam-get-id)) ;; Reuse your existing method to get the node ID
        (data (sync0-db-extract-title-and-aliases file-path))) ;; Use your custom extraction function
    (when (and node-id data)
      (let ((title (car data))
            (aliases (cadr data)))
        (when aliases
          (org-roam-db-query
           [:insert :into aliases :values $v1]
           (mapcar (lambda (alias)
                     (vector node-id alias))
                   aliases)))))))

(defun md-roam-db-insert-file-node ()
  "Insert the file-level node into the Org-roam cache."
  ;; `org-roam-db-update-file' turns the mode to org-mode (in `org-roam-with-file' macro)
  ;; `markdown-mode' needs to be explicitly turned back on.
  ;; (markdown-mode)
  ;; Run org-roam hooks to re-set after-save-hooks, etc.
  (run-hooks 'org-roam-find-file-hook)
  ;; `org-with-point-at' macro does not seem to assume the buffer is Org
  (org-with-point-at 1
    (when-let ((id (md-roam-get-id)))
      (let* ((file (buffer-file-name (buffer-base-buffer)))
             (title (or (md-roam-get-title)
                        (file-relative-name file org-roam-directory)))
             (pos (point))
             (todo nil)
             (priority nil)
             (scheduled nil)
             (deadline nil)
             (level 0)
             (tags (md-roam-get-tags))
             ;; Properties are required for `org-roam-ui'
             ;; TODO other properties in frontmatter?
             (properties (list (cons "TITLE" title) (cons "ID" id)))
             (olp nil))
        (org-roam-db-query!
         (lambda (err)
           (lwarn 'org-roam :warning "%s for %s (%s) in %s"
                   (error-message-string err)
                   title id file))
         [:insert :into nodes
                  :values $v1]
         (vector id file level pos todo priority
                 scheduled deadline title properties olp))
        (when tags
          (org-roam-db-query
           [:insert :into tags
                    :values $v1]
           (mapcar (lambda (tag)
                     (vector id (substring-no-properties tag)))
                   tags)))
        (md-roam-db-insert-aliases file)
        (md-roam-db-insert-refs)))))

(cl-defun md-roam-node-insert (&optional filter-fn &key templates info)
  "Find an Org-roam node and insert (where the point is) a Markdown link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node`,
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'."
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (unwind-protect
        ;; Group functions together to avoid inconsistent state on quit
        (atomic-change-group
          (let* (region-text
                 beg end
                 (_ (when (region-active-p)
                      (setq beg (set-marker (make-marker) (region-beginning)))
                      (setq end (set-marker (make-marker) (region-end)))
                      (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
                 (node (org-roam-node-read region-text filter-fn))
                 (description (or region-text
                                  (org-roam-node-formatted node))))
            (if (org-roam-node-id node)
                (progn
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  ;; Insert Markdown link
                  (insert (format "[%s](%s.md)" (org-roam-node-title node) (org-roam-node-id node)))
                  ;; for advice
                  t)
              (org-roam-capture-
               :node node
               :info info
               :templates templates
               :props (append
                       (when (and beg end)
                         (list :region (cons beg end)))
                       (list :insert-at (point-marker)
                             :link-description description
                             :finalize 'insert-link)))
              ;; for advice
              t)))
      (deactivate-mark)
      ;; for advice
      t)))
  
(defun md-roam--parse-markdown-link (link)
  "Parse a Markdown link of the form [title](id.md) into the id.
Returns nil if the link format is invalid."
  (if (and (string-match "\\[.*?\\](\\([^/]+\\)\\.md)" link)
           (match-string 1 link))
      (match-string 1 link)
    (message "Invalid Markdown link format: %s" link)
    nil))

(defun md-roam-follow-markdown-link (name &optional other)
  "Follow Markdown link NAME (e.g. [title](id.md)) if the linked file exists.
If the linked NAME does not yet exist, display a message.

It is meant to advice `markdown-follow-link-at-point`.

When OTHER is non-nil (via prefix argument), open the file in
another window. This is not relevant if the file does not exist."
  (when (org-roam-file-p (buffer-file-name (buffer-base-buffer)))
    (if-let* ((link-id (md-roam--parse-markdown-link name))
              (node (org-roam-node-from-title-or-alias link-id))
              (node-populated (org-roam-populate node))
              (file (org-roam-node-file node-populated)))
        (when file
          (if other (find-file-other-window file) (find-file file)))
      ;; If no node is found, just show a message instead of capturing a new node
      (message (format "No Org-roam node found for \"%s\". File not found." name))
      ;; Optionally, you can also add any further handling here
      ;; (message "Try creating the node manually or using Org-roam capture.")
      )))

(advice-add #'markdown-follow-markdown-link :before-until #'md-roam-follow-markdown-link)

  (md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
  (org-roam-db-autosync-mode 1)) ; autosync-mode triggers db-sync. md-roam-mode must be already active

(provide 'sync0-org-roam)
