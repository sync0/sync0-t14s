(require 'sync0-pandoc)
(require 'sync0-print)
(require 'sync0-yaml)
(require 'sync0-obsidian)
(require 'sync0-bibtex-utils)
(require 'sync0-bibtex-actions)

(defun sync0-markdown-next-heading ()
  "Move to the next heading in the document."
  (interactive)
  (let ((found nil))
    (while (not found)
      (search-forward-regexp "^#+" nil t)
      (unless (or (eobp) (looking-at "^#\\+"))
        (search-forward-regexp "^#+" nil t))
      (setq found (not (eobp))))
    (beginning-of-line)))

(defun sync0-markdown-previous-heading ()
  "Move to the previous heading in the document."
  (interactive)
  (let ((found nil))
    (while (not found)
      (search-backward-regexp "^#+" nil t)
      (unless (or (bobp) (looking-at "^#\\+"))
        (search-backward-regexp "^#+" nil t))
      (setq found (not (bobp))))
    (beginning-of-line)))

;; Bind the functions to keys
(evil-define-key 'normal markdown-mode-map
  "<" 'sync0-markdown-previous-heading
  ">" 'sync0-markdown-next-heading)

;; (setq markdown-list-item-bullets '("●" "◎" "○" "◆" "◇" "►" "•"))
(setq markdown-list-item-bullets '("•"))


(setq sync0-markdown-zettel-template 
      (concat
"---
zettel_type: 
aliases: []
created: " (format-time-string "%Y-%m-%d") 
"\ntags: [inbox]
---
#"))

(defun sync0-markdown-new-zettel ()
  "Create new Zettel in Obsidian vault."
  (interactive)
  (let* ((filename (format-time-string "%Y%m%d%H%M%S"))
         (obsidian-file
          (concat sync0-obsidian-directory filename ".md")))
    (with-temp-buffer 
      (insert sync0-markdown-zettel-template)
      (write-file obsidian-file))
    (find-file obsidian-file)))

(setq markdown-command
      (concat
       ;; "/usr/local/bin/pandoc"
       "pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments"
       ;; " --resource-path=.:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/bibliographies"
       " --resource-path=.:/home/sync0/.local/share/pandoc/filters:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/bibliographies:/home/sync0/Pictures/archives"
       ;; " --resource-path=.:/home/sync0/Gdrive/typography/css:/home/sync0/Gdrive/typography/csl:/home/sync0/Gdrive/bibliographies"
       " --shift-heading-level-by=1" 
       ;; " --shift-heading-level-by=-1" 
       " --css=markdown.css"
       ;; " --csl=history-of-political-economy.csl"
       " --csl=histoire-at-politique.csl"
       ;; " --csl=histoire-et-mesure.csl"
       ;; " --quiet"
       ;; " --number-sections"
       ;; " --lua-filter=lua.lua"
       ;; " --lua-filter=/home/sync0/.local/share/pandoc/filters/noexport-subtrees.lua"
       " --metadata=reference-section-title:Références"
       " --citeproc"
       " --filter=/home/sync0/.local/share/pandoc/filters/delink.hs"
       " --bibliography=bibliography.bib"
       ))

(defun sync0-markdown-copy-pdf-in-cabinet ()
  "Copy the pdf corresponding to current file and paste it to the cabinet."
  (interactive)
  (let* ((full-path (buffer-file-name))
         (current-file (when (string-match "^.+/\\([[:alnum:]]+\\)\\.md$" full-path)
                         (match-string-no-properties 1 full-path)))
         (old-path (concat sync0-zettelkasten-references-directory current-file ".pdf"))
         (pdf-path (concat sync0-zettelkasten-attachments-directory current-file ".pdf")))
    (unless (null (file-exists-p old-path))
    (if (and (file-exists-p pdf-path)
             (yes-or-no-p "Keep current file in cabinet?"))
        (message "File already present in cabinet for %s" current-file)
      (copy-file old-path pdf-path t)))))

(defun sync0-markdown-copy-pdf-to-goodreads ()
  "Copy the pdf corresponding to current file and paste it to the goodreads directory."
  (interactive)
  (let* ((full-path (buffer-file-name))
         (current-file (when (string-match "^.+/\\([[:alnum:]]+\\)\\.md$" full-path)
                         (match-string-no-properties 1 full-path)))
         (path-sans-file (when (string-match "\\(^.+\\)/[[:alnum:]]+\\.md$" full-path)
                         (match-string-no-properties 1 full-path)))
         (old-path (concat path-sans-file "/" current-file ".pdf"))
         (pdf-path (concat sync0-goodreads-directory current-file ".pdf")))
    (unless (null (file-exists-p old-path))
    (if (and (file-exists-p pdf-path)
             (yes-or-no-p "Keep current file in cabinet?"))
        (message "File already present in cabinet for %s" current-file)
      (copy-file old-path pdf-path t)))))

(defun sync0-markdown-open-pdf-in-zathura ()
  "Open the PDF for the current markdown file if it exists."
  (interactive)
  (let* ((current-file (file-name-sans-extension (buffer-file-name))) ;; Get file path without extension
         (pdf-path (concat current-file ".pdf"))) ;; Add .pdf to the base file name
    (if (file-exists-p pdf-path)
        (call-process "zathura" nil 0 nil pdf-path) ;; Open PDF with Zathura
      (message "No PDF found for %s" (buffer-file-name))))) ;; Message if PDF is not found

(defun sync0-markdown-open-docx-in-libreoffice ()
  "Open the DOCX for the current markdown file in LibreOffice if it exists."
  (interactive)
  (let* ((current-file (file-name-sans-extension (buffer-file-name))) ;; Get file path without extension
         (docx-path (concat current-file ".docx"))) ;; Add .docx to the base file name
    (if (file-exists-p docx-path)
        (call-process "libreoffice" nil 0 nil docx-path) ;; Open DOCX with LibreOffice
      (message "No DOCX file found for %s" (buffer-file-name))))) ;; Message if DOCX is not found

(defun sync0-markdown-print-pdf ()
  "Print the pdf provided in the argument. Generalized for
interactive use and use in pipes to output to the printer."
  (interactive)
  (let ((pdf (concat (substring (buffer-file-name) 0 -2) "pdf"))
         (command (sync0-print-define-command))) 
    (if (file-exists-p pdf)
        (shell-command (concat command pdf))
      (message "No pdf found for current markdown note"))))

(defun sync0-markdown-copy-pdf-to-path ()
  "Copy attached pdf to path and change the title to make it
readable."
  (interactive)
  (let* ((pdf (concat (substring (buffer-file-name) 0 -2) "pdf"))
         (date (format-time-string "%Y-%m-%d"))
         (regex-title "^title: \"\\(.+\\)\"")
         (regex-subtitle "^subtitle: \"\\(.+\\)\"")
         (title (save-excursion
                  (goto-char (point-min))
                  (when  (re-search-forward regex-title nil t 1)
                    (match-string 1))))
         (subtitle (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward regex-subtitle nil t 1)
                       (match-string 1))))
         (target-path (read-directory-name "Où envoyer ce pdf ?" sync0-inbox-directory))
         (command (concat "cp "
                          pdf
                          " \""
                          target-path
                          "Rivera Carreño_"
                          date
                          "_"
                          title
                          "_"
                          subtitle
                          ".pdf\"")))
    (if (file-exists-p pdf)
        (progn 
          (shell-command command)
          ;; (message "%s" command)
          (message "PDF moved to target location"))
      (message "No PDF found"))))

(defun sync0-markdown-save-exported-pdf-in-cabinet ()
  "Create a copy of pdf corresponding to current file in our
cabinet (defined by sync0-zettelkasten-exported-pdfs-directory)."
  (interactive)
  (let* ((current-path (file-name-directory buffer-file-name))
         (current-file (sync0-yaml-get-property "key"))
         (current-pdf (concat current-path current-file ".pdf"))
         (target-pdf (concat sync0-zettelkasten-exported-pdfs-directory current-file ".pdf"))
         (command (concat "cp " current-pdf " " target-pdf)))
    (cond ((and  (file-exists-p current-pdf)
                 (file-exists-p target-pdf)
                 (yes-or-no-p "Overwrite copy in cabinet with Obsidian vault version?"))
           (shell-command command))
          ((and  (file-exists-p current-pdf)
                 (not (file-exists-p target-pdf))
                 (yes-or-no-p "Create copy in cabinet from Obsidian vault version?"))
           (shell-command command))
          (t (message "Command failed. Probably no pdf corresponds to current file.")))))

(defun sync0-markdown-correct-footnotes ()
  "Correct the numbering of footnotes in a Markdown file.
Renumbers the footnotes sequentially and reorganizes the footnote definitions at the bottom."
  (interactive)
  (let ((footnote-references '())
        (footnote-definitions '())
        (new-footnote-map (make-hash-table :test 'equal))
        (counter 1))

    ;; Collect footnote references and positions
    (goto-char (point-min))
    (while (re-search-forward "\\[\\^\\([0-9]+\\)\\]" nil t)
      (let ((footnote (match-string 1))
            (pos (match-beginning 0)))
        (push (cons footnote pos) footnote-references)))

    ;; Collect footnote definitions
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\^\\([0-9]+\\)\\]:\\s-*\\(.*\\)$" nil t)
      (let ((footnote (match-string 1))
            (definition (match-string 2)))
        (push (cons footnote definition) footnote-definitions)))

    ;; Create a mapping from old footnote numbers to new sequential numbers
    (dolist (footnote-pair (reverse footnote-references))
      (let ((old-number (car footnote-pair)))
        (unless (gethash old-number new-footnote-map)
          (puthash old-number (number-to-string counter) new-footnote-map)
          (setq counter (1+ counter)))))

    ;; Update footnote references in the text
    (dolist (footnote-pair footnote-references)
      (let* ((old-number (car footnote-pair))
             (pos (cdr footnote-pair))
             (new-number (gethash old-number new-footnote-map)))
        (goto-char pos)
        (re-search-forward "\\[\\^\\([0-9]+\\)\\]" (point-at-eol) t)
        (replace-match (format "[^%s]" new-number))))

    ;; Update footnote definitions and reorder them
    (let ((new-definitions (make-hash-table :test 'equal))
          (sorted-definitions '()))
      (dolist (footnote-pair footnote-definitions)
        (let* ((old-number (car footnote-pair))
               (definition (cdr footnote-pair))
               (new-number (gethash old-number new-footnote-map)))
          (puthash new-number definition new-definitions)))
      (maphash (lambda (key value) (push (cons (string-to-number key) value) sorted-definitions)) new-definitions)
      (setq sorted-definitions (sort sorted-definitions (lambda (a b) (< (car a) (car b)))))

      ;; Remove old footnote definitions
      (goto-char (point-min))
      (while (re-search-forward "^\\[\\^\\([0-9]+\\)\\]:.*$" nil t)
        (replace-match ""))

      ;; Insert new footnote definitions in sequential order at the end of the buffer
      (goto-char (point-max))
      (insert "\n")
      (dolist (definition sorted-definitions)
        (insert (format "[^%d]: %s\n\n" (car definition) (cdr definition)))))

    (message "Footnotes renumbered and reorganized successfully.")))

(defun sync0-markdown-correct-footnotes-with-starting-number ()
  "Correct the numbering of footnotes in a Markdown file.
Renumbers the footnotes sequentially starting from a user-specified number
and reorganizes the footnote definitions at the bottom."
  (interactive)
  (let ((starting-number (read-number "Enter starting number for footnotes: "))
        (footnote-references '())
        (footnote-definitions '())
        (new-footnote-map (make-hash-table :test 'equal))
        (counter starting-number))

    ;; Collect footnote references and positions
    (goto-char (point-min))
    (while (re-search-forward "\\[\\^\\([0-9]+\\)\\]" nil t)
      (let ((footnote (match-string 1))
            (pos (match-beginning 0)))
        (push (cons footnote pos) footnote-references)))

    ;; Collect footnote definitions
    (goto-char (point-min))
    (while (re-search-forward "^\\[\\^\\([0-9]+\\)\\]:\\s-*\\(.*\\)$" nil t)
      (let ((footnote (match-string 1))
            (definition (match-string 2)))
        (push (cons footnote definition) footnote-definitions)))

    ;; Create a mapping from old footnote numbers to new sequential numbers
    (dolist (footnote-pair (reverse footnote-references))
      (let ((old-number (car footnote-pair)))
        (unless (gethash old-number new-footnote-map)
          (puthash old-number (number-to-string counter) new-footnote-map)
          (setq counter (1+ counter)))))

    ;; Update footnote references in the text
    (dolist (footnote-pair footnote-references)
      (let* ((old-number (car footnote-pair))
             (pos (cdr footnote-pair))
             (new-number (gethash old-number new-footnote-map)))
        (goto-char pos)
        (re-search-forward "\\[\\^\\([0-9]+\\)\\]" (point-at-eol) t)
        (replace-match (format "[^%s]" new-number))))

    ;; Update footnote definitions and reorder them
    (let ((new-definitions (make-hash-table :test 'equal))
          (sorted-definitions '()))
      (dolist (footnote-pair footnote-definitions)
        (let* ((old-number (car footnote-pair))
               (definition (cdr footnote-pair))
               (new-number (gethash old-number new-footnote-map)))
          (puthash new-number definition new-definitions)))
      (maphash (lambda (key value) (push (cons (string-to-number key) value) sorted-definitions)) new-definitions)
      (setq sorted-definitions (sort sorted-definitions (lambda (a b) (< (car a) (car b)))))

      ;; Remove old footnote definitions
      (goto-char (point-min))
      (while (re-search-forward "^\\[\\^\\([0-9]+\\)\\]:.*$" nil t)
        (replace-match ""))

      ;; Insert new footnote definitions in sequential order at the end of the buffer
      (goto-char (point-max))
      (insert "\n")
      (dolist (definition sorted-definitions)
        (insert (format "[^%d]: %s\n\n" (car definition) (cdr definition)))))

    (message "Footnotes renumbered and reorganized successfully.")))

(defun sync0-markdown-copy-header-choose ()
  "Let the user choose a header and copy its content including subheaders to the kill ring."
  (interactive)
  (when (derived-mode-p 'markdown-mode)
    (let* ((headers (sync0-markdown-get-headers))
           (current-header (sync0-markdown-current-header))
           (header (completing-read "Choose header: " headers nil t nil nil current-header))
           (header-regexp (concat "^\\(#+\\) " (regexp-quote header)))
           (start (save-excursion
                    (goto-char (point-min))
                    (re-search-forward header-regexp nil t)
                    (point)))
           (end (save-excursion
                  (goto-char start)
                  (sync0-markdown-end-of-header)
                  (point))))
      (save-excursion
        (goto-char end)
        (while (and (not (eobp))
                    (not (or (looking-at-p "^\\(#+\\) ")
                             (looking-at-p "^\\* "))))
          (forward-line))
        (setq end (point)))
      (kill-new (buffer-substring-no-properties start end))
      (message "Copied content from header '%s' and its subheaders to the kill ring." header))))

(defun sync0-markdown-get-headers ()
  "Return a list of all headers in the current markdown buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((headers '()))
      (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
        (push (match-string 2) headers))
      headers)))

(defun sync0-markdown-current-header ()
  "Return the current header at point or the closest header moving backwards."
  (save-excursion
    (let ((current-header nil)
          (point (point)))
      ;; Move to the start of the line and search backwards for headers
      (goto-char (point-at-bol))
      (if (re-search-backward "^\\(#+\\) \\(.*\\)" nil t)
          (let ((level (length (match-string 1))))
            ;; Check if the found header is at the same level or above
            (if (>= level (length (match-string 1)))
                (setq current-header (match-string 2))))
        ;; If no header found, check if the current line is a header
        (goto-char point)
        (if (looking-at-p "^\\(#+\\) \\(.*\\)")
            (setq current-header (match-string 2))))
      current-header)))

(defun sync0-markdown-end-of-header ()
  "Move point to the end of the current header's content, including subheaders."
  (let ((header-level (save-excursion
                        (goto-char (point-at-bol))
                        (length (match-string 1)))))
    (while (and (not (eobp))
                (not (looking-at-p (concat "^" (make-string header-level ?#) " "))))
      (forward-line))
    (goto-char (point-at-bol))
    (if (not (eobp))
        (backward-char))))

(defun sync0-markdown-generate-toc-for-file ()
  "Generate a table of contents for the entire file."
  (save-excursion
    (goto-char (point-min))
    (let ((toc '()))
      (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
        (let* ((level (length (match-string 1)))
               (title (match-string 2))
               (indent (make-string (* (- level 1) 4) ?\s)))  ; Indentation of 4 spaces
          (push (format "%s- %s" indent title) toc)))
      (mapconcat 'identity (nreverse toc) "\n"))))

(defun sync0-markdown-generate-toc-for-heading (heading toc)
  "Generate a table of contents (TOC) for the specified HEADING and its subheadings, and copy it to the kill ring.
The specified HEADING itself will not be included in the TOC."
  (let* ((toc-lines (split-string toc "\n")) ; Split the TOC into lines
         (origheader-level (length (split-string heading "\\-"))) ; Determine the level of the chosen header
         (header-level (* origheader-level 2)) ; Determine the level of the chosen header
         ;; (regex (concat "^[^ ]" (make-string origheader-level ?\s)))
         (regex (concat "^" (make-string origheader-level ?\s) "-"))
         (header-found nil)
         (result '()))
    ;; Iterate through each line of the TOC
    (dolist (line toc-lines)
      ;; Check if the line contains the heading
      (when (string-match-p (regexp-quote heading) line)
        (setq header-found t)) ; Mark the header as found

      ;; If the header has been found, add the line to the result
      (when header-found
        ;; Skip adding the chosen heading itself
        (unless (string-match-p (regexp-quote heading) line)
          (push (substring line (+ header-level 2)) result))

        ;; Stop if we reach a line with a heading that is at the same or higher level
        (when (string-match-p "^[^ ]" line)
        ;; (when (string-match-p regex line)
          (setq header-found nil))))
    ;; Join the result list into a single string
    (mapconcat 'identity (reverse result) "\n")))

(defun sync0-markdown-generate-toc ()
  "Generate a table of contents (TOC) for the entire file or a specific heading and copy it to the kill ring."
  (interactive)
  (when (derived-mode-p 'markdown-mode)
    (let* ((choice (completing-read "Generate TOC for (file/heading): " '("file" "heading")))
           (toc (sync0-markdown-generate-toc-for-file)))
      (when (and toc
	       (string= choice "heading"))
          (let ((header (completing-read "Choose header: " (sync0-markdown-get-headers) nil t)))
            (setq toc (sync0-markdown-generate-toc-for-heading header toc))))
      (when toc
        (kill-new toc)
        (message "Copied TOC to the kill ring.")))))

(defun sync0-toggle-footnote ()
  "Insert a new footnote if none is present on the current line.
If a footnote reference is found, navigate to its definition.
If a footnote definition is found, navigate back to the reference."
  (interactive)
  (let ((current-line (thing-at-point 'line t)))
    (cond
     ;; Case 1: We are at a footnote definition
     ((string-match "^\\[\\^\\([0-9]+\\)\\]:" current-line)
      (let ((footnote-number (match-string 1 current-line)))
        (goto-char (point-min)) ;; Start searching from the beginning of the buffer
        (if (search-forward (concat "[^" footnote-number "]") nil t)
            (message "Navigated to footnote reference.")
          (message "Footnote reference not found."))))

     ;; Case 2: We are at a footnote reference
     ((string-match "\\[\\^\\([0-9]+\\)\\]" current-line)
      (let ((footnote-number (match-string 1 current-line)))
        (goto-char (point-min)) ;; Start searching from the beginning of the buffer
        (if (search-forward (concat "[^" footnote-number "]:") nil t)
            (message "Navigated to footnote definition.")
          (message "Footnote definition not found."))))

     ;; Case 3: No footnote found, insert a new one
     (t
      (markdown-insert-footnote)))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq comment-start "%% ")
            (setq comment-end " %%")
            (setq comment-add 0)
            (setq comment-style 'extra)
            (setq comment-multi-line t)
            (setq comment-use-syntax t)
            (local-set-key (kbd "M-;") 'sync0-comment-or-uncomment)))

;; Custom comment/uncomment function
(defun sync0-comment-or-uncomment ()
  "Comment or uncomment the current line or region using `%%` markers."
  (interactive)
  (let ((comment-regex (concat "^" (regexp-quote comment-start) "\\(.*?\\)" (regexp-quote comment-end) "$")))
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (if (save-excursion
                (goto-char start)
                (looking-at comment-regex))
              ;; Uncommenting
              (progn
                (goto-char start)
                (while (re-search-forward comment-regex end t)
                  (replace-match (match-string 1) nil nil))
                (delete-trailing-whitespace start end))  ;; Clean up trailing whitespace
            ;; Commenting
            (comment-region start end)))
      (let ((line (thing-at-point 'line t)))
        (if (string-match comment-regex line)
            ;; Uncommenting
            (let ((new-line (replace-match (match-string 1 line) nil nil line)))
              (delete-region (line-beginning-position) (line-end-position))
              (insert new-line))
          ;; Commenting
          (insert (concat comment-start (string-trim line) comment-end)))))))

;; Function to set up custom font-lock rules for Markdown mode
(defun sync0-markdown-font-lock-setup ()
  "Set up custom font-lock rules for Markdown mode to recognize `%%` comments using the default comment face."
  (font-lock-add-keywords
   nil
   '(("^%% .*%%$"
      . font-lock-comment-face))))

;; Apply custom font-lock rules in Markdown mode
(add-hook 'markdown-mode-hook 'sync0-markdown-font-lock-setup)

(defun sync0--markdown-headings-with-levels ()
  "Generate a list of Markdown headings with hierarchy formatting, skipping first-level headings."
  (let ((headings '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(#+\\)\\(.*\\)\n" nil t)
        (let* ((heading-level (length (match-string 1)))
	       ;; Remove markdown link formatting to declutter visuals
	       (sanitized-text (replace-regexp-in-string "\\[\\([^]]+\\)\\](.*)" "\\1" (match-string 2)))
               (heading-text (concat "● " (string-trim-left sanitized-text))))
          ;; Skip first-level headings (heading-level = 1)
          (unless (= heading-level 1)
            (let* ((counter (string-to-number (cond ((equal heading-level 2) "0")
						    ((equal heading-level 3) "1")
						    ((equal heading-level 4) "2")
						    ((equal heading-level 5) "3")
						    ((equal heading-level 6) "4"))))
                  (formatted-heading
                   (if (= heading-level 2)
                       heading-text  ;; Level 2 has no indentation
                     (concat (make-string (* counter (- heading-level counter)) ?\s) heading-text))))
              ;; Remove any font-lock properties (e.g., face, size)
              (set-text-properties 0 (length formatted-heading) nil formatted-heading)
              ;; Store the formatted heading and its position
              (push (cons formatted-heading (line-beginning-position)) headings))))))
    headings))

(defun sync0--find-current-or-previous-heading (headings)
  "Find the nearest heading at or before the current point from the list of HEADINGS."
  (save-excursion
    (let ((pos (point)))
      (catch 'found
        (dolist (heading headings)  ;; No need to reverse the list here
          (when (<= (cdr heading) pos)  ;; Check if the heading is before or at the current position
            (throw 'found heading)))))))

(defun sync0-imenu-markdown-headings ()
  "Jump to a Markdown heading within the current buffer, excluding the first-level title.
Preselects the nearest heading at or before point."
  (interactive)
  (let* ((candidates (sync0--markdown-headings-with-levels))  ; Get all formatted headings with positions
;;          (preselected (sync0--find-current-or-previous-heading candidates))  ; Find closest heading
         (selected (consult--read candidates
                                  :prompt "Headings: "
                                  :require-match t
                                  :sort nil
;;                                   :initial (car preselected)
				  :inherit-input-method t
				  )))  ; Preselect the closest heading
    ;; Find the unformatted text of the selected heading and its position
    (let* ((selected-heading (assoc selected candidates))    ; Find the heading's position by formatted text
           (unformatted-heading (if selected-heading
                                     (string-trim-left (substring (car selected-heading) 2)) ; Remove the "●"
                                   nil)))  ; Extract the unformatted heading text
      (when unformatted-heading
        (let ((position (cdr selected-heading)))  ; Extract the position from the cons cell
          (save-selected-window
            (goto-char position)                   ; Move to the position
            (recenter)))))))
	      

(evil-leader/set-key-for-mode 'markdown-mode "M" 'sync0-imenu-markdown-headings)

(defun sync0-markdown-link-finder-with-search (url)
  "Find and open an Obsidian-style link URL in `sync0-obsidian-search-directories`.
If not found, fallback to an interactive search using `sync0-search-obsidian-notes`."
  (when (and (string-suffix-p ".md" url)
             (not (file-name-absolute-p url)))
    (let* ((found-file (catch 'found
                         (dolist (dir sync0-obsidian-search-directories)
                           (let ((file-path (expand-file-name url dir)))
                             (when (file-exists-p file-path)
                               (throw 'found file-path)))))))
      (if found-file
          (find-file found-file)
        (message "File not found in specified directories; opening interactive search.")
        (sync0-search-obsidian-notes)
        nil))))

;; Add our enhanced function to the markdown-follow-link hook
(add-hook 'markdown-follow-link-functions 'sync0-markdown-link-finder-with-search)

(defun sync0-markdown-open-attachment-at-point ()
  "Open the PDF associated with the nearest citation in Markdown.
Searches backward for a citation in visual lines, or prompts for a BibTeX key if none is found."
  (interactive)
  (save-excursion
    (let ((citation-id nil)
          (search-regex "\\[@\\([a-zA-Z0-9-]+\\)[^]]*\\]")) ;; Regex to match citation
      ;; Check if the point is already at a citation
      (if (looking-at search-regex)
          (setq citation-id (match-string 1))
        ;; Otherwise, search backward within the current visual line
        (progn
          (beginning-of-visual-line)
          (if (re-search-backward search-regex (line-beginning-position) t)
              (setq citation-id (match-string 1))
            ;; If no citation is found, prompt for a key
            (setq citation-id (sync0-bibtex-completion-choose-key t nil "Attachment to search: ")))))
      ;; Open the PDF if a citation ID is found
      (if citation-id
          (sync0-bibtex-open-pdf citation-id)
        (message "No citation found or provided.")))))

(major-mode-hydra-define markdown-mode nil 
  ("Links"
   ;; ("s" org-store-link)
   (("i" markdown-insert-wiki-link "Insert wiki-link")
    ("k" markdown-insert-link "Insert markdown link")
    ("I" markdown-insert-image "Insert image"))
   "Scholarly"
   (("f" sync0-toggle-footnote "Add or navigate footnote")
;;     ("F" markdown-footnote-return "Footnote navigate back")
    ("o" sync0-markdown-open-attachment-at-point "Open citation")
    ("c" counsel-bibtex "Show bibtex entry"))
   "Visualization"
   (("m" markdown-toggle-markup-hiding "Toggle markup")
    ("b" markdown-narrow-to-subtree "Narrow to header")
    ("z" sync0-markdown-open-pdf-in-zathura "Show PDF")
    ("L" sync0-markdown-open-docx-in-libreoffice "Show DOCX")
    ("r" sync0-org-ref-open-pdf-at-point-zathura "Open reference pdf"))
   "Export"
   (("p" sync0-pandoc-export-md-to-pdf "Pdf")
;;     ("p" sync0-pandoc-process-markdown "Pdf (quick)")
    ("l" sync0-pandoc-export-md-to-tex "TeX")
    ("d" sync0-pandoc-export-md-to-docx "Docx"))
   ;; ("b" org-epub-export-to-epub)
   ;; ("t" sync0-pandoc-export-md-to-tex)
   ;; ("E" sync0-org-export-headlines-to-latex)
   "Etc"
   (("a" sync0-define-local-abbrev "Define abbrev")
   ("P" sync0-markdown-print-pdf "Print corresp. pdf")
   ("C" sync0-markdown-copy-pdf-in-cabinet "Copy pdf to cabinet")
;;    ("P" sync0-markdown-print-pdf-from-markdown "Print pdf")
   ("1" sync0-markdown-correct-footnotes "Correct footnotes (all)")
   ;; ("2" sync0-markdown-rename-footnotes-in-section "Correct footnotes (section)")
   ("2" sync0-markdown-correct-footnotes-with-starting-number "Correct footnotes (w/ number)")
   ("3" sync0-markdown-copy-header-choose "Copy text under header")
   ("4" sync0-markdown-generate-toc "Generate TOC")
   ("g" sync0-markdown-copy-pdf-to-goodreads "Copy pdf to goodreads")
   ;; ("C" sync0-markdown-save-exported-pdf-in-cabinet "Copy pdf to cabinet")
   ("M" sync0-markdown-copy-pdf-to-path "Move to path"))))

(provide 'sync0-markdown)
