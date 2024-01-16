;; this function was integrated and it seems to work successfully. 
(setq deft-strip-summary-regexp
  (concat "\\("
           "[\n\t]" ;; blank
           "\\|^:[[:ascii:]]+:.*$" ;; org-mode properties string
            "\\|^#\\+[[:alpha:]_]+.*$" ;; org-mode metadata
           "\\|^Origin:.*$" ;; Origin string
           "\\|^\\[\\[file:.*$" ;; org-mode inline-images
           "\\)"))

(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file))
           (fixed (replace-regexp-in-string  "\\[\\[[[:alnum:][:space:]-:]+\\]\\[\\([[:graph:][:space:]]+\\)\\]\\]" "\\1" summary)) 
           (reduced (if (> (length fixed) 500)
                        (substring-no-properties fixed nil 500)
                      fixed))
           (new-string (string-join (split-string-every reduced 100) "\n"))
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
           (summary-width (min (deft-string-width summary)
                               (- line-width
                                  (length deft-separator)))))
      (widget-create 'link
                      :button-prefix ""
                      :button-suffix ""
                      :button-face 'deft-title-face
                      :format "%[%v%]"
                      :tag file
                      :help-echo "Edit this file"
                      :notify (lambda (widget &rest ignore)
                                (deft-open-file (widget-get widget :tag)))
                      (if title (truncate-string-to-width title title-width)
                        deft-empty-file-title))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (when (> summary-width 0)
        (widget-insert "\n")
        (widget-insert (propertize new-string 
                                   'face 'deft-summary-face)))
      (widget-insert "\n"))))














(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file))
           ;; (fixed (replace-regexp-in-string  "\\[\\[[[:graph:]]+\\]\\[\\([[:print:]]+\\)\\]\\]" "\\1" summary))
           ;; (fixed (substring (replace-regexp-in-string  "\\[\\[[[:alnum:][:space:]-:]+\\]\\[\\([[:graph:][:space:]]+\\)\\]\\]" "\\1" summary) 0 1000))
           (fixed (replace-regexp-in-string  "\\[\\[[[:alnum:][:space:]-:]+\\]\\[\\([[:graph:][:space:]]+\\)\\]\\]" "\\1" summary)) 
           (reduced (if (> (length fixed) 500)
                        (substring-no-properties fixed nil 500)
                      fixed))
           (new-string (string-join (split-string-every reduced 100) "\n"))
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
           (summary-width (min (deft-string-width summary)
                               (- line-width
                                  (length deft-separator)))))
      (widget-create 'link
                      :button-prefix ""
                      :button-suffix ""
                      :button-face 'deft-title-face
                      :format "%[%v%]"
                      :tag file
                      :help-echo "Edit this file"
                      :notify (lambda (widget &rest ignore)
                                (deft-open-file (widget-get widget :tag)))
                      (if title (truncate-string-to-width title title-width)
                        deft-empty-file-title))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (when (> summary-width 0)
        (widget-insert "\n")
        ;; (widget-insert (propertize  (string-join (split-string fixed 100) "\n")
        ;;                            'face 'deft-summary-face)))
        (widget-insert (propertize new-string 
                                   'face 'deft-summary-face)))
        ;; (widget-insert (propertize (truncate-string-to-width summary summary-width)
        ;;                            'face 'deft-summary-face)))
      (widget-insert "\n"))))




(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file))
           ;; (fixed (replace-regexp-in-string  "\\[\\[[[:graph:]]+\\]\\[\\([[:print:]]+\\)\\]\\]" "\\1" summary))
           ;; (fixed (substring (replace-regexp-in-string  "\\[\\[[[:alnum:][:space:]-:]+\\]\\[\\([[:graph:][:space:]]+\\)\\]\\]" "\\1" summary) 0 1000))
           (fixed (replace-regexp-in-string  "\\[\\[[[:alnum:][:space:]-:]+\\]\\[\\([[:graph:][:space:]]+\\)\\]\\]" "\\1" summary)) 
           (reduced (if (> (length fixed) 500)
                        (substring-no-properties fixed nil 500)
                      fixed))
           (new-string (string-join (split-string-every reduced 100) "\n"))
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
           (summary-width (min (deft-string-width summary)
                               (- line-width
                                  (length deft-separator)))))
      (widget-create 'link
                      :button-prefix ""
                      :button-suffix ""
                      :button-face 'deft-title-face
                      :format "%[%v%]"
                      :tag file
                      :help-echo "Edit this file"
                      :notify (lambda (widget &rest ignore)
                                (deft-open-file (widget-get widget :tag)))
                      (if title (truncate-string-to-width title title-width)
                        deft-empty-file-title))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (when (> summary-width 0)
        (widget-insert "\n")
        ;; (widget-insert (propertize  (string-join (split-string fixed 100) "\n")
        ;;                            'face 'deft-summary-face)))
        (widget-insert (propertize new-string 
                                   'face 'deft-summary-face)))
        ;; (widget-insert (propertize (truncate-string-to-width summary summary-width)
        ;;                            'face 'deft-summary-face)))
      (widget-insert "\n"))))







(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file))
     ;; (fixed (replace-regexp-in-string  "\\[\\[id:[[:ascii:]]+\\]\\[\\([[:print:]]+\\)\\]\\]" "\\1" summary))
    ;; (fixed (replace-regexp-in-string  "\\[\\[[[:ascii:]]+\\]\\[\\([[:print:]]+\\)\\]\\]" "\\1" summary))
;; [[id:6cb65d2f-4012-4390-9c3d-c35a565f27e9][Marx]] 
     ;;(fixed (replace-regexp-in-string  "\\[\\[[\\.\\/-_~:[:alnum:]]+\\]\\[\\([-_:'\"[:alpha:][:blank:]]+\\)\\]\\]" "\\1" summary))
     (fixed (replace-regexp-in-string  "\\[\\[[[:alnum:]_-]+\\]\\[\\([:'\"-_A-Za-z0-9 ]+\\)\\]\\]" "\\1" summary))
     ;;(fixed (replace-regexp-in-string  "\\[\\[[-_A-Za-z0-9]+\\]\\[\\([[:alpha][:blank]]+\\)\\]\\]" "\\1" summary))
     ;; (fixed (replace-regexp-in-string  "\\[\\[[[:ascii:]]+\\]\\[\\([[:alpha][:blank]]+\\)\\]\\]" "\\1" summary))
           ;; (tags (with-current-buffer (find-file-noselect file) 
           ;; (cadar (org-collect-keywords '("FILETAGS")))))
           ;;(type (org-entry-get 1 "ZETTEL_TYPE"))
           ;; (type (with-current-buffer (find-file-noselect file) 
           ;;       (org-entry-get 1 "ZETTEL_TYPE")))
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
(summary-width (min (deft-string-width summary)
                               (- line-width
                            ;;      title-width
                                  (length deft-separator)))))
           ;; (summary-width (min (deft-string-width summary)
           ;;                     (- line-width))))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face 'deft-title-face
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
                     :notify (lambda (widget &rest ignore)
                               (deft-open-file (widget-get widget :tag)))
                     (if title (truncate-string-to-width title title-width)
                       deft-empty-file-title))
         ;; (widget-insert (propertize deft-separator 'face 'deft-separator-face))
         ;; (widget-insert (concat type " " tags))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (when (> summary-width 0)
        (widget-insert "\n")
        ;; (widget-insert (propertize deft-separator 'face 'deft-separator-face))
        (widget-insert (propertize (truncate-string-to-width fixed summary-width)
                                   'face 'deft-summary-face)))
      (widget-insert "\n"))))


;; 
;;
;;   Man should not be ready to show that he can live like a
;;   badly-fed animal. He should decline to live like that, and
;;   should either steal or go on the rates, which is considered by
;;   many to be a form of stealing. As for begging, it is safer to
;;   beg than to take, but it is finer to take than to beg. No: a
;;   poor man who is ungrateful, unthrifty, discontented, and
;;   rebellious, is probably a real personality, and has much in him.
;;   He is at any rate a healthy protest. As for the virtuous poor,
;;   one can pity them, of course, but one cannot possibly admire
;;   them. They have made private terms with the enemy, and sold
;;   their birthright for very bad pottage.
;;
;;   Oscar Wilde
;;   The Soul of Man under Socialism (1891)
;; deft-strip-summary-regexp
Its value is

(setq deft-strip-summary-regexp
      (concat "\\("
              "[
	]\\|^#\\+[[:upper:]_]+:.*$\\)"
              )
      (setq deft-strip-summary-regexp
  (concat "\\("
           "[\n\t]" ;; blank
           "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
           "\\|:[[:ascii:]]+:.*$" ;; org-mode metadata
           "\\)"))

      (defun deft-parse-summary (contents title)
  "Parse the file CONTENTS, given the TITLE, and extract a summary.
The summary is a string extracted from the contents following the
title."
  (let ((summary (let ((case-fold-search nil))
                   (replace-regexp-in-string deft-strip-summary-regexp " " contents))))
    (deft-chomp
      (if (and title
               (not deft-use-filename-as-title)
               (string-match (regexp-quote
                              (if deft-org-mode-title-prefix
                                  (concat "^#+TITLE: " title)
                                title))
                             summary))
          (substring summary (match-end 0) nil)
        summary))))



      (defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file)) 
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
           (summary-width (min (deft-string-width summary)
                               (* line-width 4)
                               )))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face 'deft-title-face
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
                     :notify (lambda (widget &rest ignore)
                               (deft-open-file (widget-get widget :tag)))
                     (if title (truncate-string-to-width title title-width)
                       deft-empty-file-title))
(when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (when (> summary-width 0)
        (widget-insert "\n")
        (widget-insert (propertize (split-string-every summary 70)
                                   'face 'deft-summary-face)))
      (widget-insert "\n\n\n\n"))))

        ;;(widget-insert (propertize (truncate-string-to-width summary summary-width)


(defun split-string-every (string chars)
  "Split STRING into substrings of length CHARS characters.

This returns a list of strings."
  (cond ((string-empty-p string)
         nil)
        ((< (length string)
            chars)
         (list string))
    (t (cons (substring string 0 chars)
             (split-string-every (substring string chars)
                                 chars)))))

(split-string-every (buffer-string-no) 70)



      (setq org-recoll-filename (replace-regexp-in-string ">" "" org-recoll-filename))
      (setq org-recoll-filename (replace-regexp-in-string "#<buffer " "" org-recoll-filename))

      (setq org-recoll-filename (prin1-to-string (window-buffer (selected-window))))

      (setq org-recoll-filename (with-temp-buffer (window-buffer (selected-window))
            (cadar (org-collect-keywords '("TITLE"))
                                                  )))


      (setq test (with-temp-buffer (window-buffer (selected-window))
            (cadar (org-collect-keywords '("TITLE"))
                                                  )))


      (defun org-recoll-post-open-actions (squery)
  "Perform rendering or search actions on opened file.
Prompt to start a file-search for SQUERY in the opened file or to
call pdf-occur for a pdf.  isearch can be a bit slow with pdfs
due to rendering speed.  PDF-OCCUR provides a speedy alternative.
Falls back gracefully to a modified doc-view-search if in
doc-view (where isearch doesn't work."
  (interactive)
  ;;For some reason at the stage in the org hook where this is called,
  ;;the opened file document is the "selected window" but is not the
  ;;"current buffer."  This lead to weird results attempting to start
  ;;searches, and this line fixes it.
  (switch-to-buffer (window-buffer (selected-window)))
  ;;Retrieve the filename from the buffer title.
(if (string-match "\\.org" (prin1-to-string (window-buffer (selected-window))))
  (setq org-recoll-filename (cadar (org-collect-keywords '("TITLE"))))
(progn 
  (setq org-recoll-filename (prin1-to-string (window-buffer (selected-window))))
  (setq org-recoll-filename (replace-regexp-in-string ">" "" org-recoll-filename))
  (setq org-recoll-filename (replace-regexp-in-string "#<buffer " "" org-recoll-filename))))
  ;;If it's html, render it with shr
  (if (and org-recoll-render-html (featurep 'shr) (org-recoll-compare-string-to-list (file-name-extension org-recoll-filename) org-recoll-html-file-types))
      (org-recoll-shr-render-current-buffer))
  ;; Set pdfs and docs to page view by default
  (if (string= major-mode "doc-view-mode")
      (doc-view-fit-page-to-window)
    (if (string= (file-name-extension org-recoll-filename) "pdf") (pdf-view-fit-page-to-window)))
  ;;Search logic
  (if org-recoll-file-search-automatically
      (progn
	(if org-recoll-file-search-prompt (setq squery (read-string (concat "Enter file-search query: (default: " squery ")") nil nil squery)))
	;;If its a pdf, call pdf-occur (if available); otherwise start
	;;an isearch
	(if (string= (file-name-extension org-recoll-filename) "pdf")
	    (if (featurep 'pdf-tools) (pdf-occur squery) (message "Install pdf-tools and org-pdfview for pdf searching"))
	  (if(string= major-mode "doc-view-mode")
	      (org-recoll-doc-view-search squery)
	    (progn
	      (isearch-forward nil 1)
	      (isearch-yank-string squery))))))
    (if org-recoll-result-file-read-only (setq buffer-read-only t)))

(string-width (buffer-string))


booktitle
crossref
author
language
lagid
doctype lecture audio
institution
created
theme
year
status fetch
century


("Approccio alla storia concettuale" "Lezione del 22 settembre 2020" "2020-09-22" "https://elearning.unipd.it/spgi/mod/resource/view.php?id=49365")
("La modernità come “rottura”" "Lezione del 23 settembre 2020" "2020-09-22" "https://elearning.unipd.it/spgi/mod/resource/view.php?id=50260")
("Il lavoro dei concetti e la rivoluzione. sulla rappresentanza politica" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
("" "Lezione del 2020" "2020-" "")
