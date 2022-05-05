;; (defun test ()
;;   (interactive)
;;     (let ((filename (sync0-new-name)))
;;     (when (re-search-forward "\\(^@[[:lower:]]+{\\)[[:alnum:]_-]+," nil t 1)
;;         (kill-whole-line 1)
;;         (insert (concat (match-string-no-properties 1) filename ",\n")))))

(defun test ()
  (interactive)
  (when-let ((filename (sync0-new-name))
             (type
              (when (re-search-forward "\\(^@[[:lower:]]+{\\)[[:alnum:]_-]+," nil t 1)
(match-string-no-properties 1))))
        (kill-whole-line 1)
        (insert (concat type filename ",\n"))))

;; (save-match-data
;;                 (re-search-forward "{\\([a-z_-]+[[:digit:]_-]+\\)," nil t 1))))
;;     (replace-match filename nil nil stringy 1)))

;; (defun test ()
;;   (interactive)
;;     (let ((filename (sync0-new-name)))
;;      (replace-regexp "{\\([a-z_-]+[[:digit:]_-]+\\)," filename nil nil nil nil))



(setq sync0-counter 0)

(defun sync0-new-name ()
"Create a new filename from a time string"
(interactive)
(let ((value (1+ sync0-counter))
       (base (string-to-number (format-time-string "%Y%m%d%H%M%S"))))
(setq sync0-counter value)
(number-to-string (+ base value))))

(defun foo-single ()
  "replace the filename for numbers in current filename and in its
corresponding bibtex entry"
  (interactive)
  (let* ((filename (sync0-new-name))
         (directory "/home/sync0/Documents/pdfs/")
         (new-path (concat directory filename ".pdf"))
         (pdf-path
          (save-excursion
          (when  (re-search-forward "file = {\\(.+\\)}," nil t 1)
            (match-string-no-properties 1)))))
  (when-let ((type
              (when (re-search-forward "\\(^@[[:lower:]]+{\\)[[:alnum:]_-]+," nil t 1)
(match-string-no-properties 1))))
        (kill-whole-line 1)
        (insert (concat type filename ",\n")))
    (when (re-search-forward "file = {" nil t 1)
        (kill-whole-line 1)
        (insert (concat "file = {" new-path "},\n"))
      (when (file-exists-p pdf-path)
        (rename-file pdf-path new-path)))))

(defun foo-two ()
  "replace the filename for numbers in current filename and in its
corresponding bibtex entry"
  (interactive)
  (let* ((filename
          (save-excursion
            (when (re-search-forward "{\\([[:digit:]]+\\)," nil t 1)
            (match-string-no-properties 1))))
         (directory "/home/sync0/Documents/pdfs/")
         (new-path (concat directory filename ".pdf"))
         (pdf-path
          (save-excursion
           (when  (re-search-forward "file = {\\(.+\\)}," nil t 1)
            (match-string-no-properties 1)))))
    (when (re-search-forward "file = {" nil t 1)
        (kill-whole-line 1)
        (insert (concat "file = {" new-path "},\n"))
      (when (file-exists-p pdf-path)
        (rename-file pdf-path new-path)))))














(interactive)
(let* ((filename (sync0-new-name))
        (inter-path (org-collect-keywords '("INTERLEAVE_PDF")))
        (directory "/home/sync0/Documents/pdfs/")
        (path (concat directory filename ".pdf"))
        (pdf-path
         (when inter-path
             (cadar (org-collect-keywords '("INTERLEAVE_PDF")))))
        (raw-ref (org-entry-get 1 "ROAM_REFS"))
        (ref (if (string-match-p "\"[[:graph:]]+\"" raw-ref)
             (string-trim raw-ref "\"" "\"")
                raw-ref))
        (old-location (concat default-directory ref ".org"))
        (new-location (concat default-directory filename ".org")))
;; do this on the file
;; first, rename it
(rename-file old-location new-location)
    (with-current-buffer
        (find-file-noselect new-location)
      (org-with-point-at 1
        (org-set-property "ROAM_REFS" filename)  
        (while (search-forward ref (point-max) t)
          (replace-match filename))
       (when  (re-search-forward "^#\\+INTERLEAVE_PDF:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+INTERLEAVE_PDF: " path "\n\n")))))
(dolist (biblio sync0-biblios)
    (with-current-buffer
        (find-file-noselect biblio)
        (goto-char (point-min))
        (when (search-forward ref (point-max) t)
          (replace-match filename)
       ;; (when  (re-search-forward "^file = " (point-max) t)
       ;;  (kill-whole-line 1)
       ;;  (insert (concat "#+INTERLEAVE_PDF: " path "\n\n")))
        (if pdf-path
            (progn
              (search-forward pdf-path (point-max) t)
       (replace-match path)
(when (file-exists-p pdf-path)
(rename-file pdf-path path)))
(progn 
(next-line)
(beginning-of-line)
        (insert (concat "file = {" path "},\n")))))))))
