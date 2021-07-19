
(defun sync0-bibtex-next-key ()
  "Print the bibtex key of the document"
  (interactive)
  (let ((bibtex-key (re-search-forward "@.+{" nil nil 1)))
    (goto-char bibtex-key)))

(defun sync0-bibtex-previous-key ()
  "Print the bibtex key of the document"
  (interactive)
  (let ((bibtex-key (re-search-backward "@.+{" nil nil 2)))
    (goto-char bibtex-key)
    (re-search-forward "@.+{" nil nil 1)))

(defun sync0-bibtex-update-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S"))
         (directory "/home/sync0/Documents/pdfs/")
         (new-path (concat directory new-key ".pdf"))
         (pdf-path
          (save-excursion
            (when  (re-search-forward "file = {\\(.+\\)}," nil t 1)
              (match-string-no-properties 1)))))
    (when-let ((type
                (when (re-search-forward "\\(^@[[:lower:]]+{\\)[[:digit:]]+," nil t 1)
                  (match-string-no-properties 1))))
      (kill-whole-line 1)
      (insert (concat type new-key ",\n")))
    (when (re-search-forward "file = {" nil t 1)
      (kill-whole-line 1)
      (insert (concat "file = {" new-path "},\n")))))

;; (when (file-exists-p pdf-path)
;;   (rename-file pdf-path new-path)))))


(provide 'sync0-bibtex-functions)
