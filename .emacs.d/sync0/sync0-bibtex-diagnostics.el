
(defun sync0-bibtex-file-exists-p (&optional refkey extension collect)
  "Check whether a file with extension of an associated bibtex
entry exists or not."
  (interactive)
  (let* ((bibkey (or refkey 
                     (sync0-bibtex-choose-key)))
         ;; when file found, this function outputs a list of
         ;; files matching for the current bibtex entry
         (entry (citar-get-entry bibkey))
         (predicate (when extension
                      (if (string-match-p "^\\." extension)
                          (lambda (x) (string-match extension x))
                        (lambda (x) (string-match (concat ".+\\." extension) x)))))
         (file-list-raw (citar-get-value "file" bibkey))
         (file-list (or (split-string file-list-raw ";") 
                      (list file-list-raw)))
         messages)  
    (if collect
        (if (>= (length file-list) 1)
            (progn
              (dolist (file file-list messages)
                (push (format "%s : exists %s" bibkey file) messages))
              (sync0-show-elements-of-list messages "\n"))
          (if-let ((file-field (citar-get-value "file" entry)))
              (format "%s : absent %s" bibkey file-field)
            (format "%s : field empty" bibkey)))
      (if (>= (length file-list) 1)
          (progn
            (dolist (file file-list messages)
              (push (format "%s : exists %s" bibkey file) messages))
            (message-or-box (sync0-show-elements-of-list messages "\n")))
        (if-let ((file-field (citar-get-value "file" entry)))
            (message "%s : absent %s" bibkey file-field)
          (message "%s : field empty" bibkey))))))


(provide 'sync0-bibtex-diagnostics)
