(defun sync0-yaml-get-property (property)
  "Look for the value of current property in file and extract it."
  (save-excursion
    (goto-char (point-min))
    (let* ((heading (concat "^" property ": "))
           (raw-property (progn
                           (re-search-forward (concat heading "\\([[:graph:]]+\\)\n") nil t 1)
                           (match-string-no-properties 1))))
      (unless (null raw-property)
      (if (string-match "\"" raw-property)
          (substring raw-property 1 -1)
        raw-property)))))

(provide 'sync0-yaml)
