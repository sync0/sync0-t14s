;; (defun sync0-yaml-get-property (property)
;;   "Look for the value of current property in file and extract it."
;;   (goto-char (point-min))
;;   (let* ((heading (concat "^" property ": "))
;;          (raw-property (progn
;;                          (re-search-forward (concat heading "\\([[:graph:]]+\\)$") nil t 1)
;;                          (match-string-no-properties 1))))
;;     (unless (null raw-property)
;;       (if (or (string-match "^\"" raw-property)
;;               (string-match "^[" raw-property))
;;           (format "%s" (substring raw-property 1 -1))
;;         (format "%s" raw-property)))))

;; (setq sync0-obsidian-delimiters '("\"" "[" "{"))

(defun sync0-delimiter-remover (string)
  (let (x)
    (setq x string)
    (if (or (string-match "^\"" x)
            (string-match "^\\[" x))
        (progn
          (setq x (substring x 1 -1))
          (sync0-delimiter-remover x))
      x)))

(defun sync0-yaml-get-property (property string)
  "Look for the value of current property in file and extract it."
  (let* ((heading (concat "^" property ": "))
         (raw-property (when (string-match (concat heading "\\([[:blank:][:graph:]]+\\)$") string)
                         (match-string-no-properties 1 string))))
    (unless (null raw-property)
      (sync0-delimiter-remover raw-property))))

(provide 'sync0-yaml)
