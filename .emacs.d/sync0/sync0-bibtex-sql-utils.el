;; (defun sync0-bibtex-sql-sanitize-value (value)
;;   "Sanitize VALUE for SQL by escaping single quotes and ensuring it's a string."
;;   (when value
;;     (replace-regexp-in-string "'" "''" (format "%s" value))))

(require 'xah-replace-pairs)

(defun sync0-bibtex-sql-sanitize-value (value)
  "Sanitize VALUE by replacing straight quotes with smart quotes (opening and closing)."
  (when value
    (let ((sanitized (format "%s" value)))

      ;; Replace apostrophes (') with a closing single quote (’)
      (setq sanitized (replace-regexp-in-string "'" "’" sanitized))

      ;; Replace single quotes (') used for quotation (like in "quoted") with opening and closing quotes
      (setq sanitized (replace-regexp-in-string "‘" "’" sanitized))

      ;; Handle single quotes in words, replace with opening and closing quotes based on position
      (setq sanitized (replace-regexp-in-string "'" 
                                                (lambda (x)
                                                  (if (or (zerop (length x)) (not (string-match "[a-zA-Z0-9]" (substring x 0 1))))
                                                      "’"  ;; closing single quote
                                                    "‘")) sanitized))

      ;; Handle straight double quotes (") by replacing them with opening or closing smart quotes
      (setq sanitized (replace-regexp-in-string "\"" 
                                                (lambda (x)
                                                  (if (zerop (length x)) 
                                                      "“"  ;; opening double quote
                                                    (if (string-match "[a-zA-Z0-9]" (substring x 0 1))
                                                        "”"  ;; closing double quote
                                                      "“"))) sanitized))

      sanitized)))



(provide 'sync0-bibtex-sql-utils)
