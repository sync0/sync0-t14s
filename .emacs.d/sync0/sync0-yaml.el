(require 'yaml-mode)

(defun sync0-parse-yaml-front-matter ()
  "Parse the YAML front matter in the current buffer into a hash-table.
Each key in the hash-table corresponds to a YAML property, with values stored as associated values.
Invalid or malformed YAML front matter is ignored and nil is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^---$" nil t)  ;; Check for YAML start
      (let ((start (point)))
        (when (re-search-forward "^---$" nil t)  ;; End of YAML front matter
          (let ((yaml-content (buffer-substring-no-properties start (point))))
            ;; Remove the final `---` if present, which is part of YAML syntax
            (setq yaml-content (replace-regexp-in-string "^---$" "" yaml-content))
            ;; Parse the cleaned YAML content using yaml.el
            (condition-case err
                (let ((parsed-yaml (yaml-parse-string yaml-content)))
                  (if (hash-table-p parsed-yaml)
                      ;; Return the parsed hash-table
                      parsed-yaml
                    (error "Parsed result is not a hash-table: %S" parsed-yaml)))
              (error (message "Error parsing YAML: %s" err)
                     nil))))))))

(defun sync0-yaml-get-property (property)
  "Retrieve the values associated with PROPERTY in the YAML front matter.
The property is looked up in the YAML hash-table. If PROPERTY is a string, it's converted to a symbol before lookup."
  (let ((yaml-hash-table (sync0-parse-yaml-front-matter)))
    (let ((property-symbol (if (stringp property)
                               (intern property)  ;; Convert string to symbol
                             property)))        ;; If it's already a symbol, keep it
      (gethash property-symbol yaml-hash-table))))

(defun sync0-parse-yaml-keys (file)
  "Parse FILE for YAML 'key', 'title', and 'aliases' values from a hash-table.
Returns nil if the YAML front matter is invalid."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((yaml (sync0-parse-yaml-front-matter)))
      (if yaml
          (list :file file
                :key (gethash 'key yaml)  ;; Retrieve 'key' from the hash table
                :title (gethash 'title yaml)  ;; Retrieve 'title' from the hash table
                :aliases (gethash 'aliases yaml))
        nil))))

(defun sync0-extract-yaml-property (property file-path)
  "Extract the value of a YAML PROPERTY from the top of a markdown file."
  (with-temp-buffer
    (insert-file-contents file-path)
    ;; Check if the file starts with a YAML section
    (when (re-search-forward "^---" nil t)
      (let (yaml-end yaml-alist)
        ;; Look for the end of the YAML section
        (if (re-search-forward "^---" nil t)
            (setq yaml-end (point))
          (error "No closing YAML delimiter found"))
        ;; Extract the YAML section and split into lines
        (let ((yaml-section (buffer-substring-no-properties (point-min) yaml-end)))
          (dolist (line (split-string yaml-section "\n" t))
            ;; Create alist from property-value pairs
            (when (string-match "^\\([^:]+\\):[[:blank:]]*\\(.+\\)" line)
              (let ((key (match-string 1 line))
                    (value (match-string 2 line)))
                (push (cons key (sync0-delimiter-remover value)) yaml-alist))))
          ;; Return the value of the requested property
          (cdr (assoc property yaml-alist)))))))

(provide 'sync0-yaml)
