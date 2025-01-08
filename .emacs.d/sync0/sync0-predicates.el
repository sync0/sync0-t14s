
(defun sync0-bibtex-has-attachments-p (bibkey)
  "Check if there are any attachments associated with BIBKEY.
Returns t if attachments are found, nil otherwise."
  (let ((attachment-dir sync0-zkn-attachments-dir)
        (key-prefix (concat "^" (regexp-quote bibkey))))
    (if (file-exists-p attachment-dir)
        (directory-files attachment-dir nil key-prefix)
      nil)))

(defun sync0-has-dups-p (LIST) ""
       (let ((unique1 (remove-duplicates LIST :test #'equal)))
     (if (eq LIST unique1)
         nil
       t)))

(defun sync0-null-p (var)
"General purpose predicate to determine whether an object var is
empty (not in the lisp sense but in a human-readable sense)."
  (cond ((stringp var)
         (or (string= var "")
             (string= var "nil")))
        ((listp var)
          (or (null var)
              (equal var '(""))))
          (t (null var))))

(defun sync0-validate-path (path)
  "Ensure the input path is valid for copying a file.
If PATH is a directory, append a slash (\"/\") to indicate it's a directory.
Check if the directory is writable. If PATH is not writable or invalid, prompt again."
  (while (or (not (file-exists-p path))
             (and (file-directory-p path) (not (file-writable-p path))))
    (setq path (read-string (format "Path %s is not valid or writable. Please enter a valid path: " path))))
  (if (file-directory-p path)
      (concat (file-name-as-directory path))
    path))

(defun sync0-file-has-extension-p (file extension)
  "Check whether FILE has the given EXTENSION."
  (let ((actual-extension (file-name-extension file)))
    (string= actual-extension
             (if (string-prefix-p "." extension)
                 (substring extension 1)
               extension))))

(provide 'sync0-predicates)
