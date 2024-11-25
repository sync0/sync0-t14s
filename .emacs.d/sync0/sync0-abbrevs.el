
(use-package abbrev
  :straight nil
  :custom
  ;; Tell Emacs where to read abbrevs.  
  (abbrev-file-name (concat user-emacs-directory "abbrev_defs"))
  ;; Save abbrevs when files are saved.
  (save-abbrevs t)
  ;; Don't notify when abbrevs are saved.
  (save-abbrevs 'silently)
  ;; Accept ' as a word constituent. 
  (dabbrev-abbrev-char-regexp  "\\sw")
  :config 
  ;; (require 'sync0-language)
  ;; Avoid errors when reading abbrev_defs.
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  ;; Avoid expansion character insertion. 
  ;; Use this function on a per-abbrev basis.
  ;; This is the "hook" function
  (defun dont-insert-expansion-char ()  t) 
  ;; The hook should have a "no-self-insert" property set 
  (put 'dont-insert-expansion-char 'no-self-insert t) 

  ;; Initialize abbrev-mode by default. 
  (setq-default abbrev-mode t)

(defun sync0-abbrev-read-into-list (str)
  "Remove the first and last lines of a multi-line string and return the abbrevs as Lisp lists."
  (let ((lines (split-string str "\n")))  ; Split the string into lines
    (if (> (length lines) 2)
        (mapcar 'read (seq-drop (butlast lines 1) 2))  ; Drop the first and last line, and read each line into a list
      nil)))  ; Return nil if there are less than 3 lines

(defun sync0-abbrev-load ()
  "Load abbrevs from the abbrev file into `sync0-abbrev-cache`."
  (setq sync0-abbrev-cache nil)  ; Clear any existing cache
  (with-temp-buffer
    (insert-file-contents abbrev-file-name)
    (goto-char (point-min))
    (let ((found-tables))  ; List to hold all abbrev tables found
      (while (re-search-forward "^(define-abbrev-table '\\(.+\\)-mode-abbrev-table$" nil t)  ; Find each abbrev table
        (let* ((start (point))
	       (mode (match-string 1))
	       (end (progn (re-search-forward "^   ))$" nil t)  ; Find the corresponding end position
                           (point)))
	       (table-entries (buffer-substring-no-properties start end))
	       (abbrevs (sync0-abbrev-read-into-list table-entries))
	       (mode-string (concat mode "-mode-abbrev-table")))
          (push (cons mode-string abbrevs) found-tables)))
      (if (null found-tables)
          (message "No abbrev tables found in the abbrev file.")
        (setq sync0-abbrev-cache (reverse found-tables))))))

(sync0-abbrev-load)

(defun sync0-abbrev-save-to-file ()
  "Save the abbrev cache into a file, formatted as a Lisp file."
  (with-temp-buffer
    (insert ";;-*-coding: utf-8;-*-\n")
    (dolist (entry sync0-abbrev-cache)
      (let ((mode (car entry))
            (abbrevs (cdr entry)))
        ;; Write the abbrev table definition for each mode
        (insert (format "(define-abbrev-table '%s\n  '(\n" mode))
        (dolist (abbrev abbrevs)
	  (let ((name (car abbrev))
		(expansion (nth 1 abbrev))
		(hook (nth 2 abbrev))
		(num (car (last abbrev))))
            (insert (format "    (\"%s\" \"%s\" %s :count %s)\n" name expansion hook num))))
        (insert "   ))\n\n")))
    ;; Save the buffer to the specified file
    (write-file abbrev-file-name)
    (message "Abbrevs saved to %s" abbrev-file-name)))



(defun sync0-define-local-abbrev (name expansion &optional hook)
  "Define a new abbrev in the current mode and update the cache."
  (interactive "sEnter abbrev:\nsEnter expansion:")
  (if (bound-and-true-p sync0-language-active-table)
      (let* ((mode sync0-language-active-table)
             (mode-table (concat mode "-abbrev-table"))
             (mode-cache (assoc mode-table sync0-abbrev-cache))
             (new-abbrev (if hook
			     (list name expansion 'dont-insert-expansion-char :count 0)
			   (list name expansion nil :count 0))))
        ;; Check if the mode's abbrev cache exists, otherwise create it
        (unless mode-cache
          (setq sync0-abbrev-cache
                (cons (cons mode-table nil) sync0-abbrev-cache))
          (setq mode-cache (assoc mode-table sync0-abbrev-cache)))  ;; Re-fetch the mode cache after it's created

        ;; Replace or add the abbrev in the cache
        (if (assoc name (cdr mode-cache))  ;; If abbrev already exists, replace it
            (setcdr mode-cache
                    (cons new-abbrev (assq-delete-all name (cdr mode-cache))))
          ;; Otherwise, add the new abbrev to the cache
          (setcdr mode-cache (cons new-abbrev (cdr mode-cache))))

        ;; Define or redefine the abbrev in the local-abbrev-table
        (define-abbrev local-abbrev-table name expansion)
        (define-abbrev (symbol-value (intern mode-table)) name expansion)
        (message "%s now expands to %s in %s." name expansion mode-table))
    (error "No local abbrev language table active.")))

  (add-hook 'kill-emacs-hook #'sync0-abbrev-save-to-file))

(provide 'sync0-abbrevs)
