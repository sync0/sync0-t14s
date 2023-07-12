(require 'sync0-bibtex-vars)
(require 'cl-lib)

(sync0-set-variable-from-files sync0-bibtex-completion-variables-alist)

(with-temp-buffer
  (insert-file-contents sync0-bibtex-keys-file)
  (goto-char (point-min))
  (let (x)
    ;; (keep-lines "contexts" (point-min) (point-max)) 
    (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
      (push (match-string-no-properties 1) x))
    (setq sync0-bibtex-keys x)))

;; load initial value of sync0-bibtex-today-keys
(setq sync0-bibtex-today-keys
      (cl-delete-if-not
       (lambda (x) (string-match-p (concat "^" (format-time-string "%y%-j"))  x)) sync0-bibtex-keys))

(defun sync0-bibtex-populate-keys ()
  "Load all bibtex keys on variable sync0-bibtex-keys."
  (interactive)
  ;; Load variable sync0-bibtex-keys extracting the keys from the
  ;; bibtex-completion-candidates
  (setq sync0-bibtex-keys (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                                  (bibtex-completion-candidates)))
  ;; Write contents of sync0-bitex-keys to file
  (with-temp-file sync0-bibtex-keys-file
    (sync0-insert-elements-of-list sync0-bibtex-keys))
  ;; Rewrite file in Gdrive to allow for manipulation in iOS
  (copy-file sync0-bibtex-keys-file sync0-bibtex-keys-backup-file t))

(defun sync0-random-alnum ()
  "From a defined alphabet variable (sync0-alpha), take one
character at random."
  (let* ((i (% (abs (random)) (length sync0-alpha))))
    (substring sync0-alpha i (1+ i))))

(defun sync0-bibtex-duplicate-entry-key-p (key &optional daily)
  (if daily
      (member key sync0-bibtex-today-keys)
    (member key sync0-bibtex-keys)))

(defun sync0-bibtex-add-key-to-keyvars (key) 
  (setq sync0-bibtex-keys (cons key sync0-bibtex-keys))
  (setq sync0-bibtex-today-keys (cons key sync0-bibtex-today-keys)))

(defun sync0-bibtex-entry-key-define (&optional daily lengthy)
  "Create new bibtex key following a pre-defined rule. In this
case, keys are outputed in the YYMMDDxx format, in which the last
two characters are produced using the sync0-random-alnum function
to produce random characters."
  (let* ((iterations (or lengthy sync0-bibtex-default-key-length))
         ;; 2 is the default value 
         (random-alnum (loop repeat iterations concat (sync0-random-alnum)))
         (new-key (concat sync0-bibtex-timeday random-alnum)))
    (if (sync0-bibtex-duplicate-entry-key-p new-key daily)
        ;; Call recursively the function again
        (sync0-bibtex-entry-key-define daily)
      (progn 
        (sync0-bibtex-add-key-to-keyvars new-key)
        (format "%s" new-key)))))

(defun sync0-bibtex-entry-define-keys-list (times &optional daily) 
  "Create as many new keys as requested. They are put on a list"
  (let (values)
    (dotimes (i times)
      (push (sync0-bibtex-entry-key-define daily) values))
    (if (sync0-duplicates-p values)
        (sync0-bibtex-entry-define-keys-list times daily)
      values)))

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

(defun sync0-bibtex-entry-key-redefine (&optional suggester)
  "Recreate new bibtex key following a pre-defined rule."
  (if (yes-or-no-p "Create automatic key?")
      (sync0-bibtex-entry-key-define t)
    (let ((x (read-string "New key : " suggester)))
      (while (member x sync0-bibtex-keys) 
        (message "Invalid key. %s is already present in database." x)
        (setq x (read-string "New key : " suggester)))
      (sync0-bibtex-add-key-to-keyvars)
      (format "%s" x))))

(defun sync0-bibtex-update-key-in-buffer (oldkey newkey)
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward oldkey nil t)
      (replace-match newkey))))

(defun sync0-bibtex-completion-choose-key (&optional unique pointer)
  "Choose key with completion. When optional pointer is t,
   preselect the entry at point."
  (if unique
      (let* ((entry (when (or pointer
                              (string= major-mode "bibtex-mode"))
                      (save-excursion (bibtex-beginning-of-entry)
			              (bibtex-parse-entry))))
             (preselect (when entry
                          (cdr (assoc "=key=" entry))))
             (candidates (bibtex-completion-candidates))
             (selection (ivy-read "Bibliography candidates: "
                                  candidates
                                  :preselect preselect
                                  :caller 'ivy-bibtex
                                  :history 'ivy-bibtex-history)))
        (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
    (let* ((entry (when (or pointer
                            (string= major-mode "bibtex-mode"))
                    (save-excursion (bibtex-beginning-of-entry)
			            (bibtex-parse-entry))))
           (preselect (when entry
                        (cdr (assoc "=key=" entry))))
           (candidates (bibtex-completion-candidates))
           (counter 0)
           selection
           x)
      (while (not (equal selection "nilnil"))
        (setq selection (ivy-read (format "Bibliography candidates%s: "
                                          (if (> counter 0)
                                              (concat " (selected: " (number-to-string counter) ")") ""))
                                  candidates
                                  :preselect preselect
                                  :caller 'ivy-bibtex
                                  :history 'ivy-bibtex-history))
        (setq counter (1+ counter))
        (setq x (concat (cdr (assoc "=key=" (cdr (assoc selection candidates)))) ", " x)))
      (if (equal counter 1)
          (format "%s" x)
        (format "%s" (substring x 2 -2))))))

(provide 'sync0-bibtex-key-functions)
