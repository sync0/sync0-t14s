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

;; (defun sync0-bibtex-populate-keys ()
;;   "Load all bibtex keys on variable sync0-bibtex-keys."
;;   (interactive)
;;   ;; Load variable sync0-bibtex-keys extracting the keys from the
;;   ;; bibtex-completion-candidates
;;   (setq sync0-bibtex-keys (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
;;                                   (bibtex-completion-candidates)))
;;   ;; Write contents of sync0-bitex-keys to file
;;   (with-temp-file sync0-bibtex-keys-file
;;     (sync0-insert-elements-of-list sync0-bibtex-keys))
;;   ;; Rewrite file in Gdrive to allow for manipulation in iOS
;;   (copy-file sync0-bibtex-keys-file sync0-bibtex-keys-backup-file t))

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
  "Generate a new BibTeX key following a predefined rule.
The key format is YYMMDDxx, where 'xx' are random alphanumeric characters.
If DAILY is non-nil, ensure uniqueness for the current day.
If LENGTHY is non-nil, specify the length of random characters."
  (let* ((iterations (or lengthy sync0-bibtex-default-key-length))
         ;; 2 is the default value 
         (random-string (sync0-generate-random-string iterations))
         (new-key (concat sync0-bibtex-timeday random-string)))
    (if (sync0-bibtex-duplicate-entry-key-p new-key daily)
        (sync0-bibtex-entry-key-define daily iterations)
      (progn 
        (sync0-bibtex-add-key-to-keyvars new-key)
        new-key))))

(defun sync0-bibtex-entry-define-keys-list (times &optional daily) 
  "Generate a list of new BibTeX keys.
The keys are unique and put in a list.
If DAILY is non-nil, ensure uniqueness for the current day."
  (let (keys)
    (dotimes (_ times)
      (push (sync0-bibtex-entry-key-define daily) keys))
    (unless (sync0-duplicates-p keys)
      keys)))

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

;; (defun sync0-bibtex-completion-choose-key (&optional unique pointer query-message use-cache)
;;   "Choose key with completion. When optional pointer is t,
;; preselect the entry at point. If UNIQUE is non-nil, return a single key, otherwise allow multiple selections."
;;   (let* ((entry (when (or pointer
;;                           (string= major-mode "bibtex-mode"))
;;                   (save-excursion
;;                     (bibtex-beginning-of-entry)
;;                     (bibtex-parse-entry))))
;;          (preselect (if entry
;; 			(cdr (assoc "=key=" entry))
;; 		    sync0-bibtex-choose-key-cache))
;;          (candidates (if use-cache
;;                          sync0-bibtex-completion-candidate-cache
;;                        (setq sync0-bibtex-completion-candidate-cache (bibtex-completion-candidates)))))
;;     (if unique
;;         (let ((selection (ivy-read (or query-message
;;                                        "Bibliography candidates: ")
;;                                    candidates
;;                                    :preselect preselect
;;                                    :caller 'ivy-bibtex
;;                                    :history 'ivy-bibtex-history)))
;; 	  (setq sync0-bibtex-choose-key-cache selection)
;;           (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
;;       (let ((counter 0)
;;             (selection nil)
;;             (result ""))
;;         (while (not (equal selection "nilnil"))
;;           (setq selection (ivy-read (format (concat (or query-message
;;                                                         "Bibliography candidates")
;;                                                     "%s: ")
;;                                             (if (> counter 0)
;;                                                 (concat " (selected: " (number-to-string counter) ")") ""))
;;                                     candidates
;;                                     :preselect preselect
;;                                     :caller 'ivy-bibtex
;;                                     :history 'ivy-bibtex-history))
;;                 counter (1+ counter)
;;                 result (concat (cdr (assoc "=key=" (cdr (assoc selection candidates)))) ", " result)))
;;         (if (= counter 1)
;;             (format "%s" result)
;;           (format "%s" (substring result 2 -2))))))

(provide 'sync0-bibtex-key-functions)
