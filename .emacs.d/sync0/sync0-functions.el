(require 'sync0-vars)

(defun sync0-insert-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (insert (concat (car list) "\n"))
    (setq list (cdr list))))

(defun sync0-show-elements-of-list (list sep)
  "Print massive string with each element of list separated by sep"
  (cond
   ((listp list)
    (if (> (length list) 1)
	(let (x)
	  (while list
	    (setq x (concat (car list) sep x))
	    (setq list (cdr list)))
	  (string-trim-right x sep))
      (car list)))
   ((stringp list)
    list)
   ((symbolp list)
    (symbol-name list))
   (t
    (error "Unknwon input to sync0-show-elements-of-list."))))

(defun sync0-update-list (newelt list file)
  "Saves my projects in my home folder."
  (if (member newelt list)
      (message "%s already exists in %s" newelt file)
    (let ((file-path
           (concat "~/.emacs.d/sync0-vars/" file ".txt"))
          (new-list (cons newelt list)))
      ;; (add-to-list list newelt)
      (sync0-redefine list new-list)
      (with-temp-file file-path
        (sync0-insert-elements-of-list list)
        (save-buffer)
        (message "%s added to %s" newelt file)))))

(defun sync0-split-string-with-separator (string separator)
  "Check the presence of a separator in current string and split
when necessary."
  (interactive)
;; check for the presence of a separator
  (if (string-match-p separator string)
      (string-trim
       (prin1-to-string
        (split-string-and-unquote string separator))
       "(" ")")
    string))

;; Useful function to deal with strings: 
;; Taken from: https://emacs.stackexchange.com/questions/36200/split-line-every-n-characters

(defun split-string-every (string chars)
  "Split STRING into substrings of length CHARS characters.
    This returns a list of strings."
  (cond ((string-empty-p string)
         nil)
        ((< (length string)
            chars)
         (list string))
        (t (cons (substring string 0 chars)
                 (split-string-every (substring string chars)
                                     chars)))))

;; (defun sync0-string-split-with-sep-and-list (string sep &optional to-string)
;;   "Split a string into a list using sep. When optional to-string is
;; true, produce a string and not a list."
;;   (cl-flet  ((conditional () (if to-string
;;                                  string
;;                                (list string))))
;;     (if (string-match sep string)
;;         (progn 
;;           (split-string string sep)
;;           (conditional))
;;       (conditional))))

(defun sync0-string-split-with-sep-and-list (string sep &optional to-string)
  "Split a string into a list using sep. When optional to-string is
true, produce a string and not a list."
  (if (string-match sep string)
      (if to-string
          (format "%s" (split-string string sep))
        (split-string string sep))
    (if to-string
        string
      (list string))))

(defun sync0-add-prefix-to-list-convert-to-string (my-string separator prefix &optional postfix)
  "Break my-string using separator. Then, add prefix to every
element of the resulting list. When postfix is set, also add
postfix to every element. The product of the function is a string
of all elements separeted by separator."
  (let* ((old-list (split-string my-string separator))
         (new-list (let (x)
                     (dolist (element old-list x)
                       (if postfix 
                           (push (concat prefix element postfix) x)
                         (push (concat prefix element) x))))))
    (sync0-show-elements-of-list new-list separator)))

;; Taken from
;; https://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(defun how-many-str (regexp str)
  "Count number of occurences of regexp in str."
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

;; Taken from
;; https://stackoverflow.com/questions/34432246/how-to-read-contents-of-the-file-programmatically-in-emacs

(defun sync0-read-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun sync0-duplicates-p (list)
  "Check existence of duplicates in list"
  (not (equal (remove-duplicates list) list)))

;; (defun sync0-define-list-interactively (define-string continue-string &optional initial-input)
;;   (interactive)
;;   (or initial-input
;;       (let (x)
;;         (push (read-string define-string) x)
;;         (while (yes-or-no-p continue-string) 
;;           (push (read-string define-string) x))
;;         (if (> (length x) 1)
;;             x
;;           (car x)))))

(defun sync0-define-list-interactively (prompt-string continue-string &optional initial-input)
  "Interactively define a list of strings.
  
PROMPT-STRING is the prompt to display when defining each element of the list.
CONTINUE-STRING is the prompt to display when asking whether to continue defining more elements.
INITIAL-INPUT is an optional initial value or list of values.

If INITIAL-INPUT is a list, it's used as the initial value.
If INITIAL-INPUT is a string, it's converted to a single-element list.
If no INITIAL-INPUT is provided, the user is prompted to enter the first element.

When region is active, a list is automatically created, in which
each element is every new-line-separated string in the region.

Returns a list of strings."
  (let ((input (if (and initial-input (listp initial-input))
                   initial-input
                 (if (stringp initial-input)
                     (list initial-input)
                   (if (use-region-p)
                       (split-string (buffer-substring (region-beginning) (region-end)) "\n" t)
                     nil)))))
    (if input
        input
      (let ((result (list (read-string prompt-string))))
        (while (yes-or-no-p continue-string)
          (push (read-string prompt-string) result))
        (nreverse result)))))

;; (defun sync0-define-list-interactively (define-string continue-string &optional initial-input)
;;   (cond ((and initial-input 
;;               (listp initial-input))
;;          initial-input)
;;         ((and initial-input 
;;               (stringp initial-input))
;;          (list initial-input))
;;         (t 
;;          (let (x)
;;            (push (read-string define-string) x)
;;            (while (yes-or-no-p continue-string) 
;;              (push (read-string define-string) x))
;;            x))))

;; (defun sync0-define-list-interactively (define-string continue-string &optional initial-input)
;;   (interactive)
;;   (or initial-input
;;       (let (x)
;;         (push (read-string define-string) x)
;;         (while (yes-or-no-p continue-string) 
;;           (push (read-string define-string) x))
;;             x)))

;; from https://www.reddit.com/r/emacs/comments/weuiqs/deleting_files_in_emacs_oc/
(defun sync0-delete-this-file (&optional forever)
  "Delete the file associated with `current-buffer'.
If FOREVER is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let ((file (or (buffer-file-name)
                       (user-error "Current buffer is not visiting a file")))
             ((y-or-n-p "Delete this file? ")))
    (delete-file file (not forever))
    (kill-buffer (current-buffer))))

(defun sync0-correct-string-with-corrector (stringy corrector &optional end)
  "Correct stringy with corrector. Matches corrector to the
beginning of the stringy by default unless optional end is set to
true."
  (let ((corrector-regex (if end
                             (concat corrector "$")
                           (concat "^" corrector))))
    (if (string-match corrector-regex stringy)
        stringy
      (if end 
          (concat stringy corrector)
        (concat corrector stringy)))))

;; (defun sync0-erase-file-contents (file-path)
;;   "Erase the contents of the specified file."
;;   (interactive "FEnter file path: ") ; Use "F" to prompt for a file
;;   (with-temp-buffer
;;     (write-region "" nil file-path)))

(defun sync0-erase-file-contents (file-path &optional optional-content)
  "Erase the contents of the specified file.
If OPTIONAL-CONTENT is provided, replace the old content with it."
  (interactive "FEnter file path: ") ; Use "F" to prompt for a file
  (with-temp-buffer
    (insert (or optional-content ""))
    (write-region (point-min) (point-max) file-path)))

(defun sync0-completion-finish-key (command)
  "Return the key binding that finishes a completion command.
COMMAND is the command to finish, one of the symbols
`completing-read' or `read-file-name'."
  (cond
   ((and (boundp 'selectrum-mode) selectrum-mode) (key-description (where-is-internal 'selectrum-submit-exact-input (list selectrum-minibuffer-map) 'non-ascii)))
   ((and (boundp 'ivy-mode) ivy-mode) (key-description (where-is-internal 'ivy-immediate-done (list ivy-minibuffer-map) 'non-ascii)))
   ((and (boundp 'helm-mode) helm-mode) (let ((map (symbol-value (alist-get command '((completing-read . helm-comp-read-map)
										      (read-file-name . helm-read-file-map))))))
					  (key-description (where-is-internal 'helm-cr-empty-string (list map) 'non-ascii))))
   (t (key-description [return]))))

;; (defun sync0-completing-read-collection (collection)
;;   "Read keywords with completion from COLLECTION.
;; Return the keywords entered as a list.  If no keywords are
;; entered, the return value is nil."
;;   (let* ((prompt (format "Add projects (%s to finish) [%%s]" (sync0-completion-finish-key 'completing-read))))
;;     (cl-loop for project = (completing-read (format prompt (mapconcat #'identity collection " "))
;; 					    collection) 
;;              until (string= project "")
;;              collecting (let ((matching-code (car (rassoc project sync0-projects-alist))))
;;                           (if matching-code
;;                               matching-code
;;                             project))
;;              into projects
;;              finally return projects)))

(defun sync0-process-bibkeys (keys)
  "Process the KEYS argument into a list of strings."
  (cond
    ;; Case 1: List of strings or symbols
   ((listp keys)
    (if (> (length keys) 1)
	(mapcar (lambda (k)
		  (if (symbolp k)
                      (symbol-name k)
                    k))
		keys)
      (car keys)))
    ;; Case 2: Single string
    ((stringp keys)
      keys)
    ;; Case 3: Other data types
    (t
     (error "You must supply either a list or a string as keys."))))

(defun sync0-eval-last-sexp-or-region ()
  "Evaluate the last sexp if no region is active, otherwise evaluate the region."
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end))
    (eval-last-sexp nil)))

(defun sync0-string-member (element string &optional separator)
  "Check if ELEMENT is a member of STRING when split by SEPARATOR. Default separator is ', '."
  (unless separator (setq separator ", "))
  (let ((list (split-string string separator)))
    (if list
        (member element list)
      (string= element string))))

(defun sync0-generate-random-string (length)
  "Generate a random string of LENGTH characters."
  (let ((charset sync0-alpha))
    (cl-loop for i below length
             concat (string (elt charset (random (length charset)))))))

(defun sync0-generate-unique-filename (existing-files)
  "Generate a unique filename that does not exist in EXISTING-FILES."
  (let ((current-date sync0-bibtex-timeday))
    (cl-loop for i from 0
             for filename = (concat current-date (sync0-generate-random-string 3))
             until (not (member filename existing-files))
             finally return filename)))

;; (defun sync0-obsidian-generate-unique-filename (directory)
;;   "Generate a unique filename for an Obsidian markdown note in DIRECTORY."
;;   (let* ((current-date (format-time-string "%y%j"))
;;          (regex (concat "^" current-date "[a-zA-Z]\\{3\\}$"))
;;          (existing-files (sync0-obsidian-find-existing-markdown-files directory)))
;;     (cl-loop for i from 0
;;              for filename = (concat current-date (sync0-generate-random-string 3))
;;              until (not (member filename existing-files))
;;              finally return filename)))

(defun sync0-list-files-in-directory (directory &optional full-path extension)
  "Return a newline-separated list of all files in DIRECTORY.
If FULL-PATH is non-nil, return the full paths of the files.
Otherwise, return only the filenames with extensions.
If EXTENSION is non-nil, only include files with that extension."
  (interactive)
  (let ((file-list (directory-files-recursively directory ".*"))
        (result-list nil))
    (when extension
      (setq file-list (cl-remove-if-not
                       (lambda (file)
                         (string= (file-name-extension file) extension))
                       file-list)))
    (setq result-list
          (if full-path
              file-list
            (mapcar 'file-name-nondirectory file-list)))
    (mapconcat 'identity result-list "\n")))

(defun sync0-delete-blank-lines-in-buffer ()
  "Delete all blank lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\n" nil t)
      (replace-match "")))
  (message "Blank lines removed from the buffer."))

(defun sync0-write-to-file (content filepath)
  "Write CONTENT to the file indicated by FILEPATH.
If the file already exists, prompt for confirmation to overwrite.
If the directory doesn't exist, create it."
  (let ((directory (file-name-directory filepath)))
    ;; Ensure the directory exists, create it if necessary
    (unless (file-exists-p directory)
      (make-directory directory t))
    
    ;; If file exists, ask for confirmation to overwrite, otherwise write without prompt
    (if (file-exists-p filepath)
        (if (y-or-n-p (format "File %s already exists. Overwrite?" filepath))
            (with-temp-buffer 
              (insert content)
              (write-file filepath)))
      (with-temp-buffer 
        (insert content)
        (write-file filepath)))))

(defun sync0-downcase-and-no-whitespace (x)
  "Downcase and replace whitespace by _ in the current string"
  (downcase
   (replace-regexp-in-string "[[:space:]-]+" "_" x)))

(defun sync0-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun sync0-create-file-with-content (content filepath)
  (if (file-exists-p filepath)
      (error "%s is already present in file system." filepath)
    (with-temp-buffer 
      (insert content)
      (write-file filepath))))

(defun sync0-split-and-follow-horizontally ()
  " Split the selected window into two side-by-side windows.
  The selected window, which displays the same buffer, is on the
  right."
  (interactive)
  (progn
    (split-window-below)
    (balance-windows)
    (other-window 1)))

(defun sync0-split-and-follow-vertically ()
  " Split the selected window into two windows, one above the other.
  The selected window, which displays the same buffer, is below."
  (interactive)
  (progn
    (split-window-right)
    (balance-windows)
    ;; (sync0-restore-margins)
    (other-window 1)))

(defun sync0-delimiter-remover (string)
  (let (x)
    (setq x string)
    (if (or (string-match "^\"" x)
            (string-match "^\\[" x))
        (progn
          (setq x (substring x 1 -1))
          (sync0-delimiter-remover x))
      x)))

(defun sync0-create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun replace-smart-quotes-regexp (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (mapcar
   (lambda (r)
     (save-excursion
       (replace-regexp (car r) (cdr r) nil beg (min end (point-max)))))
   smart-quote-regexp-replacements))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  ;;(while (search-forward-regexp "- " nil to)
  ;; (replace-match "") nil t)
  ;; add alpha. And replace the alpha.

  (replace-smart-quotes-regexp beg end)
  (format-replace-strings '(("\x201C" . "``")
                            ("“" . "``")
                            ("\x201D" . "''")
                            ("”" . "''")
                            ("\x2018" . "`")
                            ("\x2019" . "'")
                            ("’" . "'")
                            ;;("''" . "\"")
                            ;;("​" . "")
                            ;;("…" . "...")
                            ("…" . "\\ldots")
                            ("..." . "\\ldots")
                            ;;("• " . "- ")
                            ;;(" " . "")
                            ("  " . " "))
                          nil   beg (min end (point-max))))


;; Taken from ESXML emacs mode
;; https://github.com/tali713/esxml/blob/master/esxml.el

(defun string-trim-whitespace (string)
  "A simple function, strips the whitespace from beginning and
end of the string.  Leaves all other whitespace untouched."
  (replace-regexp-in-string
   (rx string-start (* whitespace)
       (group (+? anything))
       (* whitespace) string-end)
   "\\1"
   string))

(provide 'sync0-functions)


