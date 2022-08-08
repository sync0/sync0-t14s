(defun sync0-insert-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (insert (concat (car list) "\n"))
    (setq list (cdr list))))

(defun sync0-show-elements-of-list (list sep)
  "Print massive string with each element of list separated by sep"
(unless (equal (length list) 0)
  (let (x)
    (while list
      (setq x (concat (car list) sep x))
      (setq list (cdr list)))
    (string-trim-right x sep))))

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

(provide 'sync0-functions)


