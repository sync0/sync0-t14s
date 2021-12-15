;/u/sy/beebe/emacs/bibtex-support.el, Mon Jul 10 14:10:16 1989
;Edit by Nelson H.F. Beebe <beebe@plot79.utah.edu>
; This file contains miscellaneous functions for cleaning and standardizing
; BibTeX file entries.

(defun bibtex-clean-buffer ()
  "Run bibtex-clean-entry on entire buffer, then run
bibtex-normalize-entries and bibtex-normalize-pageranges."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^@" nil t)
    (if (not (looking-at "[sS][tT][rR][iI][nN][gG]"))
	(bibtex-clean-entry)
      (forward-line 1)))
  (bibtex-normalize-entries)
  (bibtex-normalize-pageranges))

(defun bibtex-normalize-entries ()
  "Normalize BibTeX entries by converting each bibliography keyword to
  lower case with consistent standard indentation, and capitalize each entry
  type."
  (interactive)
  (goto-char (point-min))		;fix keywords
  (while (re-search-forward "^[ \t]*\\([A-Za-z]+\\)[ \t]*=[ \t]*"
			    (point-max) t)
    (downcase-region (match-beginning 1) (match-end 1))
    (let ((tag (buffer-substring (match-beginning 1) (match-end 1))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert-string "    " tag "\t= "))
    (forward-line))
  (goto-char (point-min))		;fix entry names
  (while (re-search-forward "^@\\([A-Za-z]+\\)[ \t]*{" nil t)
    (beginning-of-line)
    (forward-char 1)
    (capitalize-word 1))
  (goto-char (point-min))
  (replace-regexp "[ \t]+$" "")		;blank trim lines
  (goto-char (point-min))
  
  )

(defun bibtex-normalize-pageranges ()
  "Normalize page ranges in BibTeX entries to use TeX en-dash (--) instead
of hyphen (-) or em-dash (--)."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward
	  "^[ \t]*pages[ \t]*=[ \t]*[\"{][ ]*\\([0-9]+\\)\\([-]+\\)\\([0-9]+\\)" nil t)
    (delete-region (match-beginning 2) (match-end 2))
    (goto-char (match-beginning 2))
    (insert-string "--"))
  )


(defun bibtex-quote-fields ()
  "Change braced keyword = {value} fields to keyword = \"value\" style so
BibTeX editing support can be used."
  (interactive)
  (while (re-search-forward "^[ \t]*\\([A-Za-z]+\\)[ \t]*=[ \t]*" nil t)
    (if (looking-at "{")
	(let ((brace-count 1))
	  (delete-char 1)
	  (insert-string "\"")
	  (while (and
		  (not (= brace-count 0))
		  (re-search-forward "[{}]" nil t))
	    (backward-char 1)
	    (if (looking-at "{")
		(setq brace-count (1+ brace-count))
	      (setq brace-count (1- brace-count)))
	    (forward-char 1))
	  (backward-char 1)
	  (if (looking-at "}")
	      (progn 
		(delete-char 1)
		(insert-string "\"")))))))
