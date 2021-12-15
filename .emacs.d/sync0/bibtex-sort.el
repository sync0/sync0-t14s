(defun sort-bibtex-entries ()
  "Sort a BibTeX bibliography file by the cite keys.  @string{} entries
at the start of the file are left in place.  The remaining entries are
sorted, and left with one intervening blank line between each of them.
"
  (interactive)
  (let ((sort-tag "\001SORT-TAG:"))	;sort-tag is a unique prefix string
    (goto-char (point-min))
    (insert "\f" sort-tag "\001\n")	;Ctl-A tag ensures we keep @string{}
    (while				;stuff in first page
	(re-search-forward "^@string" nil t)) ;skip past @string{} entries
    (save-excursion
      (while				;find bibliography entry
	  (re-search-forward "^@[^{]*{" nil t)
	(progn
	  (let ((the-tag))
	    (let ((k (point)))
	      (end-of-line)
	      (setq the-tag (buffer-substring k (point))))
	    (beginning-of-line)
	    (let ((k (point)))
	      (insert "\f" sort-tag the-tag "\n") ;and tag it for sorting
	      (upcase-region k (point))) ;alas, sort is case-sensitive
	    (forward-line 1)))))
    (sort-pages nil (point-min) (point-max)) ;do the sort
    (goto-char (point-min))
    (flush-lines sort-tag))		;remove the sort tags
  (goto-char (point-min))
  (replace-string "\f" "")		;remove left-over page marks
  (goto-char (point-min))
  (while
      (re-search-forward "^@string" nil t)) ;skip past @string{} entries
  (while				;find bibliography entry
      (re-search-forward "^@[^{]*{" nil t)
    (progn
      (beginning-of-line)
      (insert "\n\n")			;and regularize space before it
      (forward-line -1)
      (delete-blank-lines)
      (forward-line 2)))
  (goto-char (point-max))
  (delete-blank-lines)
  (goto-char (point-min))
  (replace-regexp "[ \t]+$" "")		;blank trim lines
  (goto-char (point-min))
  )
