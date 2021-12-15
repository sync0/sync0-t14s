(defun BibTeX-brace-words (&optional word-count)
  "Insert { ... } around next ARG words (on \\<BibTeX-mode-map>\\[BibTeX-font-tt])."
  (interactive)
  (setq word-count (internal-BibTeX-default word-count 1))
  (internal-BibTeX-wrap "{" "}" word-count)
 )

(defun internal-BibTeX-default (value default-value)
  "Return a non-nil value from VALUE, current-prefix-arg, or
DEFAULT-VALUE, in that order.  Do

   (setq argvar (internal-BibTeX-default argvar 23))

to set &optional argvar to (a) argvar, if argvar is non-nil, else (b)
current-prefix-arg, if that is non-nil, else (c) 23.  This simplifies
optional argument assignment in many functions."
  (cond
   ((not (null value))			;do nothing if explicit arg
    value)
   ((not (null current-prefix-arg))	;else use prefix arg if any
    (prefix-numeric-value current-prefix-arg))
   (t					;else no arg at all, so use default
    default-value)
   )
  )

(defun internal-BibTeX-wrap (prefix suffix word-count)
  "Wrap the next WORD-COUNT words with PREFIX and SUFFIX."
  (if (> word-count 0) ;do something only if word-count > 0
      (progn
	(forward-word 1)
	(forward-word -1) ;position to start of next word
	(insert prefix)
	(forward-word word-count)
	(insert suffix)))
)

(local-set-key "\C-c\C-e{" 'BibTeX-brace-words)
