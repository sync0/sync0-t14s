;;; -*-Emacs-Lisp-*-
;;; bibtex-labels.el --- BibTeX citation label support in emacs

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Maintainer: Nelson H. F. Beebe <beebe@math.utah.edu>
;; Created: 09 September 1999
;; Version: 1.00
;; Keywords: BibTeX

;; This file is part of GNU Emacs.

;;; Commentary:

;;; ====================================================================
;;;  @Emacs-Lisp-file{
;;;     author          = "Nelson H. F. Beebe",
;;;     version         = "1.02",
;;;     date            = "17 September 2014",
;;;     time            = "06:15:09 MDT",
;;;     filename        = "bibtex-labels.el",
;;;     address         = "University of Utah
;;;                        Department of Mathematics, 110 LCB
;;;                        155 S 1400 E RM 233
;;;                        Salt Lake City, UT 84112-0090
;;;                        USA",
;;;     telephone       = "+1 801 581 5254",
;;;     FAX             = "+1 801 581 4148",
;;;     URL             = "http://www.math.utah.edu/~beebe",
;;;     checksum        = "53309 556 2515 21990",
;;;     email           = "beebe@math.utah.edu, beebe@acm.org,
;;;                        beebe@computer.org (Internet)",
;;;     codetable       = "ISO/ASCII",
;;;     keywords        = "bibliography, BibTeX, editing support, GNU Emacs",
;;;     license         = "public domain",
;;;     supported       = "yes",
;;;     docstring       = "This library for BibTeX editing support
;;;                        extends the bibtools library with a
;;;                        replacement for the function
;;;                        bibtex-standard-BibNet-citation-label, this
;;;                        new one efficiently generating unique labels,
;;;                        plus additional functions for maintaining and
;;;                        printing the internal table of citation
;;;                        labels.  Two new entries prefixed to the
;;;                        BibTeX-Edit menu provide easy access to
;;;                        important functions in this library.
;;;
;;;                        The checksum field above contains a CRC-16
;;;                        checksum as the first value, followed by the
;;;                        equivalent of the standard UNIX wc (word
;;;                        count) utility output of lines, words, and
;;;                        characters.  This is produced by Robert
;;;                        Solovay's checksum utility.",
;;;  }
;;; ====================================================================


(defconst bibtex-labels-version "1.02 [10-Jun-2012]"
  "Version of bibtex-labels library")


;;; Revision history (reverse chronological order):
;;;
;;; 1.02 [10-Jun-2012]
;;;	Make minor tweak of patterns and code in
;;;	bibtex-standard-BibNet-citation-label to handle a wider
;;;	variety of input.
;;;
;;; 1.01 [20-Apr-2001]
;;;	Update bibtex-standard-BibNet-citation-label to accept an OPTyear
;;;	key/value if a year key/value is missing, and if both are missing
;;;	to produce a clearer error message.  If the search for the start
;;;	of the BibTeX entry fails, issue a better error message than the
;;;	cryptic re-search-backward failing-search message.
;;;
;;; 1.00 [09-Sep-1999]
;;;	Complete and release first version of bibtex-labels library.
;;;     The code has been tested under emacs-19.34 and emacs-20.4.

;;; ====================================================================
;;; This library requires the support of the hash table library, and the
;;; text-property feature introduced in emacs-19.
;;;
;;; The library provides support for generation of unique citation
;;; labels with O(1) cost, something that could not be provided
;;; previously until the hash and primes libraries were written in
;;; February 1999.  Since bibliographies of up to 4000 entries are
;;; encountered, any O(N^2) cost implementation, such as one using emacs
;;; association lists, would be intolerably slow.
;;;
;;; The bibtex-goto-citation-label and bibtex-mouse-goto-citation-label
;;; functions, and the functions bibtex-print-citation-label-table and
;;; internal-bibtex-print-cld-line that write the temporary buffers
;;; acted upon by those first two, deserve some explanation.  Their code
;;; is based very loosely on the undocumented occur-xxx functions in the
;;; emacs-20.y/lisp/replace.el library.
;;;
;;; Each navigable line of the *Citation Label Diagnostics* and *Sorted
;;; Citation Labels* buffers has associated with it two text properties:
;;; the character position in the BibTeX buffer corresponding to the
;;; diagnostic line, and the name of the BibTeX buffer. When a
;;; diagnostic line is selected by either of the latter two functions
;;; above, the text properties of the selected line can be used to find
;;; the required buffer, and position to the associated line in that
;;; buffer.
;;;
;;; The contents of both buffers look much like an *Occur* buffer, so
;;; the user can readily select a line to jump to the corresponding
;;; BibTeX buffer location.
;;;
;;; Because emacs does not maintain line numbers, they have to be
;;; computed on-the-fly.  If the line number of each line with a
;;; citation label is computed in isolation, then for N labels, the cost
;;; is O(N^2).  Thus, the potentially large *Sorted Citation Labels*
;;; buffer intentionally does not record line numbers, so that it can be
;;; created with O(N) cost, and sorted with O(N lg N) cost.  The
;;; *Citation Label Diagnostics* buffer is normally quite small, so line
;;; numbers are generated for it.
;;;
;;; All access to hash-xxx functions is encapsulated in private
;;; functions named internal-bibtex-hash-xxx, for information hiding,
;;; and argument list simplification.
;;;
;;; All complex regular expressions are given symbolic names and defined
;;; and documented in (defconst ...) forms below.  In particular, rather
;;; arbitrary existing citation labels are recognized, but only ones
;;; conforming to the BibNet Project conventions are generated.
;;;
;;; ====================================================================


(provide 'bibtex-labels)

(require 'bibtools) ; only bibtex-standard-BibNet-citation-label is overridden below
(require 'hash)


(defconst bibtex-entry-regexp "^[ \t]*@[ \t]*[A-Za-z]+[ \t]*{[ \t]*"
  "Regular expression matching a BibTeX \"@Name{\" line, with optional
 embedded and/or trailing space.")


(defconst bibtex-entry-label-comma-regexp
  (concat bibtex-entry-regexp "[ \t]*[^,=\"]*[ \t]*,")
  "Regular expression matching a BibTeX \"@EntryName{label,\" line, with
optional embedded space. This must NOT match @Preamble{\"...\"}
@String{name = \"value\"} lines.")


(defconst bibtex-citation-label-regexp "[^ \t,]+"
  "Regular expression matching a citation label.")


(defconst bibtex-citation-label-comma-regexp
  (concat bibtex-citation-label-regexp "[ \t]*,")
  "Regular expression matching a citation label, optional space, and
comma.")


(defconst bibtex-standard-citation-label-regexp "[-A-Za-z0-9:]+"
 "Regular expression matching a BibNet-Project-style citation label.")


(defconst bibtex-standard-citation-label-regexp-with-comma
  (concat bibtex-standard-citation-label-regexp "[ \t]*,")
"Regular expression matching a BibNet-Project-style citation label with
optional trailing space, followed by a comma.")


(defvar bibtex-cld-buffer "*Citation Label Diagnostics*"
  "Name of buffer in which to show duplicate citation labels.")


(defvar citation-label-table nil
  "Hash table containing pairs of (label,position) values.")


(defun bibtex-count-entries ()
  "Return the number of BibTeX entries in the buffer, excluding
@Preamble{...} and @String{...}  specifications.  Ignore any narrowing
in effect, and preserve point."
  (interactive)
  (let ((n 0) (s nil))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward bibtex-entry-regexp nil t)
	  (setq s (match-string 0))
	  (cond

	   ;; Ignore @Preamble{...}
	   ((string-match "preamble" s) nil)

	   ;; Ignore @String{...}
	   ((string-match "string" s) nil)

	   ;; Anything else is a countable entry
	   (t (setq n (1+ n)))))))
    n))


(defun bibtex-goto-citation-label ()
  "With the cursor on a message line in the bibtex-cld-buffer buffer, go
to the position in the BibTeX buffer where the label is found."
  (interactive)
  (let ((pos))
    (setq  pos (get-text-property (point) 'label-pos))
    (if (null pos)
	(error "No label position recorded on this line")
      (pop-to-buffer (get-text-property (point) 'label-buffer))
      (goto-char pos))))


(defun bibtex-insert-standard-BibNet-citation-label ()
  "Replace the citation label, if any, on the current BibTeX entry, with
one standardized to the BibNet Project form, Lastname:1994:ABC.

Provided that the citation-label-table has not been invalidated by
manual generation of labels (use bibtex-update-citation-label-table to
make it consistent if needed), the label is unique in the narrowed
region or buffer.  The new label is added to, and the old one deleted
from, the citation-label-table,

Duplicate citation labels can be detected by the function
bibtex-update-citation-label-table, or externally by BibTeX or
bibcheck."
  (interactive)
  (let ((old-syntax-table
	 bibtex-mode-syntax-table)
	(begin)
	(label))
    (internal-bibtex-maybe-update-citation-label-table)
    (unwind-protect
	(progn
	  (set-syntax-table bibtex-extended-syntax-table)
	  ;; Exception: hyphens are word separators, to match
	  ;; biblabel/citesub conventions
	  (modify-syntax-entry ?\- " " bibtex-extended-syntax-table)
	  (if (not (re-search-backward bibtex-entry-label-comma-regexp nil t))
	      (error "No preceding BibTeX entry found"))
	  (re-search-forward "{[ \t]*")
	  (delete-horizontal-space)
	  (and (looking-at bibtex-citation-label-comma-regexp)
	       (looking-at bibtex-citation-label-regexp)
	       (internal-bibtex-hash-delete-pos (match-string 0)))
	  (looking-at bibtex-citation-label-regexp)
	  (setq label (bibtex-standard-BibNet-citation-label))
	  (cond
	   ((looking-at bibtex-citation-label-comma-regexp)
	      (kill-region (match-beginning 0) (match-end 0)))
	   ((looking-at ",")
	    (delete-char 1)))
	  (internal-bibtex-hash-put-pos label (point))
	  (insert label ","))
      (set-syntax-table old-syntax-table))))


(defun bibtex-mouse-goto-citation-label (event)
  "Go to the occurrence whose line you click on in the bibtex-cld-buffer
buffer."
  ;; Code adapted from (defun occur-mode-mouse-goto (event) ...)
  ;; in emacs-20.y/lisp/replace.el
  (interactive "e")
  (let (buffer pos)
    (save-excursion
      (set-buffer
       (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq buffer (get-text-property (point) 'label-buffer))
	(setq pos (get-text-property (point) 'label-pos))))
    (pop-to-buffer buffer)
    (goto-char pos)))


(defun bibtex-print-citation-label-table ()
  "Print a sorted table of citation labels into the temporary buffer
*Sorted Citation Labels*, and show it.

You can use \\[bibtex-goto-citation-label] or \\[bibtex-mouse-goto-citation-label] in that buffer
to go to the line in the BibTeX buffer containing the label."
  (interactive)
  (internal-bibtex-maybe-update-citation-label-table)
  (let ((buffer (current-buffer)))
    (with-output-to-temp-buffer "*Sorted Citation Labels*"
      (princ "Type C-c C-c, or click mouse-2, on label line to move to that line.\n\n")
      (princ (format "%-31s\t%s\n" "Lowercased label" "+/- Offset"))
      (princ (format "%-31s\t%s\n" "----------------" "----------"))

      ;; emacs-19.x needs this save-excursion; emacs-20.x does not, but
      ;; it is harmless to have it.
      (save-excursion
	(set-buffer "*Sorted Citation Labels*")
	(local-set-key "\C-c\C-c" 'bibtex-goto-citation-label)
	(local-set-key [mouse-2] 'bibtex-mouse-goto-citation-label))

      (internal-bibtex-hash-apply-sorted
       '(lambda (key value arg)
	  (save-excursion
	    (set-buffer "*Sorted Citation Labels*")
	    (goto-char (point-max))
	    (let ((start (point)))
	      (princ (format "%-31s\t%s\n" key value))
	      (put-text-property start (point) 'label-buffer buffer)
	      (put-text-property start (point) 'label-pos value))))))))


(defun bibtex-standard-BibNet-citation-label (&optional warn)
  "Starting with point inside the current BibTeX entry, move to the
beginning of the entry, create a unique standard BibNet citation label,
and return it.  Point is guaranteed to be left unchanged on return.
Citation label uniqueness is guaranteed only if citation-label-table is
up-to-date.

The author/editor, year, and title string values are checked for
consistency, and an error is raised if they are invalid.

If there is a collision with an existing label, and the optional
argument WARN is non-nil, a warning message is displayed in the echo
area, and the location of that label is pushed onto the mark stack, so
that the user can readily locate it for possible manual modification."
  (let ((begin (point)) (label nil) (year) (result))
    (unwind-protect
      (setq year (or (bibtex-get-value "year")
		     (bibtex-get-value "OPTyear")
		     "ERROR: This entry has no year value"))

      ;;; (if (null (string-match "^[12][0-9][0-9x][0-9x]([-/]+[12][0-9][0-9x][0-9x])?$" year)) ... )

      (if (null (string-match "^[12][0-9][0-9x][0-9x]" year))
	  (error "Illegal year pattern [%s]" year))
      (setq label (bibtex-reduce-string
		   (concat (bibtex-first-author-or-editor-name) ":"
			   (substring year 0 4) ":"
			   (bibtex-title-abbrev))))
      (goto-char begin))
    (setq result (internal-bibtex-unique-standard-BibNet-citation-label label))
    (if (and warn (not (null (nth 2 result))) (> (nth 2 result) 0))
	(progn
	  (push-mark (nth 2 result) t)
	  (message "Mark pushed at colliding label %s" (nth 1 result))))
    (nth 0 result)))


(defun bibtex-update-citation-label-table ()
  "Update the internal table of BibTeX citation labels.

citation-label-table is a buffer-local variable initialized to a hash
table with key-value pairs of (citation-label,buffer-position).  The
latter is negative if the citation-label is one reduced by stripping
lowercase letter suffixes from an existing label.  The buffer-position
may later be reset negative to indicate that a diagnostic has already
been issued for this label.

Error and warning messages appear in the temporary buffer
*Citation Label Diagnostics*, and you can use \\[bibtex-goto-citation-label] or \\[bibtex-mouse-goto-citation-label]
in that buffer to go to the line in the BibTeX buffer containing the
label.

This function is normally invoked automatically and internally the
first time a unique new citation label is generated, but it can also
be invoked by the user to update the citation label table after manual
modification of labels, or insertion of new entries."
  (interactive)
  (save-excursion
    (let ((case-fold-search t) (label nil) (pos nil) (s nil))
      (message "Generating citation-label-table ... this may take awhile")
      (internal-bibtex-hash-create-table)
      (goto-char (point-min))
      (with-output-to-temp-buffer bibtex-cld-buffer
	(while (re-search-forward bibtex-entry-regexp nil t)
	  (setq s (match-string 0))
	  (cond

	   ;; Ignore @Preamble{...}
	   ((string-match "preamble" s) nil)

	   ;; Ignore @String{...}
	   ((string-match "string" s) nil)

	   ;; Match "standard-label,"
	   ((looking-at bibtex-standard-citation-label-regexp-with-comma)
	    (looking-at bibtex-standard-citation-label-regexp)
	    (setq label (match-string 0))
	    (setq pos (internal-bibtex-hash-get-pos label))
	    (if (null pos)
		(internal-bibtex-hash-put-pos label (point)))
	    (internal-bibtex-make-cld-entry "duplicate label"
					    label pos (buffer-name) t)
	    (if (not (null pos))
		(internal-bibtex-make-cld-entry "duplicate label"
						label (point) (buffer-name) nil))
	    (let ((base-label) (case-fold-search nil))
	      (and (string-match "[a-z]+$" label) ;then have lowercase suffix letters
		   (setq base-label (substring label 0 (match-beginning 0)))
		   (setq pos (internal-bibtex-hash-get-pos base-label))
		   (internal-bibtex-make-cld-entry "unsuffixed base label"
						   base-label pos
						   (buffer-name) t))))
	   ;; Match "nonstandard-label,"
	   ((looking-at "[^ ,]+[ \t]*,")
	    (looking-at "[^ ,]+")
	    (internal-bibtex-make-cld-entry "nonstandard label" (match-string 0)
					    (point) (buffer-name)))
	   ;; Anything else is senseless
	   (t
	    (internal-bibtex-make-cld-entry "missing label" "" (point) (buffer-name))))))

      ;; If nothing was written to bibtex-cld-buffer, kill it
      (save-excursion
	(set-buffer bibtex-cld-buffer)
	(if (= (buffer-size) 0)
	    (kill-buffer bibtex-cld-buffer)))
					;clear message area
      (message nil))))


(defun internal-bibtex-base-26-integer (n)
  "Convert N to a base-26 integer (a..z, aa..zz, aaa..zzz, ...) and
return the resulting string."
  (let ((k) (s))
    (setq s "")
    (if (= n 0)
	(setq s "a")
      (while (> n 0)
	(setq s (concat (char-to-string (+ 97 (% n 26))) s))
	(setq n (/ n 26))))
    s))


(defun internal-bibtex-hash-apply-sorted (funct)
  "Apply funct to a sorted list of labels in citation-label-table."
  (hash-apply-sorted funct nil citation-label-table nil))


(defun internal-bibtex-hash-create-table ()
  "Create citation-label-table."

  (make-local-variable 'citation-label-table)
  (internal-bibtex-hash-delete-table)

  ;; Ideally, we should create a hash table big enough to avoid grow
  ;; operations.  The largest reasonable BibTeX bibliography is
  ;; about 4000 entries, so I initially tried a fixed size of 5003
  ;; (for 80% loading), but this proved too slow on older machines.
  ;; We therefore pick a size that matches the count of labels
  ;; actually used.

  (setq citation-label-table
	(hash-create-table t (* 5 (max 1 (/ (bibtex-count-entries) 4))))))


(defun internal-bibtex-hash-delete-table ()
  "Delete citation-label-table."
  (if (boundp 'citation-label-table)
      (setq citation-label-table (hash-delete-table citation-label-table))))


(defun internal-bibtex-hash-delete-pos (label)
  "Delete the buffer position recorded for LABEL [cost: O(1)]."
  (hash-delete-entry label citation-label-table))


(defun internal-bibtex-hash-get-cursize ()
  "Return the number of buffer positions currently stored in
citation-label-table."
  (hash-get-cursize citation-label-table))


(defun internal-bibtex-hash-get-pos (label)
  "Return the buffer position of LABEL, or nil if unknown [cost: O(1)]."
  (hash-get-entry label citation-label-table))


(defun internal-bibtex-hash-put-pos (label pos)
  "Store the buffer position POS corresponding to LABEL [cost: O(1)]."
  (hash-put-entry label pos citation-label-table))


(defun internal-bibtex-make-cld-entry (message label pos buffer &optional hide-label)
  "If POS is non-nil and positive, print MESSAGE and LABEL in
bibtex-cld-buffer, and record (POS,BUFFER) in the message text-property.
If the optional argument HIDE-LABEL is non-nil, reset the position in
citation-label-table to negative as a signal that a diagnostic for this
label has already been issued."
  (if (and pos (> pos 0))
      (progn
	(internal-bibtex-print-cld-line
	 (format "%5d: %s %s\n" (count-lines 1 pos) message label)
	 pos buffer)
	(if hide-label
	    (internal-bibtex-hash-put-pos label (- (abs pos)))))))


(defun internal-bibtex-maybe-update-citation-label-table ()
  "Update citation-label-table if it is unbound, nil, or empty."
  (if (or (not (boundp 'citation-label-table))
	  (null citation-label-table)
	  (= 0 (internal-bibtex-hash-get-cursize)))
      (bibtex-update-citation-label-table)))


(defun internal-bibtex-print-cld-line (message pos buffer)
  "Print MESSAGE at the end of bibtex-cld-buffer, attaching (POS,BUFFER)
as text properties to the message text for later selection by
\\[bibtex-goto-citation-label] or \\[bibtex-mouse-goto-citation-label].

Printing is suppressed if POS is nil or negative; this feature is used
to prevent duplicate message lines,

The current point and current buffer are left unchanged."
  (if (and (not (null pos)) (> pos 0))
      (let ((start))
	(save-excursion
	  (set-buffer bibtex-cld-buffer)
	  (if (= (buffer-size) 0)
	      (progn
		(princ "Type C-c C-c, or click mouse-2, on label line to move to that line.\n\n")
		(local-set-key [mouse-2] 'bibtex-mouse-goto-citation-label)
		(local-set-key "\C-c\C-c" 'bibtex-goto-citation-label)))
	  (goto-char (point-max))
	  (setq start (point))
	  (princ message)
	  (put-text-property start (point) 'label-buffer buffer)
	  (put-text-property start (point) 'label-pos pos)))))


(defun internal-bibtex-unique-standard-BibNet-citation-label (label)
  "Given a tentative citation label, LABEL, make it unique in the buffer
if necessary by adding a base-26 suffix (a..z, aa..zz, aaa..zzz, ...) to
it, and return a list containing the new unique label, the colliding
label, and the buffer position of the colliding label."
  (let ((unique-label label) (n 1) (collision-pair nil) (pos nil))
    (internal-bibtex-maybe-update-citation-label-table)
    (while (setq pos (internal-bibtex-hash-get-pos unique-label))
      (if (= n 1)
	  (setq collision-pair (list unique-label pos)))
      (setq unique-label (concat label (internal-bibtex-base-26-integer n)))
      (setq n (1+ n)))
    (append (list unique-label) collision-pair)))


;;; bibtex-mode menu additions: these augment entries set in bibtex.el,
;;; and go at the top of the BibTeX-Edit menu, in reverse order.


(define-key bibtex-mode-map
  [menu-bar move/edit bibtex-update-citation-label-table]
  '("    update citation label table    " . bibtex-update-citation-label-table))


(define-key bibtex-mode-map
  [menu-bar move/edit bibtex-print-citation-label-table]
  '("    print citation label table     " . bibtex-print-citation-label-table))
