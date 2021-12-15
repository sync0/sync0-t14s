(provide 'bibtex-tools)

(require 'bibtools)			;for (defun gsub...)

(defconst bibtex-tools-awk "mawk"
  "Name of the awk implementation to use (usually, one of: awk, bawk,
gawk, mawk, or nawk.")


(defconst bibtex-tools-libdir "/u/sy/beebe/tex/bib"
"Location of bibtex-tools awk programs and scripts")


(defun bibtex-tools-bibcheck ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "bibcheck %s" (buffer-file-name)))))


(defun bibtex-tools-bibparse ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "bibparse %s" (buffer-file-name)))))


(defun bibtex-tools-check-bbl ()
  (interactive)
  (let ((bbl-file (gsub "[.]bib$" ".bbl" (buffer-file-name))))
    (if (null bbl-file)
	(error "No filename available")
      (compile (format "%s -f %s/%s %s %s"
		       bibtex-tools-awk 
		       bibtex-tools-libdir "check-bbl.awk" "+short" bbl-file)))))


(defun bibtex-tools-check-page-gaps ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "check-page-gaps.awk" (buffer-file-name)))))


(defun bibtex-tools-check-page-range ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "check-page-range.awk" (buffer-file-name)))))


(defun bibtex-tools-chkdelim ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "chkdelim -bibtex -par -i LG\\' %s" (buffer-file-name)))))


(defun bibtex-tools-chkdelim-no-par ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "chkdelim -bibtex -i LG\\' %s" (buffer-file-name)))))


(defun bibtex-tools-find-author-page-matches ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-author-page-matches.awk" (buffer-file-name)))))


(defun bibtex-tools-find-braceable-initial-title-words ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-braceable-initial-title-words.awk" (buffer-file-name)))))


(defun bibtex-tools-find-crossref-year-mismatches ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-crossref-year-mismatches.awk" (buffer-file-name)))))


(defun bibtex-tools-find-duplicate-author-editor ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-duplicate-author-editor.awk" (buffer-file-name)))))


(defun bibtex-tools-find-duplicate-pages ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-duplicate-pages.awk" (buffer-file-name)))))


(defun bibtex-tools-find-german-titles ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-german-titles.awk" (buffer-file-name)))))


(defun bibtex-tools-find-hyphenated-title-words ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-hyphenated-title-words.awk" (buffer-file-name)))))


(defun bibtex-tools-find-math-prefixes ()
  (interactive)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (occur "^ *\\(book\\)?title *= *\"[^\"]*[ ][a-z0-9]-")))


(defun bibtex-tools-find-missing-parbreaks ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-missing-parbreaks.awk" 
		     (buffer-file-name)))))


(defun bibtex-tools-find-page-matches ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-page-matches.awk" (buffer-file-name)))))

(defun bibtex-tools-find-possessive-title-words ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-possessive-title-words.awk" (buffer-file-name)))))


(defun bibtex-tools-find-superfluous-label-suffixes ()
  (interactive)
  (if (null (buffer-file-name))
      (error "No filename available")
    (compile (format "%s -f %s/%s %s"
		     bibtex-tools-awk 
		     bibtex-tools-libdir "find-superfluous-label-suffixes.awk" (buffer-file-name)))))


;;; Now bind the various tool functions to menu bar items (in reverse
;;; alphabetical order):

(define-key bibtex-mode-map
  [menu-bar move/edit find-superfluous-label-suffixes]
  '("  find-superfluous-label-suffixes  " . bibtex-tools-find-superfluous-label-suffixes))


(define-key bibtex-mode-map
  [menu-bar move/edit find-possessive-title-words]
  '("    find-possessive-title-words    " . bibtex-tools-find-possessive-title-words))


(define-key bibtex-mode-map
  [menu-bar move/edit find-page-matches]
  '("         find-page-matches         " . bibtex-tools-find-page-matches))


(define-key bibtex-mode-map
  [menu-bar move/edit find-missing-parbreaks]
  '("      find-missing-parbreaks       " . bibtex-tools-find-missing-parbreaks))


(define-key bibtex-mode-map
  [menu-bar move/edit find-math-prefixes]
  '("        find-math-prefixes         " . bibtex-tools-find-math-prefixes))


(define-key bibtex-mode-map
  [menu-bar move/edit find-hyphenated-title-words]
  '("    find-hyphenated-title-words    " . bibtex-tools-find-hyphenated-title-words))


(define-key bibtex-mode-map
  [menu-bar move/edit find-german-titles]
  '("        find-german-titles         " . bibtex-tools-find-german-titles))


(define-key bibtex-mode-map
  [menu-bar move/edit find-duplicate-pages]
  '("       find-duplicate-pages        " . bibtex-tools-find-duplicate-pages))


(define-key bibtex-mode-map
  [menu-bar move/edit find-duplicate-author-editor]
  '("   find-duplicate-author-editor    " . bibtex-tools-find-duplicate-author-editor))


(define-key bibtex-mode-map
  [menu-bar move/edit find-crossref-year-mismatches]
  '("   find-crossref-year-mismatches   " . bibtex-tools-find-crossref-year-mismatches))


(define-key bibtex-mode-map
  [menu-bar move/edit find-braceable-initial-title-words]
  '("find-braceable-initial-title-words " . bibtex-tools-find-braceable-initial-title-words))


(define-key bibtex-mode-map
  [menu-bar move/edit find-author-page-matches]
  '("     find-author-page-matches      " . bibtex-tools-find-author-page-matches))


(define-key bibtex-mode-map
  [menu-bar move/edit chkdelim-no-par]
  '("          chkdelim-no-par          " . bibtex-tools-chkdelim-no-par))


(define-key bibtex-mode-map
  [menu-bar move/edit chkdelim]
  '("             chkdelim              " . bibtex-tools-chkdelim))


(define-key bibtex-mode-map
  [menu-bar move/edit check-page-range]
  '("         check-page-range          " . bibtex-tools-check-page-range))


(define-key bibtex-mode-map
  [menu-bar move/edit check-page-gaps]
  '("         check-page-gaps           " . bibtex-tools-check-page-gaps))


(define-key bibtex-mode-map
  [menu-bar move/edit check-bbl]
  '("            check-bbl              " . bibtex-tools-check-bbl))


(define-key bibtex-mode-map
  [menu-bar move/edit bibparse]
  '("            bibparse               " . bibtex-tools-bibparse))


(define-key bibtex-mode-map
  [menu-bar move/edit bibcheck]
  '("            bibcheck               " . bibtex-tools-bibcheck))
