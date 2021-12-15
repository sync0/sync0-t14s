(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article" '("author" "title" "journal" "year")
		'("volume" "number" "pages" "month" "note" "keyword" "annote")))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book" '("author" "title" "publisher" "year")
		'("editor" "volume" "series" "address"
			   "edition" "month" "note" "keyword" "annote")))

(defun bibtex-Booklet ()
  (interactive)
  (bibtex-entry "Booklet" '("title")
		'("author" "howpublished" "address" "month" "year" "note" "keyword" "annote")))

;; France: Dipl\^{o}me d'Etudes Approfondies (similar to Master's)
(defun bibtex-DEAthesis ()
  (interactive)
  (bibtex-entry "DEAthesis" '("author" "title" "school" "year")
		'("address" "month" "note" "keyword" "annote")))

(defun bibtex-InBook ()
  (interactive)
  (bibtex-entry "InBook" '("author" "title" "chapter" "publisher" "year")
		'("editor" "pages" "volume" "series" "address"
			   "edition" "month" "note" "keyword" "annote")))

(defun bibtex-InCollection ()
  (interactive)
  (bibtex-entry "InCollection" '("author" "title" "booktitle"
					  "publisher" "year")
		'("editor" "chapter" "pages" "address" "month" "note" "keyword" "annote")))


(defun bibtex-InProceedings ()
  (interactive)
  (bibtex-entry "InProceedings" '("author" "title" "booktitle" "year")
		'("editor" "pages" "organization" "publisher"
			   "address" "month" "note" "keyword" "annote")))

(defun bibtex-Manual ()
  (interactive)
  (bibtex-entry "Manual" '("title")
		'("author" "organization" "address" "edition" "year"
			   "month" "note" "keyword" "annote")))

(defun bibtex-MastersThesis ()
  (interactive)
  (bibtex-entry "MastersThesis" '("author" "title" "school" "year")
		'("address" "month" "note" "keyword" "annote")))

(defun bibtex-Misc ()
  (interactive)
  (bibtex-entry "Misc" '()
		'("author" "title" "howpublished" "year" "month" "note" "keyword" "annote")))

(defun bibtex-PhdThesis ()
  (interactive)
  (bibtex-entry "PhdThesis" '("author" "title" "school" "year")
		'("address" "month" "note" "keyword" "annote")))

(defun bibtex-Proceedings ()
  (interactive)
  (bibtex-entry "Proceedings" '("title" "year")
		'("editor" "publisher" "organization"
			   "address" "month" "note" "keyword" "annote")))
(defun bibtex-TechReport ()
  (interactive)
  (bibtex-entry "TechReport" '("author" "title" "institution" "year")
		'("type" "number" "address" "month" "note" "keyword" "annote")))


(defun bibtex-Unpublished ()
  (interactive)
  (bibtex-entry "Unpublished" '("author" "title" "note")
		'("year" "month" "keyword" "annote")))

