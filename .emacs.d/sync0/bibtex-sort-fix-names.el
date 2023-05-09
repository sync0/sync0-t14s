;; (defun my-bibtex-get-year ()
;;   (bibtex-autokey-get-field "year"))

(defun my-bibtex-get-year ()
  (bibtex-autokey-get-field "date"))

(defun my-bibtex-sort-chronologically (ascending)
  "Sort bibtex buffer chronologically.
Use descending order by default; prefix arg ASCENDING specifies ascending order.
Does not work with bibtex-maintain-sorted-entries, that sorts only by key."
  (interactive "P")
  ;; stolen from bibtex-sort-buffer
  (bibtex-beginning-of-first-entry)     ; Needed by `sort-subr'
  (sort-subr (not ascending)
             'bibtex-skip-to-valid-entry   ; NEXTREC function
             'bibtex-end-of-entry          ; ENDREC function
             'my-bibtex-get-year           ; STARTKEY function (returns whole key)
             nil                           ; ENDKEY function
             nil))                         ; PREDICATE: "<" for numbers

;; TODO: is this any better? (autoload 'bibtex-normalize-author-editor-names "bibtools" nil t)
(defun my-bibtex-fixup-names (field)
  "Fixup FIELD (author/editor) by replacing , ; & with `and'. 
Also remove digits (footnote number to affiliation).
TODO name inversion Last, First"
  (let* ((OPTfield (concat "OPT" field))
         (bounds
          (or (bibtex-search-backward-field field t)
              (bibtex-search-forward-field field t)
              (bibtex-search-backward-field OPTfield t)
              (bibtex-search-forward-field OPTfield t)
              (error "No `%s' field found" field)))
         (beg-text (copy-marker (bibtex-start-of-text-in-field bounds)))
         (end-text (copy-marker (bibtex-end-of-text-in-field bounds))))
    (goto-char beg-text)
    (while (re-search-forward "[0-9 ]*[,;&][0-9 ]*" end-text t)
      (replace-match " and "))))

(defun my-bibtex-fixup-author ()
  "Fixup the author field (see my-bibtex-fixup-names)"
  (interactive)
  (my-bibtex-fixup-names "author"))

(defun my-bibtex-fixup-editor ()
  "Fixup the author field (see my-bibtex-fixup-names)"
  (interactive)
  (my-bibtex-fixup-names "editor"))

(defadvice bibtex-fill-field-bounds (around disable-fill activate)
  "I want bibtex-clean-entry and bibtex-fill-entry to do 'realign, but I don't care for autofill"
  (cl-flet (fill-region-as-paragraph 'ignore)
    ad-do-it))

(set 'bibtex-mode-hook ; not add-hook since someone puts BibTeX-auto-store in: no thanks
  (defun my-bibtex-mode-hook ()
    (require 'cl)
    (make-local-variable 'TeX-quote-after-quote)
    (setq TeX-quote-after-quote t)
    (define-key bibtex-mode-map "\""     'self-insert-command) ; 'TeX-insert-quote)
    (define-key bibtex-mode-map "\C-c\""   'my-toggle-TeX-quote-after-quote)
    (define-key bibtex-mode-map "\C-ca"    'my-bibtex-fixup-author)
    (define-key bibtex-mode-map "\C-c,"    'my-bibtex-fixup-author)
    (define-key bibtex-mode-map "\C-ce"    'my-bibtex-fixup-editor)
    (define-key bibtex-mode-map "\C-c;"    'my-bibtex-fixup-editor)
    (define-key bibtex-mode-map "\C-ck"    'bu-make-field-keywords)
    (define-key bibtex-mode-map "\C-c\C-s" 'my-bibtex-sort-chronologically)
    (define-key bibtex-mode-map "\C-cs"    'bibtex-sort-buffer)))
