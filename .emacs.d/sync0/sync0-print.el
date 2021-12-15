(defvar sync0-print-file nil)

(defvar sync0-print-number-copies nil)

(defvar sync0-print-page-size nil)

(defvar sync0-print-page-layout nil)

(defvar sync0-print-page-quality nil)

(defvar sync0-print-command "lpr -P HP_Deskjet_4640_series -o fit-to-page -o position=center") 

;; (defun sync0-print-define-file (&optional)
;;   "Define the language for new BibLaTeX entry."
;;   (setq sync0-bibtex-entry-language
;;         (if bibkey
;;             (let ((entry (bibtex-completion-get-entry bibkey)))
;;               (bibtex-completion-get-value "language" entry))
;;           (completing-read "Choose language : "
;;                            sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))))

(defun sync0-print-define-command ()
  (let ((copies (concat " -# " (read-string "Enter number of copies: ") " "))
        (page-size (completing-read "Choose page size: " '(" -o media=a4 " " -o media=letter " " -o media=legal ")))
        (layout (completing-read "Choose page layout: " '(" -o sides=one-sided " " -o sides=two-sided-long-edge " " -o sides=two-sided-short-edge ")))
        (quality (completing-read "Choose print quality: " '(" -o print-quality=3 " " -o print-quality=4 " " -o print-quality=5 "))))
    (concat sync0-print-command copies page-size layout quality)))

(provide 'sync0-print)
