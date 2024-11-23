(defvar sync0-print-file nil)

(defvar sync0-print-default-printer "bizhub_c451"
  "Default printer to use with the lp or the lpr terminal commands.")

(defvar sync0-print-default-two-sided "two-sided-long-edge")

(defvar sync0-print-default-print-quality "3")

(defvar sync0-print-default-pages-per-sheet "1")

(defvar sync0-print-default-paper-size "a4")

(defvar sync0-print-number-copies nil)

(defvar sync0-print-page-size nil)

(defvar sync0-print-page-layout nil)

(defvar sync0-print-page-quality nil)

;; (defvar sync0-print-command "lpr -P HP_Deskjet_4640_series -o fit-to-page -o position=center") 

(defvar sync0-print-command "lpr -o fit-to-page -o position=center") 

(defvar sync0-print-command
  (concat "lpr -P " sync0-print-default-printer " -o fit-to-page -o position=center") 
  "Default command to pass to lpr")

;; (defun sync0-print-define-file (&optional)
;;   "Define the language for new BibLaTeX entry."
;;   (setq sync0-bibtex-entry-language
;;         (if bibkey
;;             (let ((entry (bibtex-completion-get-entry bibkey)))
;;               (bibtex-completion-get-value "language" entry))
;;           (completing-read "Choose language : "
;;                            sync0-bibtex-completion-language nil nil sync0-bibtex-entry-initial-language))))

(defun sync0-print-define-command ()
  (let ((copies (concat " -# "
                        (read-string "Enter number of copies: " "1")))
        (page-size (concat " -o media="
                           (completing-read "Choose page size: " '("a4" "a5" "a6" "letter" "legal") nil t sync0-print-default-paper-size)))
        (page-list (unless (yes-or-no-p "Print all pages?")
                     (concat " -o page-ranges="
                             (read-string "Page range (ex. 1-3 or 1,3): "))))
        (page-per-sheet (let ((pages (read-string "Enter number of pages per sheet (max. 16): " sync0-print-default-pages-per-sheet)))
                          (if (string= pages "1")
                              nil
                            (concat " -o number-up=" pages))))
        (double-sided (concat " -o sides="
                              (completing-read "Choose page layout: " '("one-sided" "two-sided-long-edge" "two-sided-short-edge") nil t sync0-print-default-two-sided)))
        ;; Remember the blank space at the end of this command. This
        ;; is good to prevent unwanted effects when using this command
        ;; in with the function "concat"
        (quality (concat " -o print-quality=" (completing-read "Choose print quality: " '("3" "4" "5") nil t sync0-print-default-print-quality) " ")))
    (concat sync0-print-command
            copies
            page-size
            (unless (null page-list)
              page-list)
            (unless (null page-per-sheet)
              page-per-sheet)
            double-sided
            quality)))

(defun sync0-print-pdf (&optional pdf command)
  "Print the pdf provided in the argument. Generalized for
interactive use and use in pipes to output to the printer."
  (interactive)
  (cond ((and pdf
              command)
         (if (file-exists-p pdf)
             (shell-command (concat command pdf))
           (message "File %s does not exist" pdf)))
        (pdf (let ((command (sync0-print-define-command)))
               (if (file-exists-p pdf)
                   (shell-command (concat command pdf))
                 (message "File %s does not exist" pdf))))
        (t (let* ((pdf (read-string "what pdf file (absolute path): "))
                  (command (sync0-print-define-command))) 
             (if (file-exists-p pdf)
                 (shell-command (concat command pdf))
               (message "No attachment found for entry %s" pdf))))))

;; pdfjam --outfile output2.pdf --paper a4paper --nup 2x1 --landscape

(provide 'sync0-print)
