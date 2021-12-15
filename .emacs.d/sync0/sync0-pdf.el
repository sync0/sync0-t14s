;; This is a library for manipulating pdfs using various tools but
;; mostly ghostscript

(defvar sync0-pdf-crop-command (concat " -sDEVICE=pdfwrite -c \" /PAGES pdfmark\" "))

;; "gs -o cropped.pdf                 
;;   -sDEVICE=pdfwrite              
;;   -c "[/CropBox [24 72 559 794]"
;;   -c " /PAGES pdfmark"           
;;   -f uncropped-input.pdf


(defvar sync0-units-milimeter-to-point-constant 2.83465
  "Multiplying constant to pass from milimeters to points")

(defun sync0-units-convert-milimeters-to-points (value)
  "Convert value in milimeters to points"
  (round 
   (if (numberp value)
       (* value sync0-units-milimeter-to-point-constant)
     (* (string-to-number  value) sync0-units-milimeter-to-point-constant))))

(defun sync0-pdf-convert-dimensions-percentage-to-points (value reference)
  "Convert value in milimeters to points"
   (if (numberp value)
      (/ (* reference value) 100)
      (/ (* reference (string-to-number value)) 100)))

  ;; (cond ((and (numberp value)
  ;;             as-string)
  ;;         (* (number-to-string  value) sync0-units-milimeter-to-point-constant))
  ;;       (as-string (* (number-to-string  value) sync0-units-milimeter-to-point-constant))
  ;;       (t   (* value sync0-units-milimeter-to-point-constant))))

;; (defun sync0-pdf-define-cropbox (pdf)
;;   "Define the cropbox for pdf to be used by ghostcript. This is a
;; helper function; it does not call ghostscript itself. Beware! The
;; pdf argument is mandatory because crop boxes are defined based on
;; the attributes of the pdf: They cannot be defined in the
;; absolute. Consequently, when this function is called in loops,
;; the pdfs to be cropped must have the same page sizes. Otherwise,
;; the crop boxes will not produce the expected result."
;;   (let* ((page-size 
;;           (shell-command-to-string (concat "pdfinfo \"" pdf "\" | grep \"Page size: \"")))
;;          (regex "[a-zA-Z: ]*\\([0-9]+\\)[x ]*\\([0-9]+\\)")
;;          (width
;;           (round 
;;            (string-to-number
;;             (save-match-data
;;               (string-match regex page-size)
;;               (match-string 1 page-size)))))
;;          (height
;;           (round 
;;            (string-to-number
;;             (save-match-data
;;               (string-match regex page-size)
;;               (match-string 2 page-size)))))
;;          (l-margin
;;           (number-to-string
;;            (sync0-units-convert-milimeters-to-points (read-string "Enter left margin (in mm): "))))
;;          (r-margin
;;           (number-to-string
;;            (- width (sync0-units-convert-milimeters-to-points (read-string "Enter right margin (in mm): ")))))
;;          (t-margin
;;           (number-to-string
;;            (- height (sync0-units-convert-milimeters-to-points (read-string "Enter top margin (in mm): ")))))
;;          (b-margin
;;           (number-to-string
;;            (sync0-units-convert-milimeters-to-points (read-string "Enter bottom margin (in mm): ")))))
;;     (concat " -c \"[/CropBox [" l-margin " " b-margin " " r-margin " " t-margin "]")))

(defun sync0-pdf-define-cropbox (pdf)
  "Define the cropbox for pdf to be used by ghostcript. This is a
helper function; it does not call ghostscript itself. Beware! The
pdf argument is mandatory because crop boxes are defined based on
the attributes of the pdf: They cannot be defined in the
absolute. Consequently, when this function is called in loops,
the pdfs to be cropped must have the same page sizes. Otherwise,
the crop boxes will not produce the expected result."
  (let* ((page-size 
          (shell-command-to-string (concat "pdfinfo \"" pdf "\" | grep \"Page size: \"")))
         (regex "[a-zA-Z: ]*\\([0-9]+\\)[x ]*\\([0-9]+\\)")
         (width
          (round 
           (string-to-number
            (save-match-data
              (string-match regex page-size)
              (match-string 1 page-size)))))
         (height
          (round 
           (string-to-number
            (save-match-data
              (string-match regex page-size)
              (match-string 2 page-size)))))
         (l-margin
          (number-to-string
           (sync0-pdf-convert-dimensions-percentage-to-points (read-string "Enter left margin (in percentage): ") width)))
         (r-margin
          (number-to-string
           (- width (sync0-pdf-convert-dimensions-percentage-to-points (read-string "Enter right margin (in percentage): ") width))))
         (t-margin
          (number-to-string
           (- height (sync0-pdf-convert-dimensions-percentage-to-points (read-string "Enter top margin (in percentage): ") height))))
         (b-margin
          (number-to-string
           (sync0-pdf-convert-dimensions-percentage-to-points (read-string "Enter bottom margin (in percentage): ") height))))
    (concat " -c \"[/CropBox [" l-margin " " b-margin " " r-margin " " t-margin "]")))

(defun sync0-pdf-download-from-url (url filename &optional pirate)
  "Download a paper from url with filename. When pirate is true,
download the pdf from Sci-hub. File name and url must be absolute
values since no error protection is taken into account."
  (interactive "sEnter url: \nFFile name to save to: ")
  (if pirate 
      (if (file-exists-p filename)
          (message "Error: File %s already exists" filename)
        (progn (scihub url filename)
               (message "AARGHHH! PDF downloaded from %s" url)))
    (let ((command (concat "wget  -O " filename " \"" url "\"")))
      (if (file-exists-p filename)
          (message "Error: File %s already exists" filename)
        (progn 
          (shell-command command)
          (message "PDF downloaded from %s" url))))))

(provide 'sync0-pdf)
