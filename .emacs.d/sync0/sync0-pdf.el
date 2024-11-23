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

;; (defun sync0-pdf-download-from-url (url filepath)
;;   "Download a paper from url with filename. When pirate is true,
;; download the pdf from Sci-hub. File name and url must be absolute
;; values since no error protection is taken into account."
;;   (interactive "sEnter url: \nFFile name to save to: ")
;;   (let ((command (concat "wget  -O " filepath " \"" url "\"")))
;;     (if (file-exists-p filepath)
;;         (message "Error: File %s already exists" filepath)
;;       (progn 
;;         (if (url-file-exists-p url)
;;             (progn 
;;               (shell-command command)
;;               (message "PDF downloaded from %s" url))
;;           (message "No PDF exists for %s" url))))))

(defun sync0-add-text-to-pdf (pdf-file text-string start-page end-page)
  "Add TEXT-STRING to PDF-FILE on pages from START-PAGE to END-PAGE using cpdf.
Text will be placed in the bottom left corner with font Courier-Bold size 16."
  (interactive
   (list (read-file-name "Choose a PDF file: " sync0-bibnotes-directory)
         (read-string "Enter the text to add: ")
         (read-number "Enter the start page: " 1)
         (read-number "Enter the end page: " 1)))
  (let* ((output (concat (file-name-sans-extension pdf-file) "_temp.pdf"))
         (command (format "cpdf -utf8 -add-text \"%s\" -font \"Courier-Bold\" -bottomleft 20 -font-size 16 %s %d-%d -o %s"
                          text-string
                          pdf-file
                          start-page
                          end-page
                          output)))
    (if (and (file-exists-p pdf-file)
             (equal (file-name-extension pdf-file) "pdf"))
        (progn
          (shell-command command)
          (if (file-exists-p output)
              (rename-file output pdf-file t)
            (message "cpdf failed to create the PDF; investigate the error.")))
      (message "The specified file does not exist or is not a PDF."))))

(defun sync0-resize-and-add-text-to-pdf (pdf-file text-string start-page end-page)
  "Resize PDF-FILE to create a top margin and then add TEXT-STRING to pages from START-PAGE to END-PAGE using cpdf.
Afterward, ensure that the pages are resized back to A6."
  (interactive
   (list (read-file-name "Choose a PDF file: " sync0-bibnotes-directory)
         (read-string "Enter the text to add: ")
         (read-number "Enter the start page: " 1)
         (read-number "Enter the end page: " 1)))
  (let* ((output (concat (file-name-sans-extension pdf-file) "_temp.pdf"))
         (resized (concat (file-name-sans-extension pdf-file) "_resized.pdf"))
         (final (concat (file-name-sans-extension pdf-file) "_final_a6.pdf"))
         ;; Resize page to add space at the top (increase height slightly)
         (resize-command (format "cpdf -mediabox \"0 0 297 440\" %s -o %s" 
                                 pdf-file resized))
         ;; Add text in the new top margin area
         (add-text-command (format "cpdf -utf8 -add-text \"%s\" -font \"Courier-Bold\" -top-left 20 40 -font-size 16 %s %d-%d -o %s"
                                   text-string
                                   resized
                                   start-page
                                   end-page
                                   output))
         ;; Resize the PDF back to A6 size
         (resize-back-command (format "cpdf -mediabox \"0 0 297 420\" %s -o %s"
                                      output final)))
    (if (and (file-exists-p pdf-file)
             (equal (file-name-extension pdf-file) "pdf"))
        (progn
          (shell-command resize-command)
          (shell-command add-text-command)
          (shell-command resize-back-command)
          (if (file-exists-p final)
              (rename-file final pdf-file t)
            (message "cpdf failed to create the final PDF; investigate the error.")))
      (message "The specified file does not exist or is not a PDF."))))

(defun sync0-pdf-download-from-url (url filepath)
  "Download a paper from url with filename. When pirate is true,
download the pdf from Sci-hub. File name and url must be absolute
values since no error protection is taken into account."
  (interactive "sEnter url: \nFFile name to save to: ")
  (let ((command (concat "wget  -O " filepath " \"" url "\"")))
    (if (file-exists-p filepath)
        (message "Error: File %s already exists" filepath)
      (progn 
        (if (url-file-exists-p url)
            (progn 
              (shell-command command)
              (message "PDF downloaded from %s" url))
          (message "No PDF exists for %s" url))))))

;; (defun sync0-overlay-pdf (base-pdf overlay-pdf)
;;   "Overlay OVERLAY-PDF on BASE-PDF, scaling it to fit under the header."
;;   (let* ((scaled-overlay (expand-file-name "scaled_overlay.pdf" (file-name-directory overlay-pdf)))
;;          (scale-factor 0.85)
;;          (offset-x 28.35)
;;          (offset-y 3))
;;     ;; Create scaled overlay PDF using pdfjam
;;     (shell-command
;;      ;; (format "pdfjam \"%s\" --scale %s --offset '%s %s' --outfile \"%s\"" overlay-pdf scale-factor offset-x offset-y scaled-overlay))
;;      (format "pdfjam \"%s\" --scale %s --outfile \"%s\""
;;              overlay-pdf scale-factor scaled-overlay))
;;     ;; Now overlay the scaled PDF
;;     (let ((output-file (expand-file-name "overlay_output.pdf" (file-name-directory overlay-pdf))))
;;       (shell-command
;;        (format "pdftk \"%s\" stamp \"%s\" output \"%s\""
;;                scaled-overlay base-pdf output-file))
;;       (message "Overlay PDF created at: %s" output-file)
;;       output-file)))

;; (defun sync0-generate-pdf-with-overlay (header)
;;   "Create a PDF with headers and page numbers, then overlay it on a user-selected PDF.
;; HEADER is the text to display in the header."
;;   (interactive "sEnter the header text: ")  ;; Prompt for header text
;;   (let* ((overlay-pdf (read-file-name "Choose a PDF file: " sync0-bibnotes-directory))  ;; Prompt for PDF file
;;          (empty-pdf (sync0-create-empty-pdf-with-numbers header overlay-pdf)))  ;; Create empty PDF
;;     (sync0-overlay-pdf empty-pdf overlay-pdf)))  

;; (defun sync0-create-empty-pdf-with-numbers (header overlay-pdf)
;;   "Create a PDF with headers and page numbers using LuaLaTeX.
;; HEADER is the text to display in the header. OVERLAY-PDF is used to determine the number of pages."
;;   (let* ((pdfinfo-output (shell-command-to-string (format "pdfinfo \"%s\"" overlay-pdf)))  ;; Added quotes
;;          (page-count (if (string-match "Pages:\\s-+\\([0-9]+\\)" pdfinfo-output)  ;; Adjusted regex for whitespace
;;                          (string-to-number (match-string 1 pdfinfo-output))
;;                        (error "Could not get page count for %s" overlay-pdf)))
;;          (latex-template
;;           (format "\\documentclass[12pt]{article}
;; \\usepackage{fontspec}
;; \\setmainfont{Consolas}
;; \\usepackage[a6paper,right=5mm,bottom=5mm,bindingoffset=10mm,nofoot]{geometry}
;; \\usepackage{fancyhdr}
;; \\usepackage{pgffor}  
;; \\setlength{\\headheight}{15pt}  
;; \\pagestyle{fancy}
;; \\fancyhf{}
;; \\fancyhead[R]{%s/\\thepage}
;; \\begin{document}
;; \\foreach \\n in {1, 2, ..., %d} {
;;   \\newpage
;;   \\noindent
;;  {\\tiny \\n}
;; }
;; \\end{document}"
;;                   header
;;                   page-count))  ; Add the header text on each page
;;          (temp-file (concat temporary-file-directory "temp-latexy" (make-temp-name "") ".tex"))
;;          (output-file (concat (file-name-sans-extension temp-file) ".pdf")))
;;     (with-temp-file temp-file
;;       (insert latex-template))
;;     (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s" temporary-file-directory temp-file))
;;     (message "PDF created at: %s" output-file)
;;     output-file))

;; (defun sync0-overlay-pdf (base-pdf overlay-pdf)
;;   "Overlay OVERLAY-PDF on BASE-PDF without shifting the base."
;;   (let* ((scaled-overlay (expand-file-name "scaled_overlay.pdf" (file-name-directory overlay-pdf)))
;;          ;; (scale-factor 0.85)
;;          (scale-factor 1.3))
;;     ;; Create scaled overlay PDF using pdfjam without altering the base
;;     (shell-command
;;      (format "pdfjam \"%s\" --scale %s --outfile \"%s\""
;;              overlay-pdf scale-factor scaled-overlay))
    
;;     ;; Now overlay the scaled PDF using pdftk cat option to merge without altering base
;;     (let ((output-file (expand-file-name "overlay_output.pdf" (file-name-directory overlay-pdf))))
;;       (shell-command
;;        (format "pdftk \"%s\" multistamp \"%s\" output \"%s\""
;;                base-pdf scaled-overlay output-file))  ;; Use multistamp to merge
;;       (message "Overlay PDF created at: %s" output-file)
;;       output-file)))

;; (defun sync0-generate-pdf-with-overlay (header)
;;   "Create a PDF with headers and page numbers, then overlay it on a user-selected PDF, and output as 4 A6 pages on an A4 page with crop marks."
;;   (interactive "sEnter the header text: ")  ;; Prompt for header text
;;   (let* ((overlay-pdf (read-file-name "Choose a PDF file: " sync0-bibnotes-directory))  ;; Prompt for PDF file
;;          (empty-pdf (sync0-create-empty-pdf-with-numbers header overlay-pdf))  ;; Create empty PDF
;;          (overlayed-pdf (sync0-overlay-pdf empty-pdf overlay-pdf))  ;; Perform the overlay
;;          (combined-pdf (concat (file-name-sans-extension overlayed-pdf) "-combined.pdf")))  ;; Output combined PDF

;;     ;; Step 1: Use pgfpages and crop marks in LaTeX to combine four A6 pages onto one A4 page with crop marks
;;     (let ((latex-template (expand-file-name "output_with_cropmarks.tex" temporary-file-directory))
;;           (final-pdf (concat (file-name-sans-extension combined-pdf) "-with-cropmarks.pdf")))

;;       ;; Create the LaTeX file to combine pages and add crop marks
;;       (with-temp-file latex-template
;;         (insert "\\documentclass{article}
;; \\usepackage[a4,luatex,axes,frame,cross,cam]{crop}
;; \\usepackage{pgfpages}
;; \\pgfpagesuselayout{4 on 1}[a4paper,border shrink=0mm,odd numbered pages right]
;; \\usepackage{pdfpages}
;; \\begin{document}
;; \\includepdf[pages=-]{"
;;                 overlayed-pdf
;;                 "}
;; \\end{document}"))

;;       ;; Compile the LaTeX file to generate the final PDF with crop marks
;;       (shell-command (format "lualatex -output-directory=%s %s" temporary-file-directory latex-template))
      
;;       ;; Rename the final output to make it clear
;;       (rename-file (concat temporary-file-directory "/output_with_cropmarks.pdf") final-pdf t)

;;       (message "Final PDF with crop marks created at: %s" final-pdf)
;;       final-pdf)))


;; (defun sync0-create-pdf-with-overlay (&optional header pdf)
;;   "Create a PDF with headers and page numbers, generate crop marks PDF, then overlay it on a user-selected PDF."
;;   (interactive)  ;; Prompt for header text
;;   (let* ((header (or header (read-string "Enter the header text: ")))
;; 	 (overlay-pdf (or pdf  (read-file-name "Choose a PDF file: " sync0-bibnotes-directory)))  ;; Prompt for PDF file
;;          (overlay-tex (concat temporary-file-directory "overlay.tex"))  ;; Temporary LaTeX file for overlay
;;          (overlayed-pdf (concat temporary-file-directory "overlay.pdf"))  ;; Temporary LaTeX file for overlay
;;          (final-pdf (concat (file-name-sans-extension overlay-pdf) "-with-cropmarks.pdf")))  ;; Output PDF file

;;     ;; Step 1: Create the LaTeX file to include overlay PDF with headers
;;     (with-temp-file overlay-tex
;;       (insert (format "
;; \\documentclass{article}
;; \\usepackage[a6paper,top=17.5mm,right=5mm,bottom=5mm,bindingoffset=11mm,nofoot]{geometry}
;; \\usepackage{pdfpages}  %% For including PDF pages
;; \\usepackage{pgfpages}  %% Have 4 A6 sheets fit an A4 page.
;; \\pgfpagesuselayout{4 on 1}[a4paper,border shrink=0mm,odd numbered pages right]
;; \\usepackage{fancyhdr}  %% For custom headers and footers
;; \\usepackage{fontspec}  %% To specify custom fonts
;; \\setmainfont{Consolas}  %% Set Consolas font
;; \\pagestyle{fancy}
;; \\fancyhf{}
;; \\fancyhead[R]{%s/\\thepage}  %% Header text with page number
;; \\begin{document}
;; \\includepdf[
;;   pages=-,
;;   width=0.85\\paperwidth,
;;   offset=0 -2.5mm,
;;   pagecommand={\\thispagestyle{fancy}}
;; ]{%s}
;; \\end{document}"
;;                       header overlay-pdf)))  ;; Insert header and overlay PDF path

;;     ;; Step 2: Compile the overlay PDF
;;     (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s"
;;                            temporary-file-directory overlay-tex))

;;     ;; Step 3: Get the number of pages in the overlay PDF
;;     (let* ((pdfinfo-output (shell-command-to-string (format "pdfinfo \"%s\"" overlayed-pdf)))  ;; Get PDF info
;;            (page-count (if (string-match "Pages:\\s-+\\([0-9]+\\)" pdfinfo-output)  ;; Extract number of pages
;;                            (string-to-number (match-string 1 pdfinfo-output))
;;                          (error "Could not get page count for %s" overlayed-pdf))))

;;       ;; Step 4: Create LaTeX file for crop marks with the same number of pages as overlay PDF
;;       (let ((cropmarks-tex (concat temporary-file-directory "cropmarks.tex")))  ;; Temporary LaTeX file for crop marks
;;         (with-temp-file cropmarks-tex
;;           (insert "\\documentclass{article}\n")
;;           (insert "\\usepackage[a4paper,margin=0mm]{geometry}  % Full A4 page\n")
;;           (insert "\\usepackage{tikz}\n")
;;           (insert "\\begin{document}\n")
;;           (insert "\\thispagestyle{empty}\n")

;;           (dotimes (n page-count)  ;; Loop to create pages
;;             (insert "\\begin{tikzpicture}[remember picture, overlay]\n")
;;             (insert "  % Center cross (horizontal and vertical lines crossing in the middle of the A4 page)\n")
;;             (insert "  \\draw[black, thick] (current page.center) ++(-10mm,0) -- ++(20mm,0); % Horizontal line at center\n")
;;             (insert "  \\draw[black, thick] (current page.center) ++(0,-10mm) -- ++(0,20mm); % Vertical line at center\n")
;;             (insert "  % Side crop marks\n")
;;             (insert "  \\draw[black, thick] (current page.north) ++(0,-5mm) -- ++(0,-10mm);   % Top center\n")
;;             (insert "  \\draw[black, thick] (current page.south) ++(0,5mm) -- ++(0,10mm);     % Bottom center\n")
;;             (insert "  \\draw[black, thick] (current page.west) ++(5mm,0) -- ++(10mm,0);      % Left center\n")
;;             (insert "  \\draw[black, thick] (current page.east) ++(-5mm,0) -- ++(-10mm,0);    % Right center\n")
;;             (insert "\\end{tikzpicture}\n\n")
;;             (insert "\\newpage\n\n"))  ;; Start a new page for each iteration

;;           (insert "\\end{document}\n"))  ;; End of the document

;;         ;; Step 5: Compile the crop marks PDF
;;         (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s"
;;                                temporary-file-directory cropmarks-tex)))

;;       ;; Step 6: Overlay crop marks PDF on the main overlay PDF using pdftk
;;       (let ((cropmarks-pdf (concat temporary-file-directory "cropmarks.pdf")))
;;         (shell-command (format "pdftk \"%s\" multistamp \"%s\" output \"%s\""
;;                                cropmarks-pdf
;;                                overlayed-pdf
;;                                final-pdf)))

;;       ;; Display the final PDF path
;;       (message "Final PDF with crop marks created at: %s" final-pdf)
;;       final-pdf)))

;; (defun sync0-create-pdf-with-overlay (&optional header pdf)
;;   "Create a PDF with headers and page numbers, generate crop marks PDF, then overlay it on a user-selected PDF."
;;   (interactive)  ;; Prompt for header text
;;   (let* ((header (or header (read-string "Enter the header text: ")))
;;          (overlay-pdf (or pdf (read-file-name "Choose a PDF file: " sync0-bibnotes-directory)))  ;; Prompt for PDF file
;;          (overlay-tex (concat temporary-file-directory "overlay.tex"))  ;; Temporary LaTeX file for overlay
;;          (overlayed-pdf (concat temporary-file-directory "overlay.pdf"))  ;; Temporary PDF for overlay
;;          (final-pdf (concat (file-name-sans-extension overlay-pdf) "-with-cropmarks.pdf")))  ;; Output PDF file

;;     ;; Step 1: Create the LaTeX file to include overlay PDF with headers
;;     (with-temp-file overlay-tex
;;       (insert (format "
;; \\documentclass{article}
;; \\usepackage[a6paper,top=17.5mm,right=5mm,bottom=5mm,bindingoffset=11mm,nofoot]{geometry}
;; \\usepackage{pdfpages}  %% For including PDF pages
;; \\usepackage{pgfpages}  %% Have 4 A6 sheets fit an A4 page.
;; \\pgfpagesuselayout{4 on 1}[a4paper,border shrink=0mm,odd numbered pages right]
;; \\usepackage{fancyhdr}  %% For custom headers and footers
;; \\usepackage{fontspec}  %% To specify custom fonts
;; \\setmainfont{Consolas}  %% Set Consolas font
;; \\pagestyle{fancy}
;; \\fancyhf{}
;; \\fancyhead[R]{%s/\\thepage}  %% Header text with page number
;; \\begin{document}
;; \\includepdf[
;;   pages=-,
;;   width=0.85\\paperwidth,
;;   offset=0 -2.5mm,
;;   pagecommand={\\thispagestyle{fancy}}
;; ]{%s}
;; \\end{document}"
;;                       header overlay-pdf)))  ;; Insert header and overlay PDF path

;;     ;; Step 2: Compile the overlay PDF
;;     (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s"
;;                            temporary-file-directory overlay-tex))
;;     (message "Overlay PDF generated: %s" overlayed-pdf)  ;; Diagnostic output

;;     ;; Check if overlay PDF exists
;;     (unless (file-exists-p overlayed-pdf)
;;       (error "Overlay PDF was not created: %s" overlayed-pdf))

;;     ;; Step 3: Get the number of pages in the overlay PDF
;;     (let* ((pdfinfo-output (shell-command-to-string (format "pdfinfo \"%s\"" overlayed-pdf)))  ;; Get PDF info
;;            (page-count (if (string-match "Pages:\\s-+\\([0-9]+\\)" pdfinfo-output)  ;; Extract number of pages
;;                            (string-to-number (match-string 1 pdfinfo-output))
;;                          (error "Could not get page count for %s" overlayed-pdf))))

;;       ;; Step 4: Create LaTeX file for crop marks with the same number of pages as overlay PDF
;;       (let ((cropmarks-tex (concat temporary-file-directory "cropmarks.tex")))  ;; Temporary LaTeX file for crop marks
;;         (with-temp-file cropmarks-tex
;;           (insert "\\documentclass{article}\n")
;;           (insert "\\usepackage[a4paper,margin=0mm]{geometry}  % Full A4 page\n")
;;           (insert "\\usepackage{tikz}\n")
;;           (insert "\\begin{document}\n")
;;           (insert "\\thispagestyle{empty}\n")

;;           (dotimes (n page-count)  ;; Loop to create pages
;;             (insert "\\begin{tikzpicture}[remember picture, overlay]\n")
;;             (insert "  % Center cross (horizontal and vertical lines crossing in the middle of the A4 page)\n")
;;             (insert "  \\draw[black, thick] (current page.center) ++(-10mm,0) -- ++(20mm,0); % Horizontal line at center\n")
;;             (insert "  \\draw[black, thick] (current page.center) ++(0,-10mm) -- ++(0,20mm); % Vertical line at center\n")
;;             (insert "  % Side crop marks\n")
;;             (insert "  \\draw[black, thick] (current page.north) ++(0,-5mm) -- ++(0,-10mm);   % Top center\n")
;;             (insert "  \\draw[black, thick] (current page.south) ++(0,5mm) -- ++(0,10mm);     % Bottom center\n")
;;             (insert "  \\draw[black, thick] (current page.west) ++(5mm,0) -- ++(10mm,0);      % Left center\n")
;;             (insert "  \\draw[black, thick] (current page.east) ++(-5mm,0) -- ++(-10mm,0);    % Right center\n")
;;             (insert "\\end{tikzpicture}\n\n")
;;             (insert "\\newpage\n\n"))  ;; Start a new page for each iteration

;;           (insert "\\end{document}\n"))  ;; End of the document

;;         ;; Step 5: Compile the crop marks PDF
;;         (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s"
;;                                temporary-file-directory cropmarks-tex))
;;         (message "Crop marks PDF generated: %s" cropmarks-pdf)))  ;; Diagnostic output

;;       ;; Step 6: Overlay crop marks PDF on the main overlay PDF using pdftk
;;       (let ((cropmarks-pdf (concat temporary-file-directory "cropmarks.pdf")))
;;         (shell-command (format "pdftk \"%s\" multistamp \"%s\" output \"%s\""
;;                                cropmarks-pdf
;;                                overlayed-pdf
;;                                final-pdf)))

;;       ;; Display the final PDF path
;;       (message "Final PDF with crop marks created at: %s" final-pdf)
;;       final-pdf))

(defun sync0-create-pdf-with-overlay (&optional header pdf)
  "Create a PDF with headers and page numbers, generate crop marks PDF, then overlay it on a user-selected PDF."
  (interactive)  ;; Prompt for header text
  (let* ((header (or header (read-string "Enter the header text: ")))
         (overlay-pdf (or pdf (read-file-name "Choose a PDF file: " sync0-bibnotes-directory)))  ;; Prompt for PDF file
         (overlay-tex (concat temporary-file-directory "overlay.tex"))  ;; Temporary LaTeX file for overlay
         (overlayed-pdf (concat temporary-file-directory "overlay.pdf"))  ;; Temporary PDF for overlay
         (cropmarks-pdf (concat temporary-file-directory "cropmarks.pdf"))  ;; Temporary PDF for crop marks
         (final-pdf (concat (file-name-sans-extension overlay-pdf) "-with-cropmarks.pdf")))  ;; Output PDF file

    ;; Step 1: Create the LaTeX file to include overlay PDF with headers
    (with-temp-file overlay-tex
      (insert (format "
\\documentclass{article}
\\usepackage[a6paper,top=17.5mm,right=5mm,bottom=5mm,bindingoffset=11mm,nofoot]{geometry}
\\usepackage{pdfpages}  %% For including PDF pages
\\usepackage{pgfpages}  %% Have 4 A6 sheets fit an A4 page.
\\pgfpagesuselayout{4 on 1}[a4paper,border shrink=0mm,odd numbered pages right]
\\usepackage{fancyhdr}  %% For custom headers and footers
\\usepackage{fontspec}  %% To specify custom fonts
\\setmainfont{Consolas}  %% Set Consolas font
\\pagestyle{fancy}
\\fancyhf{}
\\fancyhead[R]{%s/\\thepage}  %% Header text with page number
\\begin{document}
\\includepdf[
  pages=-,
  width=0.85\\paperwidth,
  offset=0 -2.5mm,
  pagecommand={\\thispagestyle{fancy}}
]{%s}
\\end{document}"
                      header overlay-pdf)))  ;; Insert header and overlay PDF path

    ;; Step 2: Compile the overlay PDF
    (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s"
                           temporary-file-directory overlay-tex))
    (message "Overlay PDF generated: %s" overlayed-pdf)  ;; Diagnostic output

    ;; Check if overlay PDF exists
    (unless (file-exists-p overlayed-pdf)
      (error "Overlay PDF was not created: %s" overlayed-pdf))

    ;; Step 3: Get the number of pages in the overlay PDF
    (let* ((pdfinfo-output (shell-command-to-string (format "pdfinfo \"%s\"" overlayed-pdf)))  ;; Get PDF info
           (page-count (if (string-match "Pages:\\s-+\\([0-9]+\\)" pdfinfo-output)  ;; Extract number of pages
                           (string-to-number (match-string 1 pdfinfo-output))
                         (error "Could not get page count for %s" overlayed-pdf))))

      ;; Step 4: Create LaTeX file for crop marks with the same number of pages as overlay PDF
      (let ((cropmarks-tex (concat temporary-file-directory "cropmarks.tex")))  ;; Temporary LaTeX file for crop marks
        (with-temp-file cropmarks-tex
          (insert "\\documentclass{article}\n")
          (insert "\\usepackage[a4paper,margin=0mm]{geometry}  % Full A4 page\n")
          (insert "\\usepackage{tikz}\n")
          (insert "\\begin{document}\n")
          (insert "\\thispagestyle{empty}\n")

          (dotimes (n page-count)  ;; Loop to create pages
            (insert "\\begin{tikzpicture}[remember picture, overlay]\n")
            (insert "  % Center cross (horizontal and vertical lines crossing in the middle of the A4 page)\n")
            (insert "  \\draw[black, thick] (current page.center) ++(-10mm,0) -- ++(20mm,0); % Horizontal line at center\n")
            (insert "  \\draw[black, thick] (current page.center) ++(0,-10mm) -- ++(0,20mm); % Vertical line at center\n")
            (insert "  % Side crop marks\n")
            (insert "  \\draw[black, thick] (current page.north) ++(0,-5mm) -- ++(0,-10mm);   % Top center\n")
            (insert "  \\draw[black, thick] (current page.south) ++(0,5mm) -- ++(0,10mm);     % Bottom center\n")
            (insert "  \\draw[black, thick] (current page.west) ++(5mm,0) -- ++(10mm,0);      % Left center\n")
            (insert "  \\draw[black, thick] (current page.east) ++(-5mm,0) -- ++(-10mm,0);    % Right center\n")
            (insert "\\end{tikzpicture}\n\n")
            (insert "\\newpage\n\n"))  ;; Start a new page for each iteration

          (insert "\\end{document}\n"))  ;; End of the document

        ;; Step 5: Compile the crop marks PDF
        (shell-command (format "lualatex -interaction=nonstopmode -output-directory=%s %s"
                               temporary-file-directory cropmarks-tex))
        (message "Crop marks PDF generated: %s" cropmarks-pdf)))  ;; Diagnostic output

      ;; Step 6: Overlay crop marks PDF on the main overlay PDF using pdftk
      (shell-command (format "pdftk \"%s\" multistamp \"%s\" output \"%s\""
                             overlayed-pdf
                             cropmarks-pdf
                             final-pdf))

      ;; Display the final PDF path
      (message "Final PDF with crop marks created at: %s" final-pdf)
      final-pdf))

(provide 'sync0-pdf)
