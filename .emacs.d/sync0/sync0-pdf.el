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

(defun sync0-add-text-to-pdf (pdf-file text-string start-page end-page)
  "Add TEXT-STRING to PDF-FILE on pages from START-PAGE to END-PAGE using cpdf.
Text will be placed in the bottom left corner with font Courier-Bold size 16."
  (interactive
   (list (read-file-name "Choose a PDF file: " sync0-bibnotes-dir)
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
   (list (read-file-name "Choose a PDF file: " sync0-bibnotes-dir)
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

(defun sync0-create-pdf-with-overlay (&optional header pdf)
  "Create a PDF with headers and page numbers, generate crop marks PDF, then overlay it on a user-selected PDF."
  (interactive)  ;; Prompt for header text
  (let* ((header (or header (read-string "Enter the header text: ")))
         (overlay-pdf (or pdf (read-file-name "Choose a PDF file: " sync0-bibnotes-dir)))  ;; Prompt for PDF file
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
            (insert "  \\draw[black, line width=0.3mm] (current page.center) ++(-10mm,0) -- ++(20mm,0); % Horizontal line at center\n")
            (insert "  \\draw[black, line width=0.3mm] (current page.center) ++(0,-10mm) -- ++(0,20mm); % Vertical line at center\n")
            (insert "  % Side crop marks\n")
            (insert "  \\draw[black, line width=0.3mm] (current page.north) ++(0,-5mm) -- ++(0,-10mm);   % Top center\n")
            (insert "  \\draw[black, line width=0.3mm] (current page.south) ++(0,5mm) -- ++(0,10mm);     % Bottom center\n")
            (insert "  \\draw[black, line width=0.3mm] (current page.west) ++(5mm,0) -- ++(10mm,0);      % Left center\n")
            (insert "  \\draw[black, line width=0.3mm] (current page.east) ++(-5mm,0) -- ++(-10mm,0);    % Right center\n")
            (insert "\\end{tikzpicture}\n\n")
            (insert "\\newpage\n\n"))  ;; Start a new page for each iteration

          (insert "\\end{document}\n"))  ;; End of the document

        ;; Step 5: Compile the crop marks PDF
        (shell-command (format "pdflatex -interaction=nonstopmode -output-directory=%s %s"
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
