
(defun sync0-lyon2-create-student-list ()
  "Prompt for a CSV file and create a list of students formatted for roll call.
If a previous file was used, it proposes that file as the default choice."
  (interactive)
  (let* ((default-file sync0-last-file)
         (default-file-name (when default-file
			      (file-name-nondirectory default-file)))
         (default-path (if default-file
			   (file-name-directory default-file)
			 sync0-directory-teaching))
         (file-path
	  (if default-file
	      (read-file-name "Choose the CSV file: " default-path default-file-name)
	    (read-file-name "Choose the CSV file: " sync0-directory-teaching))))
    (if (file-exists-p file-path)
        (progn
          (setq sync0-last-file file-path) ;; Store the file path
          (create-student-list file-path))
      (message "Error: File does not exist!"))))

(defun sync0-lyon2-generate-group-student-lists (directory)
  "Generates a report listing students in each group from CSV files in the specified DIRECTORY.
If a previous directory was used, it proposes that directory as the default choice.
Assumes each CSV file represents a group, with the file name as the group name and each row (after the header) as a student.
The report is also copied to the clipboard for easy handling."
  (interactive
   (let* ((default-dir sync0-last-directory)
          (default-path (or default-dir sync0-directory-teaching)))
     (list (read-directory-name "Select directory with CSV files: " default-path))))
  (setq sync0-last-directory directory)  ;; Store the selected directory
  (let ((csv-files (directory-files directory t "\\.csv$"))
        (report ""))
    (dolist (file csv-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((lines (cdr (split-string (buffer-string) "\n" t))) ;; Skip header
               (group-name (file-name-base file))
               (student-list "")
               (student-count 0))
          ;; Process each line to extract student information
          (dolist (line lines)
            (let* ((fields (split-string line ";"))
                   (nom (nth 2 fields))     ;; Extract "Nom"
                   (prenom (nth 3 fields))  ;; Extract "Prénom"
                   (numero (nth 4 fields))  ;; Extract "Numéro"
                   (email (nth 7 fields))   ;; Extract "Mail"
                   (formatted-student (format "- %s, %s (%s) : %s\n" nom prenom numero email)))
              (setq student-list (concat student-list formatted-student))
              (setq student-count (1+ student-count))))
          ;; Append group name, student count, and student list to report
          (setq report (concat report (format "## %s (%d students):\n%s\n" group-name student-count student-list))))))
    (if (string= report "")
        (message "No CSV files found in the directory.")
      (progn
        (kill-new report)  ;; Copy the report to the clipboard
        (message "%s\n\n[Copied to clipboard]" report)))))

(defun sync0-lyon2-count-students-per-group (directory)
  "Counts the number of students in each CSV file within the specified DIRECTORY.
If a previous directory was used, it proposes that directory as the default choice.
The function assumes each CSV file represents a group and that the first row is a header,
with each subsequent row representing a student."
  (interactive
   (let* ((default-dir sync0-last-directory)
          (default-path (or default-dir sync0-directory-teaching)))
     (list (read-directory-name "Select directory with CSV files: " default-path))))
  (setq sync0-last-directory directory)  ;; Store the selected directory
  (let ((csv-files (directory-files directory t "\\.csv$"))
        (results ""))
    (dolist (file csv-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((lines (split-string (buffer-string) "\n" t))
               (group-name (file-name-base file))
               (student-count (1- (length lines)))) ;; Subtract header row
          (setq results (concat results (format "## %s: %d students\n" group-name student-count))))))
    (if (string= results "")
        (message "No CSV files found in the directory.")
      (message "%s" results))))

(defun create-student-list (file-path)
  "Create a list of students from a CSV file, formatted for roll call."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((students (split-string (buffer-string) "\n" t))
          (result ""))
      ;; Skip header row
      (setq students (cdr students))
      ;; Process each student
      (dolist (student students)
        ;; Split the student line by `;` to extract fields
        (let* ((fields (split-string student ";"))
               (nom (nth 2 fields))     ;; Extract "Nom"
               (prenom (nth 3 fields))  ;; Extract "Prénom"
               (numero (nth 4 fields))  ;; Extract "Numéro"
               (email (nth 7 fields))   ;; Extract "Mail"
               (formatted-student (format "- [ ] %s, %s (%s) : %s\n" nom prenom numero email)))
          (setq result (concat result formatted-student))))
      (kill-new result)  ;; Copy the result to the clipboard
      (message "Student list created and copied to clipboard!"))))

;; (defun create-student-list (file-path)
;;   "Create a list of students from a CSV file, formatted for roll call."
;;   (with-temp-buffer
;;     (insert-file-contents file-path)
;;     (let ((students (split-string (buffer-string) "\n" t))
;;           (result ""))
;;       ;; Skip header row
;;       (setq students (cdr students))
;;       ;; Process each student
;;       (dolist (student students)
;;         ;; Split the student line by `;` to extract fields
;;         (let* ((fields (split-string student ";"))
;;                (nom (nth 2 fields))     ;; Extract "Nom"
;;                (prenom (nth 3 fields))  ;; Extract "Prénom"
;;                (numero (nth 4 fields))  ;; Extract "Numéro"
;;                (email (nth 7 fields))   ;; Extract "Mail"
;;                (formatted-student (format "%s, %s (%s) : %s\n" nom prenom numero email)))
;;           (setq result (concat result formatted-student))))
;;       (kill-new result)  ;; Copy the result to the clipboard
;;       (message "Student list created and copied to clipboard!"))))

(defun shuffle-list (list)
  "Randomly shuffle LIST."
  (let ((vector (vconcat list)))
    (dotimes (i (length vector) list)
      (let ((j (+ i (random (- (length vector) i)))))
        (cl-rotatef (aref vector i) (aref vector j))))
    (append vector nil)))  ;; Convert back to a list

(defun sync0-create-student-groups (file-path group-size)
  "Create random groups of students from a CSV file and format them in Markdown."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((students (split-string (buffer-string) "\n" t))
          (group-count 1)
          (group '())
          (result ""))
      ;; Skip header row
      (setq students (cdr students))
      ;; Randomize students
      (setq students (shuffle-list students))
      ;; Process each student
      (dolist (student students)
        ;; Split the student line by `;` to extract fields
        (let* ((fields (split-string student ";"))
               (nom (nth 2 fields))     ;; Extract "Nom"
               (prenom (nth 3 fields))  ;; Extract "Prénom"
               (numero (nth 4 fields))  ;; Extract "Numéro"
               (email (nth 7 fields))   ;; Extract "Mail"
               (formatted-student (format "    - %s, %s (%s) : %s\n" nom prenom numero email)))
          (push formatted-student group))
        ;; If the group reaches the specified size or there are no more students
        (when (or (>= (length group) group-size) (not students))
          (setq result (concat result (format "- Group %d\n%s\n" group-count (apply 'concat (reverse group)))))
          (setq group '())
          (setq group-count (1+ group-count))))
      (kill-new result)  ;; Copy the result to the clipboard
      (message "Randomized groups created and copied to clipboard!"))))

(defun create-student-pdf (file-path)
  "Generate a PDF of the student list from a CSV file, using Literata font."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((students (split-string (buffer-string) "\n" t))
          (md-file "students.md")
          (pdf-file "students.pdf"))
      ;; Skip header row
      (setq students (cdr students))
      ;; Prepare Markdown content
      (with-temp-file md-file
        (insert "---\n")
        (insert "geometry: a6paper\n")
        (insert "fontsize: 12pt\n")
        (insert "header-includes:\n")
        (insert "  - \\usepackage{fontspec}\n")
        (insert "  - \\setmainfont{Literata}\n")
        (insert "---\n\n")
        ;; Process each student
        (dolist (student students)
          (let* ((fields (split-string student ";"))
                 (nom (nth 2 fields))     ;; Extract "Nom"
                 (prenom (nth 3 fields))  ;; Extract "Prénom"
                 (numero (nth 4 fields))  ;; Extract "Numéro"
                 (email (nth 7 fields)))  ;; Extract "Mail"
            (insert (format "- %s, %s (%s) : %s\n" nom prenom numero email)))))
      ;; Convert Markdown to PDF using Pandoc
      (call-process "pandoc" nil nil nil md-file "-o" pdf-file "--pdf-engine=pdflatex")
      (message "PDF created: %s" pdf-file))))

(defun sync0-lyon2-create-consolidated-student-list ()
  "Create a consolidated list of students from all CSV files in a directory.
Exports the data to a CSV file 'consolidated_student_list.csv' in the selected directory.
Extracts 'nom', 'prenom', 'numero', and 'email' columns from each CSV.
Adds a 'Class' column based on the filename to distinguish each group."
  (interactive)
  (let* ((default-dir sync0-last-directory)
         (selected-dir (read-directory-name "Choose the directory: "
                                            (or default-dir sync0-directory-teaching))))
    (setq sync0-last-directory selected-dir)  ;; Store the selected directory
    (let ((csv-files (directory-files selected-dir t "\\.csv$"))
          (student-data '(("Class" "Nom" "Prenom" "Numero" "Email")))) ;; Column headers
      (dolist (file csv-files)
        (let ((class-name (file-name-base file))) ;; Use filename (without extension) as class name
          (with-temp-buffer
            (insert-file-contents file)
            (let ((lines (split-string (buffer-string) "\n" t)))
              ;; Skip header row
              (setq lines (cdr lines))
              (dolist (line lines)
                (let* ((fields (split-string line ";"))
                       (nom (nth 2 fields))
                       (prenom (nth 3 fields))
                       (numero (nth 4 fields))
                       (email (nth 7 fields)))
                  ;; Add each student's info as a row, including the class name
                  (push (list class-name nom prenom numero email) student-data)))))))
      ;; Write all data to a consolidated CSV file
      (with-temp-file (expand-file-name "consolidated_student_list.csv" selected-dir)
        (dolist (row (reverse student-data))
          (insert (mapconcat 'identity row ",") "\n")))
      (message "Consolidated student list created at %s" 
               (expand-file-name "consolidated_student_list.csv" selected-dir)))))

(provide 'sync0-lyon2)
