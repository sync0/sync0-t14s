
(defun shuffle-list (list)
  "Randomly shuffle LIST."
  (let ((vector (vconcat list)))
    (dotimes (i (length vector) list)
      (let ((j (+ i (random (- (length vector) i)))))
        (cl-rotatef (aref vector i) (aref vector j))))
    (append vector nil)))  ;; Convert back to a list

(defun sync0-lyon2-create-student-groups (file-path group-size)
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
          (setq result (concat result (format "- Group %d\n%s" group-count (apply 'concat (reverse group)))))
          (setq group '())
          (setq group-count (1+ group-count))))
      (kill-new result)  ;; Copy the result to the clipboard
      (message "Randomized groups created and copied to clipboard!"))))

(defun sync0-lyon2-create-student-list (file-path)
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
               (formatted-student (format "- %s, %s (%s) : %s\n" nom prenom numero email)))
          (setq result (concat result formatted-student))))
      (kill-new result)  ;; Copy the result to the clipboard
      (message "Student list created and copied to clipboard!"))))

(sync0-lyon2-create-student-list "/home/sync0/Gdrive/projects/teaching/2024/2024-09_intro-macro/Etudiantsdugroupe11EAAA03TD003.csv")

(sync0-lyon2-create-student-groups "/home/sync0/Gdrive/projects/teaching/2024/2024-09_intro-macro/Etudiantsdugroupe11EAAA03TD001.csv" 3)

;; (defun sync0-lyon2-create-student-pdf (file-path)
;;   "Generate a PDF of the student list from a CSV file, using Literata font."
;;   (with-temp-buffer
;;     (insert-file-contents file-path)
;;     (let ((students (split-string (buffer-string) "\n" t))
;;           (md-file "students.md")
;;           (pdf-file "students.pdf"))
;;       ;; Skip header row
;;       (setq students (cdr students))
;;       ;; Prepare Markdown content
;;       (with-temp-file md-file
;;         (insert "---\n")
;;         (insert "geometry: a6paper\n")
;;         (insert "fontsize: 9pt\n")
;;         (insert "header-includes:\n")
;;         (insert "  - \\usepackage{fontspec}\n")
;;         (insert "  - \\setmainfont{Literata}\n")
;;         (insert "---\n\n")
;;         ;; Process each student
;;         (dolist (student students)
;;           (let* ((fields (split-string student ";"))
;;                  (nom (nth 2 fields))     ;; Extract "Nom"
;;                  (prenom (nth 3 fields))  ;; Extract "Prénom"
;;                  (numero (nth 4 fields))  ;; Extract "Numéro"
;;                  (email (nth 7 fields)))  ;; Extract "Mail"
;;             (insert (format "- %s, %s (%s) : %s\n" nom prenom numero email)))))
;;       ;; Convert Markdown to PDF using Pandoc
;;       (call-process "pandoc" nil nil nil md-file "-o" pdf-file "--pdf-engine=pdflatex")
;;       (message "PDF created: %s" pdf-file))))

(sync0-lyon2-create-student-pdf "/home/sync0/Gdrive/projects/teaching/2024/2024-09_intro-macro/Etudiantsdugroupe11EAAA03TD001.csv")


(provide 'sync0-temp)
