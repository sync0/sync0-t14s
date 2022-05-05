1. decompose the project string into the individual elements.
  1. Check whether the project string has more than one project
2. add those elements (or element) to the tags string.
3. ...
4. If the property string has no quotes around add them

Bottom line: avoid non quoted strings for the projects tags or any other non unique identifier.  

(setq test "\"the man\" \"the gust\"")



;; check what the project title(s) is 
(let* ((project-prop  (org-entry-get 1 "PROJECT_TITLE")))
;; check if there are more than one titles
;; if true, output a list whose elements are
;; the titles
  (if (string-match-p "\" \"" project-prop)
    (delete " "  (split-string project-prop "\"" "\""))
;;when there is only one title, check wheter it is
;; surrounded by double quotes
;; output a string whose content is the one and only title.  
(if (string-match "\"\\([[:print:]]+\\)\""  project-prop )
    (string-trim project-prop "\"" "\"")
  project-prop)))



(cond ((string-match " and " author)
       ;; create a list with parts 
       (let* ((author-list  (split-string author " and "))
              (names (let (x)
                       (dolist  (element author-list x)
                         (setq x (concat x
                                         (progn
                                           (string-match ", \\([[:graph:]]+\\)$"   element)
                                           (match-string 1 element))
                                         " "
                                         (progn
                                           (string-match "\\([[:graph:]]+\\),"   element)
                                           (match-string 1 element))
                                         ", "))))))
         (substring names 0 -2)))
      ((string-match "^{" author)
       (string-match "{\\([[:print:]]+\\)}" author)
       (match-string 1 author))
      (t (let* ((author-list (split-string author ", "))
                (last-name (nth 0 author-list))
                (first-name (nth 1 author-list)))
           (concat first-name " " last-name))))




            (project-prop  (org-entry-get 1 "PROJECT_TITLE"))
            (project
;;check whether the zettel belongs to two or more projects 



(if (string-match-p "\" \"" project-prop))
(let* ((project-list (split-string project-prop "\"" "\""))


       (author-list  (split-string author " and "))


             (string-trim
                    (split-string-and-unquote
                     (org-entry-get 1 "PROJECT_TITLE") "\"" "\"")))



   (author-fixed (cond ((string-match " and " author)
                           ;; create a list with parts 
                        (let* ((author-list  (split-string author " and "))
                                 (names (let (x)
                                          (dolist  (element author-list x)
                                             (setq x (concat x
                                                     (progn
                                          (string-match ", \\([[:graph:]]+\\)$"   element)
                                                       (match-string 1 element))
                                                                " "
                                                              (progn
                                                                (string-match "\\([[:graph:]]+\\),"   element)
                                                                  (match-string 1 element))
                                                                ", "))))))
                                (substring names 0 -2)))
                             ((string-match "^{" author)
                              (string-match "{\\([[:print:]]+\\)}" author)
                              (match-string 1 author))
                             (t (let* ((author-list (split-string author ", "))
                                       (last-name (nth 0 author-list))
                                       (first-name (nth 1 author-list)))
                                  (concat first-name " " last-name)))))




  (let*  ((zettel-properties
           (let (x)
             (dolist (property sync0-zettelkasten-properties-list x)
                 (when (org-entry-get 1 property)
                      ;; check if there are more than one titles
                      ;; if true, output a list whose elements are
                      ;; the titles
                      (if (string-match-p "\" \"" property)
                          (push
                           (delete " "  (split-string property "\"" "\"")) x) 
             ;;when there is only one title, check wheter it is
             ;; surrounded by double quotes
             ;; output a string whose content is the one and only title.  
             (if (string-match "\"\\([[:print:]]+\\)\""  property)
               (push (match-string 1 property) x)
               (push property  x)))))
  (delete nil x))))
zettel-properties)

;; to preven so much nesting try these examples

(insert test"doctorat" "the_way")

(setq test "\"doctorat\" \"the_way\"")
  
(setq test )

(let ((x (list "jimmy" "jason")))
                      ;; (mapcar #'(lambda (y) (push y x))

                      (append (delete " " (split-string-and-unquote test "\" \"")) x))  

            (if (string-match "\"\\([[:print:]]+\\)\""  value)

  (defun foo ()
    (interactive)
    (let (x)
      (dolist (property sync0-zettelkasten-properties-list x)
        (when-let ((value (org-entry-get 1 property)))
          (if (string-match-p "\" \"" value)
                    (append  (delete " " (split-string-and-unquote value "\" \"")) x)  
            (if (string-match "\"\\([[:print:]]+\\)\""  value)
                (push (match-string 1 value) x)
              (push value  x)))))
      (delete nil x)
      (insert (prin1-to-string x))))

  (defun foo ()
    (interactive)
    (let (x)
      (dolist (property sync0-zettelkasten-properties-list x)
        (when-let ((value (org-entry-get 1 property)))
          (if (string-match-p "\" \"" value)
               (append  (delete " " (split-string-and-unquote value "\" \"")) x)  
            (if (string-match "\"\\([[:print:]]+\\)\""  value)
                (push (match-string 1 value) x)
              (push value  x))))
        x)
      ;;(delete nil x)
      (insert (prin1-to-string x))))











              ;; (mapcar #'(lambda (y) (push y x))


  (defun foo ( )
    (interactive)
    (let (x)
      (dolist (property sync0-zettelkasten-properties-list x)
        (when-let ((value (org-entry-get 1 property)))
          (if (string-match-p "\" \"" value)
              (mapcar #'(lambda (y) (push y x))
                      (delete " " (split-string-and-unquote value "\" \"")))  
            (if (string-match "\"\\([[:print:]]+\\)\""  value)
                (push (match-string 1 value) x)
              (push value  x)))))
      ;; (delete nil x)
      (insert (prin1-to-string x))))


           (let (x)
             (dolist (property sync0-zettelkasten-properties-list x)
                 (when-let ((value (org-entry-get 1 property)))
                      (if (string-match-p "\" \"" value)
      (mapcar #'(lambda (y) (push y x))
       (delete " " (split-string-and-unquote value "\"" "\"")))  
             (if (string-match "\"\\([[:print:]]+\\)\""  value)
               (push (match-string 1 value) x)
               (push value  x)))))
  (delete nil x)
  x) 






  (defun foo ()
    (interactive)
    (let (x)
      (dolist (property sync0-zettelkasten-properties-list x)
        (when-let ((value (org-entry-get 1 property)))
          (if (string-match-p "\" \"" value)
                    (append  (delete " " (split-string-and-unquote value "\" \"")) x)  
            (if (string-match "\"\\([[:print:]]+\\)\""  value)
                (push (match-string 1 value) x)
              (push value  x)))))
      ;;(delete nil x)
      (insert (prin1-to-string x))))


  (defun foo ()
    (interactive)
    (let ((x)
        (value (org-entry-get 1 "PROJECT_TITLE")))
          ;; (when (string-match-p "\" \"" value)
               (append  (delete " " (split-string-and-unquote value "\" \"")) x)  
      ;;(delete nil x)
      (insert (prin1-to-string x))))
