
    (defhydra sync0-hydra-org-roam-insert (:color blue :hint nil)
    "
    ^Zettelkasten link insert functions^   
    ^--------------------
    ^Org-roam^          ^Org-mode^          ^Org-roam-bibtex^  ^Org-emms^
    ^----------------------------------------- 
    _i_nsert roam link  insert org _l_ink   citation link    _t_rack link
    _r_oam buffer       _s_tore link        note actions     track _p_osition 
    _b_uild cache       last stored lin_k_        
    plot _g_raph
    _d_efine property

    _q_uit
    "
          ("i" sync0-org-roam-insert)
          ("d" sync0-zettelkasten-set-property)
          ("r" org-roam)
          ("b" org-roam-db-build-cache)
          ("g" org-roam-graph)
          ("l" org-insert-link)
          ("s" org-store-link)
          ("k" org-insert-last-stored-link)
          ("t" org-emms-insert-track)
          ("p" org-emms-insert-track-position)
          ;; ("c" orb-insert)
          ;; ("a" orb-note-actions)
          ("q" nil :color blue))


(defun sync0-zettelkasten-set-property ()
  (interactive)
  (let  ((property
          (completing-read "Quel projet ?"
                           sync0-zettelkasten-all-properties-list)))
    (cond ((equal property "ROAM_ALIASES")
           (let* ((x (read-string "Alias (comma separated): "
                                  nil nil nil t))
                  (alias-string (if (string-match-p "," x)
                                    (string-trim
                                     (prin1-to-string
                                      (split-string-and-unquote x ","))
                                     "(" ")")
                                  (concat "\"" x "\""))))
             (org-set-property "ROAM_ALIASES" alias-string)))
          ((equal property "FICHE_TYPE")
           (let ((x (completing-read "Quel type de fiche ?"
                                     sync0-zettelkasten-fiche-types)))
             (org-set-property property (concat "\"" x "\""))))
          ((equal property "ZETTEL_FUNCTION")
           (let* ((x (completing-read-multiple "Quel type de fiche ?"
                                               sync0-zettelkasten-zettel-functions))
                  (y 
                   (if (> (length x) 1)
                       (string-trim
                        (prin1-to-string x)
                        "(" ")")
                     (concat "\""   (car x) "\""))))
             (org-set-property property y)))
          ((equal property "PROJECT_TITLE")
           (let* ((x (completing-read-multiple "Quel project ?"
                                               sync0-zettelkasten-projects))
                  (y 
                   (if (> (length x) 1)
                       (string-trim
                        (prin1-to-string x)
                        "(" ")")
                     (concat "\""   (car x) "\""))))
             (org-set-property property y)))
          (t (let ((x (read-string "What to set this thing to? "
                                  nil nil nil t)))
             (org-set-property property x))))))

                     




  (org-with-point-at 1
    (let* ((x (read-string "Alias (comma separated): "
                           nil nil nil t))
           (alias-string  (if (string-match-p "," x)
                              (string-trim
                               (prin1-to-string
                                (split-string-and-unquote x ","))
                               "(" ")")
                            (concat "\"" x "\""))))
      (org-set-property "ROAM_ALIASES" alias-string))))



(defun sync0-zettelkasten-define-aliases ()
  (interactive)
  (org-with-point-at 1
    (let* ((x (read-string "Alias (comma separated): "
                           nil nil nil t))
           (alias-string  (if (string-match-p "," x)
                              (string-trim
                               (prin1-to-string
                                (split-string-and-unquote x ","))
                               "(" ")")
                            (concat "\"" x "\""))))
      (org-set-property "ROAM_ALIASES" alias-string))))


(defvar sync0-zettelkasten-fiche-types 
  '("people" "reference" "place" "publication" "concept" "association" nil) 
  "List of projects")

  (defvar sync0-zettelkasten-properties-list
  '("ZETTEL_TYPE" "BIBLATEX_TYPE" "ZETTEL_FUNCTION" "FICHE_TYPE" "PROJECT_TITLE" "ANNOTATION_REFS" "ROAM_REFS") 
  "List of zettel properties")

(setq sync0-zettelkasten-properties-list
  '("ZETTEL_TYPE" "BIBLATEX_TYPE" "ZETTEL_FUNCTION" "FICHE_TYPE" "PROJECT_TITLE" "ANNOTATION_REFS" "ROAM_REFS") 
      )


(defun sync0-zettelkasten-update-org-properties ()
  (interactive)
          ;; this functions returns a list of conses whose second
          ;; element is the value of the property (each element is a dot list)
          ;; or nil when the property has not been defined for the
          ;; current zettel
  (let*  ((zettel-properties
           (let (x)
             (dolist (property sync0-zettelkasten-properties-list x)
               (push  (org-entry-get 1 property) x))
             (delete nil x)))
          (type  (org-entry-get 1 "ZETTEL_TYPE"))
          (project  (org-entry-get 1 "PROJECT_TITLE"))
          (corrected-project
           (downcase  
            (replace-regexp-in-string "[[:space:]]+" "_"
                                      (string-trim
                                       (org-entry-get 1 "PROJECT_TITLE") "\"" "\""))))
          (path default-directory)
          (path-dirs (split-string-and-unquote path "/"))
          (zettelkasten-dirs (split-string-and-unquote sync0-zettelkasten-directory "/"))
          (current-dir (car (cl-set-difference path-dirs zettelkasten-dirs  :test #'equal)))
          (new-elements (list corrected-project current-dir))
          (corrected-properties (cl-union new-elements zettel-properties  :test #'equal))                                 
          ;; extract the existing tags in the tags line
          (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
          (tags (split-string-and-unquote tags-line ":"))
          (new-tags (cl-union corrected-properties tags :test #'equal))
          (new-tags-line
           (let (x)
             (dolist (element new-tags x)
               (setq x (concat  element ":" x)))))
          (corrected-tags-line (concat ":" new-tags-line)))
  (org-with-point-at 1
(re-search-forward "^#\\+FILETAGS:" (point-max) t)
(kill-whole-line 1)
(insert (concat "#+FILETAGS: " corrected-tags-line "\n"))
(when  (not (equal project corrected-project))
        (org-set-property "PROJECT_TITLE" corrected-project))
    (when (not (equal type current-dir))
        (org-set-property "ZETTEL_TYPE" current-dir)))))
      

  (defun foo ()
    (interactive)
    (let 
        ((projects (string-trim
                    (split-string-and-unquote
                     (org-entry-get 1 "PROJECT_TITLE") "\"" "\"")))

         (corrected-project
          (downcase  
           (replace-regexp-in-string "[[:space:]]+" "_"
                                     (string-trim
                                      (org-entry-get 1 "PROJECT_TITLE") "\"" "\"")))))
      (insert (concat projects "\n"))
      (insert corrected-project)))




-1. create list of my own properties
-2. have the functions extract for those properties 
-3. have the function extract the existing tags
4. have the function compare tags and properties
5. have the function delete and replace the tags line wiht the properties


(defun sync0-zettelkasten-update-org-properties ()
  (interactive)
  ;; this functions returns a list of conses whose second
  ;; element is the value of the property (each element is a dot list)
  ;; or nil when the property has not been defined for the
  ;; current zettel




  (let*  ((zettel-properties
           (let (x)
             (dolist (property sync0-zettelkasten-properties-list x)
                 (when (org-entry-get 1 property)
                      ;; check if there are more than one titles
                      ;; if true, output a list whose elements are
                      ;; the titles
                      (if (string-match-p "\" \"" property)
                          (push
                           (delete " "  (split-string property "\"" "\""))
                           x)
                        ;;when there is only one title, check wheter it is
                        ;; surrounded by double quotes
                        ;; output a string whose content is the one and only title.  
                        (if (string-match "\"\\([[:print:]]+\\)\""  property)
                          (push (string-trim property "\"" "\"") x)
                          (push property  x)))))
             (delete nil x)))
          (type  (org-entry-get 1 "ZETTEL_TYPE"))
          (corrected-type  "\"" type "\"")
          ;;(project  (org-entry-get 1 "PROJECT_TITLE"))
          ;; check what the project title(s) is 
          (projects (when (org-entry-get 1 "PROJECT_TITLE")
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
                          project-prop))))













  (let*  ((zettel-properties
           (let (x)
             (dolist (property sync0-zettelkasten-properties-list x)
               (push  (org-entry-get 1 property) x))
             (delete nil x)))
          (type  (org-entry-get 1 "ZETTEL_TYPE"))
          (corrected-type  "\"" type "\"")
          ;;(project  (org-entry-get 1 "PROJECT_TITLE"))
          ;; check what the project title(s) is 
          (projects (when (org-entry-get 1 "PROJECT_TITLE")
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
                          project-prop))))
          (corrected-project
           (when project
             (downcase  
              (replace-regexp-in-string "[[:space:]]+" "_"
                                        (string-trim
                                         (org-entry-get 1 "PROJECT_TITLE") "\"" "\"")))))
          (path default-directory)
          (path-dirs (split-string-and-unquote path "/"))
          (zettelkasten-dirs (split-string-and-unquote sync0-zettelkasten-directory "/"))
          (current-dir (car (cl-set-difference path-dirs zettelkasten-dirs  :test #'equal)))
          (new-elements 
           (if project
               (list corrected-project current-dir)
             (list current-dir)))
          (corrected-properties (cl-union new-elements zettel-properties  :test #'equal))                                 
          ;; extract the existing tags in the tags line
          (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
          (tags (split-string-and-unquote tags-line ":"))
          (new-tags (cl-union corrected-properties tags :test #'equal))
          (new-tags-line
           (let (x)
             (dolist (element new-tags x)
               (setq x (concat element  ":" x)))))
          (corrected-tags-line (concat ":" new-tags-line)))
    (org-with-point-at 1
      (re-search-forward "^#\\+FILETAGS:" (point-max) t)
      (kill-whole-line 1)
      (insert (concat "#+FILETAGS: " corrected-tags-line "\n"))
      (when project 
        (unless  (equal project corrected-project)
          (org-set-property "PROJECT_TITLE" corrected-project)))
      (unless (equal type current-dir)
        (org-set-property "ZETTEL_TYPE" current-dir)))))


(defun sync0-downcase-and-no-whitespace (x)
    (downcase
     (replace-regexp-in-string "[[:space:]]+" "_" x)))



(defun foo ()
  (interactive)
  (let*  ((zettel-properties
           (let (x)
             (dolist (property sync0-zettelkasten-properties-list x)
               (when-let ((value (org-entry-get 1 property)))
                 (if (string-match-p "\" \"" value)
                     (let ((elements
                            (delete " "
                                    (split-string-and-unquote value "\" \""))))  
                       (mapcar #'(lambda (y)
                                   (push
                                    (sync0-downcase-and-no-whitespace y)
                                    x)) elements))
                   ;; (delete " " (split-string-and-unquote value "\" \"")))  
                   (if (string-match "\"\\([[:print:]]+\\)\""  value)
                       (push
                        (sync0-downcase-and-no-whitespace (match-string 1 value))
                        x)
                     (push (sync0-downcase-and-no-whitespace value)  x)))))
             x))
          (path default-directory)
          (path-dirs (split-string-and-unquote path "/"))
          (zettelkasten-dirs (split-string-and-unquote sync0-zettelkasten-directory "/"))
          ;; this produces a list not a string
          (current-dir  (cl-set-difference path-dirs zettelkasten-dirs  :test #'equal))
          (corrected-properties (cl-union current-dir zettel-properties  :test #'equal))                                 
          (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
          (tags (split-string-and-unquote tags-line ":"))
          (new-tags (cl-union corrected-properties tags :test #'equal))
          (new-tags-line
           (let (x)
             (dolist (element new-tags x)
               (setq x (concat element  ":" x)))))
          (corrected-tags-line (concat ":" new-tags-line)))
    (org-with-point-at 1
      (re-search-forward "^#\\+FILETAGS:" (point-max) t)
      (kill-whole-line 1)
      (insert (concat "#+FILETAGS: " corrected-tags-line "\n"))
(dolist (property sync0-zettelkasten-properties-list)
       (when-let ((value (org-entry-get 1 property)))
         (unless (or (string-match-p "\" \"" value)
                     (string-match-p "\"[[:print:]]+\"" value))
          (org-set-property property (concat "\""  value "\""))))))))

                       (push
                        (sync0-downcase-and-no-whitespace (match-string 1 value))
                        x)
                     (push (sync0-downcase-and-no-whitespace value)  x)))))
             x))



        (unless (string-match-p "\" \"" value)

      (when  (string-match "\"\\([[:print:]]+\\)\""  property)


                   (if (string-match "\"\\([[:print:]]+\\)\""  value)


      (when project 
        (unless  (equal project corrected-project)
          (org-set-property "PROJECT_TITLE" corrected-project)))
      (unless (equal type current-dir)
        (org-set-property "ZETTEL_TYPE" current-dir)))))




(insert corrected-tags-line)))


    (org-with-point-at 1
      (re-search-forward "^#\\+FILETAGS:" (point-max) t)
      (kill-whole-line 1)
      (insert (concat "#+FILETAGS: " corrected-tags-line "\n"))
      (when project 
        (unless  (equal project corrected-project)
          (org-set-property "PROJECT_TITLE" corrected-project)))
      (unless (equal type current-dir)
        (org-set-property "ZETTEL_TYPE" current-dir)))))



                 ;; (delete nil x)
                 (insert (prin1-to-string x))))
              (type  (org-entry-get 1 "ZETTEL_TYPE")))
