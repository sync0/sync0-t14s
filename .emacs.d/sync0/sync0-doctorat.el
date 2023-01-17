
(defun sync0-date-creator (year months)
  "year is number, months is a string"
  (let ((year-string (format "%s" year)))
    (cond ((strig= months "JanMars")
           (concat year-string  "-01/" year-string "-03"))
          ((strig= months "AvrJun")
           (concat year-string  "-04/" year-string "-06"))
          ((strig= months "JulSep")
           (concat year-string  "-07/" year-string "-09"))
          ((strig= months "OctDec")
           (concat year-string  "-10/" year-string "-12")))))

(defun sync0-repeat-string-for-list (n list)
  "create a big list that is an iterations of list n times"
  (interactive)
  (let (x)
    (cl-loop repeat n
             do
             (setq x (append list x))
             finally return x)))

;; JDE 2 Série
(let* ((bibfile (completing-read "Which bibliography file to append to ? "
                                 sync0-bibtex-bibliographies))
       (crossrefkey "231gb")
       (file-base "JDE-")
       (voltotal 48)
       (volumes (cl-loop for n from 1 below (1+ voltotal) collect n))
       (keylist (sync0-bibtex-entry-define-keys-list voltotal))
       (crossref-related (sync0-show-elements-of-list keylist ", "))
       (years (cl-loop for n from 1854 below 1866 collect n))
       (year-total (length years))
       (years-list (let (x)
                     (dolist (element years x) 
                       (push (format "%s" element) x)
                       (push (format "%s" element) x)
                       (push (format "%s" element) x)
                       (push (format "%s" element) x))
                     (reverse x)))
       (url-base "http://davidmhart.com/liberty/FrenchPolEc/JDE/02DeuxiemeSerie1854-1865/")
       (vol-letters (mapcar #'(lambda (x) (format "-T%s-" x)) volumes))
       (month-seq (list "JanMars" "AvrJun" "JulSep" "OctDec"))
       (month-list-iter (/ voltotal (length month-seq)))
       (month-list (sync0-repeat-string-for-list month-list-iter month-seq)))
  ;; begin setting common varialbes for all jde vols
  (setq sync0-bibtex-entry-type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
  (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
  (setq sync0-bibtex-entry-title "Journal des économistes")
  (setq sync0-bibtex-entry-subtitle "Revue de la science économique et de la statistique")
  (setq sync0-bibtex-entry-language "french")
  (setq sync0-bibtex-entry-langid "french")
  (setq sync0-bibtex-entry-series "Deuxième Série")
  (setq sync0-bibtex-entry-publisher "Guillaumin")
  (setq sync0-bibtex-entry-location "Paris")
  (setq sync0-bibtex-entry-urldate "2023-01-03")
  (setq sync0-bibtex-entry-created "2023-01-03")
  (setq sync0-bibtex-entry-journaltitle "Journal des économistes")
  (setq sync0-bibtex-entry-related "231gb")
  (setq sync0-bibtex-entry-country "France")
  (setq sync0-bibtex-entry-relatedtype "partof")
  (setq sync0-bibtex-entry-theme "political_economy, economics, politics, liberalism, laissez_faire, publications")
  ;; begin loop
  (cl-loop for i from 0 to voltotal
  ;; define certain vars specific to entry i
    do
    (let* ((bibkey (elt keylist i))
           (year (nth i years-list))
           (month-tag (nth i month-list))
           (month-beg (cond ((string= "JanMars" month-tag) "01")
                            ((string= "AvrJun" month-tag) "04")
                            ((string= "JulSep" month-tag) "07")
                            ((string= "OctDec" month-tag) "10")))
           (month-end (cond ((string= "JanMars" month-tag) "03")
                            ((string= "AvrJun" month-tag) "06")
                            ((string= "JulSep" month-tag) "09")
                            ((string= "OctDec" month-tag) "12")))
           ;; produces a string of the form
           ;; 
           ;; 
           ;; 1854 T1 Janvier Mars :  JDE-1854-T1-JanMars.pdf
           ;; 1854 T2 Avril Juin : JDE-1854-T2-AvrJun.pdf
           ;; 1854 T3 Juillet  Septembr : JDE-1854-T3-JulSep.pdf
           ;; 1854 T4 Octobre  décembre : JDE-1854-T4-OctDec.pdf
           ;; 
           ;; 
           ;; which is necessary to get the end of the url right
           ;; which will be used for downloading the pdfs 
           (url-ending (concat file-base 
                               year
                               ;; (nth i years-list)
                               (nth i vol-letters)
                               ;; (nth i month-list)
                               month-tag
                               ".pdf")))
      (setq sync0-bibtex-entry-key bibkey)
      (setq sync0-bibtex-entry-year year)
      (setq sync0-bibtex-entry-volume (format "%s" (1+ i)))
      (setq sync0-bibtex-entry-url (concat url-base url-ending))
      (setq sync0-bibtex-entry-date (concat year "-" month-beg "/" year "-" month-end))
      (setq sync0-bibtex-entry-keywords (concat 
                                         "created/2023/01/03, urldate/2023/01/03, date/" 
                                         year "/" month-beg ", date/" 
                                         year "/" month-end ", reference/collection, journaltitle/journal_des_economistes, language/french, relatedtype/partof, related/" 
                                         crossrefkey ", year/" year ", theme/political_economy, theme/economics, theme/politics, theme/liberalism, theme/laissez_faire, theme/publications, country/france"))
      (setq sync0-bibtex-entry-file
            (concat ":" sync0-zettelkasten-attachments-directory bibkey ".pdf:PDF"))
          ;; Beginning of loop actions
          ;; append entry to bibfile file.
      (sync0-bibtex-entry-append-to-bibliography bibkey bibfile)))
  (with-temp-file bibfile
   (insert-file-contents bibfile)
   (goto-char  (point-min))
   (search-forward crossrefkey)
   (bibtex-beginning-of-entry)
   (bibtex-make-field (list "related" "Whatever string" crossref-related nil) t)))
  
;; JDE 3e Série
(let* ((bibfile (completing-read "Which bibliography file to append to ? "
                                 sync0-bibtex-bibliographies))
       (crossrefkey "232xe")
       (file-base "JDE-")
       (voltotal 48)
       (volumes (cl-loop for n from 1 to voltotal collect n))
       (keylist (sync0-bibtex-entry-define-keys-list voltotal))
       (crossref-related (sync0-show-elements-of-list keylist ", "))
       ;; the starting year is normal but the ending year has to one
       ;; above the ending year
       (years (cl-loop for n from 1866 below 1878 collect n))
       (year-total (length years))
       (years-list (let (x)
                     (dolist (element years x) 
                       (push (format "%s" element) x)
                       (push (format "%s" element) x)
                       (push (format "%s" element) x)
                       (push (format "%s" element) x))
                     (reverse x)))
       (url-base "http://davidmhart.com/liberty/FrenchPolEc/JDE/03TroisiemeSerie1866-1877/")
       (vol-letters (mapcar #'(lambda (x) (format "-T%s-" x)) volumes))
       (month-seq (list "JanMar" "AvrJun" "JulSep" "OctDec"))
       (month-list-iter (/ voltotal (length month-seq)))
       (month-list (sync0-repeat-string-for-list month-list-iter month-seq)))
  ;; begin setting common varialbes for all jde vols
  (setq sync0-bibtex-entry-type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
  (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
  (setq sync0-bibtex-entry-title "Journal des économistes")
  (setq sync0-bibtex-entry-subtitle "Revue de la science économique et de la statistique")
  (setq sync0-bibtex-entry-language "french")
  (setq sync0-bibtex-entry-langid "french")
  (setq sync0-bibtex-entry-series "Troisième Série")
  (setq sync0-bibtex-entry-publisher "Guillaumin")
  (setq sync0-bibtex-entry-location "Paris")
  (setq sync0-bibtex-entry-urldate "2023-01-03")
  (setq sync0-bibtex-entry-created "2023-01-03")
  (setq sync0-bibtex-entry-journaltitle "Journal des économistes")
  (setq sync0-bibtex-entry-related crossrefkey)
  (setq sync0-bibtex-entry-country "France")
  (setq sync0-bibtex-entry-relatedtype "partof")
  (setq sync0-bibtex-entry-shorthand nil)
  (setq sync0-bibtex-entry-volumes nil)
  (setq sync0-bibtex-entry-theme "political_economy, economics, politics, liberalism, laissez_faire, publications")
  ;; begin loop
  (cl-loop for i from 0 below voltotal
  ;; define certain vars specific to entry i
    do
    (let* ((bibkey (elt keylist i))
           (year (nth i years-list))
           (month-tag (nth i month-list))
           (month-beg (cond ((string= "JanMar" month-tag) "01")
                            ((string= "AvrJun" month-tag) "04")
                            ((string= "JulSep" month-tag) "07")
                            ((string= "OctDec" month-tag) "10")))
           (month-end (cond ((string= "JanMar" month-tag) "03")
                            ((string= "AvrJun" month-tag) "06")
                            ((string= "JulSep" month-tag) "09")
                            ((string= "OctDec" month-tag) "12")))
           ;; produces a string of the form
           ;; 
           ;; 
           ;; 1854 T1 Janvier Mars :  JDE-1854-T1-JanMars.pdf
           ;; 1854 T2 Avril Juin : JDE-1854-T2-AvrJun.pdf
           ;; 1854 T3 Juillet  Septembr : JDE-1854-T3-JulSep.pdf
           ;; 1854 T4 Octobre  décembre : JDE-1854-T4-OctDec.pdf
           ;; 
           ;; 
           ;; which is necessary to get the end of the url right
           ;; which will be used for downloading the pdfs 
           (url-ending (concat file-base 
                               year
                               ;; (nth i years-list)
                               (nth i vol-letters)
                               ;; (nth i month-list)
                               month-tag
                               ".pdf")))
      (setq sync0-bibtex-entry-key bibkey)
      (setq sync0-bibtex-entry-year year)
      (setq sync0-bibtex-entry-volume (format "%s" (1+ i)))
      (setq sync0-bibtex-entry-url (concat url-base url-ending))
      (setq sync0-bibtex-entry-date (concat year "-" month-beg "/" year "-" month-end))
      (setq sync0-bibtex-entry-keywords (concat 
                                         "created/2023/01/03, urldate/2023/01/03, date/" 
                                         year "/" month-beg ", date/" 
                                         year "/" month-end ", reference/collection, journaltitle/journal_des_economistes, language/french, relatedtype/partof, related/" 
                                         crossrefkey ", year/" year ", theme/political_economy, theme/economics, theme/politics, theme/liberalism, theme/laissez_faire, theme/publications, country/france"))
      (setq sync0-bibtex-entry-file
            (concat ":" sync0-zettelkasten-attachments-directory bibkey ".pdf:PDF"))
          ;; Beginning of loop actions
          ;; append entry to bibfile file.
      (sync0-bibtex-entry-append-to-bibliography bibkey bibfile)))
  (with-temp-file bibfile
   (insert-file-contents bibfile)
   (goto-char  (point-min))
   (search-forward crossrefkey)
   (bibtex-beginning-of-entry)
   (bibtex-make-field (list "related" "Whatever string" crossref-related nil) t)))
  
;; JDE 4e Série
(let* ((bibfile (completing-read "Which bibliography file to append to ? "
                                 sync0-bibtex-bibliographies))
       (crossrefkey "232hg")
       (file-base "JDE-")
       (voltotal 48)
       (volumes (cl-loop for n from 1 to voltotal collect n))
       ;; this function fails in a very particular manner although it
       ;; does seem to not produce duplicate keys in the sense that
       ;; the keys produced are checked against the keys that exist in
       ;; the database, the function is not able to prevent itself
       ;; from repeating keys that it is producing at the time it is
       ;; called. In other words, the function is failling to check
       ;; against itself
       (keylist (sync0-bibtex-entry-define-keys-list voltotal))
       (crossref-related (sync0-show-elements-of-list keylist ", "))
       (years (cl-loop for n from 1878 to 1889 collect n))
       (year-total (length years))
       (years-list (let (x)
                     (dolist (element years x) 
                       (push (format "%s" element) x)
                       (push (format "%s" element) x)
                       (push (format "%s" element) x)
                       (push (format "%s" element) x))
                     (reverse x)))
       (url-base "http://davidmhart.com/liberty/FrenchPolEc/JDE/04QuatriemeSerie1878-1889/")
       (vol-letters (mapcar #'(lambda (x) (format "-T%s-" x)) volumes))
       (month-seq (list "JanMar" "AvrJun" "JulSep" "OctDec"))
       (month-list-iter (/ voltotal (length month-seq)))
       (month-list (sync0-repeat-string-for-list month-list-iter month-seq)))
  ;; begin setting common varialbes for all jde vols
  (setq sync0-bibtex-entry-type (completing-read "Choose Bibtex entry type: " sync0-bibtex-entry-types))
  (setq sync0-bibtex-entry-type-downcase (downcase sync0-bibtex-entry-type))
  (setq sync0-bibtex-entry-title "Journal des économistes")
  (setq sync0-bibtex-entry-subtitle "Revue de la science économique et de la statistique")
  (setq sync0-bibtex-entry-language "french")
  (setq sync0-bibtex-entry-langid "french")
  (setq sync0-bibtex-entry-series "Quatrième Série")
  (setq sync0-bibtex-entry-publisher "Guillaumin")
  (setq sync0-bibtex-entry-location "Paris")
  (setq sync0-bibtex-entry-urldate "2023-01-04")
  (setq sync0-bibtex-entry-created "2023-01-04")
  (setq sync0-bibtex-entry-journaltitle "Journal des économistes")
  (setq sync0-bibtex-entry-related crossrefkey)
  (setq sync0-bibtex-entry-country "France")
  (setq sync0-bibtex-entry-relatedtype "partof")
  (setq sync0-bibtex-entry-shorthand nil)
  (setq sync0-bibtex-entry-volumes nil)
  (setq sync0-bibtex-entry-theme "political_economy, economics, politics, liberalism, laissez_faire, publications")
  ;; begin loop
  (cl-loop for i from 0 below voltotal
  ;; define certain vars specific to entry i
    do
    (let* ((bibkey (elt keylist i))
           (year (nth i years-list))
           (month-tag (nth i month-list))
           (month-beg (cond ((string= "JanMar" month-tag) "01")
                            ((string= "AvrJun" month-tag) "04")
                            ((string= "JulSep" month-tag) "07")
                            ((string= "OctDec" month-tag) "10")))
           (month-end (cond ((string= "JanMar" month-tag) "03")
                            ((string= "AvrJun" month-tag) "06")
                            ((string= "JulSep" month-tag) "09")
                            ((string= "OctDec" month-tag) "12")))
           ;; produces a string of the form
           ;; 
           ;; 
           ;; 1854 T1 Janvier Mars :  JDE-1854-T1-JanMars.pdf
           ;; 1854 T2 Avril Juin : JDE-1854-T2-AvrJun.pdf
           ;; 1854 T3 Juillet  Septembr : JDE-1854-T3-JulSep.pdf
           ;; 1854 T4 Octobre  décembre : JDE-1854-T4-OctDec.pdf
           ;; 
           ;; 
           ;; which is necessary to get the end of the url right
           ;; which will be used for downloading the pdfs 
           (url-ending (concat file-base 
                               year
                               ;; (nth i years-list)
                               (nth i vol-letters)
                               ;; (nth i month-list)
                               month-tag
                               ".pdf")))
      (setq sync0-bibtex-entry-key bibkey)
      (setq sync0-bibtex-entry-year year)
      (setq sync0-bibtex-entry-volume (format "%s" (1+ i)))
      (setq sync0-bibtex-entry-url (concat url-base url-ending))
      (setq sync0-bibtex-entry-date (concat year "-" month-beg "/" year "-" month-end))
      (setq sync0-bibtex-entry-keywords (concat 
                                         "created/2023/01/04, urldate/2023/01/04, date/" 
                                         year "/" month-beg ", date/" 
                                         year "/" month-end ", reference/collection, journaltitle/journal_des_economistes, language/french, relatedtype/partof, related/" 
                                         crossrefkey ", year/" year ", theme/political_economy, theme/economics, theme/politics, theme/liberalism, theme/laissez_faire, theme/publications, country/france"))
      (setq sync0-bibtex-entry-file
            (concat ":" sync0-zettelkasten-attachments-directory bibkey ".pdf:PDF"))
          ;; Beginning of loop actions
          ;; append entry to bibfile file.
      (sync0-bibtex-entry-append-to-bibliography bibkey bibfile)))
  (with-temp-file bibfile
   (insert-file-contents bibfile)
   (goto-char  (point-min))
   (search-forward crossrefkey)
   (bibtex-beginning-of-entry)
   (bibtex-make-field (list "related" "Whatever string" crossref-related nil) t)))
  
