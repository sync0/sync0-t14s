
                                          ;; (not (or (lambda (k) (string-match-p "journal" k))
                                          ;;      (lambda (k) (string-match-p "fiche" k))
                                          ;;      (lambda (k) (string-match-p "etc" k))
                                          ;;      (lambda (k) (string-match-p "project" k))
                                          ;;      (lambda (k) (string-match-p "todo" k))
                                          ;;      (lambda (k) (string-match-p "reference" k)))))
(alias (when (equal key "f")
         (let
             ((x (read-string "Alias : " nil nil nil t)))
           (if (string-match-p "," x)
               (string-trim
                (prin1-to-string
                 (split-string-and-unquote x ","))
                "(" ")")
             (concat "\"" x "\"")))


(setq sync0-zettelkasten-projects '("doctorat" "sefardi" "study
techniques" "zettelkasten" "cahiers de
revoltologie" "writing" "programming" "lisp" "python" nil))



(defvar sync0-zettelkasten-excluded-candidates '("journal" "fiche" "etc" "project" "todo" "reference"))

(defvar sync0-zettelkasten-project-directories '("project" "todo"))

(defvar sync0-zettelkasten-projects '("doctorat" "sefardi" "study
techniques" "zettelkasten" "cahiers de
revoltologie" "writing" "programming" "lisp" "python" nil)
"List of projects")

(defvar sync0-zettelkasten-zettel-functions 
'("reference" "log" "repository" "todo" "drill" "notes" nil) 
"List of projects")
 
(defvar sync0-zettelkasten-fiche-types 
'("people" "reference" "place" "publication" "concept" "association" nil) 
"List of projects")

(defun foo ()
"print string with multiple crap "
(interactive)
(let ((x
(completing-read-multiple "Quel projet ?" sync0-zettelkasten-projects)))
(insert (string-trim (prin1-to-string x) "(" ")"))))


              ;; (directory-files-recursively sync0-org-zettel-path ".org$" nil
              ;;                              ;; exclude these directories from title completion candidates
              ;;                              (dolist (i sync0-zettelkasten-excluded-candidates)
              ;;                                (lambda (k) (string-match-p i k))))

(defun sync0-org-capture-zettel-body ()
  (let* (;;(key (org-capture-get :key))
         ;; Determine the filter for title completion candidates
         ;; i.e., do not complete with all files
         (path-elements (split-string-and-unquote sync0-org-zettel-path "/"))
         (zettelkasten-path (split-string-and-unquote sync0-zettelkasten-directory "/"))
         (last-directory (car (cl-set-difference path-elements zettelkasten-path  :test #'equal)))
         (type (if (or (equal last-directory "project")
                       (equal last-directory "todo"))
                   "project"
                 last-directory))
;;; Define the candidates for title completions 
         (candidates
          (cond ((or (equal type "permanent")
                     (equal type "annotation"))
                 (f-files sync0-zettelkasten-directory
                          (lambda (k) (string-match-p "\\<permanent\\>\\|\\<annotation\\>" k)) t))
                ((equal type "project")
                 (f-files sync0-zettelkasten-directory
                          (lambda (k) (string-match-p "\\<todo\\>\\|\\<project\\>" k)) t))
                (t (f-files sync0-org-zettel-path (lambda (k) (string-match-p ".org$" k)) t))))
         ;; Determine the type of the zettel for use in org-property
         (title (completing-read "Titre du Zettel : "
                                 (mapcar  #'(lambda (x) (org-roam-db--get-title x))
                                          candidates)
                                 nil nil nil nil nil t))
         (alias (let ((x
                       (read-string "Alias (comma separated): "
                                    nil nil nil t)))
                  (if (string-match-p "," x)
                      (string-trim
                       (prin1-to-string
                        (split-string-and-unquote x ","))
                       "(" ")")
                    (concat "\"" x "\""))))
         (subtitle (unless (equal type "fiche")
                     (read-string "Sous-titre : " nil nil nil t)))
         (buffer (buffer-file-name))     
         (project (unless (or (equal type  "reference")
                              (equal type "annotation")
                              (equal type "fiche"))
                    (string-trim
                     (prin1-to-string
                      (completing-read-multiple "Quel projet ?"
                                                sync0-zettelkasten-projects)) "(" ")")))
         (fiche-type (when (equal type "fiche")
                       (completing-read "Quel type de fiche ?"
                                        sync0-zettelkasten-fiche-types)))
         (func (unless (or (equal type "fiche")
                           (equal type "annotation")
                           (equal type "reference"))
                 (completing-read-multiple "Quel fonction ?"
                                           sync0-zettelkasten-zettel-functions)))
         (creation  (format-time-string "%Y-%m-%d")))
    ;; define string of zettel
    (concat
     ":PROPERTIES:\n"
     (unless (or (null alias)
                 (equal alias ""))
       (concat ":ROAM_ALIASES: "  alias "\n"))
     ":CREATED: " creation "\n"
     ":LAST_MODIFIED: " creation "\n"
     (if (equal type "project")
         ":ZETTEL_TYPE: project\n"
       (concat ":ZETTEL_TYPE: "  type "\n"))
     (when (equal type "annotation")
       (concat ":ANNOTATION_REFS: " sync0-zettelkasten-annotations-key "\n"))
     (unless (or (null func)
                 (equal func ""))
       (concat ":ZETTEL_FUNCTION: " func "\n"))
     (when (equal type "fiche")
       (concat ":FICHE_TYPE: " fiche-type "\n"))
     (unless (or (null project)
                 (equal project ""))
       (concat ":PROJECT_TITLE: " "\"" project "\"\n"))
     ":END:\n"
     "#+TITLE: " title "\n"
     (unless (or (null subtitle)
                 (equal subtitle ""))
       (concat "#+SUBTITLE: " subtitle "\n"))
     (when (equal last-directory "todo")
       (concat "#+CATEGORY: " (upcase project) "\n"))
     ;; add roam tags according to zettel type
     "#+FILETAGS: "
     (cond ((equal type "annotation")
            (concat ":" sync0-zettelkasten-annotations-key ":" sync0-current-month-downcase
                    (format-time-string ":%Y:\n")))
           ((equal type "project")
            (concat
             (when (equal last-directory "todo") ":todo")
             ":" project ":" sync0-current-month-downcase ":"
             (format-time-string ":%Y:\n")))
           (t (concat ":" sync0-current-month-downcase
                      (format-time-string ":%Y:\n"))))
     "\n"
     "Origin: [[id:" (sync0-org-get-id buffer)
     "]["
     (sync0-org-get-file-title-keyword buffer)
     "]]\n\n"
     (when (equal key "a")
       (concat "Dans la page X de [[id:"
               (sync0-org-get-id buffer)
               "]["
               (sync0-org-get-previous-heading-or-title buffer)
               "]] "
               (sync0-org-get-author-keyword buffer)
               " ")))))



(defun sync0-org-capture-zettel-path ()
  "Output the path where the new zettel will be created"
  (let* ((key (org-capture-get :key)))
    ;; Add this zettel to the productivy chart
    (with-current-buffer
        (find-file-noselect
         (concat sync0-zettelkasten-directory 
                 (format-time-string "chart/%Y%m.org")))
      (goto-char (point-min))
      (let* ((date (format-time-string "%Y/%m/%d"))
             (entry (concat "\n| " date " | 1 | 0 |"))
             (second-exist
              (concat "^| "
                      date                    
                      " |\\([[:blank:]]+\\)|[[:blank:]]+[[:digit:]]+ |$"))
             (previous-value
              (concat "^| " date " |[[:blank:]]+\\([[:digit:]]+\\) |[[:blank:]]+[[:digit:]]+ |$")))
        (cond ((re-search-forward previous-value nil t 1)
               (let* ((old-value (string-to-number
                                  (match-string-no-properties 1)))
                      (new-value (number-to-string
                                  (1+ old-value))))
                 (replace-match new-value nil nil nil 1)))
              ((re-search-forward second-exist nil t 1)
               (replace-match " 1 " nil nil nil 1))
              (t (progn
                   (goto-char (point-max))
                   (insert entry)))))
      ;; second part
      (goto-char (point-min))
      (let* ((word
              (cond ((or (equal key "r") 
                         (equal key "w"))
                     "Références")
                    ((or (equal key "p") 
                         (equal key "t"))
                     "Projets")
                    ((equal key "f") 
                     "Fiches")
                    ((equal key "a") 
                     "Annotations")
                    (t 
                     "Zettel")))
             (regex (concat "^| " word " [[:blank:]]+|[[:blank:]]+\\([[:digit:]]+\\) |")))
        (if (re-search-forward regex nil t 1)
            (let* ((old-value (string-to-number
                               (match-string-no-properties 1)))
                   (new-value (number-to-string
                               (1+ old-value))))
              (replace-match new-value nil nil nil 1))
          (goto-char (point-max))
          (insert (concat "| " word "   |        1 |       0 |")))))
    ;; output the file name and path
    (concat sync0-zettel-path "/" sync0-zettel-filename ".org")))



(defun sync0-org-capture-zettel-body ()
         ;; Determine the filter for title completion candidates
         ;; i.e., do not complete with all files
  (let* ((key (org-capture-get :key))
         (id (org-id-new))
         (filename (format-time-string "%Y%m%d%H%M%S"))
         (path (if  (equal key "z")
                   (completing-read "Dossier de la fiche : "
                      (f-directories sync0-zettelkasten-directory
;; exclude hidden directories from completion
                          (lambda (k) (not (string-match-p "\\.+" k))) t))
          (concat sync0-zettelkasten-directory
                         (cond ((equal key "f")  "fiche")
                               ((equal key "p")  "project")
                               ((equal key "t") "todo")
                               ((equal key "a") "annotation")
                               (t "")))))
         (last-directory (car (last (split-string-and-unquote path "/"))))
         (type (if (or (equal last-directory "project")
                       (equal last-directory "todo"))
                   "project"
                 last-directory))
;;; Define the candidates for title completions 
         (candidates
          (cond ((or (equal type "permanent")
                     (equal type "annotation"))
                 (f-files sync0-zettelkasten-directory
                          (lambda (k) (string-match-p "\\<permanent\\>\\|\\<annotation\\>" k)) t))
                ((equal type "project")
                 (f-files sync0-zettelkasten-directory
                          (lambda (k) (string-match-p "\\<todo\\>\\|\\<project\\>" k)) t))
                (t (f-files path (lambda (k) (string-match-p ".org$" k)) t))))
         ;; Determine the type of the zettel for use in org-property
         (title (completing-read "Titre du Zettel : "
                                 (mapcar  #'(lambda (x) (org-roam-db--get-title x))
                                          candidates)
                                 nil nil nil nil nil t))
         (alias (let ((x
                       (read-string "Alias (comma separated): "
                                    nil nil nil t)))
                  (if (string-match-p "," x)
                      (string-trim
                       (prin1-to-string
                        (split-string-and-unquote x ","))
                       "(" ")")
                    (concat "\"" x "\""))))
         (subtitle (unless (equal type "fiche")
                     (read-string "Sous-titre : " nil nil nil t)))
         (buffer (buffer-file-name))     
         (project (unless (or 
                              (equal type "annotation")
                              (equal type "fiche"))
                    (string-trim
                     (prin1-to-string
                      (completing-read-multiple "Quel projet ?"
                                                sync0-zettelkasten-projects)) "(" ")")))
         (fiche-type (when (equal type "fiche")
                       (completing-read "Quel type de fiche ?"
                                        sync0-zettelkasten-fiche-types)))
         (func (unless (or (equal type "fiche")
                           (equal type "annotation"))
                 (completing-read-multiple "Quel fonction ?"
                                           sync0-zettelkasten-zettel-functions)))
         (creation  (format-time-string "%Y-%m-%d")))
;; filename
    (setq  sync0-zettel-filename filename)
    (setq  sync0-zettel-path path)
    ;; define string of zettel
    (concat
     ":PROPERTIES:\n"
     ":ID:      " id "\n"
     (unless (or (null alias)
                 (equal alias "\"\""))
       (concat ":ROAM_ALIASES: "  alias "\n"))
     ":CREATED: " creation "\n"
     ":LAST_MODIFIED: " creation "\n"
     (if (equal type "project")
         ":ZETTEL_TYPE: project\n"
       (concat ":ZETTEL_TYPE: "  type "\n"))
     (when (equal type "annotation")
       (concat ":ANNOTATION_REFS: " sync0-zettelkasten-annotations-key "\n"))
     (unless (or (null func)
                 (equal func ""))
       (concat ":ZETTEL_FUNCTION: " func "\n"))
     (when (equal type "fiche")
       (concat ":FICHE_TYPE: " fiche-type "\n"))
     (unless (or (null project)
                 (equal project "nil"))
       (concat ":PROJECT_TITLE: " "\"" project "\"\n"))
     ":END:\n"
     "#+TITLE: " title "\n"
     (unless (or (null subtitle)
                 (equal subtitle ""))
       (concat "#+SUBTITLE: " subtitle "\n"))
     (when (equal last-directory "todo")
       (concat "#+CATEGORY: " (upcase project) "\n"))
     ;; add roam tags according to zettel type
     "#+FILETAGS: "
     (cond ((equal type "annotation")
            (concat ":" sync0-zettelkasten-annotations-key ":" sync0-current-month-downcase
                    (format-time-string ":%Y:\n")))
           ((equal type "project")
            (concat
             (when (equal last-directory "todo") ":todo")
             ":" project ":" sync0-current-month-downcase ":"
             (format-time-string ":%Y:\n")))
           (t (concat ":" sync0-current-month-downcase
                      (format-time-string ":%Y:\n"))))
     "\n"
     "Origin: [[id:" (sync0-org-get-id buffer)
     "]["
     (sync0-org-get-file-title-keyword buffer)
     "]]\n\n"
     (if (equal type "annotation")
       (concat "Dans la page X de [[id:"
               (sync0-org-get-id buffer)
               "]["
               (sync0-org-get-previous-heading-or-title buffer)
               "]] "
               (sync0-org-get-author-keyword buffer)
               " %?")
"%?"
       ))))
