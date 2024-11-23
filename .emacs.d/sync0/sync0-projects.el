(require 'sync0-functions)
(require 'sync0-bibtex-var-functions)

(defvar sync0-projects-alist 
'(("2410pvu" . "49th EBHS and Association of Business Historians Conference")
  ("2493pdn" . "Grupo de pesquisa: Crírica do direito e subjetivdade jurídica (Mascaro)")
  ("23199xnj" . "History of Economics Society Annual Meeting 2024 (Santiago, Chile)"))
"My list of default projects")

;; (defvar sync0-pedagogy-courses-alist 
;; '(("2410pvu" . "49th EBHS and Association of Business Historians Conference")
;;   ("2493pdn" . "Grupo de pesquisa: Crírica do direito e subjetivdade jurídica (Mascaro)")
;;   ("23199xnj" . "History of Economics Society Annual Meeting 2024 (Santiago, Chile)"))
;; "My list of default projects")

(defun sync0-completing-read-projects (collection)
  "Read keywords with completion from COLLECTION.
Return the keywords entered as a list.  If no keywords are
entered, the return value is nil."
  (let* ((prompt (format "Add projects (%s to finish) [%%s]" (sync0-completion-finish-key 'completing-read))))
    (cl-loop for project = (completing-read (format prompt (mapconcat #'identity projects " "))
					    collection) 
             until (string= project "nil")
             collecting (let ((matching-code (car (rassoc project sync0-projects-alist))))
                          (if matching-code
                              matching-code
                            project))
             into projects
             finally return projects)))

(provide 'sync0-projects)
