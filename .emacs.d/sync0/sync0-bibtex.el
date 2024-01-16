;; -*- lexical-binding: t -*-

(setq sync0-bibtex-bibliobraphy-directory (concat (getenv "HOME") "/Gdrive/bibliographies/")
      sync0-bibtex-default-bibliography (concat (getenv "HOME") "/Gdrive/bibliographies/bibliography.bib")
      sync0-bibtex-master-bibliography (concat (getenv "HOME") "/Gdrive/bibliographies/master.bib")
      sync0-bibtex-sick-bibliography (concat (getenv "HOME") "/Gdrive/bibliographies/sick.bib"))

(setq sync0-bibtex-excluded-bibliographies '("/home/sync0/Gdrive/bibliographies/trash.bib"
                                             "/home/sync0/Gdrive/bibliographies/archived.bib"
                                             "/home/sync0/Gdrive/bibliographies/master.bib"
                                             "/home/sync0/Gdrive/bibliographies/jabref.bib"
                                             "/home/sync0/Gdrive/bibliographies/jabref.bib.sav"
                                             "/home/sync0/Gdrive/bibliographies/exclude.bib"
                                             "/home/sync0/Gdrive/bibliographies/sick.bib"
                                             "/home/sync0/Gdrive/bibliographies/backup.bib"
                                             "/home/sync0/Gdrive/bibliographies/bibliography.bib.bak"))

(defun sync0-bibtex-recalc-bibliographies ()
  "Recalculate files in default bibliography directory
(set by sync0-bibtex-bibliography-directory)."
  (interactive)
  (setq sync0-bibtex-bibliographies 
        (cl-set-difference 
         (directory-files sync0-bibtex-bibliobraphy-directory t ".+\\.bib")
         sync0-bibtex-excluded-bibliographies :test #'equal))
  (when (bound-and-true-p reftex-default-bibliography)
    (setq reftex-default-bibliography sync0-bibtex-bibliographies))
  (when (bound-and-true-p org-ref-default-bibliography)
    (setq org-ref-default-bibliography sync0-bibtex-bibliographies))
  (when (bound-and-true-p bibtex-completion-bibliography)
    (setq bibtex-completion-bibliography sync0-bibtex-bibliographies))
  (let ((x (length sync0-bibtex-bibliographies)))
    (message "%s files have been recognized as bibliographies." x)))

(sync0-bibtex-recalc-bibliographies)

(defun sync0-bibtex-recalc-master-bibliography ()
  "Recalculate files in default bibliography directory
(set by sync0-bibtex-bibliography-directory)."
  (interactive)
  (with-temp-file sync0-bibtex-master-bibliography 
    (let ((bibliographies
           (remove sync0-bibtex-sick-bibliography
                   sync0-bibtex-bibliographies)))
      (dolist (bib bibliographies)
        (insert-file-contents bib)
        (goto-char (point-max))
        (insert "\n")))
    (message "Master bibliography file has been recalculated.")))

(defun sync0-bibtex-visit-bibliography ()
  (interactive)
  (let ((bib-file
         (completing-read "Fichier Biblatex : " sync0-bibtex-bibliographies)))
    (find-file
     (expand-file-name bib-file))))

(provide 'sync0-bibtex)
