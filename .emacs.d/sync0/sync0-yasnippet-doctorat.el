(require 'bibtex-completion)

(setq sync0-doctorat-reference-types '("history" "background" "mention" "hommage" "support" "critical" "contextual" "nil"))

(setq sync0-doctorat-country-list '("rome" "greece" "france" "chile" "usa" "uk" "germany" "italy" "spain" "nil"))

(setq sync0-doctorat-school-list '("socialism" "liberalism" "republicanism" "nationalism" "nil"))

(defun sync0-doctorat-complete-biblatex-key ()
    (let* ((candidates (bibtex-completion-candidates))
           (key (bibtex-completion-key-at-point))
           (selection (ivy-read "BibTeX entries: "
                                candidates
                                ;; :preselect preselect
                                :caller 'ivy-bibtex
                                :history 'ivy-bibtex-history)))

      (cdr (assoc "=key=" (cdr (assoc selection candidates))))))


(defun sync0-doctorat-complete-reference-types ()
  (completing-read "BibLaTex entry type: " sync0-doctorat-reference-types))

(defun sync0-doctorat-complete-languages ()
  (completing-read "BibLaTex entry type: " sync0-biblatex-languages))

(defun sync0-doctorat-complete-explicit ()
  (completing-read "BibLaTex entry type: " '("explicit" "implicit")))

(defun sync0-doctorat-complete-person-id ()
(let* ((string  (with-temp-buffer
                  (insert-file-contents
                   "/home/sync0/Dropbox/org/data/20210503164252.org")
                  (buffer-string)))
       (pos 0)
       (case-fold-search nil) 
       (regexp "^| [[:alpha:]\|[:blank:]\|-]+|[[:blank:]]+\\([[:digit:]]+\\) |")
       (completion
        (let (x)
          (while (string-match regexp string pos)
            (push (match-string 0 string)  x)
            (setq pos (match-end 0)))x))
       (candidate (completing-read "Idéntifiant : " completion)))
        (string-match regexp candidate)
        (match-string 1 candidate)))

(defun sync0-doctorat-complete-person-name ()
  (interactive)
  (let* ((string  (with-temp-buffer
                    (insert-file-contents
                     "/home/sync0/Dropbox/org/data/20210503164252.org")
                    (buffer-string)))
         (pos 0)
         (case-fold-search nil) 
         (regexp "^| \\([[:alpha:]\|[:blank:]\|-]+\\)|[[:blank:]]+[[:digit:]]+ |")
         (completion
          (let (x)
            (while (string-match regexp string pos)
              (push (match-string 0 string)  x)
              (setq pos (match-end 0)))
            x)))
          (completing-read "Idéntifiant : " completion nil nil)))
       
  ;; (string-trim-whitespace (third (split-string candidate "| ")))


(defun sync0-doctorat-add-courcelle-reference ()
"Add one entry to my file for counting the references that Jean Gustave 
Courcelle Seneuil does to other authors"
(interactive)
  (let* ((key (sync0-doctorat-complete-biblatex-key))
          (id (sync0-doctorat-complete-person-id))
          (type (sync0-doctorat-complete-reference-types))
          (explicit (sync0-doctorat-complete-explicit))
          (pages (read-string "Pages (ex. : 90-180) : "))
          (language (sync0-doctorat-complete-languages))
(file "/home/sync0/Dropbox/org/data/20210503163456.org")
(entry (concat 
"| " key " "
"| " id " "
"| " type " "
"| " explicit " "
"| " pages " "
"| " language " "
"|  |\n")))
(append-to-file entry nil file)))

(defun sync0-doctorat-add-new-author ()
"Add one entry to my file for counting the references that Jean Gustave 
Courcelle Seneuil does to other authors"
(interactive)
(let* ((author (sync0-doctorat-complete-person-name))
       (string  (with-temp-buffer
                  (insert-file-contents
                   "/home/sync0/Dropbox/org/data/20210503164252.org")
                  (buffer-string)))
       (id
        (let ((x (substring-no-properties (number-to-string (random 100000000)) 0 4)))
        (while (string-match x string)
          (setq x (substring-no-properties (number-to-string (random 100000000)) 0 4)))
        x))
          (liaison (read-string "Liaison : "))
          (profession (read-string "Profession : "))
          (school (completing-read "Ecole : " sync0-doctorat-school-list))
          (nationality (completing-read "Nationalite : " sync0-doctorat-country-list))
(file "/home/sync0/Dropbox/org/data/20210503164252.org")
(entry (concat 
"| " author " "
"| " id " "
"| " liaison " "
"| " profession " "
"| " school " "
"| " nationality " |\n ")))
(append-to-file entry nil file)))

    (defhydra sync0-hydra-doctorat-functions (:color amaranth :hint nil :exit t)
      "
   ^Fonctions du doctorat^
   ^---------------
   new _a_uthor
   new _r_eference
                                                                     
   _q_uit
        "
      ("a" sync0-doctorat-add-new-author)
      ("r" sync0-doctorat-add-courcelle-reference)
      ("q" nil :color blue))

(evil-leader/set-key
  "D" 'sync0-hydra-doctorat-functions/body)


(provide 'sync0-yasnippet-doctorat)
