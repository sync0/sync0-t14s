(defun sync0-zettelkasten-set-property ()
  (interactive)
  (let  ((property
          (completing-read "What property to set?"
                           sync0-zettelkasten-all-properties-list)))
    (cond ((equal property "ROAM_ALIASES")
           (let* ((x (read-string "Define aliases (comma separated): "
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
             (org-set-property property x)))
          ((equal property "ZETTEL_TYPE")
           (let ((x (completing-read "Quel type de fiche ?"
                                     sync0-zettelkasten-zettel-types)))
             (org-set-property property x)))
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
          ((equal property "PARENT")
           (let* ((candidates (bibtex-completion-candidates))
                  (key (bibtex-completion-key-at-point))
                  (preselect (and key
                                  (cl-position-if (lambda (cand)
                                                    (member (cons "=key=" key)
                                                            (cdr cand)))
                                                  candidates)))
                  (selection (ivy-read "Parent's ref : "
                                       candidates
                                       :preselect preselect
                                       :caller 'ivy-bibtex
                                       :history 'ivy-bibtex-history))
                  (chosen-key (cdr (assoc "=key=" (cdr (assoc selection candidates)))))
                  (title (sync0-org-ref-get-citation-title chosen-key)))
             (org-set-property property (concat "\"" title "\""))))
          ((or (equal property "ANNOTATION_REFS")
               (equal property "CROSSREF"))
           (let* ((candidates (bibtex-completion-candidates))
                  (key (bibtex-completion-key-at-point))
                  (preselect (and key
                                  (cl-position-if (lambda (cand)
                                                    (member (cons "=key=" key)
                                                            (cdr cand)))
                                                  candidates)))
                  (selection (ivy-read "Crossref : "
                                       candidates
                                       :preselect preselect
                                       :caller 'ivy-bibtex
                                       :history 'ivy-bibtex-history))
                  (chosen-key (cdr (assoc "=key=" (cdr (assoc selection candidates))))))
             (org-set-property property (concat "cite:" chosen-key))))
          (t (let ((x (read-string "What to set this thing to? "
                                   nil nil nil t)))
               (org-set-property property x))))))

(defun sync0-zettelkasten-set-aliases ()
  (interactive)
  (org-with-point-at 1
    (let* ((x (read-string "Define aliases (comma separated): "
                           nil nil nil t))
           (alias-string  (if (string-match-p "," x)
                              (string-trim
                               (prin1-to-string
                                (split-string-and-unquote x ","))
                               "(" ")")
                            (concat "\"" x "\""))))
      (org-set-property "ROAM_ALIASES" alias-string))))

(defvar sync0-zettel-link-counter 0
  "The number of newly created zettels for this Emacs session.")

        ;;; Function to replace all org links with their description.
        ;;; Taken from https://dev.to/mostalive/how-to-replace-an-org-mode-link-by-its-description-c70
        ;;; This is useful when exporting my documents or
        ;;; when sending them somebody. 

(defun sync0-org-replace-link-by-description ()
  "Remove the link part of an org-mode link at point and keep
            only the description"
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                (insert (concat content " "))))))))

(defun sync0-org-replace-all-links-by-descriptions ()
  "Remove the link part of an org-mode link at point and keep
            only the description"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp  "\\[\\[file:[[:print:]]+\\.org.*\\]\\[\\([[:print:]]+\\)\\]\\]" "\\1")))
;; (replace-regexp  "\\[\\[.*\\]\\[\\(.*\\)\\]\\]" "\\2")))


;;   (setq org-roam-capture-templates '( 
;;    ("n" "Numéroté" plain (function org-roam--capture-get-point)
;;     "%?"
;;     :file-name "%<%Y%m%d%H%M%S>"
;;     :head "#+TITLE: ${slug}\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: permanent %<%Y>  %<%B>\n\nOrigin: %a\n"
;;     :unnarrowed t)))

;; (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "reference/%<%Y%m%d%H%M%S>"
;;            :head "#+TITLE: \n#+ROAM_KEY: ${ref}\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: websites %<%Y>\n\n"
;;            :unnarrowed t)))

;;   (setq org-roam-dailies-capture-templates
;;         '(("d" "default" entry
;;            #'org-roam-capture--get-point
;;            "* %?"
;;            :file-name "journal/%<%Y%m%d>"
;;            :head "#+TITLE: %<%A, %d %B %Y>\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: journal %<%Y> %<%B>\n\n")))

(require 'org-journal)
;;   (require 'org-roam-protocol)                

(defun sync0-org-roam-insert ()
  (interactive)
  (with-current-buffer
      (find-file-noselect
       (concat sync0-zettelkasten-directory 
               (format-time-string "chart/%Y%m.org")))
    (goto-char (point-min))
    (let* ((date (format-time-string "%Y/%m/%d"))
           (entry (concat "\n| " date " | 0 | 1 |"))
           (second-blank
            (concat "^| "
                    date
                    " |[[:blank:]]+[[:digit:]]+ |\\([[:blank:]]*\\)|$"))
           (first-blank
            (concat "^| "
                    date                    
                    " |[[:blank:]]+|[[:blank:]]+\\([[:digit:]]+\\) |$"))
           (both-there
            (concat "^| "
                    date 
                    " |[[:blank:]]+[[:digit:]]+ |[[:blank:]]+\\([[:digit:]]+\\) |$")))
      (cond ((or
              (re-search-forward first-blank nil t 1)
              (re-search-forward both-there nil t 1))
             (let* ((old-value (string-to-number
                                (match-string-no-properties 1)))
                    (new-value (number-to-string
                                (1+ old-value))))
               (replace-match new-value nil nil nil 1)))
            ((re-search-forward second-blank nil t 1)
             (replace-match " 1 " nil nil nil 1))
            (t (progn 
                 (goto-char (point-max))
                 (insert entry))))))
  (org-roam-node-insert))

(defhydra sync0-hydra-org-roam-insert (:color blue :hint nil)
"
^Org-roam^           ^Org-mode^            ^Orb & Org-ref^    ^Org-emms^        ^Etc^ 
^----------------------------------------------------------------------------------------------------
_I_nsert roam link   Insert lin_k_         Insert _c_itation  Link _t_rack      Replace _m_arks
Roam _b_uffer        Store _l_ink          Open _n_otes       Track _p_osition  
_U_pdate cache       Insert last _s_tored  Note _a_ctions
Plot _g_raph         New _f_ootnote        Notes _u_pdate
_S_et property       _Q_uote (display)     _E_xtract field
_D_elete link        _F_oreign quote       Bibtex _e_ntry
_R_emove all links   ^ ^                   PDF _o_pen
Open _d_eft          ^ ^                   PDF in _z_athura
_V_isit corr. PDF    ^ ^                   _C_opy pdf 
_M_ove headline      ^ ^                   _B_ib files
^ ^                  ^ ^                   Create _h_eadline
_q_uit
"
  ("a" orb-note-actions)
  ("B" sync0-visit-bibliography-in-buffer)
  ("b" org-roam-buffer-toggle)
  ("c" orb-insert)
  ("C" sync0-org-ref-copy-pdf-to-path)
  ("D" sync0-org-replace-link-by-description)
  ("d" deft)
  ("E" sync0-ivy-bibtex-extractor)
  ("e" org-ref-open-citation-at-point)
  ("F" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_foreign_displayquote"))))
  ("f" org-footnote-new)
  ("h" sync0-org-ref-create-headline)
  ("I" sync0-org-roam-insert)
  ("U" org-roam-db-sync)
  ("M" org-roam-refile)
  ("m" replace-smart-quotes)
  ("n" sync0-org-ref-open-notes)
  ("u" sync0-org-ref-update-notes-file)
  ("g" org-roam-graph)
  ("k" org-insert-link)
  ("l" org-store-link)
  ("o" sync0-org-ref-open-pdf-at-point)
  ("p" org-emms-insert-track-position)
  ("R" sync0-org-replace-all-links-by-descriptions)
  ("S" sync0-zettelkasten-set-property)
  ("s" org-insert-last-stored-link)
  ("t" org-emms-insert-track)
  ("Q" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_displayquote"))))
  ("V" sync0-org-open-corresponding-pdf)
  ("z" sync0-org-ref-open-pdf-at-point-zathura)
  ("q" nil :color blue))


(provide 'sync0-org-roam-functions)
