(defun sync0-bibtex-create-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S")))
    (insert new-key)))

(defun sync0-bibtex-next-key ()
  "Print the bibtex key of the document"
  (interactive)
  (let ((bibtex-key (re-search-forward "@.+{" nil nil 1)))
    (goto-char bibtex-key)))

(defun sync0-bibtex-previous-key ()
  "Print the bibtex key of the document"
  (interactive)
  (let ((bibtex-key (re-search-backward "@.+{" nil nil 2)))
    (goto-char bibtex-key)
    (re-search-forward "@.+{" nil nil 1)))

(defun sync0-bibtex-update-key ()
  "Change bibtex key at point with a key using the format provided
by org-roam files"
  (interactive)
  (let* ((new-key (format-time-string "%Y%m%d%H%M%S"))
         (directory "/home/sync0/Documents/pdfs/")
         (new-path (concat directory new-key ".pdf"))
         (pdf-path
          (save-excursion
            (when  (re-search-forward "file = {\\(.+\\)}," nil t 1)
              (match-string-no-properties 1)))))
    (when-let ((type
                (when (re-search-forward "\\(^@[[:lower:]]+{\\)[[:digit:]]+," nil t 1)
                  (match-string-no-properties 1))))
      (kill-whole-line 1)
      (insert (concat type new-key ",\n")))
    (when (re-search-forward "file = {" nil t 1)
      (kill-whole-line 1)
      (insert (concat "file = {" new-path "},\n")))))

;; (when (file-exists-p pdf-path)
;;   (rename-file pdf-path new-path)))))

(defhydra sync0-hydra-bibtex-functions (:color amaranth :hint nil :exit t)
  "
     ^Refs^              ^PDFs^             ^Notes^              
     ^-----------------------------------------------------
     Key _u_pdate        Pdf _o_pen         Open _n_otes
     ^ ^                 Open in _z_athura
     ^ ^                 Copy to _p_ath  
     ^----------------------------------------------------
     ^Bibliographies^ 
     ^---------------------------------------------------
     Bibfile _v_isit 
                                                                     
     _q_uit
          "

  ("u" sync0-bibtex-update-key)
  ("p" sync0-org-ref-copy-pdf-to-path)
  ("n" sync0-org-ref-open-notes)
  ("v" sync0-visit-bibliography-in-buffer)
  ("o" sync0-org-ref-open-pdf-at-point)
  ("z" sync0-org-ref-open-pdf-at-point-zathura)
  ("q" nil :color blue))

;; (evil-leader/set-key
;;   "O" 'org-open-at-point
;;   "#" 'sync0-org-open-other-frame)
;; "O" 'sync0-overview-tree-window
;; "o" 'sync0-overview-jump-to-overview
;; "I" 'org-insert-link
;; "z" 'sync0-org-tree-to-indirect-buffer
;; "z" 'sync0-hydra-org-functions/body

(evil-leader/set-key-for-mode 'bibtex-mode "z" 'sync0-hydra-bibtex-functions/body)


(provide 'sync0-bibtex-functions)
