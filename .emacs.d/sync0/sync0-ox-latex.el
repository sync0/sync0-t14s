
;; To use KOMA-Script classes in LaTeX documents created through Org mode
;; export, it is necessary to explicitely add them to ~org-latex-classes~.
;; Moreover, this method can be used to create custom LaTeX classes.
(add-to-list 'org-latex-classes '("scrartcl"
                                  "\\documentclass{scrartcl}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrartcl-subsection"
                                  "\\documentclass{scrartcl}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrartcl-section"
                                  "\\documentclass{scrartcl}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrreprt"
                                  "\\documentclass{scrreprt}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrbook"
                                  "\\documentclass{scrbook}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\part{%s}" . "\\part*{%s}")
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrbook-chapter"
                                  "\\documentclass{scrbook}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrbook-section"
                                  "\\documentclass{scrbook}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrbook-subsection"
                                  "\\documentclass{scrbook}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("article"
                                  "\\documentclass{article}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("book"
                                  "\\documentclass{book}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("report"
                                  "\\documentclass{report}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("beamer"
                                  "\\documentclass{beamer}
                                         [NO-DEFAULT-PACKAGES]
   \\input{/home/sync0/Dropbox/typography/latex_preamble-beamer.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


(defun sync0-org-export-latex-and-beamer ()
  "Export current org file with beamer if it has beamer as latex class."
  (interactive)
  (cond ((equal major-mode 'org-mode) 
         (if (string-match "^\\#\\+SETUPFILE: .*beamer\\.org.*" (buffer-string))
             (progn
               (setq org-latex-pdf-process '("latexmk -xelatex -bibtex -output-directory=%o -f %f"))
               (org-beamer-export-to-pdf))
           (progn
             (setq org-latex-pdf-process '("latexmk -lualatex -bibtex -output-directory=%o -f %f"))
             (org-latex-export-to-pdf))))
        ((or (equal major-mode 'tex-mode) 
             (equal major-mode 'latex-mode)) 
         (tex-compile))
        (t  (message "Impossible de produire un pdf à partir de ce fichier"))))

;; export headlines to separate files
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

(defun sync0-org-export-headlines-to-latex ()
  "Export all subtrees that are *not* tagged with :noexport: to
 separate files.

 Subtrees that do not have the :EXPORT_FILE_NAME: property set
 are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-latex-export-to-latex nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

;; ;; Set tags to excluce from export. 
(add-to-list 'org-export-exclude-tags "消")
(add-to-list 'org-export-exclude-tags "noexport")

(evil-leader/set-key
  "X" 'sync0-org-export-latex-and-beamer)

(provide 'sync0-ox-latex)
