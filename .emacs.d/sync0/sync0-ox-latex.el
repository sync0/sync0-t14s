
;; To use KOMA-Script classes in LaTeX documents created through Org mode
;; export, it is necessary to explicitely add them to ~org-latex-classes~.
;; Moreover, this method can be used to create custom LaTeX classes.
(add-to-list 'org-latex-classes '("scrartcl"
                                  "\\documentclass{scrartcl}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrartcl-subsection"
                                  "\\documentclass{scrartcl}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrartcl-section"
                                  "\\documentclass{scrartcl}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrreprt"
                                  "\\documentclass{scrreprt}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
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
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
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
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
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
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("scrbook-subsection"
                                  "\\documentclass{scrbook}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\chapter{%s}" . "\\chapter*{%s}")
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("article"
                                  "\\documentclass{article}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("book"
                                  "\\documentclass{book}
                                         [NO-DEFAULT-PACKAGES]
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
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
\\input{/home/sync0/Gdrive/typography/latex_preamble.tex}
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
\\input{/home/sync0/Gdrive/typography/latex_preamble-beamer.tex}
                                         [EXTRA]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))


(defun sync0-org-export-headline-replace (backend)
  "Remove all headlines in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  (org-map-entries
   (lambda ()
     (when-let ((new-title
                 (org-element-property :EXPORT_TITLE (org-element-at-point))))
       (org-edit-headline new-title))))) 

;;      (delete-region (point) (line-beginning-position 2)))))

;; (org-element-map (org-element-parse-buffer)
;;                        'headline
;;                      (lambda (hl)
;;                        (when (org-element-property :EXPORT_TITLE hl)
;;                          (goto-char (org-element-property :begin hl))
;;                          (org-edit-headline (org-element-property :EXPORT_TITLE hl)))))

(add-hook 'org-export-before-parsing-hook 'sync0-org-export-headline-replace)

;; Function to add \newpage before headlines
(defun sync0-zettel-custom-latex-headline (headline contents info)
  "Export HEADLINE to LaTeX with a page break before and after."
  (concat "\\newpage\n"  ; Page break before the headline
          (org-latex-headline headline (concat "\n\\newpage\n" contents) info)))  ; Page break after the headline

;; (defun sync0-zettel-custom-latex-headline (headline contents info)
;;   "Export HEADLINE to LaTeX with a page break before and after."
;;   (concat "\\newpage\n"  ; Page break before the headline
;;           "\\noindent\n"  ; Prevent indent if necessary
;;           (org-latex-headline headline contents info)
;;           "\\newpage\n"))  ; Page break after the headline

;; ;; Function to add \newpage after each paragraph
;; (defun sync0-zettel-custom-latex-paragraph (paragraph contents info)
;;   "Export PARAGRAPH to LaTeX with a page break."
;;   (concat paragraph "\n\\newpage\n"))

(defun sync0-zettel-custom-latex-paragraph (paragraph contents info)
  "Export PARAGRAPH to LaTeX with a page break."
  (concat contents "\n\\newpage\n"))

;; Define a custom export backend
(org-export-define-derived-backend 'zettel-latex 'latex
  :translate-alist '((headline . sync0-zettel-custom-latex-headline)
                     (paragraph . sync0-zettel-custom-latex-paragraph)))

;; Use the custom backend for exporting
;; (defun sync0-org-export-to-zettel-latex ()
;;   "Export the current Org buffer to LaTeX with custom formatting."
;;   (interactive)
;;   (org-export-to-file 'zettel-latex "output.tex"))

;; (defun sync0-org-export-to-zettel-latex ()
;;   "Export the current Org buffer to PDF with custom formatting."
;;   (interactive)
;;   (let* ((org-file-name (file-name-base (buffer-file-name)))
;;          (output-pdf (concat org-file-name ".pdf")))
;;     (org-export-to-file 'zettel-latex "output.tex"
;;       nil nil nil nil (lambda (_) (org-latex-compile "output.tex")))))

;; (defun sync0-org-export-to-zettel-latex ()
;;   "Export the current Org buffer to LaTeX and compile to PDF using LuaLaTeX."
;;   (interactive)
;;   (let* ((org-file-name (file-name-base (buffer-file-name)))
;;          (output-tex (concat org-file-name ".tex"))
;;          (output-pdf (concat org-file-name ".pdf")))
;;     ;; Set the LaTeX export process to use lualatex
;;     (setq org-latex-pdf-process
;;           (list "latexmk -lualatex -bibtex -output-directory=%o -f %f"))
;;     ;; Export to LaTeX
;;     (org-export-to-file 'zettel-latex output-tex
;;       nil nil nil nil
;;       (lambda (output-tex)
;; 	(shell-command (concat "latexmk -lualatex -bibtex -output-directory=" 
;;                                (file-name-directory output-tex) " -f " output-tex))))))

(defun sync0-org-export-to-zettel-latex (&optional async subtreep visible-only body-only ext-plist)
  "Export the current Org buffer to LaTeX and compile to PDF using LuaLaTeX."
  (interactive)
  ;; Set the LaTeX export process to use lualatex
  (setq org-latex-pdf-process
        (list "latexmk -lualatex -bibtex -output-directory=%o -f %f"))
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'zettel-latex file
      async subtreep visible-only body-only ext-plist
      #'org-latex-compile)))

;; (defun sync0-org-export-to-zettel-latex ()
;;   "Export the current Org buffer to LaTeX and compile to PDF using LuaLaTeX."
;;   (interactive)
;;   (let* ((org-file-name (file-name-base (buffer-file-name)))
;;          (output-tex (concat org-file-name ".tex"))
;;          (output-pdf (concat org-file-name ".pdf"))
;;          (output-dir (file-name-directory output-tex)))
;;     ;; Export to LaTeX
;;     (org-export-to-file 'zettel-latex output-tex
;;       nil nil nil nil
;;       (lambda (_)
;;         ;; After export, run latexmk to compile the .tex file to .pdf
;;         (shell-command (concat "latexmk -lualatex -bibtex -output-directory="
;;                                (shell-quote-argument output-dir) " -f "
;;                                (shell-quote-argument output-tex)))))))

      ;; (lambda (output-tex)
      ;;   ;; After export, compile the .tex file to .pdf synchronously
      ;;   (call-process "latexmk" nil nil nil "-lualatex" "-bibtex" "-output-directory=" output-dir "-f " output-tex)))))

(defun sync0-org-export-latex-and-beamer ()
  "Export current org file with beamer, custom LaTeX backend, or default LaTeX based on context."
  (interactive)
  (cond
   ;; For Org mode files
   ((derived-mode-p 'org-mode)
    (let ((buffer-string (buffer-string)))
      (cond
       ;; Check for Beamer setup
       ((string-match "^\\#\\+SETUPFILE: .*beamer\\.org.*" buffer-string)
        (setq org-latex-pdf-process '("latexmk -xelatex -bibtex -output-directory=%o -f %f"))
        (org-beamer-export-to-pdf))
       ;; Check for custom LaTeX setup
       ((string-match "^\\#\\+SETUPFILE: ~/Gdrive/typography/settings_zettel.org" buffer-string)
        (sync0-org-export-to-zettel-latex))
       ;; Check for custom LaTeX setup
       ((string-match "^\\#\\+SETUPFILE: ~/Gdrive/typography/settings_zettel_a7.org" buffer-string)
        (sync0-org-export-to-zettel-latex))
       ;; Check for custom LaTeX setup
       ((string-match "^\\#\\+SETUPFILE: ~/Gdrive/typography/settings_zettel_card.org" buffer-string)
        (sync0-org-export-to-zettel-latex))
       ;; Default LaTeX export
       (t
        (setq org-latex-pdf-process '("latexmk -lualatex -bibtex -output-directory=%o -f %f"))
        (org-latex-export-to-pdf)))))
   ;; For TeX or LaTeX files
   ((derived-mode-p 'tex-mode 'latex-mode)
    (tex-compile))
   ;; Otherwise
   (t (message "Impossible de produire un pdf à partir de ce fichier"))))

;; (defun sync0-org-export-latex-and-beamer ()
;;   "Export current org file with beamer if it has beamer as latex class."
;;   (interactive)
;;   (cond ((derived-mode-p 'org-mode) 
;;            (if (string-match "^\\#\\+SETUPFILE: .*beamer\\.org.*" (buffer-string))
;;                (progn
;;                  (setq org-latex-pdf-process '("latexmk -xelatex -bibtex -output-directory=%o -f %f"))
;;                  (org-beamer-export-to-pdf))
;;              (progn
;;                (setq org-latex-pdf-process '("latexmk -lualatex -bibtex -output-directory=%o -f %f"))
;;                (org-latex-export-to-pdf))))
;;          ((or (derived-mode-p 'tex-mode) 
;;               (derived-mode-p 'latex-mode)) 
;;           (tex-compile))
;;          (t  (message "Impossible de produire un pdf à partir de ce fichier"))))

;; (defun sync0-org-export-latex-and-beamer ()
;;   "Export current org file with beamer if it has beamer as latex class."
;;   (interactive)
;;   (cond ((equal major-mode 'org-mode) 
;;          ;; Replace headlines by EXPORT_T
;;          ;; https://emacs.stackexchange.com/questions/44497/org-latex-export-name-sections-according-to-custom-id-rather-than-headline-s
;;          (let ((org-export-before-parsing-hook
;;                 '(lambda (_)
;;                    (org-element-map (org-element-parse-buffer)
;;                        'headline
;;                      (lambda (hl)
;;                        (when (org-element-property :EXPORT_TITLE hl)
;;                          (goto-char (org-element-property :begin hl))
;;                          (org-edit-headline (org-element-property :EXPORT_TITLE hl))))))))
;;            (if (string-match "^\\#\\+SETUPFILE: .*beamer\\.org.*" (buffer-string))
;;                (progn
;;                  (setq org-latex-pdf-process '("latexmk -xelatex -bibtex -output-directory=%o -f %f"))
;;                  (org-beamer-export-to-pdf))
;;              (progn
;;                (setq org-latex-pdf-process '("latexmk -lualatex -bibtex -output-directory=%o -f %f"))
;;                (org-latex-export-to-pdf)))))
;;          ((or (equal major-mode 'tex-mode) 
;;               (equal major-mode 'latex-mode)) 
;;           (tex-compile))
;;          (t  (message "Impossible de produire un pdf à partir de ce fichier"))))

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

(provide 'sync0-ox-latex)
