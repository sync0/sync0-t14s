(use-package org-id
  :after org
  :straight nil
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-track-globally t)
;;   :init
;;   (require 'find-lisp)
  :config
  ;; Update ID file on startup
  (org-id-update-id-locations))

(use-package org-ref
  :after org
  :init
  (require 'sync0-bibtex-vars) 
  :custom
  (reftex-default-bibliography (list sync0-bibtex-master-bibliography))
  (org-ref-default-bibliography (list sync0-bibtex-master-bibliography))
  (org-ref-pdf-directory sync0-zkn-attachments-dir)
  ;; (org-ref-completion-library 'org-ref-ivy-cite)
  ;; (org-ref-open-pdf-function 'sync0-org-ref-open-pdf-at-point)
  :config
  ;; (require 'sync0-org-ref-functions)
  (require 'doi-utils))

(use-package org-bullets
  :after org
  :custom
  ;; Hide all bullets by setting the bullet list to a space
  (org-bullets-bullet-list '(" "))
  :hook
  (org-mode . (lambda () 
                (org-bullets-mode 1))))

(use-package org-capture 
  :straight nil
  :after org 
  :commands org-capture
  :custom
  (org-default-notes-file (concat sync0-zkn-dir "org/itineris.org"))
  :config 
  (require 'org-ref)
  (require 'sync0-org-capture-functions)

  ;; (add-hook 'org-capture-mode-hook 'evil-insert-state)

;;   (setq org-capture-templates 
;;         '(("a" "Annotation" entry
;;            (file buffer-file-name)
;;            (function sync0-org-capture-annotation-body)
;;            ;; (file+headline "~/Gdrive/org/todo/todo.org" "Autres")
;;            ;; (file "~/Gdrive/org/todo/todo.org")
;;            ;; "** %^{Titre}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
;;            :unnarrowed t)
;;           ;; ("j" "Journal" entry (function org-journal-find-location)
;;           ;;  "* %(format-time-string org-journal-time-format)\n\n%?"
;;           ;;  ;; "* %(format-time-string org-journal-time-format)\n\n%?"
;;           ;;  :jump-to-captured t :immediate-finish t)
;; ;;           ("n" "Note permanente (capture rapide)" plain 
;; ;;            (file sync0-org-capture-zettel-path)
;; ;;            (function sync0-org-capture-permanent-body)
;; ;;            :unnarrowed t)
;; ;;           ("q" "Référence (capture rapide)" plain 
;; ;;            (file sync0-org-capture-zettel-path)
;; ;;            (function sync0-org-capture-quick-reference)
;; ;;            :unnarrowed t)
;; ;;           ("r" "Référence" plain 
;; ;;            (file sync0-org-capture-zettel-path)
;; ;;            (function sync0-org-capture-reference)
;; ;;            :unnarrowed t)
;;           ("j" "Journal" entry 
;; 	  (function (lambda () (list 'file+headline "~/Gdrive/obsidian/org/itineris.org" (format-time-string "%Y-%m, %B"))))
;;            ;; (file+headline "~/Gdrive/obsidian/org/itineris.org" (format-time-string "%Y-%m, %B"))
;;            "** %(format-time-string \"%Y-%m-%d, %A\")\n\n%?"
;;            ;; "* %(format-time-string org-journal-time-format)\n\n%?"
;;            :immediate-finish t)
;;           ("t" "Tâche" entry
;;            (file+headline "~/Gdrive/obsidian/org/agenda.org" "Unclassed")
;;            ;; (file "~/Gdrive/org/todo.org")
;;            "** 未 %^{Task}\n"
;;            :immediate-finish t)
;; ;;           ("w" "Référence web" plain 
;; ;;            (file sync0-org-capture-zettel-path)
;; ;;            (function sync0-org-capture-reference)
;; ;;            :unnarrowed t)
;;           ("z" "Zettel (Tous les types)" plain 
;;            (file sync0-org-capture-zettel-path)
;;            (function sync0-org-capture-zettel-body)
;;            :unnarrowed t)))
  (setq org-capture-templates 
        `(("a" "Annotation" entry
           (file buffer-file-name)
           (function sync0-org-capture-annotation-body)
           :unnarrowed t)
          ("j" "Journal" entry 
	  (file+headline "~/Gdrive/obsidian/itineris/itineris.org" ,(format-time-string "%Y-%m, %B"))
           "** %<%Y-%m-%d, %A>\n\n%?")
          ("t" "Tâche" entry
           (file+headline "~/Gdrive/obsidian/tasks/tasks.org" "Unclassed")
           "** 未 %^{Task}\n"
           :immediate-finish t)
          ("z" "Zettel (Tous les types)" plain 
           (file sync0-org-capture-zettel-path)
           (function sync0-org-capture-zettel-body)
           :unnarrowed t)))
	  )

;; (use-package org-noter
;;   :straight (org-noter :type git :host github :repo "weirdNox/org-noter") 
;;   :after (:any org pdf-view)
;;   :config
;;   (setq
;;    ;; The WM can handle splits
;;    org-noter-notes-window-location 'horizontal-split
;;    ;; Please stop opening frames
;;    org-noter-always-create-frame nil
;;    ;; I want to see the whole file
;;    org-noter-hide-other nil
;;    ;; Use interleave properties 
;;    org-noter-property-doc-file "INTERLEAVE_PDF"
;;    ;; 
;;    org-noter-default-heading-title (format-time-string "%Y%m%d%H%M%S")
;;    ;; Everything is relative to the main notes file
;;    org-noter-notes-search-path (list sync0-zkn-dir)))

  (use-package ox
    :straight nil
    :after org
    :custom
    ;; Set latex compiler for org export. 
    (org-latex-compiler "lualatex")
    ;; Set latex bibtex compiler for org export. 
    (org-latex-bibtex-compiler "lualatex")
    ;; Export references (to tables, graphics, etc.) properly, evaluating the +NAME property. 
    (org-latex-prefer-user-labels t)
    ;; (org-latex-pdf-process (list "latexmk -lualatex -bibtex -f %f"))
    ;; export process is sent to the background
    (org-latex-listings 'minted)
    ;; set word wrap for code blocks
    (org-latex-minted-options '(("breaklines" "true")
                                ("breakanywhere" "true")))
    ;;  (org-latex-pdf-process (list "latexmk -lualatex -bibtex-cond -f %f")
    ;; (org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))
    (org-export-in-background t)
    (org-export-with-broken-links t)
    ;; select tasks (i.e., TODOs) for export
    (org-export-with-tasks t)
    ;; (org-export-with-tasks '("完" "未"))
    ;; (org-export-with-tasks '("來" "完" "未" "中" "待" "見"))
    (org-export-date-timestamp-format "%Y-%m-%d")
    ;; Export to Microsoft Word (doc).
    (org-export-odt-preferred-output-format "doc")
    (org-odt-preferred-output-format "doc")
    (org-latex-logfiles-extensions '("aux" "lof" "lot" "tex~" "idx" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "run.xml"))
    :config
    (require 'sync0-ox-latex)
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))
    :bind 
    (:map org-mode-map 
          ("M-p" . sync0-org-export-latex-and-beamer)))

;; (use-package ox-epub
;;   :straight (ox-epub :type git :host github :repo "ofosos/ox-epub")
;;   :after org)

(use-package org-download
  :after org
  :commands (org-download-clipboard org-download-screenshot org-download-yank)
  :custom
  (org-download-image-dir "~/Pictures/org")
  (org-download-screenshot-method "spectacle"))
  ;; (org-download-screenshot-method "xfce4-screenshooter")

(use-package org-pomodoro
  :after org
  :commands org-pomodoro)

;; (use-package org-tempo
;;   :straight nil
;;   :after org)

(use-package org-num
  :after org
  :straight nil
  :custom
  (org-num-skip-tags '("unnumbered" "noexport"))
  (org-num-skip-footnotes t)
  (org-num-skip-commented t))

(use-package oc
  :after org
  :straight nil
  ;; :ensure org-plus-contrib
  :custom
  (org-cite-csl-styles-dir "/home/sync0/Gdrive/typography/csl/")  ;; Set path for CSL styles, if needed
  (org-cite-bibliography-files '("/home/sync0/Gdrive/bibliographies/master.bib"))  ;; Set bibliography files
  (org-cite-global-bibliography '("/home/sync0/Gdrive/bibliographies/master.bib")))  ;; BibTeX file
;; (setq org-cite-bibliography '("~/path/to/another.bib"))      ;; More than one file is allowed
;; (setq org-cite-export-csl-style "apa")  ;; Or any other CSL style
;; (setq org-cite-global-bibliography '("~/path/to/your.bib"))


(provide 'sync0-org-stuff)
