(use-package org-ref
  ;; :straight (org-ref :type git :host github :repo "jkitchin/org-ref") 
  :init
  (require 'sync0-bibtex-vars) 
  :custom
  (reftex-default-bibliography sync0-bibtex-bibliographies)
  (org-ref-default-bibliography sync0-bibtex-bibliographies)
  (org-ref-pdf-directory sync0-zettelkasten-attachments-directory)
  ;; (org-ref-completion-library 'org-ref-ivy-cite)
  ;; (org-ref-open-pdf-function 'sync0-org-ref-open-pdf-at-point)
  :config
  (require 'doi-utils))
  ;; (require 'org-ref-ivy)
  ;; (require 'sync0-org-ref-functions)

(use-package org-capture 
  :straight nil
  :after org 
  ;; :custom
  ;; (org-default-notes-file (concat sync0-zettelkasten-directory "scratch.org"))
  :config 
  (require 'org-ref)
  ;; (require 'sync0-org-capture-functions)

  ;; (evil-leader/set-key "c" 'org-capture)

  ;; (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (setq org-capture-templates 
        '(("a" "Annotation" entry
           (file buffer-file-name)
           (function sync0-org-capture-annotation-body)
           ;; (file+headline "~/Gdrive/org/todo/todo.org" "Autres")
           ;; (file "~/Gdrive/org/todo/todo.org")
           ;; "** %^{Titre}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :unnarrowed t)
          ;; ("j" "Journal" entry (function org-journal-find-location)
          ;;  "* %(format-time-string org-journal-time-format)\n\n%?"
          ;;  ;; "* %(format-time-string org-journal-time-format)\n\n%?"
          ;;  :jump-to-captured t :immediate-finish t)
          ("n" "Note permanente (capture rapide)" plain 
           (file sync0-org-capture-zettel-path)
           (function sync0-org-capture-permanent-body)
           :unnarrowed t)
          ("q" "Référence (capture rapide)" plain 
           (file sync0-org-capture-zettel-path)
           (function sync0-org-capture-quick-reference)
           :unnarrowed t)
          ("r" "Référence" plain 
           (file sync0-org-capture-zettel-path)
           (function sync0-org-capture-reference)
           :unnarrowed t)
          ("t" "Tâche" entry
           ;; (file+headline "~/Gdrive/org/todo/todo.org" "Autres")
           (file "~/Dropbox/org/todo/todo.org")
           "* 未 %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :immediate-finish t)
          ("w" "Référence web" plain 
           (file sync0-org-capture-zettel-path)
           (function sync0-org-capture-reference)
           :unnarrowed t)
          ("z" "Zettel (Tous les types)" plain 
           (file sync0-org-capture-zettel-path)
           (function sync0-org-capture-zettel-body)
           :unnarrowed t)
          ;;    ("c" "Correspondant (messages)" plain 
          ;; (file sync0-org-capture-message-name)
          ;;   "%(format \"#+TITLE: Messages pour %s\n#+CREATED: %s\n#+DATE: \n#+ROAM_TAGS: fiches %s\" sync0-zettel-title-upcase sync0-zettel-time-ordered sync0-zettel-title)\n\nOrigin: [[file:%(sync0-org-get-abbreviated-path (org-capture-get :original-file))][%(sync0-org-get-file-title-keyword (org-capture-get :original-file))]]\n\n"
          ;;   :unnarrowed t :jump-to-captured t)
          ("m" "Email" entry 
           (file+headline "~/Dropbox/org/todo/messages.org" "À répondre")
           ;; "** 無 %^{Description}\n%A\n%?\n"
           "** 未 %?\n%A\n" :jump-to-captured t :prepend t)))

  :bind 
  (("\C-c c" . org-capture)))

;;   (use-package org-bullets 
;;     :straight (org-bullets :type git :host github :repo "sabof/org-bullets") 
;;     :custom
;;     ;; Hide all bullets:
;;     (org-bullets-bullet-list '(" ")))

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
;;    org-noter-notes-search-path (list sync0-zettelkasten-directory)))

  (use-package ox-latex 
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
    ;; select tasks (i.e., TODOs) for export
    (org-export-with-tasks '("完" "未"))
    ;; (org-export-with-tasks '("來" "完" "未" "中" "待" "見"))
    (org-export-date-timestamp-format "%Y/%m/%d")
    ;; Export to Microsoft Word (doc).
    (org-export-odt-preferred-output-format "doc")
    (org-odt-preferred-output-format "doc")
    (org-latex-logfiles-extensions '("aux" "lof" "lot" "tex~" "idx" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "run.xml"))
    :config
    (require 'sync0-ox-latex)
    :bind 
    (:map org-mode-map 
          ("M-p" . sync0-org-export-latex-and-beamer)))

;; (use-package ox-epub
;;   :straight (ox-epub :type git :host github :repo "ofosos/ox-epub")
;;   :after org)

;; (use-package org-download
;;   :straight (org-download :type git :host github :repo "abo-abo/org-download") 
;;   :after org
;;   :hook (dired-mode . org-download-enable)
;;   :custom
;;   (org-download-image-dir "~/Pictures/org")
;;   (org-download-screenshot-method "spectacle")
;;   ;; (org-download-screenshot-method "xfce4-screenshooter")

;;   :config
;;   (defhydra sync0-hydra-org-download-functions (:color amaranth :hint nil :exit t)
;;     "
;;     ^Download functions^   
;;     ^--------------------
;;     _c_lipboard
;;     _y_ank
;;     _s_creenshot
                                                                     
;;     _q_uit
;;          "
;;     ("c" org-download-clipboard)
;;     ("y" org-download-yank)
;;     ("s" org-download-screenshot)
;;     ("q" nil :color blue))
  
;;   (evil-leader/set-key
;;     "D" 'sync0-hydra-org-download-functions/body))




(provide 'sync0-org-stuff)
