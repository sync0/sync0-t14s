(use-package general
  :after org evil 
  :config
  ;; global general
  (general-define-key
   "M-H" 'next-buffer
   "M-L" 'previous-buffer
   "<C-tab>" 'bury-buffer
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)
  ;; Cycle through buffers
  ;;   ;; evil-mode bindings
  (general-define-key
   :keymaps 'evil-normal-state-map
   ;; "C-m" nil
   ;; "M-." nil
   "<remap> <evil-next-line>" 'evil-next-visual-line
   "<remap> <evil-previous-line>" 'evil-previous-visual-line)
  (general-define-key
   :keymaps 'evil-motion-state-map
   ;; "<SPC>" nil ; Uncomment if you want to unbind space
   ;; "C-d" nil
   ;; "C-e" nil
   "<remap> <evil-next-line>" 'evil-next-visual-line
   "<remap> <evil-previous-line>" 'evil-previous-visual-line)
  (general-define-key
   :states 'normal
   :keymaps 'global-map
   "zw" 'transpose-words
   "zl" 'transpose-lines
   "zp" 'transpose-paragraphs
   "zs" 'transpose-sentences
   "zk" 'move-text-up
   "zj" 'move-text-down
   ;; "gb" 'consult-bookmark
   "U" 'undo
   ;; "s" 'fill-paragraph
   "S" 'sync0-insert-line-below
   "M" 'bookmark-set)
  ;; ;; evil-escape
  (general-define-key
   :keymaps 'evil-insert-state-map
   "C-g" 'evil-escape)
  (general-define-key
   :keymaps 'evil-replace-state-map
   "C-g" 'evil-escape)
  (general-define-key
   :keymaps 'evil-visual-state-map
   "C-g" 'evil-escape)
  (general-define-key
   :keymaps 'evil-operator-state-map
   "C-g" 'evil-escape)
  (with-eval-after-load 'sync0-language
    (general-define-key
    "M-i" 'sync0-ispell-word-then-abbrev))
  (with-eval-after-load 'vertico
    (general-define-key
     :keymaps 'vertico-map
     ;; "<tab>" 'vertico-insert
     "<escape>" 'minibuffer-keyboard-quit
     "?" 'minibuffer-completion-help
     "C-M-n" 'vertico-next-group
     "C-M-p" 'vertico-previous-group
     ;; Multiform toggles
     "<backspace>" 'vertico-directory-delete-char
     "C-w" 'vertico-directory-delete-word
     "C-<backspace>" 'vertico-directory-delete-word
     "RET" 'vertico-directory-enter
     "C-i" 'vertico-quick-insert
     "C-o" 'vertico-quick-exit
     "M-o" 'kb/vertico-quick-embark
     "M-G" 'vertico-multiform-grid
     "M-F" 'vertico-multiform-flat
     "M-R" 'vertico-multiform-reverse
     "M-U" 'vertico-multiform-unobtrusive
     "C-l" 'kb/vertico-multiform-flat-toggle))
  (with-eval-after-load 'embark
    (general-define-key
     "C-."  'embark-act         ;; pick some comfortable binding
     "C-;"  'embark-dwim        ;; good alternative: M-.
     "C-h B"  'embark-bindings)) ;; alternative for `describe-bindings'
  (with-eval-after-load 'consult
    (general-define-key
     ;; Global bindings
     "C-c M-x" 'consult-mode-command
;;      "C-c h" 'consult-history
     "C-c k" 'consult-kmacro
     "C-c m" 'consult-man
;;      "C-c i" 'consult-info
     "C-x M-:" 'consult-complex-command ;; orig. repeat-complex-command
     "C-x b" 'consult-buffer ;; orig. switch-to-buffer
     "C-x 4 b" 'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
     "C-x 5 b" 'consult-buffer-other-frame ;; orig. switch-to-buffer-other-frame
     "C-x t b" 'consult-buffer-other-tab ;; orig. switch-to-buffer-other-tab
     "C-x r b" 'consult-bookmark ;; orig. bookmark-jump
     "C-x p b" 'consult-project-buffer ;; orig. project-switch-to-buffer
     ;; "M-#" 'consult-register-load
     ;; "M-'" 'consult-register-store ;; orig. abbrev-prefix-mark (unrelated)
     ;; "C-M-#" 'consult-register
     "M-y" 'consult-yank-pop ;; orig. yank-pop
     ;; M-g bindings in `goto-map'
     "M-g e" 'consult-compile-error
     "M-g f" 'consult-flymake ;; Alternative: consult-flycheck
     "M-g g" 'consult-goto-line ;; orig. goto-line
     "M-g M-g" 'consult-goto-line ;; orig. goto-line
     "M-g o" 'consult-outline ;; Alternative: consult-org-heading
     "M-g m" 'consult-mark
     "M-g k" 'consult-global-mark
     "M-g i" 'consult-imenu
     "M-g I" 'consult-imenu-multi
     ;; M-s bindings in `search-map'
     "M-s d" 'consult-find ;; Alternative: consult-fd
     "M-s c" 'consult-locate
     "M-s g" 'consult-grep
     "M-s G" 'consult-git-grep
     "M-s r" 'consult-ripgrep
     "M-s l" 'consult-line
     "M-s L" 'consult-line-multi
     "M-s k" 'consult-keep-lines
     "M-s u" 'consult-focus-lines
     ;; Isearch integration
     "M-s e" 'consult-isearch-history))
     ;; :keymaps 'isearch-mode-map
     ;; "M-e" 'consult-isearch-history ;; orig. isearch-edit-string
     ;; "M-s e" 'consult-isearch-history ;; orig. isearch-edit-string
     ;; "M-s l" 'consult-line ;; needed by consult-line to detect isearch
     ;; "M-s L" 'consult-line-multi ;; needed by consult-line to detect isearch
  (with-eval-after-load 'company
    (general-define-key
     :keymaps 'company-active-map
     "M-j" 'company-select-next
     "M-k" 'company-select-previous))
  ;; org-mode bindings
  (general-define-key
   :keymaps 'org-mode-map
   "C-c ]" nil
   "C-l" 'org-demote-subtree
;;    "C-h" 'sync0-org-up-back-and-preview
   ;;    "C-j" 'sync0-org-forward-and-preview
   ;;    "C-k" 'sync0-org-back-and-preview
   "C-j" 'org-metadown          
   "C-k" 'org-metaup
   "M-<return>" 'sync0-org-meta-return-dwim
   "M-S-<return>" 'sync0-org-insert-todo-heading-dwim
;;    "C-l" 'sync0-org-up-forward-and-preview
   "C-h" 'org-promote-subtree)
  ;; evil bindings for org-mode
  (general-define-key
   :states 'normal              ; Apply to Evil's normal state
   :keymaps 'org-mode-map       ; Apply to org-mode
   "<" 'outline-previous-visible-heading
   ">" 'outline-next-visible-heading
   "C->" 'org-forward-heading-same-level
   "C-<" 'org-backward-heading-same-level
   "<S-tab>" 'sync0-org-tree-open-in-right-frame
   "H" 'org-metaleft
   "L" 'org-metaright
   ;; "K" 'org-metaup
   ;; "J" 'org-metadown          ; Uncomment to enable this binding
   "k" 'previous-line
   "j" 'next-line
   "o" '(lambda () (interactive) (sync0-evil-org-eol-call 'sync0-clever-insert-item))
   "O" '(lambda () (interactive) (sync0-evil-org-eol-call 'org-insert-heading))
   "$" 'org-end-of-line
   "^" 'org-beginning-of-line
   "[" 'backward-sentence
   "]" 'forward-sentence
   "{" 'org-backward-paragraph
   "}" 'org-forward-paragraph
   "-" 'org-cycle-list-bullet
   "<tab>" 'org-cycle)
  (general-define-key
   :states 'visual              ; Apply to Evil's visual state
   :keymaps 'org-mode-map       ; Apply to org-mode
   "z" 'org-emphasize)
  (with-eval-after-load 'pdf-tools
    (general-define-key
     :keymaps 'pdf-view-mode-map
     "C-s" 'isearch-forward
     "j" 'pdf-view-next-line-or-next-page
     "J" 'pdf-view-scroll-up-or-next-page
     "k" 'pdf-view-previous-line-or-previous-page
     "K" 'pdf-view-scroll-down-or-previous-page
     "y" 'pdf-view-kill-ring-save
     "+" 'pdf-view-enlarge
     "=" 'pdf-view-enlarge
     "-" 'pdf-view-shrink
     "/" 'isearch-forward
     "?" 'isearch-backward
     "n" 'isearch-repeat-forward
     "N" 'isearch-repeat-backward
     "0" 'pdf-view-scale-reset
     "H" 'pdf-annot-add-highlight-markup-annotation
     "l" 'image-forward-hscroll
     "h" 'image-backward-hscroll
     "t" 'pdf-annot-add-text-annotation
     "g" 'pdf-view-goto-page
     "G" 'pdf-view-last-page
     "D" 'pdf-view-dark-minor-mode
     "d" 'pdf-annot-delete))
  (with-eval-after-load 'pdf-outline
    (general-define-key
     :keymaps 'pdf-outline-buffer-mode-map
     "j" 'next-line
     "k"  'previous-line))
  (with-eval-after-load 'recentf
    (general-define-key
     :keymaps 'recentf-dialog-mode-map
     "j" 'next-line
     "k"  'previous-line))
  (with-eval-after-load 'major-mode-hydra
    (general-define-key
     "M-SPC"  'major-mode-hydra))
  (with-eval-after-load 'bibtex
    (general-define-key
     :states 'normal              ; Apply to Evil's normal state
     :keymaps 'bibtex-mode-map       ; Apply to org-mode
     "K" 'sync0-bibtex-previous-key
     "zf" 'bibtex-fill-entry
     "J" 'sync0-bibtex-next-key))
  (with-eval-after-load 'markdown-mode
    (general-define-key
     :keymaps 'markdown-mode-map       ; Apply to org-mode
     "M-<right>" 'markdown-demote
     "M-<left>"  'markdown-promote
     "C-S-h" (lambda () (interactive) (sync0-markdown-adjust-heading-level -1)) ; Downgrade
     "C-S-l" (lambda () (interactive) (sync0-markdown-adjust-heading-level 1)) ; Upgrade
     "C-<" 'markdown-backward-same-level ; Move to previous heading at the same level
     "C->" 'markdown-forward-same-level ; Move to next heading at the same level
     "C-k" 'markdown-move-up             ; Move current heading up
     "C-j" 'markdown-move-down           ; Move current heading down
     "C-h" 'markdown-promote             ; Promote heading (reduce # symbols)
     "C-l" 'markdown-demote             ; Demote heading (add # symbols)
     ))
  (with-eval-after-load 'markdown-mode
    (general-define-key
     :states 'visual              ; Apply to Evil's normal state
     :keymaps 'markdown-mode-map       ; Apply to org-mode
     "z" 'markdown-insert-italic))
  (with-eval-after-load 'markdown-mode
    (general-define-key
     :states 'normal              ; Apply to Evil's normal state
     :keymaps 'markdown-mode-map       ; Apply to org-mode
     "TAB" 'markdown-cycle
     "<" 'sync0-markdown-previous-heading
     ">" 'sync0-markdown-next-heading
     "H" 'markdown-promote
     "L" 'markdown-demote
     "k" 'evil-previous-visual-line
     "j" 'evil-next-visual-line
     "$" 'evil-end-of-visual-line
     "^" 'evil-beginning-of-visual-line
     "[" 'evil-backward-sentence-begin
     "]" 'evil-forward-sentence-begin
     "{" 'markdown-backward-paragraph
     "}" 'markdown-forward-paragraph))
  (with-eval-after-load 'org-roam
    (general-define-key
     "C-c c" 'org-roam-capture
     "C-c I" 'sync0-org-roam-node-insert-filtered))
  (with-eval-after-load 'org-roam
    (general-define-key
     :keymaps 'org-mode-map
     "C-c g" 'sync0-org-roam-find-linked-file
     "C-c p" 'sync0-org-roam-set-directional-link))
  (with-eval-after-load 'smooth-scrolling
    (general-define-key
     "M-k" 'sync0-scroll-up
     "M-h"  'sync0-scroll-right
     "M-l"  'sync0-scroll-left
     "M-j"  'sync0-scroll-down))
  ;; (with-eval-after-load 'markdown-mode
  ;;   (general-define-key
  ;;    :states 'normal              ; Apply to Evil's normal state
  ;;    :keymaps 'bibtex-mode-map       ; Apply to org-mode
  ;;     "K" 'sync0-bibtex-previous-key
  ;;     "zf" 'bibtex-fill-entry
  ;;     "J" 'sync0-bibtex-next-key))
  ;; ;; vertico keybings
  ;; ;(general-define-key
  ;; ; :states '(normal visual insert motion)
  ;; ; :keymaps 'global-map
  ;; ; "M-." 'vertico-repeat)
  (with-eval-after-load 'sync0-ivy-bibtex-functions
    (general-define-key
     :keymaps 'markdown-mode-map       
     "C-c i" 'sync0-bibtex-completion-insert-markdown-citation
     :keymaps 'org-mode-map       
;;      "C-c i" 'sync0-bibtex-completion-clipboard-to-org-quote-block
     "C-c i" 'sync0-bibtex-completion-insert-markdown-citation))
  (with-eval-after-load 'citar
    (general-define-key "C-c ]" 'citar-insert-citation)))

(use-package major-mode-hydra
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  :config
  (with-eval-after-load 'org
    (major-mode-hydra-define org-mode nil 
      ("Links"
       (("s" org-store-link "Link insert")
	("i" org-insert-link "Link store")
	("k" org-insert-last-stored-link "Last stored link"))
       "Footnotes"
       (("f" org-footnote-new "New footnote")
	("a" org-footnote-action "Footnote actions"))
       "Trees"
       (("I" sync0-org-tree-to-indirect-buffer "Indirect buffer")
	("j" sync0-overview-jump-to-overview "Open overview")
	("o" sync0-overview-tree-window "Overview jump")
	("t" org-sparse-tree "Show sparse tree"))
       "Export"
       (("e" sync0-org-export-latex-and-beamer "Latex export")
	("E" sync0-org-export-headlines-to-latex "Export headlines")
	("g" sync0-copy-pdf-or-docx-to-goodreads "Copy pdf or docx to goodreads")
	("d" sync0-pandoc-export-org-to-docx "Export to DOCX"))
       "Open"
       (("L" (sync0-open-exported-file "docx") "Open DOCX with LibreOffice")
	("v" (sync0-open-exported-file "pdf") "Open PDF")) ; New command to open DOCX with LibreOffice
       "Etc."
       (("T" sync0-org-tangle-initfile "Tangle init file")))))
  (with-eval-after-load 'bibtex-completion
    (major-mode-hydra-define bibtex-mode nil 
      ("Entries"
       (("c" sync0-bibtex-clean-entry "Clean this entry")
	("E" sync0-bibtex-define-entry "New entry")
	("e" (sync0-bibtex-define-entry t) "Quick new entry")
	;; ("d" doi-utils-add-bibtex-entry-from-doi "Entry from DOI")
	;; ("m" sync0-bibtex-define-multiple-entries "Define entries")
	("d" sync0-bibtex-derive-entries-from-collection "Derive from collection")
	("t" sync0-bibtex-transplant-obsidian-ref-into-biblatex "Entry from mdnote")
	("u" sync0-bibtex-update-key "Update key")
	("n" sync0-bibtex-open-notes-at-point "Open notes")
	("M" sync0-bibtex-define-similar-type-entries "Define X similar entries")
	("f" sync0-bibtex-define-entries-from-bibkey "Define multiple from this entry")
	;; ("g" sync0-bibtex-python-bibentry-from-gallica "Define entry from Gallica")
	("W" sync0-bibtex-python-bibentry-from-webscrapper "Define from webscrapper")
	("Z" sync0-bibtex-bibentry-from-anystyle "Define from AnyStyle")
	("V" bibtex-completion-download-from-youtube "Download Youtube video")
	("2" sync0-bibtex-duplicate-attachment-from-bibkey "Duplicate attachment")
	("D" sync0-bibtex-python-bibentry-from-doi-or-isbn "Define from DOI or ISBN"))
       ;; ("w" sync0-bibtex-open-url "Open url")
       ;; ("M" sync0-bibtex-move-entry-to-bibfile "Move entry to bibfile")
       ;; ("D" sync0-bibtex-delete-entry "Delete entry")
       ;; ("A" sync0-bibtex-archive-entry "Archive entry")
       ;; ("1" sync0-bibtex-file-exists-p "Check file exists")
       "PDF editing & more"
       (("X" sync0-bibtex-delete-attachments "Delete attachments")
	;; ("P" sync0-pandoc-export-epub-to-pdf "EPUB to PDF")
	("P" bibtex-completion-search-pdf-with-python "Search PDF (Python)")
	("C" sync0-bibtex-copy-attachment-at-point "Copy attachment")
	;; ("C" sync0-bibtex-crop-pdf "Crop attached PDF")
	("3" bibtex-completion-ocr-pdf-files "OCR pdf")
	;; ("T" sync0-bibtex-recalc-tags-and-mdnote-at-point "Recalc keywords at point")
	("o" sync0-bibtex-open-pdf-at-point "Show PDF")
	("O" (sync0-bibtex-open-pdf-at-point t) "Show crossref PDF")
	;; ("x" sync0-bibtex-extract-from-crossref "Extract from crossref")
	;; ("P" sync0-bibtex-copy-pdf-to-path "Copy to path")
	("p" bibtex-completion-print-pdf-list "Print att. from entry"))
       ;; This does note work for some reason
       ;; ("x" sync0-bibtex-arrange-pdf "Arrange pdf")
       ;; ("T" sync0-bibtex-add-toc-to-pdf "Add TOC to PDF")
       ;; ("K" sync0-bibtex-add-key-to-pdf "Add key to PDF")
       ;; ("s" sync0-bibtex-extract-subpdf "Extract subpdf")
       ;; ("d" sync0-bibtex-download-pdf "Download pdf from url")
       ;; "Visit"
       ;; ("o" sync0-org-ref-open-pdf-at-point "Open in pdfview")
       ;; ("n" sync0-bibtex-open-notes "Open annotations")
       "Bibliographies"
       (
	;; ("U" sync0-ivy-bibtex-update-cache "Update BibTeX keys cache")
;; 	("S" sync0-consult-bibtex-multi-select "Search entry")
;; 	("s" sync0-consult-bibtex-with-local-bibliography "Search entry locally")
	;; ("b" sync0-bibtex-recalc-bibliographies "Recalc bibliographies")
	("B" sync0-bibtex-recalc-master-bibliography "Recalc master bib file")
	;; ("r" sync0-bibtex-populate-keys "Populate keys")
	("v" sync0-bibtex-visit-bibliography "Visit bibfile"))
       "Etc"
       ;; ("r" (sync0-bibtex-update-completion-files sync0-bibtex-completion-variables-list) "Refresh completion vars")
       (("a" sync0-bibtex-add-field-at-point "Add field")
	;; ("A" (sync0-bibtex-add-field-at-point t) "Add field and recalc")
	("I" sync0-bibtex-convert-jpg-to-pdf "Convert jpg to pdf")
	("i" bibtex-completion-open-url "Open URL")
	("k" sync0-add-field-theme "Add theme")
	("w" sync0-search-in-catalogs "Search in catalogs")
	("1" bibtex-convert-pdf-to-txt "Convert to TXT")
	;; ("2" sync0-bibtex-python-summarize-txt "Summarize TXT")
	;; ("N" sync0-bibtex-create-note-at-point "Create mdnote")
	;; ("R" (sync0-bibtex-create-note-at-point t) "Rewrite mdnote")
	("y" bibtex-completion-yank-citations-from-bibkeys "Yank citation.")))))
  (with-eval-after-load 'markdown-mode
    (major-mode-hydra-define markdown-mode nil 
      ("Links"
       ;; ("s" org-store-link)
       (("i" markdown-insert-wiki-link "Insert wiki-link")
	("k" markdown-insert-link "Insert markdown link")
	("I" markdown-insert-image "Insert image"))
       "Scholarly"
       (("f" sync0-toggle-footnote "Add or navigate footnote")
	;;     ("F" markdown-footnote-return "Footnote navigate back")
	("o" sync0-markdown-open-attachment-at-point "Open citation")
	("c" counsel-bibtex "Show bibtex entry"))
       "Visualization"
       (("m" markdown-toggle-markup-hiding "Toggle markup")
	("b" markdown-narrow-to-subtree "Narrow to header")
	("z" (sync0-open-exported-file "pdf") "Show PDF")
	("L" (sync0-open-exported-file "docx") "Show DOCX")
	("r" sync0-org-ref-open-pdf-at-point-zathura "Open reference pdf"))
       "Export"
       (("p" sync0-pandoc-export-md-to-pdf "Pdf")
	;;     ("p" sync0-pandoc-process-markdown "Pdf (quick)")
	("l" sync0-pandoc-export-md-to-tex "TeX")
	("d" sync0-pandoc-export-md-to-docx "Docx"))
       ;; ("b" org-epub-export-to-epub)
       ;; ("t" sync0-pandoc-export-md-to-tex)
       ;; ("E" sync0-org-export-headlines-to-latex)
       "Etc"
       (("a" sync0-markdown-adjust-heading-level "Adjust heading level")
	;;     ("a" sync0-define-local-abbrev "Define abbrev")
	("P" sync0-markdown-print-pdf "Print corresp. pdf")
	("C" sync0-markdown-copy-pdf-in-cabinet "Copy pdf to cabinet")
	;;    ("P" sync0-markdown-print-pdf-from-markdown "Print pdf")
	("1" sync0-markdown-correct-footnotes "Correct footnotes (all)")
	;; ("2" sync0-markdown-rename-footnotes-in-section "Correct footnotes (section)")
	("2" sync0-markdown-correct-footnotes-with-starting-number "Correct footnotes (w/ number)")
	("3" sync0-markdown-copy-header-choose "Copy text under header")
	("4" sync0-markdown-generate-toc "Generate TOC")
	("g" sync0-copy-pdf-or-docx-to-goodreads "Copy pdf to goodreads")
	;; ("C" sync0-markdown-save-exported-pdf-in-cabinet "Copy pdf to cabinet")
	("M" sync0-markdown-copy-pdf-to-path "Move to path")))))
  )

(use-package evil-leader
  :hook (after-init . global-evil-leader-mode)
  :custom
  (evil-leader/in-all-states t)
  :config
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "1" 'delete-other-windows
    "m" 'bookmark-set
    "q" 'keyboard-quit
    "w" 'write-file
    "s" 'save-buffer
    "R" 'revert-buffer
    "t" 'term
    "f" 'find-file
    "o" 'other-window
    "p" 'previous-buffer
    "<ESC>" 'keyboard-quit
    "n" 'next-buffer
    "k" 'quit-window
    "K" 'kill-buffer-and-window)
  (with-eval-after-load 'markdown-mode
    (evil-leader/set-key-for-mode 'markdown-mode "M" 'sync0-imenu-markdown-headings))
  (with-eval-after-load 'sync0-language
    (evil-leader/set-key "L" 'sync0-ispell-word-then-abbrev))
  (with-eval-after-load 'consult
    (evil-leader/set-key
      "M" 'consult-imenu
      "r" 'consult-recent-file
      "y" 'consult-yank-pop
      "j" 'consult-bookmark
      "x" 'consult-mode-command
      "b" 'consult-buffer))
  (with-eval-after-load 'sync0-functions
    (evil-leader/set-key
      "2" 'sync0-split-and-follow-horizontally
      "3" 'sync0-split-and-follow-vertically
      "<SPC>" 'sync0-insert-whitespace
      "D" 'sync0-delete-file-and-kill-buffer
      "4" 'sync0-create-scratch-buffer
      ;; "N" 'sync0-find-next-file
      "e" 'sync0-eval-last-sexp-or-region))
  (with-eval-after-load 'org-roam
    (evil-leader/set-key-for-mode 'org-mode "I" 'sync0-org-roam-node-insert-filtered)
    (evil-leader/set-key "J" 'sync0-org-roam-node-find-non-literature))
  (with-eval-after-load 'org
    (evil-leader/set-key-for-mode 'org-mode "M" 'org-goto)
    (evil-leader/set-key-for-mode 'org-mode "O" 'org-open-at-point)
    (evil-leader/set-key-for-mode 'org-mode "#" 'sync0-org-open-other-frame))
  ;; (evil-leader/set-key "V" 'sync0-consult-bibtex-with-local-bibliography)
  (with-eval-after-load 'sync0-ox-latex
    (evil-leader/set-key-for-mode 'org-mode "X" 'sync0-org-export-latex-and-beamer))
  (with-eval-after-load 'magit
    (evil-leader/set-key  "g" 'magit-status))
  (with-eval-after-load 'citar
    (evil-leader/set-key "v" 'citar-open))
  ;; (define-key org-mode-map (kbd "C-c ]") 'org-cite-insert)
;;   (with-eval-after-load 'sync0-ivy-bibtex
;;     (evil-leader/set-key "V" 'sync0-consult-bibtex-multi-select))
  (with-eval-after-load 'sync0-ivy-bibtex-functions
    (evil-leader/set-key-for-mode 'markdown-mode "i" 'sync0-bibtex-completion-insert-markdown-citation)
    (evil-leader/set-key-for-mode 'org-mode "i" 'sync0-bibtex-completion-clipboard-to-org-quote-block)))

(use-package hydra
  :after evil-leader
  :config
  (defhydra sync0-hydra-help (:color amaranth :hint nil :exit t)
    "
   ^Help functions^
   ^^^------------------------
   Describe _f_unction
   Describe _v_ariable
   Describe _k_eybindings

   _q_uit
   "
    ;; Quickly work with bookmarks
    ("f" describe-function)
    ("v" describe-variable)
    ("k" describe-key)
    ("q"  nil :color blue))

  (evil-leader/set-key "h" 'sync0-hydra-help/body)

  (with-eval-after-load 'org-download
    (defhydra sync0-hydra-org-download-functions (:color amaranth :hint nil :exit t)
      "
    ^Download functions^   
    ^--------------------
    _c_lipboard
    _y_ank
    _s_creenshot
                                                                     
    _q_uit
         "
      ("c" org-download-clipboard)
      ("y" org-download-yank)
      ("s" org-download-screenshot)
      ("q" nil :color blue))
    (evil-leader/set-key "D" 'sync0-hydra-org-download-functions/body))

  (with-eval-after-load 'avy
    (defhydra hydra-avy (:hint nil :color blue)
      "
Avy Navigation:
  [_c_] Goto Char   [_C_] Goto Char 2    [_w_] Goto Word
  [_l_] Goto Line   [_e_] End of Line    [_q_] Quit
"
      ("c" avy-goto-char)
      ("C" avy-goto-char-2)
      ("w" avy-goto-word-1)
      ("l" avy-goto-line)
      ("e" avy-goto-end-of-line)
      ("q" nil "Quit"))

    (evil-leader/set-key "a" 'hydra-avy/body))

  (with-eval-after-load 'sync0-language
    (defhydra sync0-hydra-language-functions (:color amaranth :hint nil :exit t)
      "
     ^Language functions^
     ^^^------------------------
     Show _d_efinition
     Show _c_onjugation
     Show in _t_hesaurus

     _q_uit
        "
      ;; Quickly work with bookmarks
      ("d" sync0-lookup-word)
      ;; ("i" sync0-ispell-word-then-abbrev)
      ("c" sync0-lookup-conjugation)
      ("t" sync0-lookup-thesaurus)
      ("q"  nil :color blue)))
  ;; (evil-leader/set-key "l" 'sync0-hydra-language-functions/body)

  (with-eval-after-load 'workgroups2
    (defhydra hydra-workgroups (:hint nil :exit t)
      "
^Workgroups Commands^
^^^^^^^^---------------------------------
_w_: Switch Workgroup       _n_: Next Workgroup
_c_: Create Workgroup       _p_: Previous Workgroup
_r_: Rename Workgroup       _d_: Delete Workgroup
_s_: Save Workgroups        _l_: Load Workgroups
_o_: Open Workgroup         _h_: Show Hydra
"
      ("w" wg-switch-to-workgroup)
      ("n" wg-next-workgroup)
      ("p" wg-prev-workgroup)
      ("c" wg-create-workgroup)
      ("r" wg-rename-workgroup)
      ("d" wg-delete-workgroup)
      ("s" wg-save-session)
      ("l" wg-load-session)
      ("o" wg-open-workgroup)
      ("h" hydra-workgroups/body)
      ("q" nil "Quit" :exit t))
    ;; Bind the Hydra to a leader key or any key of your choice
    (evil-leader/set-key
      "W" 'hydra-workgroups/body))

  (with-eval-after-load 'smartparens
    (defhydra sync0-hydra-smart-parens (:hint nil)
      "
 Sexps functions (_q_uit)
 ^Nav^            ^Barf/Slurp^                 ^Depth^
 ^---^------------^----------^-----------------^-----^-----------------
 _f_: forward     _→_:          slurp forward   _R_: splice
 _b_: backward    _←_:          barf forward    _r_: raise
 _u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
 _d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
 _p_: backward ↓
 _n_: forward ↑

 ^Kill^           ^Misc^                       ^Wrap^
 ^----^-----------^----^-----------------------^----^------------------
 _w_: copy        _j_: join                    _(_: wrap with ( )
 _k_: kill        _s_: split                   _{_: wrap with { }
 ^^               _t_: transpose               _'_: wrap with ' '
 ^^               _c_: convolute               _\"_: wrap with \" \"
 ^^               _i_: indent defun"

      ("q" nil)
      ;; Wrapping
      ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
      ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
      ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
      ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
      ;; Navigation
      ("f" sp-forward-sexp )
      ("b" sp-backward-sexp)
      ("u" sp-backward-up-sexp)
      ("d" sp-down-sexp)
      ("p" sp-backward-down-sexp)
      ("n" sp-up-sexp)
      ;; Kill/copy
      ("w" sp-copy-sexp)
      ("k" sp-kill-sexp)
      ;; Misc
      ("t" sp-transpose-sexp)
      ("j" sp-join-sexp)
      ("s" sp-split-sexp)
      ("c" sp-convolute-sexp)
      ("i" sp-indent-defun)
      ;; Depth changing
      ("R" sp-splice-sexp)
      ("r" sp-splice-sexp-killing-around)
      ("<up>" sp-splice-sexp-killing-backward)
      ("<down>" sp-splice-sexp-killing-forward)
      ;; Barfing/slurping
      ("<right>" sp-forward-slurp-sexp)
      ("<left>" sp-forward-barf-sexp)
      ("C-<left>" sp-backward-barf-sexp)
      ("C-<right>" sp-backward-slurp-sexp))))

;; (evil-leader/set-key "S" 'sync0-hydra-smart-parens/body))

;; (evil-leader/set-key-for-mode 'markdown-mode "O" 'sync0-zkn-follow-link-at-point)

;; (defhydra sync0-hydra-obsidian-functions (:color amaranth :hint nil :exit t)
;;   "
;; ^Zettel Functions^               ^Other Obsidian Functions^
;; ^^^----------------------------   ^^^-----------------------------
;; _z_ettel quick                    _o_pen at point
;; _s_tructure                       _j_ump to note
;; _f_iche                           tag find
;; _k_eyword                         x search by expr
;; _p_eople fiche                    update tags/aliases
;; _c_reate zettel (generic)         _d_raft create
;; _S_earch in catalogs              _i_nsert (mdlink)
;; _r_ename zkn file                 _R_efactor notes
;; _m_ove file (in vault) 
;; ^^^                               _q_uit
;; "
;;   ("z" sync0-obsidian-create-quick-zettel)
;;   ("c" sync0-obsidian-create-zettel)
;;   ("s" sync0-obsidian-create-structure-zettel)
;;   ("f" sync0-obsidian-create-fiche-zettel)
;;   ("k" sync0-obsidian-create-keyword-zettel)
;;   ("p" sync0-obsidian-create-multiple-people-fiche)
;;   ("m" sync0-zkn-move-file)
;;   ("r" sync0-zkn-rename-file)
;;   ;; ("m" sync0-obsidian-create-multiple-notes)
;;   ("S" sync0-search-in-catalogs)
;;   ("o" sync0-zkn-follow-link-at-point)
;;   ("j" sync0-zkn-find-file)
;;   ;; ("t" obsidian-tag-find)
;;   ;; ("x" obsidian-search)
;;   ;; ("u" obsidian-update)
;;   ("d" sync0-obsidian-new-draft)
;;   ("i" sync0-zkn-insert-md-link :color blue)
;;   ("R" sync0-zkn-note-refactor :color blue)
;;   ;; ("c" obsidian-capture)
;;   ("q" nil :color blue))
;;
;; (evil-leader/set-key
;;   "c" 'sync0-hydra-obsidian-functions/body)

(provide 'sync0-keybindings)

