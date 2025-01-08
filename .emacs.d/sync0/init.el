(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)

  (setq straight-use-package-by-default t)

(eval-when-compile
   ;; Activate "use-package". 
   (require 'use-package))
 ;; Necessary to shorten mode line package names with ":diminish".
;; (require 'diminish)               
 ;; Necessary to allow use-package to bind keys through ":bind" keyword.
 (require 'bind-key)

 (setq use-package-verbose t)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'load-path (concat user-emacs-directory "sync0/"))

(setq user-full-name "Carlos Alberto Rivera Carreño"
;; Define my Dropbox location
         sync0-dropbox-dir "~/Dropbox/"
         user-mail-address "carc.sync0@gmail.com")

;; Bookmarks directory
(setq bookmark-default-file (concat user-emacs-directory "bookmarks")
      bookmark-save-flag 1)

(setq auto-save-interval 100
      auto-save-timeout 60)


(use-package custom
  :straight nil
  :custom
  ;; Allow automatic theme changing 
  (custom--inhibit-theme-enable nil)
  ;; (custom-enabled-themes  (quote (doom-flatwhite)))
  :config
  ;; Set CUSTOM directory.
  (setq custom-file (expand-file-name "custom_settings.el" user-emacs-directory))
  ;; Default settings for all themes.
 (custom-theme-set-faces 'user
                         `(org-default ((t (:family "Minion Pro" :weight normal :height 1.0))))
                         `(org-link ((t (:inherit org-default :underline t))))
                         `(org-ref-cite-face ((t (:inherit org-link)))) 
                         `(org-footnote ((t (:family "Minion Pro" :height 0.7 :weight ultra-bold))))
                         ;; `(org-footnote ((t (:family "Inconsolata" :height 0.7 :weight ultra-bold))))
                         `(org-checkbox ((t (:family "Inconsolata" :weight bold))))
                         `(org-document-info ((t (:family "Myriad Pro" :height 1.1))))
                         `(org-document-title ((t (:inherit org-document-info))))
                         `(org-level-1 ((t (:family "Myriad Pro" :height 1.2 :weight bold))))
                         `(org-level-2 ((t (:family "Myriad Pro" :height 1.1 :weight normal))))
                         `(org-level-3 ((t (:family "Myriad Pro" :height 1.0 :weight semi-bold)))) 
                         `(org-level-4 ((t (:family "Myriad Pro" :height 1.0 :weight normal)))) 
                         `(org-level-5 ((t (:family "Myriad Pro" :height 0.9 :weight semi-bold)))) 
                         `(org-level-6 ((t (:family "Myriad Pro" :height 0.9 :weight normal)))) 
                         `(org-meta-line ((t (:family "Inconsolata" :height 0.95 :slant normal :inherit fixed-pitch)))) 
                         `(org-document-info-keyword ((t (:inherit org-meta-line))))
                         `(org-special-keywords ((t (:inherit org-meta-line))))
                         `(org-drawer ((t (:inherit org-meta-line)))) 
                         `(org-property-value ((t (:inherit org-meta-line)))) 
                         `(org-ellipsis ((t (:family "Fira Code" :underline nil :box nil)))) 
                         ;; `(org-hide ((t (:family "Symbola" :weight bold)))) 
                         ;; `(org-indent ((t (:inherit org-hide)))) 
                         `(org-date ((t (:family "Inconsolata" :height 0.95))))
                         `(org-agenda-date ((t (:family "Minion Pro" :weight normal :height 1.2))))
                         `(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
                         `(org-agenda-date-today ((t (:family "Minion Pro" :weight normal :height 1.2 :slant italic))))
                         `(org-agenda-structure ((t (:family "Minion Pro" :weight normal :height 1.6))))
                         `(org-scheduled ((t (:weight normal :slant normal))))
                         `(org-scheduled-today ((t (:family "Inconsolata" :weight normal :slant normal))))
                         `(org-scheduled-previously ((t (:family "Inconsolata" :weight normal :slant normal))))
                         `(org-upcoming-deadline ((t (:inherit org-scheduled-previously))))
                         `(org-agenda-diary ((t (:family "Inconsolata" :inherit fixed-pitch))))
                         `(org-agenda-done ((t (:strke-through t :inherit fixed-pitch))))
                         `(org-table ((t (:family "Inconsolata" :height 0.95 :inherit fixed-pitch))))
                         `(org-block ((t (:family "Inconsolata" :height 0.95 :inherit fixed-pitch :background nil))))
                          `(org-block-begin-line ((t (:family "Inconsolata" :height 0.95 :inherit fixed-pitch :weight bold))))
                          `(org-block-end-line ((t (:inherit org-block-begin-line))))
                         ;; `(org-column ((t (:family "Inconsolata"))))
                         ;; `(org-code ((t (:family "Inconsolata" :height 0.75  :inherit fixed-pitch))))
                         `(org-tag ((t (:family "Inconsolata" :height 0.75))))))

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Store all autosave files in the tmp directory.
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      ;; Store all backups in the "backups" directory.
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 5
      kept-old-versions 0
      ;; Use versioned backups.
      version-control t
      ;; Don't create lockfiles.
      create-lockfiles nil)

(setq system-time-locale "EN_US.UTF-8")

;; Improve slow down due to undo
(setq-default undo-limit 100000
              ;; Split verticly by default
              ;; split-width-threshold 0         
              ;; split-width-threshold nil
              split-width-threshold (- (window-width) 10)
              ;; Split verticly by default
              split-height-threshold nil        
              ;; hide cursors in other windows
              cursor-in-non-selected-windows nil  
              ;; Don't resize frames implicitly.
              frame-inhibit-implied-resize t
              highlight-nonselected-windows nil
              ;; Don't show the "Welcome to GNU Emacs ..." at startup
              inhibit-startup-screen t
              ;; Stop asking whether themes are safe
              custom-safe-themes t
              ;; Loop animated images such as gif files. 
              image-animate-loop nil)

(defun sync0-count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown. Defaults to selected frame."
  (length (mapcar #'window-buffer (window-list frame))))

(defun sync0-prevent-split-over-two-windows (window &optional horizontal)
  (if (and horizontal (> (sync0-count-visible-buffers) 1))
      nil
    t))

(advice-add 'window-splittable-p :before-while #'sync0-prevent-split-over-two-windows)

;; Font size change
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;; Cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)
;; EVIL friendly keybindings for next-buffer
(global-set-key (kbd "M-h") 'next-buffer)
;; Quickly save
(global-set-key (kbd "M-w") 'save-buffer)
;; EVIL friendly keybindings for previous-buffer
(global-set-key (kbd "M-l") 'previous-buffer)

(defvar sync0-zkn-all-properties-list
    '("ZETTEL_TYPE" "BIBLATEX_TYPE" "ZETTEL_FUNCTION" "FICHE_TYPE" "PROJECT_TITLE" "ANNOTATION_REFS" "ROAM_REFS" "ROAM_ALIASES" "CROSSREF" "PARENT" "WEBSITE") 
    "List of zettel properties")

    (defvar sync0-zkn-properties-list
    '("PROJECT_TITLE" "DATE" "ORIG_DATE") 
    "List of zettel properties")

    (defvar sync0-zkn-excluded-candidates
    '("journal" "fiche" "etc" "project" "todo" "reference"))

    (defvar sync0-zkn-project-dirs '("project" "todo"))

    (defvar sync0-zkn-zettel-types 
     '()
    "List of projects in my Zkn.")

    (defvar sync0-zkn-projects 
     '()
    "List of projects in my Zkn.")

    (defvar sync0-zkn-zettel-functions 
    '() 
    "List of possible functions for a Zettel.")

    (defvar sync0-zkn-fiche-types 
    '() 
    "List of fiche types.")

  (defvar sync0-zkn-variables-list
  '((sync0-zkn-projects . "~/.emacs.d/sync0-vars/projects.txt")
  (sync0-zkn-zettel-types . "~/.emacs.d/sync0-vars/zettel-types.txt")
  (sync0-zkn-zettel-functions . "~/.emacs.d/sync0-vars/zettel-functions.txt")
  (sync0-zkn-fiche-types . "~/.emacs.d/sync0-vars/fiche-types.txt")))

  (defvar sync0-biblatex-entry-types
    '("article" "book" "inbook" "incollection" "collection" "unpublished" "thesis" "proceedings" "inproceedings" "online" "report" "manual")
    "List of BibLaTeX entry types")

  (defvar sync0-biblatex-fields
  '("title" "subtitle" "date" "origdate" "author" "journaltitle" "booktitle" "booksubtitle" "crossref" "volume" "number" "publisher" "location" "pages" "addendum" "url" "urldate" "language" "langid" "medium" "trace" "file")
    "List of BibLaTeX entry fields")

(defvar sync0-biblatex-quick-fields
'("title" "subtitle" "date" "author" "addendum" "url" "urldate" "language" "langid" "medium" "trace" "file")
  "List of BibLaTeX entry fields")

    (defvar sync0-bibtex-booktitles 
     '()
    "List of bibtex authors")

    (defvar sync0-bibtex-publishers 
     '()
    "List of bibtex authors")

    (defvar sync0-bibtex-journals 
     '()
    "List of bibtex authors")

    (defvar sync0-bibtex-locations 
     '()
    "List of bibtex authors")

    (defvar sync0-bibtex-authors 
     '()
    "List of bibtex authors")

  (defvar sync0-bibtex-languages
  '()
    "List of BibLaTeX languages")

  (defvar sync0-bibtex-media
  '()
    "List of BibLaTeX media")

  (defvar sync0-bibtex-traces
  '()
    "List of BibLaTeX traces")

  (defvar sync0-bibtex-variables-list
  '((sync0-bibtex-booktitles . "~/.emacs.d/sync0-vars/bibtex-booktitles.txt")
  (sync0-bibtex-publishers . "~/.emacs.d/sync0-vars/bibtex-publishers.txt")
  (sync0-bibtex-journals . "~/.emacs.d/sync0-vars/bibtex-journals.txt")
  (sync0-bibtex-locations . "~/.emacs.d/sync0-vars/bibtex-locations.txt")
  (sync0-bibtex-authors .  "~/.emacs.d/sync0-vars/bibtex-authors.txt")
  (sync0-bibtex-traces .  "~/.emacs.d/sync0-vars/bibtex-trace.txt")
  (sync0-bibtex-media .  "~/.emacs.d/sync0-vars/bibtex-media.txt")
  (sync0-bibtex-languages .  "~/.emacs.d/sync0-vars/languages.txt")))

  ;; define the rest
    (setq sync0-zkn-dir (concat (getenv "HOME") "/Dropbox/org/")
    sync0-zkn-dir-sans (concat (getenv "HOME") "/Dropbox/org")
    sync0-exported-pdfs-dir (concat (getenv "HOME") "/Dropbox/pdfs/")
    sync0-default-bibliography (concat (getenv "HOME") "/Dropbox/bibliographies/doctorat.bib")
    sync0-zkn-dir-references (concat (getenv "HOME") "/Dropbox/org/reference/")
    sync0-emacs-dir (concat (getenv "HOME") "/.emacs.d/sync0/")
    sync0-pdfs-folder (concat (getenv "HOME") "/Documents/pdfs/")
    sync0-current-year (format-time-string "%Y")
    sync0-current-month (format-time-string "%B")
    sync0-current-month-downcase (downcase (format-time-string "%B"))
    sync0-current-day (format-time-string "%d")
    sync0-english-parts-speech '("noun" "intransitive verb" "transitive verb" "verb" "conjunction" "adjective" "adverb")
    sync0-french-parts-speech '("nom féminin" "nom masculin" "verbe intransitif" "verbe transitif" "verbe" "conjonction" "adjectif" "adverbe")
    sync0-portuguese-parts-speech '("sustantivo femenino" "sustantivo masculino" "verbo intransitivo" "verbo transitivo" "verbo" "conjunção" "adjetivo" "advérbio")
              sync0-spanish-parts-speech '("sustantivo femenino" "sustantivo masculino" "verbo intransitivo" "verbo transitivo" "verbo" "conjunción" "adjectivo" "adverbio"))

(defmacro sync0-redefine (symbol value)
  `(setf ,symbol ,value))

(defmacro sync0-nullify-variable (var)
  "Make target variable nil"
  `(setf ,var nil))

(defun sync0-set-variable-from-files  (varlist)
    "From a list of pairs of variable and files, define all of them
  with a loop"
    (dolist (element varlist) 
      (let ((var (car element))
            (file (cdr element)))
        ;; (sync0-nullify-variable var)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; (keep-lines "contexts" (point-min) (point-max)) 
          (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
            (add-to-list var (match-string-no-properties 1)))))))

(sync0-set-variable-from-files sync0-zkn-variables-list)
(sync0-set-variable-from-files sync0-bibtex-variables-list)

(defun sync0-downcase-and-no-whitespace (x)
"Downcase and replace whitespace by _ in the current string"
    (downcase
     (replace-regexp-in-string "[[:space:]-]+" "_" x)))

        (defun sync0-insert-today-timestamp () 
          "Insert today's date in the YYYY/MM/DD format"
          (insert (format-time-string "%Y/%m/%d")))

        (defun sync0-org-insert-today-timestamp () 
          "Insert today's date in the YYYY/MM/DD format"
          (insert (format-time-string "<%Y-%m-%d>")))

        (defun sync0-update-timestamp ()
          "Update current #+DATE timestamp"
       (org-with-point-at 1
            (let ((regex ":LAST_MODIFIED: \\(.+\\)")
                  (date (format-time-string "%Y-%m-%d")))
               (when (re-search-forward regex nil nil 1)
                     (replace-match date nil nil nil 1)))))

  (defun sync0-zkn-update-org-properties ()
    (let*  ((zettel-properties
             (let (x)
               (dolist (property sync0-zkn-properties-list x)
                 (when-let ((value (org-entry-get 1 property)))
                   (if (string-match-p "\" \"" value)
                       (let ((elements
                              (delete " "
                                      (split-string-and-unquote value "\" \""))))  
                         (mapcar #'(lambda (y)
                                     (push
                                      (sync0-downcase-and-no-whitespace y)
                                      x)) elements))
                     (if (string-match "\"\\([[:print:]]+\\)\""  value)
                         (push
                          (sync0-downcase-and-no-whitespace (match-string 1 value))
                          x)
                       (push (sync0-downcase-and-no-whitespace value)  x)))))
               x))
            ;; (path default-directory)
            ;; (path-dirs (split-string-and-unquote path "/"))
            ;; (zkn-dirs (split-string-and-unquote sync0-zkn-dir "/"))
            ;; ;; this produces a list not a string
            ;; (current-dir  (cl-set-difference path-dirs zkn-dirs  :test #'equal))
            ;; (corrected-properties (cl-union current-dir zettel-properties  :test #'equal))                                 
            (tags-line (cadar (org-collect-keywords '("FILETAGS"))))
            (tags (split-string-and-unquote tags-line ":"))
            ;; (new-tags (cl-union corrected-properties tags :test #'equal))
            (new-tags (cl-union zettel-properties tags :test #'equal))
            (new-tags-line
             (let (x)
               (dolist (element new-tags x)
                 (setq x (concat element  ":" x)))))
            (corrected-tags-line (concat ":" new-tags-line)))
      (org-with-point-at 1
        (re-search-forward "^#\\+FILETAGS:" (point-max) t)
        (kill-whole-line 1)
        (insert (concat "#+FILETAGS: " corrected-tags-line "\n"))
  (dolist (property sync0-zkn-properties-list)
         (when-let ((value (org-entry-get 1 property)))
           (unless (or (string-match-p "\" \"" value)
                       (string-match-p "\"[[:print:]]+\"" value))
            (org-set-property property (concat "\""  value "\""))))))))

        (add-hook 'before-save-hook (lambda ()
      ;; Check whether file is in org-mode and whether it is located in my Zkn directory
                                      (when (and (equal major-mode 'org-mode)
                                            ;;(not (string-prefix-p "archives" (buffer-file-name)))
                                            (string-prefix-p sync0-zkn-dir (buffer-file-name)))
                                                     ;; (equal default-dir (concat (getenv "HOME") "/Dropbox/annotations/"))
                                        (sync0-zkn-update-org-properties)
                                        (sync0-update-timestamp))))

(defun sync0-show-file-path ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name)))

;; https://emacs.stackexchange.com/questions/36850/copy-to-kill-ring-selected-file-names-full-path
(defun sync0-dired-copy-path-at-point ()
  "In dired buffers, copy the full path of file at point." 
    (interactive)
    (dired-copy-filename-as-kill 0))

(defun sync0-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun sync0-split-and-follow-horizontally ()
  " Split the selected window into two side-by-side windows.
  The selected window, which displays the same buffer, is on the
  right."
  (interactive)
  (progn
    (split-window-below)
    (balance-windows)
    (setq truncate-lines t)
    (setq truncate-partial-width-windows t)
    (other-window 1)))

(defun sync0-split-and-follow-vertically ()
  " Split the selected window into two windows, one above the other.
  The selected window, which displays the same buffer, is below."
  (interactive)
  (progn
    (split-window-right)
    (balance-windows)
    (setq truncate-lines t)
    (setq truncate-partial-width-windows t)
    ;; (sync0-restore-margins)
    (other-window 1)))

(defun sync0-find-next-file (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(defun sync0-insert-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (insert (concat (car list) "\n"))
    (setq list (cdr list))))

(defun sync0-update-list (newelt list file)
  "Saves my projects in my home folder."
  (if (member newelt list)
      (message "%s already exists in %s" newelt file)
    (let ((file-path
           (concat "~/.emacs.d/sync0-vars/" file ".txt"))
          (new-list (cons newelt list)))
      ;; (add-to-list list newelt)
       (sync0-redefine list new-list)
      (with-temp-file file-path
        (sync0-insert-elements-of-list list)
        (save-buffer)
        (message "%s added to %s" newelt file)))))

;; (defun replace-smart-quotes (beg end)
  ;; "Replace 'smart quotes' in buffer or region with ascii quotes."
  ;; (interactive "r")
  ;; (format-replace-strings '(("\x201C" . "\"")
  ;;                           ("\x201D" . "\"")
  ;;                           ("\x2018" . "'")
  ;;                           ("\x2019" . "'"))
  ;;                         nil beg end))

(setq smart-quote-regexp-replacements
      '(("\\(\\w\\)- " . "\\1")
        ("\\(\\w\\)\\(  [-—] \\|—\\)" . "\\1---")))

;; Replace smart quotes with straight quotes so that spell check can recognize
;; words with contractions like “don’t” and “can’t.” For when I paste text in
;; that I’ve copied from the web.
(defun replace-smart-quotes-regexp (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (mapcar
   (lambda (r)
     (save-excursion
       (replace-regexp (car r) (cdr r) nil beg (min end (point-max)))))
   smart-quote-regexp-replacements))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  ;;(while (search-forward-regexp "- " nil to)
  ;; (replace-match "") nil t)
  ;; add alpha. And replace the alpha.

  (replace-smart-quotes-regexp beg end)
  (format-replace-strings '(
                            ("\x201C" . "``")
                            ("“" . "``")
                            ("\x201D" . "''")
                            ("”" . "''")
                            ("\x2018" . "`")
                            ("\x2019" . "'")
                            ("’" . "'")
                            ;;("''" . "\"")
                            ;;("​" . "")
                            ;;("…" . "...")
                            ("…" . "\\ldots")
                            ("..." . "\\ldots")
                            ;;("• " . "- ")
                            ;;(" " . "")
                            ("  " . " "))
                          nil   beg (min end (point-max))))

;; (defun org-keyword-title-p ()
  ;;   (interactive)
  ;;   (if (and (equal major-mode 'org-mode)
  ;;            (cadar (org-collect-keywords '("TITLE"))))
  ;;       (message "t")
  ;;     (message "nil")))

  (defun org-keyword-title-p ()
"Check whether current buffer is an org-mode file with a non-null
TITLE keyword."
    (and (equal major-mode 'org-mode)
         (cadar (org-collect-keywords '("TITLE")))))

(defun split-string-every (string chars)
  "Split STRING into substrings of length CHARS characters.

  This returns a list of strings."
  (cond ((string-empty-p string)
         nil)
        ((< (length string)
            chars)
         (list string))
        (t (cons (substring string 0 chars)
                 (split-string-every (substring string chars)
                                     chars)))))

(defun sync0-split-string-with-separator (string separator)
  "Check the presence of a separator in current string and split
when necessary."
  (interactive)
;; check for the presence of a separator
  (if (string-match-p separator string)
      (string-trim
       (prin1-to-string
        (split-string-and-unquote string separator))
       "(" ")")
    string))

(use-package undo-tree
:custom
(undo-tree-enable-undo-in-region nil)
:config
(global-undo-tree-mode))

(use-package hydra
  :straight (hydra :type git :host github :repo "abo-abo/hydra"))

(use-package which-key
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
    :custom
(which-key-popup-type 'side-window)
(which-key-side-window-location 'bottom)
(which-key-side-window-max-width 0.33)
(which-key-side-window-max-height 0.25)
;; (which-key-idle-delay 10000)
;; (which-key-idle-secondary-delay 0.05)
    :config
      (which-key-mode))

(use-package evil-leader
        :straight (evil-leader :type git :host github :repo "cofi/evil-leader") 
        :hook (after-init . global-evil-leader-mode)
       :config
         (evil-leader/set-leader "<SPC>")
        (setq evil-leader/in-all-states t)

(evil-leader/set-key
  "1" 'delete-other-windows
  "2" 'sync0-split-and-follow-horizontally
  "3" 'sync0-split-and-follow-vertically
  "m" 'bookmark-set
  "q" 'keyboard-quit
  "w" 'write-file
  "e" 'eval-last-sexp
  "s" 'save-buffer
  "o" 'other-window
  "p" 'previous-buffer
  "n" 'next-buffer
  "N" 'sync0-find-next-file
  "k" 'kill-buffer-and-window 
  ;; "k" 'delete-window
  "K" 'kill-buffer))

(use-package evil-escape 
  :straight (evil-escape :type git :host github :repo "syl20bnr/evil-escape") 
  :after evil
  ;; :commands evil-escape-mode
  :custom
  (evil-escape-excluded-states '(normal visual multiedit emacs motion))
  ;;(evil-escape-excluded-major-modes '(neotree-mode))
  (evil-escape-key-sequence "fd")
  (evil-escape-unordered-key-sequence t)
  (evil-escape-delay 0.25)
  :config
  ;; no `evil-escape' in minibuffer
  (push #'minibufferp evil-escape-inhibit-functions)
  :bind (:map evil-insert-state-map
              ("C-g"  . evil-escape)
              :map evil-replace-state-map
              ("C-g"  . evil-escape)
              :map evil-visual-state-map
              ("C-g"  . evil-escape)
              :map evil-operator-state-map
              ("C-g"  . evil-escape)))

(use-package evil-multiedit 
  :straight (evil-multiedit :type git :host github :repo "hlissner/evil-multiedit") 
  :commands 
  (evil-multiedit-and-next evil-multiedit-match-and-next evil-multiedit-toggle-marker-here)
  :after evil
  :bind (:map evil-visual-state-map
              ;; Highlights all matches of the selection in the buffer.
              ("R" . evil-multiedit-match-all)
              ;; Match selected region.
              ("M-d" . evil-multiedit-and-next)
              ("M-D" . evil-multiedit-and-prev)
              ;; Restore the last group of multiedit regions.
              ("C-M-D"  . evil-multiedit-restore)
              :map evil-normal-state-map
              ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
              ;; incrementally add the next unmatched match.
              ("M-d"  . evil-multiedit-match-and-next)
              :map evil-insert-state-map
              ;; Insert marker at point
              ("M-d"  . evil-multiedit-toggle-marker-here)
              :map evil-normal-state-map
              ;; Same as M-d but in reverse.
              ("M-D"  . evil-multiedit-match-and-prev)
              ;; OPTIONAL: If you prefer to grab symbols rather than words, use
              ;; `evil-multiedit-match-symbol-and-next` (or prev).
              ;; RET will toggle the region under the cursor
              :map evil-multiedit-state-map
              ("RET"  . evil-multiedit-toggle-or-restrict-region)
              ;; ...and in visual mode, RET will disable all fields outside the selected region
              ("RET"  . evil-multiedit-toggle-or-restrict-region)
              ;; For moving between edit regions
              ("C-n"  . evil-multiedit-next)
              ("C-p"  . evil-multiedit-prev)
              ("C-n"  . evil-multiedit-next)
              ("C-p"  . evil-multiedit-prev)))

(use-package ivy
  :hook 
  (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
(evil-leader/set-key
  "b" 'ivy-switch-buffer))

(use-package swiper 
  :after evil
  :commands swiper
  :config 
  (evil-define-key 'normal global-map "/" 'swiper)
  :bind (("C-s" . swiper)))

(use-package counsel 
    :after evil
    :config

    (evil-define-key 'normal global-map "gb" 'counsel-bookmark)

    (defhydra sync0-hydra-help (:color amaranth :hint nil :exit t)
  "
  ^Help functions^
  ^^^------------------------
  Describe _f_unction
  Describe _v_ariable
  Describe _k_eybindings
  Load _l_ibrary
  Search _s_ymbol
  Search _u_nicode char

  _q_uit
  "
      ;; Quickly work with bookmarks
      ("f" counsel-describe-function)
      ("v" counsel-describe-variable)
      ("k" describe-key)
      ("l" counsel-load-library)
      ("s" counsel-info-lookup-symbol)
      ("u" counsel-unicode-char)
      ("q"  nil :color blue))

(evil-leader/set-key
  "r" 'counsel-recentf
  "y" 'counsel-yank-pop
  "j" 'counsel-bookmark
  "f" 'counsel-find-file
  "x" 'counsel-M-x
  "h" 'sync0-hydra-help/body)

    :bind
    (("M-x" . counsel-M-x)
     ("M-y" . counsel-yank-pop)
     ("<f1>" . sync0-hydra-help/body)
     ("C-x C-f" . counsel-find-file)))

(use-package evil  
        ;; :straight (evil :type git :host github :repo "emacs-evil/evil") 
        :custom
        ;; Make horizontal movement cross lines                                    
        (evil-cross-lines t)
        ;; turn off auto-indent 
        (evil-auto-indent nil)
        :bind (("M-H" . next-buffer)
               ("M-L" . previous-buffer)
               (:map evil-normal-state-map
                     :map minibuffer-local-map
                     ("ESC" . minibuffer-keyboard-quit)
                     :map minibuffer-local-ns-map
                     ("ESC" . minibuffer-keyboard-quit)
                     :map minibuffer-local-completion-map
                     ("ESC" . minibuffer-keyboard-quit)
                     :map minibuffer-local-must-match-map
                     ("ESC" . minibuffer-keyboard-quit)
                     :map minibuffer-local-isearch-map
                     ("ESC" . minibuffer-keyboard-quit)))

        :config 
        (defun sync0-insert-line-below ()
          "Insert an empty line below the current line."
          (interactive)
          (save-excursion
            (end-of-line)
            ;; To insert the line above
            ;; (end-of-line 0)
            (open-line 1)))

        ;; insert whitespace
        (defun sync0-insert-whitespace ()
          " Add a whitespace"
          (interactive)
          (insert " "))

        (defun sync0-delete-text-block ()
          "Delete selection or current or next text block and also copy to `kill-ring'.
           URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
           Version 2016-08-13"
          (interactive)
          (if (use-region-p)
              (kill-region (region-beginning) (region-end))
            (progn
              (beginning-of-line)
              (if (search-forward-regexp "[[:graph:]]" (line-end-position) 'NOERROR )
                  (sync0-delete-current-text-block)
                (when (search-forward-regexp "[[:graph:]]" )
                  (sync0-delete-current-text-block))))))

        ;; Turn on evil mode when enabled.
        (evil-mode 1)
        ;; Turn on evil-escape mode when enabled.
        (evil-escape-mode 1)
;; prevent conflict with calf bindings. 
        (add-to-list 'evil-emacs-state-modes 'cfw:details-mode)

        ;; Change global key bindings
        (unbind-key "C-m" evil-normal-state-map)
        (unbind-key "M-." evil-normal-state-map)
        (unbind-key "C-d" evil-motion-state-map)
       ;; (unbind-key "<SPC>" evil-motion-state-map)

        (evil-define-key 'normal global-map
          "U" 'undo-tree-redo
          "s" 'fill-paragraph
          "S" 'sync0-insert-line-below
          "M" 'bookmark-set
          "zc" 'transpose-chars
          "zb" 'sync0-delete-text-block
          "zl" 'transpose-lines
          "zw" 'transpose-words
          "zj" 'evil-join
          "zp" 'transpose-paragraphs
          "zs" 'transpose-sentences)

       (evil-leader/set-key
           "<SPC>" 'sync0-insert-whitespace
           "<ESC>" 'keyboard-quit)

        ;; Improve EVIL behavior with visual lines (visual-line-mode).
        (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
        (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))

(use-package s)

(use-package simple-secrets
:straight nil
    :load-path "~/.emacs.d/sync0/" 
    :config 
    (setq secret-password-file "~/.emacs.d/sync0_secrets.gpg")
     (secret-load-keys))

(use-package xah-find
  :straight (xah-find :type git :host github :repo "xahlee/xah-find"))

(use-package epa-file
  :straight nil
  :load-path "~/.emacs.d/sync0/" 
  :custom
  (epa-file-encrypt-to '("carc.sync0@gmail.com"))
  (epa-file-select-keys 'silent)
  :config (epa-file-enable))

(use-package recentf
  :straight nil
  :custom
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 10)
  :config 
  (recentf-mode +1)
  (require 'dired-x)
  :bind (:map recentf-dialog-mode-map
              ("j"  . next-line)
              ("k"  . previous-line)))

(use-package saveplace
:straight nil
    :init
    (defun sync0-save-place-reposition ()
      "Force windows to recenter current line (with saved position)."
      (run-with-timer 0 nil
                      (lambda (buf)
                        (when (buffer-live-p buf)
                          (dolist (win (get-buffer-window-list buf nil t))
                            (with-selected-window win (recenter)))))
                      (current-buffer)))
    ;; Start save-place-mode.
    :config (save-place-mode)
    :hook (find-file . sync0-save-place-reposition))

(use-package exec-path-from-shell
    :straight (exec-path-from-shell :type git :host github :repo "purcell/exec-path-from-shell")
      :config
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(defun sync0-toggle-mode-line () 
  "toggles the modeline on and off"
  (interactive) 
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)) )
  (redraw-display))

(tool-bar-mode -1) 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-showhide-fringe-menu-customize-disable)

(defhydra sync0-hydra-menu-toggle (:color amaranth :hint nil :exit t)
      "
^Toolbar toggle functions^
^^^----------------
Hide mode _l_ine
Toggle _t_ool bar
Toggle _m_enu bar

_q_uit
"
      ("l" sync0-toggle-mode-line)
      ("t" tool-bar-mode)
      ("m" menu-bar-mode)
      ("q" nil :color blue))

(evil-leader/set-key
  "M" 'sync0-hydra-menu-toggle/body)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default 
               window-divider-default-bottom-width 2
               window-divider-default-right-width 2
               ;; Show both window dividers (right and bottom)
               window-divider-default-places 'right-only)

(add-hook 'emacs-startup-hook #'window-divider-mode)

(defun sync0-no-fringes-in-minibuffer ()
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window) 0 0 nil))

(add-hook 'minibuffer-setup-hook #'sync0-no-fringes-in-minibuffer)

(if (> (display-pixel-width) 1900)
;; High resolution settings (t14s)
   (setq-default                    
    ;; Avoid ugly problemes with git-gutter.
    fringes-outside-margins t
    left-margin-width 3
    ;; left-margin-width 2
    right-margin-width 0
    left-fringe-width 0
    ;; left-fringe-width 1
    right-fringe-width 0
    ;; Remove continuation arrow on right fringe.
    fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                 fringe-indicator-alist)
    indicate-buffer-boundaries nil
    indicate-empty-lines nil
    max-mini-window-height 0.3)
;; Low resolution settings:
   (setq-default                    
    ;; Avoid ugly problemes with git-gutter.
    fringes-outside-margins t
    left-margin-width 1
    ;; left-margin-width 2
    right-margin-width 0
    left-fringe-width 0
    ;; create a function to restore the fringe value when using git-gutter-fringe
    ;; left-fringe-width 1
    right-fringe-width 0
    ;; Remove continuation arrow on right fringe.
    fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                                 fringe-indicator-alist)
    indicate-buffer-boundaries nil
    indicate-empty-lines nil
    max-mini-window-height 0.3))

(use-package battery
 :custom
  (battery-mode-line-format "%t")
  (battery-update-interval 60)
 :config
  (display-battery-mode t))

;; Define a local variable with the total number of lines.
        (defvar-local sync0-mode-line-buffer-line-count nil)

        ;; Define a function that counts the number of lines in the
        ;; current buffer.
        (defun sync0-mode-line-count-lines ()
          "Count the number of lines in the current buffer."
          (setq-local sync0-mode-line-buffer-line-count 
                      (int-to-string (count-lines (point-min) (point-max)))))

        ;; Recalculate the total number of lines using hooks. This is
        ;; not the best approach, but I have not been able to devise a
        ;; dynamic way to calculate these that does not result in Emacs
        ;; "inventing" these results.
        (add-hook 'find-file-hook 'sync0-mode-line-count-lines)
        (add-hook 'after-save-hook 'sync0-mode-line-count-lines)
        (add-hook 'after-revert-hook 'sync0-mode-line-count-lines)

  ;;; Taken from 
  ;;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline

  (defun mode-line-fill (reserve)
    "Return empty space using FACE and leaving RESERVE space on the right."
    (when
      (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (propertize " "
      'display
      `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

  (defun sync0-mode-line-zettel-identification ()
    "For org-mode files display contents of the TITLE keyword when
    not null. Otherwise, display the file title with extension."
    (if (equal major-mode 'org-mode)
        (if-let* ((type (org-entry-get 1 "ZETTEL_TYPE"))
                  (subtype (upcase-initials (substring type 0 3)))
                  ;; (subtype (upcase (substring type 0 1)))
                  (type-string (concat "[" subtype "] ")))
            (propertize type-string 'face '(:weight bold))
          "")
      ""))

  (defun sync0-mode-line-buffer-identification ()
    "For org-mode files display contents of the TITLE keyword when
    not null. Otherwise, display the file title with extension."
    (if (and (equal major-mode 'org-mode)
             (org-keyword-title-p))
        (let*  ((title (cadar (org-collect-keywords '("TITLE")))) 
                (fixed-title (if (> (length title) 60) 
                                 (let ((start (substring title 0 35))
                                       (end (substring title -20 nil)))
                                   (concat start  "..." end))
                               title)))
          (propertize fixed-title 'face '(:height 1.0 :family "Myriad Pro" :weight normal) 'help-echo (buffer-file-name)))
      (propertize (buffer-name) 'face '(:weight bold) 'help-echo (buffer-file-name))))


        (defun sync0-mode-line-guess-language ()
          (if (boundp 'guess-language-current-language) 
              (cond  ((string-equal guess-language-current-language "en") 
                      (propertize "EN" 'face '(:weight bold)))
                     ((string-equal guess-language-current-language "de") 
                      (propertize "DE" 'face '(:weight bold)))
                     ((string-equal guess-language-current-language "pt") 
                      (propertize "PT" 'face '(:weight bold)))
                     ((string-equal guess-language-current-language "it") 
                      (propertize "IT" 'face '(:weight bold)))
                     ((string-equal guess-language-current-language "fr") 
                      (propertize "FR" 'face '(:weight bold)))
                     ((string-equal guess-language-current-language "es") 
                      (propertize "ES" 'face '(:weight bold)))
                     (t (propertize "NIL" 'face '(:weight bold))))
            (propertize "NIL" 'face '(:weight bold))))

(setq-default mode-line-format
              '(" " 
                (:eval (cond 
                        (buffer-read-only (propertize "🔒"
                                                      'face '(:family "Noto Color Emoji")
                                                      'help-echo "buffer is read-only!!!"))
                        ((buffer-modified-p) (propertize "✗"
                                                         'face '(:family "Noto Color Emoji")))
                        (t (propertize "✓"
                                       'face '(:family "Noto Color Emoji")))))
                "  " 
                (:eval (sync0-mode-line-zettel-identification))
                (:eval (sync0-mode-line-buffer-identification))
                "  " 
                (:eval (sync0-mode-line-guess-language))
                ;; evil-mode-line-tag
                "  "
                (:eval 
                 (let ((line-string "%l"))
                   (if (equal major-mode 'pdf-view-mode)
                       ;; this is necessary so that pdf-view displays the page numbers of the pdf
                       ;; otherwise, it is very hard to read documents. 
                       mode-line-position
                     (if (and (not (buffer-modified-p))
                              sync0-mode-line-buffer-line-count)
                         (setq line-string 
                               (concat "(" line-string "/" sync0-mode-line-buffer-line-count ")"))
                       (concat "(" line-string ")"))
                     )))
                (:eval (mode-line-fill 30))
                (:eval (if (equal debug-on-error nil)
                           (propertize "🐛" 'mouse-face 'mode-line-highlight 'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error) 'face '(:family "Noto Color Emoji"))
                         (propertize "🦋" 'mouse-face 'mode-line-highlight 'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error) 'face '(:family "Noto Color Emoji"))
                         ))

                " " 
                (:eval (propertize 
                        (capitalize 
                         (s-replace "-mode" "" (format "%s" major-mode)))
                        'face '(:weight bold)))
                " " 
                (vc-mode vc-mode)
                " " 
                (:eval (when (boundp 'org-mode-line-string)
                         (propertize  org-mode-line-string 'face '(:weight semi-bold))))
                ;; (:eval (propertize (format-time-string " %H:%M ")
                ;;                    'face '(:weight bold))) 
                ;; " " 
                (:eval  (propertize "⚡" 'face '(:family "Noto Color Emoji")))
                mode-line-misc-info
                emacs-mode-line-end-spaces))

(if (> (display-pixel-width) 1900)
    ;; high resolution font size (t14s)
    (progn (set-face-attribute 'default nil 
                          :family "Inconsolata"
                          :height 175)
                          ;;:height 170
      (setq line-spacing 0.2))
  ;; low resolution font size
  (progn (set-face-attribute 'default nil 
                        :family "Inconsolata"
                        :height 130)
    (setq line-spacing 0.1)))

(defun sync0-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (if (> (display-pixel-width) 1900)
  ;; high resolution font size (t14s)
    (progn
      (setq buffer-face-mode-face '(:family "Minion Pro" :height 230))
      ;;(setq buffer-face-mode-face '(:family "Minion Pro" :height 200))
  (setq line-spacing 0.35))
  ;; low resolution font size
    (progn
    ;; (setq buffer-face-mode-face '(:family "Minion Pro" :height 155))
     (setq buffer-face-mode-face '(:family "Minion Pro" :height 130))
  ;; (setq line-spacing 0.2)
  (setq line-spacing 0.25)))
  (buffer-face-mode))

(add-hook 'erc-mode-hook 'sync0-buffer-face-mode-variable)
  (add-hook 'Info-mode-hook 'sync0-buffer-face-mode-variable)
  (add-hook 'text-mode-hook 'sync0-buffer-face-mode-variable)
;;  (add-hook 'org-roam-mode-hook 'sync0-buffer-face-mode-variable)

;; End sentences with a single espace.
(setq-default sentence-end-double-space nil
              header-line-format " "
              ;; Use spaces instead of tabs
              indent-tabs-mode nil              
              ;; disable bidirectional text for tiny performance boost
              bidi-display-reordering nil 
              ;; Never truncate lines
              truncate-lines t
              truncate-partial-width-windows t
              ;; Help with displaying fonts
              inhibit-compacting-font-caches t)

(use-package dashboard
  :after (all-the-icons org)
  :straight (dashboard :type git :host github :repo "emacs-dashboard/emacs-dashboard") 
  :config
  (setq dashboard-items '(
  ;; (bookmarks . 5)
;;(recents  . 5)
                        ;; (projects . 5)
                        ;; (registers . 5)
                        (agenda . 5)))
  ;; (setq dashboard-banner-logo-title "Die Geschichte aller bisherigen Gesellschaft ist die Geschichte von Klassenkämpfen.")
  (setq dashboard-center-content t)
  ;; (setq dashboard-startup-banner "/home/sync0/Pictures/communism/hammer_sickle.png")
  ;; (setq dashboard-startup-banner "/home/sync0/Pictures/communism/marx_silhouette_small.png")
  ;; (setq dashboard-startup-banner "/home/sync0/Pictures/communism/toiling_workers.png")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-footer nil)
  (setq dashboard-week-agenda t)
  (dashboard-setup-startup-hook))

(use-package all-the-icons 
    :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el") 
;;    :after ivy
    ;; improve performance 
    :custom (inhibit-compacting-font-caches t))

(use-package solaire-mode
:disabled t
   :straight (solaire-mode :type git :host github :repo "hlissner/emacs-solaire-mode") 
   :hook
   (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
   (minibuffer-setup . solaire-mode-in-minibuffer))
   ;; :custom
   ;; (solaire-mode-remap-fringe nil)
   :config
   ;; (setq solaire-mode-remap-alist
   ;;       '(((default solaire-default-face)                       . nil)
   ;;         ((hl-line solaire-hl-line-face)                       . nil)
   ;;         ((org-hide solaire-org-hide-face)                     . nil)
   ;;         ((org-indent solaire-org-hide-face)                   . nil)
   ;;         ((linum solaire-line-number-face)                     . nil)
   ;;         ((mode-line solaire-mode-line-face)                   . solaire-mode-remap-modeline)
   ;;         ((mode-line-inactive solaire-mode-line-inactive-face) . solaire-mode-remap-modeline)))

 (setq solaire-mode-auto-swap-bg nil)

   (solaire-global-mode +1))

(use-package doom-themes  
 :straight (doom-themes :type git :host github :repo "hlissner/emacs-doom-themes") 
 :after (org custom)
 :init
      ;; (load-theme 'doom-one t)
      ;; (load-theme 'doom-nord t)
      ;; (load-theme 'doom-nova t)
      ;; (load-theme 'doom-spacegrey t)
      ;; (load-theme 'doom-solarized-light t)
      ;; (load-theme 'doom-plain t)
      ;; (load-theme 'doom-gruvbox t)
       (load-theme 'doom-zenburn t)
      (load-theme 'doom-flatwhite t)
 :config
    ;; Enable flashing mode-line on errors
     ;; (doom-themes-visual-bell-config)
    ;; Correct org-mode's native fontification.
    (doom-themes-org-config))

(use-package cycle-themes 
  :straight (cycle-themes :type git :host github :repo "toroidal-code/cycle-themes.el") 
  :after doom-themes
  :commands cycle-themes
  :init
  (require 'cl)
  :bind (("C-c C-t" . cycle-themes))
  :config 
  ;; The order has to be set this way for the hook to work
  ;; (setq cycle-themes-theme-list '(doom-zenburn doom-flatwhite))
  (setq cycle-themes-theme-list '(doom-zenburn doom-flatwhite)))

(use-package emojify
:disabled t
    :straight (emojify :type git :host github :repo "iqbalansari/emacs-emojify") 
    :hook (after-init . global-emojify-mode-line-mode))

(use-package hl-line 
  :straight nil
  :disabled t
  :hook ((text-mode conf-mode prog-mode) . hl-line-mode)
  :custom
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

(use-package el-patch
   :straight (el-patch :type git
                       :host github
                       :repo "raxod502/el-patch")
   :disabled t)

 ;; (eval-when-compile
 ;;   (require 'el-patch))

 (use-package deft
   :straight (deft :type git :host github :repo "jrblevin/deft") 
   :after org
   :commands deft
   :custom
   (deft-recursive t)
   (deft-use-filter-string-for-filename t)
   (deft-default-extension "org")
   (deft-directory sync0-zkn-dir-sans)
   (deft-new-file-format "%Y%m%d%H%M%S")
   :config
(require 'sync0-deft))

(use-package smooth-scrolling 
  :straight (smooth-scrolling :type git :host github :repo "aspiers/smooth-scrolling") 
  :commands (sync0-scroll-up sync0-scroll-down)
  :custom
  (smooth-scroll-margin 5)
  ;; prevent ugly jumps when cursor is near the end of the screen
  (scroll-conservatively 101)
  :preface
  (defun sync0-scroll-up ()
    "Improve scroll up behavior"
    (interactive)
    (scroll-down 1))

  (defun sync0-scroll-down ()
    "Improve scroll down behavior"
    (interactive)
    (scroll-up 1))

  (defun sync0-scroll-right ()
    "Improve scroll down behavior"
    (interactive)
    (scroll-right 1))

  (defun sync0-scroll-left ()
    "Improve scroll down behavior"
    (interactive)
    (scroll-left 1))

  :config (smooth-scrolling-mode 1)
  :bind (("M-k" . sync0-scroll-up)
         ("M-h" . sync0-scroll-right)
         ("M-l" . sync0-scroll-left)
         ("M-j" . sync0-scroll-down)))

(use-package warnings
    :straight nil
    :config
;; Remove annoying message when expanding yasnippets. 
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package google-this 
  :straight (google-this :type git :host github :repo "Malabarba/emacs-google-this") 
  :commands (google-this-search google-this)
  :bind 
         (:map evil-visual-state-map ("g"  . google-this)))

(use-package mu4e
       :commands mu4e
       :init
       (require 'smtpmail)
       ;; (require 'org-mu4e)
       :custom
       (user-full-name "Carlos Alberto Rivera Carreño")
       (mu4e-root-maildir "~/Mail")
       (mu4e-attachment-dir "~/Downloads")
       (message-signature-file "~/.emacs.d/sync0/.sync0_signature") 
       (mu4e-compose-signature-auto-include t)
       ;; get mail
       ;; (mu4e-get-mail-command "mbsync -V -c ~/.emacs.d/sync0/.mbsyncrc -a")
       (mu4e-get-mail-command "mbsync -Va -c ~/.emacs.d/sync0/.mbsyncrc")
       (mu4e-update-interval nil)
       ;; show images
       (mu4e-show-images t)
       (mu4e-view-show-images t)
       (mu4e-view-show-addresses t)
       (mu4e-headers-auto-update t)
       (mu4e-use-fancy-chars t)
       ;; This allows me to use 'ivy' to select mailboxes
       (mu4e-completing-read-function 'ivy-completing-read)
       ;; Don't ask for a 'context' upon opening mu4e
       (mu4e-context-policy 'pick-first)
      (mu4e-compose-context-policy nil)
       ;; don't save message to Sent Messages, IMAP takes care of this
       ;; GMail already adds sent mail to the Sent Mail folder.
       (mu4e-sent-messages-behavior 'delete)
       ;; Don't ask to quit... why is this the default?
       (mu4e-confirm-quit nil)
       ;; Why would I want to leave my message open after I've sent it?
       (message-kill-buffer-on-exit t)
       ;; Rename files when moving
       (mu4e-change-filenames-when-moving t)
       (mu4e-headers-include-related t)
       (mu4e-headers-skip-duplicates t)
       ;; Needed for mbsync
       ;; Configure smtpmail
       (message-send-mail-function 'smtpmail-send-it)
       ;; (starttls-use-gnutls t)
       (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
       (smtpmail-auth-credentials "~/.authinfo.gpg")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-debug-info t)

       :config
(require 'mu4e-context)
(require 'sync0-mu4e)

       :bind  (( 
                :map mu4e-main-mode-map
                ("J" . mu4e~headers-jump-to-maildir)
                ("j" . next-line)
                ("k" . previous-line)
                ("u" . mu4e-update-mail-and-index)
                ("b" . mu4e-headers-search-bookmark)
                ("B" . mu4e-headers-search-bookmark-edit)
                ("N" . mu4e-news)
                (";" . mu4e-context-switch)
                ("H" . mu4e-display-manual)
                ("C" . mu4e-compose-new)
                ;; ("cc" . mu4e-compose-new)
                ("x" . mu4e-kill-update-mail)
                ("A" . mu4e-about)
                ("f" . smtpmail-send-queued-mail)
                ("m" . mu4e~main-toggle-mail-sending-mode)
                ("s" . mu4e-headers-search)
                ("q" . mu4e-quit)
                :map mu4e-headers-mode-map
                ("q" . mu4e~headers-quit-buffer)
                ("J" . mu4e~headers-jump-to-maildir)
                ("C" . mu4e-compose-new)
                ("E" . mu4e-compose-edit)
                ("F" . mu4e-compose-forward)
                ("R" . mu4e-compose-reply)
                ("o" .   mu4e-headers-change-sorting)
                ("j" . mu4e-headers-next)
                ("k" . mu4e-headers-prev)
                ("b" . mu4e-headers-search-bookmark)
                ("B" . mu4e-headers-search-bookmark-edit)
                (";" . mu4e-context-switch)
                ("/" . mu4e-headers-search-narrow)
                ("s" . mu4e-headers-search)
                ("S" . mu4e-headers-search-edit)
                ("x" . mu4e-mark-execute-all)
                ("a" . mu4e-headers-action)
                ("*" . mu4e-headers-mark-for-something) 
                ("&" . mu4e-headers-mark-custom)
                ("A" . mu4e-headers-mark-for-action)
                ("m" . mu4e-headers-mark-for-move)
                ("r" . mu4e-headers-mark-for-refile)
                ("D" . mu4e-headers-mark-for-delete)
                ("d" . mu4e-headers-mark-for-trash)
                ("=" . mu4e-headers-mark-for-untrash)
                ("u" . mu4e-headers-mark-for-unmark)
                ("U" . mu4e-mark-unmark-all)
                ("?" . mu4e-headers-mark-for-unread)
                ("!" . mu4e-headers-mark-for-read)
                ("%" . mu4e-headers-mark-pattern)
                ("+" . mu4e-headers-mark-for-flag)
                ("-" . mu4e-headers-mark-for-unflag)
                ("[" . mu4e-headers-prev-unread)
                ("]" . mu4e-headers-next-unread)
                ("C-j" . mu4e-headers-next)
                ("C-k" . mu4e-headers-prev)
                :map mu4e-view-mode-map
                ("j" . next-line)
                ("k" . previous-line)
                ("l" . evil-forward-char)
                ("h" . evil-backward-char)
                ("v" . evil-visual-char)
                ("$" . evil-end-of-visual-line)
                ("^" . evil-beginning-of-visual-line)
                ("]" . evil-next-visual-line)
                ("[" . evil-previous-visual-line)
                (" " . mu4e-view-scroll-up-or-next)
                ([tab] . shr-next-link)
                ([backtab] . shr-previous-link)
                ("q" . mu4e~view-quit-buffer)
                ("C" . mu4e-compose-new)
                ("H" . mu4e-view-toggle-html)
                ("R" . mu4e-compose-reply)
                ("p" . mu4e-view-save-attachment)
                ("P" . mu4e-view-save-attachment-multi) 
                ("O" . mu4e-headers-change-sorting)
                ("o" . mu4e-view-open-attachment)
                ("A" . mu4e-view-attachment-action)
                ("a" . mu4e-view-action)
                ("J" . mu4e~headers-jump-to-maildir)
                ("C-j" . mu4e-view-headers-next)
                ("C-k" . mu4e-view-headers-prev)
                ("x" . mu4e-view-marked-execute)
                ("&" . mu4e-view-mark-custom)
                ("*" . mu4e-view-mark-for-something)   
                ("m" . mu4e-view-mark-for-move)
                ("r" . mu4e-view-mark-for-refile)
                ("D" . mu4e-view-mark-for-delete)
                ("d" . mu4e-view-mark-for-trash)
                ("=" . mu4e-view-mark-for-untrash)
                ("u" . mu4e-view-unmark)
                ("U" . mu4e-view-unmark-all)
                ("?" . mu4e-view-mark-for-unread)
                ("!" . mu4e-view-mark-for-read)
                ("%" . mu4e-view-mark-pattern)
                ("+" . mu4e-view-mark-for-flag)
                ("-" . mu4e-view-mark-for-unflag)
                ("s" . mu4e-view-search-edit)
                ("|" . mu4e-view-pipe)
                ("." . mu4e-view-raw-message)
                ("C--" . mu4e-headers-split-view-shrink)
                ("C-+" . mu4e-headers-split-view-grow))))

(use-package calendar 
  :custom
  (calendar-date-style 'european) 
  (european-calendar-style t)
  ;; Week starts on monday.
  (calendar-week-start-day 0)    
  (calendar-day-name-array     ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
  (calendar-day-abbrev-array   ["Sun." "Mon." "Tue." "Wed." "Thu." "Fri." "Sat."])
  (calendar-month-name-array   ["January" "February" "March" "April" "May" "June" "July"
                                "August" "September" "October" "November" "December"])
  (calendar-month-abbrev-array ["Jan." "Feb." "Mar." "Avr." "May" "Jun." "Jul." "Aug" "Sep." "Oct." "Nov." "Dec."]))

(use-package cal-korea-x
  :defer t
  :straight (cal-korea-x :type git :host github :repo "cinsk/cal-korea-x")
  :custom
   (cal-korea-x-use-korean-month-name nil))

(use-package holidays 
     :straight nil
     :after calendar
     :custom
     (holiday-christian-holidays nil)
     (holiday-hebrew-holidays nil)
     (holiday-islamic-holidays nil)
     (holiday-bahai-holidays nil)
     (holiday-oriental-holidays nil)
     :config
(require 'sync0-holidays))

(use-package calfw-org
  :after calfw
  :straight (calfw-org :type git :host github :repo "kiwanami/emacs-calfw"))  

(use-package calfw 
  :straight (calfw :type git :host github :repo "kiwanami/emacs-calfw") 
  :custom
  (cfw:fchar-junction ?╋)
  (cfw:fchar-vertical-line ?┃)
  (cfw:fchar-horizontal-line ?━)
  (cfw:fchar-left-junction ?┣)
  (cfw:fchar-right-junction ?┫)
  (cfw:fchar-top-junction ?┯)
  (cfw:fchar-top-left-corner ?┏)
  (cfw:fchar-top-right-corner ?┓)

  :config 
  (require 'calfw-org)
  ;; :bind (:map cfw:details-mode-map
  ;;        ("SPC"  . cfw:details-kill-buffer-command))

(evil-leader/set-key
  "Q" 'cfw:open-org-calendar))

  ;; (defun sync0-open-calendar ()
  ;;   (interactive)
  ;;   (let ((buf (get-buffer "*cfw-calendar*")))
  ;;     (if buf
  ;;         (pop-to-buffer buf nil)
  ;;       (cfw:open-calendar-buffer
  ;;        :contents-sources
  ;;        (list (cfw:org-create-source "#c0c5ce")) :view 'week))))


  ;; Redefinition
  ;; (eval-after-load "calfw-org"
  ;;   '(defun cfw:org-collect-schedules-period (begin end)
  ;;      "[internal] Return org schedule items between BEGIN and END."
  ;;      (let ((org-agenda-prefix-format " ")
  ;;            (span 'day))
  ;;        (setq org-agenda-buffer
  ;;              (when (buffer-live-p org-agenda-buffer)
  ;;                org-agenda-buffer))
  ;;        (org-compile-prefix-format nil)
  ;;        (loop for date in (cfw:enumerate-days begin end) append
  ;;              (loop for file in sync0-org-agenda-files 
  ;;                    append
  ;;                    (progn
  ;;                      (org-check-agenda-file file)
  ;;                      (apply 'org-agenda-get-day-entries
  ;;                             file date
  ;;                             cfw:org-agenda-schedule-args)))))))


  ;; :bind (:map cfw:details-mode-map
  ;;        ("SPC"  . cfw:details-kill-buffer-command))

(use-package magit
  :straight (magit :type git :host github :repo "magit/magit") 
  :commands (magit-status magit-blame)
  :custom
  (magit-branch-arguments nil)
  (magit-push-always-verify nil)
  ;; Get rid of the previous advice to go into fullscreen
  (magit-restore-window-configuration t)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

(use-package magithub
  :disabled t
  :straight (magithub :type git :host github :repo "vermiculus/magithub") 
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/github"))

(use-package magit-todos
  :disabled t
  :straight (magit-todos :type git :host github :repo "alphapapa/magit-todos") 
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :custom 
  (magit-todos-keywords-list (list "無" "次" "中" "待"))
  (magit-todos-recursive t)
  (magit-todos-depth 100))

(use-package git-gutter 
      :straight (git-gutter :type git :host github :repo "emacsorphanage/git-gutter") 
      :commands git-gutter-mode
      :custom
      (git-gutter:hide-gutter nil)
      (git-gutter:window-width 1)
      (git-gutter:modified-sign " ") 
      (git-gutter:added-sign " ")    
      (git-gutter:deleted-sign " ")

      :custom-face
      (git-gutter:modified ((t (:background "#3a81c3"))))
      (git-gutter:added    ((t (:background "#7ccd7c"))))
      (git-gutter:deleted  ((t (:background "ee6363"))))

      :config
  (setq git-gutter:disabled-modes '(asm-mode image-mode mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-compose-mode))

      (defhydra sync0-hydra-git-gutter
        (:body-pre (git-gutter-mode 1) :hint nil)
        "
                                                                   ╭─────────────────┐
                                Movement   Hunk Actions     Misc.  │ gg: +%-4s(car (git-gutter:statistic))/ -%-3s(cdr (git-gutter:statistic)) │
                                ╭──────────────────────────────────┴─────────────────╯
                                   ^_g_^       [_s_] stage        [_R_] set start Rev
                                   ^_k_^       [_r_] revert
                                   ^↑ ^      [_m_] mark
                                   ^↓ ^      [_p_] popup          ╭──────────────────────
                                   ^_j_^                          │[_q_] quit
                                   ^_G_^                          │[_Q_] Quit and disable"
        ("j" (progn (git-gutter:next-hunk 1) (recenter)))
        ("k" (progn (git-gutter:previous-hunk 1) (recenter)))
        ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
        ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
        ("s" git-gutter:stage-hunk)
        ("r" git-gutter:revert-hunk)
        ("m" git-gutter:mark-hunk)
        ("p" git-gutter:popup-hunk)
        ("R" git-gutter:set-start-revision)
        ("q" nil :color blue)
        ("Q" (git-gutter-mode -1) :color blue))

(evil-leader/set-key
  "G" 'sync0-hydra-git-gutter/body))

(use-package git-timemachine
:straight (git-timemachine :type git :host gitlab :repo "pidu/git-timemachine") 
    :defer t
    :commands 
    (git-timemachine git-timemachine-toggle)
    :custom
    (git-timemachine-show-minibuffer-details nil)
    :config
    (require 'magit-blame)

    ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
    ;; showing revision details in the minibuffer, show them in
    ;; `header-line-format', which has better visibility.

    ;; (add-hook 'git-timemachine-mode-hook #'+vcs|init-header-line)
    ;; (advice-add #'git-timemachine-show-revision :after #'+vcs*update-header-line)

    ;; Force evil to rehash keybindings for the current state
    (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state))

(use-package ediff
:straight nil
    :defer t
    :custom
    ;; No separate frame for ediff control buffer
    (ediff-window-setup-function #'ediff-setup-windows-plain)
    ;; Split windows horizontally in ediff (instead of vertically)
    (ediff-split-window-function #'split-window-vertically))

(use-package org-id
    :straight nil
    :custom
(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
:init
    (require 'find-lisp)
    :config
;; Update ID file on startup
(org-id-update-id-locations))

(use-package org-journal 
    :straight (org-journal :type git :host github :repo "bastibe/org-journal") 
    :custom
    ;; Set default directory to search for journal files. 
    ;;(org-journal-dir (concat sync0-dropbox-dir "org"))
    (org-journal-dir (concat sync0-dropbox-dir "org/journal"))
    ;; Delete the date prefix to new journal entries.
    (org-journal-time-format "")
    ;; Create one journal file per month. 
    ;; (org-journal-file-type 'daily)
    (org-journal-file-type 'monthly)
    ;; Change the title of journal files to the format: "YYYY_MM.gpg".
    (org-journal-file-format "%Y%m.org")
    ;; Change the format of journal entries (org headlines) to "[Day], DD/MM/YYYY".
    ;; (org-journal-date-format "%A, %Y/%m/%d")
    (org-journal-date-format "%A, %Y/%m/%d")
    ;; Encrypt journal files.
    (org-journal-encrypt-journal nil)
    ;; Don't encript individual entires in journal files. It's too cumbersome. 
    (org-journal-enable-encryption nil)
    (org-journal-carryover-items "TODO=\"無\"|TODO=\"次\"|TODO=\"中\"|TODO=\"待\"|TODO=\"阻\"")
    (org-journal-enable-agenda-integration nil)
    (org-journal-file-header "#+TITLE: %B, %Y\n#+FILETAGS: :journal:%Y:%B:\n\n")
    ;;(org-journal-file-header "#+TITLE: %A, %Y/%m/%d\n#+CREATED: %Y/%m/%d\n#+DATE: %Y/%m/%d\n#+ROAM_TAGS: journal %Y %B\n\n")

    :config

    (defun sync0-org-journal-new-scheduled-entry (prefix &optional scheduled-time)
      "Create a new entry in the future."
      (interactive "P")
      (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "%Y-%m-%d %a")))
            (raw (prefix-numeric-value prefix)))
        (org-journal-new-entry (= raw 16) (org-time-string-to-time scheduled-time))
        (unless (= raw 16)
          (if (not prefix)
              (insert "")))))

(defhydra sync0-hydra-org-journal (:color amaranth :hint nil :exit t)
  "
   ^Journaling functions^
   ^---------------
   _t_oday's note
   other date            
   previous note
   next note

   _q_uit
        "
  ("t" sync0-org-journal-new-scheduled-entry)
  ("p" org-roam-dailies-find-previous-note)
  ("n" org-roam-dailies-find-next-note)
  ("o" org-roam-dailies-find-date)
  ("q" nil :color blue))

(evil-leader/set-key
  "J" 'sync0-hydra-org-journal/body)

    :bind (("C-c j" . sync0-org-journal-new-scheduled-entry)
           :map org-journal-mode-map
           ("C-c C-s" . org-schedule)))

(use-package org-agenda 
     :straight nil
     :after org
     ;; :after (org all-the-icons)
     ;;  :commands       (sync0-pop-to-org-agenda org-agenda)
     :custom
     (org-agenda-todo-keyword-format "%-1s ")
     (org-agenda-include-diary t)
     (org-agenda-inhibit-startup t)
     (org-agenda-dim-blocked-tasks nil)
     (org-cycle-separator-lines 0)
     ;; Choose the placement of org tags in org files.
     (org-tags-column 80)
     ;; Place org agenda tags in the same place as org tags.
     (org-agenda-tags-column 0)
     ;; Make org-agenda the only window by default.
     (org-agenda-window-setup 'only-window)
     (org-agenda-block-separator (string-to-char " "))
     ;; Build agenda manually (to update press "r").
     (org-agenda-sticky t)
     ;; Compact the block agenda view. This deletes the section separators.
     (org-agenda-compact-blocks nil)
     ;; Allow one-key todo selection.
     (org-use-fast-todo-selection t)
     ;; Include the todo keywords in fast tag selection buffer.
     (org-fast-tag-selection-include-todo t)
     ;; Allow one-key tag selection.
     (org-fast-tag-selection-single-key t)
     ;; each habit to show up when it is next scheduled, but no further repetitions
     (org-agenda-repeating-timestamp-show-all nil)
     ;; This variable may be set to nil, t, or a number which will then
     ;; give the number of days before the actual deadline when the
     ;; prewarnings should resume.
     ;; (org-agenda-skip-deadline-prewarning-if-scheduled 'post-deadline)
     (org-agenda-skip-scheduled-if-deadline-is-shown t)
     ;; (org-agenda-skip-scheduled-if-deadline-is-shown t)
     ;; Add appointments duration to column view's effort estimates.
     (org-agenda-columns-add-appointments-to-effort-sum t)
     (org-agenda-ignore-drawer-properties '(effort appt category))
     (org-agenda-deadline-leaders (quote ("今日" "%-1d日後" "%-1d日前")))
     (org-agenda-scheduled-leaders (quote ("今日" "以前")))

     :config
;; Set org-agenda files
(setq  org-agenda-files (list "~/Dropbox/org/todo/"
                              "~/Dropbox/org/etc/Gcal.org"
                              "~/Dropbox/org/etc/Events.org"
                              "~/Dropbox/org/etc/Habits.org"
                              "~/Dropbox/org/etc/Classes.org"))

     (require 'cal-iso)
     (require 'sync0-org-agenda-functions)
     (require 'sync0-org-agenda)

     ;; workaround developed by some smart user to circumvent
     ;; org-agenda's slow performance (run-with-idle-timer 5 nil
     ;; (lambda () (org-agenda-list) (delete-window)))

(evil-leader/set-key
  "A" 'org-agenda
  "a" 'sync0-pop-to-org-agenda)

     :bind 
     (([f6] . sync0-pop-to-org-agenda)
      :map org-agenda-mode-map
      ("S" . org-agenda-schedule)
      ("D" . org-agenda-deadline)
      ("j" . org-agenda-next-item)
      ("k" . org-agenda-previous-item)
      ("J" . sync0-org-agenda-next-header)
      ("K" . sync0-org-agenda-previous-header)
      ("N" . sync0-org-agenda-new)))

(use-package org-expiry 
  :straight nil
  :load-path "~/.emacs.d/sync0/org-expiry.el" 
  :after org
  :custom
  ;; Name of property when an item is created
    (org-expiry-created-property-name "CREATED") 
  ;; Don't have everything in the agenda view
    (org-expiry-inactive-timestamps   t)         
  :config
  (defun sync0-insert-created-timestamp()
    "Insert a CREATED property using org-expiry.el for TODO entries"
    (org-expiry-insert-created)
    (org-back-to-heading)
    (org-end-of-line)
    (insert " "))

(add-hook 'org-insert-todo-heading-hook #'org-expiry-insert-created))

(use-package emms)

(use-package org-emms
:after emms
:commands (org-emms-insert-track
           org-emms-insert-track-position))

(use-package org-fc
:straight (org-fc :type git :host github :repo "l3kn/org-fc" :files (:defaults "awk" "demo.org" "contrib/*.el")) 
:commands (org-fc-hydra/body
           org-fc-review
           org-fc-review-all)
:custom
(org-fc-directories sync0-zkn-dir)
:config
(require 'org-fc-hydra)

(defhydra sync0-hydra-org-fc-functions (:color amaranth :hint nil :exit t)
  "
   ^Flip^       ^Rate^       ^Create card^
   ^--------------------------------------
   _f_lip       _e_asy      _d_ouble (no back)
   _s_uspend    _g_ood      _n_ormal
   sto_p_       _h_ard      _t_ext input
   ^ ^          _a_gain     _c_loze
   _q_uit
        "

  ("f" org-fc-review-flip)
  ("s" org-fc-review-suspend-card)
  ("p" org-fc-review-quit)
  ("a" org-fc-review-rate-again)
  ("h" org-fc-review-rate-hard)
  ("g" org-fc-review-rate-good)
  ("e" org-fc-review-rate-easy)
  ("n" org-fc-type-normal-init)
  ("d" org-fc-type-double-init)
  ("t" org-fc-type-text-input-init)
  ("c" org-fc-type-cloze-init)
  ("q" nil :color blue))

(evil-leader/set-key-for-mode 'org-mode "t" 'sync0-hydra-org-fc-functions/body)

)

(server-start)

(use-package org-protocol
:after org
:straight nil)

(use-package org-ref
    :straight (org-ref :type git :host github :repo "jkitchin/org-ref") 
    :custom
    (reftex-default-bibliography '("~/Dropbox/bibliographies/bibliography.bib"
                                   "~/Dropbox/bibliographies/doctorat.bib"))
    (org-ref-default-bibliography reftex-default-bibliography)
    (org-ref-pdf-directory sync0-zkn-attachments-dir)
    (org-ref-completion-library 'org-ref-ivy-cite)
    (org-ref-open-pdf-function 'sync0-org-ref-open-pdf-at-point)

    :config
   (require 'doi-utils)
   (require 'bibtex-completion)
   (require 'sync0-org-ref-functions)

(ivy-set-display-transformer
 'org-ref-ivy-insert-cite-link
 'ivy-bibtex-display-transformer)

    :bind 
    (:map org-mode-map
          ("C-c [" . org-ref-ivy-insert-cite-link)))

;; (use-package org-roam
;;   ;; :after evil-leader
;;   :straight (org-roam :type git :host github :repo "org-roam/org-roam") 
;;   :init 
;;   (require 'org-id)
;;   :custom
;;   (org-roam-directory "~/Dropbox/org/")
;;   (org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))
;;   (org-roam-file-extensions '("org"))
;; ;; disable warning
;;   (org-roam-v2-ack t) 
;; ;; exclude useless files from my org directory 
;;   (org-roam-file-exclude-regexp "etc/[[:graph:]]+.org")
;;   :config
;; ;;   (setq org-roam-directory "~/Dropbox/org/")
;; ;;   (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))
;; ;;   (setq org-roam-file-extensions '("org"))
;; ;; ;; disable warning
;; ;;   (setq org-roam-v2-ack t) 
;; ;; ;; exclude useless files from my org directory 
;; ;;   (setq org-roam-file-exclude-regexp "etc/[[:graph:]]+.org")

;;   (org-roam-setup)

;;   (require 'org-ref)
;;   (require 'org-emms)
;;   (require 'deft)
;;   (require 'sync0-org-roam-functions)

;;   (cl-defmethod org-roam-node-zettel-type ((node org-roam-node))
;;      (cdr
;;       (assoc "ZETTEL_TYPE" (org-roam-node-properties node)))) 

;;   (cl-defmethod org-roam-node-fiche-type ((node org-roam-node))
;;      (cdr
;;       (assoc "FICHE_TYPE" (org-roam-node-properties node)))) 

;;   (cl-defmethod org-roam-node-zettel-function ((node org-roam-node))
;;      (cdr
;;       (assoc "ZETTEL_FUNCTION" (org-roam-node-properties node)))) 

;;   (cl-defmethod org-roam-node-biblatex-type ((node org-roam-node))
;;      (cdr
;;       (assoc "BIBLATEX_TYPE" (org-roam-node-properties node)))) 

;;   ;; (cl-defmethod org-roam-node-creation-date ((node org-roam-node))
;;   ;;    (cdr
;;   ;;     (assoc "CREATED" (org-roam-node-properties node)))) 

;; ;; (setq org-roam-node-display-template "${title:80}  | ${zettel-type:4}:${biblatex-type}${fiche-type}${zettel-function} | ${tags}")

;; (setq org-roam-node-display-template "${title:80}  ${tags:50} ${zettel-type} : ${biblatex-type}${fiche-type}${zettel-function}")

;;   ;; add the possiblity to follow links in the org-roam buffer
;;   (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)

;;   (evil-leader/set-key
;;     "F" 'org-roam-node-find
;;     "B" 'org-roam-buffer-toggle
;;     "i" 'sync0-org-roam-insert
;;     "I" 'sync0-hydra-org-roam-insert/body))

;; (use-package org-roam-bibtex
;;   :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex") 
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :custom
;;       (orb-process-file-keyword t)
;;       (orb-file-field-extensions '("pdf" "epub"))
;;       ;; Use this to insert citation keys
;;       (orb-insert-link-description 'citekey)
;;       (orb-insert-interface 'ivy-bibtex)
;;       (orb-note-actions-interface 'hydra)
;;   :config

;; (setq orb-preformat-keywords
;;       '("citekey" "title" "subtitle" "booktitle" "booksubtitle" "journaltitle" "url" "author-or-editor" "keywords" "file"))
;;            )

(use-package org-pdftools
:disabled t
 :straight nil
 :config (org-pdftools-setup-link))

(use-package org-crypt 
  :straight nil
  :after org
  :custom
  (org-crypt-key "carc.sync0@gmail.com")
  :config
  (org-crypt-use-before-save-magic))

(use-package org-capture 
  :straight nil
  :after org 
  :preface 
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  :custom
  (org-default-notes-file "~/Dropbox/etc/notes.org")
  :config 
  (require 'org-ref)
  (require 'sync0-org-capture-functions)

  (evil-leader/set-key "c" 'org-capture)

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (setq org-capture-templates 
        '(("j" "Journal" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)\n\n%?"
           ;; "* %(format-time-string org-journal-time-format)\n\n%?"
           :jump-to-captured t :immediate-finish t)
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
           ;; (file+headline "~/Dropbox/org/todo/todo.org" "Autres")
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

(use-package org-protocol-capture-html
  :straight (org-protocol-capture-html :type git :host github :repo "alphapapa/org-protocol-capture-html") 
  :after (org-protocol s))

(use-package org-habit 
  :straight nil
  :after org 
  ;; :commands org-bullets-mode
  :config
  (setq org-habit-graph-column 80
        org-habit-show-habits-only-for-today nil))

(use-package org-clock 
        :straight nil
        :after org
        :custom
        ;; Set default column view headings: Task Priority Effort Clock_Summary
        (org-columns-default-format "%1PRIORITY %2TODO %DEADLINE %60ITEM(Task) %5EFFORT(Effort){:} %5CLOCKSUM")
        (org-agenda-clockreport-parameter-plist
         '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))
        ;; Agenda clock report parameters
        ;; global Effort estimate values
        ;;        1    2    3    4    5    6    7    8    9    0
        ;; These are the hotkeys ^
        (org-global-properties  '(("Effort_ALL" . "1:00 2:00 4:00 5:00 8:00 10:00 12:00 15:00 20:00 24:00")))
        ;; If idle for more than 15 minutes, resolve the things by asking what to do
        ;; with the clock time
        (org-clock-idle-time 5)
        ;; Show lot of clocking history so it's easy to pick items off the `C-c I` list
        (org-clock-history-length 23)
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        (org-clock-persist 'history)
        ;; org-clock-persist t
        ;; Resume clocking task on clock-in if the clock is open
        (org-clock-in-resume t)
        ;; Do not prompt to resume an active clock, just resume it
        (org-clock-persist-query-resume nil)
        ;; Change tasks to whatever when clocking in
        (org-clock-in-switch-to-state "中")
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        (org-clock-into-drawer t)
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
        ;; with 0:00 duration
        (org-clock-out-remove-zero-time-clocks t)
        ;; Clock out when moving task to a done state
        (org-clock-out-when-done t)
        ;; Enable auto clock resolution for finding open clocks
        (org-clock-auto-clock-resolution (quote when-no-clock-is-running))
        ;; Include current clocking task in clock reports
        (org-clock-report-include-clocking-task t)
        ;; use pretty things for the clocktable
        (org-pretty-entities t)
        (org-clock-string-limit 8)

        :config
          ;; Avoid annoying space in mode line when no clock is defined.
          (add-hook 'org-clock-out-hook
                    '(lambda ()
                       (setq org-mode-line-string nil)))

        (defun sync0-org-clock-in ()
          (interactive)
          (org-clock-in '(4)))

        ;; This function was taken from Sacha Chua's configuration.
        ;; Display words typed and minutes spent in an org subtree.
        (defun sync0-org-entry-word-count ()
          (interactive)
          (save-restriction
            (save-excursion
              (org-narrow-to-subtree)
              (goto-char (point-min))
              (let* ((words (count-words-region (point-min) (point-max)))
                     (minutes (org-clock-sum-current-item))
                     (wpm (/ words minutes)))
                (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
                (kill-new (number-to-string wpm))))))

        ;; Resume clocking task when emacs is restarted
        (org-clock-persistence-insinuate)

     (defhydra sync0-hydra-org-clock (:color blue :hint nil)
"
^Clock functions^   
^--------------------
^In/out^    ^Edit^    ^Summary (_?_)
^----------------------------------------- 
_i_n        _e_dit    _g_oto entry 
_c_ontinue  _Q_uit    _d_isplay 
_o_ut       ^ ^       _r_eport 
            ^ ^       _w_ord count

_q_uit
"
       ("i" sync0-org-clock-in)
       ("c" org-clock-in-last)
       ("o" org-clock-out)
       ("e" org-clock-modify-effort-estimate)
       ("Q" org-clock-cancel)
       ("g" org-clock-goto)
       ("d" org-clock-display)
       ("r" org-clock-report)
       ("w" sync0-org-entry-word-count)
       ("?" (org-info "Clocking commands"))
       ("q" nil :color blue))

    (evil-leader/set-key
      "t" 'sync0-hydra-org-clock/body))

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
     (org-export-with-tasks '("來" "完" "未" "中" "待" "見"))
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

(use-package ox-epub
  :straight (ox-epub :type git :host github :repo "ofosos/ox-epub")
  :after org)

(use-package org-bullets 
  :straight (org-bullets :type git :host github :repo "sabof/org-bullets") 
  :custom
  ;; Hide all bullets:
  (org-bullets-bullet-list '(" ")))

(use-package org-mu4e 
  :disabled t
  :after mu4e
  :straight nil
  :custom
  ;; Store link to message if in header view, not to header query.
  (org-mu4e-link-query-in-headers-mode nil))

(use-package nov
    :straight nil
    :after org-noter
    :load-path "~/.emacs.d/sync0/nov.el" 
;; :custom
;; (nov-text-width 66)
    :config
      (push '("\\.epub\\'" . nov-mode) auto-mode-alist)

       (evil-define-key 'normal nov-mode-map
    "r" 'nov-render-document
   ;; "S" 'nov-view-content-source
    ;; "g?" 'nov-display-metadata
    "J" 'nov-next-document
    "K" 'nov-previous-document
    "T" 'nov-goto-toc
    "i" 'org-noter-insert-note
    "I" 'org-noter-insert-precise-note
      )

   (defun sync0-nov-font-setup ()
      (if (> (display-pixel-width) 1900)
      ;; high resolution (t14s)
    (progn
      (face-remap-add-relative 'variable-pitch
                               :family "Minion Pro"
                               ;; :height 200
                               ;; :height 220
                               :height 250)

        (setq nov-text-width 66)
        (nov-render-document))
      ;; low resolution 
    (progn
      (face-remap-add-relative 'variable-pitch
                               :family "Minion Pro"
                               ;; :height 200
                               ;; :height 155
                               ;; :height 130
                               :height 150)
        (setq nov-text-width 60)
        (nov-render-document))))

    (add-hook 'nov-mode-hook 'sync0-nov-font-setup))

    (use-package esxml
    :straight (esxml :type git :host github :repo "tali713/esxml"))

    (use-package org-noter
      :straight (org-noter :type git :host github :repo "weirdNox/org-noter") 
      :after (:any org pdf-view)
      :config
      (setq
       ;; The WM can handle splits
       org-noter-notes-window-location 'horizontal-split
       ;; Please stop opening frames
       org-noter-always-create-frame nil
       ;; I want to see the whole file
       org-noter-hide-other nil
       ;; Use interleave properties 
       org-noter-property-doc-file "INTERLEAVE_PDF"
       ;; 
       org-noter-default-heading-title (format-time-string "%Y%m%d%H%M%S")
       ;; Everything is relative to the main notes file
       org-noter-notes-search-path (list sync0-zkn-dir)))

(use-package org-download
:straight (org-download :type git :host github :repo "abo-abo/org-download") 
:after org
:hook (dired-mode . org-download-enable)
:custom
(org-download-image-dir "~/Pictures/org")
(org-download-screenshot-method "spectacle")
;; (org-download-screenshot-method "xfce4-screenshooter")

:config
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

(evil-leader/set-key
  "d" 'sync0-hydra-org-download-functions/body))

(use-package org 
             :after evil
             :custom
             (org-hide-leading-stars t)
             ;; Leave one line between headlines 
             (org-cycle-separator-lines 1)
             ;; Don't fontify the whole damn line
             (org-fontify-whole-block-delimiter-line t)
             ;; Disable word wrap in org mode.
             ;; (org-startup-truncated t)
             ;; Initial indentation
             (org-startup-indented nil)         
             ;; Necessary to avoid crazy inconsistenscies using org-download and org-roam
             (org-link-file-path-type 'absolute)
             ;; Begin displaying entire trees.
             (org-startup-folded nil)
             ;; Better display of italics & bold.
             (org-hide-emphasis-markers t)
             ;; Define org-tags.
             ;; (org-tag-alist '(("projects" . ?p)
             ;;                  ;; ("noexport" . ?n)
             ;;                  ("readings" . ?r)
             ;;                  ;; ("reviews" . ?r)
             ;;                  ("exams" . ?e)
             ;;                  ("urgent" . ?u)
             ;;                  ("this_week" . ?t)
             ;;                  ("this_month" . ?m)
             ;;                  ("next_week" . ?n)
             ;;                  ("short_term" . ?s)
             ;;                  ("long_term" . ?l)
             ;;                  ;; ("university" . ?u)
             ;;                  ("important" . ?i)))
             ;; Hide inherited tags from Org's agenda view.
             ;; org-agenda-show-inherited-tags nil
             ;; Define todo keywords.
             (org-blank-before-new-entry '((heading . nil)(plain-list-item . nil)))
             ;; Stop emacs asking for confirmation
             (org-confirm-babel-evaluate nil)
             (org-ellipsis "  ⌄ ") ;; folding symbol
             ;; Do not show export buffer.
             (org-export-show-temporary-export-buffer nil)
             ;; Set path for org default directory (necessary for refile and agenda).
             (org-directory (concat (getenv "HOME") "/Dropbox/org"))
             (org-refile-use-outline-path 'file)
             (org-outline-path-complete-in-steps nil)
             (org-startup-with-inline-images t)
             (org-refile-use-cache nil)
             ;; Have org-mode indent elisp sections.
             (org-src-tab-acts-natively nil)
             ;; Color embeded source code
             (org-src-fontify-natively t)
             (org-fontify-done-headline t) 
             (org-fontify-whole-heading-line t)
             (org-fontify-quote-and-verse-blocks t)
             ;; Don't fontify sub and superscripts.
             (org-pretty-entities-include-sub-superscripts nil)
             ;; Limit inheritance for certain tags. 
             (org-tags-exclude-from-inheritance (quote ("crypt" "ignore")))
             ;; Automatically add closing time when changing to DONE states. 
             (org-log-done 'time)

             :config 
(require 'sync0-org)
             :bind (;;("<f5>" . sync0-hydra-file-access/body)
                    ("C-x 2" . sync0-split-and-follow-horizontally)
                    ("C-x 3" . sync0-split-and-follow-vertically)
                    (:map org-mode-map
                     ("M-<return>" . sync0-org-meta-return-dwim)
                     ("M-S-<return>" . sync0-org-insert-todo-heading-dwim))))

(use-package org-gcal 
:straight (org-gcal :type git :host github :repo "kidd/org-gcal.el") 
    :after (org simple-secrets)
    :commands (org-gcal-fetch org-gcal-sync)
    :custom (org-gcal-auto-archive nil)
    :config
    (let* ((username (secret-lookup "sync0-gcal-client-id"))
           (password (secret-lookup "sync0-gcal-client-secret")))
      (setq org-gcal-client-id username)
      (setq org-gcal-client-secret password))

    ;; After learning how to use loops (cl-loop?), this function can
    ;; be rewritten in a much more concise way.
    (defun sync0-org-gcal-erase-buffers ()
     (interactive)
      "Erase buffers of calendar files"
      (let ((delete-classes (find-file-noselect "~/Dropbox/org/etc/Classes.org"))
            (delete-events (find-file-noselect "~/Dropbox/org/etc/Events.org"))
            (delete-gcal (find-file-noselect "~/Dropbox/org/etc/Gcal.org"))
            (delete-habits (find-file-noselect "~/Dropbox/org/etc/Habits.org")))
        (progn 
          (with-current-buffer delete-classes
            (erase-buffer))
          (with-current-buffer delete-events
            (erase-buffer))
          (with-current-buffer delete-gcal
            (erase-buffer))
          (with-current-buffer delete-habits
            (erase-buffer)))))

    (setq org-gcal-file-alist '(("carc.sync0@gmail.com" .  "~/Dropbox/org/etc/Gcal.org")
                                ("5iudo90h5e3nabbubvsj1lov4o@group.calendar.google.com" . "~/Dropbox/org/etc/Classes.org")
                                ("p9vu3a782nahsma6ud1rdg1qpc@group.calendar.google.com" . "~/Dropbox/org/etc/Events.org")
                                ("vbnn8eksqpqun2mbtdlknhh9uk@group.calendar.google.com" . "~/Dropbox/org/etc/Habits.org")
                                ("addressbook#contacts@group.v.calendar.google.com" . "~/Dropbox/org/etc/Birthdays.org"))))

(use-package org2blog
  :straight (org2blog :type git :host github :repo "org2blog/org2blog") 
  :after (org simple-secrets)
  :commands (org2blog-user-interface)
  :bind (("C-c b" . org2blog-user-interface))
  :custom
  (org-list-allow-alphabetical t)
  :config
  ;;    (setq load-path (cons "~/.emacs.d/org2blog/" load-path))
  ;; (require 'org2blog-autoloads)
  ;; blog setup
  ;; (require 'auth-source)
  (let* ((username (secret-lookup "sync0-blog-cybernetic-username"))
         (password (secret-lookup "sync0-blog-cybernetic-password"))
         (track-posts (list "org2blog.org" "Cahiers de révoltologie"))
         (config `(("cahiers"
                   :url "https://cyberneticrevolutionary.wordpress.com/xmlrpc.php"
                   :username ,username
                   :password ,password
                   :default-title "Penseé"
                   :track-posts ,track-posts
                   :tags-as-categories nil))))
    (setq org2blog/wp-blog-alist config)))

(use-package auto-fill
  :straight nil
  :hook 
  (text-mode . turn-on-auto-fill)
  (mu4e-compose-mode . turn-off-auto-fill)
  (mu4e-view-mode . turn-off-auto-fill)
  :preface
  ;; Configure exceptions for auto-fill mode. 
  (defun sync0-nobreak-p ()
    (and (looking-at "+[[:alnum:]]")
         (looking-back "^\\\[A-z]+{.+" (line-beginning-position))))
  ;; Define column width for auto-fill mode. 
  :custom
  (fill-column 66)
  :config
  ;; Respect de la typographie française par auto-fill mode.
  ;; (setq fill-nobreak-predicate '(fill-french-nobreak-p))
  ;; Set hook for exceptions to auto-fill-mode.
  (add-hook 'fill-nobreak-predicate #'sync0-nobreak-p))

(use-package nobreak-fade 
:straight nil
:after auto-fill 
:defer t
:load-path "~/.emacs.d/sync0/nobreak-fade.el" 
  ;; :command nobreak-fade
  :config
  (autoload 'nobreak-fade-single-letter-p "nobreak-fade")
  ;; (add-hook 'tex-mode-hook 'nobreak-fade)
  (add-hook 'fill-nobreak-predicate 'nobreak-fade-single-letter-p))

(use-package visual-line
  :straight nil
  :commands visual-line-mode
  :hook 
  ;; (mu4e-compose-mode . visual-line-mode)
  (mu4e-view-mode . visual-line-mode) 
  (mu4e-compose-mode . visual-line-mode))

;; (straight-use-package '(visual-fill-column :type git :host github :repo "joostkremers/visual-fill-column"))

  (use-package visual-fill-column
    :straight (visual-fill-column :type git :host github :repo "joostkremers/visual-fill-column")
    :commands visual-fill-column-mode
    :hook 
    (mu4e-view-mode . visual-fill-column-mode)
    (mu4e-compose-mode . visual-fill-column-mode)
    ;; (add-hook 'mu4e-view-mode-hook 'mu4e-view-fill-long-lines)
    :config (setq visual-fill-column-width 66))

(use-package abbrev
  :straight nil
  :custom
  ;; Tell Emacs where to read abbrevs.  
  (abbrev-file-name "~/.emacs.d/abbrev_defs")
  ;; Save abbrevs when files are saved.
  (save-abbrevs t)
  ;; Don't notify when abbrevs are saved.
  (save-abbrevs 'silently)
  ;; Accept ' as a word constituent. 
  (dabbrev-abbrev-char-regexp  "\\sw")
  :config 
  ;; Avoid errors when reading abbrev_defs.
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  ;; Avoid expansion character insertion. 
  ;; Use this function on a per-abbrev basis.
  ;; This is the "hook" function
  (defun dont-insert-expansion-char ()  t) 
  ;; The hook should have a "no-self-insert" property set 
  (put 'dont-insert-expansion-char 'no-self-insert t) 

  ;; Initialize abbrev-mode by default. 
  (setq-default abbrev-mode t)

  ;; Add abbrevs manually.
  (defun sync0-define-local-abbrev (name expansion)
    "Defines a new abbrev for current local abbrev table."
    (interactive "sEnter abbrev:\nsEnter expansion:")
    (when (and name expansion (not (equal name expansion)))
      (define-abbrev local-abbrev-table name expansion)
      (message "\"%s\" now expands to \"%s\" %sally"
               name expansion "loc")))

  ;; Auto-update abbrev table on save.
  (add-hook 'after-save-hook (lambda ()
                               (when (equal buffer-file-name "~/.emacs.d/abbrev_defs")
                                 (read-abbrev-file)))))

(use-package ispell
   :hook (text-mode . ispell-minor-mode)
  :custom
  ;; Save a new word to personal dictionary without asking
  (ispell-silently-savep t)
  ;; Set up hunspell dictionaries
  (ispell-hunspell-dict-paths-alist
   '(("en_US-large" "/usr/share/hunspell/en_US-large.aff")
     ("de_DE" "/usr/share/hunspell/de_DE.aff")
     ("it_IT" "/usr/share/hunspell/it_IT.aff")
     ("es" "/usr/share/hunspell/es.aff")
     ("pt_BR" "/usr/share/hunspell/pt_BR.aff")
     ("fr_FR" "/usr/share/hunspell/fr_FR.aff")))
  :config 
  ;; if hunspell does NOT exist, use aspell
  (cond ((executable-find "hunspell")
         (setq ispell-program-name "hunspell")
         ;;(setq ispell-local-dictionary "en_US")
         (setq ispell-local-dictionary-alist '(("en_US-large" "[[:alpha:]]" "[^[:alpha:]]" "['’-]" t ("-d" "en_US-large" ) nil utf-8)
                                               ("de_DE" "[[:alpha:]ÄÖÜéäöüß]" "[^[:alpha:]ÄÖÜéäöüß]" "['’-]" t ("-d" "de_DE") nil utf-8)
                                               ("es" "[[:alpha:]ÁÉÍÓÚÄËÏÖÜÑáéíóúäëïöüñ]" "[^[:alpha:]ÁÉÍÓÚÄËÏÖÜÑáéíóúäëïöüñ]" "['’-]" t ("-d" "es") nil utf-8)
                                               ("pt_BR" "[[:alpha:]a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]" "[^[:alpha:]a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]" "['-]" t  ("-d" "pt_BR") nil utf-8)
                                               ("it_IT" "[[:alpha:]AEÉIOUàèéìòù]" "[^[:alpha:]AEÉIOUàèéìòù]" "['’-]" t ("-d" "it_IT") nil utf-8)
                                               ("fr_FR" "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[’'-]" t ("-d" "fr_FR")  nil utf-8))))

        ((executable-find "aspell")
         (setq ispell-program-name "aspell")
         ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
         (setq ispell-extra-args '("--sug-mode=ultra"))))

  ;; This functions was borrowed from Artur Malabarba. See his discussion
  ;; here:
  ;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

  ;; Ignore sections of files for spellcheck
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXEMPLE"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_equation" . "#\\+END_equation"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_labeling" . "#\\+END_labeling"))
  (add-to-list 'ispell-skip-region-alist '("#\\+[A-z]+: .+$"))
  (add-to-list 'ispell-skip-region-alist '("\\[\\[" . "\\]\\]"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_equation*" . "#\\+END_equation*"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_align" . "#\\+END_align"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_align*" . "#\\+END_align*"))
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("\\$" . "\\$")))

(use-package flyspell 
  :diminish flyspell-mode
  :hook (text-mode . flyspell-mode)
  :custom
  (ispell-parser 'tex)
  (flyspell-issue-message-flag nil))

(use-package guess-language
  :straight (guess-language :type git :host github :repo "tmalsburg/guess-language.el") 
  :after ispell 
  :hook (text-mode . guess-language-mode)
  :init
  (set-input-method nil)

  (defvar sync0-language-active 'english
    "Currently active natural language")

  :custom
  (guess-language-languages '(en it pt de fr es))
  (guess-language-min-paragraph-length 30)
  (guess-language-langcodes
   '((en . ("en_US-large" "english"))
     (it . ("it_IT" "italian"))
     (pt . ("pt_BR" "portuguese"))
     (de . ("de_DE" "german"))
     (fr . ("fr_FR" "french"))
     (es . ("es" "spanish"))))

  :config 
  (require 'sync0-guess-language)
  :bind (("M-i" . sync0-ispell-word-then-abbrev)))

(use-package festival 
  :straight nil
  :defer t
  :load-path "~/.emacs.d/sync0/" 
  :commands say-minor-mode
  :config
  (autoload 'say-minor-mode "festival" "Menu for using Festival." t)
  ;; (say-minor-mode t)

  (defun sync0-festival-el () 
    (interactive)
    (festival-send-command '(voice_el_diphone)))

  (defun sync0-festival-english-male () 
    (interactive)
    (festival-send-command '(voice_nitech_us_awb_arctic_hts)))

  (defun sync0-festival-english-female () 
    (interactive)
    (festival-send-command '(voice_nitech_us_slt_arctic_hts)))

  :bind (:map evil-visual-state-map 
        ("s" . festival-say-region)))

(use-package focus
  :straight (focus :type git :host github :repo "larstvei/Focus") 
  :commands focus-mode)

(use-package centered-window
                    :straight (centered-window :type git :host github :repo "anler/centered-window-mode") 
                    :config

                (defun sync0-text-mode-centered-window ()
      "Set font to a variable width (proportional) fonts in current buffer"
      (if (> (display-pixel-width) 1900)
      ;; high resolution (t14s)
        (progn
              ;; (setq cwm-left-fringe-ratio 80)
              (setq cwm-left-fringe-ratio 50)
            (centered-window-mode t))
      ;; low resolution 
        (progn
              (setq cwm-left-fringe-ratio 100)
            (centered-window-mode t))))

                (defun sync0-prog-mode-centered-window ()
                 (progn
            ;; Ratio by which the left fringe is padded more than the right.
            ;; Should be a value between 0 and 100
            (setq cwm-left-fringe-ratio 30)
            (centered-window-mode t)))

                    :hook 
            ((text-mode . sync0-text-mode-centered-window)
             ;; (mu4e-compose-mode . sync0-text-mode-centered-window)
             ;; (mu4e-view-mode . sync0-text-mode-centered-window)
             (prog-mode . sync0-prog-mode-centered-window)))

(use-package follow-mode
 :straight nil
 :commands follow-mode
 :custom (follow-auto t)
 :bind ("C-c f" . follow-delete-other-windows-and-split))

(use-package rainbow-delimiters
  :straight (rainbow-delimiters :type git :host github :repo "Fanael/rainbow-delimiters") 
  :hook 
  ((text-mode . rainbow-delimiters-mode)
   (prog-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 3))

(use-package paren
 :straight nil
:after evil
:custom
   (show-paren-delay 0.1)
         (show-paren-highlight-openparen t)
         ;; don't blink--too distracting
         (blink-matching-paren nil)
         (show-paren-when-point-inside-paren t)
:config
   (show-paren-mode 1))

(use-package smartparens
    :straight (smartparens :type git :host github :repo "Fuco1/smartparens") 
    :after evil
    :hook 
    ((emacs-startup . smartparens-global-mode)
      (emacs-startup . show-smartparens-global-mode)
     ;; Disable smartparens in evil-mode's replace state; they conflict.
     (evil-replace-state-entry-hook . turn-off-smartparens-mode)
     (evil-replace-state-exit-hook  . turn-on-smartparens-mode))
    :custom
    (sp-autowrap-region nil) ; let evil-surround handle this
    (sp-highlight-pair-overlay nil)
    (sp-cancel-autoskip-on-backward-movement nil)
    (sp-show-pair-delay 0)
    (sp-max-pair-length 3)
    :config
    (require 'smartparens-config)
    (require 'smartparens-latex)

;; Make org-mode handle latex quotes without too much hassle
(sp-local-pair 'org-mode "``" "''"
                 ;;:trigger nil
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-skip-double-quote))

;; Make org-mode handle latex quotes without too much hassle
(sp-local-pair 'org-mode "`" "'"
                 ;; :trigger nil
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-skip-double-quote))

;; Do not complete the single quote pair at the end of words.
;; Othwersie, the apostrophe in English becomes caotic.
(sp-local-pair 'org-mode "'" "'"
                 ;; :trigger nil
                 :unless '(sp-point-after-word-p))


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
    ("C-<right>" sp-backward-slurp-sexp))

(evil-leader/set-key
  "S" 'sync0-hydra-smart-parens/body))

(use-package company-jedi
:straight (company-jedi :type git :host github :repo "emacsorphanage/company-jedi") 
:after company)

(use-package company
;;        :straight (company :type git :host github :repo "company-mode/company-mode") 
        :hook
        (after-init . global-company-mode)
        :custom
                (company-idle-delay 0.1)
                (company-minimum-prefix-length 2)
                (company-tooltip-limit 10)
                (company-tooltip-align-annotations t)
                (company-require-match 'never)
                (company-global-modes '(not erc-mode message-mode help-mode gud-mode))
                (company-frontends '(company-pseudo-tooltip-frontend 
                            company-echo-metadata-frontend))  
                (company-backends '(company-capf))
                (company-auto-complete nil)
    :config
;; Disable company-mode in bibtex-mode (clashes with yasnippets)
 (add-hook 'bibtex-mode-hook (company-mode -1))

(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)

    (defvar +company-backend-alist
      '((text-mode company-capf  company-yasnippet)
      ;; '((text-mode company-capf  company-yasnippet company-org-roam)
      ;; '((text-mode company-capf  company-yasnippet company-ispell company-org-roam)
      ;; '((text-mode company-capf company-dabbrev company-yasnippet company-ispell company-org-roam)
      ;;(text-mode company-capf company-yasnippet company-ispell company-bibtex)
        (prog-mode company-capf company-yasnippet)
        (elisp-mode company-elisp company-capf company-yasnippet)
        (nxml-mode company-capf company-yasnippet company-nxml)
        (python-mode company-capf company-yasnippet company-jedi)
        (conf-mode company-capf company-dabbrev-code company-yasnippet))
      "An alist matching modes to company backends. The backends for any mode is
    built from this.")

    (defun +company--backends ()
      (let (backends)
        (let ((mode major-mode)
              (modes (list major-mode)))
          (while (setq mode (get mode 'derived-mode-parent))
            (push mode modes))
          (dolist (mode modes)
            (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                     (default-value 'company-backends)))
              (push backend backends)))
          (delete-dups
           (append (cl-loop for (mode . backends) in +company-backend-alist
                            if (or (eq major-mode mode)  ; major modes
                                   (and (boundp mode)
                                        (symbol-value mode))) ; minor modes
                            append backends)
                   (nreverse backends))))))

    (defun doom-temp-buffer-p (buf)
      "Returns non-nil if BUF is temporary."
      (equal (substring (buffer-name buf) 0 1) " "))

    (defun +company-init-backends-h ()
      "Set `company-backends' for the current buffer."
      (or (memq major-mode '(fundamental-mode special-mode))
          buffer-read-only
          (doom-temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
          (setq-local company-backends (+company--backends))))

    (put '+company-init-backends-h 'permanent-local-hook t)

    (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)

    (defun sync0-config-prose-completion ()
      "Make auto-complete less agressive in this buffer."
      (setq-local company-minimum-prefix-length 4))

    (add-hook 'text-mode-hook #'sync0-config-prose-completion))

(use-package company-bibtex
:straight (company-bibtex :type git :host github :repo "gbgar/company-bibtex") 
:disabled t
:custom
(company-bibtex-key-regex "[[:alnum:]+_]*")
(company-bibtex-bibliography '("~/Dropbox/notes/bibliography.bib")))

(use-package company-box
  :straight (company-box :type git :host github :repo "sebastiencs/company-box") 
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 10
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"            :face 'all-the-icons-pink))))))

(use-package nxml-mode
:straight nil
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 2
        ;; nxml-auto-insert-xml-declaration-flag nil
        nxml-auto-insert-xml-declaration-flag t
        nxml-bind-meta-tab-to-complete-flag t
        nxml-slash-auto-complete-flag t)
;;; Taken from
;;;  https://martinfowler.com/articles/emacs-nxml-completion.html
(defun sync0-nxml-tag-start ()
  "returns position of < before point"
  (save-excursion (search-backward "<" nil t)))

(defun sync0-nxml-at-attribute-name-p ()
  "truthy if in name of an attribute"
  (save-excursion (re-search-backward rng-in-attribute-regex (sync0-nxml-tag-start) t)))

(defun sync0-nxml-at-attribute-value-p ()
  "truthy if in value of an attribute"
  (save-excursion (re-search-backward rng-in-attribute-value-regex (sync0-nxml-tag-start) t)))

(defun sync0-nxml-completion-at-point ()
  "completion at point for nxml mode"
  (interactive)
  (cond
   ((sync0-nxml-at-attribute-name-p)
    (completion-at-point)
    (insert "=\""))
   ((sync0-nxml-at-attribute-value-p)
    (completion-at-point)
    (insert "\""))
   (t (completion-at-point))))


;;; taken from
;;; https://www.reddit.com/r/emacs/comments/eji55u/usepackage_ensure_nil_not_working_as_intended/

  ;; Outline hook
  (add-hook 'nxml-mode-hook
            (lambda ()
              (outline-minor-mode)
              (setq outline-regexp "^[ \t]*\<[a-zA-Z]+")))

  ;; Helper to format
  (defun sync0-pretty-print-xml-region (begin end)
    "Pretty format XML markup in region. You need to have nxml-mode
 http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
 this.  The function inserts linebreaks to separate tags that have
 nothing but whitespace between them.  It then indents the markup
 by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

;;; taken from 
;;; https://www.manueluberti.eu/emacs/2016/12/03/xmllint/
(defun sync0-xml-format ()
  "Format an XML buffer with `xmllint'."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))
        )

(use-package flycheck
:commands flycheck-mode
:config
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package py-autopep8
:straight (py-autopep8 :type git :host github :repo "paetzke/py-autopep8.el") 
:config
(setq py-autopep8-options '("--max-line-length=100")))

(use-package python
:straight nil
:config
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'flycheck-mode))

(use-package yasnippet 
    :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet") 
    :config
    (require 'bibtex-completion)
    (require 'sync0-yasnippet-bibtex)
    (require 'sync0-yasnippet-doctorat)

;; Fix conflict with Yasnippets
;; See https://emacs.stackexchange.com/questions/29758/yasnippets-and-org-mode-yas-next-field-or-maybe-expand-does-not-expand
(defun yas-org-very-safe-expand ()
  (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))

(add-hook 'org-mode-hook
      (lambda ()
        (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
        (define-key yas-keymap [tab] 'yas-next-field)))

    :hook 
    ((text-mode . yas-minor-mode)
     (prog-mode . yas-minor-mode)
     (bibtex-mode . yas-minor-mode)
     (mu4e-mode . yas-minor-mode)))

(use-package ess
:straight (ess :type git :host github
          :repo "emacs-ess/ESS")
  ;; :init (require 'ess-site)
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.Rnw\\'" . Rnw-mode))
)

(use-package latex
    :straight nil
    :mode
    ("\\.tex\\'" . latex-mode)
    :custom
    (TeX-auto-save t)
;; Don't prompt for saving the .tex file
    (TeX-save-query nil)       
    (TeX-parse-self t)
;; If `t`, automatically shows compilation log
    (TeX-show-compilation nil)         
;; Disable language-specific hyphen insertion.
    (LaTeX-babel-hyphen nil)
    ;; `"` expands into csquotes macros (for this to work, babel pkg must be loaded after csquotes pkg).
    (LaTeX-csquotes-close-quote "}")
    (LaTeX-csquotes-open-quote "\\autoquote{")
    (TeX-file-extensions '("Rnw" "rnw" "Snw" "snw" "tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx"))
    (preview-gs-command "/usr/local/bin/gs")
    ;; Activate forward/reverse search
    (TeX-source-correlate-mode t)        
    (TeX-PDF-mode t)
    :config
    (define-key LaTeX-mode-map (kbd "M-p")
      (lambda ()
        "Save the buffer and run `TeX-command-run-all`."
        (interactive)
        (save-buffer)
        (TeX-command-run-all nil)))

    ;; Zathura settings
    (add-to-list 'TeX-view-program-list  '("Zathura"     ("zathura "
                                                          (mode-io-correlate " --synctex-forward %n:0:%b -x \"emacsclient +%{line} %{input}\" ")
                                                          " %o") "zathura"))

    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "Zathura"))

    (evil-define-key 'normal LaTeX-mode-map
      "k" 'previous-line
      "j" 'next-line
      ;;  "m" 'set-mark-command
      "q" 'fill-paragraph
      "Q" 'sync0-insert-line-below
      (kbd "SPC") 'sync0-insert-whitespace
      "[" 'backward-sentence
      "]" 'forward-sentence)

  (setq-default TeX-master nil ; by each new fie AUCTEX will ask for a master fie.
                TeX-PDF-mode t
                TeX-engine 'luatex)     ; optional

  ;; Font-lock for AuCTeX
  ;; Note: '«' and '»' is by pressing 'C-x 8 <' and 'C-x 8 >', respectively
  (font-lock-add-keywords 'latex-mode (list (list "\\(«\\(.+?\\|\n\\)\\)\\(+?\\)\\(»\\)" '(1 'font-latex-string-face t) '(2 'font-latex-string-face t) '(3 'font-latex-string-face t))))
  ;; Add standard Sweave file extensions to the list of files recognized  by AuCTeX.
  (add-hook 'TeX-mode-hook (lambda () (reftex-isearch-minor-mode))))

(use-package bibtex-completion
  :custom 
  (bibtex-completion-bibliography '("~/Dropbox/bibliographies/bibliography.bib"
                                    "~/Dropbox/bibliographies/doctorat.bib")) 
  (bibtex-completion-notes-path '"~/Dropbox/org/permanent")
  (bibtex-completion-library-path '("~/Documents/pdfs/"))
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-pdf-symbol "P")
  (bibtex-completion-notes-symbol "N")
  (bibtex-completion-pdf-extension '(".pdf" ".epub"))
  (bibtex-completion-additional-search-fields '(editor journaltitle origdate subtitle volume booktitle location publisher))

  :config 
  (setq bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title}: ${subtitle} @ ${journaltitle} [${=key=}]")
          (book          . "${=has-pdf=:1}${=has-note=:1}| ${author} [${origdate}](${date:4}) ${title} ${volume}: ${subtitle} [${=key=}]")
          (inbook        . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} @ ${booktitle} [${=key=}]")
          (incollection  . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} @ ${booktitle} [${=key=}]")
          (collection    . "${=has-pdf=:1}${=has-note=:1}| ${editor} (${date:4}) ${title:55} ${volume}: ${subtitle} [${=key=}]")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} @ ${booktitle} [${=key=}]")
          (t             . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title}: ${subtitle} [${=key=}]")))

  (setq bibtex-completion-notes-template-multiple-files  
        ":PROPERTIES:
:ID:      %(org-id-new) 
:ROAM_REFS: cite:${=key=}
:BIBLATEX_TYPE: ${=type=}
:PARENT: \"${booktitle}\"
:PARENT: \"${journaltitle}\"
:AUTHOR: \"%(sync0-bibtex-completion-reverse-author ${=key=})\"
:CREATED: %(format-time-string \"%Y-%m-%d\")
:LAST_MODIFIED: %(format-time-string \"%Y-%m-%d\")
:ZETTEL_TYPE: reference
:LANGUAGE: ${language}
:DATE: \"${date}\"
:END:
#+TITLE: ${title}
#+SUBTITLE: ${subtitle}
#+AUTHOR: %(sync0-bibtex-completion-reverse-author ${=key=})
#+FILETAGS: :${=type=}:${=key=}:${date}:
#+INTERLEAVE_PDF: ${file}


")

  (use-package ivy-bibtex
;; :custom
;; (ivy-bibtex-default-action 'ivy-bibtex-edit-notes)
    :config
    (require 'sync0-ivy-bibtex-functions)))

(use-package bibtex
  :straight nil
  :custom
  (bibtex-dialect 'biblatex) ;; biblatex as default bib format
  (bibtex-maintain-sorted-entries t)
  (bibtex-field-delimiters 'braces)
  (bibtex-entry-delimiters 'braces)
  (bibtex-comma-after-last-field t)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 0)
  (bibtex-autokey-names 1)
  (bibtex-autokey-names-stretch 1)
  (bibtex-autokey-additional-names "_et_al")
  (bibtex-autokey-name-separator "_")
  (bibtex-autokey-name-year-separator "")
  (bibtex-autokey-name-length t)
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-titleword-length 0)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-titleword-case-convert "uppercase")
  (bibtex-autokey-titlewords 0)
  (bibtex-entry-format '(opts-or-alts numerical-fields page-dashes whitespace braces last-comma delimiters sort-fields))
  ;; (bibtex-entry-format '(opts-or-alts required-fields numerical-fields page-dashes whitespace braces last-comma delimiters sort-fields))

  :config
  (require 'bibtex-completion)
  (require 'sync0-bibtex-functions)
  (require 'sync0-bibtex-fields)

      (unbind-key "TAB" bibtex-mode-map)

  (defvar sync0-bibtex-reference-keys
    (lazy-completion-table sync0-bibtex-reference-keys
                           (lambda () (sync0-bibtex-parse-keys nil t)))
    "Completion table for BibTeX reference keys.
The CDRs of the elements are t for header keys and nil for crossref keys.")

    (evil-define-key 'normal bibtex-mode-map
      "K" 'sync0-bibtex-previous-key
      "J" 'sync0-bibtex-next-key))

(use-package pdf-tools
  ;; :straight (pdf-tools :type git :host github :repo "politza/pdf-tools") 
  :after evil
  :magic ("%PDF" . pdf-view-mode)
  :custom
  ;; automatically annotate highlights
  ;; (pdf-annot-activate-created-annotations t)
  ;; more fine-grained zooming
  (pdf-view-resize-factor 1.1)
  (pdf-view-midnight-colors '("#C0C5CE" . "#4F5B66" ))
  :config
  (pdf-tools-install :no-query)
  (add-to-list 'evil-emacs-state-modes 'pdf-view-mode)
  (add-to-list 'evil-emacs-state-modes 'pdf-outline-buffer-mode)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)

  ;; change midnite mode colours functions
  (defun sync0-pdf-view--original-colors ()
    "Set pdf-view-midnight-colors to original colours."
    (interactive)
    (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
    (pdf-view-midnight-minor-mode))

  (defun sync0-pdf-view-dark-colors ()
    "Set pdf-view-midnight-colors to amber on dark slate blue."
    (interactive)
    (setq pdf-view-midnight-colors '("#C0C5CE" . "#4F5B66" )) ; amber
    (pdf-view-midnight-minor-mode))

  (unbind-key "<SPC>" pdf-view-mode-map)

  :bind ((:map pdf-view-mode-map
               ("C-s" . isearch-forward)
               ("j" . pdf-view-next-line-or-next-page)
               ("J" . pdf-view-scroll-up-or-next-page)
               ("k" . pdf-view-previous-line-or-previous-page)
               ("K" . pdf-view-scroll-down-or-previous-page)
               ("y" . pdf-view-kill-ring-save)
               ("+" . pdf-view-enlarge)
               ("=" . pdf-view-enlarge)
               ("-" . pdf-view-shrink)
               ("/" . isearch-forward)
               ("?" . isearch-backward)
               ("n" . isearch-repeat-forward)
               ("N" . isearch-repeat-backward)
               ("0" . pdf-view-scale-reset)
               ("H" . pdf-annot-add-highlight-markup-annotation)
               ("l" . image-forward-hscroll)
               ("h" . image-backward-hscroll)
               ("t" . pdf-annot-add-text-annotation)
               ("g" . pdf-view-goto-page)
               ("G" . pdf-view-last-page)
               ("D" . pdf-view-dark-minor-mode)
               ("d" . pdf-annot-delete))))

(use-package pdf-outline
:straight nil
    ;; :load-path "site-lisp/pdf-tools/lisp"
    :after pdf-tools
    :bind ((:map pdf-outline-buffer-mode-map
                 ("j" . next-line)
                 ("k" . previous-line))))

(use-package interleave
:after pdf-tools
:commands
(interleave-mode interleave-pdf-mode))

(use-package doc-view 
  :disabled t
  :custom (doc-view-continuous t)
  :bind (:map doc-view-mode-map
              ("q" . quit-window)
              ("+" . doc-view-enlarge)
              ("=" . doc-view-enlarge)
              ("-" . doc-view-shrink)
              ("0" . doc-view-scale-reset)
              ("G" . doc-view-last-page)
              ("g" . doc-view-goto-page)
              ("K" . doc-view-previous-page)
              ("J" . doc-view-next-page)
              ("k" . doc-view-scroll-down-or-previous-page)
              ("j" . doc-view-scroll-up-or-next-page)
              ("W" . doc-view-fit-width-to-window)
              ("H" . doc-view-fit-height-to-window)
              ("P" . doc-view-fit-page-to-window)
              ("X" . doc-view-kill-proc)
              ("s s" . doc-view-set-slice)
              ("s m" . doc-view-set-slice-using-mouse)
              ("s b" . doc-view-set-slice-from-bounding-box)
              ("s r" . doc-view-reset-slice)
              ("/" . doc-view-search)
              ("?" . doc-view-search-backward)))
