;; -*- lexical-binding: t -*-

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

;; Install use-package using the straight package manager. 
(straight-use-package 'use-package)
;; Have use-package use straight by default to install package; i.e.,
;; use-package has to be told not to use straight when this is not
;; desired, such as when packages are already present by default in Emacs.
(setq straight-use-package-by-default t)
;; Turn off 
(setq package-enable-at-startup nil)

(eval-when-compile
  ;; Activate "use-package". 
  (require 'use-package))
;; bind-key is necessary to allow use-package to bind keys through the ":bind" keyword.
(require 'bind-key)

(setq use-package-verbose t)

(add-to-list 'load-path (concat user-emacs-directory "sync0/"))

(setq user-full-name "Carlos Alberto Rivera Carreño"
      ;; Define my Dropbox location
      sync0-dropbox-directory "~/Dropbox/"
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
     ;; Stop asking whether themes are safe
     (custom-safe-themes t)
     :config
     ;; Set CUSTOM directory.
     (setq custom-file (expand-file-name "custom_settings.el" user-emacs-directory))
     ;; Default settings for all themes.
     (custom-theme-set-faces 'user
                              ;; `(org-default ((t (:family "Minion Pro" :style display :height 1.0))))
                              `(markdown-header-face ((t (:family "Helvetica Neue LT Std" :weight light :width condensed :background nil :inherit variable-pitch))))
                             `(markdown-metadata-key-face ((t (:family "Inconsolata" :weight bold :height 0.9 :slant normal :spacing monospace :background nil :inherit fixed-pitch)))) 
                             `(markdown-metadata-value-face ((t (:family "Inconsolata" :height 0.9 :slant normal :spacing monospace :inherit fixed-pitch)))) 
                             `(markdown-gfm-checkbox-face ((t (:family "Inconsolata" :weight bold :spacing monospace))))
                             `(markdown-footnote-marker-face ((t (:family "Sitka Text" :style small :weight bold :height 0.7))))
                             `(markdown-link-face ((t (:family "Sitka Text"  :underline t :background nil :height 1.0 :inherit variable-pitch))))
                             `(markdown-markup-face ((t (:family "Sitka Text"  :underline nil :background nil :height 1.0 :inherit variable-pitch))))
                             `(markdown-url-face ((t (:family "Sitka Text"  :underline nil :background nil :height 1.0 :inherit variable-pitch))))
                             `(markdown-plain-url-face ((t (:inherit markdown-url-face))))
                             `(markdown-code-face ((t (:family "Inconsolata"  :height 1.0 :spacing monospace :inherit fixed-pitch))))
                             `(markdown-reference-face ((t (:inherit markdown-code-face))))
                             `(org-default ((t (:family "Sitka Text"  :inherit variable-pitch))))
                             `(org-link ((t (:inherit org-default :underline t))))
                             `(org-ref-cite-face ((t (:inherit org-link)))) 
                             `(org-footnote ((t (:family "Sitka Text" :style small :weight bold :height 0.7))))
                             `(org-checkbox ((t (:family "Inconsolata" :weight bold :spacing monospace))))
                              `(org-document-title ((t (:family "Helvetica Neue LT Std" :height 2.074  :weight light :width condensed :inherit variable-pitch))))
                              `(org-document-info ((t (:family "Helvetica Neue LT Std" :height 1.728  :weight light :width condensed :inherit variable-pitch))))
                              `(org-level-1 ((t (:family "Helvetica Neue LT Std" :height 2.074  :weight light :width condensed :inherit variable-pitch))))
                              `(org-level-2 ((t (:family "Helvetica Neue LT Std" :height 1.728  :weight light :width condensed :inherit variable-pitch))))
                              `(org-level-3 ((t (:family "Helvetica Neue LT Std" :height 1.44  :weight light :width condensed :inherit variable-pitch))))
                              `(org-level-4 ((t (:family "Helvetica Neue LT Std" :height 1.2  :weight medium :width condensed :inherit variable-pitch))))
                              `(org-level-5 ((t (:family "Helvetica Neue LT Std" :height 1.0  :weight medium :width condensed :inherit variable-pitch))))
                              `(org-level-6 ((t (:family "Helvetica Neue LT Std" :height 0.833  :weight medium :width condensed :inherit variable-pitch))))
                             `(org-meta-line ((t (:family "Inconsolata" :height 0.95 :slant normal :spacing monospace :inherit fixed-pitch)))) 
                             `(org-document-info-keyword ((t (:inherit org-meta-line))))
                             `(org-special-keywords ((t (:inherit org-meta-line))))
                             `(org-drawer ((t (:inherit org-meta-line)))) 
                             `(org-property-value ((t (:inherit org-meta-line)))) 
                             `(org-ellipsis ((t (:family "Fira Code" :underline nil :box nil)))) 
                             `(org-date ((t (:family "Inconsolata" :height 0.95 :spacing monospace :inherit fixed-pitch))))
                              `(org-agenda-date ((t (:family "Helvetica Neue LT Std" :height 1.563  :weight light :width condensed :inherit variable-pitch))))
                              `(org-agenda-date-weekend ((t (:family "Helvetica Neue LT Std" :height 1.563  :weight light :width condensed :inherit variable-pitch))))
                              `(org-agenda-date-today ((t (:family "Helvetica Neue LT Std" :height 1.563  :weight light :width condensed :inherit variable-pitch))))
                              `(org-agenda-structure ((t (:family "Helvetica Neue LT Std" :height 1.953  :weight light :width condensed :inherit variable-pitch))))
                             `(org-scheduled ((t (:weight medium :slant normal))))
                             `(org-scheduled-today ((t (:family "Inconsolata" :weight medium :slant normal :spacing monospace :inherit fixed-pitch))))
                             `(org-scheduled-previously ((t (:family "Inconsolata" :weight normal :slant normal :spacing monospace :inherit fixed-pitch))))
                             `(org-upcoming-deadline ((t (:inherit org-scheduled-previously))))
                             `(org-agenda-diary ((t (:family "Inconsolata" :spacing monospace :inherit fixed-pitch))))
                             `(org-agenda-done ((t (:strike-through t))))
                             `(org-table ((t (:family "Inconsolata" :height 0.95 :spacing monospace :inherit fixed-pitch))))
                             `(org-block ((t (:family "Inconsolata" :height 0.95 :spacing monospace :background nil :inherit fixed-pitch))))
                             `(org-block-begin-line ((t (:family "Inconsolata" :height 0.95 :spacing monospace :weight bold :inherit fixed-pitch))))
                             `(org-block-end-line ((t (:inherit org-block-begin-line))))
                             `(org-tag ((t (:family "Inconsolata" :height 0.75 :spacing monospace :inherit fixed-pitch))))))

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
(setq-default undo-limit 800000
              ;; Split vertically by default
              split-height-threshold nil
              ;; split-width-threshold (- (window-width) 10)
              split-width-threshold 0
              ;; hide cursors in other windows
              cursor-in-non-selected-windows nil  
              ;; Don't resize frames implicitly.
              frame-inhibit-implied-resize t
              ;; Do not let overly long lines in the buffer without truncation
              truncate-lines t
              ;; truncate-partial-width-windows t
              highlight-nonselected-windows nil
              ;; Don't show the "Welcome to GNU Emacs ..." at startup
              inhibit-startup-screen t
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
;; (global-set-key (kbd "M-h") 'next-buffer)
;; Quickly save
(global-set-key (kbd "M-w") 'save-buffer)
;; EVIL friendly keybindings for previous-buffer
;; (global-set-key (kbd "M-l") 'previous-buffer)

  (defvar sync0-zettelkasten-all-properties-list
    '("ZETTEL_TYPE" "BIBLATEX_TYPE" "ZETTEL_FUNCTION" "FICHE_TYPE" "PROJECT_TITLE" "ANNOTATION_REFS" "ROAM_REFS" "ROAM_ALIASES" "CROSSREF" "PARENT" "WEBSITE") 
    "List of zettel properties")

  (defvar sync0-zettelkasten-properties-list
    '("PROJECT_TITLE") 
    "List of zettel properties")

  (defvar sync0-zettelkasten-excluded-candidates
    '("journal" "fiche" "etc" "project" "todo" "reference"))

  (defvar sync0-zettelkasten-project-directories '("project" "todo"))

  (defvar sync0-zettelkasten-zettel-types 
    '()
    "List of projects in my Zettelkasten.")

  (defvar sync0-zettelkasten-projects 
    '()
    "List of projects in my Zettelkasten.")

  (defvar sync0-zettelkasten-zettel-functions 
    '() 
    "List of possible functions for a Zettel.")

  (defvar sync0-zettelkasten-fiche-types 
    '() 
    "List of fiche types.")

  (defvar sync0-zettelkasten-variables-list
    '((sync0-zettelkasten-projects . "~/.emacs.d/sync0-vars/projects.txt")
      (sync0-zettelkasten-zettel-types . "~/.emacs.d/sync0-vars/zettel-types.txt")
      (sync0-zettelkasten-zettel-functions . "~/.emacs.d/sync0-vars/zettel-functions.txt")
      (sync0-zettelkasten-fiche-types . "~/.emacs.d/sync0-vars/fiche-types.txt")))

  (defvar sync0-bibtex-entry-types
    '("Article" "Book" "InBook" "InCollection" "Collection" "Unpublished" "Thesis" "Proceedings" "InProceedings" "Online" "Report" "Manual" "Misc")
    "List of Bibtex entry types")

  (defvar sync0-bibtex-crossref-types
    '("InBook" "InCollection" "InProceedings")
    "List of Bibtex entry types")

  (defvar sync0-bibtex-fields
    '("title" "subtitle" "eventtitle" "date" "origdate" "eventdate" "author" "journaltitle" "edition" "booktitle" "booksubtitle" "crossref" "chapter" "volume" "number" "series" "publisher" "location" "pages" "note" "doi" "url" "urldate" "language" "langid" "medium" "institution" "library" "related" "relatedtype" "file" "shorthand" "keywords")
    "List of Bibtex entry fields")

  (defvar sync0-bibtex-full-fields
    '("title" "subtitle" "date" "origdate" "author" "journaltitle" "booktitle" "booksubtitle" "translator" "crossref"  "eventdate" "eventtitle" "venue" "volume" "number" "chapter" "edition" "pages" "publisher" "location" "pages" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
    "List of Bibtex entry fields")

  (defvar sync0-bibtex-quick-fields
    '("title" "subtitle" "date" "author" "note" "url" "urldate" "language" "langid" "library" "file" "keywords")
    "List of Bibtex entry fields")

  (defvar sync0-bibtex-extract-fields
    '("title" "date" "author" "crossref" "pages" "language" "langid" "file" "keywords")
    "List of Bibtex entry fields")

  (defvar sync0-bibtex-completion-booktitle 
    '()
    "List of bibtex authors")

  (defvar sync0-bibtex-completion-publisher 
    '()
    "List of bibtex authors")

  (defvar sync0-bibtex-completion-journaltitle
    '()
    "List of bibtex authors")

  (defvar sync0-bibtex-completion-title
    '()
    "List of bibtex authors")

  (defvar sync0-bibtex-completion-location 
    '()
    "List of bibtex authors")

  (defvar sync0-bibtex-completion-author 
    '()
    "List of bibtex authors")

  (defvar sync0-bibtex-completion-language
    '()
    "List of Bibtex languages")

  (defvar sync0-bibtex-completion-medium
    '()
    "List of Bibtex media")

  (defvar sync0-bibtex-completion-library
    '()
    "List of Bibtex traces")

  (defvar sync0-bibtex-completion-institution
    '()
    "List of Bibtex traces")

  (defvar sync0-bibtex-completion-keywords
    '()
    "List of Bibtex traces")

  (defvar sync0-bibtex-completion-note
    '()
    "List of Bibtex traces")

  (defvar sync0-bibtex-completion-variables-alist
    '((sync0-bibtex-completion-publisher . "~/.emacs.d/sync0-vars/bibtex-completion-publisher.txt")
      (sync0-bibtex-completion-journaltitle . "~/.emacs.d/sync0-vars/bibtex-completion-journaltitle.txt")
      (sync0-bibtex-completion-location . "~/.emacs.d/sync0-vars/bibtex-completion-location.txt")
      (sync0-bibtex-completion-title . "~/.emacs.d/sync0-vars/bibtex-completion-title.txt")
      (sync0-bibtex-completion-author .  "~/.emacs.d/sync0-vars/bibtex-completion-author.txt")
      (sync0-bibtex-completion-keywords .  "~/.emacs.d/sync0-vars/bibtex-completion-keywords.txt")
      (sync0-bibtex-completion-note .  "~/.emacs.d/sync0-vars/bibtex-completion-note.txt")
      (sync0-bibtex-completion-library .  "~/.emacs.d/sync0-vars/bibtex-completion-library.txt")
      (sync0-bibtex-completion-medium .  "~/.emacs.d/sync0-vars/bibtex-completion-medium.txt")
      (sync0-bibtex-completion-institution .  "~/.emacs.d/sync0-vars/bibtex-completion-institution.txt")
      (sync0-bibtex-completion-language .  "~/.emacs.d/sync0-vars/bibtex-completion-language.txt"))
"Alist of variables used to define their initial values to be used in completion.")

      ;; (sync0-bibtex-completion-booktitle . "~/.emacs.d/sync0-vars/bibtex-completion-booktitle.txt")

        ;; sync0-zettelkasten-directory-references (concat (getenv "HOME") "/Dropbox/org/reference/")

  ;; define the rest
  (setq sync0-zettelkasten-directory (concat (getenv "HOME") "/Dropbox/org/")
        sync0-obsidian-directory (concat (getenv "HOME") "/Dropbox/obsidian/")
        sync0-zettelkasten-directory-sans (concat (getenv "HOME") "/Dropbox/org")
        sync0-exported-pdfs-directory (concat (getenv "HOME") "/Dropbox/pdfs/")
        sync0-default-bibliography (concat (getenv "HOME") "/Dropbox/bibliographies/bibliography.bib")
        sync0-emacs-directory (concat (getenv "HOME") "/.emacs.d/sync0/")
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

(defun sync0-nullify-variable-list (varlist)
  "Set all variables from varlist nil"
  (mapc #'(lambda (a) (set a nil)) varlist))

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

(sync0-set-variable-from-files sync0-zettelkasten-variables-list)
(sync0-set-variable-from-files sync0-bibtex-completion-variables-alist)

(defun sync0-downcase-and-no-whitespace (x)
  "Downcase and replace whitespace by _ in the current string"
  (downcase
   (replace-regexp-in-string "[[:space:]-]+" "_" x)))

(defun sync0-update-timestamp ()
  "Update current #+DATE timestamp"
  (org-with-point-at 1
    (let ((regex-one "^:LAST_MODIFIED: \\(.+\\)")
          (regex-two "^#\\+DATE: \\([0-9-]+\\)")
          (date (format-time-string "%Y-%m-%d")))
      (when (re-search-forward regex-one nil t 1)
        (replace-match date nil nil nil 1))
      (when (re-search-forward regex-two nil t 1)
        (replace-match date nil nil nil 1)))))

    (defun sync0-zettelkasten-update-org-properties ()
      (let*  ((zettel-properties
               (let (x)
                 (dolist (property sync0-zettelkasten-properties-list x)
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
              ;; (zettelkasten-dirs (split-string-and-unquote sync0-zettelkasten-directory "/"))
              ;; ;; this produces a list not a string
              ;; (current-dir  (cl-set-difference path-dirs zettelkasten-dirs  :test #'equal))
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
    (dolist (property sync0-zettelkasten-properties-list)
           (when-let ((value (org-entry-get 1 property)))
             (unless (or (string-match-p "\" \"" value)
                         (string-match-p "\"[[:print:]]+\"" value))
              (org-set-property property (concat "\""  value "\""))))))))

(defun sync0-before-save-actions ()
  "Set of functions to hook to before-save-hook"
  (when (and (equal major-mode 'org-mode)
             (string-prefix-p sync0-zettelkasten-directory (buffer-file-name)))
    (sync0-zettelkasten-update-org-properties)
    (sync0-update-timestamp)))

(add-hook 'before-save-hook #'sync0-before-save-actions)

(defun sync0-copy-file-path-in-clipboard ()
  "Copy absolute path of file visited in current buffer into the clipboard and kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

;; https://emacs.stackexchange.com/questions/36850/copy-to-kill-ring-selected-file-names-full-path
(defun sync0-dired-copy-path-at-point ()
  "In dired buffers, copy the full path of file at point." 
  (interactive)
  (dired-copy-filename-as-kill 0))

;; Another one to copy file name to clipboard:
;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/

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
    (other-window 1)))

(defun sync0-split-and-follow-vertically ()
  " Split the selected window into two windows, one above the other.
  The selected window, which displays the same buffer, is below."
  (interactive)
  (progn
    (split-window-right)
    (balance-windows)
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
           (pos (mod (+ (cl-position file files :test #'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(defun sync0-insert-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (insert (concat (car list) "\n"))
    (setq list (cdr list))))

(defun sync0-show-elements-of-list (list sep)
  "Print massive string with each element of list separated by sep"
  (let (x)
    (while list
      (setq x (concat (car list) sep x))
      (setq list (cdr list)))
    (string-trim-right x sep)))

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

(defun sync0-null-p (var)
"General purpose predicate to determine whether an object var is
empty (not in the lisp sense but in a human-readable sense)."
  (cond ((stringp var)
         (or (string= var "")
             (string= var "nil")))
        ((listp var)
          (or (null var)
              (equal var '(""))))
          (t (null var))))

;; (defun sync0-null-p (var)
;; (cond ((stringp var)
;;   (or (null var)
;;       (string= var "")
;;       (string= var "nil")))

(require 'sync0-functions)

(use-package s)

  (use-package undo-tree
    :custom
    (undo-tree-enable-undo-in-region nil)
    :config
    (global-undo-tree-mode))

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

(use-package hydra
  :straight (hydra :type git :host github :repo "abo-abo/hydra"))

(use-package major-mode-hydra
  :straight (major-mode-hydra :type git :host github :repo "jerrypnz/major-mode-hydra.el")
  :bind
  ("M-SPC" . major-mode-hydra)
  :custom
  (major-mode-hydra-invisible-quit-key "q")
  :config
  (setq major-mode-hydra-title-generator
        '(lambda (mode)
           (s-concat "\n"
                     (s-repeat 10 " ")
                     (all-the-icons-icon-for-mode mode :v-adjust 0.05)
                     " "
                     (symbol-name mode)
                     " commands"))))

(use-package which-key
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key")
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-width 0.33)
  (which-key-side-window-max-height 0.25)
  :config
  (which-key-mode))

(use-package swiper 
  :commands swiper
  :bind (("C-s" . swiper)))

(use-package ivy
  :hook (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (setq ivy-re-builders-alist
        '((ivy-bibtex . ivy--regex-ignore-order)
          (t . ivy--regex-plus))))

(use-package counsel 
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("<f1>" . sync0-hydra-help/body)
   ("C-x C-f" . counsel-find-file)))

(use-package evil-escape 
  :straight (evil-escape :type git :host github :repo "syl20bnr/evil-escape") 
  :after evil
  :commands evil-escape-mode
  :custom
  (evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (evil-escape-excluded-major-modes '(neotree-mode))
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

(use-package evil-leader
  :straight (evil-leader :type git :host github :repo "cofi/evil-leader") 
  :hook (after-init . global-evil-leader-mode)
  :custom
  (evil-leader/in-all-states t)
  :config
  (evil-leader/set-leader "<SPC>")

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
    "b" 'ivy-switch-buffer
    "K" 'kill-buffer)

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
    "h" 'sync0-hydra-help/body))

  (use-package evil  
    :custom
    ;; Make horizontal movement cross lines                                    
    (evil-cross-lines t)
    ;; turn off auto-indent 
    (evil-auto-indent t)
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
    ;; Turn on evil mode when enabled.
    (evil-mode 1)
    ;; Turn on evil-escape mode when enabled.
    (evil-escape-mode 1)
    ;; prevent conflict with calf bindings. 
    ;; (add-to-list 'evil-emacs-state-modes 'cfw:details-mode)

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

    ;; Change global key bindings
    (unbind-key "C-m" evil-normal-state-map)
    (unbind-key "M-." evil-normal-state-map)
    (unbind-key "C-d" evil-motion-state-map)
    ;; (unbind-key "<SPC>" evil-motion-state-map)

    (evil-define-key 'normal global-map
      "/" 'swiper
      "gb" 'counsel-bookmark
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

  (use-package epa-file
    :straight nil
    :load-path "~/.emacs.d/sync0/" 
    :custom
    (epa-file-encrypt-to '("carc.sync0@gmail.com"))
    (epa-file-select-keys 'silent))

  (use-package saveplace
    :straight nil
    :config (save-place-mode))

  (use-package projectile
    :straight (projectile :type git :host github :repo "bbatsov/projectile")
    :custom
    (projectile-sort-order 'recentf)
    (projectile-completion-system 'ivy)
    :config
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
 
    (evil-leader/set-key  "P" 'projectile-commander))

(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git :host github :repo "purcell/exec-path-from-shell")
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq initial-scratch-message ";; 
;; « Ces bonnes gens qui dorment tranquilles, c'est drôle!
;; Patience! un nouveau 89 se prépare! On est las de constitutions,
;; de chartes, de subtilités, de mensonges! Ah! si j'avais un
;; journal ou une tribune, comme je vous secouerais tout cela! Mais,
;; pour entreprendre n'importe quoi, il faut de l'argent! Quelle
;; malédiction que d'être le fils d'un cabaretier et de perdre sa
;; jeunesse à la quête de son pain! »
;;
;; Gustave Flaubert
;; L'éducation sentimentale (1885)
;; "
)

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
           left-margin-width 2
           ;; left-margin 2
           right-margin-width 0
           ;; Remove continuation arrow on right fringe.
           ;; fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
           ;;                              fringe-indicator-alist)
           indicate-buffer-boundaries nil
           indicate-empty-lines nil
           max-mini-window-height 0.3)

       ;; Low resolution settings:
          (setq-default                    
           ;; Avoid ugly problemes with git-gutter.
           fringes-outside-margins t
           left-margin-width 1
           right-margin-width 0
           left-fringe-width 0
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
         (add-hook 'find-file-hook #'sync0-mode-line-count-lines)
         (add-hook 'after-save-hook #'sync0-mode-line-count-lines)
         (add-hook 'after-revert-hook #'sync0-mode-line-count-lines)

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
           (propertize fixed-title 'face '(:height 1.0 :family "Helvetica Neue LT Std" :width condensed :weight medium) 'help-echo (buffer-file-name)))
           ;; (propertize fixed-title 'face '(:height 1.0 :family "Myriad Pro" :weight medium) 'help-echo (buffer-file-name)))
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
                          (s-replace "-mode" "" (format "%s" major-mode))
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
                                 :height 150)
             ;;:height 175
             (setq line-spacing 7))
    ;; low resolution font size
    (progn (set-face-attribute 'default nil 
                               :family "Inconsolata"
                               :height 130)
           (setq line-spacing 3)))

  (defun sync0-buffer-face-proportional ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (if (> (display-pixel-width) 1900)
        ;; high resolution font size (t14s)
        (progn
          (setq buffer-face-mode-face '(:family "Sitka Text" :height 165))
          (setq line-spacing 0.5))
      ;; low resolution font size
      (progn
        ;; (setq buffer-face-mode-face '(:family "Minion Pro" :height 155 :spacing proportional))
        (setq buffer-face-mode-face '(:family "Sitka Text" :height 130))
        ;; (setq line-spacing 0.2)
        (setq line-spacing 0.225)))
    (buffer-face-mode))

;; (add-hook 'prog-mode-hook #'sync0-buffer-face-fixed)
(add-hook 'erc-mode-hook #'sync0-buffer-face-proportional)
(add-hook 'Info-mode-hook #'sync0-buffer-face-proportional)
(add-hook 'org-mode-hook #'sync0-buffer-face-proportional)
(add-hook 'markdown-mode-hook #'sync0-buffer-face-proportional)
;; (add-hook 'text-mode-hook #'sync0-buffer-face-proportional)

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

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode org-mode neotree-mode markdown-mode deft-mode help-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(defun sync0-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 0)
  (setq right-margin-width 0))

(defun sync0-set-neotree-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 0)
  (setq left-fringe-width 0)
  (setq right-margin-width 0))

(add-hook 'prog-mode-hook #'sync0-set-margins)
(add-hook 'bibtex-mode-hook #'sync0-set-margins)
(add-hook 'neotree-mode-hook #'sync0-set-neotree-margins)

   (use-package all-the-icons 
     :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el") 
     ;; improve performance 
     :custom (inhibit-compacting-font-caches t))

(use-package doom-themes  
  :straight (doom-themes :type git :host github :repo "hlissner/emacs-doom-themes") 
  :after custom
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
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Correct org-mode's native fontification.
  (doom-themes-org-config))

(use-package cycle-themes 
  :straight (cycle-themes :type git :host github :repo "toroidal-code/cycle-themes.el") 
  :config 
  ;; The order has to be set this way for the hook to work
  ;; (setq cycle-themes-theme-list '(doom-zenburn doom-flatwhite))
  (evil-leader/set-key
    "T" 'cycle-themes)
  (setq cycle-themes-theme-list '(doom-zenburn doom-flatwhite)))

(use-package hl-line 
  :straight nil
  :hook (prog-mode . hl-line-mode)
  ;; :hook ((conf-mode prog-mode) . hl-line-mode)
  :custom
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

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

(use-package magit
  :straight (magit :type git :host github :repo "magit/magit") 
  ;; :commands (magit-status magit-blame)
  :custom
  (magit-branch-arguments nil)
  (magit-push-always-verify nil)
  ;; Get rid of the previous advice to go into fullscreen
  (magit-restore-window-configuration t)
  :config
  (evil-leader/set-key  "g" 'magit-status))

(use-package git-timemachine
  :straight (git-timemachine :type git :host gitlab :repo "pidu/git-timemachine") 
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

  (evil-leader/set-key  "G" 'git-timemachine)

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
  (org-id-track-globally t)
  :init
  (require 'find-lisp)
  :config
  ;; Update ID file on startup
  (org-id-update-id-locations))

(use-package org-ref
  ;; :straight (org-ref :type git :host github :repo "jkitchin/org-ref") 
  :custom
  (reftex-default-bibliography '("~/Dropbox/bibliographies/bibliography.bib"
                                 "~/Dropbox/bibliographies/doctorat.bib"))
  (org-ref-default-bibliography reftex-default-bibliography)
  (org-ref-pdf-directory sync0-pdfs-folder)
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

;; disable warning
(setq org-roam-v2-ack t) 

(use-package org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam") 
  :init 
  (require 'org-id)
  :custom
  (org-roam-directory "~/Dropbox/obsidian/")
  (org-roam-file-exclude-regexp "\(task\|img\|doctorat\|templates\)/[[:graph:]]+.md")
  (org-roam-file-extensions '("org" "md"))
  ;; exclude useless files from my org directory 
  ;; (org-roam-file-exclude-regexp "etc/[[:graph:]]+.org")
  :config
  (setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.md"))
  ;; (org-roam-setup)

  (require 'org-ref)
  ;; (require 'md-roam)
  ;; (require 'org-emms)
  ;; (require 'deft)
  (require 'sync0-org-roam-functions)

  ;; (cl-defmethod org-roam-node-zettel-type ((node org-roam-node))
  ;;   (cdr
  ;;    (assoc "ZETTEL_TYPE" (org-roam-node-properties node)))) 

  ;; (cl-defmethod org-roam-node-fiche-type ((node org-roam-node))
  ;;   (cdr
  ;;    (assoc "FICHE_TYPE" (org-roam-node-properties node)))) 

  ;; (cl-defmethod org-roam-node-zettel-function ((node org-roam-node))
  ;;   (cdr
  ;;    (assoc "ZETTEL_FUNCTION" (org-roam-node-properties node)))) 

  ;; (cl-defmethod org-roam-node-biblatex-type ((node org-roam-node))
  ;;   (cdr
  ;;    (assoc "BIBLATEX_TYPE" (org-roam-node-properties node)))) 

  ;; (setq org-roam-node-display-template "${title:80}  ${tags:50} ${zettel-type} : ${biblatex-type}${fiche-type}${zettel-function}")

  ;; add the possiblity to follow links in the org-roam buffer
  (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing)

(evil-leader/set-key-for-mode 'org-mode "B" 'org-roam-buffer-toggle)
;; (evil-leader/set-key-for-mode 'org-mode "i" 'sync0-org-roam-insert)
;; (evil-leader/set-key-for-mode 'org-mode "I" 'sync0-hydra-org-roam-insert/body)

  (evil-leader/set-key
    "F" 'org-roam-node-find))

(use-package md-roam
  :straight '(md-roam :type git :host github :repo "nobiot/md-roam")
  :custom
  ;; default "md". Specify an extension such as "markdown"
  (md-roam-file-extension "md") 
  :config
  (md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
  (org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

  (setq md-roam-regex-aliases
        ;; Assumed to be case insensitive
        "\\(^.*aliases:[ \t]*\\)\\(.*\\)")

  (setq md-roam-ref-keys
        ;; Assumed to be case insensitive
        "\\(^.*citekey:[ \t]*\\)\\(.*\\)")

(cl-defun md-roam-node-insert (&optional filter-fn &key templates info)
  "Find an Org-roam node and insert (where the point is) an \"id:\" link to it.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
The TEMPLATES, if provided, override the list of capture templates (see
`org-roam-capture-'.)
The INFO, if provided, is passed to the underlying `org-roam-capture-'."
  (when (md-roam--markdown-file-p (buffer-file-name (buffer-base-buffer)))
    (unwind-protect
        ;; Group functions together to avoid inconsistent state on quit
        (atomic-change-group
          (let* (region-text
                 beg end
                 (_ (when (region-active-p)
                      (setq beg (set-marker (make-marker) (region-beginning)))
                      (setq end (set-marker (make-marker) (region-end)))
                      (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
                 (node (org-roam-node-read region-text filter-fn))
                 (description (or region-text
                                  (org-roam-node-formatted node))))
            (if (org-roam-node-id node)
                (progn
                  (when region-text
                    (delete-region beg end)
                    (set-marker beg nil)
                    (set-marker end nil))
                  (insert (concat "["
                                  (cond
                                   ((eq md-roam-node-insert-type 'id)
                                    (concat description "](" (org-roam-node-id node) ".md)" ))
                                   ((eq md-roam-node-insert-type 'title-or-alias)
                                    (concat  (org-roam-node-title node) "](" (org-roam-node-id node) ".md)")))))
                  ;; for advice
                  t)
              (org-roam-capture-
               :node node
               :info info
               :templates templates
               :props (append
                       (when (and beg end)
                         (list :region (cons beg end)))
                       (list :insert-at (point-marker)
                             :link-description description
                             :finalize 'insert-link)))
              ;; for advice
              t)))
      (deactivate-mark)
      ;; for advice
      t)))
  )

(use-package org-capture 
  :straight nil
  :after org 
  :custom
  (org-default-notes-file "~/Dropbox/etc/notes.org")
  :config 
  (require 'org-ref)
  (require 'sync0-org-capture-functions)

  ;; (evil-leader/set-key "c" 'org-capture)

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (setq org-capture-templates 
        '(("a" "Annotation" entry
           (file buffer-file-name)
           (function sync0-org-capture-annotation-body)
           ;; (file+headline "~/Dropbox/org/todo/todo.org" "Autres")
           ;; (file "~/Dropbox/org/todo/todo.org")
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

  (use-package org-bullets 
    :straight (org-bullets :type git :host github :repo "sabof/org-bullets") 
    :custom
    ;; Hide all bullets:
    (org-bullets-bullet-list '(" ")))

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
   org-noter-notes-search-path (list sync0-zettelkasten-directory)))

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

(use-package ox-epub
  :straight (ox-epub :type git :host github :repo "ofosos/ox-epub")
  :after org)

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
    "D" 'sync0-hydra-org-download-functions/body))

  (use-package org 
    :after evil
    :custom
    (org-hide-leading-stars t)
    ;; Leave one line between headlines 
    (org-cycle-separator-lines 0)
    ;; (org-cycle-separator-lines 2)
    ;; Don't fontify the whole damn line
    (org-fontify-whole-block-delimiter-line nil)
    ;; Disable word wrap in org mode.
    ;; (org-startup-truncated t)
    ;; Initial indentation
    (org-startup-indented t)         
    ;; Necessary to avoid crazy inconsistenscies using org-download and org-roam
    (org-link-file-path-type 'absolute)
    ;; Begin displaying entire trees.
    (org-startup-folded nil)
    ;; Better display of italics & bold.
    (org-hide-emphasis-markers t)
    ;; Define org-tags.
    (org-tag-alist '(("urgent" . ?u)
                     ("current" . ?c)
                     ("next" . ?n)
                     ("skim" . ?s)
                     ("exegesis" . ?e)
                     ("waiting" . ?w)
                     ;; ("postponed" . ?p)
                     ("revise" . ?r)
                     ("someday" . ?a)
                     ("fetch" . ?f)
                     ("@office" . ?o)
                     ("@home" . ?h)
                     ("@deepwork" . ?p)
                     ("transcribe" . ?t)
                     ("ignore" . ?i)
                     ("delegated" . ?d)))
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
    ;; (org-blank-before-new-entry '((heading . nil)(plain-list-item . nil)))
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
    (org-src-tab-acts-natively t)
    (org-src-preserve-indentation t)
    (org-edit-src-content-indentation 0)
    ;; Color embeded source code
    (org-src-fontify-natively t)
    (org-fontify-done-headline t) 
    (org-fontify-whole-heading-line t)
    (org-fontify-quote-and-verse-blocks t)
    ;; Don't fontify sub and superscripts.
    (org-pretty-entities-include-sub-superscripts nil)
    ;; Limit inheritance for certain tags. 
    (org-tags-exclude-from-inheritance (quote ("crypt" "ignore" "next" "current" "waiting" "someday" "delegated" "urgent")))
    (org-log-done 'time)
    :config 
    (require 'sync0-org)
    ;; This is necessary to avoid conflict with my motion bindings. 
    (unbind-key "M-h" org-mode-map)

    :bind (;;("<f5>" . sync0-hydra-file-access/body)
           ("C-x 2" . sync0-split-and-follow-horizontally)
           ("C-x 3" . sync0-split-and-follow-vertically)
           (:map org-mode-map
                 ("M-<return>" . sync0-org-meta-return-dwim)
                 ("M-S-<return>" . sync0-org-insert-todo-heading-dwim))))

(use-package auto-fill
  :straight nil
  :hook 
  (text-mode . turn-on-auto-fill)
  (markdown-mode . turn-off-auto-fill)
  ;; (mu4e-compose-mode . turn-off-auto-fill)
  ;; (mu4e-view-mode . turn-off-auto-fill)
  :preface
  ;; Configure exceptions for auto-fill mode. 
  (defun sync0-nobreak-p ()
    (and (looking-at "+[[:alnum:]]")
         (looking-back "^\\\[A-z]+{.+" (line-beginning-position))))
  ;; Define column width for auto-fill mode. 
  :config
  (setq-local fill-column 66)
  ;; Respect de la typographie française par auto-fill mode.
  ;; (setq fill-nobreak-predicate '(fill-french-nobreak-p))
  ;; Set hook for exceptions to auto-fill-mode.
  (add-hook 'fill-nobreak-predicate #'sync0-nobreak-p))

  (use-package visual-line
    :straight nil
    :commands visual-line-mode
    :hook 
    ;; (text-mode . visual-line-mode) 
    (markdown-mode . visual-line-mode))

   (use-package visual-fill-column
     :straight (visual-fill-column :type git :host github :repo "joostkremers/visual-fill-column")
     :commands visual-fill-column-mode
     ;; :hook 
     ;; (text-mode . visual-fill-column-mode)
     ;; (markdown-mode . visual-fill-column-mode)
     ;; (mu4e-view-mode . visual-fill-column-mode)
     ;; (mu4e-compose-mode . visual-fill-column-mode)
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
  :hook (text-mode . flyspell-mode)
  :custom
  (ispell-parser 'tex)
  (flyspell-issue-message-flag nil))

(use-package smart-quotes 
  :straight (smart-quotes :type git :host github :repo "gareth-rees/smart-quotes") 
  :hook (markdown-mode . smart-quotes-mode))

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

  (use-package writeroom-mode
    :commands writeroom-mode
    :straight (writeroom-mode :type git :host github :repo "joostkremers/writeroom-mode")
    :hook (markdown-mode . writeroom-mode)
    :custom
    (writeroom-width 66))

(use-package esxml
  :straight (esxml :type git :host github :repo "tali713/esxml"))

(use-package nov
  :straight nil
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
    "I" 'org-noter-insert-precise-note)

  (defun sync0-nov-font-setup ()
    (if (> (display-pixel-width) 1900)
        ;; high resolution (t14s)
        (progn
          (face-remap-add-relative 'variable-pitch
                                   :family "Sitka Text"
                                   ;; :height 200
                                   ;; :height 220
                                   :height 250)

          (setq nov-text-width 66)
          (nov-render-document))
      ;; low resolution 
      (progn
        (face-remap-add-relative 'variable-pitch
                                 :family "Sitka Text"
                                 ;; :height 200
                                 ;; :height 155
                                 ;; :height 130
                                 :height 150)
        (setq nov-text-width 60)
        (nov-render-document))))

  (add-hook 'nov-mode-hook #'sync0-nov-font-setup))

(require 'sync0-obsidian)

(use-package deft
  :custom
  (deft-recursive t)
  (deft-use-filename-as-title t)
  ;; (deft-use-filter-string-for-filename t)
  (deft-default-extension "md")
  ;; (deft-default-extension "org")
  (deft-directory sync0-obsidian-directory)
  (deft-new-file-format "%Y%m%d%H%M%S")
  :config
  (require 'sync0-deft)
  (evil-leader/set-key "d" 'deft))

 (use-package follow-mode
  :straight nil
  :commands follow-mode
  :custom (follow-auto t))

   (use-package festival 
     :straight nil
     :load-path "~/.emacs.d/sync0/" 
     :commands say-minor-mode
     :config
     (autoload 'say-minor-mode "festival" "Menu for using Festival." t)
     ;; (say-minor-mode t)

     ;; (defun sync0-festival-el () 
     ;;   (interactive)
     ;;   (festival-send-command '(voice_el_diphone)))

     ;; (defun sync0-festival-english-male () 
     ;;   (interactive)
     ;;   (festival-send-command '(voice_nitech_us_awb_arctic_hts)))

     ;; (defun sync0-festival-english-female () 
     ;;   (interactive)
     ;;   (festival-send-command '(voice_nitech_us_slt_arctic_hts)))

     :bind (:map evil-visual-state-map 
           ("s" . festival-say-region)))

(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode :type git :host github :repo "ppareit/graphviz-dot-mode") 
  :custom
  (graphviz-dot-indent-width 4))
;; (use-package company-graphviz-dot
;;   :straight (company-graphviz-dot :type git :host github :repo "ppareit/graphviz-dot-mode"))

  (use-package yaml-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :custom
    (markdown-enable-wiki-links t)
    (markdown-enable-math t)
    (markdown-coding-system 'utf-8)
    (markdown-asymmetric-header t)
    (markdown-hide-markup t)
    ;; (markdown-hide-markup nil)
    (markdown-header-scaling t)
    ;; (markdown-header-scaling-values '(1.953 1.563 1.25 1.0 0.8 0.64))
    (markdown-header-scaling-values '(2.074 1.728 1.44 1.2 1.0 0.833))
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))

    :config
    (require 'sync0-markdown)

    (evil-define-key 'visual markdown-mode-map
      "z" 'markdown-insert-italic)

    (evil-leader/set-key-for-mode 'markdown-mode "O" 'markdown-follow-link-at-point)
    ;; (evil-leader/set-key-for-mode 'markdown-mode "i" 'markdown-insert-wiki-link)
;; (evil-leader/set-key-for-mode 'markdown-mode "B" 'org-roam-buffer-toggle)
(evil-leader/set-key-for-mode 'markdown-mode "i" 'sync0-org-roam-insert)

    (evil-define-key 'normal markdown-mode-map
      (kbd "<tab>") 'markdown-cycle
      "<" 'markdown-backward-same-level
      ">" 'markdown-forward-same-level
      (kbd "C->") 'markdown-forward-same-level
      (kbd "C-<") 'markdown-backward-same-level
      "H" 'markdown-promote
      "L" 'markdown-demote
      "K" 'markdown-move-up
      "J" 'markdown-move-down
      "k" 'evil-previous-visual-line
      "j" 'evil-next-visual-line
      ;; "o" '(lambda () (interactive) (sync0-evil-org-eol-call 'sync0-clever-insert-item))
      ;; "O" '(lambda () (interactive) (sync0-evil-org-eol-call 'org-insert-heading))
      "$" 'evil-end-of-visual-line
      "^" 'evil-beginning-of-visual-line
      "[" 'evil-backward-sentence-begin
      "]" 'evil-forward-sentence-begin
      "{" 'markdown-backward-paragraph
      "}" 'markdown-forward-paragraph)

    :bind ((:map markdown-mode-map
                 ("M-<right>" . markdown-demote)
                 ("M-<left>" . markdown-promote))))

(use-package web-mode
  :straight (web-mode :type git :host github :repo "fxbois/web-mode") 
  :mode
  (("\\.phtml\\'" . web-mode)
   ;; ("\\.jsx\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (require 'sync0-web-mode-functions))

(use-package emmet-mode
  :custom
  (emmet-move-cursor-between-quotes t)
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook #'emmet-mode) 
  ;; Auto-start on web mode
  (add-hook 'web-mode-hook #'emmet-mode) 
  ;; enable Emmet's css abbreviation.
  (add-hook 'css-mode-hook  #'emmet-mode))

(use-package neotree
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)

  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

  (evil-leader/set-key
    "t" 'neotree-project-dir)
  
  (evil-define-key 'normal neotree-mode-map
    "q" 'neotree-hide
    "I" 'neotree-hidden-file-toggle
    "z" 'neotree-stretch-toggle
    "R" 'neotree-refresh
    "m" 'neotree-rename-node
    "c" 'neotree-create-node
    "d" 'neotree-delete-node
    "s" 'neotree-enter-vertical-split
    "S" 'neotree-enter-horizontal-split
    (kbd "RET") 'neotree-enter))

  (use-package paren
    :straight nil
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

  (use-package flycheck
    :commands flycheck-mode
    :config
    (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package lsp-mode
  :commands lsp
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-tex-server 'digestif)
  :hook ((python-mode . lsp)
         ;; (typescript-mode . lsp)
         ;; (js2-mode . lsp)
         (js-mode . lsp)
         (LaTeX-mode . lsp)
         ;; (tex-mode . lsp-deferred)
         ;; (nxml-mode . lsp)
         ;; (emacs-lisp-mode . lsp-deferred)
         (web-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package company-lsp
  :after company-mode
  :config
  (setq company-lsp-cache-candidates 'auto
        company-lsp-async t
        company-lsp-enable-snippet nil
        company-lsp-enable-recompletion t))

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

  (use-package company
    :hook
    (after-init . global-company-mode)
    :custom
    (company-idle-delay 0.1)
    (company-minimum-prefix-length 2)
    (company-tooltip-limit 10)
    (company-tooltip-align-annotations t)
    (company-require-match 'never)
    (company-global-modes '(not erc-mode message-mode deft-mode help-mode gud-mode neotree-mode))
    ;; (company-frontends '(company-pseudo-tooltip-frontend 
    ;;                      company-echo-metadata-frontend))  
    (company-backends '(company-capf))
    (company-auto-complete nil)
    :config
    ;; Disable company-mode in bibtex-mode (clashes with yasnippets)
    (add-hook 'bibtex-mode-hook (company-mode -1))

    (define-key company-active-map (kbd "M-j") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)

    (defvar +company-backend-alist
      '((text-mode company-capf company-yasnippet)
        (markdown-mode company-capf company-yasnippet company-ispell)
        (org-mode company-capf company-yasnippet company-elisp company-ispell)
        ;; '((text-mode company-capf  company-yasnippet company-org-roam)
        ;; '((text-mode company-capf  company-yasnippet company-ispell company-org-roam)
        ;; '((text-mode company-capf company-dabbrev company-yasnippet company-ispell company-org-roam)
        ;;(text-mode company-capf company-yasnippet company-ispell company-bibtex)
        (graphviz-dot-mode company-capf company-graphviz-dot-backend company-yasnippet)
        (prog-mode company-capf company-lsp company-yasnippet company-files)
        (elisp-mode company-elisp company-capf company-yasnippet company-files)
        ;; (nxml-mode company-capf company-yasnippet company-nxml)
        ;; (python-mode company-capf company-yasnippet company-jedi)
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

(use-package browse-url
  :straight nil
  :custom
  (browse-url-browser-function 'browse-url-default-browser)
  (browse-url-chrome-program "google-chrome-stable")
  (browse-url-generic-program "google-chrome-stable"))

(use-package palette
  :straight nil
  :init
  (require 'hexrgb)
  :commands palette
  :load-path "~/.emacs.d/sync0/")

(use-package lsp-jedi
  :straight (lsp-jedi :type git :host github :repo "fredcamps/lsp-jedi")
  :config
  (with-eval-after-load "lsp-mode"
    ;; (add-to-list 'lsp-enabled-clients 'jedi)
    (add-to-list 'lsp-disabled-clients 'pyls)))

(use-package py-autopep8
  :straight (py-autopep8 :type git :host github :repo "paetzke/py-autopep8.el") 
  :config
  (setq py-autopep8-options '("--max-line-length=100")))

(use-package python
  :straight nil
  ;; :custom 
  ;; (jedi:setup-keys t)
  ;; (jedi:complete-on-dot t)
  :config
  ;; (add-hook 'python-mode-hook 'jedi:setup)
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
     (bibtex-mode . yas-minor-mode)))

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
  ;; (define-key LaTeX-mode-map (kbd "M-p")
  ;;   (lambda ()
  ;;     "Save the buffer and run `TeX-command-run-all`."
  ;;     (interactive)
  ;;     (save-buffer)
  ;;     (TeX-command-run-all nil)))

  (require 'sync0-tex)

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

(use-package bibtex
  :straight nil
  :custom
  ;; (bibtex-dialect 'biblatex) ;; biblatex as default bib format
  (bibtex-maintain-sorted-entries t)
  (bibtex-field-delimiters 'braces)
  (bibtex-entry-delimiters 'braces)
  ;; This line is necessary to prevent strange problem
  ;; caused by lack of support for my bibtex key naming scheme
  ;; (bibtex-entry-maybe-empty-head t)
  (bibtex-comma-after-last-field t)
  (bibtex-text-indentation 0)
  (bibtex-autokey-names 0)
  (bibtex-autokey-name-length 0)
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-titleword-length 0)
  (bibtex-autokey-year-length 0)
  (bibtex-autokey-titlewords 0)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 22)
  (bibtex-entry-format '(opts-or-alts page-dashes whitespace braces last-comma inherit-booktitle delimiters sort-fields realign))
  :init
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 9999)))
  :config
  (bibtex-set-dialect 'biblatex)
  (require 'bibtex-completion)
  (require 'bibtex-utils)
  (require 'sync0-bibtex-functions)
  (require 'sync0-bibtex-fields)
  (require 'scihub)

  (setq scihub-homepage "https://sci-hub.ee/")
  (setq scihub-download-directory sync0-pdfs-folder)
  (setq scihub-open-after-download nil)

  (setq bu-keywords-values sync0-bibtex-completion-keywords)

  (setq bibtex-autokey-prefix-string (format-time-string "%Y%m%d%H%M%S"))

  (unbind-key "TAB" bibtex-mode-map)

  ;; (defvar sync0-bibtex-reference-keys
  ;;   (lazy-completion-table sync0-bibtex-reference-keys
  ;;                          (lambda () (sync0-bibtex-parse-keys nil t)))
  ;;   "Completion table for BibTeX reference keys.
  ;;  The CDRs of the elements are t for header keys and nil for crossref keys.")


  (evil-define-key 'normal bibtex-mode-map
    "K" 'sync0-bibtex-previous-key
    "J" 'sync0-bibtex-next-key))

(use-package bibtex-completion
  :custom 
  (bibtex-completion-bibliography '("~/Dropbox/bibliographies/bibliography.bib"
				    "~/Dropbox/bibliographies/doctorat.bib")) 
  (bibtex-completion-notes-path '"~/Dropbox/obsidian")
  ;; (bibtex-completion-notes-path '"~/Dropbox/org/permanent")
  (bibtex-completion-library-path '("~/Documents/pdfs/"))
  (bibtex-completion-pdf-field "file")
  (bibtex-completion-pdf-symbol "P")
  (bibtex-completion-notes-symbol "N")
  (bibtex-completion-notes-extension ".md")
  ;; (bibtex-completion-notes-extension ".org")
  (bibtex-completion-pdf-extension '(".pdf" ".epub"))
  (bibtex-completion-additional-search-fields '(journaltitle origdate subtitle volume location publisher note library institution keywords edition))
  ;; (bibtex-completion-additional-search-fields '(editor journaltitle origdate subtitle volume booktitle location publisher note library medium institution keywords))

  :config 
  (setq bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title}: ${subtitle} @ ${journaltitle} [${=key=}]")
	  (mvbook          . "${=has-pdf=:1}${=has-note=:1}| ${author} [${origdate}](${date:4}) ${title} ${volume}: ${subtitle} Ed. ${edition} [${=key=}]")
	  (book          . "${=has-pdf=:1}${=has-note=:1}| ${author} [${origdate}](${date:4}) ${title} ${volume}: ${subtitle} Ed. ${edition} [${=key=}]")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} [${=key=}]")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} [${=key=}]")
	  (collection    . "${=has-pdf=:1}${=has-note=:1}| ${editor} (${date:4}) ${title:55} ${volume}: ${subtitle} [${=key=}]")
	  (mvcollection    . "${=has-pdf=:1}${=has-note=:1}| ${editor} (${date:4}) ${title:55} ${volume}: ${subtitle} [${=key=}]")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} [${=key=}]")
	  (t             . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title}: ${subtitle} [${=key=}]")))

  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))

  (setq bibtex-completion-notes-template-multiple-files  
        (concat 
	   "---\n"
         "citekey: ${=key=}\n"
         "biblatex_type: ${=type=}\n"
         "zettel_type: reference\n"
         "aliases: [\"%(sync0-bibtex-extract-lastname (sync0-bibtex-completion-reverse-author ${=key=})) (${date}) ${title} : ${subtitle}\"]\n"
         "created: %(format-time-string \"%Y-%m-%d\")\n"
         "parent: \"${booktitle}\"\n" 
         "parent: \"${journaltitle}\"\n"
         "author: \"${author-or-editor}\"\n"
         "language: ${language}\n"
         "date: ${date}\n"
         "tags: [reference/${=type=},bibkey/${=key=},author,language/${language},date/${date}]\n"
         "---\n"
         "# %(sync0-bibtex-extract-lastname (sync0-bibtex-completion-reverse-author ${=key=})) (${date}) ${title} : ${subtitle}\n\n"
         "## Description\n\n"
         "## Progrès de la lecture\n\n"
         "## Annotations\n\n"
         "## Références\n\n"))

  (use-package ivy-bibtex
    :config
    (setq ivy-bibtex-default-action 'ivy-bibtex-show-entry)
    (setq ivy-bibtex-default-multi-action 'ivy-bibtex-show-entry)
    (require 'sync0-ivy-bibtex-functions)))

   (use-package pdf-tools
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
