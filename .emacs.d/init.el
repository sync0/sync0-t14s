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

(with-eval-after-load 'org
(defun sync0-export-tangle ()
  "Shortcut for exporting and tangling the current org-mode buffer."
  (when (equal (buffer-file-name)
               (expand-file-name 
                (concat user-emacs-directory "sync0_emacs.org")))
    (let*  ((contents (progn (org-org-export-as-org)
                             (with-current-buffer "*Org ORG Export*"
                               (buffer-string))))
            (exported-file (make-temp-file "to-tangle-" nil ".org" contents))
            (tangled-file (car (org-babel-tangle-file exported-file))))
               (with-temp-buffer 
                 (insert-file-contents tangled-file)
                 (write-file "init.el")))))

  (add-hook 'after-save-hook 'sync0-export-tangle))

;; (with-eval-after-load 'org
;; (defun sync0-export-tangle ()
;;   "Shortcut for exporting and tangling the current org-mode buffer."
;;   (when (equal (buffer-file-name)
;;                (expand-file-name 
;;                 (concat user-emacs-directory "sync0_emacs.org")))
;;     (let*  ((contents (progn (org-org-export-as-org)
;;                              (with-current-buffer "*Org ORG Export*"
;;                                (buffer-string))))
;;             (exported-file (make-temp-file "to-tangle-" nil ".org" contents))
;;             (tangled-file (car (org-babel-tangle-file exported-file))))
;;                (with-temp-buffer 
;;                  (insert-file-contents tangled-file)
;;                  (write-file "init.el")
;;                (byte-compile-file "init.el")))))

;;   (add-hook 'after-save-hook 'sync0-export-tangle))

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
                          `(org-block-end-line ((t (:inherit org-block-begin-line :weight bold))))
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

(setq sync0-zettelkasten-directory (concat (getenv "HOME") "/Dropbox/org/")
sync0-zettelkasten-directory-sans (concat (getenv "HOME") "/Dropbox/org")
sync0-default-bibliography (concat (getenv "HOME") "/Dropbox/bibliographies/doctorat.bib")
sync0-zettelkasten-directory-references (concat (getenv "HOME") "/Dropbox/org/notes/references/")
sync0-emacs-directory (concat (getenv "HOME") "/.emacs.d/sync0/")
sync0-pdfs-folder (concat (getenv "HOME") "/Documents/pdfs/")
sync0-current-year (format-time-string "%Y")
sync0-current-month (format-time-string "%B")
sync0-current-month-downcase (downcase (format-time-string "%B"))
sync0-current-day (format-time-string "%d")
sync0-biblatex-entry-types '("article" "book" "inbook" "incollection" "collection" "unpublished" "thesis" "proceedings" "inproceedings" "online" "report" "manual")
sync0-biblatex-languages '("spanish" "english" "french" "german" "portuguese" "italian" "korean" "japanese" "dutch" "arabic" "hebrew" "latin" "greek" "unknown")
sync0-english-parts-speech '("noun" "intransitive verb" "transitive verb" "verb" "conjunction" "adjective" "adverb")
sync0-french-parts-speech '("nom féminin" "nom masculin" "verbe intransitif" "verbe transitif" "verbe" "conjonction" "adjectif" "adverbe")
sync0-portuguese-parts-speech '("sustantivo femenino" "sustantivo masculino" "verbo intransitivo" "verbo transitivo" "verbo" "conjunção" "adjetivo" "advérbio")
          sync0-spanish-parts-speech '("sustantivo femenino" "sustantivo masculino" "verbo intransitivo" "verbo transitivo" "verbo" "conjunción" "adjectivo" "adverbio")
          sync0-zettelkasten-annotations-author  "John Doe"
          sync0-zettelkasten-annotations-key  "doe1991"
           sync0-zettelkasten-annotations-roam-key "cite:doe1991")

(defun sync0-log-today-timestamp () 
  "Insert today's date in the YYYY/MM/DD format"
    (insert (format-time-string "%A, %Y/%m/%d")))

  (defun insert-current-day () 
    (insert (format-time-string "%d")))

  (defun insert-current-month () 
    (insert (format-time-string "%B")))

  (defun sync0-insert-today-timestamp () 
    "Insert today's date in the YYYY/MM/DD format"
    (insert (format-time-string "%Y/%m/%d")))

  (defun sync0-org-insert-today-timestamp () 
    "Insert today's date in the YYYY/MM/DD format"
    (insert (format-time-string "<%Y-%m-%d>")))

  (defun sync0-update-timestamp ()
    "Update current #+DATE timestamp"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((regex "[0-9][0-9][0-9][0-9]\/[0-9][0-9]\/[0-9][0-9]")
            (date (shell-command-to-string "echo -n $(date +'%Y/%m/%d')")))
         (when (search-forward "#+DATE:" nil nil 1)
           (if (progn (forward-char 1)(looking-at regex))
               (replace-match date)
             (sync0-insert-today-timestamp))))))

        ;; (when (search-forward "Last modified: &" nil nil 1)
        ;;   (if (progn (forward-char 1)(looking-at regex))
        ;;       (replace-match date)
        ;;     (sync0-insert-today-timestamp)))

  (add-hook 'before-save-hook (lambda ()
;; Check whether file is in org-mode and whether it is located in my Zettelkasten directory
                                (when (and (eq major-mode 'org-mode)
                                      (not (string-prefix-p "archives" (buffer-file-name)))
                                      (string-prefix-p sync0-zettelkasten-directory (buffer-file-name)))
                                               ;; (equal default-directory (concat (getenv "HOME") "/Dropbox/annotations/"))
                                  (sync0-update-timestamp))))

  (defun sync0-zettelkasten-annotations-update-author ()
    "Save current #+AUTHOR for use later on."
    (interactive)
 (when (equal default-directory sync0-zettelkasten-directory-references)
    (save-excursion
      (goto-char (point-min))
         (when (re-search-forward "^#\\+AUTHOR: \\([[:print:]]+\\)$" nil t 1)
             (setq sync0-zettelkasten-annotations-author (match-string-no-properties 1))))))

  (defun sync0-zettelkasten-annotations-update-roam-key ()
    "Save current #+ROAM_KEY for use later on."
    (interactive)
 (when (equal default-directory sync0-zettelkasten-directory-references)
    (save-excursion
      (goto-char (point-min))
         (when (re-search-forward "^#\\+ROAM_KEY: cite:\\([[:print:]]+\\)$" nil t 1)
           (progn
             (setq sync0-zettelkasten-annotations-key (match-string-no-properties 1))
             (setq sync0-zettelkasten-annotations-roam-key (concat "cite:" sync0-zettelkasten-annotations-key)))))))

(add-hook 'find-file-hook 'sync0-zettelkasten-annotations-update-author)
(add-hook 'find-file-hook 'sync0-zettelkasten-annotations-update-roam-key)
(add-hook 'buffer-list-update-hook 'sync0-zettelkasten-annotations-update-author)
(add-hook 'buffer-list-update-hook 'sync0-zettelkasten-annotations-update-roam-key)

(defun sync0-show-file-path ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name)))

;; https://emacs.stackexchange.com/questions/36850/copy-to-kill-ring-selected-file-names-full-path
(defun sync0-dired-copy-path-at-point ()
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
      (interactive)
(progn
      (split-window-below)
      (balance-windows)
                (setq truncate-lines t)
                (setq truncate-partial-width-windows t)
      (other-window 1)))

    (defun sync0-split-and-follow-vertically ()
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

(use-package undo-tree
  ;; :straight nil
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

(use-package ivy
  :hook 
  (after-init . ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) "))

(use-package evil-leader
        :straight (evil-leader :type git :host github :repo "cofi/evil-leader") 
        :init
        (setq evil-leader/in-all-states t)
        :hook (after-init . global-evil-leader-mode)
       :config
         (evil-leader/set-leader "<SPC>")

(require 'ivy)
(require 'counsel)
(require 'org-capture)

(evil-leader/set-key
  "1" 'delete-other-windows
  "2" 'sync0-split-and-follow-horizontally
  "3" 'sync0-split-and-follow-vertically
  "m" 'bookmark-set
  "j" 'counsel-bookmark
  "q" 'keyboard-quit
  "w" 'write-file
  "e" 'eval-last-sexp
  "s" 'save-buffer
  "b" 'ivy-switch-buffer
  "r" 'counsel-recentf
  "y" 'counsel-yank-pop
  "f" 'counsel-find-file
  "x" 'counsel-M-x
  "o" 'other-window
  "p" 'previous-buffer
  "n" 'next-buffer
  "N" 'sync0-find-next-file
  "K" 'kill-buffer
  "k" 'delete-window))

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

(use-package evil  
  ;; :straight (evil :type git :host github :repo "emacs-evil/evil") 
  :hook (after-init . evil-mode)
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

  (add-to-list 'evil-emacs-state-modes 'cfw:details-mode)

  ;; Change global key bindings
  (unbind-key "C-m" evil-normal-state-map)
  (unbind-key "M-." evil-normal-state-map)
  (unbind-key "C-d" evil-motion-state-map)

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
   ;; (kbd "SPC") 'sync0-insert-whitespace
    "zs" 'transpose-sentences)

 (evil-leader/set-key
     "<SPC>" 'sync0-insert-whitespace
     "<ESC>" 'keyboard-quit )

  ;; Improve EVIL behavior with visual lines (visual-line-mode).
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))

(use-package s)

(use-package simple-secrets
:straight nil
    :load-path "~/.emacs.d/sync0/" 
    :after s
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
    :preface
    (defun sync0-save-place-reposition ()
      "Force windows to recenter current line (with saved position)."
      (run-with-timer 0 nil
                      (lambda (buf)
                        (when (buffer-live-p buf)
                          (dolist (win (get-buffer-window-list buf nil t))
                            (with-selected-window win (recenter)))))
                      (current-buffer)))
    ;; Start save-place-mode.
    :init (save-place-mode)
    :hook (find-file . sync0-save-place-reposition))

(use-package el-patch
     :straight (el-patch :type git
                         :host github
                         :repo "raxod502/el-patch"))

  (eval-when-compile
    (require 'el-patch))

  (use-package deft
      :straight (deft :type git :host github :repo "jrblevin/deft") 
      :after (org org-roam)
      :custom
      (deft-recursive t)
      (deft-default-extension "org")
      (deft-directory sync0-zettelkasten-directory-sans)
      (deft-new-file-format "%Y%m%d%H%M%S")
      (deft-file-naming-rules
      '((noslash . "-")
        (nospace . "_")
        (case-fn . downcase)))
    :config/el-patch
    (defun deft-parse-title (file contents)
      "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
      (el-patch-swap (if deft-use-filename-as-title
                         (deft-base-filename file)
                       (let ((begin (string-match "^.+$" contents)))
                         (if begin
                             (funcall deft-parse-title-function
                                      (substring contents begin (match-end 0))))))
                     (org-roam-db--get-title file)))

    (defhydra sync0-hydra-deft-functions (:color amaranth :hint nil :exit t)
      "
   ^Deft^
   ^------------------
   _n_: New file
   _f_: Filter
   _c_: Clear filter
   _d_: Delete file

   [q] Quit
        "
      ("f" deft-filter)
      ("c" deft-filter-clear)
      ("n" deft-new-file)
      ("d" deft-delete-file)
      ("q" nil :color blue))

(evil-leader/set-key-for-mode 'deft-mode "z" 'sync0-hydra-deft-functions/body))

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
  "h" 'sync0-hydra-help/body)

    :bind
    (("M-x" . counsel-M-x)
     ("M-y" . counsel-yank-pop)
     ;; ("<f1>" . sync0-hydra-help/body)
     ("C-x C-f" . counsel-find-file)))

(use-package swiper 
  :after evil
  :commands swiper
  :config 
  (evil-define-key 'normal global-map "/" 'swiper)
  :bind (("C-s" . swiper)))

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

(use-package alert
:straight (alert :type git :host github :repo "jwiegley/alert"))

(use-package warnings
    :straight nil
    :config
;; Remove annoying message when expanding yasnippets. 
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package google-this 
  :straight (google-this :type git :host github :repo "Malabarba/emacs-google-this") 
  :commands (google-this-search google-this)
  :after evil
  ;; :init
  ;; (google-this-mode 1)
  ;; Query google search. 
  :bind (("C-c g" . google-this-search)
         ;; Search selection with google.
         :map evil-visual-state-map ("g"  . google-this)))

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
      (add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
      (add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
      (evil-set-initial-state 'mu4e-compose-mode 'insert)

      (setq mu4e-headers-fields
            '( (:date          .  25)    ;; alternatively, use :human-date
               (:flags         .   10)
               (:from          .  30)
               (:subject       .  nil))) ;; alternatively, use :thread-subject

      ;; Configure contexts
      (require 'mu4e-context)

      (setq mu4e-contexts
            `( ,(make-mu4e-context
                 :name "Principal (carc.sync0)"
                 :enter-func (lambda () (mu4e-message "Entering carc.sync0"))
                 :leave-func (lambda () (mu4e-message "Leaving carc.sync0"))
                 :match-func (lambda (msg)
                               (when msg
                                 (mu4e-message-contact-field-matches
                                  msg '(:from :to :cc :bcc) "carc.sync0@gmail.com")))
                 :vars '(
                         (user-mail-address . "carc.sync0@gmail.com")
                         (mu4e-trash-folder . "/carc.sync0/[carc.sync0]/Trash")
                         (mu4e-refile-folder . "/carc.sync0/[carc.sync0]/All Mail")
                         (mu4e-sent-folder . "/carc.sync0/[carc.sync0]/Sent Mail")
                         (mu4e-drafts-folder . "/carc.sync0/[carc.sync0]/Drafts")
                         (mu4e-maildir-shortcuts . (("/carc.sync0/[carc.sync0]/Trash"       . ?t)
                                                    ("/carc.sync0/[carc.sync0]/Sent Mail" . ?s)
                                                    ("/carc.sync0/INBOX"            . ?i)
                                                    ("/carc.sync0/[carc.sync0]/Drafts"    . ?d)
                                                  ;; ("/carc.sync0/[carc.sync0]/Starred"   . ?r)
                                                    ("/carc.sync0/[carc.sync0]/All Mail"  . ?a)))))

               ,(make-mu4e-context
                 :name "Backup (cantorlunae)"
                 :enter-func (lambda () (mu4e-message "Entering cantorlunae"))
                 :leave-func (lambda () (mu4e-message "Leaving cantorlunae"))
                 :match-func (lambda (msg)
                               (when msg
                                 (mu4e-message-contact-field-matches
                                  msg '(:from :to :cc :bcc) "cantorlunae@gmail.com")))
                 :vars '(
                         (user-mail-address . "cantorlunae@gmail.com")
                         (mu4e-trash-folder . "/cantorlunae/[cantorlunae]/Trash")
                         (mu4e-refile-folder . "/cantorlunae/[cantorlunae]/All Mail")
                         (mu4e-sent-folder . "/cantorlunae/[cantorlunae]/Sent Mail")
                         (mu4e-drafts-folder . "/cantorlunae/[cantorlunae]/Drafts")
                         (mu4e-maildir-shortcuts . (("/cantorlunae/[cantorlunae]/Trash"       . ?t)
                                                    ("/cantorlunae/[cantorlunae]/Sent Mail" . ?s)
                                                    ("/cantorlunae/INBOX"            . ?i)
                                                    ("/cantorlunae/[cantorlunae]/Drafts"    . ?d)
                                                    ("/cantorlunae/[cantorlunae]/All Mail"  . ?a)))))))


      ;; Use imagemagick, if available.
      (when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))

      (setq mu4e-bookmarks `(;; ("\\\\Inbox" "Inbox" ?i)
                             ("flag:flagged" "Flagged messages" ?f)
                             ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                             ("date:today..now" "Today's messages" ?t)
                             ("date:7d..now" "Last 7 days" ?w)
                             ("mime:image/*" "Messages with images" ?p)
                             ("maildir:/cantorlunae/INBOX OR maildir:/carc.sync0/INBOX" "All inboxes" ?i)))


      (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

      (evil-define-key 'normal mu4e-compose-mode-map
        "$" 'evil-end-of-visual-line
        "^" 'evil-beginning-of-visual-line
        "gg" 'mu4e-compose-goto-top
        "G" 'mu4e-compose-goto-bottom
        "]" 'evil-next-visual-line
        "[" 'evil-previous-visual-line)

;; Taken from https://github.com/abo-abo/hydra/wiki/mu4e
(defhydra sync0-hydra-mu4e-headers (:color blue :hint nil)
  "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups 
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------ 
_C_: compose | _}_: next query    | _a_: action  | _|_: thru shl  | _`_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
             | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

  ;; general
  ("n" mu4e-headers-next)
  ("p" mu4e-headers-previous)
  ("[" mu4e-select-next-unread)
  ("]" mu4e-select-previous-unread)
  ("y" mu4e-select-other-view)
  ("R" mu4e-compose-reply)
  ("C" mu4e-compose-new)
  ("F" mu4e-compose-forward)
;;  ("o" my/org-capture-mu4e)                  ; differs from built-in

  ;; search
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("/" mu4e-headers-search-narrow)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ("{" mu4e-headers-query-prev)              ; differs from built-in
  ("}" mu4e-headers-query-next)              ; differs from built-in
  ("C-+" mu4e-headers-split-view-grow)
  ("C--" mu4e-headers-split-view-shrink)

  ;; mark stuff 
  ("!" mu4e-headers-mark-for-read)
  ("?" mu4e-headers-mark-for-unread)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("U" mu4e-mark-unmark-all)
  ("d" mu4e-headers-mark-for-trash)
  ("D" mu4e-headers-mark-for-delete)
  ("m" mu4e-headers-mark-for-move)
  ("a" mu4e-headers-action)                  ; not really a mark per-se
  ("A" mu4e-headers-mark-for-action)         ; differs from built-in
  ("*" mu4e-headers-mark-for-something)

  ("#" mu4e-mark-resolve-deferred-marks)
  ("%" mu4e-headers-mark-pattern)
  ("&" mu4e-headers-mark-custom)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)
  ("t" mu4e-headers-mark-subthread)
  ("T" mu4e-headers-mark-thread)

  ;; miscellany
  ("q" mu4e~headers-quit-buffer)
  ("H" mu4e-display-manual)
  ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

  ;; switches
  ("O" mu4e-headers-change-sorting)
  ("P" mu4e-headers-toggle-threading)
  ("Q" mu4e-headers-toggle-full-search)
  ("V" mu4e-headers-toggle-skip-duplicates)
  ("W" mu4e-headers-toggle-include-related)

  ;; more miscellany
  ("`" mu4e-update-mail-and-index)           ; differs from built-in
  (";" mu4e-context-switch)  
  ("j" mu4e~headers-jump-to-maildir)

  ("." nil))

      ;; we seem to need this to fix the org-store-link issue
       ;; (org-link-set-parameters "mu4e" :follow #'org-mu4e-open :store 
       ;; #'org-mu4e-store-link)

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
  (calendar-day-name-array     ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])
  (calendar-day-abbrev-array   ["Dim." "Lun." "Mar." "Mer." "Jeu." "Ven." "Sam."])
  (calendar-month-name-array   ["Janvier" "Février" "Mars" "Avril" "Mai" "Juin" "Juillet"
                                "Août" "Septembre" "Octobre" "Novembre" "Décembre"])
  (calendar-month-abbrev-array ["Jan." "Fév." "Mars" "Avr." "Mai" "Juin" "Jul." "Août" "Sep." "Oct." "Nov." "Déc."]))

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
  (defvar holiday-korean-holidays nil "Korean holidays")
  (defvar holiday-french-holidays nil "French holidays")
  (defvar holiday-colombian-holidays nil "Colombian holidays")

  (setq holiday-korean-holidays
        '((holiday-fixed 1 1          "신정")
          ;; (holiday-lunar-ko 1 nil 1   "설날" -1)
          ;; (holiday-lunar-ko 1 nil 1   "설날")
          ;; (holiday-lunar-ko 1 nil 1   "설날" 1)
          (holiday-fixed 3 1          "3.1절")
          ;; (holiday-lunar-ko 4 nil 8   "석가탄신일")
          (holiday-fixed 5 5          "어린이날")
          (holiday-fixed 6 6          "현충일")
          (holiday-fixed 8 15         "광복절")
          (holiday-fixed 10 3         "개천절")
          (holiday-fixed 10 9         "한글날")
          ;; (holiday-lunar-ko 8 nil 15  "추석" -1)
          ;; (holiday-lunar-ko 8 nil 15  "추석")
          ;; (holiday-lunar-ko 8 nil 15  "추석" 1)
          (holiday-fixed 12 25        "성탄절")))

  (setq holiday-french-holidays
        '((holiday-fixed 1 1 "Jour de l'an")
          (holiday-fixed 1 6 "Épiphanie")
          (holiday-fixed 2 2 "Chandeleur")
          (holiday-fixed 2 14 "Saint Valentin")
          (holiday-fixed 5 1 "Fête du travail")
          (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
          (holiday-fixed 6 21 "Fête de la musique")
          (holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
          (holiday-fixed 8 15 "Assomption (Religieux)")
          (holiday-fixed 11 11 "Armistice de 1918")
          (holiday-fixed 11 1 "Toussaint")
          (holiday-fixed 11 2 "Commémoration des fidèles défunts")
          (holiday-fixed 12 25 "Noël")
          ;; fetes a date variable
          (holiday-easter-etc 0 "Pâques")
          (holiday-easter-etc 1 "Lundi de Pâques")
          (holiday-easter-etc 39 "Ascension")
          (holiday-easter-etc 49 "Pentecôte")
          (holiday-easter-etc -47 "Mardi gras")
          (holiday-float 5 0 4 "Fête des mères")
          ;; dernier dimanche de mai ou premier dimanche de juin si c'est le
          ;; même jour que la pentecôte TODO
          (holiday-float 6 0 3 "Fête des pères")))
  ;; troisième dimanche de juin

  (setq holiday-colombian-holidays 
        '((holiday-fixed 1 1 "Año nuevo")
          (holiday-sexp '(calendar-nth-named-day 1 1 1 year 6) "Día de Reyes")
          (holiday-sexp '(calendar-nth-named-day 1 1 3 year 19) "Día de San José")
          (holiday-easter-etc -3 "Jueves Santo")
          (holiday-easter-etc -2 "Viernes Santo")
          (holiday-fixed 5 1 "Día del trabajo")
          (holiday-easter-etc +43 "Día de la ascención")
          (holiday-sexp '(calendar-nth-named-day 1 1 6 year 29)
                        "San Pedro y San Pablo")
          (holiday-easter-etc +64 "Corpus Christi")
          (holiday-easter-etc +71 "Sagrado corazón")
          (holiday-fixed 7 20 "Día de la independencia")
          (holiday-fixed 8 7 "Batalla de Boyacá")
          (holiday-sexp '(calendar-nth-named-day 1 1 8 year 15)
                        "Asunción de la virgen")
          (holiday-sexp '(calendar-nth-named-day 1 1 10 year 12) "Día de la raza")
          (holiday-sexp '(calendar-nth-named-day 1 1 11 year 1)
                        "Todos los santos")
          (holiday-sexp '(calendar-nth-named-day 1 1 11 year 11)
                        "Independencia de Cartagena")
          (holiday-fixed 12 25 "Navidad")
          (holiday-fixed 12 8 "Inmaculada concepción")))

  (setq holiday-other-holidays
        (append holiday-colombian-holidays holiday-french-holidays holiday-korean-holidays))

  (setq calendar-holidays
        (append holiday-general-holidays holiday-other-holidays)))

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

  ;; (defun sync0-open-calendar ()
  ;;   (interactive)
  ;;   (let ((buf (get-buffer "*cfw-calendar*")))
  ;;     (if buf
  ;;         (pop-to-buffer buf nil)
  ;;       (cfw:open-calendar-buffer
  ;;        :contents-sources
  ;;        (list (cfw:org-create-source "#c0c5ce")) :view 'week))))

  ;; (setq sync0-org-agenda-files 
  ;;       (let ((agenda-files   (org-agenda-files nil 'ifmode)))
  ;;         (delete "~/Dropbox/org/etc/Habits.org"  agenda-files)
  ;;         (delete "~/Dropbox/org/messages"  agenda-files)))

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

(evil-leader/set-key
  "C" 'cfw:open-org-calendar))

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

(use-package git-gutter 
      :straight (git-gutter :type git :host github :repo "emacsorphanage/git-gutter") 
      :commands git-gutter-mode
      ;; :init
      ;; (global-git-gutter-mode +1)
      ;; :hook 
      ;; (text-mode . git-gutter-mode)
      ;; (prog-mode . git-gutter-mode)
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
    :after evil
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
      '((text-mode company-capf  company-yasnippet company-org-roam)
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
(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
)

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
    (require 'ivy-bibtex)
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

(use-package org-journal 
    :straight (org-journal :type git :host github :repo "bastibe/org-journal") 
    :custom
    ;; Set default directory to search for journal files. 
    ;;(org-journal-dir (concat sync0-dropbox-directory "org"))
    (org-journal-dir (concat sync0-dropbox-directory "org/journal"))
    ;; Delete the date prefix to new journal entries.
    (org-journal-time-format "")
    ;; Create one journal file per month. 
    (org-journal-file-type 'daily)
    ;; Change the title of journal files to the format: "YYYY_MM.gpg".
    (org-journal-file-format "%Y%m%d.org")
    ;; Change the format of journal entries (org headlines) to "[Day], DD/MM/YYYY".
    ;; (org-journal-date-format "%A, %Y/%m/%d")
    (org-journal-date-format "%A")
    ;; Encrypt journal files.
    (org-journal-encrypt-journal nil)
    ;; Don't encript individual entires in journal files. It's too cumbersome. 
    (org-journal-enable-encryption nil)
    (org-journal-carryover-items "TODO=\"無\"|TODO=\"次\"|TODO=\"中\"|TODO=\"待\"|TODO=\"阻\"")
    (org-journal-enable-agenda-integration nil)
    (org-journal-file-header "#+TITLE: %A, %Y/%m/%d\n#+CREATED: %Y/%m/%d\n#+DATE: %Y/%m/%d\n#+ROAM_TAGS: journal %Y %B\n\n")

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
   _o_ther date            
   _p_revious note
   _n_ext note

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
        :after (org all-the-icons)
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
        (org-agenda-window-setup 'only-window )
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
        (org-agenda-deadline-leaders (quote ("!" "%-1d日<" "%-1d日>")))
        (org-agenda-scheduled-leaders (quote ("!" "?")))

        :preface

        (defun sync0--org-agenda-goto-header (&optional backwards)
          "Find the next agenda series header forwards or BACKWARDS."
          (let ((pos (save-excursion
                       (goto-char (if backwards
                                      (line-beginning-position)
                                    (line-end-position)))
                       (let* ((find-func (if backwards
                                             'previous-single-property-change
                                           'next-single-property-change))
                              (end-func (if backwards 'max 'min))
                              (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                                 (funcall find-func (point) 'org-agenda-date-header)))
                              (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                              (prop-pos (if all-pos (apply end-func all-pos) nil)))
                         prop-pos))))
            (if pos (goto-char pos))
            (if backwards (goto-char (line-beginning-position)))))

        (defun sync0-number-to-month (arg &optional no-abbrev)
          "Helper function to convert a number into the month name"
          (if no-abbrev
              (cond ((equal 1 arg) "Janvier")
                    ((equal 2 arg) "Février")
                    ((equal 3 arg) "Mars")
                    ((equal 4 arg) "Avril")
                    ((equal 5 arg) "Mai")
                    ((equal 6 arg) "Juin")
                    ((equal 7 arg) "Juillet")
                    ((equal 8 arg) "Août")
                    ((equal 9 arg) "Septembre")
                    ((equal 10 arg) "Octobre")
                    ((equal 11 arg) "Novembre")
                    ((equal 12 arg) "Décembre")
                    (t "nil"))
            (cond ((equal 1 arg) "Jan.")
                  ((equal 2 arg) "Fév.")
                  ((equal 3 arg) "Mars")
                  ((equal 4 arg) "Avr.")
                  ((equal 5 arg) "Mai")
                  ((equal 6 arg) "Juin")
                  ((equal 7 arg) "Jul.")
                  ((equal 8 arg) "Août")
                  ((equal 9 arg) "Sep.")
                  ((equal 10 arg) "Oct.")
                  ((equal 11 arg) "Nov.")
                  ((equal 12 arg) "Déc.")
                  (t "nil"))))

        (defun sync0-number-to-day (arg &optional no-abbrev)
          "Helper function to convert a number into the day name"
          (if no-abbrev
              (cond ((equal 1 arg) "Dimanche")
                    ((equal 2 arg) "Lundi")
                    ((equal 3 arg) "Mardi")
                    ((equal 4 arg) "Mercredi")
                    ((equal 5 arg) "Jeudi")
                    ((equal 6 arg) "Vendredi")
                    ((equal 7 arg) "Samedi")
                    (t "nil"))
            (cond ((equal 1 arg) "Dim.")
                  ((equal 2 arg) "Lun.")
                  ((equal 3 arg) "Mar.")
                  ((equal 4 arg) "Mer.")
                  ((equal 5 arg) "Jeu.")
                  ((equal 6 arg) "Ven.")
                  ((equal 7 arg) "Sam.")
                  (t "nil"))))

        :config

        ;; workaround developed by some smart user to circumvent org-agenda's slow performance
        ;; (run-with-idle-timer 5 nil (lambda () (org-agenda-list) (delete-window)))

        ;; Set icons for use in agenda views. 
        (setq org-agenda-category-icon-alist `(
                                               ("[Tt][aâ]ches" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
                                               ("[Tt]asks" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
                                               ("[Cc]hores" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
                                               ("[Mh][ée]nage" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
                                               ("[Hh]abitudes" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                               ("[Hh]abits" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                               ("[ÉEée]tudes" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                               ("[Hh]historiographie" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                               ("[Cc]lasses" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                               ("[ÉEée]v[eé]nements" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center)
                                               ("[Ee]vents" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center)
                                               ("[Ee]macs" ,(list (all-the-icons-material "code" :height 1.2)) nil nil :ascent center)
                                               ("[Ll]yon" ,(list (all-the-icons-material "pageview" :height 1.2)) nil nil :ascent center)
                                               ("[Ss]ocialism" ,(list (all-the-icons-material "build" :height 1.2)) nil nil :ascent center)
                                               ("[Ss]ocialisme" ,(list (all-the-icons-material "build" :height 1.2)) nil nil :ascent center)
                                               ("[Oo]utils" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                               ("[Tt]ools" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                               ("[Tt]ravail" ,(list (all-the-icons-material "business_center" :height 1.2)) nil nil :ascent center)
                                               ("[Dd]octorat" ,(list (all-the-icons-material "school" :height 1.2)) nil nil :ascent center)
                                               ("[Ss]efardi" ,(list (all-the-icons-material "timeline" :height 1.2)) nil nil :ascent center)
                                               ("[Dd]iary" ,(list (all-the-icons-material "today" :height 1.2)) nil nil :ascent center)
                                               ("[Mm]essages" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                               ("[Mm]essages ([[:graph:]]+)" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                               ("[Mm]\. ([[:graph:]]+)" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                               ("[EÉée]criture" ,(list (all-the-icons-material "create" :height 1.2)) nil nil :ascent center)
                                               ("[Ww]riting" ,(list (all-the-icons-material "create" :height 1.2)) nil nil :ascent center)
                                               ("[Pp]ortuguês" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                               ("[Ff]rançais" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                               ("[Ee]spañol" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                               ("[Ee]nglish" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                               ("[Bb]log" ,(list (all-the-icons-material "speaker_notes" :height 1.2)) nil nil :ascent center)))

        (defun sync0-org-agenda-format-date-aligned (date)
          "Format a DATE string for display in the daily/weekly agenda, or timeline.
                         This function makes sure that dates are aligned for easy reading."
          (require 'cal-iso)
          (let* ((dayname (calendar-day-name date nil nil))
                 (day (cadr date))
                 ;; (french (calendar-french-date-string date))
                 ;; (french (substring (calendar-french-date-string date) 0 -6))
                 (day-of-week (calendar-day-of-week date))
                 (month (car date))
                 (monthname (calendar-month-name month nil))
                 (year (nth 2 date))
                 (iso-week (org-days-to-iso-week
                            (calendar-absolute-from-gregorian date)))
                 (weekyear (cond ((and (= month 1) (>= iso-week 52))
                                  (1- year))
                                 ((and (= month 12) (<= iso-week 1))
                                  (1+ year))
                                 (t year)))
                 (weekstring (if (= day-of-week 1)
                                 (format " W%02d" iso-week)
                               "")))
            (format "%-2s %2d %s"
                    dayname day monthname)))

        (setq org-agenda-format-date 'sync0-org-agenda-format-date-aligned)

        ;; Set of functions to have evil bindings in org-agenda.
        (defun sync0-org-agenda-next-header ()
          "Jump to the next header in an agenda series."
          (interactive)
          (sync0--org-agenda-goto-header))

        (defun sync0-org-agenda-previous-header ()
          "Jump to the previous header in an agenda series."
          (interactive)
          (sync0--org-agenda-goto-header t))

        ;; Fast access agenda view.
        (defun sync0-pop-to-org-agenda (&optional split)
          "Visit the org agenda, in the current window or a SPLIT."
          (interactive "P")
          (org-agenda nil "h")
          (when (not split)
            (delete-other-windows)))

        (defun sync0-org-agenda-get-timestamp-time ()
          "Get timestamp from current org-agenda time"
          ;; Firs, determine whether the headline has both a schedule and
          ;; deadeline?
          ;; 
          ;; NOTE: The first part of the conditional (the "((and ...)"
          ;; part) has schedules take precedence over deadelines based on
          ;; the assumption that headlines are scheduled so as to be
          ;; accomplished before the deadline. Therefore, although
          ;; deadlines could occur before schedules, displaying this
          ;; information in the org-agenda would not offer any useful
          ;; information for planning purpose. In such cases, for real
          ;; tasks the headline would be eventually re-scheduled so as to
          ;; observe the rule that schedules take precedence over
          ;; deadlines. 
          ;;  
          ;;  When both a schedule and a deadline have been defined:
          (cond ((and (org-get-scheduled-time (point))
                      (org-get-deadline-time (point)))
                 (let* ((schedule (org-get-scheduled-time (point)))
                        (tomorrow-string  (shell-command-to-string "echo -n $(date -d tomorrow +'%Y-%m-%d')"))
                        (org-schedule-string  (format-time-string "%Y-%m-%d" schedule))
                        (month-start-name (capitalize (format-time-string "%b" schedule)))
                        (day-start (format-time-string "%d" schedule))
                        (day-start-name (capitalize (format-time-string "%a" schedule)))
                        (time-start-test (format-time-string "%H:%M" schedule))
                        (time-start (if (string= "00" (format-time-string "%M" schedule)) 
                                        (format-time-string "%Hh" schedule)
                                      (format-time-string "%H:%M" schedule)))
                        (deadline (org-get-deadline-time (point)))
                        (org-deadline-string (format-time-string "%Y-%m-%d" deadline))
                          (day-end-raw (format-time-string "%d" deadline))
                          (day-end (if (string-match "0\\([[:digit:]]$\\)" day-end-raw)
                                      (match-string 1 day-end-raw) day-end-raw))
                        ;; (day-end  (format-time-string "%d" deadline))
                        (day-end-name (capitalize (format-time-string "%a" deadline)))
                        (time-end-test (format-time-string "%H:%M" deadline))
                        (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                                      (format-time-string "%Hh" deadline)
                                    (format-time-string "%H:%M" deadline))))
                   ;; Test whether the hour and minute "%H:%M" string is
                   ;; relevant and, thus, should be included in org-agenda
                   ;; views.
                   (cond ((and  (string= "00:00" time-start-test)
                                (string= "00:00" time-end-test))
                          (if (or (string= tomorrow-string org-schedule-string)
                                  (string= tomorrow-string org-deadline-string))
                              (concat "Demain" "-" day-end " " month-start-name)
                            (concat day-start-name " " day-start "-" day-end " " month-start-name)))
                         ((or (not (string= "00:00" time-start-test))
                              (not (string= "00:00" time-end-test)))
                          (if (or (string= tomorrow-string org-schedule-string)
                                  (string= tomorrow-string org-deadline-string))
                              (concat "Demain" "-" day-end " " month-start-name ", " time-start "-" time-end)
                            (concat day-start "-" day-end " " month-start-name ", " time-start "-" time-end))))))
                ;; Second part, when either schedule or deadline have been
                ;; defined:
                ((or (org-get-scheduled-time (point))
                     (org-get-deadline-time (point)))
                 ;; Follow the convention that schedules take precedence
                 ;; over deadlines. If schedule has been defined:
                 (if (org-get-scheduled-time (point))
                     (let* ((schedule (org-get-scheduled-time (point)))
                            (tomorrow-string  (shell-command-to-string "echo -n $(date -d tomorrow +'%Y-%m-%d')"))
                            (org-schedule-string (format-time-string "%Y-%m-%d" schedule))
                            ;; Define the object "scheduled" containing the date
                            ;; information from which all the other variables wiil be
                            ;; defined.
                            (element (org-element-at-point))
                            (scheduled (org-element-property :scheduled element))
                            (year-start (org-element-property :year-start scheduled))
                            (year-start-string (number-to-string year-start))  
                            (year-end (org-element-property :year-end scheduled))
                            (year-end-string (if year-end (number-to-string year-end) year-start-string))
                            (month-start (org-element-property :month-start scheduled))
                            (month-start-string (number-to-string month-start))
                            (month-start-name (sync0-number-to-month month-start))
                            (month-start-name-full (sync0-number-to-month month-start t))
                            (month-end (org-element-property :month-end scheduled))
                            (month-end-string (if month-end (number-to-string month-end) "0"))
                            (month-end-name (sync0-number-to-month month-end))
                            (month-end-name-full (sync0-number-to-month month-end t))
                            (day-start  (org-element-property :day-start scheduled))
                            (day-start-string (number-to-string day-start)) 
                            ;; (day-start-string (if (<= day-start 9) 
                            ;;                       (concat "0" (number-to-string day-start))
                            ;;                               (number-to-string day-start)))
                            (day-start-name   (calendar-day-name (list month-start day-start year-start)))
                            (day-start-name-abbrev   (calendar-day-name (list month-start day-start year-start) t))
                            (day-end (org-element-property :day-end scheduled))
                            (day-end-string (when day-end (number-to-string day-end)))
                            (day-end-name  (calendar-day-name (list month-end day-end year-end)))
                            (day-end-name-abbrev  (calendar-day-name (list month-end day-end year-end) t))
                            (hour-start (org-element-property :hour-start scheduled))
                            (hour-start-string (if hour-start (number-to-string hour-start) "0"))
                            (hour-end (org-element-property :hour-end scheduled))
                            (hour-end-string (if hour-end (number-to-string hour-end) "0"))
                            (minute-start (org-element-property :minute-start scheduled))
                            (minute-start-string (if minute-start (number-to-string minute-start) "0"))
                            (minute-end (org-element-property :minute-end scheduled)) 
                            (minute-end-string (if minute-end (number-to-string minute-end) "0"))
                            (time-end-test (concat hour-end-string ":" minute-end-string))
                            (time-end (if (string= "0" minute-end-string) 
                                          (concat hour-end-string "h")
                                        (concat hour-end-string ":" minute-end-string)))
                            (time-start-test (concat hour-start-string ":" minute-start-string))
                            (time-start (if (string= "0" minute-start-string) 
                                            (if time-end (concat hour-start-string "")
                                              (concat hour-start-string "h"))
                                          (concat hour-start-string ":" minute-start-string))))
                       ;; First, let's see what to do when the schedule is not on the same day 
                       (cond 
                        ((and (string= month-start-string month-end-string)
                              (not (string= day-start-string day-end-string))
                              (not (string= time-start-test "0:0"))
                              (not (string= time-end-test "0:0")))
                         ;; same-month, different-day, time-start, time-end
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain, " time-start "-" time-end)
                           (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)))
                        ((and (string= month-start-string month-end-string)
                              (not (string= day-start-string day-end-string))
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain - " day-end-string ", " time-start "-" time-end)
                           ;; same-month, different-day, time-start
                           (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)))
                        ((and (string= month-start-string month-end-string)
                              (not (string= day-start-string day-end-string))
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         ;; same-month, different-day
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain - " day-end-string " " month-start-name-full)
                           (concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full)))
                        ((and (string= month-start-string month-end-string)
                              (string= day-start-string day-end-string)
                              (not (string= time-start-test "0:0"))
                              (not (string= time-end-test "0:0")))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain, " time-start "-" time-end)
                           ;; same-month, same-day, time-start, time-end 
                           (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start "-" time-end)))
                        ((and (string= month-start-string month-end-string)
                              (string= day-start-string day-end-string)
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain, " time-start)
                           ;; same-month, same-day, time-start
                           (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start)))
                        ((and (string= month-start-string month-end-string)
                              (string= day-start-string day-end-string)
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain")
                           ;; same-month, same-day, same-year
                           (concat day-start-name-abbrev " " day-start-string " " month-start-name-full)))
                        ((and (not (string= month-start-string month-end-string))
                              (not (string= day-start-string day-end-string))
                              (not (string= time-start-test "0:0"))
                              (not (string= time-end-test "0:0")))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain, " time-start " " day-end-string " " month-end-name ", " time-end)
                           ;; different-month, different-day, time-start, time-end
                           (concat day-start-string " " month-start-name ", " time-start " - " day-end-string " " month-end-name ", " time-end)))
                        ((and (not (string= month-start-string month-end-string))
                              (not (string= day-start-string day-end-string))
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain, " time-start " - " day-end-string " " month-end-name)
                           ;; different-month, different-day, time-start
                           (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name)))
                        ((and (not (string= month-start-string month-end-string))
                              (not (string= day-start-string day-end-string))
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         (if (string= tomorrow-string org-schedule-string)
                             (concat "Demain"  " - " day-end-name-abbrev " " day-end-string " " month-end-name)
                           ;; different-month, different-day
                           (concat day-start-name-abbrev " " day-start-string " " month-start-name " - " day-end-name-abbrev " " day-end-string " " month-end-name)))))
                   ;; If deadline has been defined:
                   (let* ((deadline (org-get-deadline-time (point)))
                          (org-deadline-string  (format-time-string "%Y-%m-%d" deadline))
                          (tomorrow-string  (shell-command-to-string "echo -n $(date -d tomorrow +'%Y-%m-%d')"))
    ;; Eliminate the annoying zeroes at the beginning
                          (day-end-raw (format-time-string "%d" deadline))
                          (day-end (if (string-match "0\\([[:digit:]]$\\)" day-end-raw)
                                      (match-string 1 day-end-raw) day-end-raw))
                          (month-end-name-abbrev  (capitalize (format-time-string "%b" deadline)))
                          (month-end-name  (capitalize (format-time-string "%B" deadline)))
                          (day-end-name  (capitalize (format-time-string "%a" deadline)))
                          (time-end-test (format-time-string "%H:%M" deadline))
                          (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                                        (format-time-string "%Hh" deadline)
                                      (format-time-string "%H:%M" deadline))))
                     (cond ((and (string= "00:00" time-end-test)
                                 (string= tomorrow-string org-deadline-string))
                            (concat "Demain")) 
                           ((and (not (string= "00:00" time-end-test))
                                 (string= tomorrow-string org-deadline-string))
                            (concat "Demain, " time-end)) 
                           ((and  (string= "00:00" time-end-test)
                                  (not (string= tomorrow-string org-deadline-string)))
                            (concat day-end-name " " day-end " " month-end-name)) 
                           ((and (not (string= "00:00" time-end-test))
                                 (not (string= tomorrow-string org-deadline-string)))
                            (concat day-end-name " " day-end " " month-end-name ", " time-end))))))
                ;; If neither schedule nor deadline have been defined:
                (t " ")))


        (defun sync0-org-agenda-get-project-timestamp-time-today ()
          "Get timestamp from current org-agenda time"
          ;; Check whether both schedule and deadline are defined.
          (cond ((and (org-get-scheduled-time (point))
                      (org-get-deadline-time (point)))
                 (let* ((schedule (org-get-scheduled-time (point)))
                        (month-start-name (capitalize (format-time-string "%b" schedule)))
                        (day-start (capitalize (format-time-string "%d" schedule)))
                        (day-start-name (capitalize (format-time-string "%a" schedule)))
                        (time-start-test (format-time-string "%H:%M" schedule))
                        (time-start (if (string= "00" (format-time-string "%M" schedule)) 
                                        (format-time-string "%Hh" schedule)
                                      (format-time-string "%H:%M" schedule)))
                        (deadline (org-get-deadline-time (point)))
                        (month-end-name (capitalize (format-time-string "%b" deadline)))
                        (day-end (capitalize (format-time-string "%d" deadline)))
                        (day-end-name (capitalize (format-time-string "%a" deadline)))
                        (time-end-test (format-time-string "%H:%M" deadline))
                        (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                                      (format-time-string "%Hh" deadline)
                                    (format-time-string "%H:%M" deadline))))
                   ;; Stylize the date output
                   (cond ((and  (string= "00:00" time-start-test)
                                (string= "00:00" time-end-test))
                          (if (equal month-start-name month-end-name)
                              (concat day-start-name " " day-start "-" day-end " " month-start-name)
                            (concat day-start " " month-start-name " - " day-end " " month-end-name)))
                         ((or (not (string= "00:00" time-start-test))
                              (not (string= "00:00" time-end-test)))
                          (if (equal month-start-name month-end-name)
                              (concat day-start "-" day-end " " month-start-name ", " time-start "-" time-end)
                            (concat day-start " " month-start-name ", " time-start " " day-end " " month-end-name ", "time-end))))))
                ;; If either schedule or deadline have been defined. 
                ((or (org-get-scheduled-time (point))
                     (org-get-deadline-time (point)))
                 ;; If schedule has been defined.
                 (if (org-get-scheduled-time (point))
                     (let* ((element (org-element-at-point))
                            (scheduled (org-element-property :scheduled element))
                            ;; Get current year (from shell) and convert to number for conditional comparisons
                            (current-year (string-to-number (shell-command-to-string "echo -n $(date +'%Y')")))
                            (year-start (org-element-property :year-start scheduled))
                            (year-start-string (when year-start (number-to-string year-start)))
                            (year-end (org-element-property :year-end scheduled))
                            (year-end-string (if year-end (number-to-string year-end) year-start-string))
                            (month-start (org-element-property :month-start scheduled))
                            (month-start-string (if month-start (number-to-string month-start) "0"))
                            (month-start-name (sync0-number-to-month month-start))
                            (month-start-name-full (sync0-number-to-month month-start t))
                            (month-end (org-element-property :month-end scheduled))
                            (month-end-string (if month-end (number-to-string month-end) "0"))
                            (month-end-name (sync0-number-to-month month-end))
                            (month-end-name-full (sync0-number-to-month month-end t))
                            (day-start (org-element-property :day-start scheduled))
                            (day-start-string (when day-start (number-to-string day-start)))
                            (day-start-name   (calendar-day-name (list month-start day-start year-start)))
                            (day-start-name-abbrev   (calendar-day-name (list month-start day-start year-start) t))
                            (day-end (org-element-property :day-end scheduled))
                            (day-end-string (when day-end (number-to-string day-end)))
                            (day-end-name  (calendar-day-name (list month-end day-end year-end)))
                            (day-end-name-abbrev  (calendar-day-name (list month-end day-end year-end) t))
                            (hour-start (org-element-property :hour-start scheduled))
                            (hour-start-string (if hour-start (number-to-string hour-start) "0"))
                            (hour-end (org-element-property :hour-end scheduled))
                            (hour-end-string (if hour-end (number-to-string hour-end) "0"))
                            (minute-start (org-element-property :minute-start scheduled))
                            (minute-start-string (if minute-start (number-to-string minute-start) "0"))
                            (minute-end (org-element-property :minute-end scheduled)) 
                            (minute-end-string (if minute-end (number-to-string minute-end) "0"))
                            (time-end-test (concat hour-end-string ":" minute-end-string))
                            (time-end (if (string= "0" minute-end-string) 
                                          (concat hour-end-string "h")
                                        (concat hour-end-string ":" minute-end-string)))
                            (time-start-test (concat hour-start-string ":" minute-start-string))
                            (time-start (if (string= "0" minute-start-string) 
                                            (if time-end (concat hour-start-string "")
                                              (concat hour-start-string "h"))
                                          (concat hour-start-string ":" minute-start-string))))

                       ;; First, let's see what to do when the schedule is not on the same day 
                       (cond 
                        ((and (= month-start month-end)
                              (not (= day-start day-end))
                              (not (string= time-start-test "0:0"))
                              (not (string= time-end-test "0:0")))
                         ;; same-month, different-day, time-start, time-end
                         (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end))
                        ((and (= month-start month-end)
                              (= current-year year-end)
                              (not (= day-start day-end))
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         ;; same-month, different-day, time-start, same-year
                         (concat day-start-string "-" day-end-string " " month-start-name ", " time-start))
                        ((and (= month-start month-end)
                              (not (= current-year year-end))
                              (not (= day-start day-end))
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         ;; same-month, different-day, time-start, different-year
                         (concat day-start-string "-" day-end-string " " month-start-name ", " time-start year-end-string))
                        ((and (= month-start month-end)
                              (not (= day-start day-end))
                              (= current-year year-end)
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         ;; same-month, different-day, same-year
                         (concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full))
                        ((and (= month-start month-end)
                              (not (= day-start day-end))
                              (not (= current-year year-end))
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         ;; same-month, different-day, different-year
                         (concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full " " year-end-string))
                        ((and (= month-start month-end)
                              (= day-start day-end)
                              (not (string= time-start-test "0:0"))
                              (not (string= time-end-test "0:0")))
                         ;; same-month, same-day, time-start, time-end 
                         (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start "-" time-end))
                        ((and (= month-start month-end)
                              (= day-start day-end)
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         ;; same-month, same-day, time-start
                         (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start))
                        ((and (= month-start month-end)
                              (= day-start day-end)
                              (= current-year year-end)
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         ;; same-month, same-day, same-year
                         (concat day-start-name-abbrev " " day-start-string " " month-start-name-full))
                        ((and (= month-start month-end)
                              (= day-start day-end)
                              (not (= current-year year-end))
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         ;; same-month, same-day
                         (concat day-start-name-abbrev " " day-start-string " " month-start-name-full " " year-end-string))
                        ((and (not (= month-start month-end))
                              (not (= day-start day-end))
                              (not (string= time-start-test "0:0"))
                              (not (string= time-end-test "0:0")))
                         ;; different-month, different-day, time-start, time-end
                         (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name ", " time-end))
                        ((and (not (= month-start month-end))
                              (not (= day-start day-end))
                              (not (string= time-start-test "0:0"))
                              (string= time-end-test "0:0"))
                         ;; different-month, different-day, time-start
                         (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name))
                        ((and (not (= month-start month-end))
                              (not (= day-start day-end))
                              (string= time-start-test "0:0")
                              (string= time-end-test "0:0"))
                         ;; different-month, different-day
                         (concat day-start-name-abbrev " " day-start-string " " month-start-name " - " day-end-name-abbrev " " day-end-string " " month-end-name))))

                   ;; If deadline has been defined
                   (let* ((deadline (org-get-deadline-time (point)))
                          (element (org-element-at-point))
                          (deadlined (org-element-property :deadline element))
                          (day-end (org-element-property :day-end deadlined))
                          (day-end-string (when day-end (number-to-string day-end)))
                          (month-end-name-abbrev  (capitalize (format-time-string "%b" deadline)))
                          (month-end-name  (capitalize (format-time-string "%B" deadline)))
                          (day-end-name  (capitalize (format-time-string "%a" deadline)))
                          (time-end-test (format-time-string "%H:%M" deadline))
                          (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                                        (format-time-string "%Hh" deadline)
                                      (format-time-string "%H:%M" deadline))))
                     (if (string= "00:00" time-end-test)
                         (concat day-end-name " " day-end-string " " month-end-name) 
                       (concat day-end-name " " day-end-string " " month-end-name ", " time-end)))))
                (t " ")))

        (defun sync0-org-agenda-get-project-timestamp-time ()
          "Get timestamp from current org-agenda time"
          (let* ((schedule (org-get-scheduled-time (point)))
                 (deadline (org-get-deadline-time (point)))
                 (schedule-date (when schedule (let ((time (capitalize (format-time-string "%a %d %b (%H:%M) %Y" schedule)))
                                                     (hour (format-time-string "%H:%M" schedule))
                                                     (time-no-hour (capitalize (format-time-string "%a %d %B %Y" schedule))))
                                                 (if (not (string= "00:00" hour)) time time-no-hour))))
                 ;; For the second block, I use "if" instead of "when" to print a
                 ;; blank when neither "schedules" nor "deadlines" are set.
                 (deadline-date (if deadline (let ((time (capitalize (format-time-string "%a %d %b (%H:%M) %Y" deadline)))
                                                   (hour (format-time-string "%H:%M" deadline))
                                                   (time-no-hour (capitalize (format-time-string "%a %d %B %Y" deadline))))
                                               (if (not (string= "00:00" hour)) time time-no-hour)) "")))
            (if schedule (princ schedule-date) (princ deadline-date))))

        ;; This function was borrowed from Sacha Chua's configuration. 
        (defun sync0-org-agenda-new ()
          "Create a new note or task at the current agenda item. Creates it
                                                                 at
                                                                 the
                                                                 same
                                                                 level
                                                                 as
                                                                 the
                                                                 previous
                                                                 task,
                                                                 so
                                                                 it's
                                                                 better
                                                                 to
                                                                 use
                                                                 this
                                                                 with
                                                                 to-do
                                                                 items
                                                                 than
                                                                 with
                                                                 projects
                                                                 or
                                                                 headings."
          (interactive)
          (org-agenda-switch-to)
          (org-capture 0))

        ;; necessary function 1
        (defun sync0-org-skip-subtree-if-priority (priority)
          "Skip an agenda subtree if it has a priority of PRIORITY. PRIORITY may be one of the characters ?A, ?B, or ?C."
          (let ((subtree-end (save-excursion (org-end-of-subtree t)))
                (pri-value (* 1000 (- org-lowest-priority priority)))
                (pri-current (org-get-priority (thing-at-point 'line t))))
            (if (= pri-value pri-current)
                subtree-end
              nil)))

        ;; necessary function 2
        (defun sync0-org-skip-subtree-if-habit ()
          "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (if (string= (org-entry-get nil "STYLE") "habit")
                subtree-end
              nil)))

        ;; org-agenda configuration
         (setq org-agenda-files (list "~/Dropbox/org/projects/todo/"))

         (let ((my-agenda-files (list "~/Dropbox/org/archived/etc/Gcal.org"
                                      "~/Dropbox/org/archived/etc/Events.org"
                                      "~/Dropbox/org/archived/etc/Classes.org"
                                      "~/Dropbox/org/projects/messages/messages.org"
                                      ;; "~/Dropbox/org/archived/etc/Habits.org"
                                       ;; "~/Dropbox/org/archived/etc/todo.org"
                                      "~/Dropbox/org/archived/etc/menage.org")))
         (setq org-agenda-files (append org-agenda-files my-agenda-files)))

    ;; This setup prevents slowing down agenda parsing. 
    ;; I create a variable to stand for the path of the journal file for the current month.  
    ;; Then, I have org-agenda parse only this path and not all the past journal files.
    ;; (setq sync0-journal-today-file 
    ;;       (concat sync0-dropbox-directory "org/journal/" (format-time-string "%Y%m%d") ".org"))

    ;; (add-to-list 'org-agenda-files sync0-journal-today-file)

        (setq org-agenda-custom-commands
              '(("d" "Deux semaines"
                 ((tags-todo "today|urgent|PRIORITY=\"A\""
                             ((org-agenda-overriding-header "Tâches prioritaires:")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                             (org-agenda-skip-entry-if 'deadline 'scheduled)))
                              (org-agenda-prefix-format " %-12t%-8s %-15c ")))
                  (agenda "" 
                          ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                                                          (sync0-org-skip-subtree-if-priority ?A)))
                           (org-agenda-span 'day)
                           (org-agenda-start-day "+0d")
                           ;; (org-agenda-prefix-format " %-12t%-12s %-15c %l ")
                           (org-deadline-warning-days 2)
                           (org-agenda-prefix-format " %-12t%-8s %-15c ")
                           ;; This format calls for two consecutive 12-character fields for time (%t)
                           ;; and scheduling information(%s). The reason for using fixed fields is to improve
                           ;; readability of the colums. Otherwise, apending an ~?~ character as in
                           ;; ~%?t~, only adds the field if the category exists. While this sound like
                           ;; a smart idea to save space, its very unreadable, so I advise against
                           ;; using it. This format is applied equally to the next two sections,
                           ;; precisely to avoid illegible output.
                           (org-agenda-start-on-weekday nil)))
                  (tags-todo "+this_week-urgent"
                             ((org-agenda-overriding-header "Sept jours :")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                             (org-agenda-skip-entry-if 'deadline 'scheduled)
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-prefix-format " %-12t%-8s %-15c ")))
                  (agenda "" ((org-agenda-span 6)
                              (org-agenda-start-day "+1d")
                              (org-agenda-start-on-weekday nil)
                              ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                              (org-agenda-prefix-format " %-12t%-8s %-15c ")))
                  (tags-todo "+next_week-urgent"
                             ((org-agenda-overriding-header "Quatorze jours :")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                             (org-agenda-skip-entry-if 'deadline 'scheduled)
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-prefix-format " %-12t%-8s %-15c ")))
                  (agenda "" ((org-agenda-span 6)
                              (org-agenda-start-day "+7d")
                              (org-agenda-start-on-weekday nil)
                              ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                              (org-agenda-prefix-format " %-12t%-8s %-15c "))))
                 ;; list options for block display
                 ((org-agenda-remove-tags t)
                  (org-agenda-view-columns-initially t)))

                ("w" "Study Planner"
                 ((tags-todo "reviews-ignore"
                             ((org-agenda-overriding-header "Revisions :")
                              (org-agenda-skip-function '(and (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                              (org-agenda-skip-entry-if 'nottodo 'any)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                  (tags-todo "readings-ignore"
                             ((org-agenda-overriding-header "Lectures :")
                              (org-agenda-skip-function '(and (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                              (org-agenda-skip-entry-if 'nottodo 'any)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                  (tags-todo "projects-ignore"
                             ((org-agenda-overriding-header "Projets :")
                              (org-agenda-skip-function '(and (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                              (org-agenda-skip-entry-if 'nottodo 'any)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                  (tags-todo "exam-ignore"
                             ((org-agenda-overriding-header "Examens :")
                              (org-agenda-skip-function '(and (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                              (org-agenda-skip-entry-if 'nottodo 'any)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  "))))
                 ;; list options for block display
                 ((org-agenda-remove-tags t)
                  (org-agenda-view-columns-initially t)))

                ("h" "Agenda"
                 ((agenda "" 
                          ((org-agenda-overriding-header " Agenda \n")
                           (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("完" "取" "阻")))
                           (org-agenda-span 'day)
                           (org-agenda-start-on-weekday nil)
                           ;; (org-agenda-skip-scheduled-delay-if-deadline t)
                            ;; (org-agenda-repeating-timestamp-show-all )
                           (org-agenda-start-day "+0d")
                           (org-deadline-warning-days 7)
                           ;; (org-agenda-current-time-string "⮜    ‧    ‧    maintenant")
                           (org-agenda-current-time-string " ")
                           (org-agenda-time-grid (quote ((daily today remove-match)
                                                         ;; the () means not to put those annoying time
                                                         ()
                                                         "     ⮜" "⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")))
                                                         ;; "     ⮜" ""
                                                        ;;  "     <" " "
                           (org-agenda-prefix-format "  %-22t  %-5s  %-3i  %-20c  ")))
                  (tags-todo "urgent|+PRIORITY=\"A\""
                             ((org-agenda-overriding-header " Tâches prioritaires \n")
                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("完" "取" "阻")))
                              ;; (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                              ;;                                (org-agenda-skip-entry-if 'scheduled 'deadline)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                              ;; (org-agenda-prefix-format "  %-22t  %-5s  %-3i %-20c  ")
                  (tags-todo "+this_week-urgent|DEADLINE>=\"<+2d>\"&DEADLINE<=\"<+7d>\"|SCHEDULED>=\"<+2d>\"&SCHEDULED<=\"<+7d>\""
                             ((org-agenda-overriding-header " Prochains sept jours \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                  (tags-todo "+next_week-urgent-this_week|DEADLINE>=\"<+8d>\"&DEADLINE<=\"<+14d>\"|SCHEDULED>=\"<+8d>\"&SCHEDULED<=\"<+14d>\""
                             ((org-agenda-overriding-header " Prochains quatorze jours \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                  (tags-todo "+this_month-urgent-this_week-next_week-ignore|DEADLINE>=\"<+15d>\"&DEADLINE<=\"<+29d>\"|SCHEDULED>=\"<+15d>\"&SCHEDULED<=\"<+29d>\""
                             ((org-agenda-overriding-header " Prochains trente jours \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取"))
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
                  (tags-todo "+research-projects-this_week-next_week-urgent-ignore-DEADLINE<=\"<+30d>\"-SCHEDULED<=\"<+30d>\""
                             ((org-agenda-overriding-header " Recherche \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻" "待"))
                                                             ;; (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time-today)  %-3i  %-20c  ")))
                  (tags-todo "+projects-research-this_week-next_week-urgent-ignore-DEADLINE<=\"<+30d>\"-SCHEDULED<=\"<+30d>\""
                             ((org-agenda-overriding-header " Projets \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻" "待"))
                                                             ;; (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                             (sync0-org-skip-subtree-if-priority ?A)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time-today)  %-3i  %-20c  "))))

                 ;; list options for block display
                 ((org-agenda-remove-tags nil)))

                ("p" "Research & Projets"
                 ;;tags-todo "+CATEGORY=\"Doctorat\""
                 ((tags-todo "+doctorat+todo|doctorat+readings|doctorat+stage"
                             ((org-agenda-overriding-header " Doctorat \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("中" "見" "次" "無"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(priority-up timestamp-up tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  ;; (tags-todo "+CATEGORY=\"Español\""
                  ;;            ((org-agenda-overriding-header " Español \n")
                  ;;             (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                  ;;                                            (sync0-org-skip-subtree-if-habit)))
                  ;;             (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                  ;;             (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+france+todo|france+readings"
                             ((org-agenda-overriding-header " Séjour en France \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+research_tools+todo|research_tools+readings"
                             ((org-agenda-overriding-header " Outils de la recherche \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("中" "見" "次" "無"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+work+todo|work+readings"
                             ((org-agenda-overriding-header " Travail \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+portuguese+todo|portuguese+readings"
                             ((org-agenda-overriding-header " Portugais \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+french+todo|french+readings"
                             ((org-agenda-overriding-header " Francais \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+writings+todo|writings+readings"
                             ((org-agenda-overriding-header " Écriture \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("中" "見" "次" "無"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+historiography+todo|historiography+readings"
                             ((org-agenda-overriding-header " Historiographie \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("中" "見" "次" "無"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+blog+todo"
                             ((org-agenda-overriding-header " Blog \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("中" "見" "次" "無"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+emacs+todo"
                             ((org-agenda-overriding-header " Emacs \n")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("中" "見" "次" "無"))
                                                             (sync0-org-skip-subtree-if-habit)))
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  ;; (tags-todo "+CATEGORY=\"Sefardi\""
                  ;;            ((org-agenda-overriding-header " Sefardi \n")
                  ;;             (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                  ;;                                            ;; (sync0-org-skip-subtree-if-priority ?A)
                  ;;                                            (sync0-org-skip-subtree-if-habit)))
                  ;;             ;; (org-agenda-prefix-format " %-12t%-8s %-15c %l")
                  ;;             (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                  ;;             (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  ")))
                  (tags-todo "+message"
                             ((org-agenda-overriding-header " Messages \n")
                             ;; (org-agenda-overriding-header "◈   Messages \n⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")
                              (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                                                             ;; (sync0-org-skip-subtree-if-priority ?A)
                                                             (sync0-org-skip-subtree-if-habit)))
                              ;; (org-agenda-prefix-format " %-12t%-8s %-15c %l")
                              (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                              (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time)  %-3i  %-20T  "))))

                 ;; list options for block display
                 ((org-agenda-remove-tags nil)
                  (org-agenda-view-columns-initially nil)))
                ;; End of custom
                ))

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

(use-package emms)

(use-package org-emms
:after emms
:commands (org-emms-insert-track
           org-emms-insert-track-position))

(server-start)

(use-package org-protocol
:after org
:straight nil)

(use-package org-roam
          :after evil-leader
          :straight (org-roam :type git :host github :repo "org-roam/org-roam") 
          :hook (after-init . org-roam-mode)
          :custom
              (org-roam-directory "~/Dropbox/org/")
        ;; make org-roam buffer sticky
              (org-roam-buffer-no-delete-other-windows t)
              (org-roam-completion-system 'default)
              (org-roam-link-file-path-type 'absolute)
              (org-roam-dailies-directory "journal/")
              (org-roam-tag-sources '(prop all-directories))
              ;; (org-roam-tag-sources '(prop last-directory))
              (org-roam-completion-everywhere t)
              (org-roam-title-sources '(alias title))
              (org-roam-index-file "~/Dropbox/org/index.org")
              (org-roam-graph-exclude-matcher '("journal" "fiches" "etc" "trash" "todo" "inbox" "projects" "archived" "references" "drafts" "spontaneous"))

        :config
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
                    (delete-region link-begin (- link-end 1))
                    (insert content)))))))

 (defun sync0-org-replace-all-links-by-descriptions ()
      "Remove the link part of an org-mode link at point and keep
    only the description"
      (interactive)
(save-excursion
(goto-char (point-min))
 (replace-regexp  "\\[\\[file:[[:print:]]+\\.org.*\\]\\[\\([[:print:]]+\\)\\]\\]" "\\1")))
;; (replace-regexp  "\\[\\[.*\\]\\[\\(.*\\)\\]\\]" "\\2")))


        (setq org-roam-capture-templates '( 
         ("n" "Numéroté" plain (function org-roam--capture-get-point)
          "%?"
          :file-name "%<%Y%m%d%H%M%S>"
          :head "#+TITLE: ${slug}\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: permanent %<%Y>  %<%B>\n\nOrigin: %a\n"
          :unnarrowed t)))

      (setq org-roam-capture-ref-templates
              '(("r" "ref" plain (function org-roam-capture--get-point)
                 "%?"
                 :file-name "notes/references/%<%Y%m%d%H%M%S>"
                 :head "#+TITLE: \n#+ROAM_KEY: ${ref}\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: websites %<%Y>\n\n"
                 :unnarrowed t)))

        (setq org-roam-dailies-capture-templates
              '(("d" "default" entry
                 #'org-roam-capture--get-point
                 "* %?"
                 :file-name "journal/%<%Y%m%d>"
                 :head "#+TITLE: %<%A, %d %B %Y>\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: journal %<%Y> %<%B>\n\n")))

(require 'org-journal)
(require 'org-roam-protocol)                

(defun sync0-org-roam-insert ()
  (interactive)
  (with-current-buffer
      (find-file-noselect
       (concat sync0-zettelkasten-directory 
               (format-time-string "charts/productivity/%Y%m.org")))
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
  (org-roam-insert))


(defhydra sync0-hydra-org-roam-insert (:color blue :hint nil)
"
^Zettelkasten link insert functions^   
^--------------------
^Org-roam^          ^Org-mode^          ^Org-roam-bibtex^  ^Org-emms^
^----------------------------------------- 
_i_nsert roam link  insert org _l_ink   _c_itation link    _t_rack link
_r_oam buffer       _s_tore link        note _a_ctions     track _p_osition 
_b_uild cache       last stored lin_k_        
plot _g_raph

_q_uit
"
      ("i" sync0-org-roam-insert)
      ("r" org-roam)
      ("b" org-roam-db-build-cache)
      ("g" org-roam-graph)
      ("l" org-insert-link)
      ("s" org-store-link)
      ("k" org-insert-last-stored-link)
      ("t" org-emms-insert-track)
      ("p" org-emms-insert-track-position)
      ("c" orb-insert)
      ("a" orb-note-actions)
      ("q" nil :color blue))

    (evil-leader/set-key
      "F" 'org-roam-find-file
      "i" 'sync0-org-roam-insert
      "I" 'sync0-hydra-org-roam-insert/body))

(use-package company-org-roam :after company)

(use-package org-roam-bibtex
  :straight (org-roam-bibtex :type git :host github :repo "org-roam/org-roam-bibtex") 
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
      ;; Use this to insert org-ref citations (cite:XXX199X)
      (orb-autokey-format "%a%y")
      (orb-process-file-keyword t)
      (orb-file-field-extensions '("pdf"))
      ;; Use this to insert citation keys
      (orb-insert-link-description 'citekey)
      (orb-insert-interface 'ivy-bibtex)
      (orb-note-actions-interface 'hydra)
  :config

(setq orb-preformat-keywords
      '("citekey" "title" "subtitle" "booktitle" "booksubtitle" "journaltitle" "url" "author-or-editor" "keywords" "file"))

  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
           :file-name "~/Dropbox/org/notes/references/${citekey}"
           :head "#+TITLE: ${title}\n#+SUBTITLE: ${subtitle}\n#+AUTHOR: ${author-or-editor}\n#+JOURNAL_TITLE: ${journaltitle}\n#+BOOK_TITLE: ${booktitle}\n#+BOOK_SUBTITLE: ${booksubtitle}\n#+ROAM_KEY: cite:${citekey}\n#+CREATED: %<%Y/%m/%d>\n#+DATE: %<%Y/%m/%d>\n#+ROAM_TAGS: ${citekey} \"${author-or-editor}\"\n#+INTERLEAVE_PDF: ${file}"
           :unnarrowed t))))

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
          :after (org evil-leader)
          :preface 
          (defun org-journal-find-location ()
            ;; Open today's journal, but specify a non-nil prefix argument in order to
            ;; inhibit inserting the heading; org-capture will insert the heading.
            (org-journal-new-entry t)
            ;; Position point on the journal's top-level heading so that org-capture
            ;; will add the new entry as a child entry.
            (goto-char (point-min)))

          :custom
          (org-default-notes-file "~/Dropbox/archived/etc/notes.org")

          :config 
          (evil-leader/set-key "c" 'org-capture)

          (add-hook 'org-capture-mode-hook 'evil-insert-state)

          ;; The following two functions are necessary to replicate the functionality of org-roam into org-capture.
          ;; https://emacs.stackexchange.com/questions/27620/orgmode-capturing-original-document-title
          (defun sync0-org-get-file-title-keyword (file)
            (let (title)
              (when file
                (with-current-buffer
                    (get-file-buffer file)
                  (pcase (org-collect-keywords '("TITLE"))
                    (`(("TITLE" . ,val))
                     (setq title (car val)))))
                title)))

          ;; Adapted from: 
          ;; https://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
          (defun sync0-org-get-keyword (KEYWORD)
            "get the value from a line like
                                                this #+KEYWORD: value in a file."
            (let ((case-fold-search t)
                  (re (format "^#\\+%s:[ \t]+\\([^\t\n]+\\)" KEYWORD)))
              (when  (save-excursion
                       (or (re-search-forward re nil t)
                           (re-search-backward re nil t)))
                (match-string-no-properties 1))))

          (defun sync0-org-get-previous-heading-or-title (file)
            (let (title)
              (when file
                (with-current-buffer
                    (get-file-buffer file)
                  (if (re-search-backward "^\\*+[ \t]+" nil t)
                      (setq title (nth 4 (org-heading-components)))
                    (pcase (org-collect-keywords '("TITLE"))
                      (`(("TITLE" . ,val))
                       (setq title (car val))))))
                title)))

          (defun sync0-org-get-author-keyword (file)
            (let (author)
              (when file
                (with-current-buffer
                    (get-file-buffer file)
                  (pcase (org-collect-keywords '("AUTHOR"))
                    (`(("AUTHOR" . ,val))
                     (setq author (car val)))))
                author)))

        (defun sync0-org-get-abbreviated-path (file)
          (interactive)
          (let (path)
            (when file
              (with-current-buffer
                  (get-file-buffer file)
                (setq path (abbreviate-file-name file))) path)))


      (defun sync0-org-capture-zettel-path ()
        "Output the path where the new zettel will be created"
        (let* ((key (org-capture-get :key))
              (filename (if (or (equal key "r") 
                                 (equal key "w"))
                             sync0-reference-filename
                           (format-time-string "%Y%m%d%H%M%S")))
               (filter (concat sync0-zettelkasten-directory
                               (cond ((equal key "a")  "notes/annotations")
                                     ((or (equal key "r")  
                                          (equal key "w"))  "notes/references")
                                     ((equal key "f")  "fiches")
                                     ((equal key "p")  "projects")
                                     ((equal key "t") "projects/todo")
                                     (t ""))))
               (path (if  (or (equal key "p")
                              (equal key "f")
                              (equal key "z"))
                       (completing-read "Dossier de la fiche : "
                                        (cons filter 
                                              (f-directories filter
                                                             (lambda (k) (not (string-match-p "\\.+" k))) t)))
                         filter)))
  ;; Add this zettel to the productivy chart
  (with-current-buffer
      (find-file-noselect
       (concat sync0-zettelkasten-directory 
               (format-time-string "charts/productivity/%Y%m.org")))
                 (goto-char (point-min))
    (let* ((date (format-time-string "%Y/%m/%d"))
           (entry (concat "\n| " date " | 1 | 0 |"))
           (second-exist
            (concat "^| "
                    date                    
                    " |\\([[:blank:]]+\\)|[[:blank:]]+[[:digit:]]+ |$"))
           (previous-value
            (concat "^| " date " |[[:blank:]]+\\([[:digit:]]+\\) |[[:blank:]]+[[:digit:]]+ |$")))
      (cond ((re-search-forward previous-value nil t 1)
             (let* ((old-value (string-to-number
                                (match-string-no-properties 1)))
                    (new-value (number-to-string
                                (1+ old-value))))
               (replace-match new-value nil nil nil 1)))
            ((re-search-forward second-exist nil t 1)
             (replace-match " 1 " nil nil nil 1))
            (t (progn
                 (goto-char (point-max))
                 (insert entry)))))
;; second part
                 (goto-char (point-min))
                 (let* ((word
                        (cond ((or (equal key "r") 
                                   (equal key "w"))
                               "Références")
                              ((or (equal key "p") 
                                   (equal key "t"))
                               "Projets")
                              ((equal key "f") 
                               "Fiches")
                              ((equal key "a") 
                               "Annotations")
                              (t 
                               "Zettel")))
                       (regex (concat "^| " word " [[:blank:]]+|[[:blank:]]+\\([[:digit:]]+\\) |")))
  (if (re-search-forward regex nil t 1)
             (let* ((old-value (string-to-number
                                (match-string-no-properties 1)))
                    (new-value (number-to-string
                                (1+ old-value))))
               (replace-match new-value nil nil nil 1))
                 (goto-char (point-max))
  (insert (concat "| " word "   |        1 |       0 |")))))
  ;; output the file name and path
          (concat (format "%s/%s.org" path filename))))


      (defun sync0-org-capture-zettel-body ()
        (let* ((key (org-capture-get :key))
               (filter (concat sync0-zettelkasten-directory
                               (cond ((equal key "a")  "notes/annotations")
                                     ((or (equal key "r")  
                                          (equal key "w"))  "notes/references")
                                     ((equal key "f")  "fiches")
                                     ((equal key "p")  "projects")
                                     ((equal key "t") "projects/todo")
                                     (t ""))))
               (candidates  (if (equal key "z")
                                  (directory-files-recursively filter ".org$" nil (or (lambda (k) (string-match-p "permanent" k))
                                                                                                        (lambda (k) (string-match-p "inbox" k))
                                                                                                        (lambda (k) (string-match-p "notes/annotations" k))))
                                (f-files filter (lambda (k) (string-match-p ".org$" k)) t)))
               (title (completing-read "Titre de la fiche : "
                                         (mapcar  #'(lambda (x) (org-roam-db--get-title x)) candidates)
                                         nil nil nil nil nil t))
               (title-quotes (concat "\"" (downcase title) "\""))
               (alias (when (equal key "f")
                  (read-string "Alias : " nil nil nil t)))
               (alias-quotes (when alias (concat "\"" (downcase alias) "\"")))
               (subtitle (unless (equal key "f")
                  (read-string "Sous-titre : " nil nil nil t)))
               (buffer (buffer-file-name))     
               (project (when (or (equal key "p")
                                  (equal key "t"))
                          (read-string "Projet : " nil nil nil t)))
               (creation  (format-time-string "%Y/%m/%d")))
  ;; define string of zettel
          (concat
           "#+TITLE: " title "\n"
           (unless (or (null subtitle)
                       (equal subtitle ""))
             (concat "#+SUBTITLE: " subtitle "\n"))
           (unless (or (null alias)
                       (equal alias ""))
             (concat "#+ROAM_ALIAS: " "\"" alias "\"\n"))
           "#+CREATED: " creation "\n"
           "#+DATE: " creation "\n"
           (when (and (equal key "t")
                      (not (equal project "")))
             (concat "#+CATEGORY: " (upcase project)))
  ;; add roam tags according to zettel type
           "#+ROAM_TAGS: "
           (cond ((equal key "a")
               (concat sync0-zettelkasten-annotations-key " " sync0-current-month-downcase
                       (format-time-string " %Y\n")))
              ((or (equal key "p")
                  (equal key "t"))
               (concat project " " sync0-current-month-downcase
                       (format-time-string " %Y\n")))
              ((equal key "f")
               (concat (if (equal alias "") title-quotes alias-quotes) " "  sync0-current-month-downcase
                       (format-time-string " %Y\n")))
             (t (concat sync0-current-month-downcase
                     (format-time-string " %Y\n"))))
           (when (equal key "t")
             (concat "#+FILETAGS: :projects:todo:" project ":\n"))
           "\n"
           "Origin: [[file:" (sync0-org-get-abbreviated-path buffer)
           "]["
           (sync0-org-get-file-title-keyword buffer)
           "]]\n\n"
           (when (equal key "a")
             (concat "Dans la page X de [[file:"
             (sync0-org-get-abbreviated-path buffer)
           "]["
           (sync0-org-get-previous-heading-or-title buffer)
           "]] "
           (sync0-org-get-author-keyword buffer)
           " ")))))

        (defun sync0-org-capture-reference ()
          (let* ((type (if (equal (org-capture-get :key) "w")
                           "online"
                         (completing-read "BibLaTex entry type: " sync0-biblatex-entry-types)))
                 ;; (biblatex-entries (cdr (assoc type sync0-capture-biblatex-fields-two)))
                 (filename 
                  (completing-read "Citation key: " (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                                                             (bibtex-completion-candidates))))
                 ;; (filename 
                 ;;  (completing-read "Citation key: "
                 ;;                            (let ((roam-bibtex-tags
                 ;;                                   (cl-delete-if (lambda (k) (or (string-match-p "[[:blank:]]" k)
                 ;;                                                                 (string-match-p "^[A-z-_]+$" k))) 
                 ;;                                                 (org-roam-db--get-tags)))
                 ;;                                  (bibliography-bibtex-tags
                 ;;                                   (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                 ;;                                             (bibtex-completion-candidates))))
                 ;;                              (delete-dups (append roam-bibtex-tags bibliography-bibtex-tags)))))
                 (creation (format-time-string "%Y/%m/%d")) 
               (date 
                (if (or (string-match "^[A-z-_]+[0-9-]\\{4,10\\}_\\([0-9-]\\{4,10\\}\\)[a-z]?[0-9_]*" filename)
                          (string-match "^[A-z-_]+\\([0-9-]\\{4,10\\}\\)[a-z]?[0-9_]*" filename))
                  (substring filename (match-beginning 1) (match-end 1))
  (read-string "Date : ")))
               (origdate 
                (when (string-match "^[A-z-_]+\\([0-9-]\\{4,10\\}\\)_[0-9-]\\{4,10\\}[a-z]?[0-9_]*" filename)
                  (substring filename (match-beginning 1) (match-end 1))))
  (file-date (if (null origdate)
  date
  (concat "(" origdate ")" date)))
                 (author (if (equal type "collection")
                             (completing-read "Editeur : "
                                              (delete-dups (mapcar #'(lambda (x) (cdr (assoc "editor" x)))
                                                                     (bibtex-completion-candidates))))
                           (completing-read "Auteur : "
                                            (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                                                                   (bibtex-completion-candidates))))))
                 (author-fixed (cond ((string-match " and " author)
                                      ;; create a list with parts 
                                      (let* ((author-list  (split-string author " and "))
                                             (names (let (x)
                                                      (dolist  (element author-list x)
                                                        (setq x (concat x
                                                                        (progn
                                                                          (string-match ", \\([[:graph:]]+\\)$"   element)
                                                                          (match-string 1 element))
                                                                        " "
                                                                        (progn
                                                                          (string-match "\\([[:graph:]]+\\),"   element)
                                                                          (match-string 1 element))
                                                                        ", "))))))
                                        (substring names 0 -2)))
                                     ((string-match "^{" author)
                                      (string-match "{\\([[:print:]]+\\)}" author)
                                      (match-string 1 author))
                                     (t (let* ((author-list (split-string author ", "))
                                               (last-name (nth 0 author-list))
                                               (first-name (nth 1 author-list)))
                                          (concat first-name " " last-name)))))
                 (author-quotes (concat "\"" (downcase author-fixed) "\""))
                 (lastname (cond ((string-match " and " author)
                                  ;; create a list with parts 
                                  (let* ((author-list  (split-string author " and "))
                                         (last-names (let (x)
                                                       (dolist  (element author-list x)
                                                         (setq x (concat x
                                                                         (progn
                                                                           (string-match "\\([[:graph:]]+\\),"   element)
                                                                           (match-string 1 element))
                                                                         ", "))))))
                                    (substring last-names 0 -2)))
                                 ((string-match "^{" author)
                                  (string-match "{\\([[:print:]]+\\)}" author)
                                  (match-string 1 author))
                                 (t (nth 0 (split-string author ", ")))))
                 (language (completing-read "Langage : " sync0-biblatex-languages))
                 (journal (when (equal type "article")
                                          (completing-read "Journal title : "
                                                   (delete-dups (mapcar #'(lambda (x) (cdr (assoc "journaltitle" x)))
                                                                  (bibtex-completion-candidates))))))
                 (volume (when (equal type "article") (read-string "Tome du journal : ")))
                 (number (when (equal type "article") (read-string "Numero du journal : ")))
                 (publisher (when (or (equal type "book")
                                      (equal type "collection"))
                              (completing-read "Maison d'edition : "
                                               (delete-dups (mapcar #'(lambda (x) (cdr (assoc "publisher" x)))
                                                                      (bibtex-completion-candidates))))))
                 (location (when (equal type "book")
                                       (completing-read "Location : "
                                             (delete-dups (mapcar #'(lambda (x) (cdr (assoc "location" x)))
                                                                    (bibtex-completion-candidates))))))
                 (pages (when (or (equal type "article")
                                  (equal type "incollection")
                                  (equal type "inbook"))
                          (read-string "Pages (ex. : 90-180) : ")))
                 (crossref (when (or (equal type "incollection")
                                     (equal type "inbook"))
                             (completing-read "Citation key: "
                                              (mapcar #'(lambda (x) (cdr (assoc "=key=" x)))
                                                        (bibtex-completion-candidates)))))
                 (booktitle (when (or (equal type "incollection")
                                      (equal type "inbook"))
                              (completing-read "Booktitle : "
                                               (delete-dups (mapcar #'(lambda (x) (cdr (assoc "booktitle" x)))
                                                                      (bibtex-completion-candidates))))))
                 (booksubtitle (when (or (equal type "incollection")
                                      (equal type "inbook"))
                                (read-string "Book subtitle  : ")))
                 (addendum (when (equal type "unpublished") (read-string "Addendum (ex. Box, Folder, etc.) : ")))
                 (url (when (equal type "online") (read-string "Url : " nil nil nil t)))
                 (urldate (when (equal type "online") (format-time-string "%Y-%m-%d")))
                 (title (read-string "Titre : " nil nil nil t))
                 (subtitle (read-string "Sous-titre : " nil nil nil t))
                 (file-title (if (equal subtitle "") title (concat title "_" subtitle)))
                 (file (concat "/home/sync0/Documents/pdfs/" lastname "_" file-date "_" file-title ".pdf"))
                 (buffer (buffer-file-name))     
    ;; define list of conses whose first element is a biblatex category and
    ;; the second element is its value, as a string, when previously defined
    ;; by this fucntion
                 (fields (list
                          (list "title" title)
                          (unless (equal subtitle "")
                          (list "subtitle" subtitle))
                          (list "origdate" origdate)
                          (list "date" date)
                          (if (equal type "collection")
                              (list "editor" author)
                            (list "author" author))
                          (list "journal" journal)
                          (if (or (equal type "book")
                                  (equal type "collection"))
                          (list "booktitle" title)
                          (list "booktitle" booktitle))
                          (if (or (equal type "book")
                                  (equal type "collection"))
                          (list "booksubtitle" subtitle)
                          (list "booksubtitle" booksubtitle))
                          (list "crossref" crossref)
                          (list "volume" volume)
                          (list "number" number)
                          (list "publisher" publisher)
                          (list "location" location)
                          (list "pages" pages)
                          (list "addendum" addendum)
                          (list "url" url)
                          (list "urldate" urldate)
                          (list "language" language)
                          (list "langid" language)
                          (list "file" file)))
    ;; define the biblatex entries
                 (entries
                  (let (x)
                    (dolist (element fields x) 
                      (unless (null (cadr element))
                      ;; (when (stringp (second element)))
                        (setq x (concat x (car element) " = {" (cadr element) "},\n"))))))
    ;; select target bibliography file (.bib)
                 (bib-file (completing-read "Fichier BibLaTeX : "
                                (f-files "~/Dropbox/bibliographies" (lambda (k) (string-match-p ".bib" k)))))
    ;; create string of new biblatex entry
                 (biblatex-entry (concat "\n@" type "{" filename "," "\n" entries "\n}\n")))
    ;; add biblatex entry to target bibliography file
            (append-to-file biblatex-entry nil bib-file)
            (setq  sync0-reference-filename filename)
    ;; define the body of the reference zettel
          (concat
           "#+TITLE: " title "\n"
           (unless (equal subtitle "") (concat "#+SUBTITLE: " subtitle "\n"))
           "#+AUTHOR: " author-fixed "\n"
           (when (equal type "article") (concat "#+JOURNAL_TITLE: " journal "\n"))
           "#+ROAM_KEY: cite:" filename "\n"
           (when (equal type "online") (concat "#+ROAM_KEY: " url "\n"))
           "#+CREATED: " creation "\n"
           "#+DATE: " creation "\n"
           "#+ROAM_TAGS: " filename " " author-quotes " " type 
           (format-time-string " %Y") "\n"
           "#+INTERLEAVE_PDF: " file "\n" 
           "Origin: [[file:"
           (sync0-org-get-abbreviated-path buffer)
           "]["
           (sync0-org-get-file-title-keyword buffer)
           "]]\n\n")))
           ;; (when (equal type "online") "%:initial%?")

          (defun sync0-org-references-fetch-title-and-subtitle ()
            (if (equal sync0-reference-subtitle "")
                (format "%s" sync0-reference-title) 
              (format "%s_%s" sync0-reference-title sync0-reference-subtitle))) 

          ;; Taken from https://github.com/abo-abo/hydra/wiki/mu4e
          (defun sync0-org-capture-mu4e ()
            (interactive)
            "Capture a TODO item via email."
            (org-capture nil "o"))

          (setq org-capture-templates 
                '(("j" "Journal" entry (function org-journal-find-location)
                   "* %(format-time-string org-journal-time-format)\n\n%?"
                   ;; "* %(format-time-string org-journal-time-format)\n\n%?"
                   :jump-to-captured t :immediate-finish t)
                  ("f" "Fiche" plain 
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-zettel-body)
                   :unnarrowed t)
                  ("p" "Note de projet" plain 
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-zettel-body)
                   :unnarrowed t)
                  ("t" "Liste de tâches" plain
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-zettel-body)
                   :unnarrowed t)
                  ("a" "Annotation" plain 
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-zettel-body)
                   :unnarrowed t)
                  ("r" "Référence" plain 
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-reference)
                   :unnarrowed t)
                  ("w" "Référence web" plain 
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-reference)
                   :unnarrowed t)
                  ("z" "Zettel" plain 
                   (file sync0-org-capture-zettel-path)
                   (function sync0-org-capture-zettel-body)
                   :unnarrowed t)
                  ;;    ("c" "Correspondant (messages)" plain 
                  ;; (file sync0-org-capture-message-name)
                  ;;   "%(format \"#+TITLE: Messages pour %s\n#+CREATED: %s\n#+DATE: \n#+ROAM_TAGS: fiches %s\" sync0-zettel-title-upcase sync0-zettel-time-ordered sync0-zettel-title)\n\nOrigin: [[file:%(sync0-org-get-abbreviated-path (org-capture-get :original-file))][%(sync0-org-get-file-title-keyword (org-capture-get :original-file))]]\n\n"
                  ;;   :unnarrowed t :jump-to-captured t)
                  ("m" "Email" entry 
                   (file+headline "~/Dropbox/org/projects/messages.org" "À répondre")
                   ;; "** 無 %^{Description}\n%A\n%?\n"
                   "** 無 %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%A\n" :jump-to-captured t :prepend t)))

          :bind 
          (("\C-c c" . org-capture)))

(use-package org-protocol-capture-html
  :straight (org-protocol-capture-html :type git :host github :repo "alphapapa/org-protocol-capture-html") 
  :after (org-protocol s))

(use-package org-habit 
  :straight nil
  :after (org org-agenda)
  ;; :commands org-bullets-mode
  :config
  (setq org-habit-graph-column 80
        org-habit-show-habits-only-for-today nil))

(setq
org-agenda-hide-tags-regexp "contacts\\|projects\\|research\\|urgent\\|important\\|short_term\\|long_term\\|no_export\\|this_month\\|this_week\\|next_week\\|next_moth\\|Métier")

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
    (org-export-with-tasks '("次" "完" "無" "中" "待" "疑" "見"))
    (org-export-date-timestamp-format "%Y/%m/%d")
    ;; Export to Microsoft Word (doc).
    (org-export-odt-preferred-output-format "doc")
    (org-odt-preferred-output-format "doc")
    (org-latex-logfiles-extensions '("aux" "lof" "lot" "tex~" "idx" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "run.xml"))

    :config
    (defun sync0-org-export-latex-and-beamer ()
      "Export current org file with beamer if it has beamer as latex class."
      (interactive)
      (when (equal major-mode 'org-mode) 
        (if (string-match "^\\#\\+SETUPFILE: .*beamer\\.org.*" (buffer-string))
            (progn
              (setq org-latex-pdf-process '("latexmk -xelatex -bibtex -output-directory=%o -f %f"))
              (org-beamer-export-to-pdf))
          (progn
            (setq org-latex-pdf-process '("latexmk -lualatex -bibtex -output-directory=%o -f %f"))
            (org-latex-export-to-pdf)))))

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
    (add-to-list 'org-export-exclude-tags "取")
    (add-to-list 'org-export-exclude-tags "noexport")

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

    :bind 
    (:map org-mode-map 
          ("M-p" . sync0-org-export-latex-and-beamer)))

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

(use-package org-ref
    :straight (org-ref :type git :host github :repo "jkitchin/org-ref") 
    :preface 
    (defun sync0-org-ref-open-pdf-at-point ()
      "Open the pdf for bibtex key under point if it exists."
      (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (car (bibtex-completion-find-pdf key))))
        (if (file-exists-p pdf-file)
            (org-open-file pdf-file))
        (message "No PDF found for %s" key)))

    :custom
    (reftex-default-bibliography '("~/Dropbox/bibliographies/bibliography.bib"
                                   "~/Dropbox/bibliographies/doctorat.bib"))
    (org-ref-default-bibliography reftex-default-bibliography)
    (org-ref-pdf-directory sync0-pdfs-folder)
    (org-ref-completion-library 'org-ref-ivy-cite)
    (org-ref-open-pdf-function 'sync0-org-ref-open-pdf-at-point)

    :config
   (require 'doi-utils)

    (setq org-ref-notes-function
          (lambda (thekey)
            (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
              (bibtex-completion-edit-notes
               (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

    (defun sync0-visit-bibliography-in-buffer ()
      (interactive)
      (find-file
       (expand-file-name "~/Dropbox/bibliographies/bibliography.bib")))

    (defhydra sync0-hydra-research-functions (:color amaranth :hint nil :exit t)
      "
   ^Research functions^   ^References^        ^Roam^              ^Roam link actions^
   ^------------------------------------------------------------------------------
   Orb _i_nsert           _I_nsert footnote   Find _f_ile         Link _s_tore 
   Orb _a_ctions          _Q_uote (Csquotes)  Open _r_oam buffer  _L_ast stored link
   Entry _n_otes          _F_oreign quote     Open _d_eft         Roam _l_ink                     
   Bibtex _e_ntry         Insert _c_itation   _B_uild cache       Link to _h_eadline
   Open _b_ibliography    ^ ^                 Open inde_x_        _D_elete link at point
   Open _p_df             ^ ^                 Show _g_raph        _R_emove all links

   _q_uit
        "
      ;; ("C" org-roam-capture)
      ("x" org-roam-jump-to-index)
      ("s" org-store-link)
      ("i" orb-insert)
      ("a" orb-note-actions)
      ("d" deft)
      ("r" org-roam)
      ("B" org-roam-db-build-cache)
      ("f" org-roam-find-file)
      ("g" org-roam-graph)
      ("l" org-roam-insert)
      ("h" org-insert-link)
      ("L" org-insert-last-stored-link)
   ;; ("L" org-roam-insert-immediate)
      ("c" org-ref-ivy-insert-cite-link)
      ("D" sync0-org-replace-link-by-description)
      ("R" sync0-org-replace-all-links-by-descriptions)
      ("n" ivy-bibtex)
      ("e" org-ref-open-citation-at-point)
      ("b" sync0-visit-bibliography-in-buffer)
      ("p" sync0-org-ref-open-pdf-at-point)
      ("I" org-footnote-new)
      ;; ("r" (progn (yas-expand-snippet (yas-lookup-snippet "org_ref_citation"))))
      ("Q" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_displayquote"))))
      ("F" (progn (yas-expand-snippet (yas-lookup-snippet "csquotes_foreign_displayquote"))))
      ("q" nil :color blue))

(evil-leader/set-key
  "R" 'sync0-hydra-research-functions/body)

    :bind 
    (:map org-mode-map
          ("C-c [" . org-ref-ivy-insert-cite-link)
          ;; ("C-c 9"  . org-ref-notes-function)
          ("<f9>" . sync0-hydra-research-functions/body)
          :map bibtex-mode-map
          ("C-c 9"  . ivy-bibtex)
          ("<f9>" . sync0-hydra-research-functions/body)
          ("C-c 8"  . sync0-org-ref-open-pdf-at-point)))

(use-package nov
 :straight nil
 :after (org-noter evil)
 :load-path "~/.emacs.d/sync0/nov.el" 
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
                            :height 200)

     (nov-text-width 66)
     (nov-render-document))
   ;; low resolution 
 (progn
   (face-remap-add-relative 'variable-pitch
                            :family "Minion Pro"
                            ;; :height 200
                            ;; :height 155
                            :height 130)
     (nov-text-width 60)
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
    org-noter-notes-search-path (list sync0-zettelkasten-directory)))

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
        (org-tag-alist '(("projects" . ?p)
                         ;; ("noexport" . ?n)
                         ("readings" . ?r)
                         ;; ("reviews" . ?r)
                         ("exams" . ?e)
                         ("urgent" . ?u)
                         ("this_week" . ?t)
                         ("this_month" . ?m)
                         ("next_week" . ?n)
                         ("short_term" . ?s)
                         ("long_term" . ?l)
                         ;; ("university" . ?u)
                         ("important" . ?i)))
        ;; Hide inherited tags from Org's agenda view.
        ;; org-agenda-show-inherited-tags nil
        ;; Define todo keywords.
        (org-todo-keywords '((sequence "無(1)" "次(2)" "中(3)" "見(4)" "待(5)" "阻(6)" "|" "完(7)" "取(8)")))
        ;; Set faces for org-todo-keywords
        (org-todo-keyword-faces '(("無" . (:foreground "#dc322f" :weight semi-bold :height 0.9))
                                  ("次" . (:foreground "#d33682" :weight semi-bold :height 0.9))
                                  ("完" . (:foreground "#859900" :weight semi-bold :height 0.9))   
                                  ("待" . (:foreground "#cb4b16" :weight semi-bold :height 0.9))
                                  ("阻" . (:foreground "#268bd2" :weight semi-bold :height 0.9)) 
                                  ("取" . (:foreground "#6c71c4" :weight semi-bold :height 0.9)) 
                                  ("見" . (:foreground "#268bd2" :weight semi-bold :height 0.9)) 
                                  ("中" . (:foreground "#b58900" :weight semi-bold :height 0.9))))
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

        :config 
        ;; (require 'org-pdftools)
        (require 'org-journal)
        (require 'org-download)
        (require 'org-ref)
        ;; Free this keybinding for cycle-themes
        (unbind-key "C-c C-t" org-mode-map)
        (unbind-key "M-h" org-mode-map)

    (defun sync0-overview-tree-window ()
      "Open a clone of the current buffer to the left, resize it to 30 columns, and bind <mouse-1> to jump to the same position in the base buffer."
      (interactive)
      (let ((new-buffer-name (concat "<tree>" (buffer-name))))
        ;; Create tree buffer
        (split-window-right 30)
        (if (get-buffer new-buffer-name)
            (switch-to-buffer new-buffer-name)  ; Use existing tree buffer
          ;; Make new tree buffer
          (progn  (clone-indirect-buffer new-buffer-name nil t)
                  (switch-to-buffer new-buffer-name)
                  (read-only-mode)
                  (hide-body)
                  (toggle-truncate-lines)

                  ;; Do this twice in case the point is in a hidden line
                  (dotimes (_ 2 (forward-line 0)))

                  ;; Map keys
                  (use-local-map (copy-keymap outline-mode-map))
                  (local-set-key (kbd "q") 'delete-window)
                  (mapc (lambda (key) (local-set-key (kbd key) 'my/jump-to-point-and-show))
                        '("<mouse-1>" "RET"))))))

    (defun sync0-overview-jump-to-overview ()
      "Switch to a cloned buffer's base buffer and move point to the cursor position in the clone."
      (interactive)
      (let ((buf (buffer-base-buffer)))
        (unless buf
          (error "You need to be in a cloned buffer!"))
        (let ((pos (point))
              (win (car (get-buffer-window-list buf))))
          (if win
              (select-window win)
            (other-window 1)
            (switch-to-buffer buf))
          (goto-char pos)
          (when (invisible-p (point))
            (show-branches)))))

        (defun sync0-org-tree-to-indirect-buffer ()
          "Open headline in the next window as a separate tree."
          (interactive)
          (org-tree-to-indirect-buffer)
          (windmove-right))

    (defhydra sync0-hydra-org-functions (:color amaranth :hint nil :exit t)
      "
   ^Links^             ^Footnotes^          ^Trees^              ^Export^          ^Etc.^
   ^---------------------------------------------------------------------------------------------------
   Link _i_nsert       New _f_ootnote       Indirect _b_uffer    Latex _e_xport    Insert _d_rawer
   Link _s_tore        Footnote _a_ctions   Open _o_verview      Export _t_rees
   Last stored lin_k_  ^ ^                  Overview _j_ump 

   _q_uit
        "

      ("s" org-store-link)
      ("i" org-insert-link)
      ("k" org-insert-last-stored-link)
      ("f" org-footnote-new)
      ("a" org-footnote-action)
      ("b" sync0-org-tree-to-indirect-buffer)
      ("j" sync0-overview-jump-to-overview)
      ("o" sync0-overview-tree-window)
      ("e" sync0-org-export-latex-and-beamer)
      ("t" sync0-org-export-headlines-to-latex)
      ("d" org-insert-drawer)
      ("q" nil :color blue))

  (evil-leader/set-key
    "O" 'org-open-at-point)
    ;; "O" 'sync0-overview-tree-window
    ;; "o" 'sync0-overview-jump-to-overview
    ;; "I" 'org-insert-link
    ;; "z" 'sync0-org-tree-to-indirect-buffer
    ;; "z" 'sync0-hydra-org-functions/body

(evil-leader/set-key-for-mode 'org-mode "z" 'sync0-hydra-org-functions/body)

      (defhydra sync0-hydra-file-access (:color amaranth :hint nil :exit t)
        "
         ^Windows^                ^Buffers^             ^Search^
      ^^^^^^---------------------------------------------------------------
      _1_: Delete others       _w_: Write           _r_: Recent
      _2_: Split horizontally  _a_: Write as        _f_: Find
      _3_: Split vertically    _b_: Open           
      ^ ^                      _k_: Kill
      ^ ^                      
      ^^^^^^---------------------------------------------------------------
         ^Bookmarks^           ^Planning^ 
      ^^^^^^---------------------------------------------------------------
      _j_: Jump to bookmark    _h_: Today
      _g_: Bookmark o. window  
      _m_: Set bookmark        
      _l_: List bookmarks      _J_: org-journal
      ^ ^                      
      [q] Quit                 ^ ^
      "
        ("1" delete-other-windows)
        ("2" sync0-split-and-follow-horizontally)
        ("3" sync0-split-and-follow-vertically)
        ("b" ivy-switch-buffer)
        ;; Quickly save
        ("w" save-buffer)
        ("a" write-file)
        ;; Kill current buffer and window
        ("k" kill-buffer-and-window)
        ;; ("o" ivy-switch-buffer-other-window)
        ("r" counsel-recentf)
        ("f" counsel-find-file)
        ("m" bookmark-set)
        ("j" counsel-bookmark)
        ("g" bookmark-jump-other-window)
        ("l" bookmark-bmenu-list)
        ;; ("A" org-agenda)
        ("J" sync0-org-journal-new-scheduled-entry)
        ("h" sync0-pop-to-org-agenda)
        ("q" nil :color blue))

        ;; font lock keywords 
        ;; org footnotes should look like real footnotes
(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords 'org-mode
 '(("\\(\\[fn:\\)[[:digit:]]+\\]" 1 '(face nil display ""))))
(font-lock-add-keywords 'org-mode
 '(("\\[fn:[[:digit:]]+\\(\\]\\)" 1 '(face nil display ""))))

(require 'cl-lib)

;; Taken from https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer
(defvar sync0-punctuation-marks '(","
                            "."
                            "'"
                            "&"
                            "\"")
  "List of Punctuation Marks that you want to count.")

(defun sync0-count-raw-word-list (raw-word-list)
  (cl-loop with result = nil
           for elt in raw-word-list
           do (cl-incf (cdr (or (assoc elt result)
                             (first (push (cons elt 0) result)))))
           finally return (sort result
                                (lambda (a b) (string< (car a) (car b))))))

(defun sync0-word-stats ()
  (interactive)
  (let* ((words (split-string
                 (downcase (buffer-string))
                 (format "[ %s\f\t\n\r\v]+"
                         (mapconcat #'identity sync0-punctuation-marks ""))
                 t))
         (punctuation-marks (cl-remove-if-not
                             (lambda (elt) (member elt sync0-punctuation-marks))
                             (split-string (buffer-string) "" t )))
         (raw-word-list (append punctuation-marks words))
         (word-list (sync0-count-raw-word-list raw-word-list)))
    (with-current-buffer (get-buffer-create "*word-statistics*")
      (erase-buffer)
      (insert "| word | occurences |
               |-----------+------------|\n")

      (dolist (elt word-list)
        (insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))

      (org-mode)
      (indent-region (point-min) (point-max))
      (goto-char 100)
      (org-cycle)
      (goto-char 79)
      (org-table-sort-lines nil ?N)))
  (pop-to-buffer "*word-statistics*"))

        (defun sync0-call-rebinding-org-blank-behaviour (fn)
          (let ((org-blank-before-new-entry
                 (copy-tree org-blank-before-new-entry)))
            (when (org-at-heading-p)
              (rplacd (assoc 'heading org-blank-before-new-entry) nil))
            (call-interactively fn)))

        (defun sync0-org-meta-return-dwim ()
          "Improved version of default org-meta-return"
          (interactive)
          (sync0-call-rebinding-org-blank-behaviour 'org-meta-return))

        (defun sync0-org-insert-todo-heading-dwim ()
          "Improved version of org-insert-todo-heading"
          (interactive)
          (sync0-call-rebinding-org-blank-behaviour 'org-insert-todo-heading))

        (defun sync0-clever-insert-item ()
          "Clever insertion of org item."
          (if (not (org-in-item-p))
              (insert "\n")
            (org-insert-item)))

        (defun sync0-evil-org-eol-call (fun)
          "Go to end of line and call provided function. FUN function callback"
          (end-of-line)
          (funcall fun)
          (evil-append nil))

        ;; redefinition evils normal mode map
        (evil-define-key 'normal org-mode-map
          "<" 'outline-previous-visible-heading
          ">" 'outline-next-visible-heading
          (kbd "C->") 'org-forward-heading-same-level
          (kbd "C-<") 'org-backward-heading-same-level
          (kbd "<S-tab>") 'sync0-org-tree-open-in-right-frame 
          "H" 'org-metaleft
          "L" 'org-metaright
          "K" 'org-metaup
          "J" 'org-metadown
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
          (kbd "<tab>") 'org-cycle)

        (evil-define-key 'visual org-mode-map
          ;; "q" 'highlight-changes-remove-highlight
          "z" 'org-emphasize)

        ;; List of files considered for org-refile.
        (setq org-refile-targets (quote ((nil :maxlevel . 4)                ;; Default value.
                                              ;; set for all agenda files
                                         ;; ("todo.org" :maxlevel . 2)
                                          (org-agenda-files :maxlevel . 4))))

        ;; (org-refile-targets '((org-agenda-files :maxlevel . 4)))

(setq org-file-apps
'((auto-mode . emacs)
 (directory . emacs)
 ("\\.mm\\'" . default)
 ("\\.x?html?\\'" . default)
 ("\\.pdf\\'" . emacs)))


        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

        ;; (setq org-src-block-faces    '(("emacs-lisp" (:family "Fira Code"  :height 0.75))
        ;;                                ("python" (:family "Fira Code"  :height 0.75))
        ;;                                ("latex" (:family "Fira Code"  :height 0.75))))

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
      (let ((delete-classes (find-file-noselect "~/Dropbox/org/archived/etc/Classes.org"))
            (delete-events (find-file-noselect "~/Dropbox/org/archived/etc/Events.org"))
            (delete-gcal (find-file-noselect "~/Dropbox/org/archived/etc/Gcal.org"))
            (delete-habits (find-file-noselect "~/Dropbox/org/archived/etc/Habits.org")))
        (progn 
          (with-current-buffer delete-classes
            (erase-buffer))
          (with-current-buffer delete-events
            (erase-buffer))
          (with-current-buffer delete-gcal
            (erase-buffer))
          (with-current-buffer delete-habits
            (erase-buffer)))))

    (setq org-gcal-file-alist '(("carc.sync0@gmail.com" .  "~/Dropbox/org/archived/etc/Gcal.org")
                                ("5iudo90h5e3nabbubvsj1lov4o@group.calendar.google.com" . "~/Dropbox/org/archived/etc/Classes.org")
                                ("p9vu3a782nahsma6ud1rdg1qpc@group.calendar.google.com" . "~/Dropbox/org/archived/etc/Events.org")
                                ("vbnn8eksqpqun2mbtdlknhh9uk@group.calendar.google.com" . "~/Dropbox/org/archived/etc/Habits.org")
                                ("addressbook#contacts@group.v.calendar.google.com" . "~/Dropbox/org/archived/etc/Birthdays.org"))))

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

(setq initial-scratch-message ";; 
;;
;;   Man should not be ready to show that he can live like a
;;   badly-fed animal. He should decline to live like that, and
;;   should either steal or go on the rates, which is considered by
;;   many to be a form of stealing. As for begging, it is safer to
;;   beg than to take, but it is finer to take than to beg. No: a
;;   poor man who is ungrateful, unthrifty, discontented, and
;;   rebellious, is probably a real personality, and has much in him.
;;   He is at any rate a healthy protest. As for the virtuous poor,
;;   one can pity them, of course, but one cannot possibly admire
;;   them. They have made private terms with the enemy, and sold
;;   their birthright for very bad pottage.
;;
;;   Oscar Wilde
;;   The Soul of Man under Socialism (1891)
;; ")

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


(setq-default mode-line-format
                  '(" " 
                  ;;  mode-line-front-espace 
                    (:eval (cond 
                            (buffer-read-only (propertize "🔒"
                                                          'face '(:family "Noto Color Emoji")
                                                          'help-echo "buffer is read-only!!!"))
                            ((buffer-modified-p) (propertize "💾"
                                                             'face '(:family "Noto Color Emoji")))
                            (t (propertize "✓"
                                           'face '(:family "Noto Color Emoji")))))
                    "  " 
                    mode-line-buffer-identification 
                    "  " 
                    (:eval 
                            (if (boundp 'guess-language-current-language) 
            (cond  ((string-equal guess-language-current-language "en") 
                            (propertize "EN" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "de") 
                            (propertize "DE" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "pt") 
                            (propertize "PT" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                 ((string-equal guess-language-current-language "it") 
                          (propertize "IT" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "fr") 
                            (propertize "FR" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "es") 
                            (propertize "ES" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                    (t (propertize "NIL" 'face '(:height 1.0 :family "Minion Pro" :weight bold))))
                                 ;; (upcase (prin1-to-string guess-language-current-language))
                             (propertize "NIL" 'face '(:height 1.0 :family "Minion Pro" :weight bold))))
                  ;; evil-mode-line-tag
                    "  "
                  (:eval 
                   (let ((line-string "L:%l"))
                     (if (and (not (buffer-modified-p))
                              sync0-mode-line-buffer-line-count)
                         (setq line-string 
                               (concat line-string "/" sync0-mode-line-buffer-line-count))
                       line-string)))
                   ;; "L:%l"
                  "                                                               "
                   (:eval (propertize 
                           (capitalize 
                            (s-replace "-mode" "" (format "%s" major-mode)))
                                'face '(:weight bold)))
                   " " 
                   (vc-mode vc-mode)
                   " " 
                   (:eval (when (boundp 'org-mode-line-string)
                            (propertize  org-mode-line-string 'face '(:weight semi-bold))))
                   (:eval (propertize (format-time-string " %H:%M ")
                                      'face '(:weight bold))) 
                   " " 
                    (:eval  (propertize "⚡" 'face '(:family "Noto Color Emoji")))
                   mode-line-misc-info
emacs-mode-line-end-spaces))

(use-package mini-modeline
    :disabled t
    :straight (mini-modeline :type git :host github :repo "kiennq/emacs-mini-modeline") 
          ;; :preface
          ;; ;; Define a local variable with the total number of lines.
          ;; (defvar-local sync0-mode-line-buffer-line-count nil)

          ;; ;; Define a function that counts the number of lines in the
          ;; ;; current buffer.
          ;; (defun sync0-mode-line-count-lines ()
          ;;   "Count the number of lines in the current buffer."
          ;;   (setq-local sync0-mode-line-buffer-line-count 
          ;;               (int-to-string (count-lines (point-min) (point-max)))))

          ;; ;; Recalculate the total number of lines using hooks. This is
          ;; ;; not the best approach, but I have not been able to devise a
          ;; ;; dynamic way to calculate these that does not result in Emacs
          ;; ;; "inventing" these results.
          ;; (add-hook 'find-file-hook 'sync0-mode-line-count-lines)
          ;; (add-hook 'after-save-hook 'sync0-mode-line-count-lines)
          ;; (add-hook 'after-revert-hook 'sync0-mode-line-count-lines)
  :custom
  (mini-modeline-display-gui-line nil)
  (mini-modeline-enhance-visual nil)
  ;; (mini-modeline-right-padding 3)
          :config
          (setq   mini-modeline-l-format
                  '(" " 
                    mode-line-front-espace 
                    (:eval (cond 
                            (buffer-read-only (propertize "🔒 "
                                                          'face '(:family "Noto Color Emoji")
                                                          'help-echo "buffer is read-only!!!"))
                            ((buffer-modified-p) (propertize "💾 "
                                                             'face '(:family "Noto Color Emoji")))
                            (t (propertize "✔ "
                                           'face '(:family "Noto Color Emoji")))))
                    mode-line-buffer-identification 
                    "  " 
                    (:eval 
                            (if (boundp 'guess-language-current-language) 
            (cond  ((string-equal guess-language-current-language "en") 
                            (propertize "EN" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "de") 
                            (propertize "DE" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "pt") 
                            (propertize "PT" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "it") 
                            (propertize "IT" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "fr") 
                            (propertize "FR" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                   ((string-equal guess-language-current-language "es") 
                            (propertize "ES" 'face '(:height 1.0 :family "Minion Pro" :weight bold)))
                    (t (propertize "NIL" 'face '(:height 1.0 :family "Minion Pro" :weight bold))))
                                 ;; (upcase (prin1-to-string guess-language-current-language))
                             (propertize "NIL" 'face '(:height 1.0 :family "Minion Pro" :weight bold))))
                    "  "
                    "L:%l"))
;; Count the number of lines in modeline.
                    ;; (:eval 
                    ;;  (let ((line-string "L:%l"))
                    ;;    (if (and (not (buffer-modified-p))
                    ;;             sync0-mode-line-buffer-line-count)
                    ;;        (setq line-string 
                    ;;              (concat line-string "/" sync0-mode-line-buffer-line-count))
                    ;;      line-string)))

          (setq  mini-modeline-r-format
                 '((:eval 
                        (propertize 
                         (capitalize 
                          (s-replace "-mode" "" (format "%s" major-mode)))
                         'face '(:weight bold)))
                   " " 
                   (vc-mode vc-mode)
                   " " 
                   (:eval (when (boundp 'org-mode-line-string)
                            (propertize  org-mode-line-string 'face '(:weight semi-bold))))
                   (:eval (propertize (format-time-string " %H:%M ")
                                      'face '(:weight bold))) 
                   " " 
                    (:eval  (propertize "⚡" 'face '(:family "Noto Color Emoji")))
                   mode-line-misc-info
                   ))

          (mini-modeline-mode t))

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

;;   (defun sync0-buffer-face-mode-fixed ()
;;     "Set font to a variable width (proportional) fonts in current buffer"
;; (if (> (display-pixel-width) 1900)
;;     ;; external monitor font size
;;     (progn 
;;         (setq buffer-face-mode-face '(:family "Inconsolata" :height 150))
;;         (setq line-spacing 0))
;;   ;; laptop font size
;;   (progn 
;;       (setq buffer-face-mode-face '(:family "Inconsolata" :height 155))
;;     (setq line-spacing 0.5)))
;;     (buffer-face-mode))

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
        (defun sync0-language-change (lang beginning end)
          "Set of functions to run after a different language is detected."
          (unless (string-equal guess-language-current-language lang)
            (cond 
             ((string-equal lang "es")
              (progn
                (setq sync0-language-active "spanish")
                (setq local-abbrev-table spanish-mode-abbrev-table)
                (set-input-method "spanish-prefix")
                (ispell-change-dictionary "es")))
             ((string-equal lang "de")
              (progn
                (setq sync0-language-active "german")
                (setq local-abbrev-table german-mode-abbrev-table)
                (set-input-method "german-prefix")
                (ispell-change-dictionary "de_DE")))
             ((string-equal lang "pt")
              (progn
                (setq sync0-language-active "portuguese")
                (setq local-abbrev-table portuguese-mode-abbrev-table)
                (set-input-method "portuguese-prefix")
                (ispell-change-dictionary "pt_BR")))
             ((string-equal lang "fr")
              (progn
                (setq sync0-language-active "french")
                (setq local-abbrev-table french-mode-abbrev-table)
                (set-input-method "french-prefix")
                (ispell-change-dictionary "fr_FR")))
             ((string-equal lang "it")
              (progn
                (setq sync0-language-active "italian")
                (setq local-abbrev-table italian-mode-abbrev-table)
                (set-input-method "italian-postfix")
                (ispell-change-dictionary "it_IT")))
             ((string-equal lang "en")
              (progn
                (setq sync0-language-active "english")
                (setq local-abbrev-table english-mode-abbrev-table)
                (set-input-method nil)
                (ispell-change-dictionary "en_US-large"))))))

    (defvar sync0-change-language-actions-alist
      '((?1 "en" (lambda ()
                (progn
                  (setq  guess-language-current-language 'en)
                  (setq sync0-language-active "english")
                  (setq local-abbrev-table english-mode-abbrev-table)
                  (set-input-method nil)
                  (ispell-change-dictionary "en_US-large"))))
        (?2 "es" (lambda ()
              (progn
                (setq  guess-language-current-language 'es)
                (setq sync0-language-active "spanish")
                (setq local-abbrev-table spanish-mode-abbrev-table)
                (set-input-method "spanish-prefix")
                (ispell-change-dictionary "es"))))
        (?3 "pt" (lambda ()
              (progn
                (setq  guess-language-current-language 'pt)
                (setq sync0-language-active "portuguese")
                (setq local-abbrev-table portuguese-mode-abbrev-table)
                (set-input-method "portuguese-prefix")
                (ispell-change-dictionary "pt_BR"))))
        (?4 "fr" (lambda ()
              (progn
                  (setq  guess-language-current-language 'fr)
                (setq sync0-language-active "french")
                (setq local-abbrev-table french-mode-abbrev-table)
                (set-input-method "french-prefix")
                (ispell-change-dictionary "fr_FR"))))
        (?5 "it" (lambda ()
              (progn
                  (setq  guess-language-current-language 'it)
                (setq sync0-language-active "italian")
                (setq local-abbrev-table italian-mode-abbrev-table)
                (set-input-method "italian-postfix")
                (ispell-change-dictionary "it_IT"))))
        (?6 "de" (lambda ()
              (progn
                  (message "Deutsch ist die aktuelle Sprache")
                  (setq  guess-language-current-language 'de)
                (setq sync0-language-active "german")
                (setq local-abbrev-table german-mode-abbrev-table)
                (set-input-method "german-prefix")
                (ispell-change-dictionary "de_DE")))))
      "List that associates number letters to descriptions and actions.")

    (defun sync0-change-current-language ()
      "Lets the user choose the animal and takes the corresponding action.
    Returns whatever the action returns."
      (interactive)
      (let ((choice
             (read-char-choice
              (mapconcat
               (lambda (item) (format "[%c] %s" (car item) (cadr item)))
               sync0-change-language-actions-alist " ")
                      (mapcar #'car sync0-change-language-actions-alist))))
        (funcall (nth 2 (assoc choice sync0-change-language-actions-alist)))))

      (defun sync0-ispell-get-word ()
        (car-safe (save-excursion (ispell-get-word nil))))

      (defun sync0-ispell-word-then-abbrev ()
        "Call `ispell-word', then create an abbrev for it.
      With prefix P, create local abbrev. Otherwise it will
      be global.
      If there's nothing wrong with the word at point, keep
      looking for a typo until the beginning of buffer. You can
      skip typos you don't want to fix with `SPC', and you can
      abort completely with `C-g'."
        (interactive)
        (let (bef aft)
          (save-excursion
            (while (if (setq bef (sync0-ispell-get-word))
                       ;; Word was corrected or used quit.
                       (if (ispell-word nil 'quiet)
                           nil ; End the loop.
                         ;; Also end if we reach `bob'.
                         (not (bobp)))
                     ;; If there's no word at point, keep looking
                     ;; until `bob'.
                     (not (bobp)))
              (backward-word)
              (backward-char))
            (setq aft (sync0-ispell-get-word)))
          (if (and aft bef (not (equal aft bef)))
              (let ((aft (downcase aft))
                    (bef (downcase bef)))
    ;; (unless
    ;;  (save-excursion
    ;;   (with-temp-buffer
    ;;    (insert-file-contents company-ispell-dictionary)
    ;;    (goto-char (point-min))
    ;;    (re-search-forward (concat "^" aft) nil t 1)))
    ;;    (write-region (concat aft "\n") nil company-ispell-dictionary 'append))
                (define-abbrev local-abbrev-table bef aft)
                (message "\"%s\" now expands to \"%s\" %sally"
                         bef aft "loc"))
            (user-error "No typo at or before point"))))

        (defun sync0-lookup-word (word)
          "Search an online dictionary for the word at point according
            to the active language minor mode."
          (interactive (list (save-excursion (car (ispell-get-word nil)))))
          (cond  ((string-equal guess-language-current-language "en") 
                  (browse-url (format "https://www.merriam-webster.com/dictionary/%s" word)))
                 ((string-equal guess-language-current-language "de") 
                  (browse-url (format "https://www.duden.de/rechtschreibung/%s" word)))
                 ((string-equal guess-language-current-language "it") 
                  (browse-url (format "https://www.duden.de/rechtschreibung/%s" word)))
                 ((string-equal guess-language-current-language "pt") 
                  (browse-url (format "https://www.dicio.com.br/%s" word)))
                 ((string-equal guess-language-current-language "fr") 
                  (browse-url (format "https://dictionnaire.lerobert.com/definition/%s#definitions" word)))
                 ((string-equal guess-language-current-language "es") 
                  (browse-url (format "https://dle.rae.es/?w=%s" word)))
                 (t "No language minor mode specified")))

        (defun sync0-lookup-conjugation (word)
          "Search an online dictionary for the word at point according
            to the active language minor mode."
          (interactive (list (save-excursion (car (ispell-get-word nil)))))
          (cond  ((string-equal guess-language-current-language "en") 
                  (browse-url (format "https://www.merriam-webster.com/dictionary/%s" word)))
                 ((string-equal guess-language-current-language "de") 
                  (browse-url (format "https://www.verbformen.de/konjugation/?w=%s" word)))
                 ((string-equal guess-language-current-language "it") 
                  (browse-url (format "https://www.verbformen.de/konjugation/?w=%s" word)))
                 ((string-equal guess-language-current-language "pt") 
                  (browse-url (format "https://www.conjugacao.com.br/verbo-%s/" word)))
                 ((string-equal guess-language-current-language "fr") 
                  (browse-url (format "http://la-conjugaison.nouvelobs.com/du/verbe/%s.php" word)))
                 ((string-equal guess-language-current-language "es") 
                  (browse-url (format "http://conjugador.reverso.net/conjugacion-espanol-verbo-%s.html" word)))
                 (t "No language minor mode specified")))

        (defun sync0-lookup-thesaurus (word)
          "Search an online dictionary for the word at point according
            to the active language minor mode."
          (interactive (list (save-excursion (car (ispell-get-word nil)))))
          (cond  ((string-equal guess-language-current-language "en") 
                  (browse-url (format "https://www.merriam-webster.com/thesaurus/%s" word)))
                 ((string-equal guess-language-current-language "fr") 
                  (browse-url (format "https://dictionnaire.lerobert.com/definition/%s#synonymes" word)))
                 ((string-equal guess-language-current-language "de") 
                  (browse-url (format "https://www.duden.de/rechtschreibung/%s#synonyme" word)))
                 ((string-equal guess-language-current-language "it") 
                  (browse-url (format "https://www.duden.de/rechtschreibung/%s#synonyme" word)))
                 ((string-equal guess-language-current-language "pt") 
                  (browse-url (format "https://www.dicio.com.br/%s" word)))
                 ((string-equal guess-language-current-language "es") 
                  (browse-url (format "http://conjugador.reverso.net/conjugacion-espanol-verbo-%s.html" word)))
                 (t "No language minor mode specified")))

  (defun sync0-guess-language-set-parts-of-speech ()
  "Choose parts of speech according to active language"
  (let* ((parts-list (list ()))
         (lang (prin1-to-string guess-language-current-language)))
    (cond ((string-equal lang "es")
           (progn
           (setq parts-list sync0-spanish-parts-speech)
            (ivy-completing-read "Elija uno: " parts-list)))
          ((string-equal lang "pt")
           (progn
           (setq parts-list sync0-portuguese-parts-speech)
            (ivy-completing-read "Escolha um: " parts-list)))
          ((string-equal lang "it")
           (progn
           (setq parts-list sync0-portuguese-parts-speech)
            (ivy-completing-read "Escolha um: " parts-list)))
           ((string-equal lang "fr")
           (progn
            (setq parts-list sync0-french-parts-speech)
            (ivy-completing-read "Choississez un : " parts-list)))
           ((string-equal lang "en")
           (progn
            (setq parts-list sync0-english-parts-speech)
             (ivy-completing-read "Choose one: " parts-list)))
                 (t "No language minor mode specified"))))

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
          ("i" sync0-ispell-word-then-abbrev)
          ("c" sync0-lookup-conjugation)
          ("t" sync0-lookup-thesaurus)
          ("q"  nil :color blue))

(evil-leader/set-key
  "L" 'sync0-ispell-word-then-abbrev
  "l" 'sync0-hydra-language-functions/body)

        (add-hook 'guess-language-after-detection-functions #'sync0-language-change)

        :bind (("M-#" . sync0-lookup-word)
               ("M-i" . sync0-ispell-word-then-abbrev)
               ;; ("C-d" . sync0-hydra-language-functions/body)
               ("M-$" . sync0-lookup-conjugation)))

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
              (setq cwm-left-fringe-ratio 60)
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
             (prog-mode . sync0-prog-mode-centered-window)))

(use-package olivetti
    :disabled t
    :straight (olivetti :type git :host github :repo "rnkn/olivetti") 
    :commands olivetti-mode
    :config
(defun sync0-text-mode-olivetti ()
 (progn
  (olivetti-set-width 66)
 (olivetti-mode 1)))

(defun sync0-prog-mode-olivetti ()
 (progn
  (olivetti-set-width 80)
 (olivetti-mode 1))))

    ;; :hook 
    ;; ((text-mode . sync0-text-mode-olivetti)
    ;;  (prog-mode . sync0-prog-mode-olivetti))

(use-package follow-mode
 :straight nil
 :commands follow-mode
 :custom (follow-auto t)
 :bind ("C-c f" . follow-delete-other-windows-and-split))

(use-package latex
    :straight nil
    :defer t
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

(use-package ivy-bibtex 
;;    :after (ivy bibtex)
    :custom 
    ;; writing completion
    (bibtex-completion-bibliography '("~/Dropbox/bibliographies/bibliography.bib"
                                      "~/Dropbox/bibliographies/doctorat.bib")) 
    (bibtex-completion-notes-path '"~/Dropbox/org/notes/references")
    (bibtex-completion-library-path '("~/Dropbox/org/notes/references/"))
    (bibtex-completion-pdf-field "file")
    (bibtex-completion-pdf-symbol "P")
    (bibtex-completion-notes-symbol "N")
    (ivy-bibtex-default-action 'ivy-bibtex-edit-notes)
    (bibtex-completion-additional-search-fields '(editor journaltitle origdate subtitle volume booktitle location publisher))

    :config 
 (setq bibtex-completion-display-formats
     '((article       . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title}: ${subtitle} @ ${journaltitle} [${=key=}]")
       (book          . "${=has-pdf=:1}${=has-note=:1}| ${author} [${origdate}](${date:4}) ${title} ${volume}: ${subtitle} [${=key=}]")
       (inbook        . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} @ ${booktitle} [${=key=}]")
       (incollection  . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} @ ${booktitle} [${=key=}]")
       (inproceedings . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date:4}) ${title:55} @ ${booktitle} [${=key=}]")
       (t             . "${=has-pdf=:1}${=has-note=:1}| ${author} (${date}) ${title}: ${subtitle} [${=key=}]")))

    (setq bibtex-completion-notes-template-multiple-files  
     "
#+TITLE: ${title}
#+SUBTITLE: ${subtitle}
#+AUTHOR: ${author-or-editor}
#+JOURNAL_TITLE: ${journaltitle}
#+BOOK_TITLE: ${booktitle}
#+BOOK_TITLE: ${booksubtitle}
#+ROAM_KEY: cite:${=key=}
#+CREATED: 
#+DATE: 
#+ROAM_TAGS: ${=key=} ${author-or-editor} 
#+INTERLEAVE_PDF: ${file}


")

    (defun sync0-bibtex-completion-journaltitle ()
                       (completing-read "Journal title : "
                        (delete-dups (mapcar #'(lambda (x) (cdr (assoc "journaltitle" x)))
                          (bibtex-completion-candidates)))))

    (defun sync0-bibtex-completion-author ()
                       (completing-read "Auteur : "
                        (delete-dups (mapcar #'(lambda (x) (cdr (assoc "author" x)))
                          (bibtex-completion-candidates)))))

(defun sync0-ivy-bibtex-extractor ()
  (interactive)
 (let*   ((pre-entry   (ivy-completing-read "Select from list: " (bibtex-completion-candidates)))
           (key   (progn (string-match "[[:blank:]]\\([[:graph:]]+$\\)" pre-entry)
                  (match-string 1 pre-entry)))
         (entry (bibtex-completion-get-entry1 key))
         (entity (ivy-completing-read "Choose one: " '("=key=" "title" "author" "journal" "date" "editor")))
         (extraction (bibtex-completion-get-value entity entry)))
       (insert  extraction)))

    (defun sync0-ivy-bibtex ()
      (interactive)
      (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
      (bibtex-completion-init)
      (let* ((candidates (bibtex-completion-candidates))
             (key (bibtex-completion-key-at-point))
             (preselect (and key
                             (cl-position-if (lambda (cand)
                                               (member (cons "=key=" key)
                                                       (cdr cand)))
                                             candidates))))
        (ivy-read "BibTeX entries%s: "
                  candidates
                  :preselect preselect
                  :caller 'ivy-bibtex
                  :action ivy-bibtex-default-action))))

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
;;   (require 'ivy-bibtex)
   (autoload 'ivy-bibtex "ivy-bibtex" "" t)


   (defvar sync0-bibtex-reference-keys
     (lazy-completion-table sync0-bibtex-reference-keys
                            (lambda () (sync0-bibtex-parse-keys nil t)))
     "Completion table for BibTeX reference keys.
 The CDRs of the elements are t for header keys and nil for crossref keys.")

   (defun sync0-bibtex-autokey-get-year ()
     "Return year field contents as a string obeying `bibtex-autokey-year-length'."
     (let ((yearfield (bibtex-autokey-get-field "date")))
       (substring yearfield (max 0 (- (length yearfield)
                                      bibtex-autokey-year-length)))))

   (defun sync0-bibtex-generate-autokey ()
     "This overwrites the bibtex-generate-autokey function that comes with Emacs.
           I want my keys to be formatted: authornameYEAR, then a letter
           if there is already an entry that matches authornameYEAR."
     (interactive)
     (let* ((names (bibtex-autokey-get-names))
            (year (sync0-bibtex-autokey-get-year))
            (existing-keys (bibtex-parse-keys)) key)
       (setq key (format "%s%s" names year))
       (let ((ret key))
         (cl-loop for c
                  from ?a to ?z
                  while (assoc ret existing-keys)
                  do (setq ret (format "%s%c" key c)))
         ret)))

   (defun sync0-bibtex-parse-keys (&optional abortable verbose)
     "Set `bibtex-reference-keys' to the keys used in the whole buffer.
 Find both entry keys and crossref entries.  If ABORTABLE is non-nil abort
 on user input.  If VERBOSE is non-nil give messages about progress.
 Return alist of keys if parsing was completed, `aborted' otherwise.
 If `bibtex-parse-keys-fast' is non-nil, use fast but simplified algorithm
 for parsing BibTeX keys.  If parsing fails, try to set this variable to nil."
     (if (eq major-mode 'bibtex-mode)
         (let (ref-keys crossref-keys)
           (save-excursion
             (save-match-data
               (if verbose
                   (bibtex-progress-message
                    (concat (buffer-name) ": parsing reference keys")))
               (catch 'userkey
                 (goto-char (point-min))
                 (if bibtex-parse-keys-fast
                     (let ((case-fold-search t)
                           (re (concat bibtex-entry-head "\\|"
                                       ",[ \t\n]*crossref[ \t\n]*=[ \t\n]*"
                                       "\\(\"[^\"]*\"\\|{[^}]*}\\)[ \t\n]*[,})]")))
                       (while (re-search-forward re nil t)
                         (if (and abortable (input-pending-p))
                             ;; user has aborted by typing a key: return `aborted'
                             (throw 'userkey 'aborted))
                         (cond ((match-end 3)
                                ;; This is a crossref.
                                (let ((key (buffer-substring-no-properties
                                            (1+ (match-beginning 3)) (1- (match-end 3)))))
                                  (unless (assoc key crossref-keys)
                                    (push (list key) crossref-keys))))
                               ;; only keys of known entries
                               ((assoc-string (bibtex-type-in-head)
                                              bibtex-entry-alist t)
                                ;; This is an entry.
                                (let ((key (bibtex-key-in-head)))
                                  (unless (assoc key ref-keys)
                                    (push (cons key t) ref-keys)))))))

                   (let (;; ignore @String entries because they are handled
                         ;; separately by `bibtex-parse-strings'
                         (bibtex-sort-ignore-string-entries t)
                         bounds)
                     (bibtex-map-entries
                      (lambda (key _beg end)
                        (if (and abortable
                                 (input-pending-p))
                            ;; user has aborted by typing a key: return `aborted'
                            (throw 'userkey 'aborted))
                        (if verbose (bibtex-progress-message))
                        (unless (assoc key ref-keys)
                          (push (cons key t) ref-keys))
                        (if (and (setq bounds (bibtex-search-forward-field "crossref" end))
                                 (setq key (bibtex-text-in-field-bounds bounds t))
                                 (not (assoc key crossref-keys)))
                            (push (list key) crossref-keys))))))

                 (dolist (key crossref-keys)
                   (unless (assoc (car key) ref-keys) (push key ref-keys)))
                 (if verbose
                     (bibtex-progress-message 'done))
                 ;; successful operation --> return `bibtex-reference-keys'
                 (setq bibtex-reference-keys ref-keys)))))))

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

   (with-eval-after-load 'evil
     (evil-define-key 'normal bibtex-mode-map
       "K" 'sync0-bibtex-previous-key
       "J" 'sync0-bibtex-next-key))

   ;; Define default fields.
   (setq bibtex-BibTeX-entry-alist '(("Article" "Article in Journal"
                                      ("author")
                                      ("date")
                                      ("title" "Title of the article (BibTeX converts it to lowercase)")
                                      ("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                      ("journaltitle")
                                      ("journalsubtitle")
                                      ("volume" "Volume of the journal")
                                      ("number" "Number of the journal (only allowed if entry contains volume)")
                                      ("issue" "Issue in the journal")
                                      ("pages" "Pages in the journal")
                                      ("url" "Pages in the journal")
                                      ("urldate" "Pages in the journal")
                                      ("doi" "Pages in the journal")
                                      ("library" "Pages in the journal")
                                      ("language" "Pages in the journal")
                                      ("langid" "Pages in the journal")
                                      ("langidopts" "Pages in the journal")
                                      ("file" "Pages in the journal")
                                      ("addendum" "Pages in the journal")
                                      ("keywords"))
                                     ("InProceedings" "Article in Conference Proceedings"
                                      ("author")
                                      ("date")
                                      ("title" "Title of the article (BibTeX converts it to lowercase)")
                                      ("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                      ("crossref")
                                      ("booktitle" "Name of the conference proceedings")
                                      ("booksubtitle" "Name of the conference proceedings")
                                      ("organization")
                                      ("eventdate")
                                      ("eventtitle")
                                      ("venue")
                                      ("series")
                                      ("volume" "Volume of the conference proceedings in the series")
                                      ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                                      ("pages" "Pages in the conference proceedings")
                                      ("edition" "Pages in the conference proceedings")
                                      ("publisher" "Publishing company, its location")
                                      ("editor" "Publishing company, its location")
                                      ("translator" "Publishing company, its location")
                                      ("location" "Publishing company, its location")
                                      ("url" "Publishing company, its location")
                                      ("urldate" "Publishing company, its location")
                                      ("doi" "Pages in the journal")
                                      ("library" "Pages in the journal")
                                      ("language" "Pages in the journal")
                                      ("langid" "Pages in the journal")
                                      ("langidopts" "Pages in the journal")
                                      ("file" "Pages in the journal")
                                      ("addendum")
                                      ("keywords"))
                                     ("InCollection" "Article in a Collection"
                                      (("author")
                                       ("title" "Title of the article (BibTeX converts it to lowercase)"))
                                      (("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                       ("date")
                                       ("crossref" "Title of the article (BibTeX converts it to lowercase)")
                                       ("booktitle" "Name of the conference proceedings")
                                       ("booksubtitle" "Name of the conference proceedings")
                                       ("series")
                                       ("volume" "Volume of the conference proceedings in the series")
                                       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                                       ("chapter" "Number of the conference proceedings in a small series (overwritten by volume)")
                                       ("pages" "Pages in the conference proceedings")
                                       ("edition" "Publishing company, its location")
                                       ("publisher" "Publishing company, its location")
                                       ("editor" "Publishing company, its location")
                                       ("translator" "Publishing company, its location")
                                       ("location" "Publishing company, its location")
                                       ("url" "Publishing company, its location")
                                       ("urldate" "Publishing company, its location")
                                       ("doi" "Pages in the journal")
                                       ("library" "Pages in the journal")
                                       ("language" "Pages in the journal")
                                       ("langid" "Pages in the journal")
                                       ("langidopts" "Pages in the journal")
                                       ("file" "Pages in the journal")
                                       ("addendum")
                                       ("keywords")))
                                     ("InBook" "Chapter or Pages in a Book"
                                      (("title" "Title of the article (BibTeX converts it to lowercase)"))
                                      (("author")
                                       ("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                       ("date")
                                       ("origdate")
                                       ("origtitle")
                                       ("crossref" "Title of the article (BibTeX converts it to lowercase)")
                                       ("booktitle" "Name of the conference proceedings")
                                       ("booksubtitle" "Name of the conference proceedings")
                                       ("series")
                                       ("volume" "Volume of the conference proceedings in the series")
                                       ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                                       ("chapter" "Number of the conference proceedings in a small series (overwritten by volume)")
                                       ("pages" "Pages in the conference proceedings")
                                       ("edition" "Publishing company, its location")
                                       ("publisher" "Publishing company, its location")
                                       ("editor" "Publishing company, its location")
                                       ("translator" "Publishing company, its location")
                                       ("location" "Publishing company, its location")
                                       ("url" "Publishing company, its location")
                                       ("urldate" "Publishing company, its location")
                                       ("doi" "Pages in the journal")
                                       ("library" "Pages in the journal")
                                       ("language" "Pages in the journal")
                                       ("langid" "Pages in the journal")
                                       ("langidopts" "Pages in the journal")
                                       ("file" "Pages in the journal")
                                       ("addendum")
                                       ("keywords")))
                                     ("Proceedings" "Conference Proceedings"
                                      ("title" "Title of the conference proceedings")
                                      ("date")
                                      nil
                                      ("booktitle" "Title of the proceedings for cross references")
                                      ("editor")
                                      ("volume" "Volume of the conference proceedings in the series")
                                      ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                                      ("series" "Series in which the conference proceedings appeared")
                                      ("address")
                                      ("month")
                                      ("organization" "Sponsoring organization of the conference")
                                      ("publisher" "Publishing company, its location")
                                      ("note"))
                                     ("Book" "Book"
                                      ("author")
                                      ("date")
                                      ("origdate")
                                      ("origtitle")
                                      ("title" "Title of the article (BibTeX converts it to lowercase)")
                                      ("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                      ("booktitle" "Name of the conference proceedings")
                                      ("booksubtitle" "Name of the conference proceedings")
                                      ("series")
                                      ("volume" "Volume of the conference proceedings in the series")
                                      ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
                                      ("edition" "Publishing company, its location")
                                      ("publisher" "Publishing company, its location")
                                      ("editor" "Publishing company, its location")
                                      ("translator" "Publishing company, its location")
                                      ("location" "Publishing company, its location")
                                      ("url" "Publishing company, its location")
                                      ("urldate" "Publishing company, its location")
                                      ("doi" "Pages in the journal")
                                      ("library" "Pages in the journal")
                                      ("isbn" "Pages in the journal")
                                      ("origlanguage" "Pages in the journal")
                                      ("language" "Pages in the journal")
                                      ("langid" "Pages in the journal")
                                      ("langidopts" "Pages in the journal")
                                      ("file" "Pages in the journal")
                                      ("addendum")
                                      ("keywords"))
                                     ("Unpublished" "Unpublished"
                                      ("author")
                                      ("date")
                                      ("title" "Title of the article (BibTeX converts it to lowercase)")
                                      ("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                      ("type" "Title of the article (BibTeX converts it to lowercase)")
                                      ("eventdate" "Title of the article (BibTeX converts it to lowercase)")
                                      ("eventtitle" "Title of the article (BibTeX converts it to lowercase)")
                                      ("venue" "Title of the article (BibTeX converts it to lowercase)")
                                      ("location" "Title of the article (BibTeX converts it to lowercase)")
                                      ("url" "Publishing company, its location")
                                      ("urldate" "Publishing company, its location")
                                      ("doi" "Pages in the journal")
                                      ("library" "Pages in the journal")
                                      ("origlanguage" "Pages in the journal")
                                      ("language" "Pages in the journal")
                                      ("langid" "Pages in the journal")
                                      ("langidopts" "Pages in the journal")
                                      ("file" "Pages in the journal")
                                      ("addendum")
                                      ("keywords"))
                                     ("Misc" "Miscellaneous" nil nil
                                      (("title" "Title of the article (BibTeX converts it to lowercase)"))
                                      (("author")
                                       ("date")
                                       ("subtitle" "Title of the article (BibTeX converts it to lowercase)")
                                       ("organization" "Title of the article (BibTeX converts it to lowercase)")
                                       ("type" "Title of the article (BibTeX converts it to lowercase)")
                                       ("version" "Title of the article (BibTeX converts it to lowercase)")
                                       ("location" "Title of the article (BibTeX converts it to lowercase)")
                                       ("url" "Publishing company, its location")
                                       ("urldate" "Publishing company, its location")
                                       ("doi" "Pages in the journal")
                                       ("library" "Pages in the journal")
                                       ("origlanguage" "Pages in the journal")
                                       ("language" "Pages in the journal")
                                       ("langid" "Pages in the journal")
                                       ("langidopts" "Pages in the journal")
                                       ("file" "Pages in the journal")
                                       ("addendum")
                                       ("keywords")))))


   (setq bibtex-biblatex-entry-alist '(("Article" "Article in Journal"
 (("author")
  ("title")
  ("journaltitle")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("translator")
  ("annotator")
  ("commentator")
  ("subtitle")
  ("titleaddon")
  ("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("journalsubtitle")
  ("issuetitle")
  ("issuesubtitle")
  ("language")
  ("origlanguage")
  ("series")
  ("volume")
  ("number")
  ("eid")
  ("issue")
  ("month")
  ("pages")
  ("version")
  ("note")
  ("issn")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Book" "Single-Volume Book"
 (("author")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("MVBook" "Multi-Volume Book"
 (("author")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("language")
  ("origlanguage")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("InBook" "Chapter or Pages in a Book"
 (("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("author")
  ("booktitle"))
 (("bookauthor")
  ("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("BookInBook" "Book in Collection"
 (("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("author")
  ("booktitle"))
 (("bookauthor")
  ("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("SuppBook" "Supplemental Material in a Book"
 (("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("author")
  ("booktitle"))
 (("bookauthor")
  ("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Booklet" "Booklet (Bound, but no Publisher)"
 (("author" nil nil 0)
  ("editor" nil nil 0)
  ("title")
  ;; ("year" nil nil 1)
  ("date" nil nil 1))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("howpublished")
  ("type")
  ("note")
  ("location")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Collection" "Single-Volume Collection"
 (("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("MVCollection" "Multi-Volume Collection"
 (("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("language")
  ("origlanguage")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("InCollection" "Article in a Collection"
 (("author")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("booktitle"))
 (("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("SuppCollection" "Supplemental Material in a Collection"
 (("author")
  ("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("booktitle"))
 (("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Manual" "Technical Manual"
 (("author" nil nil 0)
  ("editor" nil nil 0)
  ("title")
  ;; ("year" nil nil 1)
  ("date" nil nil 1))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("edition")
  ("type")
  ("series")
  ("number")
  ("version")
  ("note")
  ("organization")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Misc" "Miscellaneous"
 (("author" nil nil 0)
  ("editor" nil nil 0)
  ("title")
  ;; ("year" nil nil 1)
  ("date" nil nil 1))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("howpublished")
  ("type")
  ("version")
  ("note")
  ("organization")
  ("location")
  ("date")
  ("month")
  ("year")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Online" "Online Resource"
 (("author" nil nil 0)
  ("editor" nil nil 0)
  ("title")
  ;; ("year" nil nil 1)
  ("date" nil nil 1)
  ("url"))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("version")
  ("note")
  ("organization")
  ("date")
  ("month")
  ("year")
  ("addendum")
  ("pubstate")
  ("urldate")))
("Patent" "Patent"
 (("author")
  ("title")
  ("number")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("holder")
  ("subtitle")
  ("titleaddon")
  ("type")
  ("version")
  ("location")
  ("note")
  ("date")
  ("month")
  ("year")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Periodical" "Complete Issue of a Periodical"
 (("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editora")
  ("editorb")
  ("editorc")
  ("subtitle")
  ("issuetitle")
  ("issuesubtitle")
  ("language")
  ("series")
  ("volume")
  ("number")
  ("issue")
  ("date")
  ("month")
  ("year")
  ("note")
  ("issn")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("SuppPeriodical" "Supplemental Material in a Periodical"
 (("author")
  ("title")
  ("journaltitle")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("translator")
  ("annotator")
  ("commentator")
  ("subtitle")
  ("titleaddon")
  ("editor")
  ("editora")
  ("editorb")
  ("editorc")
  ("journalsubtitle")
  ("issuetitle")
  ("issuesubtitle")
  ("language")
  ("origlanguage")
  ("series")
  ("volume")
  ("number")
  ("eid")
  ("issue")
  ("month")
  ("pages")
  ("version")
  ("note")
  ("issn")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Proceedings" "Single-Volume Conference Proceedings"
 (("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("eventtitle")
  ("eventdate")
  ("venue")
  ("language")
  ("editor")
  ("volume")
  ("part")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("organization")
  ("publisher")
  ("location")
  ("month")
  ("isbn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("MVProceedings" "Multi-Volume Conference Proceedings"
 (("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("subtitle")
  ("titleaddon")
  ("eventtitle")
  ("eventdate")
  ("venue")
  ("language")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("organization")
  ("publisher")
  ("location")
  ("month")
  ("isbn")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("InProceedings" "Article in Conference Proceedings"
 (("author")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("booktitle"))
 (("editor")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("eventtitle")
  ("eventdate")
  ("venue")
  ("language")
  ("volume")
  ("part")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("organization")
  ("publisher")
  ("location")
  ("month")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Reference" "Single-Volume Work of Reference"
 (("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("MVReference" "Multi-Volume Work of Reference"
 (("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("language")
  ("origlanguage")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("InReference" "Article in a Work of Reference"
 (("author")
  ("editor")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 (("booktitle"))
 (("editora")
  ("editorb")
  ("editorc")
  ("translator")
  ("annotator")
  ("commentator")
  ("introduction")
  ("foreword")
  ("afterword")
  ("subtitle")
  ("titleaddon")
  ("maintitle")
  ("mainsubtitle")
  ("maintitleaddon")
  ("booksubtitle")
  ("booktitleaddon")
  ("language")
  ("origlanguage")
  ("volume")
  ("part")
  ("edition")
  ("volumes")
  ("series")
  ("number")
  ("note")
  ("publisher")
  ("location")
  ("isbn")
  ("chapter")
  ("pages")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Report" "Technical or Research Report"
 (("author")
  ("title")
  ("type")
  ("institution")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("number")
  ("version")
  ("note")
  ("location")
  ("month")
  ("isrn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Thesis" "PhD. or Master's Thesis"
 (("author")
  ("title")
  ("type")
  ("institution")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("note")
  ("location")
  ("month")
  ("isbn")
  ("chapter")
  ("pages")
  ("pagetotal")
  ("addendum")
  ("pubstate")
  ("doi")
  ("eprint")
  ("eprintclass")
  ("eprinttype")
  ("url")
  ("urldate")))
("Unpublished" "Unpublished"
 (("author")
  ("title")
  ;; ("year" nil nil 0)
  ("date" nil nil 0))
 nil
 (("subtitle")
  ("titleaddon")
  ("language")
  ("howpublished")
  ("note")
  ("location")
  ("isbn")
  ("date")
  ("month")
  ("year")
  ("addendum")
  ("pubstate")
  ("url")
  ("urldate")))))

                                       )

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
