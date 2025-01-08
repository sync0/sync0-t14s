
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
;; Turn off default package manager
(setq package-enable-at-startup nil)

(setq use-package-verbose t)
;; bind-key is necessary to allow use-package to bind keys through the ":bind" keyword.
(require 'bind-key)

(add-to-list 'load-path (concat user-emacs-directory "sync0/"))

(use-package cl-lib
     :straight nil)

(require 'yaml)
(require 'sync0-user)
(require 'sync0-vars)
(require 'sync0-bibtex-vars)
(require 'sync0-zettelkasten)
(require 'sync0-predicates)
(require 'sync0-functions)

(require 'sync0-evil)

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  :config
  (require 'sync0-yaml))

(require 'sync0-custom)
(require 'sync0-sane-defaults)
(require 'sync0-completion)

;; Setup for bibtex things
(require 'sync0-bibtex)
(require 'sync0-bibtex-completion)

(require 'sync0-org-stuff)
(require 'sync0-org-agenda)
(require 'sync0-org-roam)
(require 'sync0-org)

(require 'sync0-corrections)
;; Rest
(require 'sync0-scratch)
(require 'sync0-text)
(require 'sync0-url)
(require 'sync0-window)
(require 'sync0-modeline)

(require 'sync0-appearance)

(use-package magit
  ;; :commands (magit-status magit-blame)
  :custom
  (magit-branch-arguments nil)
  (magit-push-always-verify nil)
  ;; Get rid of the previous advice to go into fullscreen
  (magit-restore-window-configuration t))

(require 'sync0-markdown)
(require 'sync0-bibtex-markdown)
(require 'sync0-obsidian)
;; (require 'sync0-bibtex-bindings)
(require 'sync0-programming)
(require 'sync0-python)

(use-package lua-mode) 

(use-package yasnippet
  :config
  (yas-global-mode 1)  ;; Enable yasnippet globally
  ;; Optional: Fix conflict with Yasnippets in Org-mode
  (defun yas-org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))
  ;; Optionally, reload all snippets to ensure everything is loaded
  (yas-reload-all))

(use-package csv-mode
  :disabled t
  :custom
  (csv-separators ";"))

(use-package sqlite-mode
  :straight nil
  :config
  (require 'sync0-bibtex-sql))

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

  (unbind-key "<SPC>" pdf-view-mode-map))

(use-package pdf-outline
  :straight nil
  :after pdf-tools)

(require 'sync0-keybindings)
