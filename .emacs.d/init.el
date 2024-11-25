
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

(require 'sync0-user)

(use-package hydra)

(use-package major-mode-hydra
  :bind
  ("M-SPC" . major-mode-hydra)
  :custom
  (major-mode-hydra-invisible-quit-key "q"))

(require 'sync0-evil-packages)
(require 'sync0-org)
(require 'sync0-org-stuff)
(require 'sync0-custom)
(require 'sync0-sane-defaults)
(require 'sync0-vars)
;; Setup for bibtex things
(require 'sync0-corrections)
;; Rest
(require 'sync0-scratch)
(require 'sync0-predicates)
(require 'sync0-functions)
(require 'sync0-text)
(require 'sync0-url)
(require 'sync0-window)
(require 'sync0-modeline)

(use-package cl-lib
     :straight nil)

(use-package s)

(use-package f)


(use-package undo-tree
  :custom
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist '(("." . (concat sync0-emacs-directory "undo-tree-files/"))))
  (undo-tree-auto-save-history nil)
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
              ("k"  . previous-line))
  :hook (after-init . recentf-mode))

(use-package saveplace
  :straight nil
  :config (save-place-mode))

(use-package which-key
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-width 0.33)
  (which-key-side-window-max-height 0.25)
  :config
  (which-key-mode))

(require 'sync0-completion)

 (use-package move-text
  :commands (move-text-up move-text-down))

(use-package fcitx
  :straight t
  :custom
  (fcitx5-use-dbus t)  ;; or set to 'fcitx5 if you use fcitx5
  :config
  ;; (fcitx-aggressive-setup)
  )


(require 'sync0-appearance)

(use-package magit
  ;; :commands (magit-status magit-blame)
  :custom
  (magit-branch-arguments nil)
  (magit-push-always-verify nil)
  ;; Get rid of the previous advice to go into fullscreen
  (magit-restore-window-configuration t)
  :config
  (evil-leader/set-key  "g" 'magit-status))

(require 'sync0-bibtex)
(require 'sync0-markdown-setup)
(require 'sync0-bibtex-completion)
(require 'sync0-bibtex-markdown)
(require 'sync0-obsidian)
;; (require 'sync0-bibtex-bindings)
(require 'sync0-programming)
(require 'sync0-python)

(use-package lua-mode) 

(use-package yasnippet 
  :config
  ;; Fix conflict with Yasnippets
  ;; See https://emacs.stackexchange.com/questions/29758/yasnippets-and-org-mode-yas-next-field-or-maybe-expand-does-not-expand
  (defun yas-org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil)) (yas-expand))
    (yas-reload-all)
    (yas-global-mode)))

(use-package csv-mode
  :disabled t
  :custom
  (csv-separators ";"))

(use-package sqlite-mode
  :straight nil
  :config
  (require 'sync0-bibtex-sql))
