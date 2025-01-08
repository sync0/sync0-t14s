(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq default-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq save-interprogram-paste-before-kill t)

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
              visible-bell t
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
(setq text-scale-mode-step 1.05)
;; EVIL friendly keybindings for next-buffer
;; (global-set-key (kbd "M-h") 'next-buffer)
;; Quickly save
;; (global-set-key (kbd "M-w") 'save-buffer)
;; EVIL friendly keybindings for previous-buffer
;; (global-set-key (kbd "M-l") 'previous-buffer)

;; (require 'echo-bell)
;; (echo-bell-mode)

(use-package smooth-scrolling 
  ;; :commands (sync0-scroll-up sync0-scroll-down)
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

  :config (smooth-scrolling-mode 1))

(use-package warnings
  :straight nil
  :config
  ;; Remove annoying message when expanding yasnippets. 
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

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
  ;; (require 'dired-x)
  :hook (after-init . recentf-mode))

(use-package saveplace
  :straight nil
  :config (save-place-mode))

(use-package dired+ 
  :disabled t
  :after dired
  :custom
  (dired-listing-switches "-alhS"))

(use-package dired-quick-sort
  :after dired
  :custom
  (dired-listing-switches "-alhS")
  :config
  (dired-quick-sort-setup)
  (add-to-list 'evil-emacs-state-modes 'dired-mode))

(use-package which-key
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-width 0.33)
  (which-key-side-window-max-height 0.25)
  :config
  (which-key-mode))

(provide 'sync0-sane-defaults)
