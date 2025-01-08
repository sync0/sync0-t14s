(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode -1) 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-showhide-fringe-menu-customize-disable)

(setq-default 
 window-divider-default-bottom-width 2
 window-divider-default-right-width 2
 ;; Show both window dividers (right and bottom)
 window-divider-default-places 'right-only)

(add-hook 'emacs-startup-hook #'window-divider-mode)

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode org-mode neotree-mode markdown-mode deft-mode help-mode nov-mode pdf-view-mode org-agenda-mode)
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

;; (defun sync0-set-neotree-margins ()
;;   "Set margins in current buffer."
;;   (setq left-margin-width 0)
;;   (setq left-fringe-width 0)
;;   (setq right-margin-width 0))

(add-hook 'prog-mode-hook #'sync0-set-margins)
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook #'sync0-set-margins))

;; (add-hook 'bibtex-mode-hook #'sync0-set-margins)
;; (add-hook 'neotree-mode-hook #'sync0-set-neotree-margins)

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

(use-package workgroups2
  :config
  (workgroups-mode 1))

(provide 'sync0-window)
