
(use-package all-the-icons 
  ;; improve performance 
  :custom (inhibit-compacting-font-caches t))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package doom-themes  
  :after custom
  :init
  (load-theme 'doom-zenburn t)
  (load-theme 'doom-flatwhite t)
  :config
  (doom-themes-org-config))

(use-package hl-line 
  :straight nil
  :hook
  ((prog-mode . hl-line-mode)
   (text-mode . hl-line-mode))
;;   :init
;;   (require 'highlight)
  :custom
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.

;;   (defun sync0-hl-visual-line ()
;;     "Make `hl-line-mode` highlight the visual line in
;; `text-mode`-derived buffers."
;;     (interactive)
;;     (when (derived-mode-p 'text-mode)
;;       (let ((start (save-excursion (beginning-of-visual-line) (point)))
;;             (end (save-excursion (end-of-visual-line) (point))))
;; 	(hlt-highlight-region start end 'hl-line))))

;;   (defun sync0-hl-line-visual-mode ()
;;     "Toggle highlighting of the visual line for `hl-line-mode` in
;; `text-mode`-derived buffers."
;;     (interactive)
;;     (if hl-line-mode
;; 	(progn
;;           (add-hook 'post-command-hook #'sync0-hl-visual-line nil t)
;;           (hl-line-mode 1))
;;       (progn
;; 	(remove-hook 'post-command-hook #'sync0-hl-visual-line t)
;; 	(hl-line-mode -1))))

;;   (add-hook 'text-mode-hook #'sync0-hl-line-visual-mode)

;;   (defun sync0-update-visual-line-highlight ()
;;     "Update visual line highlighting when the window or wrapping changes."
;;     (when (and hl-line-mode (derived-mode-p 'text-mode))
;;       (sync0-hl-visual-line)))

;;   (add-hook 'window-configuration-change-hook #'sync0-update-visual-line-highlight)
;;   (add-hook 'visual-line-mode-hook #'sync0-update-visual-line-highlight)

  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

(use-package palette
  :straight nil
  :init
  (require 'hexrgb)
  :commands palette
  :load-path "~/.emacs.d/sync0/")


(provide 'sync0-appearance)
