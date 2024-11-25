
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
  :hook (prog-mode . hl-line-mode)
  :custom
  ;; I don't need hl-line showing in other windows. This also offers a small
  ;; speed boost when buffer is displayed in multiple windows.
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))

(use-package palette
  :straight nil
  :init
  (require 'hexrgb)
  :commands palette
  :load-path "~/.emacs.d/sync0/")


(provide 'sync0-appearance)
