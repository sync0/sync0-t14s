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

(provide 'sync0-window)
