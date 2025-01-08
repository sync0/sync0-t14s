(use-package evil-escape 
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
  (push #'minibufferp evil-escape-inhibit-functions))

(use-package evil  
    :custom
    ;; Make horizontal movement cross lines                                    
    (evil-cross-lines t)
    ;; turn off auto-indent 
    (evil-auto-indent t)
    :config 
    ;; Turn on evil mode when enabled.
    (evil-mode 1)
    ;; Turn on evil-escape mode when enabled.
    (evil-escape-mode 1)
    ;; prevent conflict with calf bindings. 
    ;; (add-to-list 'evil-emacs-state-modes 'cfw:details-mode)
 (use-package move-text
  :commands (move-text-up move-text-down)))

(provide 'sync0-evil)
