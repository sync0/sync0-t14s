(use-package move-text)

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
  :hook (after-init . global-evil-leader-mode)
  :custom
  (evil-leader/in-all-states t)
  :config
  (evil-leader/set-leader "<SPC>")

  (evil-leader/set-key
    "1" 'delete-other-windows
    "2" 'sync0-split-and-follow-horizontally
    "3" 'sync0-split-and-follow-vertically
    "4" 'sync0-create-scratch-buffer
    "m" 'bookmark-set
    "q" 'keyboard-quit
    "w" 'write-file
    "e" 'sync0-eval-last-sexp-or-region
    "M" 'consult-imenu
    "s" 'save-buffer
    "R" 'revert-buffer
    "t" 'term
    "o" 'other-window
    "p" 'previous-buffer
    "n" 'next-buffer
    "N" 'sync0-find-next-file
    "k" 'quit-window
    "b" 'consult-buffer
    "K" 'kill-buffer-and-window)

  (defhydra sync0-hydra-help (:color amaranth :hint nil :exit t)
    "
   ^Help functions^
   ^^^------------------------
   Describe _f_unction
   Describe _v_ariable
   Describe _k_eybindings

   _q_uit
   "
    ;; Quickly work with bookmarks
    ("f" describe-function)
    ("v" describe-variable)
    ("k" describe-key)
    ("q"  nil :color blue))

    ;; ("l" counsel-load-library)
    ;; ("s" counsel-info-lookup-symbol)
    ;; ("u" counsel-unicode-char)

  (evil-leader/set-key
    "r" 'consult-recent-file
    "y" 'consult-yank-pop
    "j" 'consult-bookmark
    "f" 'find-file
    "x" 'consult-mode-command
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
    (require 'sync0-evil))

(provide 'sync0-evil-packages)

