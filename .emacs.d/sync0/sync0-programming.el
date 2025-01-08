(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package paren
  :straight nil
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  ;; don't blink--too distracting
  (blink-matching-paren nil)
  (show-paren-when-point-inside-paren t)
  :config
  (show-paren-mode 1))

(use-package smartparens
  ;; :straight (smartparens :type git :host github :repo "Fuco1/smartparens") 
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

  ;; Make org-mode handle latex quotes without too much hassle
  (sp-local-pair 'org-mode "``" "''"
                 ;;:trigger nil
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-skip-double-quote))

  ;; Make org-mode handle latex quotes without too much hassle
  (sp-local-pair 'org-mode "`" "'"
                 ;; :trigger nil
                 :unless '(sp-latex-point-after-backslash)
                 :post-handlers '(sp-latex-skip-double-quote))

  ;; Do not complete the single quote pair at the end of words.
  ;; Othwersie, the apostrophe in English becomes caotic.
  (sp-local-pair 'org-mode "'" "'"
                 ;; :trigger nil
                 :unless '(sp-point-after-word-p)))

(use-package flycheck
  :commands flycheck-mode
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package lsp-mode
  :commands lsp
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-tex-server 'digestif)
  :hook ((python-mode . lsp)
         ;; (typescript-mode . lsp)
         ;; (js2-mode . lsp)
         (js-mode . lsp)
         (LaTeX-mode . lsp)
         ;; (tex-mode . lsp-deferred)
         ;; (nxml-mode . lsp)
         ;; (emacs-lisp-mode . lsp-deferred)
         (web-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(provide 'sync0-programming)
