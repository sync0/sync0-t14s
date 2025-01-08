(use-package vertico
  :demand t                             ; Otherwise won't get loaded immediately
  :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :hook (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode))

(use-package embark
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
		'("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  nil
                  (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :demand t
  ;; :bind
  ;; ([remap Info-search] . consult-info)
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"))

(use-package company-lsp
  :after company-mode
  :config
  (setq company-lsp-cache-candidates 'auto
        company-lsp-async t
        company-lsp-enable-snippet nil
        company-lsp-enable-recompletion t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 10
        company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"            :face 'all-the-icons-pink))))))

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-global-modes '(not erc-mode message-mode deft-mode help-mode gud-mode neotree-mode))
  ;; (company-frontends '(company-pseudo-tooltip-frontend 
  ;;                      company-echo-metadata-frontend))  
  (company-backends '(company-capf))
  (company-auto-complete nil)
  :config
  ;; Disable company-mode in bibtex-mode (clashes with yasnippets)
  ;; (add-hook 'bibtex-mode-hook (company-mode -1))

  (defvar +company-backend-alist
    '((text-mode company-capf company-yasnippet)
      (markdown-mode company-capf company-yasnippet company-ispell)
      (org-mode company-capf company-yasnippet company-elisp company-ispell)
      ;; '((text-mode company-capf  company-yasnippet company-org-roam)
      ;; '((text-mode company-capf  company-yasnippet company-ispell company-org-roam)
      ;; '((text-mode company-capf company-dabbrev company-yasnippet company-ispell company-org-roam)
      ;;(text-mode company-capf company-yasnippet company-ispell company-bibtex)
      (graphviz-dot-mode company-capf company-graphviz-dot-backend company-yasnippet)
      (prog-mode company-capf company-lsp company-yasnippet company-files)
      (elisp-mode company-elisp company-capf company-yasnippet company-files)
      ;; (nxml-mode company-capf company-yasnippet company-nxml)
      ;; (python-mode company-capf company-yasnippet company-jedi)
      (conf-mode company-capf company-dabbrev-code company-yasnippet))
    "An alist matching modes to company backends. The backends for any mode is
       built from this.")

  (defun +company--backends ()
    (let (backends)
      (let ((mode major-mode)
            (modes (list major-mode)))
        (while (setq mode (get mode 'derived-mode-parent))
          (push mode modes))
        (dolist (mode modes)
          (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                   (default-value 'company-backends)))
            (push backend backends)))
        (delete-dups
         (append (cl-loop for (mode . backends) in +company-backend-alist
                          if (or (eq major-mode mode)  ; major modes
                                 (and (boundp mode)
                                      (symbol-value mode))) ; minor modes
                          append backends)
                 (nreverse backends))))))

  (defun doom-temp-buffer-p (buf)
    "Returns non-nil if BUF is temporary."
    (equal (substring (buffer-name buf) 0 1) " "))

  (defun +company-init-backends-h ()
    "Set `company-backends' for the current buffer."
    (or (memq major-mode '(fundamental-mode special-mode))
        buffer-read-only
        (doom-temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
        (setq-local company-backends (+company--backends))))

  (put '+company-init-backends-h 'permanent-local-hook t)

  (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)

  (defun sync0-config-prose-completion ()
    "Make auto-complete less agressive in this buffer."
    (setq-local company-minimum-prefix-length 4))

  (add-hook 'text-mode-hook #'sync0-config-prose-completion))

(provide 'sync0-completion)
