
(use-package custom
  :straight nil
  :custom
  ;; Allow automatic theme changing 
  (custom--inhibit-theme-enable nil)
  ;; Stop asking whether themes are safe
  (custom-safe-themes t)
  :config
  ;; Set CUSTOM directory.
  (setq custom-file (expand-file-name "custom_settings.el" user-emacs-directory))
  ;; Default settings for all themes.
;;   (custom-theme-set-faces 'user
;;                           ;; `(org-default ((t (:family "Minion Pro" :style display :height 1.0))))
;;                           `(markdown-header-face ((t (:family "Verlag" :weight normal :background nil :inherit variable-pitch))))
;;                           ;; `(markdown-header-face ((t (:family "Verlag" :weight light :width condensed :background nil :inherit variable-pitch))))
;;                           `(markdown-metadata-key-face ((t (:family "Inconsolata" :weight bold :height 0.9 :slant normal :spacing monospace :background nil :inherit fixed-pitch)))) 
;;                           `(markdown-metadata-value-face ((t (:family "Inconsolata" :height 0.9 :slant normal :spacing monospace :inherit fixed-pitch)))) 
;;                           `(markdown-gfm-checkbox-face ((t (:family "Inconsolata" :weight bold :spacing monospace))))
;;                           `(markdown-footnote-marker-face ((t (:family "Literata" :style small :weight normal :height 0.7))))
;;                           `(markdown-link-face ((t (:family "Literata" :weight light :underline nil :foreground "#268bd2" :background nil :height 1.0 :inherit variable-pitch))))
;;                           ;; `(markdown-link-face ((t (:family "Literata"  :underline t :background nil :height 1.0 :inherit variable-pitch))))
;;                           `(markdown-markup-face ((t (:family "Literata" :weight light  :underline nil :background nil :height 1.0 :inherit variable-pitch))))
;;                           `(markdown-url-face ((t (:family "Literata" :weight light  :underline nil :background nil :height 1.0 :inherit variable-pitch))))
;;                           `(markdown-plain-url-face ((t (:inherit markdown-url-face))))
;;                           `(markdown-code-face ((t (:family "Inconsolata"  :height 1.0 :spacing monospace :inherit fixed-pitch))))
;;                           `(markdown-reference-face ((t (:inherit markdown-code-face))))
;;                           `(org-default ((t (:family "Literata" :weight light  :inherit variable-pitch))))
;;                           `(org-num-face ((t (:family "Literata" :weight light  :inherit variable-pitch))))
;;                           `(org-link ((t (:inherit org-default :underline t))))
;;                           ;; `(org-ref-cite-face ((t (:inherit org-link)))) 
;;                           `(org-footnote ((t (:family "Literata" :style small :weight normal :height 0.7))))
;;                           `(org-checkbox ((t (:family "Inconsolata" :weight bold :spacing monospace))))
;;                           `(org-document-title ((t (:family "Verlag" :height 2.074 :inherit variable-pitch))))
;;                           `(org-document-info ((t (:family "Verlag" :height 1.728 :inherit variable-pitch))))
;;                           `(org-level-1 ((t (:family "Verlag" :height 2.074 :inherit variable-pitch))))
;;                           `(org-level-2 ((t (:family "Verlag" :height 1.728 :inherit variable-pitch))))
;;                           `(org-level-3 ((t (:family "Verlag" :height 1.44 :inherit variable-pitch))))
;;                           `(org-level-4 ((t (:family "Verlag" :height 1.2 :inherit variable-pitch))))
;;                           `(org-level-5 ((t (:family "Verlag" :height 1.0 :inherit variable-pitch))))
;;                           `(org-level-6 ((t (:family "Verlag" :height 0.833 :inherit variable-pitch))))
;;                           `(org-meta-line ((t (:family "Inconsolata" :height 0.95 :slant normal :spacing monospace :inherit fixed-pitch)))) 
;;                           `(org-document-info-keyword ((t (:inherit org-meta-line))))
;;                           `(org-special-keywords ((t (:inherit org-meta-line))))
;;                           `(org-todo ((t (:family "Noto Sans CJK KR" :height 0.95 :slant normal :spacing monospace :inherit fixed-pitch)))) 
;;                           `(org-drawer ((t (:inherit org-meta-line)))) 
;;                           `(org-property-value ((t (:inherit org-meta-line)))) 
;;                           `(org-ellipsis ((t (:family "Fira Code" :underline nil :box nil)))) 
;;                           `(org-date ((t (:family "Inconsolata" :height 0.95 :spacing monospace :inherit fixed-pitch))))
;;                           `(org-agenda-date ((t (:family "Verlag" :height 1.563  :inherit variable-pitch))))
;;                           `(org-agenda-date-weekend ((t (:family "Verlag" :height 1.563 :inherit variable-pitch))))
;;                           `(org-agenda-date-today ((t (:family "Verlag" :height 1.563  :inherit variable-pitch))))
;;                           `(org-agenda-structure ((t (:family "Verlag" :height 1.953 :inherit variable-pitch))))
;;                           `(org-scheduled ((t (:weight medium :slant normal))))
;;                           `(org-scheduled-today ((t (:family "Inconsolata" :weight medium :slant normal :spacing monospace :inherit fixed-pitch))))
;;                           `(org-scheduled-previously ((t (:family "Inconsolata" :weight normal :slant normal :spacing monospace :inherit fixed-pitch))))
;;                           `(org-upcoming-deadline ((t (:inherit org-scheduled-previously))))
;;                           `(org-agenda-diary ((t (:family "Inconsolata" :spacing monospace :inherit fixed-pitch))))
;;                           `(org-agenda-done ((t (:strike-through t))))
;;                           `(org-table ((t (:family "Inconsolata" :height 0.95 :spacing monospace :inherit fixed-pitch))))
;;                           `(org-block ((t (:family "Inconsolata" :height 0.95 :spacing monospace :background nil :inherit fixed-pitch))))
;; ;;                           `(org-block ((t (:family "Literata" :underline nil :background nil :height 1.05 :inherit variable-pitch))))
;;                           `(org-quote ((t (:family "Literata" :underline nil :background nil :height 1.05 :inherit variable-pitch))))
;;                           `(org-block-begin-line ((t (:family "Inconsolata" :height 0.95 :spacing monospace :weight bold :inherit fixed-pitch :background nil))))
;;                           `(org-block-end-line ((t (:inherit org-block-begin-line))))
;;                           `(org-tag ((t (:family "Inconsolata" :height 0.75 :spacing monospace :inherit fixed-pitch)))))

(custom-theme-set-faces 'user
                        ;; Org document structure
                        `(org-document-title ((t (:family "Verlag" :height 2.074 :inherit variable-pitch))))
                        `(org-document-info ((t (:family "Verlag" :height 1.728 :inherit variable-pitch))))
                         ;; `(org-document-info-keyword ((t (:inherit org-meta-line))))
                        `(org-document-info-keyword ((t (:family "Inconsolata" :height 0.95 :slant normal :inherit fixed-pitch))))
                        `(org-default ((t (:family "Literata" :weight light  :inherit variable-pitch))))
                        `(org-date ((t (:family "Inconsolata" :height 0.95 :spacing monospace :inherit fixed-pitch))))
                        `(org-quote ((t (:family "Literata" :underline nil :background nil :height 1.05 :inherit variable-pitch))))
                        `(org-num-face ((t (:family "Literata" :weight light  :inherit variable-pitch))))

                        ;; Org headlines with perfect fourth scale
                        `(org-level-1 ((t (:family "Verlag" :height 2.074 :inherit variable-pitch))))
                        `(org-level-2 ((t (:family "Verlag" :height 1.728 :inherit variable-pitch))))
                        `(org-level-3 ((t (:family "Verlag" :height 1.44 :inherit variable-pitch))))
                        `(org-level-4 ((t (:family "Verlag" :height 1.2 :inherit variable-pitch))))
                        `(org-level-5 ((t (:family "Verlag" :height 1.0 :inherit variable-pitch))))
                        `(org-level-6 ((t (:family "Verlag" :height 0.833 :inherit variable-pitch))))

                        ;; Org meta information
                        `(org-meta-line ((t (:family "Inconsolata" :height 0.95 :slant normal :inherit fixed-pitch))))
                        `(org-property-value ((t (:family "Inconsolata" :height 0.95 :slant normal :inherit fixed-pitch))))
                        `(org-special-keyword ((t (:family "Inconsolata" :height 0.95 :slant normal :inherit fixed-pitch))))
                        `(org-drawer ((t (:inherit org-meta-line)))) 
                        `(org-todo ((t (:family "Noto Sans CJK KR" :height 0.95 :slant normal :spacing monospace :inherit fixed-pitch)))) 
                        `(org-ellipsis ((t (:family "Fira Code" :underline nil :box nil)))) 
                        `(org-checkbox ((t (:family "Inconsolata" :weight bold :inherit fixed-pitch))))
                        `(org-table ((t (:family "Inconsolata" :height 0.95 :inherit fixed-pitch))))
                        `(org-block ((t (:family "Inconsolata" :height 0.95 :inherit fixed-pitch :background nil))))
                        `(org-block-begin-line ((t (:inherit org-block))))
                        `(org-block-end-line ((t (:inherit org-block-begin-line))))

                        ;; Org agenda
                        `(org-agenda-date ((t (:family "Verlag" :height 1.563 :inherit variable-pitch))))
                        `(org-agenda-date-weekend ((t (:inherit org-agenda-date))))
                        `(org-agenda-date-today ((t (:inherit org-agenda-date :weight bold))))
                        `(org-agenda-structure ((t (:family "Verlag" :height 1.953 :inherit variable-pitch))))
                        `(org-scheduled ((t (:weight medium :slant normal))))
                        `(org-scheduled-today ((t (:family "Inconsolata" :weight medium :slant normal :spacing monospace :inherit fixed-pitch))))
                        `(org-scheduled-previously ((t (:family "Inconsolata" :weight normal :slant normal :spacing monospace :inherit fixed-pitch))))
                        `(org-upcoming-deadline ((t (:inherit org-scheduled-previously))))
                        `(org-agenda-diary ((t (:family "Inconsolata" :spacing monospace :inherit fixed-pitch))))
                        `(org-agenda-done ((t (:strike-through t))))

                        ;; Org links and special elements
                        `(org-link ((t (:inherit org-default :underline t))))
                        `(org-footnote ((t (:family "Literata" :height 0.7 :inherit variable-pitch))))
                        `(org-tag ((t (:family "Inconsolata" :height 0.75 :inherit fixed-pitch))))

                        ;; Markdown headers with similar scaling
                        `(markdown-header-face ((t (:family "Verlag" :background nil :weight normal :inherit variable-pitch))))
                        `(markdown-metadata-key-face ((t (:family "Inconsolata" :weight bold :height 0.9 :slant normal :spacing monospace :background nil :inherit fixed-pitch))))
                        `(markdown-metadata-value-face ((t (:family "Inconsolata" :weight normal :height 0.9 :slant normal :spacing monospace :background nil :inherit fixed-pitch))))
                        `(markdown-gfm-checkbox-face ((t (:family "Inconsolata" :weight bold :spacing monospace))))
                        `(markdown-footnote-marker-face ((t (:family "Literata" :style small :weight normal :height 0.7))))
                        `(markdown-link-face ((t (:family "Literata" :weight light :underline nil :foreground "#268bd2" :background nil :height 1.0 :inherit variable-pitch))))
                        `(markdown-markup-face ((t (:family "Literata" :weight light  :underline nil :background nil :height 1.0 :inherit variable-pitch))))
                        `(markdown-url-face ((t (:family "Literata" :weight light  :underline nil :background nil :height 1.0 :inherit variable-pitch))))
                        `(markdown-plain-url-face ((t (:inherit markdown-url-face))))
                        `(markdown-code-face ((t (:family "Inconsolata" :spacing monospace :inherit fixed-pitch))))
                        `(markdown-reference-face ((t (:inherit markdown-code-face)))))


  )

(provide 'sync0-custom)
