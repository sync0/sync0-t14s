;; End sentences with a single espace.
(use-package unidecode)

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)))

(setq-default sentence-end-double-space nil
              header-line-format " "
              ;; Use spaces instead of tabs
              indent-tabs-mode nil              
              ;; disable bidirectional text for tiny performance boost
              bidi-display-reordering nil 
              ;; Never truncate lines
              truncate-lines t
              truncate-partial-width-windows t
              ;; Help with displaying fonts
              inhibit-compacting-font-caches t)

(use-package auto-fill
  :straight nil
  :hook 
  (text-mode . turn-on-auto-fill)
  (markdown-mode . turn-off-auto-fill)
  :preface
  ;; Configure exceptions for auto-fill mode. 
  (defun sync0-nobreak-p ()
    (and (looking-at "+[[:alnum:]]")
         (looking-back "^\\\[A-z]+{.+" (line-beginning-position))))
  ;; Define column width for auto-fill mode. 
  :config
  (setq-local fill-column 70)
  ;; Set hook for exceptions to auto-fill-mode.
  (add-hook 'fill-nobreak-predicate #'sync0-nobreak-p))

(use-package visual-line
  :straight nil
  :commands visual-line-mode
  :hook 
  ;; (text-mode . visual-line-mode) 
  (markdown-mode . visual-line-mode))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom 
  (visual-fill-column-center-text t)
  :hook 
  (visual-line-mode . visual-fill-column-mode)
  :config (setq visual-fill-column-width 70))

(require 'sync0-abbrevs)

(use-package ispell
  :hook (text-mode . ispell-minor-mode)
  :custom
  ;; Save a new word to personal dictionary without asking
  (ispell-silently-savep t)
  ;; Set up hunspell dictionaries
  (ispell-hunspell-dict-paths-alist
   '(("en_US-large" "/usr/share/hunspell/en_US-large.aff")
     ("de_DE" "/usr/share/hunspell/de_DE.aff")
     ("it_IT" "/usr/share/hunspell/it_IT.aff")
     ("es" "/usr/share/hunspell/es.aff")
     ("pt_BR" "/usr/share/hunspell/pt_BR.aff")
     ("fr_FR" "/usr/share/hunspell/fr_FR.aff")))
  :config 
  ;; if hunspell does NOT exist, use aspell
  (cond ((executable-find "hunspell")
         (setq ispell-program-name "hunspell")
         ;;(setq ispell-local-dictionary "en_US")
         (setq ispell-local-dictionary-alist '(("en_US-large" "[[:alpha:]]" "[^[:alpha:]]" "['’-]" t ("-d" "en_US-large" ) nil utf-8)
                                               ("de_DE" "[[:alpha:]ÄÖÜéäöüß]" "[^[:alpha:]ÄÖÜéäöüß]" "['’-]" t ("-d" "de_DE") nil utf-8)
                                               ("es" "[[:alpha:]ÁÉÍÓÚÄËÏÖÜÑáéíóúäëïöüñ]" "[^[:alpha:]ÁÉÍÓÚÄËÏÖÜÑáéíóúäëïöüñ]" "['’-]" t ("-d" "es") nil utf-8)
                                               ("pt_BR" "[[:alpha:]a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]" "[^[:alpha:]a-zàáâãçéêíóôõúüA-ZÀÁÂÃÇÉÊÍÓÔÕÚÜ]" "['-]" t  ("-d" "pt_BR") nil utf-8)
                                               ("it_IT" "[[:alpha:]AEÉIOUàèéìòù]" "[^[:alpha:]AEÉIOUàèéìòù]" "['’-]" t ("-d" "it_IT") nil utf-8)
                                               ("fr_FR" "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]" "[’'-]" t ("-d" "fr_FR")  nil utf-8))))

        ((executable-find "aspell")
         (setq ispell-program-name "aspell")
         ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
         (setq ispell-extra-args '("--sug-mode=ultra"))))

  ;; This functions was borrowed from Artur Malabarba. See his discussion
  ;; here:
  ;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

  ;; Ignore sections of files for spellcheck
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXEMPLE"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_equation" . "#\\+END_equation"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_labeling" . "#\\+END_labeling"))
  (add-to-list 'ispell-skip-region-alist '("#\\+[A-z]+: .+$"))
  (add-to-list 'ispell-skip-region-alist '("\\[\\[" . "\\]\\]"))
  ;; ignore pandoc markdown citations
  (add-to-list 'ispell-skip-region-alist '("\\[@" "\\]"))
  (add-to-list 'ispell-skip-region-alist '("\\](" "md)"))
  (add-to-list 'ispell-skip-region-alist '("{\\." "}"))
  (add-to-list 'ispell-skip-region-alist '("^---$" "^---$"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_equation*" . "#\\+END_equation*"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_align" . "#\\+END_align"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_align*" . "#\\+END_align*"))
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("\\$" . "\\$")))

(require 'sync0-language)

(use-package flyspell 
  :hook (text-mode . flyspell-mode)
  :custom
  (ispell-parser 'tex)
  (flyspell-issue-message-flag nil))

(use-package smart-quotes 
  :hook (markdown-mode . smart-quotes-mode))

(use-package writeroom-mode
:commands writeroom-mode
:custom
(writeroom-width 70))

 (use-package follow-mode
  :straight nil
  :commands follow-mode
  :custom (follow-auto t))

(provide 'sync0-text)
