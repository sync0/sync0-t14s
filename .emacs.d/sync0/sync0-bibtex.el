;; -*- lexical-binding: t -*-
(use-package bibtex
  :straight nil
  :custom
  (bibtex-dialect 'biblatex) ;; biblatex as default bib format
  (bibtex-maintain-sorted-entries t)
  (bibtex-field-delimiters 'braces)
  ;; This line is necessary to prevent strange problem
  ;; caused by lack of support for my bibtex key naming scheme
  ;; (bibtex-entry-maybe-empty-head t)
  (bibtex-comma-after-last-field t)
  (bibtex-text-indentation 0)
  (bibtex-autokey-names 0)
  (bibtex-autokey-name-length 0)
  (bibtex-autokey-year-title-separator "")
  (bibtex-autokey-titleword-length 0)
  (bibtex-autokey-year-length 0)
  (bibtex-autokey-titlewords 0)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 22)
  (bibtex-entry-format '(opts-or-alts page-dashes whitespace braces last-comma inherit-booktitle delimiters sort-fields realign))
  :config
  (require 'sync0-bibtex-vars)

  (unbind-key "TAB" bibtex-mode-map)

  (evil-define-key 'normal bibtex-mode-map
    "K" 'sync0-bibtex-previous-key
    "zf" 'bibtex-fill-entry
    "J" 'sync0-bibtex-next-key))

(provide 'sync0-bibtex)
