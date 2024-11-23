    (require 'move-text)

    (defun sync0-insert-line-below ()
      "Insert an empty line below the current line."
      (interactive)
      (save-excursion
        (end-of-line)
        ;; To insert the line above
        ;; (end-of-line 0)
        (open-line 1)))

    ;; insert whitespace
    (defun sync0-insert-whitespace ()
      " Add a whitespace"
      (interactive)
      (insert " "))

    (defun sync0-delete-text-block ()
      "Delete selection or current or next text block and also copy to `kill-ring'.
               URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
               Version 2016-08-13"
      (interactive)
      (if (use-region-p)
          (kill-region (region-beginning) (region-end))
        (progn
          (beginning-of-line)
          (if (search-forward-regexp "[[:graph:]]" (line-end-position) 'NOERROR )
              (sync0-delete-current-text-block)
            (when (search-forward-regexp "[[:graph:]]" )
              (sync0-delete-current-text-block))))))

    ;; Change global key bindings
    (unbind-key "C-m" evil-normal-state-map)
    (unbind-key "M-." evil-normal-state-map)
    (unbind-key "C-d" evil-motion-state-map)
;; Used for calling ebib
    (unbind-key "C-e" evil-motion-state-map)
    ;; (unbind-key "<SPC>" evil-motion-state-map)


    (evil-define-key 'normal global-map
      ;; "zs" 'sync0-evil-swap-sentences-in-paragraph
      "zw" 'transpose-words
      "zl" 'transpose-lines
      "zp" 'transpose-paragraphs
      "zs" 'transpose-sentences
      "zk" 'move-text-up
      "zj" 'move-text-down)

    (evil-define-key 'normal global-map
      ;; "/" 'consult-line
      "gb" 'consult-bookmark
;;       "U" 'undo-tree-redo
      "U" 'undo
      "s" 'fill-paragraph
      "S" 'sync0-insert-line-below
      "M" 'bookmark-set)

    ;; (evil-define-key 'normal global-map
    ;;   "/" 'swiper
    ;;   "gb" 'counsel-bookmark
    ;;   "U" 'undo-tree-redo
    ;;   "s" 'fill-paragraph
    ;;   "S" 'sync0-insert-line-below
    ;;   "M" 'bookmark-set
    ;;   "zc" 'transpose-chars
    ;;   "zb" 'sync0-delete-text-block
    ;;   "zl" 'transpose-lines
    ;;   "zw" 'transpose-words
    ;;   "zj" 'evil-join
    ;;   "zp" 'transpose-paragraphs
    ;;   "zs" 'transpose-sentences)

    (evil-leader/set-key
      "<SPC>" 'sync0-insert-whitespace
      "<ESC>" 'keyboard-quit)

(evil-define-key '(normal visual insert motion) global-map (kbd "M-.") 'vertico-repeat)

    ;; Improve EVIL behavior with visual lines (visual-line-mode).
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(provide 'sync0-evil)
