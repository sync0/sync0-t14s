;; Define a local variable with the total number of lines.
(defvar-local sync0-mode-line-buffer-line-count nil)

(defun sync0-toggle-mode-line () 
  "toggles the modeline on and off"
  (interactive) 
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)) )
  (redraw-display))

(use-package battery
  :custom
  (battery-mode-line-format "%t")
  (battery-update-interval 60)
  :config
  (display-battery-mode t))

;; Define a function that counts the number of lines in the
;; current buffer.
(defun sync0-mode-line-count-lines ()
  "Count the number of lines in the current buffer."
  (setq-local sync0-mode-line-buffer-line-count 
              (int-to-string (count-lines (point-min) (point-max)))))

;; Recalculate the total number of lines using hooks. This is
;; not the best approach, but I have not been able to devise a
;; dynamic way to calculate these that does not result in Emacs
;; "inventing" these results.
(add-hook 'find-file-hook #'sync0-mode-line-count-lines)
(add-hook 'after-save-hook #'sync0-mode-line-count-lines)
(add-hook 'after-revert-hook #'sync0-mode-line-count-lines)

   ;;; Taken from 
   ;;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline

(defun mode-line-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (when
      (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
	      'display
	      `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

;; (defun sync0-mode-line-zettel-identification ()
;;   "For org-mode files display contents of the TITLE keyword when
;;      not null. Otherwise, display the file title with extension."
;;   (if (equal major-mode 'org-mode)
;;       (if-let* ((type (org-entry-get 1 "ZETTEL_TYPE"))
;;                 (subtype (upcase-initials (substring type 0 3)))
;;                 ;; (subtype (upcase (substring type 0 1)))
;;                 (type-string (concat "[" subtype "] ")))
;;           (propertize type-string 'face '(:weight bold))
;;         "")
;;     ""))

;; (defun sync0-mode-line-buffer-identification ()
;;   "For org-mode files display contents of the TITLE keyword when
;;      not null. Otherwise, display the file title with extension."
;;   (if (and (equal major-mode 'org-mode)
;;            (org-keyword-title-p))
;;       (let*  ((title (cadar (org-collect-keywords '("TITLE")))) 
;;               (fixed-title (if (> (length title) 60) 
;;                                (let ((start (substring title 0 35))
;;                                      (end (substring title -20 nil)))
;;                                  (concat start  "..." end))
;;                              title)))
;;         (propertize fixed-title 'face '(:height 1.0 :family "Verlag") 'help-echo (buffer-file-name)))
;;     ;; (propertize fixed-title 'face '(:height 1.0 :family "Helvetica Neue LT Std" :width condensed :weight medium) 'help-echo (buffer-file-name))
;;     ;; (propertize fixed-title 'face '(:height 1.0 :family "Myriad Pro" :weight medium) 'help-echo (buffer-file-name))
;;     (propertize (buffer-name) 'face '(:weight bold) 'help-echo (buffer-file-name))))

(defun sync0-mode-line-buffer-identification ()
  (propertize (buffer-name) 'face '(:weight bold) 'help-echo (buffer-file-name)))


(defun sync0-mode-line-guess-language ()
  (if (boundp 'sync0-language-active-code) 
      (cond  ((string-equal sync0-language-active-code "en") 
              (propertize "EN" 'face '(:weight bold)))
             ((string-equal sync0-language-active-code "de") 
              (propertize "DE" 'face '(:weight bold)))
             ((string-equal sync0-language-active-code "pt") 
              (propertize "PT" 'face '(:weight bold)))
             ((string-equal sync0-language-active-code "it") 
              (propertize "IT" 'face '(:weight bold)))
             ((string-equal sync0-language-active-code "fr") 
              (propertize "FR" 'face '(:weight bold)))
             ((string-equal sync0-language-active-code "es") 
              (propertize "ES" 'face '(:weight bold)))
             (t (propertize "NIL" 'face '(:weight bold))))
    (propertize "NIL" 'face '(:weight bold))))

(setq-default mode-line-format
              '(" " 
                (:eval (cond 
                        (buffer-read-only (propertize "🔒"
                                                      'face '(:family "Noto Color Emoji")
                                                      'help-echo "buffer is read-only!!!"))
                        ((buffer-modified-p) (propertize "✗"
                                                         'face '(:family "Noto Color Emoji")))
                        (t (propertize "✓"
                                       'face '(:family "Noto Color Emoji")))))
                "  " 
                ;; (:eval (sync0-mode-line-zettel-identification))
                (:eval (sync0-mode-line-buffer-identification))
                "  " 
                (:eval (sync0-mode-line-guess-language))
                ;; evil-mode-line-tag
                "  "
                (:eval 
                 (let ((line-string "%l"))
                   (if (equal major-mode 'pdf-view-mode)
                       ;; this is necessary so that pdf-view displays the page numbers of the pdf
                       ;; otherwise, it is very hard to read documents. 
                       mode-line-position
                     (if (and (not (buffer-modified-p))
                              sync0-mode-line-buffer-line-count)
                         (setq line-string 
                               (concat "(" line-string "/" sync0-mode-line-buffer-line-count ")"))
                       (concat "(" line-string ")"))
                     )))
                (:eval (mode-line-fill 30))
                (:eval (if (equal debug-on-error nil)
                           (propertize "🐛" 'mouse-face 'mode-line-highlight 'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error) 'face '(:family "Noto Color Emoji"))
                         (propertize "🦋" 'mouse-face 'mode-line-highlight 'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error) 'face '(:family "Noto Color Emoji"))
                         ))

                " " 
                (:eval (propertize 
                        (s-replace "-mode" "" (format "%s" major-mode))
                        'face '(:weight bold)))
                " " 
                (vc-mode vc-mode)
                " " 
                (:eval (when (boundp 'org-mode-line-string)
                         (propertize  org-mode-line-string 'face '(:weight semi-bold))))
                ;; (:eval (propertize (format-time-string " %H:%M ")
                ;;                    'face '(:weight bold))) 
                ;; " " 
                (:eval  (propertize "⚡" 'face '(:family "Noto Color Emoji")))
                mode-line-misc-info
                emacs-mode-line-end-spaces))

(if (> (display-pixel-width) 1900)
    ;; high resolution font size (t14s)
    (progn (set-face-attribute 'default nil 
                               :family "Inconsolata"
                               :height 150)
           ;;:height 175
           (setq line-spacing 7))
  ;; low resolution font size
  (progn (set-face-attribute 'default nil 
                             :family "Inconsolata"
                             :height 130)
         (setq line-spacing 3)))

(defun sync0-buffer-face-proportional ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (if (> (display-pixel-width) 1900)
      ;; high resolution font size (t14s)
      (progn
        (setq buffer-face-mode-face '(:family "Literata" :height 165))
        (setq line-spacing 0.25))
    ;; low resolution font size
    (progn
      ;; (setq buffer-face-mode-face '(:family "Minion Pro" :height 155 :spacing proportional))
      (setq buffer-face-mode-face '(:family "Literata" :height 130))
      ;; (setq line-spacing 0.2)
      (setq line-spacing 0.225)))
  (buffer-face-mode))

;; (add-hook 'prog-mode-hook #'sync0-buffer-face-fixed)
(add-hook 'erc-mode-hook #'sync0-buffer-face-proportional)
(add-hook 'Info-mode-hook #'sync0-buffer-face-proportional)
(add-hook 'org-mode-hook #'sync0-buffer-face-proportional)
(add-hook 'markdown-mode-hook #'sync0-buffer-face-proportional)
;; (add-hook 'text-mode-hook #'sync0-buffer-face-proportional)

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode org-mode neotree-mode markdown-mode deft-mode help-mode nov-mode pdf-view-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(defun sync0-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 0)
  (setq right-margin-width 0))

;; (defun sync0-set-neotree-margins ()
;;   "Set margins in current buffer."
;;   (setq left-margin-width 0)
;;   (setq left-fringe-width 0)
;;   (setq right-margin-width 0))

(add-hook 'prog-mode-hook #'sync0-set-margins)
;; (add-hook 'bibtex-mode-hook #'sync0-set-margins)
;; (add-hook 'neotree-mode-hook #'sync0-set-neotree-margins)

(provide 'sync0-modeline)
