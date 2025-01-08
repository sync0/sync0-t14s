;; Define a local variable with the total number of lines.
(defvar-local sync0-mode-line-buffer-line-count nil)

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

(defun sync0-mode-line-zettel-identification ()
  "For org-mode files display contents of the TITLE keyword when
     not null. Otherwise, display the file title with extension."
  (if (derived-mode-p 'org-mode)
      (let* ((type (org-entry-get 1 "ZTYPE"))
	     (subtype (org-entry-get 1 "ZSTYPE"))
	     (uptype (when type (upcase (substring type 0 3))))
	     (upsubtype (when subtype (upcase (substring subtype 0 3))))
	     (type-string (if subtype
			      (concat uptype ":" upsubtype " ")
			    (concat uptype " "))))
	(unless (null type)
          (propertize type-string 'face '(:weight bold))))
    ""))

(defun sync0-mode-line-buffer-identification ()
  "For org-mode files display contents of the TITLE keyword when
     not null. Otherwise, display the file title with extension."
  (if (and (derived-mode-p 'org-mode)
           (org-keyword-title-p))
      (let*  ((title (cadar (org-collect-keywords '("TITLE")))) 
              (fixed-title (if (> (length title) 60) 
                               (let ((start (substring title 0 35))
                                     (end (substring title -20 nil)))
                                 (concat start  "..." end))
                             title))
	      (version (org-entry-get 1 "VERSION"))
	      (lastversion (org-entry-get 1 "LAST_VERSION"))
	      (versionstring 
	       (cond ((and version lastversion
			   (string= version lastversion))
		      (concat " V"  version))
		     (lastversion
		      (concat " V"  version "/" lastversion))
		     (version
		      (concat " V"  version))
		     (t ""))))
	    (concat "["
		    (propertize (buffer-name) 'face '(:weight bold) 'help-echo (buffer-file-name))
		    "] "
		    (propertize fixed-title 'face '(:height 1.0 :family "Verlag") 'help-echo (buffer-file-name))
		    (propertize versionstring 'face '(:weight bold) 'help-echo (buffer-file-name))))
    ;; (propertize fixed-title 'face '(:height 1.0 :family "Helvetica Neue LT Std" :width condensed :weight medium) 'help-echo (buffer-file-name))
    ;; (propertize fixed-title 'face '(:height 1.0 :family "Myriad Pro" :weight medium) 'help-echo (buffer-file-name))
    (propertize (buffer-name) 'face '(:weight bold) 'help-echo (buffer-file-name))))

;; (defun sync0-mode-line-buffer-identification ()
;;   (propertize (buffer-name) 'face '(:weight bold) 'help-echo (buffer-file-name)))

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
                (:eval (sync0-mode-line-zettel-identification))
                (:eval (sync0-mode-line-buffer-identification))
                "  " 
                (:eval (sync0-mode-line-guess-language))
                ;; evil-mode-line-tag
                "  "
                (:eval 
                 (let ((line-string "%l"))
                   (if (derived-mode-p 'pdf-view-mode)
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

(provide 'sync0-modeline)
