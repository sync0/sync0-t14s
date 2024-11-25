(require 'markdown-mode)

(defvar markdown-toc-buffer-name "*Markdown TOC*"
  "Name of the buffer used to display the table of contents.")

(defface markdown-toc-header-face
  '((t (:inherit markdown-header-face)))
  "Face used for the headers in the table of contents.")

(defface markdown-toc-current-heading-face
  '((t (:background "#FFD700" :foreground "black" :weight bold)))
  "Face used to highlight the current heading in the table of contents.")

(defvar markdown-toc-timer nil
  "Name of the buffer used to display the table of contents.")

(defvar markdown-toc-highlight-max-retries 3
  "Maximum number of retries to highlight a heading in the TOC buffer.")

(defun markdown-toc-collect-headings ()
  "Collect raw headings from the current Markdown buffer.
Each heading is represented as a cons cell of the form (LEVEL . TEXT),
where LEVEL is the heading level (1 for `#`, 2 for `##`, etc.),
and TEXT is the heading text (raw, unprocessed)."
  (let (headings)
    (save-excursion
      (goto-char (point-min)) ;; Start from the beginning
      (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
        (let* ((heading-level (length (match-string 1)))    ;; Heading level
               (heading-text (match-string 2))) ;; Raw text of heading
          (push (cons heading-level heading-text) headings)))) ;; Collect (level . text)
    (nreverse headings))) ;; Reverse to preserve the order

(defun markdown-toc-cleanup-links (text)
  "Remove markdown links from TEXT, leaving only the link text."
  (replace-regexp-in-string
   "\\[\\([^]]+\\)\\](.*)"
   "\\1"
   text))

(defun markdown-toc-prettify-headings (headings)
  "Prettify the collected raw HEADINGS for insertion into the TOC buffer.
Each heading is represented as a cons cell of the form (LEVEL . TEXT),
where LEVEL is the heading level and TEXT is the prettified heading text
with bullet points replacing hashes and any extra whitespace removed.
Also removes markdown links from the heading text."
  (mapcar (lambda (heading)
            (let* ((level (car heading)) ;; Heading level
                   (text (concat "● " (string-trim-left (cdr heading))))) ;; Prettify text (replace hash with bullet)
              (setq text (markdown-toc-cleanup-links text)) ;; Remove links from text
              (cons level text))) ;; Return the prettified heading
          headings))

;; (defun markdown-toc-prettify-single-heading (headings)
;;   "Prettify the collected raw HEADINGS for insertion into the TOC buffer.
;; Each heading is represented as a cons cell of the form (LEVEL . TEXT),
;; where LEVEL is the heading level and TEXT is the prettified heading text
;; with bullet points replacing hashes and any extra whitespace removed.
;; Also removes markdown links from the heading text."
;;   (let* ((level (car heading)) ;; Heading level
;;          (text (concat "● " (string-trim-left (cdr heading))))) ;; Prettify text (replace hash with bullet)
;;     (setq text (markdown-toc-cleanup-links text)) ;; Remove links from text
;;     (cons level text)))

(defun markdown-toc-prettify-text-heading (heading)
  "Prettify the Each heading is where LEVEL is with bullet p
Also removes markdown links from the heading text."
  (let ((raw-text (cdr heading))) 
    (markdown-toc-cleanup-links raw-text)))

(defun markdown-toc-find-current-heading ()
  "Find the nearest heading at or before the current point in the buffer.
Returns a cons cell (POSITION . TEXT) where POSITION is the position of the heading and TEXT is the heading text."
  (save-excursion
    (let ((pos (point))) ;; Current position in buffer
      (catch 'found
        (goto-char pos)  ;; Start searching from the current position
        (while (re-search-backward "^\\(#+\\) \\(.*\\)" nil t)
          (let* ((heading-level (length (match-string 1)))    ;; Heading level
                 (heading-text (match-string 2))               ;; Heading text
                 (heading-pos (match-beginning 0)))             ;; Position of the heading
            (when (<= heading-pos pos) ;; Ensure the heading is before or at the current position
              (throw 'found (cons heading-pos heading-text))))))))) ;; Return cons cell (position . text)

(defun markdown-toc-insert-headings (headings)
  "Insert a list of HEADINGS into the current buffer.
Each heading is a cons cell (LEVEL . TEXT), where LEVEL is the heading level
and TEXT is the heading text."
  (insert "Table of Contents\n")
  (insert (make-string 40 ?-) "\n")
  (dolist (heading headings)
    (let ((level (car heading)) ;; Heading level
          (text (cdr heading))) ;; Heading text
      (insert (format "%s%s\n"
                      (make-string (* 2 (1- level)) ? ) ;; Indentation
                      text))
      ;; Apply the custom face to the inserted heading
      (put-text-property (line-beginning-position) (line-end-position)
                         'face 'markdown-toc-header-face)))) ;; Apply TOC header face

(defun markdown-toc-generate ()
  "Generate and display a table of contents for the current Markdown buffer.
The TOC is shown in a separate buffer."
  (let ((headings (markdown-toc-prettify-headings (markdown-toc-collect-headings))) ;; Collect headings first
        (toc-buffer (get-buffer-create markdown-toc-buffer-name))) ;; Create TOC buffer
    (with-current-buffer toc-buffer
;;       (setq buffer-read-only nil) ;; Allow modifications temporarily
      (erase-buffer) ;; Clear previous TOC content
      (markdown-toc-insert-headings headings) ;; Insert collected headings
;;       (setq buffer-read-only t) ;; Make buffer read-only after insertion
      ;; Enable visual line wrapping in the TOC buffer
      (visual-line-mode 1)) ;; Enable line wrapping
    ;; Display TOC buffer in a side window (1/3 width)
    (let ((window (display-buffer-in-side-window toc-buffer
                                                 '((side . right) (window-width . 0.33))))) ;; 1/3 width
      (set-window-dedicated-p window t)))) ;; Make TOC window dedicated

(defun markdown-toc-redraw (toc-buffer)
  "Generate and display a table of contents for the current Markdown buffer.
The TOC is shown in a separate buffer. If the TOC window is closed,
this function will recreate it, but only if the current buffer is a Markdown buffer."
    (let ((headings (markdown-toc-prettify-headings (markdown-toc-collect-headings)))) ;; Collect headings first
      (with-current-buffer toc-buffer
;;         (setq buffer-read-only nil) ;; Allow modifications temporarily
        (erase-buffer) ;; Clear previous TOC content
        (markdown-toc-insert-headings headings) ;; Insert collected headings
;;         (setq buffer-read-only t)
	)) ;; Make the buffer read-only
    ;; Ensure the TOC window is displayed correctly
    (let ((window (get-buffer-window toc-buffer)))
      (unless window
        (setq window (display-buffer-in-side-window toc-buffer
                                                    '((side . right) (window-width . 0.33)))))
      (set-window-dedicated-p window t))) ;; Make the TOC window dedicated

;; (defun markdown-toc-highlight-current-heading (current-heading toc-buffer)
;;   "Highlight the current heading in the TOC buffer based on the current position in the main buffer."
;;   (when (and current-heading (buffer-live-p toc-buffer)) ;; Ensure buffers are valid
;;     (with-current-buffer toc-buffer
;;       ;; Remove previous highlights
;;       (remove-text-properties (point-min) (point-max) '(face markdown-toc-current-heading-face))
;;       ;; Find the line corresponding to the current heading
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let ((heading-text (cdr current-heading))) ;; Only the text part, not the cons cell
;;           (when (search-forward heading-text nil t)
;;               (put-text-property (line-beginning-position) (line-end-position)
;;                                  'face 'markdown-toc-current-heading-face)))))))
;;             ;; If not found, log a message or skip

;; (defun markdown-toc-highlight-current-heading (current-heading toc-buffer)
;;   "Highlight the current heading in the TOC buffer based on the current position in the main buffer."
;;   (when current-heading
;;     (with-current-buffer toc-buffer
;;       ;; Remove previous highlights
;;       (remove-text-properties (point-min) (point-max) '(face markdown-toc-current-heading-face))
;;       ;; Find the line corresponding to the current heading
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let ((heading-text (cdr current-heading))) ;; Only the text part, not the cons cell
;;           (if (search-forward heading-text nil t)
;;               (put-text-property (line-beginning-position) (line-end-position)
;; 				 'face 'markdown-toc-current-heading-face)
;; 	    (progn
;; 	      (markdown-toc-redraw toc-buffer)
;; 	      (markdown-toc-highlight-current-heading current-heading toc-buffer))))))))

;; (defun markdown-toc-highlight-current-heading (current-heading toc-buffer)
;;   "Highlight the current heading in the TOC buffer based on the current position in the main buffer."
;;   (when (and current-heading (buffer-live-p toc-buffer)) ;; Ensure buffers are valid
;;     (with-current-buffer toc-buffer
;;       ;; Remove previous highlights
;;       (remove-text-properties (point-min) (point-max) '(face markdown-toc-current-heading-face))
;;       ;; Find the line corresponding to the current heading
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let ((heading-text (cdr current-heading))) ;; Only the text part, not the cons cell
;;           (when (search-forward heading-text nil t)
;;               (put-text-property (line-beginning-position) (line-end-position)
;;                                  'face 'markdown-toc-current-heading-face)))))))
;;             ;; If not found, log a message or skip

(defun markdown-toc-highlight-current-heading (current-heading toc-buffer &optional retries)
  "Highlight the current heading in the TOC buffer.
Optionally retry up to `markdown-toc-highlight-max-retries` times if the heading is not found."
  (when current-heading 
    (with-current-buffer toc-buffer
      ;; Remove previous highlights
      (remove-text-properties (point-min) (point-max) '(face markdown-toc-current-heading-face))
      ;; Find the line corresponding to the current heading
      (save-excursion
        (goto-char (point-min))
        (let ((heading-text (markdown-toc-prettify-text-heading current-heading)) ;; Only the text part, not the cons cell
              (retries (or retries 0)))
          (if (search-forward heading-text nil t)
              (put-text-property (line-beginning-position) (line-end-position)
                                 'face 'markdown-toc-current-heading-face)
            (when (< retries markdown-toc-highlight-max-retries)
                (progn
;;                   (markdown-toc-redraw toc-buffer)
                  (markdown-toc-highlight-current-heading current-heading toc-buffer (1+ retries))))))))))

;; (defun markdown-toc-highlight-current-heading (current-heading toc-buffer)
;;   "Highlight the current heading in the TOC buffer based on the current position in the main buffer."
;;   (when current-heading
;;     (with-current-buffer toc-buffer
;;       ;; Remove previous highlights
;;       (remove-text-properties (point-min) (point-max) '(face markdown-toc-current-heading-face))
;;       ;; Find the line corresponding to the current heading
;;       (save-excursion
;;         (goto-char (point-min))
;;         (let ((heading-text (cdr current-heading))) ;; Only the text part, not the cons cell
;;           (if (search-forward heading-text nil t)
;;               (put-text-property (line-beginning-position) (line-end-position)
;; 				 'face 'markdown-toc-current-heading-face)
;; 	    (progn
;; 	      (markdown-toc-redraw toc-buffer)
;; 	      (markdown-toc-highlight-current-heading current-heading toc-buffer))))))))

(defun markdown-toc-scroll-to-heading (current-heading toc-buffer)
  "Scroll the TOC buffer to keep the current heading centered."
  (when current-heading
    (let ((heading-text (markdown-toc-prettify-text-heading current-heading)) ;; Only the text part, not the cons cell
          (window (get-buffer-window toc-buffer t)))
      (when window
	(with-selected-window window
	  (with-current-buffer toc-buffer
            (goto-char (point-min)) ;; Start at the beginning of the buffer
            (when (search-forward heading-text nil t)
              ;; Find the window displaying the TOC buffer
              ;; Select the window and recenter it
              (recenter))))))))

(defun markdown-toc-update-on-idle ()
  "Update the TOC highlight and scroll on idle to reflect the current heading."
  (when (derived-mode-p 'markdown-mode) ;; Ensure we are in a Markdown buffer
    (let ((current-heading (markdown-toc-find-current-heading))
          (toc-buffer (get-buffer markdown-toc-buffer-name)))
      (markdown-toc-redraw toc-buffer)
      (markdown-toc-highlight-current-heading current-heading toc-buffer)
      (markdown-toc-scroll-to-heading current-heading toc-buffer))))

(defun markdown-toc-update-on-save ()
  "Update the TOC buffer when the Markdown file is saved."
  (when (derived-mode-p 'markdown-mode) ;; Ensure we are in a Markdown buffer
    (let ((current-heading (markdown-toc-find-current-heading))
          (toc-buffer (get-buffer markdown-toc-buffer-name)))
      (markdown-toc-redraw toc-buffer)
      (markdown-toc-highlight-current-heading current-heading toc-buffer)
      (markdown-toc-scroll-to-heading current-heading toc-buffer))))

(define-minor-mode markdown-toc-minor-mode
  "Minor mode for generating a table of contents buffer for Markdown files."
  :lighter " TOC"
  (if markdown-toc-minor-mode
      (progn
        ;; Update TOC on save
        (add-hook 'after-save-hook #'markdown-toc-update-on-save nil t)
        ;; Highlight current heading and scroll TOC on idle
	(setq markdown-toc-timer (run-with-idle-timer 1.5 t 'markdown-toc-update-on-idle))
	;; (run-with-idle-timer 0.5 t 'markdown-toc-update-on-idle)
        ;; Generate the initial TOC
        (markdown-toc-generate))
    ;; Clean up when disabling the mode
    (progn 
      (remove-hook 'after-save-hook #'markdown-toc-update-on-save t)
      (remove-function 'post-command-hook #'markdown-toc-update-on-idle)
      (cancel-timer markdown-toc-timer)
      (setq markdown-toc-timer nil)
      (when (get-buffer markdown-toc-buffer-name)
	(kill-buffer markdown-toc-buffer-name)))))

(provide 'markdown-toc)
