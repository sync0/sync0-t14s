(require 'markdown-mode)

(defvar markdown-toc-buffer-name "*Markdown TOC*"
  "Name of the buffer used to display the table of contents.")

(defun markdown-toc-generate ()
  "Generate a table of contents for the current Markdown buffer."
  (let ((toc-buffer (get-buffer-create markdown-toc-buffer-name))
        (current-buffer (current-buffer)))
    (with-current-buffer toc-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Table of Contents\n")
      (insert (make-string 40 ?-) "\n")
      (with-current-buffer current-buffer
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\) \\(.*\\)" nil t)
          (let ((level (length (match-string 1)))
                (heading (match-string 2))
                (pos (match-beginning 0)))
            (insert (format "%s[[%s][%s]]\n"
                            (make-string (* 2 (1- level)) ? )
                            (buffer-name current-buffer)
                            heading))
            (add-text-properties
             (line-beginning-position) (line-end-position)
             `(marker ,(copy-marker pos) level ,level))))))
      (markdown-toc-mode)
      (setq buffer-read-only t))
    toc-buffer))

(defun markdown-toc-jump ()
  "Jump to the heading in the Markdown buffer."
  (interactive)
  (let ((marker (get-text-property (point) 'marker)))
    (when marker
      (switch-to-buffer-other-window (marker-buffer marker))
      (goto-char marker))))

(defun markdown-toc-update-on-save ()
  "Update the table of contents buffer if it exists after saving the file."
  (when (and (eq major-mode 'markdown-mode)
             (get-buffer markdown-toc-buffer-name))
    (markdown-toc-generate)))

(define-minor-mode markdown-toc-mode
  "A minor mode for interacting with the Markdown table of contents."
  :lighter " TOC"
  (if markdown-toc-mode
      (progn
        (setq-local header-line-format "TOC Mode: Click or navigate to jump to headings")
        (local-set-key (kbd "RET") #'markdown-toc-jump))
    (setq header-line-format nil)))

(define-minor-mode markdown-toc-minor-mode
  "Minor mode for generating a table of contents buffer for Markdown files."
  :lighter " TOC"
  (if markdown-toc-minor-mode
      (progn
        (add-hook 'after-save-hook #'markdown-toc-update-on-save nil t)
        (let ((toc-buffer (markdown-toc-generate)))
          (display-buffer-in-side-window toc-buffer '((side . right)))))
    (remove-hook 'after-save-hook #'markdown-toc-update-on-save t)
    (when (get-buffer markdown-toc-buffer-name)
      (kill-buffer markdown-toc-buffer-name))))

(provide 'markdown-toc)
