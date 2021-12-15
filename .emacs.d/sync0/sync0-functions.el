;; (defun sync0-print-pdf (pdf)
;;   "Print the pdf provided in the argument"
;;   (interactive)
;;   (let* ((copies (concat " -# " (read-string "Enter number of copies: ") " "))
;;          (page-size (completing-read "Choose page size: " '(" -o media=a4 " " -o media=letter " " -o media=legal ")))
;;          (layout (completing-read "Choose page layout: " '(" -o sides=one-sided " " -o sides=two-sided-long-edge " " -o sides=two-sided-short-edge ")))
;;          (quality (completing-read "Choose print quality: " '(" -o print-quality=3 " " -o print-quality=4 " " -o print-quality=5 ")))
;;          (command (concat "lpr" copies page-size layout quality pdf)))
;;     (shell-command command)))

;; (defun sync0-print-with-command (command pdf)
;;   "Print the pdf using the provided provided in the argument"
;;   (interactive)
;;   (let ((full-command (concat command pdf)))
;;     (shell-command full-command)))

;; (defun sync0-print-pdf (pdf &optional command)
;;   "Print the pdf provided in the argument"
;;   (interactive)
;;   (if command 
;;       (shell-command (concat command pdf))
;;     (let* ((copies (concat " -# " (read-string "Enter number of copies: ") " "))
;;            (page-size (completing-read "Choose page size: " '(" -o media=a4 " " -o media=letter " " -o media=legal ")))
;;            (layout (completing-read "Choose page layout: " '(" -o sides=one-sided " " -o sides=two-sided-long-edge " " -o sides=two-sided-short-edge ")))
;;            (quality (completing-read "Choose print quality: " '(" -o print-quality=3 " " -o print-quality=4 " " -o print-quality=5 ")))
;;            (print-command (concat "lpr" copies page-size layout quality pdf)))
;;       (shell-command print-command))))


;; (defun sync0-bibtex-print-single-pdf-attachment (&optional pdf)
;;  "Print the PDFs of the entries with the given KEYS where available."
;;   (interactive)
;;     (let* ((copies (concat " -# " (read-string "Enter number of copies: ") " "))
;;            (page-size (completing-read "Choose page size: " '(" -o media=a4 " " -o media=letter " " -o media=legal ")))
;;            (layout (completing-read "Choose page layout: " '(" -o sides=one-sided " " -o sides=two-sided-long-edge " " -o sides=two-sided-short-edge ")))
;;            (quality (completing-read "Choose print quality: " '(" -o print-quality=3 " " -o print-quality=4 " " -o print-quality=5 ")))
;;            (print-command (concat "lpr" copies page-size layout quality pdf)))


;; (defun sync0-bibtex-print-pdf-attachment (keys)
;;   "Print the PDFs of the entries with the given KEYS where available."
;;   (let* ((copies (concat " -# " (read-string "Enter number of copies: ") " "))
;;          (page-size (completing-read "Choose page size: " '(" -o media=a4 " " -o media=letter " " -o media=legal ")))
;;          (layout (completing-read "Choose page layout: " '(" -o sides=one-sided " " -o sides=two-sided-long-edge " " -o sides=two-sided-short-edge ")))
;;          (quality (completing-read "Choose print quality: " '(" -o print-quality=3 " " -o print-quality=4 " " -o print-quality=5 ")))
;;          (command (concat "lpr" copies page-size layout quality)))
;;   (dolist (key keys)
;;     (let ((pdf (bibtex-completion-find-pdf key bibtex-completion-find-additional-pdfs)))
;;       (if pdf
;;           (mapc 'mml-attach-file pdf)
;;         (message "No PDF(s) found for this entry: %s"
;;                  key)))))

(provide 'sync0-functions)


