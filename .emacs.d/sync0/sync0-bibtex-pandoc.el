(require 'sync0-pandoc)

(defun sync0-pandoc-export-epub-to-pdf (&optional bibkey)
  (interactive)
  (let* ((refkey (if bibkey
                     bibkey
                   (sync0-bibtex-completion-choose-key t t)))
         (file (sync0-bibtex-choose-attachment refkey))
         (type (completing-read "Choose document type for export: " sync0-pandoc-export-epub-to-pdf-settings-alist))
         (lang (completing-read "Choose export language: " 
                                '("en-US" "en-GB" "pt-BR" "pt-PT" "de-DE" "fr-FR" "es-CO")))
         (type-settings (funcall
                         (cadr (assoc type sync0-pandoc-export-epub-to-pdf-settings-alist))))
         (raw-command (concat "pandoc" type-settings))
         (target-path (concat sync0-zettelkasten-attachments-directory refkey ".pdf"))
         (command (concat raw-command " " file " -o " target-path)))
    (if (and (file-exists-p file)
             (string= (file-name-extension file) "epub")) 
        (shell-command command)
      (message "PDF conversion for %s failed." refkey))))

;;      (image (concat sync0-bibtex-archive-directory refkey ".jpg"))
;;      ;; (extension (file-name-extension file))
;;      (command (concat "convert " image " -auto-orient " file)))
;; ;; (sync0-bibtex-completion-load-entry refkey)
;; (if (and (file-exists-p file)
;;          (file-exists-p image))
;;     (shell-command command)
;;   (message "Conversion for entry %s failed." refkey))))


(provide 'sync0-bibtex-pandoc)
