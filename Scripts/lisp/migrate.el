;;; The Org-roam v2 migration script
;;;
;;; Originally written by @jethrokuan,
;;; with inputs from @d12frosted
;;;
;;; CAUTION: this script converts all your Org-roam notes from v1 to v2 in-place.
;;; PLEASE BACKUP YOUR FILES BEFORE ATTEMPTING THIS.
;;;
;;; To execute this script, copy the entirety of this file into an empty buffer and execute:
;;;
;;; M-x evaluate-buffer
;;; 
(defun org-roam-v1-to-v2 ()
  ;; Create file level ID
  (org-with-point-at 1
    (org-id-get-create))
  ;; Replace roam_key into properties drawer roam_ref
  (when-let* ((refs (mapcan #'split-string-and-unquote
                            (cdar (org-collect-keywords '("roam_key"))))))
    (let ((case-fold-search t))
      (org-with-point-at 1
        (org-set-property "ROAM_REFS" (combine-and-quote-strings refs))
        (while (re-search-forward "^#\\+roam_key:" (point-max) t)
          (kill-line 1)))))

  ;; Replace roam_alias into properties drawer roam_aliases
  (when-let* ((aliases (mapcan #'split-string-and-unquote
                               (cdar (org-collect-keywords '("roam_alias"))))))    
    (let ((case-fold-search t))
      (org-with-point-at 1
        (org-set-property "ROAM_ALIASES" (combine-and-quote-strings aliases))
        (while (re-search-forward "^#\\+roam_alias:" (point-max) t)
          (kill-line 1)))))

  ;; Replace #+roam_tags into #+filetags
  (org-with-point-at 1
    (let ((case-fold-search t))
      (while (search-forward "#+roam_tags:" nil t)
        (replace-match "#+filetags:" nil t))))
  (save-buffer))

;; Step 1: Convert all v1 files to v2 files
(dolist (f (org-roam--list-all-files))
  (with-current-buffer (find-file-noselect f)
    (org-roam-v1-to-v2)))

;; Step 2: Build cache
(org-roam-db-sync 'force)

;; Step 3: Replace all file links with id links where possible
(defun org-roam-replace-file-links-with-id ()
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (let* ((mdata (match-data))
             (path (match-string 1))
             (desc (match-string 2)))
        (when (string-prefix-p "file:" path)
          (setq path (expand-file-name (substring path 5)))
          (when-let ((node-id (caar (org-roam-db-query [:select [id] :from nodes
                                                        :where (= file $s1)
                                                        :and (= level 0)] path))))
            (set-match-data mdata)
            (replace-match (org-link-make-string (concat "id:" node-id) desc))))))))

(dolist (f (org-roam--list-all-files))
  (with-current-buffer (find-file-noselect f)
    (org-roam-replace-file-links-with-id)))
