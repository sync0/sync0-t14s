(defun sync0-obsidian-create-org-id ()
  "Create id links for target note for compatibility with org-roam."
  (interactive)
  (let* ((id (org-id-new))
         (id-line (concat "id: " id "\n")))
    (goto-char (point-min))
    (unless (re-search-forward "^id: " nil t 1) 
    (goto-char (point-min))
    (forward-line)
    (insert id-line))))

(dolist (f (f-files sync0-obsidian-directory
         (lambda (k) (string-match-p ".+\\.md" k)) t))
  (with-current-buffer (find-file-noselect f)
    (sync0-obsidian-create-org-id)))

(defun sync0-obsidian-replace-org-id ()
  "Create id links for target note for compatibility with org-roam."
  (interactive)
  (when-let* ((buffer (buffer-file-name))
              (key (when (string-match "\\([0-9]+\\).md$" buffer)
                     (match-string-no-properties 1 buffer)))
              (id-line (concat "id: " key "\n")))
    (goto-char (point-min))
    (when (re-search-forward "^id: " nil t 1) 
    (kill-whole-line)
    (insert id-line))))

(dolist (f (f-files sync0-obsidian-directory
         (lambda (k) (string-match-p ".+\\.md" k)) t))
  (with-current-buffer (find-file-noselect f)
    (sync0-obsidian-replace-org-id)))
