(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-corrections)
(require 'sync0-zettelkasten)
(require 'sync0-zkn)
(require 'sync0-zkn-functions)
(require 'sync0-bibtex-url)

;; (defun obsidian-follow-markdown-link-at-point (&optional arg)
;;   "Find and follow markdown link at point.
;; Opens markdown links in other window if ARG is non-nil.."
;;   (interactive "P")
;;   (let ((normalized (s-replace "%20" " " (markdown-link-url))))
;;     (if (s-contains-p ":" normalized)
;;         (browse-url normalized)
;;       (-> normalized
;;           obsidian-prepare-file-path
;;           (obsidian-find-file arg)))))

;; (defun sync0-obsidian-jump ()
;;   "Jump to Obsidian note."
;;   (interactive)
;;   (let* ((files (obsidian-list-all-files))  ; Step 1: Get the list of all files
;;          (dict (make-hash-table :test 'equal))  ; Step 2: Initialize hash table
;;          (_ (-map (lambda (f)
;;                     (puthash (file-relative-name f obsidian-directory) f dict)) files))
;;          ;; Step 3: Cache all aliases and file names to speed up repeated access
;;          (cached-aliases (or (gethash 'all-aliases sync0-obsidian-jump-cache) 
;;                              (let ((all-aliases (obsidian--all-aliases)))
;;                                (puthash 'all-aliases all-aliases sync0-obsidian-jump-cache)
;;                                all-aliases)))
;;          (candidates (-sort #'string< (-distinct (-concat cached-aliases
;; 							  (hash-table-keys dict)))))
;; 	 (choice (completing-read "Jump to: " candidates))
;; 	 (target (obsidian--get-alias choice (gethash choice dict))))
;;     (if target
;;         (find-file target)  ; Open the file if found
;;       (user-error "Note not found: %s" choice))))

;; (evil-leader/set-key "S" 'sync0-search-obsidian-notes)
;; (evil-leader/set-key "J" 'sync0-zkn-find-file)

;; (global-set-key (kbd "C-c h") 'my-hydra/body)

  ;; Step 3: Add hooks to load/save cache on startup and exit
  ;; (add-hook 'emacs-startup-hook 'sync0-obsidian-load-cache)
  ;; (add-hook 'kill-emacs-hook 'sync0-obsidian-save-cache)

(provide 'sync0-obsidian)
