(require 'sync0-bibtex-vars)
(require 'sync0-bibtex-key-functions)
(require 'sync0-bibtex-corrections)
(require 'sync0-zettelkasten)
(require 'sync0-zkn)
(require 'sync0-zkn-functions)
(require 'sync0-bibtex-url)

(defun obsidian-follow-markdown-link-at-point (&optional arg)
  "Find and follow markdown link at point.
Opens markdown links in other window if ARG is non-nil.."
  (interactive "P")
  (let ((normalized (s-replace "%20" " " (markdown-link-url))))
    (if (s-contains-p ":" normalized)
        (browse-url normalized)
      (-> normalized
          obsidian-prepare-file-path
          (obsidian-find-file arg)))))

(defun sync0-obsidian-jump ()
  "Jump to Obsidian note."
  (interactive)
  (let* ((files (obsidian-list-all-files))  ; Step 1: Get the list of all files
         (dict (make-hash-table :test 'equal))  ; Step 2: Initialize hash table
         (_ (-map (lambda (f)
                    (puthash (file-relative-name f obsidian-directory) f dict)) files))
         ;; Step 3: Cache all aliases and file names to speed up repeated access
         (cached-aliases (or (gethash 'all-aliases sync0-obsidian-jump-cache) 
                             (let ((all-aliases (obsidian--all-aliases)))
                               (puthash 'all-aliases all-aliases sync0-obsidian-jump-cache)
                               all-aliases)))
         (candidates (-sort #'string< (-distinct (-concat cached-aliases
							  (hash-table-keys dict)))))
	 (choice (completing-read "Jump to: " candidates))
	 (target (obsidian--get-alias choice (gethash choice dict))))
    (if target
        (find-file target)  ; Open the file if found
      (user-error "Note not found: %s" choice))))

(evil-leader/set-key "S" 'sync0-search-obsidian-notes)
(evil-leader/set-key "J" 'sync0-zkn-find-file)

(defhydra sync0-hydra-obsidian-functions (:color amaranth :hint nil :exit t)
  "
^Zettel Functions^               ^Other Obsidian Functions^
^^^----------------------------   ^^^-----------------------------
_z_ettel quick                    _o_pen at point
_s_tructure                       _j_ump to note
_f_iche                           _t_ag find
_k_eyword                         _x_ search by expr
_p_eople fiche                    _u_pdate tags/aliases
multinote                       _d_raft create
_S_earch in catalogs              _i_nsert (mdlink)
^^^                               _r_efactor notes
^^^                               _m_ove file (in vault)
^^^                               _q_uit
"
  ("z" sync0-obsidian-create-quick-zettel)
  ("s" sync0-obsidian-create-structure-zettel)
  ("f" sync0-obsidian-create-fiche-zettel)
  ("k" sync0-obsidian-create-keyword-zettel)
  ("p" sync0-obsidian-create-multiple-people-fiche)
  ("m" sync0-zkn-move-file)
  ;; ("m" sync0-obsidian-create-multiple-notes)
  ("S" sync0-search-in-catalogs)
  ("o" sync0-zkn-follow-link-at-point)
  ("j" sync0-zkn-find-file)
  ("t" obsidian-tag-find)
  ("x" obsidian-search)
  ("u" obsidian-update)
  ("d" sync0-obsidian-new-draft)
  ("i" sync0-zkn-insert-md-link :color blue)
  ("r" sync0-zkn-note-refactor :color blue)
  ;; ("c" obsidian-capture)
  ("q" nil :color blue))

(evil-leader/set-key-for-mode 'markdown-mode "O" 'sync0-zkn-follow-link-at-point)

(evil-leader/set-key
  "c" 'sync0-hydra-obsidian-functions/body)

  ;; Step 3: Add hooks to load/save cache on startup and exit
  (add-hook 'emacs-startup-hook 'sync0-obsidian-load-cache)
  (add-hook 'kill-emacs-hook 'sync0-obsidian-save-cache)

(provide 'sync0-obsidian)
