(use-package org 
  :custom
  (tab-width 8) 
  (org-hide-leading-stars t)
  ;; Leave one line between headlines 
  (org-cycle-separator-lines 0)
  ;; (org-cycle-separator-lines 2)
  ;; Don't fontify the whole damn line
  (org-fontify-whole-block-delimiter-line nil)
  ;; Disable word wrap in org mode.
  ;; (org-startup-truncated t)
  ;; Initial indentation
  (org-startup-indented t)         
  ;; Necessary to avoid crazy inconsistenscies using org-download and org-roam
  (org-link-file-path-type 'absolute)
  ;; Begin displaying entire trees.
  (org-startup-folded nil)
  ;; Better display of italics & bold.
  (org-hide-emphasis-markers t)
  ;; Define org-tags.
  (org-tag-alist '(
		   ;; 		   ("urgent" . ?r)
                   ("current" . ?c)
                   ("next" . ?n)
                   ("noexport" . ?x)
                   ("skim" . ?s)
                   ("exegesis" . ?e)
                   ("waiting" . ?w)
                   ;; ("postponed" . ?p)
                   ("unnumbered" . ?u)
                   ("revise" . ?r)
                   ("someday" . ?a)
                   ("fetch" . ?f)
		   ;;                    ("@office" . ?o)
                   ("@home" . ?h)
                   ("@deepwork" . ?p)
                   ("transcribe" . ?t)
                   ("ignore" . ?i)
                   ("delegated" . ?d)))
  ;; Hide inherited tags from Org's agenda view.
  ;; org-agenda-show-inherited-tags nil
  ;; Define todo keywords.
  ;; (org-blank-before-new-entry '((heading . nil)(plain-list-item . nil)))
  ;; Stop emacs asking for confirmation
  (org-confirm-babel-evaluate nil)
  (org-ellipsis "  ⌄ ") ;; folding symbol
  ;; Do not show export buffer.
  (org-export-show-temporary-export-buffer nil)
  ;; Set path for org default dir (necessary for refile and agenda).
  ;;   (org-directory (concat (getenv "HOME") "/Gdrive/org"))
  (org-directory (concat sync0-zkn-dir "tasks/"))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-startup-with-inline-images t)
  (org-refile-use-cache nil)
  ;; Have org-mode indent elisp sections.
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  ;; Color embeded source code
  (org-src-fontify-natively t)
  (org-fontify-done-headline t) 
  (org-fontify-whole-heading-line t)
  (org-fontify-quote-and-verse-blocks t)
  ;; Don't fontify sub and superscripts.
  (org-pretty-entities-include-sub-superscripts nil)
  ;; Limit inheritance for certain tags. 
  (org-tags-exclude-from-inheritance (quote ("crypt" "ignore" "next" "current" "waiting" "someday" "delegated" "urgent")))
  (org-log-done 'time)
  (org-todo-keywords '((sequence "未(1)" "中(2)" "延(3)" "|"  "完(4)" "消(5)")))
  :config 
  ;; This is necessary to avoid conflict with my motion bindings. 
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  ;; Free this keybinding for cycle-themes
  (unbind-key "C-c C-t" org-mode-map)
  (unbind-key "M-h" org-mode-map)

  ;; Predicate for org-mode titles in org files. 
  (defun org-keyword-title-p ()
    "Check whether current buffer is an org-mode file with a non-null
  TITLE keyword."
    (and (equal major-mode 'org-mode)
	 (cadar (org-collect-keywords '("TITLE")))))

(defcustom sync0-org-bullet-replacement t
  "Replace hyphens in Org-mode lists with bullets visually."
  :type 'boolean
  :group 'sync0-org-settings)

(defun sync0-org-replace-hyphen-with-bullet ()
  "Visually replace hyphens in Org lists with bullets."
  (when sync0-org-bullet-replacement
    (font-lock-add-keywords
     'org-mode
     '(("^ *\\([-]\\) " (0 (prog1 ()
                             (compose-region (match-beginning 1)
                                             (match-end 1)
                                             "•"))))))))

(add-hook 'org-mode-hook #'sync0-org-replace-hyphen-with-bullet)

  (defun sync0-org-tangle-initfile ()
    (interactive)
    "Tangle and then load the emacs init file."
    (let* ((orgfile
            (expand-file-name 
             (concat user-emacs-directory "sync0_emacs.org")))
           (initfile
            (expand-file-name 
             (concat user-emacs-directory "init.el")))
           (tangled-file (car (org-babel-tangle-file orgfile))))
      (with-temp-buffer 
	(insert-file-contents tangled-file)
	(write-file initfile))))

  ;; taken from https://stackoverflow.com/questions/8881649/how-to-force-org-mode-to-open-a-link-in-another-frame
  (defun sync0-org-open-other-frame ()
    "Jump to bookmark in another frame. See `bookmark-jump' for more."
    (interactive)
    (let ((org-link-frame-setup (acons 'file 'find-file-other-window
                                       org-link-frame-setup)))
      (org-open-at-point)))

  (defun sync0-org-format-export-keyword ()
    "Open corresponding pdf file for current org file."
    (interactive)
    (let* ((file-path (buffer-file-name))
           (file-name 
            (progn
              (string-match "\\([[:digit:]]+\\)\\.org$"   file-path)
              (match-string 1 file-path))))
      (concat sync0-zkn-attachments-dir file-name ".pdf")))

  (defun sync0-overview-tree-window ()
    "Open a clone of the current buffer to the left, resize it to 30 columns, and bind <mouse-1> to jump to the same position in the base buffer."
    (interactive)
    (let ((new-buffer-name (concat "<tree>" (buffer-name))))
      ;; Create tree buffer
      (split-window-right 30)
      (if (get-buffer new-buffer-name)
          (switch-to-buffer new-buffer-name)  ; Use existing tree buffer
	;; Make new tree buffer
	(progn  (clone-indirect-buffer new-buffer-name nil t)
		(switch-to-buffer new-buffer-name)
		(read-only-mode)
		(hide-body)
		(toggle-truncate-lines)

		;; Do this twice in case the point is in a hidden line
		(dotimes (_ 2 (forward-line 0)))

		;; Map keys
		(use-local-map (copy-keymap outline-mode-map))
		(local-set-key (kbd "q") 'delete-window)
		(mapc (lambda (key) (local-set-key (kbd key) 'my/jump-to-point-and-show))
                      '("<mouse-1>" "RET"))))))

  (defun sync0-overview-jump-to-overview ()
    "Switch to a cloned buffer's base buffer and move point to the cursor position in the clone."
    (interactive)
    (let ((buf (buffer-base-buffer)))
      (unless buf
	(error "You need to be in a cloned buffer!"))
      (let ((pos (point))
            (win (car (get-buffer-window-list buf))))
	(if win
            (select-window win)
          (other-window 1)
          (switch-to-buffer buf))
	(goto-char pos)
	(when (invisible-p (point))
          (show-branches)))))

  (defun sync0-org-tree-to-indirect-buffer ()
    "Open headline in the next window as a separate tree."
    (interactive)
    (org-tree-to-indirect-buffer)
    (windmove-right))

  (add-to-list 'font-lock-extra-managed-props 'display)

  (font-lock-add-keywords 'org-mode
                          '(("\\(\\[fn:\\)[[:digit:]]+\\]" 1 '(face nil display ""))))

  (font-lock-add-keywords 'org-mode
                          '(("\\[fn:[[:digit:]]+\\(\\]\\)" 1 '(face nil display ""))))

  ;; Taken from https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer
  (defvar sync0-punctuation-marks '(","
                                    "."
                                    "'"
                                    "&"
                                    "\"")
    "List of Punctuation Marks that you want to count.")

  (defun sync0-count-raw-word-list (raw-word-list)
    (cl-loop with result = nil
             for elt in raw-word-list
             do (cl-incf (cdr (or (assoc elt result)
                                  (first (push (cons elt 0) result)))))
             finally return (sort result
                                  (lambda (a b) (string< (car a) (car b))))))

  (defun sync0-word-stats ()
    (interactive)
    (let* ((words (split-string
                   (downcase (buffer-string))
                   (format "[ %s\f\t\n\r\v]+"
                           (mapconcat #'identity sync0-punctuation-marks ""))
                   t))
           (punctuation-marks (cl-remove-if-not
                               (lambda (elt) (member elt sync0-punctuation-marks))
                               (split-string (buffer-string) "" t )))
           (raw-word-list (append punctuation-marks words))
           (word-list (sync0-count-raw-word-list raw-word-list)))
      (with-current-buffer (get-buffer-create "*word-statistics*")
	(erase-buffer)
	(insert "| word | occurences |
                    |-----------+------------|\n")

	(dolist (elt word-list)
          (insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))

	(org-mode)
	(indent-region (point-min) (point-max))
	(goto-char 100)
	(org-cycle)
	(goto-char 79)
	(org-table-sort-lines nil ?N)))
    (pop-to-buffer "*word-statistics*"))

  (defun sync0-call-rebinding-org-blank-behaviour (fn)
    (let ((org-blank-before-new-entry
           (copy-tree org-blank-before-new-entry)))
      (when (org-at-heading-p)
	(rplacd (assoc 'heading org-blank-before-new-entry) nil))
      (call-interactively fn)))

  (defun sync0-org-meta-return-dwim ()
    "Improved version of default org-meta-return"
    (interactive)
    (sync0-call-rebinding-org-blank-behaviour 'org-meta-return))

  (defun sync0-org-insert-todo-heading-dwim ()
    "Improved version of org-insert-todo-heading"
    (interactive)
    (sync0-call-rebinding-org-blank-behaviour 'org-insert-todo-heading))

  (defun sync0-clever-insert-item ()
    "Clever insertion of org item."
    (if (not (org-in-item-p))
	(insert "\n")
      (org-insert-item)))

  (defun sync0-evil-org-eol-call (fun)
    "Go to end of line and call provided function. FUN function callback"
    (end-of-line)
    (funcall fun)
    (evil-append nil))

  (defun sync0-org-forward-and-preview ()
    "Go to same level next heading and show preview in dedicated buffer"
    (interactive)
    (hide-subtree)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer))

  (defun sync0-org-back-and-preview ()
    "Go to same level previous heading and show preview in dedicated buffer"
    (interactive)
    (hide-subtree)
    (org-speed-move-safe (quote outline-previous-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer))

  (defun sync0-org-up-back-and-preview ()
    "Go to previous level heading and show preview in dedicated buffer"
    (interactive)
    (org-speed-move-safe (quote outline-up-heading))
    (org-tree-to-indirect-buffer)
    (hide-subtree))

  (defun sync0-org-up-forward-and-preview ()
    "Go to previous level next heading and show preview in dedicated buffer"
    (interactive)
    (org-speed-move-safe (quote outline-up-heading))
    (hide-subtree)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (org-tree-to-indirect-buffer))

  (defun sync0-org-inside-and-preview ()
    "Go to next level heading and show preview in dedicated buffer"
    (interactive)
    (org-speed-move-safe (quote outline-next-visible-heading))
    (show-children)
    (org-tree-to-indirect-buffer))

;;   ;; List of files considered for org-refile.
;;   (setq org-refile-targets (quote ((nil :maxlevel . 2)                ;; Default value.
;;                                    ;; set for all agenda files
;;                                    ;; ("todo.org" :maxlevel . 2)
;;                                    (org-agenda-files :maxlevel . 2))))

(defun sync0-org-refile-candidates-from-current-directory ()
  "Return a list of .org files in the current buffer's directory for refile targets."
  (when buffer-file-name
    (directory-files (file-name-directory buffer-file-name) t ".*\\.org$")))

(defun sync0-org-set-local-refile-targets ()
  "Set `org-refile-targets` to files in the current buffer's directory."
  (setq-local org-refile-targets
              `((,(sync0-org-refile-candidates-from-current-directory) :maxlevel . 2))))

;; Hook to adjust `org-refile-targets` dynamically
(add-hook 'org-mode-hook #'sync0-org-set-local-refile-targets)


  (setq org-goto-interface 'outline)

  (setq org-file-apps
	'((auto-mode . emacs)
          (directory . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ;; ("\\.epub\\'" . emacs)
          ("\\.jpeg\\'" . "gwenview %s")
          ("\\.jpg\\'" . "gwenview %s")
          ("\\.png\\'" . "gwenview %s")
          ("\\.odt\\'" . "libreoffice --writer %s")
          ("\\.rtf\\'" . "libreoffice --writer %s")
          ("\\.doc\\'" . "libreoffice --writer %s")
          ("\\.docx\\'" . "libreoffice --writer %s")
          ("\\.ppt\\'" . "libreoffice --impress %s")
          ("\\.pptx\\'" . "libreoffice --impress %s")
          ("\\.pdf\\'" . emacs)))

  (require 'org-num)
  (require 'oc)
  (require 'org-tempo)
  (require 'sync0-pandoc)
  )

(provide 'sync0-org)
