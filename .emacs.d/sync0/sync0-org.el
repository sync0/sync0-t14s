
;; (require 'org-pdftools)
(require 'org-journal)
(require 'org-download)
(require 'org-ref)
;; Free this keybinding for cycle-themes
(unbind-key "C-c C-t" org-mode-map)
(unbind-key "M-h" org-mode-map)

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
      (write-file "init.el"))
    (load initfile)))

;; taken from https://stackoverflow.com/questions/8881649/how-to-force-org-mode-to-open-a-link-in-another-frame
(defun sync0-org-open-other-frame ()
  "Jump to bookmark in another frame. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup (acons 'file 'find-file-other-window
                                     org-link-frame-setup)))
    (org-open-at-point)))

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

(defhydra sync0-hydra-org-functions (:color amaranth :hint nil :exit t)
  "
        ^Links^             ^Footnotes^          ^Trees^              ^Export^           ^Etc.^
        ^---------------------------------------------------------------------------------------------------
        Link _i_nsert       New _f_ootnote       Indirect _b_uffer    Latex _e_xport     Insert _d_rawer
        Link _s_tore        Footnote _a_ctions   Open _o_verview      Export _t_rees     New local _a_bbrev
        Last stored lin_k_  ^ ^                  Overview _j_ump      _T_angle init file
                                                                     
        _q_uit
             "
  ("s" org-store-link)
  ("i" org-insert-link)
  ("k" org-insert-last-stored-link)
  ("f" org-footnote-new)
  ("a" org-footnote-action)
  ("b" sync0-org-tree-to-indirect-buffer)
  ("j" sync0-overview-jump-to-overview)
  ("o" sync0-overview-tree-window)
  ("e" sync0-org-export-latex-and-beamer)
  ("t" sync0-org-export-headlines-to-latex)
  ("a" sync0-define-local-abbrev)
  ("d" org-insert-drawer)
  ("q" nil :color blue))

(evil-leader/set-key
  "O" 'org-open-at-point
  "T" 'sync0-org-tangle-initfile
  "#" 'sync0-org-open-other-frame)
;; "O" 'sync0-overview-tree-window
;; "o" 'sync0-overview-jump-to-overview
;; "I" 'org-insert-link
;; "z" 'sync0-org-tree-to-indirect-buffer
;; "z" 'sync0-hydra-org-functions/body

(evil-leader/set-key-for-mode 'org-mode "z" 'sync0-hydra-org-functions/body)

(defhydra sync0-hydra-file-access (:color amaranth :hint nil :exit t)
  "
              ^Windows^                ^Buffers^             ^Search^
           ^^^^^^---------------------------------------------------------------
           _1_: Delete others       _w_: Write           _r_: Recent
           _2_: Split horizontally  _a_: Write as        _f_: Find
           _3_: Split vertically    _b_: Open           
           ^ ^                      _k_: Kill
           ^ ^                      
           ^^^^^^---------------------------------------------------------------
              ^Bookmarks^           ^Planning^ 
           ^^^^^^---------------------------------------------------------------
           _j_: Jump to bookmark    _h_: Today
           _g_: Bookmark o. window  
           _m_: Set bookmark        
           _l_: List bookmarks      _J_: org-journal
           ^ ^                      
           [q] Quit                 ^ ^
           "
  ("1" delete-other-windows)
  ("2" sync0-split-and-follow-horizontally)
  ("3" sync0-split-and-follow-vertically)
  ("b" ivy-switch-buffer)
  ;; Quickly save
  ("w" save-buffer)
  ("a" write-file)
  ;; Kill current buffer and window
  ("k" kill-buffer-and-window)
  ;; ("o" ivy-switch-buffer-other-window)
  ("r" counsel-recentf)
  ("f" counsel-find-file)
  ("m" bookmark-set)
  ("j" counsel-bookmark)
  ("g" bookmark-jump-other-window)
  ("l" bookmark-bmenu-list)
  ;; ("A" org-agenda)
  ("J" sync0-org-journal-new-scheduled-entry)
  ("h" sync0-pop-to-org-agenda)
  ("q" nil :color blue))

;; font lock keywords 
;; org footnotes should look like real footnotes
(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords 'org-mode
                        '(("\\(\\[fn:\\)[[:digit:]]+\\]" 1 '(face nil display ""))))
(font-lock-add-keywords 'org-mode
                        '(("\\[fn:[[:digit:]]+\\(\\]\\)" 1 '(face nil display ""))))

(require 'cl-lib)

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

;; redefinition evils normal mode map
(evil-define-key 'normal org-mode-map
  "<" 'outline-previous-visible-heading
  ">" 'outline-next-visible-heading
  (kbd "C->") 'org-forward-heading-same-level
  (kbd "C-<") 'org-backward-heading-same-level
  (kbd "<S-tab>") 'sync0-org-tree-open-in-right-frame 
  "H" 'org-metaleft
  "L" 'org-metaright
  "K" 'org-metaup
  "J" 'org-metadown
  "k" 'previous-line
  "j" 'next-line
  "o" '(lambda () (interactive) (sync0-evil-org-eol-call 'sync0-clever-insert-item))
  "O" '(lambda () (interactive) (sync0-evil-org-eol-call 'org-insert-heading))
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "[" 'backward-sentence
  "]" 'forward-sentence
  "{" 'org-backward-paragraph
  "}" 'org-forward-paragraph
  "-" 'org-cycle-list-bullet
  (kbd "<tab>") 'org-cycle)

(evil-define-key 'visual org-mode-map
  ;; "q" 'highlight-changes-remove-highlight
  "z" 'org-emphasize)

;; List of files considered for org-refile.
(setq org-refile-targets (quote ((nil :maxlevel . 4)                ;; Default value.
                                 ;; set for all agenda files
                                 ;; ("todo.org" :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 4))))

;; (org-refile-targets '((org-agenda-files :maxlevel . 4)))

(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ;; ("\\.epub\\'" . emacs)
        ("\\.pdf\\'" . emacs)))

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (setq org-src-block-faces    '(("emacs-lisp" (:family "Fira Code"  :height 0.75))
;;                                ("python" (:family "Fira Code"  :height 0.75))
;;                                ("latex" (:family "Fira Code"  :height 0.75))))

(provide 'sync0-org)
