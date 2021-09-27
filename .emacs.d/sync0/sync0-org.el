(setq org-todo-keywords '((sequence "未(1)" "|" "完(2)" "消(3)")))

;; (setq org-todo-keywords '((sequence "未(1)" "來(2)" "中(3)" "見(4)" "待(5)" "推(6)" "|" "完(7)" "消(8)")
;;                           (sequence "失" "買" "|" "有")
;;                           (sequence "確"  "讀" "解" "|" "無")))

;; First sequence:
;; "未(1)" Undone
;; "來(2)" Next (like :next:)
;; "中(3)" In process
;; "見(4)" Review or look again at
;; "待(5)" Wait for something else 
;; "推(6)" On hold until further advice (like :maybe: or :someday:)
;; "|"
;; "完(7)" Done
;; "消(8)" Cancelled

;; Second sequence:
;; "失(l)" Unobtained or lost
;; "買(b)" Buy or procure
;; "|"
;; "有(h)" Obtained

;; Third sequence:
;; "確(r)" Review or skim
;; "讀(c)" Close read
;; "解(t)" Transfer notes to Zettelkasten
;; "|"
;; "無(i)" Ignore

;; Set faces for org-todo-keywords
(setq org-todo-keyword-faces '(("未" . (:foreground "#dc322f" :weight semi-bold :height 0.9))
                               ("失" . (:foreground "#dc322f" :weight semi-bold :height 0.9))
                               ("確" . (:foreground "#dc322f" :weight semi-bold :height 0.9))
                               ("買" . (:foreground "#d33682" :weight semi-bold :height 0.9))
                               ("來" . (:foreground "#d33682" :weight semi-bold :height 0.9))
                               ("完" . (:foreground "#859900" :weight semi-bold :height 0.9))   
                               ("有" . (:foreground "#859900" :weight semi-bold :height 0.9))   
                               ("解" . (:foreground "#268bd2" :weight semi-bold :height 0.9))
                               ("待" . (:foreground "#268bd2" :weight semi-bold :height 0.9))
                               ("無" . (:foreground "#6c71c4" :weight semi-bold :height 0.9)) 
                               ("消" . (:foreground "#6c71c4" :weight semi-bold :height 0.9)) 
                               ("確" . (:foreground "#2aa198" :weight semi-bold :height 0.9)) 
                               ("讀" . (:foreground "#2aa198" :weight semi-bold :height 0.9)) 
                               ("見" . (:foreground "#2aa198" :weight semi-bold :height 0.9)) 
                               ("推" . (:foreground "#859900" :weight semi-bold :height 0.9)) 
                               ("中" . (:foreground "#b58900" :weight semi-bold :height 0.9))))


;; (require 'org-pdftools)
(require 'org-journal)
(require 'org-download)
(require 'org-ref)
(require 'cl-lib)


;; Free this keybinding for cycle-themes
(unbind-key "C-c C-t" org-mode-map)
(unbind-key "M-h" org-mode-map)


;; Predicate for org-mode titles in org files. 

(defun org-keyword-title-p ()
  "Check whether current buffer is an org-mode file with a non-null
  TITLE keyword."
  (and (equal major-mode 'org-mode)
       (cadar (org-collect-keywords '("TITLE")))))

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
    (concat sync0-exported-pdfs-directory file-name ".pdf")))

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

(defun sync0-org-open-corresponding-pdf ()
  "Open corresponding pdf file for current org file."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (file-name 
          (progn
            (string-match "\\([[:digit:]]+\\)\\.org$"   file-path)
            (match-string 1 file-path)))
         (pdf-path (concat sync0-exported-pdfs-directory file-name ".pdf")))
    (if (file-exists-p pdf-path)
        (org-open-file pdf-path)
      (message "No PDF found for %s.org" file-name))))

(defhydra sync0-hydra-org-functions (:color amaranth :hint nil :exit t)
"
^Links^             ^Footnotes^          ^Trees^              ^Export^           ^Etc.^
^---------------------------------------------------------------------------------------------------
Link _i_nsert       New _f_ootnote       _I_ndirect buffer    Latex _e_xport     Insert _d_rawer
Link _s_tore        Footnote _a_ctions   Open _o_verview      _E_xport trees     New local _a_bbrev
Last stored lin_k_  ^ ^                  Overview _j_ump      _V_isit corr. PDF  _T_angle init file
^ ^                 ^ ^                  Show sparse _t_ree   Export to epu_b_
                                                                     
_q_uit
"
  ("b" org-epub-export-to-epub)
  ("s" org-store-link)
  ("i" org-insert-link)
  ("k" org-insert-last-stored-link)
  ("f" org-footnote-new)
  ("a" org-footnote-action)
  ("I" sync0-org-tree-to-indirect-buffer)
  ("j" sync0-overview-jump-to-overview)
  ("o" sync0-overview-tree-window)
  ("e" sync0-org-export-latex-and-beamer)
  ("T" sync0-org-tangle-initfile)
  ("t" org-sparse-tree)
  ("E" sync0-org-export-headlines-to-latex)
  ("a" sync0-define-local-abbrev)
  ("d" org-insert-drawer)
  ("V" sync0-org-open-corresponding-pdf)
  ("q" nil :color blue))


(evil-leader/set-key-for-mode 'org-mode "O" 'org-open-at-point)
(evil-leader/set-key-for-mode 'org-mode "#" 'sync0-org-open-other-frame)

;; (evil-leader/set-key
;;   "O" 'org-open-at-point
;;   "#" 'sync0-org-open-other-frame)

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
