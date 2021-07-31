

(add-to-list 'evil-emacs-state-modes 'mu4e-main-mode)
(add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
(evil-set-initial-state 'mu4e-compose-mode 'insert)

(setq mu4e-headers-fields
      '( (:date          .  25)    ;; alternatively, use :human-date
         (:flags         .   10)
         (:from          .  30)
         (:subject       .  nil))) ;; alternatively, use :thread-subject

;; Configure contexts
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Principal (carc.sync0)"
           :enter-func (lambda () (mu4e-message "Entering carc.sync0"))
           :leave-func (lambda () (mu4e-message "Leaving carc.sync0"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches
                            msg '(:from :to :cc :bcc) "carc.sync0@gmail.com")))
           :vars '(
                   (user-mail-address . "carc.sync0@gmail.com")
                   (mu4e-trash-folder . "/carc.sync0/[carc.sync0]/Trash")
                   (mu4e-refile-folder . "/carc.sync0/[carc.sync0]/All Mail")
                   (mu4e-sent-folder . "/carc.sync0/[carc.sync0]/Sent Mail")
                   (mu4e-drafts-folder . "/carc.sync0/[carc.sync0]/Drafts")
                   (mu4e-maildir-shortcuts . (("/carc.sync0/[carc.sync0]/Trash"       . ?t)
                                              ("/carc.sync0/[carc.sync0]/Sent Mail" . ?s)
                                              ("/carc.sync0/INBOX"            . ?i)
                                              ("/carc.sync0/[carc.sync0]/Drafts"    . ?d)
                                              ;; ("/carc.sync0/[carc.sync0]/Starred"   . ?r)
                                              ("/carc.sync0/[carc.sync0]/All Mail"  . ?a)))))

         ,(make-mu4e-context
           :name "Backup (cantorlunae)"
           :enter-func (lambda () (mu4e-message "Entering cantorlunae"))
           :leave-func (lambda () (mu4e-message "Leaving cantorlunae"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches
                            msg '(:from :to :cc :bcc) "cantorlunae@gmail.com")))
           :vars '(
                   (user-mail-address . "cantorlunae@gmail.com")
                   (mu4e-trash-folder . "/cantorlunae/[cantorlunae]/Trash")
                   (mu4e-refile-folder . "/cantorlunae/[cantorlunae]/All Mail")
                   (mu4e-sent-folder . "/cantorlunae/[cantorlunae]/Sent Mail")
                   (mu4e-drafts-folder . "/cantorlunae/[cantorlunae]/Drafts")
                   (mu4e-maildir-shortcuts . (("/cantorlunae/[cantorlunae]/Trash"       . ?t)
                                              ("/cantorlunae/[cantorlunae]/Sent Mail" . ?s)
                                              ("/cantorlunae/INBOX"            . ?i)
                                              ("/cantorlunae/[cantorlunae]/Drafts"    . ?d)
                                              ("/cantorlunae/[cantorlunae]/All Mail"  . ?a)))))))


;; Use imagemagick, if available.
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-bookmarks `(;; ("\\\\Inbox" "Inbox" ?i)
                       ("flag:flagged" "Flagged messages" ?f)
                       ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
                       ("date:today..now" "Today's messages" ?t)
                       ("date:7d..now" "Last 7 days" ?w)
                       ("mime:image/*" "Messages with images" ?p)
                       ("maildir:/cantorlunae/INBOX OR maildir:/carc.sync0/INBOX" "All inboxes" ?i)))


(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(evil-define-key 'normal mu4e-compose-mode-map
  "$" 'evil-end-of-visual-line
  "^" 'evil-beginning-of-visual-line
  "gg" 'mu4e-compose-goto-top
  "G" 'mu4e-compose-goto-bottom
  "]" 'evil-next-visual-line
  "[" 'evil-previous-visual-line)

;; Taken from https://github.com/abo-abo/hydra/wiki/mu4e
(defhydra sync0-hydra-mu4e-headers (:color blue :hint nil)
  "
  ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
 -^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
 _n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
 _p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
 _]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
 _[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups 
 _y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
 _R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------ 
 _C_: compose | _}_: next query    | _a_: action  | _|_: thru shl  | _`_: update, reindex
 _F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
              | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

  ;; general
  ("n" mu4e-headers-next)
  ("p" mu4e-headers-previous)
  ("[" mu4e-select-next-unread)
  ("]" mu4e-select-previous-unread)
  ("y" mu4e-select-other-view)
  ("R" mu4e-compose-reply)
  ("C" mu4e-compose-new)
  ("F" mu4e-compose-forward)
  ;;  ("o" my/org-capture-mu4e)                  ; differs from built-in

  ;; search
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("/" mu4e-headers-search-narrow)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ("{" mu4e-headers-query-prev)              ; differs from built-in
  ("}" mu4e-headers-query-next)              ; differs from built-in
  ("C-+" mu4e-headers-split-view-grow)
  ("C--" mu4e-headers-split-view-shrink)

  ;; mark stuff 
  ("!" mu4e-headers-mark-for-read)
  ("?" mu4e-headers-mark-for-unread)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("U" mu4e-mark-unmark-all)
  ("d" mu4e-headers-mark-for-trash)
  ("D" mu4e-headers-mark-for-delete)
  ("m" mu4e-headers-mark-for-move)
  ("a" mu4e-headers-action)                  ; not really a mark per-se
  ("A" mu4e-headers-mark-for-action)         ; differs from built-in
  ("*" mu4e-headers-mark-for-something)

  ("#" mu4e-mark-resolve-deferred-marks)
  ("%" mu4e-headers-mark-pattern)
  ("&" mu4e-headers-mark-custom)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)
  ("t" mu4e-headers-mark-subthread)
  ("T" mu4e-headers-mark-thread)

  ;; miscellany
  ("q" mu4e~headers-quit-buffer)
  ("H" mu4e-display-manual)
  ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

  ;; switches
  ("O" mu4e-headers-change-sorting)
  ("P" mu4e-headers-toggle-threading)
  ("Q" mu4e-headers-toggle-full-search)
  ("V" mu4e-headers-toggle-skip-duplicates)
  ("W" mu4e-headers-toggle-include-related)

  ;; more miscellany
  ("`" mu4e-update-mail-and-index)           ; differs from built-in
  (";" mu4e-context-switch)  
  ("j" mu4e~headers-jump-to-maildir)

  ("." nil))

;; we seem to need this to fix the org-store-link issue
;; (org-link-set-parameters "mu4e" :follow #'org-mu4e-open :store 
;; #'org-mu4e-store-link)

(provide 'sync0-mu4e)
