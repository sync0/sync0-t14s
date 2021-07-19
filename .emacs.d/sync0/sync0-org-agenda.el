;; Set icons for use in agenda views. 
(setq org-agenda-category-icon-alist `(
                                       ("[Tt][aâ]ches" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
                                       ("[Tt]asks" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]hores" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
                                       ("[Mh][ée]nage" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
                                       ("[Hh]abitudes" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                       ("[Hh]abits" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                       ("[Gg]cal" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                       ("[ÉEée]tudes" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[Hh]historiographie" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]lasses" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[ÉEée]v[eé]nements" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]vents" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]macs" ,(list (all-the-icons-material "code" :height 1.2)) nil nil :ascent center)
                                       ("[Ll]yon" ,(list (all-the-icons-material "pageview" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]ocialism" ,(list (all-the-icons-material "build" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]ocialisme" ,(list (all-the-icons-material "build" :height 1.2)) nil nil :ascent center)
                                       ("[Oo]utils" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                       ("[Tt]ools" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                       ("[Zz]ettelkasten" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                       ("[Tt]ravail" ,(list (all-the-icons-material "business_center" :height 1.2)) nil nil :ascent center)
                                       ("[Dd]octorat" ,(list (all-the-icons-material "school" :height 1.2)) nil nil :ascent center)
                                       ("CTW" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]efardi" ,(list (all-the-icons-material "timeline" :height 1.2)) nil nil :ascent center)
                                       ("[Dd]iary" ,(list (all-the-icons-material "today" :height 1.2)) nil nil :ascent center)
                                       ("[Mm]essages" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                       ("[Mm]essages ([[:graph:]]+)" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                       ("[Mm]\. ([[:graph:]]+)" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                       ("[EÉée]criture" ,(list (all-the-icons-material "create" :height 1.2)) nil nil :ascent center)
                                       ("[Ww]riting" ,(list (all-the-icons-material "create" :height 1.2)) nil nil :ascent center)
                                       ("[Pp]ortuguês" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ff]rançais" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]spañol" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]nglish" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Bb]log" ,(list (all-the-icons-material "speaker_notes" :height 1.2)) nil nil :ascent center)))

;; Set org-agenda files
(setq  org-agenda-files (list "~/Dropbox/org/todo/"
                              "~/Dropbox/org/etc/Gcal.org"
                              "~/Dropbox/org/etc/Events.org"
                              "~/Dropbox/org/etc/Classes.org"))

;; (setq org-agenda-files (list "~/Dropbox/org/todo/"))

;; (let ((my-agenda-files (list "~/Dropbox/org/etc/Gcal.org"
;;                              "~/Dropbox/org/etc/Events.org"
;;                              "~/Dropbox/org/etc/Classes.org")))
;;   (setq org-agenda-files (append org-agenda-files my-agenda-files)))

(setq org-agenda-custom-commands
      '(("h" "Agenda"
         ((agenda "" 
                  ((org-agenda-overriding-header " Agenda \n")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("完" "消")))
                   (org-agenda-span 'day)
                   (org-agenda-start-on-weekday nil)
                   ;; (org-agenda-skip-scheduled-delay-if-deadline t)
                   ;; (org-agenda-repeating-timestamp-show-all )
                   (org-agenda-start-day "+0d")
                   (org-deadline-warning-days 7)
                   ;; (org-agenda-current-time-string "⮜    ‧    ‧    maintenant")
                   (org-agenda-current-time-string " ")
                   (org-agenda-time-grid (quote ((daily today remove-match)
                                                 ;; the () means not to put those annoying time
                                                 ()
                                                 "     ⮜" "⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺")))
                   ;; "     ⮜" ""
                   (org-agenda-prefix-format "  %-22t  %-5s  %-3i  %-20c  ")))
          (tags-todo "TODO​=\"中\"|TODO​=\"來\"|+PRIORITY=\"A\""
                     ((org-agenda-overriding-header " Prochaines actions \n")
                      ;;; Do not show tasks that have been completed or cancelled. 
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("完" "消")))
                      ;; (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "取" "阻"))
                      ;;                                (org-agenda-skip-entry-if 'scheduled 'deadline)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
          (tags-todo "+this_week|DEADLINE>=\"<+2d>\"&DEADLINE<=\"<+7d>\"|SCHEDULED>=\"<+2d>\"&SCHEDULED<=\"<+7d>\""
                     ((org-agenda-overriding-header " Sept jours \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消"))
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
          (tags-todo "+next_week-this_week|DEADLINE>=\"<+8d>\"&DEADLINE<=\"<+14d>\"|SCHEDULED>=\"<+8d>\"&SCHEDULED<=\"<+14d>\""
                     ((org-agenda-overriding-header " Quatorze jours \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消"))
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
          (tags-todo "+this_month-this_week-next_week-ignore|DEADLINE>=\"<+15d>\"&DEADLINE<=\"<+29d>\"|SCHEDULED>=\"<+15d>\"&SCHEDULED<=\"<+29d>\""
                     ((org-agenda-overriding-header " Trente jours \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消"))
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  "))))
         ;; list options for block display
         ((org-agenda-remove-tags nil)))

        ("p" "Projets"
         ((tags-todo "TODO​=\"中\"|TODO​=\"來\"|PRIORITY=\"A\""
                     ((org-agenda-overriding-header " Prochaines actions \n")
                      ;;; Do not show tasks that have been completed or cancelled. 
                      ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("完" "消")))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-timestamp-time)  %-3i  %-20c  ")))
          (tags-todo "TODO​=\"待\"|TODO​=\"見\""
                     ((org-agenda-overriding-header " Actions différées \n")
                      (org-agenda-skip-function '(sync0-org-skip-subtree-if-priority ?A))
                      ;; (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消"))
                      ;;                                ;; (org-agenda-skip-entry-if 'scheduled 'deadline)
                      ;;                                (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time-today)  %-3i  %-20c  ")))
          (tags-todo "todo+research|project"
                     ((org-agenda-overriding-header " Projets \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消" "待"))
                                                     ;; (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time-today)  %-3i  %-20c  ")))
          (tags-todo "-research-project"
                     ((org-agenda-overriding-header " Tâches brutes \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'nottodo '("未"))
                                                     ;; (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format "  %-29(sync0-org-agenda-get-project-timestamp-time-today)  %-3i  %-20c  "))))
         ;; list options for block display
         ((org-agenda-remove-tags nil)
          (org-agenda-view-columns-initially nil)))
        ;; End of custom
        ))

(evil-leader/set-key
  "A" 'org-agenda
  "a" 'sync0-pop-to-org-agenda)

(provide 'sync0-org-agenda)
