;; Set icons for use in agenda views. 
(setq org-agenda-category-icon-alist `(
                                       ("[Aa]chats" ,(list (all-the-icons-material "local_grocery_store" :height 1.2)) nil nil :ascent center)
                                       ("[Tt][aâ]ches" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
                                       ("[Tt]asks" ,(list (all-the-icons-material "check_box" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]hores" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
                                       ("[Gg]cal" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                       ("[Mh][ée]nage" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
                                       ("[Hh]abitudes" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                       ("[Hh]abits" ,(list (all-the-icons-material "date_range" :height 1.2)) nil nil :ascent center)
                                       ("[Hh]historiographie" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[ÉEée]tudes" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]lasses" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[ÉEée]v[eé]nements" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]vents" ,(list (all-the-icons-material "group" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]macs" ,(list (all-the-icons-material "code" :height 1.2)) nil nil :ascent center)
                                       ("[Ii]nformatics" ,(list (all-the-icons-material "code" :height 1.2)) nil nil :ascent center)
                                       ("[Rr]echerche" ,(list (all-the-icons-material "pageview" :height 1.2)) nil nil :ascent center)
                                       ("[Ll]yon" ,(list (all-the-icons-material "pageview" :height 1.2)) nil nil :ascent center)
                                       ("[Ll]ecture" ,(list (all-the-icons-material "import_contacts" :height 1.2)) nil nil :ascent center)
                                       ;; ("[Ll]ecture" ,(list (all-the-icons-material "spellcheck" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]ocialism" ,(list (all-the-icons-material "build" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]xercise" ,(list (all-the-icons-material "directions_run" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]ports" ,(list (all-the-icons-material "directions_run" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]ocialisme" ,(list (all-the-icons-material "build" :height 1.2)) nil nil :ascent center)
                                       ("[Oo]rganisation" ,(list (all-the-icons-material "storage" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]abinet" ,(list (all-the-icons-material "folder" :height 1.2)) nil nil :ascent center)
                                       ("[Oo]utils" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                       ("[Tt]ools" ,(list (all-the-icons-material "find_in_page" :height 1.2)) nil nil :ascent center)
                                       ;; ("[Tt]ravail" ,(list (all-the-icons-material "business_center" :height 1.2)) nil nil :ascent center)
                                       ("[Dd]octorat" ,(list (all-the-icons-material "school" :height 1.2)) nil nil :ascent center)
                                       ;; ("CTW" ,(list (all-the-icons-material "account_balance" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]efardi" ,(list (all-the-icons-material "timeline" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]olfuturo" ,(list (all-the-icons-material "euro_symbol" :height 1.2)) nil nil :ascent center)
                                       ("[Ff]inance" ,(list (all-the-icons-material "euro_symbol" :height 1.2)) nil nil :ascent center)
                                       ("[Bb]anque" ,(list (all-the-icons-material "credit_card" :height 1.2)) nil nil :ascent center)
                                       ("[Ll]oisirs" ,(list (all-the-icons-material "accessibility" :height 1.2)) nil nil :ascent center)
                                       ("[Bb]ureau" ,(list (all-the-icons-material "laptop" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]uisine" ,(list (all-the-icons-material "restaurant" :height 1.2)) nil nil :ascent center)
                                       ("[Ff]amille" ,(list (all-the-icons-material "people" :height 1.2)) nil nil :ascent center)
                                       ("[Rr]éseaux" ,(list (all-the-icons-material "share" :height 1.2)) nil nil :ascent center)
                                       ("[Pp]olitique" ,(list (all-the-icons-material "star" :height 1.2)) nil nil :ascent center)
                                       ("[Cc]olombia" ,(list (all-the-icons-material "gavel" :height 1.2)) nil nil :ascent center)
                                       ("[Ff]rance" ,(list (all-the-icons-material "gavel" :height 1.2)) nil nil :ascent center)
                                       ("[Kk]orea" ,(list (all-the-icons-material "gavel" :height 1.2)) nil nil :ascent center)
                                       ("[Pp]ortugal" ,(list (all-the-icons-material "gavel" :height 1.2)) nil nil :ascent center)
                                       ;; ("[Ww]ork" ,(list (all-the-icons-material "work" :height 1.2)) nil nil :ascent center)
                                       ("[Tt]ravail" ,(list (all-the-icons-material "business_center" :height 1.2)) nil nil :ascent center)
                                       ("[Dd]iary" ,(list (all-the-icons-material "today" :height 1.2)) nil nil :ascent center)
                                       ("[Mm]essages" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                       ("[Mm]essages ([[:graph:]]+)" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                       ("[Mm]\. ([[:graph:]]+)" ,(list (all-the-icons-material "mail_outline" :height 1.2)) nil nil :ascent center)
                                       ("[EÉée]criture" ,(list (all-the-icons-material "create" :height 1.2)) nil nil :ascent center)
                                       ("[Ww]riting" ,(list (all-the-icons-material "create" :height 1.2)) nil nil :ascent center)
                                       ("[Pp]ortuguês" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ff]rançais" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Dd]eutsch" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]spañol" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ee]nglish" ,(list (all-the-icons-material "translate" :height 1.2)) nil nil :ascent center)
                                       ("[Ss]tudy hacks" ,(list (all-the-icons-material "wb_incandescent" :height 1.2)) nil nil :ascent center)
                                       ("[Zz]ettelkasten" ,(list (all-the-icons-material "dashboard" :height 1.2)) nil nil :ascent center)
                                       ("[Bb]log" ,(list (all-the-icons-material "speaker_notes" :height 1.2)) nil nil :ascent center)))

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
                   (org-agenda-prefix-format " %-21t %-6s %-3i %-15c  ")))
          (tags-todo "+current"
                     ((org-agenda-overriding-header "En cours\n")
                      (org-agenda-skip-function '(or (sync0-org-skip-subtree-if-priority ?A)
                                                      (org-agenda-skip-entry-if 'scheduled 'deadline)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "urgent|PRIORITY=\"A\""
                     ((org-agenda-overriding-header "Haute priorité\n")
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
                   ;; (org-agenda-prefix-format " %-21t %-6s %-3i %-15c %(sync0-org-agenda-print-parent-node) ")))
                   ;; (org-agenda-prefix-format "  %-21t  %-6s  %-3i  %-15c  ")))
          ;; (tags-todo "next_week|this_week|DEADLINE>=\"<+2d>\"&DEADLINE<=\"<+14d>\"|SCHEDULED>=\"<+2d>\"&SCHEDULED<=\"<+14d>\""
          ;;            ((org-agenda-overriding-header "Quatorze jours\n")
          ;;             (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消" "推"))
          ;;                                            (sync0-org-skip-subtree-if-priority ?A)))
          ;;             (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
          ;;             (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          ;; (tags-todo "this_month|DEADLINE>=\"<+15d>\"&DEADLINE<=\"<+30d>\"|SCHEDULED>=\"<+15d>\"&SCHEDULED<=\"<+30d>\""
          ;;            ((org-agenda-overriding-header "Trente jours\n")
          ;;             (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消" "推"))
          ;;                                            (sync0-org-skip-subtree-if-priority ?A)))
          ;;             (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
          ;;             (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "+next"
                     ((org-agenda-overriding-header "À suivre\n")
                      (org-agenda-skip-function '(or (sync0-org-skip-subtree-if-priority ?A)
                                                      (org-agenda-skip-entry-if 'scheduled)))
                                                      ;; (org-agenda-skip-entry-if 'scheduled 'deadline)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "+delegated"
                     ((org-agenda-overriding-header "Délégués\n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消"))
                                                      (org-agenda-skip-entry-if 'scheduled)
                                                      ;; (org-agenda-skip-entry-if 'scheduled 'deadline)
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  "))))
          ;; (tags-todo "this_week|DEADLINE>=\"<+2d>\"&DEADLINE<=\"<+7d>\"|SCHEDULED>=\"<+2d>\"&SCHEDULED<=\"<+7d>\""
          ;;            ((org-agenda-overriding-header " Sept jours \n")
          ;;             (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消" "推"))
          ;;                                            (sync0-org-skip-subtree-if-priority ?A)))
          ;;             (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
          ;;             (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          ;;             ;; (org-agenda-prefix-format "  %-29(sync0-org-agenda-tags-todo-timestamp)  %-3i  %-15c  ")))
          ;;             ;; (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c %(sync0-org-agenda-print-parent-node) ")))
          ;; (tags-todo "next_week|DEADLINE>=\"<+8d>\"&DEADLINE<=\"<+14d>\"|SCHEDULED>=\"<+8d>\"&SCHEDULED<=\"<+14d>\""
          ;;            ((org-agenda-overriding-header " Quatorze jours \n")
          ;;             (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消" "推"))
          ;;                                            (sync0-org-skip-subtree-if-priority ?A)))
          ;;             (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
          ;;             (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  "))))
                      ;; (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c %(sync0-org-agenda-print-parent-node) "))))
                      ;; (org-agenda-prefix-format "  %-29(sync0-org-agenda-tags-todo-timestamp)  %-3i  %-15c  "))))
         ;; list options for block display
         ((org-agenda-remove-tags nil)))

        ("p" "Projets"
         ((tags-todo "+current"
                     ((org-agenda-overriding-header "En cours\n")
                      (org-agenda-skip-function '(or (sync0-org-skip-subtree-if-priority ?A)
                                                      (org-agenda-skip-entry-if 'scheduled)))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "urgent|PRIORITY=\"A\""
                     ((org-agenda-overriding-header " Haute priorité \n")
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "+next"
                     ((org-agenda-overriding-header " Prochaines actions \n")
                      (org-agenda-skip-function '(or (sync0-org-skip-subtree-if-priority ?A))
                                                     (org-agenda-skip-entry-if 'scheduled))
                      (org-agenda-sorting-strategy '(timestamp-up category-keep tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "+waiting"
                     ((org-agenda-overriding-header " Actions différées \n")
                      (org-agenda-skip-function '(or (sync0-org-skip-subtree-if-priority ?A))
                                                     (org-agenda-skip-entry-if 'scheduled))
                      (org-agenda-sorting-strategy '(category-keep timestamp-up tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "-someday+research|-someday+project"
                     ((org-agenda-overriding-header " Projets \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo '("完" "消"))
                                                     (org-agenda-skip-entry-if 'scheduled)
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(category-keep timestamp-up tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp)  %-3i  %-15c  ")))
          (tags-todo "-research-project-someday-next-current-waiting"
                     ((org-agenda-overriding-header " Tâches brutes \n")
                      (org-agenda-skip-function '(or (org-agenda-skip-entry-if 'scheduled)
                                                     (sync0-org-skip-subtree-if-priority ?A)))
                      (org-agenda-sorting-strategy '(category-keep timestamp-up tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  ")))
          (tags-todo "someday"
                     ((org-agenda-overriding-header " Un jour lointain \n")
                      (org-agenda-skip-function '(or (sync0-org-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-entry-if 'scheduled)))
                      (org-agenda-sorting-strategy '(category-keep timestamp-up tag-up todo-state-up))
                      (org-agenda-prefix-format " %-29(sync0-org-agenda-tags-todo-timestamp) %-3i %-15c  "))))
         ;; list options for block display
         ((org-agenda-remove-tags nil)
          (org-agenda-view-columns-initially nil)))
        ;; End of custom
        ))

(setq
 org-agenda-hide-tags-regexp "projects\\|research\\|project\\|important\\|short_term\\|long_term\\|no_export\\|this_month\\|this_week\\|next_week\\|next_moth\\|todo\\|etc\\|doctorat\\|message\\|someday\\|delegated\\|current\\|next\\|waiting\\|menage")


(provide 'sync0-org-agenda)
