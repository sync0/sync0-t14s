
(defun sync0--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards 'max 'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

(defun sync0-number-to-month (arg &optional no-abbrev)
  "Helper function to convert a number into the month name"
  (if no-abbrev
      (cond ((equal 1 arg) "Janvier")
            ((equal 2 arg) "Février")
            ((equal 3 arg) "Mars")
            ((equal 4 arg) "Avril")
            ((equal 5 arg) "Mai")
            ((equal 6 arg) "Juin")
            ((equal 7 arg) "Juillet")
            ((equal 8 arg) "Août")
            ((equal 9 arg) "Septembre")
            ((equal 10 arg) "Octobre")
            ((equal 11 arg) "Novembre")
            ((equal 12 arg) "Décembre")
            (t "nil"))
    (cond ((equal 1 arg) "Jan.")
          ((equal 2 arg) "Fév.")
          ((equal 3 arg) "Mars")
          ((equal 4 arg) "Avr.")
          ((equal 5 arg) "Mai")
          ((equal 6 arg) "Juin")
          ((equal 7 arg) "Jul.")
          ((equal 8 arg) "Août")
          ((equal 9 arg) "Sep.")
          ((equal 10 arg) "Oct.")
          ((equal 11 arg) "Nov.")
          ((equal 12 arg) "Déc.")
          (t "nil"))))

(defun sync0-number-to-day (arg &optional no-abbrev)
  "Helper function to convert a number into the day name"
  (if no-abbrev
      (cond ((equal 1 arg) "Dimanche")
            ((equal 2 arg) "Lundi")
            ((equal 3 arg) "Mardi")
            ((equal 4 arg) "Mercredi")
            ((equal 5 arg) "Jeudi")
            ((equal 6 arg) "Vendredi")
            ((equal 7 arg) "Samedi")
            (t "nil"))
    (cond ((equal 1 arg) "Dim.")
          ((equal 2 arg) "Lun.")
          ((equal 3 arg) "Mar.")
          ((equal 4 arg) "Mer.")
          ((equal 5 arg) "Jeu.")
          ((equal 6 arg) "Ven.")
          ((equal 7 arg) "Sam.")
          (t "nil"))))



(defun sync0-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
                             This function makes sure that dates are aligned for easy reading."
  (let* ((dayname (calendar-day-name date nil nil))
         (day (cadr date))
         ;; (french (calendar-french-date-string date))
         ;; (french (substring (calendar-french-date-string date) 0 -6))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month nil))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format "%-2s %2d %s"
            dayname day monthname)))

(setq org-agenda-format-date 'sync0-org-agenda-format-date-aligned)

;; Set of functions to have evil bindings in org-agenda.
(defun sync0-org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (sync0--org-agenda-goto-header))

(defun sync0-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (sync0--org-agenda-goto-header t))

;; Fast access agenda view.
(defun sync0-pop-to-org-agenda (&optional split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil "h")
  (when (not split)
    (delete-other-windows)))


(defun sync0-org-agenda-get-timestamp-time ()
  "Get timestamp from current org-agenda time"
  ;; Firs, determine whether the headline has both a schedule and
  ;; deadeline?
  ;; 
  ;; NOTE: The first part of the conditional (the "((and ...)"
  ;; part) has schedules take precedence over deadelines based on
  ;; the assumption that headlines are scheduled so as to be
  ;; accomplished before the deadline. Therefore, although
  ;; deadlines could occur before schedules, displaying this
  ;; information in the org-agenda would not offer any useful
  ;; information for planning purpose. In such cases, for real
  ;; tasks the headline would be eventually re-scheduled so as to
  ;; observe the rule that schedules take precedence over
  ;; deadlines. 
  ;;  
  ;;  When both a schedule and a deadline have been defined:
  (cond ((and (org-get-scheduled-time (point))
              (org-get-deadline-time (point)))
         (let* ((schedule (org-get-scheduled-time (point)))
                (tomorrow-string  (shell-command-to-string "echo -n $(date -d tomorrow +'%Y-%m-%d')"))
                (org-schedule-string  (format-time-string "%Y-%m-%d" schedule))
                (month-start-name (capitalize (format-time-string "%b" schedule)))
                (day-start (format-time-string "%d" schedule))
                (day-start-name (capitalize (format-time-string "%a" schedule)))
                (time-start-test (format-time-string "%H:%M" schedule))
                (time-start (if (string= "00" (format-time-string "%M" schedule)) 
                                (format-time-string "%Hh" schedule)
                              (format-time-string "%H:%M" schedule)))
                (deadline (org-get-deadline-time (point)))
                (org-deadline-string (format-time-string "%Y-%m-%d" deadline))
                (day-end-raw (format-time-string "%d" deadline))
                (day-end (if (string-match "0\\([[:digit:]]$\\)" day-end-raw)
                             (match-string 1 day-end-raw) day-end-raw))
                ;; (day-end  (format-time-string "%d" deadline))
                (day-end-name (capitalize (format-time-string "%a" deadline)))
                (time-end-test (format-time-string "%H:%M" deadline))
                (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                              (format-time-string "%Hh" deadline)
                            (format-time-string "%H:%M" deadline))))
           ;; Test whether the hour and minute "%H:%M" string is
           ;; relevant and, thus, should be included in org-agenda
           ;; views.
           (cond ((and  (string= "00:00" time-start-test)
                        (string= "00:00" time-end-test))
                  (if (or (string= tomorrow-string org-schedule-string)
                          (string= tomorrow-string org-deadline-string))
                      (concat "Demain" "-" day-end " " month-start-name)
                    (concat day-start-name " " day-start "-" day-end " " month-start-name)))
                 ((or (not (string= "00:00" time-start-test))
                      (not (string= "00:00" time-end-test)))
                  (if (or (string= tomorrow-string org-schedule-string)
                          (string= tomorrow-string org-deadline-string))
                      (concat "Demain" "-" day-end " " month-start-name ", " time-start "-" time-end)
                    (concat day-start "-" day-end " " month-start-name ", " time-start "-" time-end))))))
        ;; Second part, when either schedule or deadline have been
        ;; defined:
        ((or (org-get-scheduled-time (point))
             (org-get-deadline-time (point)))
         ;; Follow the convention that schedules take precedence
         ;; over deadlines. If schedule has been defined:
         (if (org-get-scheduled-time (point))
             (let* ((schedule (org-get-scheduled-time (point)))
                    (tomorrow-string  (shell-command-to-string "echo -n $(date -d tomorrow +'%Y-%m-%d')"))
                    (org-schedule-string (format-time-string "%Y-%m-%d" schedule))
                    ;; Define the object "scheduled" containing the date
                    ;; information from which all the other variables wiil be
                    ;; defined.
                    (element (org-element-at-point))
                    (scheduled (org-element-property :scheduled element))
                    (year-start (org-element-property :year-start scheduled))
                    (year-start-string (number-to-string year-start))  
                    (year-end (org-element-property :year-end scheduled))
                    (year-end-string (if year-end (number-to-string year-end) year-start-string))
                    (month-start (org-element-property :month-start scheduled))
                    (month-start-string (number-to-string month-start))
                    (month-start-name (sync0-number-to-month month-start))
                    (month-start-name-full (sync0-number-to-month month-start t))
                    (month-end (org-element-property :month-end scheduled))
                    (month-end-string (if month-end (number-to-string month-end) "0"))
                    (month-end-name (sync0-number-to-month month-end))
                    (month-end-name-full (sync0-number-to-month month-end t))
                    (day-start  (org-element-property :day-start scheduled))
                    (day-start-string (number-to-string day-start)) 
                    ;; (day-start-string (if (<= day-start 9) 
                    ;;                       (concat "0" (number-to-string day-start))
                    ;;                               (number-to-string day-start)))
                    (day-start-name   (calendar-day-name (list month-start day-start year-start)))
                    (day-start-name-abbrev   (calendar-day-name (list month-start day-start year-start) t))
                    (day-end (org-element-property :day-end scheduled))
                    (day-end-string (when day-end (number-to-string day-end)))
                    (day-end-name  (calendar-day-name (list month-end day-end year-end)))
                    (day-end-name-abbrev  (calendar-day-name (list month-end day-end year-end) t))
                    (hour-start (org-element-property :hour-start scheduled))
                    (hour-start-string (if hour-start (number-to-string hour-start) "0"))
                    (hour-end (org-element-property :hour-end scheduled))
                    (hour-end-string (if hour-end (number-to-string hour-end) "0"))
                    (minute-start (org-element-property :minute-start scheduled))
                    (minute-start-string (if minute-start (number-to-string minute-start) "0"))
                    (minute-end (org-element-property :minute-end scheduled)) 
                    (minute-end-string (if minute-end (number-to-string minute-end) "0"))
                    (time-end-test (concat hour-end-string ":" minute-end-string))
                    (time-end (if (string= "0" minute-end-string) 
                                  (concat hour-end-string "h")
                                (concat hour-end-string ":" minute-end-string)))
                    (time-start-test (concat hour-start-string ":" minute-start-string))
                    (time-start (if (string= "0" minute-start-string) 
                                    (if time-end (concat hour-start-string "")
                                      (concat hour-start-string "h"))
                                  (concat hour-start-string ":" minute-start-string))))
               ;; First, let's see what to do when the schedule is not on the same day 
               (cond 
                ((and (string= month-start-string month-end-string)
                      (not (string= day-start-string day-end-string))
                      (not (string= time-start-test "0:0"))
                      (not (string= time-end-test "0:0")))
                 ;; same-month, different-day, time-start, time-end
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain, " time-start "-" time-end)
                   (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)))
                ((and (string= month-start-string month-end-string)
                      (not (string= day-start-string day-end-string))
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain - " day-end-string ", " time-start "-" time-end)
                   ;; same-month, different-day, time-start
                   (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)))
                ((and (string= month-start-string month-end-string)
                      (not (string= day-start-string day-end-string))
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 ;; same-month, different-day
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain - " day-end-string " " month-start-name-full)
                   (concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full)))
                ((and (string= month-start-string month-end-string)
                      (string= day-start-string day-end-string)
                      (not (string= time-start-test "0:0"))
                      (not (string= time-end-test "0:0")))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain, " time-start "-" time-end)
                   ;; same-month, same-day, time-start, time-end 
                   (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start "-" time-end)))
                ((and (string= month-start-string month-end-string)
                      (string= day-start-string day-end-string)
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain, " time-start)
                   ;; same-month, same-day, time-start
                   (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start)))
                ((and (string= month-start-string month-end-string)
                      (string= day-start-string day-end-string)
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain")
                   ;; same-month, same-day, same-year
                   (concat day-start-name-abbrev " " day-start-string " " month-start-name-full)))
                ((and (not (string= month-start-string month-end-string))
                      (not (string= day-start-string day-end-string))
                      (not (string= time-start-test "0:0"))
                      (not (string= time-end-test "0:0")))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain, " time-start " " day-end-string " " month-end-name ", " time-end)
                   ;; different-month, different-day, time-start, time-end
                   (concat day-start-string " " month-start-name ", " time-start " - " day-end-string " " month-end-name ", " time-end)))
                ((and (not (string= month-start-string month-end-string))
                      (not (string= day-start-string day-end-string))
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain, " time-start " - " day-end-string " " month-end-name)
                   ;; different-month, different-day, time-start
                   (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name)))
                ((and (not (string= month-start-string month-end-string))
                      (not (string= day-start-string day-end-string))
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 (if (string= tomorrow-string org-schedule-string)
                     (concat "Demain"  " - " day-end-name-abbrev " " day-end-string " " month-end-name)
                   ;; different-month, different-day
                   (concat day-start-name-abbrev " " day-start-string " " month-start-name " - " day-end-name-abbrev " " day-end-string " " month-end-name)))))
           ;; If deadline has been defined:
           (let* ((deadline (org-get-deadline-time (point)))
                  (org-deadline-string  (format-time-string "%Y-%m-%d" deadline))
                  (tomorrow-string  (shell-command-to-string "echo -n $(date -d tomorrow +'%Y-%m-%d')"))
                  ;; Eliminate the annoying zeroes at the beginning
                  (day-end-raw (format-time-string "%d" deadline))
                  (day-end (if (string-match "0\\([[:digit:]]$\\)" day-end-raw)
                               (match-string 1 day-end-raw) day-end-raw))
                  (month-end-name-abbrev  (capitalize (format-time-string "%b" deadline)))
                  (month-end-name  (capitalize (format-time-string "%B" deadline)))
                  (day-end-name  (capitalize (format-time-string "%a" deadline)))
                  (time-end-test (format-time-string "%H:%M" deadline))
                  (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                                (format-time-string "%Hh" deadline)
                              (format-time-string "%H:%M" deadline))))
             (cond ((and (string= "00:00" time-end-test)
                         (string= tomorrow-string org-deadline-string))
                    (concat "Demain")) 
                   ((and (not (string= "00:00" time-end-test))
                         (string= tomorrow-string org-deadline-string))
                    (concat "Demain, " time-end)) 
                   ((and  (string= "00:00" time-end-test)
                          (not (string= tomorrow-string org-deadline-string)))
                    (concat day-end-name " " day-end " " month-end-name)) 
                   ((and (not (string= "00:00" time-end-test))
                         (not (string= tomorrow-string org-deadline-string)))
                    (concat day-end-name " " day-end " " month-end-name ", " time-end))))))
        ;; If neither schedule nor deadline have been defined:
        (t " ")))


(defun sync0-org-agenda-get-project-timestamp-time-today ()
  "Get timestamp from current org-agenda time"
  ;; Check whether both schedule and deadline are defined.
  (cond ((and (org-get-scheduled-time (point))
              (org-get-deadline-time (point)))
         (let* ((schedule (org-get-scheduled-time (point)))
                (month-start-name (capitalize (format-time-string "%b" schedule)))
                (day-start (capitalize (format-time-string "%d" schedule)))
                (day-start-name (capitalize (format-time-string "%a" schedule)))
                (time-start-test (format-time-string "%H:%M" schedule))
                (time-start (if (string= "00" (format-time-string "%M" schedule)) 
                                (format-time-string "%Hh" schedule)
                              (format-time-string "%H:%M" schedule)))
                (deadline (org-get-deadline-time (point)))
                (month-end-name (capitalize (format-time-string "%b" deadline)))
                (day-end (capitalize (format-time-string "%d" deadline)))
                (day-end-name (capitalize (format-time-string "%a" deadline)))
                (time-end-test (format-time-string "%H:%M" deadline))
                (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                              (format-time-string "%Hh" deadline)
                            (format-time-string "%H:%M" deadline))))
           ;; Stylize the date output
           (cond ((and  (string= "00:00" time-start-test)
                        (string= "00:00" time-end-test))
                  (if (equal month-start-name month-end-name)
                      (concat day-start-name " " day-start "-" day-end " " month-start-name)
                    (concat day-start " " month-start-name " - " day-end " " month-end-name)))
                 ((or (not (string= "00:00" time-start-test))
                      (not (string= "00:00" time-end-test)))
                  (if (equal month-start-name month-end-name)
                      (concat day-start "-" day-end " " month-start-name ", " time-start "-" time-end)
                    (concat day-start " " month-start-name ", " time-start " " day-end " " month-end-name ", "time-end))))))
        ;; If either schedule or deadline have been defined. 
        ((or (org-get-scheduled-time (point))
             (org-get-deadline-time (point)))
         ;; If schedule has been defined.
         (if (org-get-scheduled-time (point))
             (let* ((element (org-element-at-point))
                    (scheduled (org-element-property :scheduled element))
                    ;; Get current year (from shell) and convert to number for conditional comparisons
                    (current-year (string-to-number (shell-command-to-string "echo -n $(date +'%Y')")))
                    (year-start (org-element-property :year-start scheduled))
                    (year-start-string (when year-start (number-to-string year-start)))
                    (year-end (org-element-property :year-end scheduled))
                    (year-end-string (if year-end (number-to-string year-end) year-start-string))
                    (month-start (org-element-property :month-start scheduled))
                    (month-start-string (if month-start (number-to-string month-start) "0"))
                    (month-start-name (sync0-number-to-month month-start))
                    (month-start-name-full (sync0-number-to-month month-start t))
                    (month-end (org-element-property :month-end scheduled))
                    (month-end-string (if month-end (number-to-string month-end) "0"))
                    (month-end-name (sync0-number-to-month month-end))
                    (month-end-name-full (sync0-number-to-month month-end t))
                    (day-start (org-element-property :day-start scheduled))
                    (day-start-string (when day-start (number-to-string day-start)))
                    (day-start-name   (calendar-day-name (list month-start day-start year-start)))
                    (day-start-name-abbrev   (calendar-day-name (list month-start day-start year-start) t))
                    (day-end (org-element-property :day-end scheduled))
                    (day-end-string (when day-end (number-to-string day-end)))
                    (day-end-name  (calendar-day-name (list month-end day-end year-end)))
                    (day-end-name-abbrev  (calendar-day-name (list month-end day-end year-end) t))
                    (hour-start (org-element-property :hour-start scheduled))
                    (hour-start-string (if hour-start (number-to-string hour-start) "0"))
                    (hour-end (org-element-property :hour-end scheduled))
                    (hour-end-string (if hour-end (number-to-string hour-end) "0"))
                    (minute-start (org-element-property :minute-start scheduled))
                    (minute-start-string (if minute-start (number-to-string minute-start) "0"))
                    (minute-end (org-element-property :minute-end scheduled)) 
                    (minute-end-string (if minute-end (number-to-string minute-end) "0"))
                    (time-end-test (concat hour-end-string ":" minute-end-string))
                    (time-end (if (string= "0" minute-end-string) 
                                  (concat hour-end-string "h")
                                (concat hour-end-string ":" minute-end-string)))
                    (time-start-test (concat hour-start-string ":" minute-start-string))
                    (time-start (if (string= "0" minute-start-string) 
                                    (if time-end (concat hour-start-string "")
                                      (concat hour-start-string "h"))
                                  (concat hour-start-string ":" minute-start-string))))

               ;; First, let's see what to do when the schedule is not on the same day 
               (cond 
                ((and (= month-start month-end)
                      (not (= day-start day-end))
                      (not (string= time-start-test "0:0"))
                      (not (string= time-end-test "0:0")))
                 ;; same-month, different-day, time-start, time-end
                 (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end))
                ((and (= month-start month-end)
                      (= current-year year-end)
                      (not (= day-start day-end))
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 ;; same-month, different-day, time-start, same-year
                 (concat day-start-string "-" day-end-string " " month-start-name ", " time-start))
                ((and (= month-start month-end)
                      (not (= current-year year-end))
                      (not (= day-start day-end))
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 ;; same-month, different-day, time-start, different-year
                 (concat day-start-string "-" day-end-string " " month-start-name ", " time-start year-end-string))
                ((and (= month-start month-end)
                      (not (= day-start day-end))
                      (= current-year year-end)
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 ;; same-month, different-day, same-year
                 (concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full))
                ((and (= month-start month-end)
                      (not (= day-start day-end))
                      (not (= current-year year-end))
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 ;; same-month, different-day, different-year
                 (concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full " " year-end-string))
                ((and (= month-start month-end)
                      (= day-start day-end)
                      (not (string= time-start-test "0:0"))
                      (not (string= time-end-test "0:0")))
                 ;; same-month, same-day, time-start, time-end 
                 (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start "-" time-end))
                ((and (= month-start month-end)
                      (= day-start day-end)
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 ;; same-month, same-day, time-start
                 (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start))
                ((and (= month-start month-end)
                      (= day-start day-end)
                      (= current-year year-end)
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 ;; same-month, same-day, same-year
                 (concat day-start-name-abbrev " " day-start-string " " month-start-name-full))
                ((and (= month-start month-end)
                      (= day-start day-end)
                      (not (= current-year year-end))
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 ;; same-month, same-day
                 (concat day-start-name-abbrev " " day-start-string " " month-start-name-full " " year-end-string))
                ((and (not (= month-start month-end))
                      (not (= day-start day-end))
                      (not (string= time-start-test "0:0"))
                      (not (string= time-end-test "0:0")))
                 ;; different-month, different-day, time-start, time-end
                 (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name ", " time-end))
                ((and (not (= month-start month-end))
                      (not (= day-start day-end))
                      (not (string= time-start-test "0:0"))
                      (string= time-end-test "0:0"))
                 ;; different-month, different-day, time-start
                 (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name))
                ((and (not (= month-start month-end))
                      (not (= day-start day-end))
                      (string= time-start-test "0:0")
                      (string= time-end-test "0:0"))
                 ;; different-month, different-day
                 (concat day-start-name-abbrev " " day-start-string " " month-start-name " - " day-end-name-abbrev " " day-end-string " " month-end-name))))

           ;; If deadline has been defined
           (let* ((deadline (org-get-deadline-time (point)))
                  (element (org-element-at-point))
                  (deadlined (org-element-property :deadline element))
                  (day-end (org-element-property :day-end deadlined))
                  (day-end-string (when day-end (number-to-string day-end)))
                  (month-end-name-abbrev  (capitalize (format-time-string "%b" deadline)))
                  (month-end-name  (capitalize (format-time-string "%B" deadline)))
                  (day-end-name  (capitalize (format-time-string "%a" deadline)))
                  (time-end-test (format-time-string "%H:%M" deadline))
                  (time-end (if (string= "00" (format-time-string "%M" deadline)) 
                                (format-time-string "%Hh" deadline)
                              (format-time-string "%H:%M" deadline))))
             (if (string= "00:00" time-end-test)
                 (concat day-end-name " " day-end-string " " month-end-name) 
               (concat day-end-name " " day-end-string " " month-end-name ", " time-end)))))
        (t " ")))

(defun sync0-org-agenda-get-project-timestamp-time ()
  "Get timestamp from current org-agenda time"
  (let* ((schedule (org-get-scheduled-time (point)))
         (deadline (org-get-deadline-time (point)))
         (schedule-date (when schedule (let ((time (capitalize (format-time-string "%a %d %b (%H:%M) %Y" schedule)))
                                             (hour (format-time-string "%H:%M" schedule))
                                             (time-no-hour (capitalize (format-time-string "%a %d %B %Y" schedule))))
                                         (if (not (string= "00:00" hour)) time time-no-hour))))
         ;; For the second block, I use "if" instead of "when" to print a
         ;; blank when neither "schedules" nor "deadlines" are set.
         (deadline-date (if deadline (let ((time (capitalize (format-time-string "%a %d %b (%H:%M) %Y" deadline)))
                                           (hour (format-time-string "%H:%M" deadline))
                                           (time-no-hour (capitalize (format-time-string "%a %d %B %Y" deadline))))
                                       (if (not (string= "00:00" hour)) time time-no-hour)) "")))
    (if schedule (princ schedule-date) (princ deadline-date))))

;; This function was borrowed from Sacha Chua's configuration. 
(defun sync0-org-agenda-new ()
  "Create a new note or task at the current agenda item. Creates it
                                                                     at
                                                                     the
                                                                     same
                                                                     level
                                                                     as
                                                                     the
                                                                     previous
                                                                     task,
                                                                     so
                                                                     it's
                                                                     better
                                                                     to
                                                                     use
                                                                     this
                                                                     with
                                                                     to-do
                                                                     items
                                                                     than
                                                                     with
                                                                     projects
                                                                     or
                                                                     headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

;; necessary function 1
(defun sync0-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY. PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; necessary function 2
(defun sync0-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))


(provide 'sync0-org-agenda-functions)
