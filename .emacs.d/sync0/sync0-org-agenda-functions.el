
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
      (cond ((equal 1 arg) "January")
            ((equal 2 arg) "February")
            ((equal 3 arg) "March")
            ((equal 4 arg) "April")
            ((equal 5 arg) "May")
            ((equal 6 arg) "June")
            ((equal 7 arg) "July")
            ((equal 8 arg) "August")
            ((equal 9 arg) "September")
            ((equal 10 arg) "October")
            ((equal 11 arg) "November")
            ((equal 12 arg) "December")
            (t "nil"))
    (cond ((equal 1 arg) "Jan.")
          ((equal 2 arg) "Feb.")
          ((equal 3 arg) "Mar.")
          ((equal 4 arg) "Apr.")
          ((equal 5 arg) "May")
          ((equal 6 arg) "Jun.")
          ((equal 7 arg) "Jul.")
          ((equal 8 arg) "Aug.")
          ((equal 9 arg) "Sep.")
          ((equal 10 arg) "Oct.")
          ((equal 11 arg) "Nov.")
          ((equal 12 arg) "Dec.")
          (t "nil"))))

(defun sync0-number-to-day (arg &optional no-abbrev)
  "Helper function to convert a number into the day name"
  (if no-abbrev
      (cond ((equal 1 arg) "Sunday")
            ((equal 2 arg) "Monday")
            ((equal 3 arg) "Tuesday")
            ((equal 4 arg) "Wednesday")
            ((equal 5 arg) "Thursday")
            ((equal 6 arg) "Friday")
            ((equal 7 arg) "Saturday")
            (t "nil"))
    (cond ((equal 1 arg) "Sun.")
          ((equal 2 arg) "Mon.")
          ((equal 3 arg) "Tue.")
          ((equal 4 arg) "Wed.")
          ((equal 5 arg) "Thu.")
          ((equal 6 arg) "Fri.")
          ((equal 7 arg) "Sat.")
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

;; (setq org-agenda-format-date 'sync0-org-agenda-format-date-aligned)

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


(defun sync0-org-agenda-tags-todo-timestamp ()
  "Get timestamp from current org-agenda item."
  ;; First, determine whether the headline has both a schedule and
  ;; deadeline?
  ;; 
  ;; NOTE: The first part of the conditional (the "((and ...)"
  ;; part) has schedules take precedence over deadelines based on
  ;; the assumption that headlines are scheduled so as to be
  ;; accomplished before the deadline. Therefore, although
  ;; deadlines could occur before schedules, displaying this
  ;; information in the org-agenda would not offer any useful
  ;; information for planning purposes. In such cases, for real
  ;; tasks the headline would be eventually re-scheduled so as to
  ;; observe the rule that schedules take precedence over
  ;; deadlines.
  ;;  
  ;;  When both a schedule and a deadline have been defined:
  (cond ((and (org-get-scheduled-time (point))
              (org-get-deadline-time (point)))
         (let* ((schedule (org-get-scheduled-time (point)))
                (schedule-date  (format-time-string "%Y-%m-%d" schedule))
                (year-start  (format-time-string "%Y" schedule))
                (today  (format-time-string "%Y-%m-%d"))
                (tomorrow (org-read-date nil nil "+1")) 
                (this-year  (format-time-string "%Y"))
                (month-start (capitalize (format-time-string "%b." schedule)))
                (month-start-full (format-time-string "%B" schedule))
                (time-start (cond
                             ((string= "00:00" (format-time-string "%H:%M" schedule))
                              "")
                             ((string= "00" (format-time-string "%M" schedule)) 
                              (format-time-string "%Hh" schedule))
                             (t (format-time-string "%H:%M" schedule))))
                (day-start (cond
                            ((string= today schedule-date)
                             "Today")
                            ((string= tomorrow schedule-date)
                             "Tomorrow")
                            (t (concat
                                ;; day name
                                (capitalize (format-time-string "%a." schedule))
                                " "
                                ;; number
                                (format-time-string "%d" schedule)))))
                (deadline (org-get-deadline-time (point)))
                (deadline-date  (format-time-string "%Y-%m-%d" deadline))
                (year-end  (format-time-string "%Y" deadline))
                (month-end (capitalize (format-time-string "%b." deadline)))
                (day-end-raw (format-time-string "%d" deadline))
                ;; remove zeroes from the day end for ease of visuals
                (day-end (if (string-match "0\\([[:digit:]]$\\)" day-end-raw)
                             (match-string 1 day-end-raw)
                           day-end-raw))
                (time-end (cond
                           ((string= "00:00" (format-time-string "%H:%M" deadline))
                            "")
                           ((string= "00" (format-time-string "%M" deadline)) 
                            (format-time-string "%Hh" deadline))
                           (t (format-time-string "%H:%M" deadline))))
                (fragment-start (if (or (string= month-start month-end)
                                        (string= day-start "Today")
                                        (string= day-start "Tomorrow"))
                                    day-start
                                  (concat day-start " " month-start)))
                (fragment-end
                 (cond ((and (string= schedule-date deadline-date)
                             (not (string= time-start time-end))
                             (not (string= time-start ""))
                             (not (string= time-end "")))
                        (concat " " month-start-full ", " time-start "-" time-end))
                       ;; check: same day, start time
                       ((and (string= schedule-date deadline-date)
                             (not (string= time-start "")))
                        (concat " " month-start-full ", " time-start))
                       ;; check: same day 
                       ((string= schedule-date deadline-date)
                        (concat " " month-start-full))
                       ;; check: same month, start time, end time
                       ((and (string= month-start month-end)
                             (not (string= time-start ""))
                             (not (string= time-end "")))
                        (concat " - " day-end " " month-start-full ", " time-start "-" time-end))
                       ;; check: same month, start time 
                       ((and (string= month-start month-end)
                             (not (string= time-start "")))
                        (concat " - " day-end " " month-start-full ", " time-start))
                       ;; check: same month 
                       ((string= month-start month-end)
                        (concat " - " day-end " " month-start-full)) 
                       ;; check: start-time, end time
                       ((and (not (string= month-start month-end))
                             (not (string= time-start ""))
                             (not (string= time-end "")))
                        (concat " - " day-end " " month-end ", " time-start "-" time-end)) 
                       ;; check: start-time
                       ((and (not (string= month-start month-end))
                             (not (string= time-start "")))
                        (concat " - " day-end-string " " month-end ", " time-start)) 
                       (t (concat " - " day-end " " month-end))))
                 ;; ;; check: schedule date and dealine date equal, end time
                 ;; (cond ((and (string= schedule-date deadline-date)
                 ;;             (not (string= time-end "")))
                 ;;        (concat "-" time-end))
                 ;; ;; check: month start equal month end, end time
                 ;;       ((and (string= month-start month-end)
                 ;;             (not (string= time-end "")))
                 ;;        (concat "-" day-end month-end ", " time-end))
                 ;; ;; check: month start equal month end, no end time
                 ;;       ((and (string= month-start month-end)
                 ;;             (string= time-end ""))
                 ;;        (concat "-" day-end month-end))
                 ;; ;; check: month start different month end, end time
                 ;;       ((and (not (string= month-start month-end))
                 ;;             (string= time-end ""))
                 ;;        (concat "-" day-end month-end ", " time-end))
                 ;;       (t (concat " " month-start)))
                (year (cond ((not (string= year-start this-year))
                             (concat " " year-start))
                            ((not (string= year-end this-year))
                             (concat " " year-end))
                            (t ""))))
           ;; Stylize the date output
           (concat fragment-start fragment-end year)))

        ;; Second part, when either schedule or deadline have been
        ;; defined:
        ;; If schedule has been defined.
        ((org-get-scheduled-time (point))
         (let* ((element (org-element-at-point))
                (schedule (org-element-property :scheduled element))
                (this-year  (format-time-string "%Y"))
                (today  (format-time-string "%Y-%m-%d"))
                (tomorrow (org-read-date nil nil "+1")) 
                (year-start (org-element-property :year-start schedule))
                (year-start-string (number-to-string year-start))
                (year-end (org-element-property :year-end schedule))
                (year-end-string (if year-end
                                     (number-to-string year-end)
                                   year-start-string))
                (month-start (org-element-property :month-start schedule))
                (month-start-string (number-to-string month-start)) 
                (month-start-name (sync0-number-to-month month-start))
                (month-start-name-full (sync0-number-to-month month-start t))
                (month-end (org-element-property :month-end schedule))
                (month-end-string (if month-end (number-to-string month-end) "00"))
                (month-end-name (sync0-number-to-month month-end))
                (day-start-raw (org-element-property :day-start schedule))
                (day-start-string (number-to-string day-start-raw))
                (day-start-name   (calendar-day-name (list month-start day-start-raw year-start)))
                (day-start-name-abbrev   (calendar-day-name (list month-start day-start-raw year-start) t))
                (day-end-raw (org-element-property :day-end schedule))
                (day-end-string (when day-end-raw (number-to-string day-end-raw)))
                (day-end-name  (calendar-day-name (list month-end day-end-raw year-end)))
                (day-end-name-abbrev  (calendar-day-name (list month-end day-end-raw year-end) t))
                (schedule-start (concat year-start-string "-" month-start-string "-" day-start-string))
                (schedule-end (concat year-end-string "-" month-end-string "-" day-end-string))
                (hour-start (org-element-property :hour-start schedule))
                (hour-start-string (if hour-start (number-to-string hour-start) "0"))
                (hour-end (org-element-property :hour-end schedule))
                (hour-end-string (if hour-end (number-to-string hour-end) "0"))
                (minute-start (org-element-property :minute-start schedule))
                (minute-start-string (if minute-start (number-to-string minute-start) "0"))
                (minute-end (org-element-property :minute-end schedule)) 
                (minute-end-string (if minute-end (number-to-string minute-end) "0"))
                (time-start-raw (concat hour-start-string ":" minute-start-string))
                (time-start (cond
                             ((string= "0:0" time-start-raw)
                              "")
                             ((string= "0" minute-start-string) 
                              (concat hour-start-string "h"))
                             (t time-start-raw)))
                (time-end-raw (concat hour-end-string ":" minute-end-string))
                (time-end (cond
                           ((string= "0:0" time-end-raw)
                            "")
                           ((string= "0" minute-end-string) 
                            (concat hour-end-string "h"))
                           (t time-end-raw)))
                (fragment-start (cond 
                                 ((string= today schedule-start)
                                  "Today")
                                 ((string= tomorrow schedule-start)
                                  "Tomorrow")
                                 ((not (string= month-start-name month-end-name))
                                  (concat
                                   day-start-name-abbrev
                                   " "
                                   day-start-string)
                                  " "
                                  month-start-name)
                                 (t (concat 
                                     day-start-name-abbrev
                                     " "
                                     day-start-string))))
                ;; (fragment-start (cond 
                ;;                  ((string= today schedule-start)
                ;;                   "Today")
                ;;                  ((string= tomorrow schedule-start)
                ;;                   "Tomorrow")
                ;;                  (t (concat 
                ;;                      day-start-name-abbrev
                ;;                      " "
                ;;                      day-start-string)
                ;;                     (unless (string= month-start-name month-end-name)
                ;;                       (concat " " month-start-name)))))
                (fragment-end
                 ;; check: same day, start time, end time
                 (cond ((and (string= schedule-start schedule-end)
                             (not (string= time-start time-end))
                             (not (string= time-start ""))
                             (not (string= time-end "")))
                        (concat " " month-start-name-full ", " time-start "-" time-end))
                       ;; check: same day, start time
                       ((and (string= schedule-start schedule-end)
                             (not (string= time-start "")))
                        (concat " " month-start-name-full ", " time-start))
                       ;; check: same day 
                       ((string= schedule-start schedule-end)
                        (concat " " month-start-name-full))
                       ;; check: same month, start time, end time
                       ((and (string= month-start-name month-end-name)
                             (not (string= time-start ""))
                             (not (string= time-end "")))
                        (concat " - " day-end-string " " month-start-name-full ", " time-start "-" time-end))
                       ;; check: same month, start time 
                       ((and (string= month-start-name month-end-name)
                             (not (string= time-start "")))
                        (concat " - " day-end-string " " month-start-name-full ", " time-start))
                       ;; check: same month 
                       ((string= month-start-name month-end-name)
                        (concat " - " day-end-string " " month-start-name-full)) 
                       ;; check: start-time, end time
                       ((and (not (string= month-start-name month-end-name))
                             (not (string= time-start ""))
                             (not (string= time-end "")))
                        (concat " - " day-end-string " " month-end-name ", " time-start "-" time-end)) 
                       ;; check: start-time
                       ((and (not (string= month-start-name month-end-name))
                             (not (string= time-start "")))
                        (concat " - " day-end-string " " month-end-name ", " time-start)) 
                       (t (concat " " month-start-name-full))))
                (year (cond ((not (string= year-start-string this-year))
                             (concat " " year-start-string))
                            ((not (string= year-end-string this-year))
                             (concat " " year-end-string))
                            (t ""))))
           (concat fragment-start fragment-end year)))

        ;; If deadline has been defined:
        ((org-get-deadline-time (point))
         (let* ((deadline (org-get-deadline-time (point)))
                (deadline-date  (format-time-string "%Y-%m-%d" deadline))
                (today (format-time-string "%Y-%m-%d")) 
                (tomorrow (org-read-date nil nil "+1")) 
                (this-year  (format-time-string "%Y"))
                (year-end  (format-time-string "%Y" deadline))
                (month-end (capitalize (format-time-string "%B" deadline)))
                (day-end-raw (format-time-string "%d" deadline))
                (day-end (cond
                            ((string= today deadline-date)
                             "Today")
                            ((string= tomorrow deadline-date)
                             "Tomorrow")
                            (t (concat
                                ;; day name
                                (capitalize (format-time-string "%a." deadline))
                                " "
                                ;; number
                                (format-time-string "%d" deadline)))))
                (time-end (cond
                           ((string= "00:00" (format-time-string "%H:%M" deadline))
                            " ")
                           ((string= "00" (format-time-string "%M" deadline)) 
                            (format-time-string ", %Hh" deadline))
                           (t (format-time-string ", %H:%M" deadline))))
                (year (if (string= year-end this-year)
                          ""
                        (concat " " year-end))))
           (concat day-end " " month-end time-end year)))
        ;; If neither schedule nor deadline have been defined:
        (t " ")))

;; (defun sync0-org-agenda-get-project-timestamp-time ()
;;   "Get timestamp from current org-agenda time"
;;   (let* ((schedule (org-get-scheduled-time (point)))
;;          (deadline (org-get-deadline-time (point)))
;;          (schedule-date (when schedule (let ((time (capitalize (format-time-string "%a %d %b (%H:%M) %Y" schedule)))
;;                                              (hour (format-time-string "%H:%M" schedule))
;;                                              (time-no-hour (capitalize (format-time-string "%a %d %B %Y" schedule))))
;;                                          (if (not (string= "00:00" hour)) time time-no-hour))))
;;          ;; For the second block, I use "if" instead of "when" to print a
;;          ;; blank when neither "schedules" nor "deadlines" are set.
;;          (deadline-date (if deadline (let ((time (capitalize (format-time-string "%a %d %b (%H:%M) %Y" deadline)))
;;                                            (hour (format-time-string "%H:%M" deadline))
;;                                            (time-no-hour (capitalize (format-time-string "%a %d %B %Y" deadline))))
;;                                        (if (not (string= "00:00" hour)) time time-no-hour)) "")))
;;     (if schedule (princ schedule-date) (princ deadline-date))))

;; borrowed from emacs-leuven
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

;; taken from
;;; https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org
;; Make it easy to mark a task as done

(defun sync0-org-agenda-done (&optional arg)
  "Mark current TODO as done.
       This changes the line at point, all other lines in the agenda referring to
       the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "完"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'sync0-org-agenda-done)

;;; Make it easy to mark a task as done and create a follow-up task
(defun sync0-org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "完")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "F" 'sync0-org-agenda-mark-done-and-add-followup)

;;; Capture something based on the agenda
(defun sync0-org-agenda-new ()
  "Create a new note or task at the current agenda item.
       Creates it at the same level as the previous task, so it's better to use
       this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'sync0-org-agenda-new)


(defun sync0-org-agenda-print-parent-node ()
  "Output parent node in agenda views"
  (let ((path (car (last (org-get-outline-path nil t)))))
    (cond ((> (length path) 43) 
        (let ((start (substring path 0 25))
              (end (substring path -15 nil)))
          (concat "[ " start  "..." end " ]")))
      ((or (null path)
              (equal path ""))
"")
(t (concat "[ " path " ]")))))

(provide 'sync0-org-agenda-functions)
