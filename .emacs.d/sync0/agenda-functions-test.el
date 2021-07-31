(cond ((and (org-get-scheduled-time (point))
            (org-get-deadline-time (point)))
       (let* ((schedule (org-get-scheduled-time (point)))
              (schedule-string  (format-time-string "%Y-%m-%d" schedule))
              (tomorrow-string (org-read-date nil nil "+1")) 
              (month-start (capitalize (format-time-string "%b" schedule)))
              (day-start
               (if (string= tomorrow-string schedule-string)
                   "Demain" 
                 (concat
                  ;; day name
                  (capitalize (format-time-string "%a" schedule))
                  " "
                  ;; number
                  (format-time-string "%d" schedule))))
              (time-start (cond
                           ((string= "00:00" (format-time-string "%H:%M" schedule))
                            "")
                           ((string= "00" (format-time-string "%M" schedule)) 
                            (format-time-string "%Hh" schedule))
                           (t (format-time-string "%H:%M" schedule))))
              (deadline (org-get-deadline-time (point)))
              (deadline-string  (format-time-string "%Y-%m-%d" deadline))
              (month-end (capitalize (format-time-string "%b" deadline)))
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
              (fragment-start 
               (concat
                day-start
                (unless (string= month-start month-end)
                  (concat " " month-start-name))
                (unless (string= time-start "")
                  (concat ", " time-start))))
              (fragment-end
               ;; first check whether deadline and schedule fall on the same day
               (cond ((and (string= schedule-string deadline-string)
                           (not (string= time-end "")))
                      (concat "-" time-end))
                     ;; ((string= schedule-string deadline-string)
                     ;;   (concat "-" time-end))
                     ((and (string= month-start month-end)
                           (not (string= time-end "")))
                      (concat "-" day-end month-end ", " time-end))
                     ((and (string= month-start month-end)
                           (string= time-end ""))
                      (concat "-" day-end month-end)))))
         ;; Stylize the date output
         (concat fragment-start fragment-end)





(let* ((schedule (org-get-scheduled-time (point)))
       (tomorrow-string (org-read-date nil nil "+1")) 
       (schedule-string (format-time-string "%Y-%m-%d" schedule))
       ;; Define the object "scheduled" containing the date
       ;; information from which all the other variables wiil be
       ;; defined.
       (element (org-element-at-point))
       (scheduled (org-element-property :scheduled element))
       (year-start-string (number-to-string
                           (org-element-property :year-start scheduled)))  
       (year-end-string (if-let
                            ((year-end (org-element-property :year-end scheduled)))
                            (number-to-string year-end)
                          year-start-string))
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
       (day-start-name   (calendar-day-name (list month-start day-start year-start)))
       (day-start-name-abbrev   (calendar-day-name (list month-start day-start year-start) t))
       (day-start
      (concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start "-" time-end)))
      (concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name)))
        (if (string= tomorrow-string schedule-string)
            "Demain" 
          (concat
           ;; day name
           (capitalize (format-time-string "%a" schedule))
           " "
           ;; number
           (format-time-string "%d" schedule))))
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
       ;; (time-start (cond
       ;;              ((string= "00:00" (format-time-string "%H:%M" schedule))
       ;;               "")
       ;;              ((string= "00" (format-time-string "%M" schedule)) 
       ;;               (format-time-string "%Hh" schedule))
       ;;              (t (format-time-string "%H:%M" schedule))))
       (time-start-test (concat hour-start-string ":" minute-start-string))
       (time-start (if (string= "0" minute-start-string) 
                       (if time-end (concat hour-start-string "")
                         (concat hour-start-string "h"))
                     (concat hour-start-string ":" minute-start-string)))
       )
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


 ;; (unless (and (string= this-month-name month-start-name)
 ;;              (string= tomorrow-string schedule-string)))

 ;; (unless (and (string= this-month-name month-start-name)
 ;;              (string= tomorrow-string schedule-string))

;; double checked: good
(fragment-start
 (concat
  (if (string= tomorrow-string schedule-string)
      "Demain"
    (concat 
     day-start-name-abbrev
     " "
     day-start-string))
  (unless (string= month-start-name month-end-name)
    (concat " " month-start-name))
 (unless (string= time-start-test "0:0")
(concat ", " time-start))))

(fragment-end
 ;; first check whether deadline and schedule fall on the same day
 (cond ((and (string= schedule-string deadline-string)
             (not (string= time-end "")))
        (concat "-" time-end))
       ;; ((string= schedule-string deadline-string)
       ;;   (concat "-" time-end))
       ((and (string= month-start month-end)
             (not (string= time-end "")))
        (concat "-" day-end month-end ", " time-end))
       ((and (string= month-start month-end)
             (string= time-end ""))
        (concat "-" day-end month-end))
       (t "")))

 (unless (string= this-year year-end))







"-"
(end-part 
(concat
 (if (string= day-start-string day-end-string))
      ""
    (concat 
     day-end-name-abbrev
     " "
     day-end-string))
  " "
    month-end-name
 (unless (string= time-end-test "0:0")

(concat ", " time-end)))

(concat ", " time-start "-" time-end)


(concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)

;;  (cond ((and (string= time-start-test "0:0")
;;          (not (string= day-start-string day-end-string)))
;;          (concat ", " time start-time-end)
;;        ((string= time-start-test "0:0")

;; (concat ", " time-end))))



  ;; same-month, different-day, time-start, time-end
 ;; ", " time-start "-" time-end)

;; " - " day-end-name-abbrev " " day-end-string " " month-end-name)))))


;; "Demain, " time-start " " day-end-string " " month-end-name ", " time-end)



;; (concat beggining-part ", " time-start "-" time-end)



;; different-month, different-day, time-start, time-end
;; (concat day-start-string " " month-start-name ", " time-start " - " day-end-string " " month-end-name ", " time-end)

;; (concat [day-start-name-abbrev " " day-start-string " " month-start-name]
;;         ", " time-start "-" time-end)


;; (start-month
;; (if (string= month-start-name month-end-name)
;; month-start-name
;; month-start-name-full
;; ))
;; (end-month
;; (if (string= month-start-name month-end-name)
;; month-start-name
;; month-end-name))



;; day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full

;; ["Demain"|day-start-name-abbrev day-start-string $ month-start? (if dif month) time-start day-end-string month-start|end]


;; (concat day-start-name-abbrev " " day-start-string $ " " month-start-name " - " day-end-name-abbrev " " day-end-string " " month-end-name)



;; "Demain," time-start "-" time-end
;;  day-start-string "-" day-end-string " " month-start-name
;; "Demain - " day-end-string ", " time-start "-" time-end

;; (concat "Demain, " time-start "-" time-end)
;; (concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)
;; (concat "Demain - " day-end-string ", " time-start "-" time-end)
;; same-month, different-day, time-start
(concat day-start-string "-" day-end-string " " month-start-name ", " time-start "-" time-end)
(concat "Demain - " day-end-string " " month-start-name-full)
(concat day-start-name-abbrev " " day-start-string "-" day-end-string " " month-start-name-full)
(concat "Demain, " time-start "-" time-end)
;; same-month, same-day, time-start, time-end 
(concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start "-" time-end)
(concat "Demain, " time-start)
;; same-month, same-day, time-start
(concat day-start-name-abbrev " " day-start-string " " month-start-name ", " time-start)
(concat "Demain")
;; same-month, same-day, same-year
(concat day-start-name-abbrev " " day-start-string " " month-start-name-full)
(concat "Demain, " time-start " " day-end-string " " month-end-name ", " time-end)
;; different-month, different-day, time-start, time-end
(concat day-start-string " " month-start-name ", " time-start " - " day-end-string " " month-end-name ", " time-end)
(concat "Demain, " time-start " - " day-end-string " " month-end-name)
;; different-month, different-day, time-start
(concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name)
(concat "Demain, " time-start " - " day-end-string " " month-end-name)
;; different-month, different-day, time-start
(concat day-start-string " " month-start-name ", " time-start " " day-end-string " " month-end-name)
;; different-month, different-day
