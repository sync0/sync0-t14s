

;; necessary functions
(defun sync0-date-creator (year months)
  "year is number, months is a string"
  (let ((year-string (format "%s" year)))
    (cond ((strig= months "JanMars")
           (concat year-string  "-01/" year-string "-03"))
          ((strig= months "AvrJun")
           (concat year-string  "-04/" year-string "-06"))
          ((strig= months "JulSep")
           (concat year-string  "-07/" year-string "-09"))
          ((strig= months "OctDec")
           (concat year-string  "-10/" year-string "-12")))))

(defun sync0-repeat-string-for-list (n list)
  "create a big list that is an iterations of list n times"
  (interactive)
  (let (x)
    (cl-loop repeat n
             do
             (setq x (append list x))
             finally return x)))

;; end of necessary functions

    ;;          (while list
    ;;            (push (car list) x)
    ;;            (setq list (cdr list)))

    ;; (while list
    ;;   (setq x (concat (car list) sep x))
    ;;   (setq list (cdr list)))


(cl-loop for n from 1854 below 1866 collect n))

(let* ((years (cl-loop for n from 1854 below 1866 collect n))
       (year-total (length years)))
(message "%s" year-total))

(let* ((years (cl-loop for n from 1854 below 1866 collect n))
       (year-total (length years))
       (years-list (let (x)
              (dolist (element years x) 
                (push (format "%s" element) x)
                (push (format "%s" element) x)
                (push (format "%s" element) x)
                (push (format "%s" element) x))
              x)))
(sync0-show-elements-of-list years-list "\n"))


  (let (x)
    (cl-loop repeat voltotal
             do
            (concat file-base)

    (cl-loop repeat n
             do
             (setq x (append list x))
             finally return x)))

;; desired result


1855 T5 JanMars  :  JDE-1855-T5-JanMars.pdf
1855 T6 AvrJun  :  JDE-1855-T5-JanMars.pdf
1855 T7 JulSep  :  JDE-1855-T5-JanMars.pdf
1855 T8 OctDec  :  JDE-1855-T5-JanMars.pdf



;; my function to create jde volume entries that can then be
;; individually or in group create notes for or download the journal
;; entry in david hartman's page




;; success : this function is able to generate the ending names of the
;; urls necessary to download the files from the journal des
;; économistes !!! remaining !!! create the rest of the function to
;; create the individual entry for each of the volumes of the journal
;; des économistes, creating both the bibtex entry and the obsidian note.


  ;; (while years-list
  ;;     (setq x (concat file-base (car years-list)  x))
  ;;     (setq list (cdr list)))

    ;;          (while list
    ;;            (push (car list) x)
    ;;            (setq list (cdr list)))

    ;; (while list
    ;;   (setq x (concat (car list) sep x))
    ;;   (setq list (cdr list)))
