
;; (require 'cal-korea-x)
;; (defvar holiday-korean-holidays nil "Korean holidays")
(defvar holiday-french-holidays nil "French holidays")
(defvar holiday-colombian-holidays nil "Colombian holidays")

;; (setq holiday-korean-holidays
;;       '((holiday-fixed 1 1          "신정")
;;         (holiday-lunar-ko 1 nil 1   "설날" -1)
;;         (holiday-lunar-ko 1 nil 1   "설날")
;;         (holiday-lunar-ko 1 nil 1   "설날" 1)
;;         (holiday-fixed 3 1          "3.1절")
;;         (holiday-lunar-ko 4 nil 8   "석가탄신일")
;;         (holiday-fixed 5 5          "어린이날")
;;         (holiday-fixed 6 6          "현충일")
;;         (holiday-fixed 8 15         "광복절")
;;         (holiday-fixed 10 3         "개천절")
;;         (holiday-fixed 10 9         "한글날")
;;         (holiday-lunar-ko 8 nil 15  "추석" -1)
;;         (holiday-lunar-ko 8 nil 15  "추석")
;;         (holiday-lunar-ko 8 nil 15  "추석" 1)
;;         (holiday-fixed 12 25        "성탄절")))

(setq holiday-french-holidays
      '((holiday-fixed 1 1 "Jour de l'an")
        (holiday-fixed 1 6 "Épiphanie")
        (holiday-fixed 2 2 "Chandeleur")
        (holiday-fixed 2 14 "Saint Valentin")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
        (holiday-fixed 6 21 "Fête de la musique")
        (holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
        (holiday-fixed 8 15 "Assomption (Religieux)")
        (holiday-fixed 11 11 "Armistice de 1918")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 11 2 "Commémoration des fidèles défunts")
        (holiday-fixed 12 25 "Noël")
        ;; fetes a date variable
        (holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc -47 "Mardi gras")
        (holiday-float 5 0 4 "Fête des mères")
        ;; dernier dimanche de mai ou premier dimanche de juin si c'est le
        ;; même jour que la pentecôte TODO
        (holiday-float 6 0 3 "Fête des pères")))
;; troisième dimanche de juin

(setq holiday-colombian-holidays 
      '((holiday-fixed 1 1 "Año nuevo")
        (holiday-sexp '(calendar-nth-named-day 1 1 1 year 6) "Día de Reyes")
        (holiday-sexp '(calendar-nth-named-day 1 1 3 year 19) "Día de San José")
        (holiday-easter-etc -3 "Jueves Santo")
        (holiday-easter-etc -2 "Viernes Santo")
        (holiday-fixed 5 1 "Día del trabajo")
        (holiday-easter-etc +43 "Día de la ascención")
        (holiday-sexp '(calendar-nth-named-day 1 1 6 year 29)
                      "San Pedro y San Pablo")
        (holiday-easter-etc +64 "Corpus Christi")
        (holiday-easter-etc +71 "Sagrado corazón")
        (holiday-fixed 7 20 "Día de la independencia")
        (holiday-fixed 8 7 "Batalla de Boyacá")
        (holiday-sexp '(calendar-nth-named-day 1 1 8 year 15)
                      "Asunción de la virgen")
        (holiday-sexp '(calendar-nth-named-day 1 1 10 year 12) "Día de la raza")
        (holiday-sexp '(calendar-nth-named-day 1 1 11 year 1)
                      "Todos los santos")
        (holiday-sexp '(calendar-nth-named-day 1 1 11 year 11)
                      "Independencia de Cartagena")
        (holiday-fixed 12 25 "Navidad")
        (holiday-fixed 12 8 "Inmaculada concepción")))

;; (setq holiday-other-holidays
;;       (append holiday-colombian-holidays holiday-french-holidays holiday-korean-holidays))

(setq holiday-other-holidays
      (append holiday-colombian-holidays holiday-french-holidays))

(setq calendar-holidays
      (append holiday-general-holidays holiday-other-holidays))

(provide 'sync0-holidays)
