;;; taken from http://xahlee.info/emacs/emacs/elisp_straight_curly_quotes.html

(require 'xah-replace-pairs)
(require 'xah-get-thing)

(defun xah-ascii-to-math-symbol (&optional Begin End)
  "Replace ASCII less-or-equal etc to unicode chars, on text block or region.

Example:
<= → ≤
>= → ≥
---> → ⟶
--> → ⟶
-- → —
~= → ≈
Each of these on the left must have a space around them.

URL `http://xahlee.info/emacs/emacs/elisp_straight_curly_quotes.html'
Version: 2019-07-25 2021-08-17 2023-11-26"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (let ((case-fold-search nil))
      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)
          (xah-replace-pairs-region
           (point-min) (point-max)
           [
            [" <= " " ≤ "]
            [" >= " " ≥ "]
            [" ---> " " ⟶ "]
            [" --> " " ⟶ "]
            ["--" " — "]
            ["~=" "≈"]
            ] t t)
          ;;
          )))))

(defun xah-prettify-punctuations (&optional Begin End)
  "Replace ASCII punctuations etc to Unicode symbols.

Example:
 -- → —
 & → ＆
 ... → …
 :) → 😊

URL `http://xahlee.info/emacs/emacs/elisp_straight_curly_quotes.html'
Version: 2019-07-25 2021-08-17 2023-11-26"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (let ((case-fold-search nil))
      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)
          (xah-replace-pairs-region
           (point-min) (point-max)
           [
            ["  —  " " — "] ; rid of extra space in em-dash
            ["--" "–"] ; replace with en dashes
            ["---" "—"] ; replace with em dashes
            ["..." "…"]
            ;; [".." "…"]
            ;; [" & " " ＆ "]
            ;; [" :)" " 😊"]
            ;; [" :(" " ☹"]
            ;; [" ;)" " 😉"]
            [" , " ", "]
            [" . " ". "]
            ;; ["—" " — "]
            ] t t)
          ;;
          )))))

(defun xah-fix-curly-single-quote-to-apostrophe (&optional Begin End)
  "Replace RIGHT SINGLE QUOTATION MARK to APOSTROPHE.

Example:
don’t  → don't
i’ve → i've
it’s → it's

URL `http://xahlee.info/emacs/emacs/elisp_straight_curly_quotes.html'
Version: 2019-07-25 2021-08-17 2023-01-04 2023-11-26"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (let ((case-fold-search nil))
      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)

          ;; (xah-replace-pairs-region
          ;;  (point-min) (point-max)
          ;;  [
          ;;   [">\'" ">‘"]
          ;;   [" \'" " ‘"]
          ;;   ["(\'" "(‘"]

          ;;   ["\' " "’ "]
          ;;   ["\'," "’,"]
          ;;   [".\'" ".’"]
          ;;   ["!\'" "!’"]
          ;;   ["?\'" "?’"]
          ;;   ["\')" "’)"]
          ;;   ["\']" "’]"]
          ;;   ] t t)

          (xah-replace-regexp-pairs-region
           (point-min) (point-max)
           [
            ["\\bcan’t\\b" "can't"]
            ["\\bdon’t\\b" "don't"]
            ["\\bdoesn’t\\b" "doesn't"]
            ["\\bwon’t\\b" "won't"]
            ["\\bisn’t\\b" "isn't"]
            ["\\baren’t\\b" "aren't"]
            ["\\bain’t\\b" "ain't"]
            ["\\bdidn’t\\b" "didn't"]
            ["\\baren’t\\b" "aren't"]
            ["\\bwasn’t\\b" "wasn't"]
            ["\\bweren’t\\b" "weren't"]
            ["\\bcouldn’t\\b" "couldn't"]
            ["\\bshouldn’t\\b" "shouldn't"]

            ["\\b’ve\\b" "'ve"]
            ["\\b’re\\b" "'re"]
            ["\\b‘em\\b" "'em"]
            ["\\b’ll\\b" "'ll"]
            ["\\b’m\\b" "'m"]
            ["\\b’d\\b" "'d"]
            ["\\b’s\\b" "'s"]
            ["s’ " "s' "]
            ["s’\n" "s'\n"]
            ] t t :hilight))))))

(defun xah-fix-double-quote-to-curly (&optional Begin End)
  "Change straight double quotes to curly.
This is a heuristic based algo, result can be wrong in some places.

Version: 2023-01-04 2023-06-05 2023-11-26"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (let ((case-fold-search nil))
      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)
          (xah-replace-regexp-pairs-region
           (point-min) (point-max)
           [ ["^\"" "“"]
             ["\"\\'" "”"]
             ] t t)
          (xah-replace-pairs-region
           (point-min) (point-max)
           [
            ["\n\"" "\n“"]
            [">\"" ">“"]
            ["(\"" "(“"]
            [" \"" " “"]
            ["\" " "” "]
            ["\"," "”,"]
            ["\",\n" "”,\n"]
            ["\". " "”. "]
            ["\".\n" "”.\n"]
            ["\"." "”."]
            ["\"?" "”?"]
            ["\";" "”;"]
            ["\":" "”:"]
            ["\")" "”)"]
            ["\"]" "”]"]
            ;; ["\"[" "\”["]
            [".\"" ".”"]
            [",\"" ",”"]
            ["!\"" "!”"]
            ["?\"" "?”"]
            ["\"<" "”<"]
            ;; ["\"\n" "”\n"]
            ] t t))))))

(defun xah-fix-double-quote-to-curly (&optional Begin End)
  "Change straight double quotes to curly.
This is a heuristic based algo, result can be wrong in some places.

Version: 2023-01-04 2023-06-05 2023-11-26"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (let ((case-fold-search nil))
      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)
          (xah-replace-regexp-pairs-region
           (point-min) (point-max)
           [ ["^\"" "“"]
             ["\"\\'" "”"]
             ] t t)
          (xah-replace-pairs-region
           (point-min) (point-max)
           [
            ["\n\"" "\n“"]
            [">\"" ">“"]
            ["(\"" "(“"]
            [" \"" " “"]
            ["\" " "” "]
            ["\"," "”,"]
            ["\",\n" "”,\n"]
            ["\". " "”. "]
            ["\".\n" "”.\n"]
            ["\"." "”."]
            ["\"?" "”?"]
            ["\";" "”;"]
            ["\":" "”:"]
            ["\")" "”)"]
            ["\"]" "”]"]
            ;; ["\"[" "\”["]
            [".\"" ".”"]
            [",\"" ",”"]
            ["!\"" "!”"]
            ["?\"" "?”"]
            ["\"<" "”<"]
            ;; ["\"\n" "”\n"]
            ] t t))))))

(defun xah-replace-straight-quotes (&optional Begin End)
  "Replace straight double quotes to curly ones, and others.
Works on current text block or selection.

Examples of changes:
 「\"…\"」 → 「“…”」
 「...」 → 「…」
 「I’m」 → 「I'm」
 「--」 → 「—」
 「~=」 → 「≈」

When called in lisp code, Begin and End are region begin/end positions.

WARNING: this command does not guarantee 100% correct conversion of quotes, because it impossible. You should double check highlighted places after.

URL `http://xahlee.info/emacs/emacs/elisp_straight_curly_quotes.html'
Version: 2015-04-29 2021-08-17"
  ;; some examples for debug
  ;; do "‘em all -- done..."
  ;; I’am not
  ;; said "can’t have it, can’t, just can’t"
  ;; ‘I’ve can’t’
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (let ((case-fold-search nil))
      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)
          (xah-prettify-punctuations (point-min) (point-max))
          (xah-ascii-to-math-symbol (point-min) (point-max))
          (xah-fix-double-quote-to-curly (point-min) (point-max))
          (xah-fix-curly-single-quote-to-apostrophe (point-min) (point-max)))))))

(provide 'sync0-corrections)
