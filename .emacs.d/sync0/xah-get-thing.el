;;; xah-get-thing.el --- get thing or selection at point. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2011, 2024 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.12.20241007133248
;; Created: 2015-05-22
;; Package-Requires: ((emacs "27"))
;; Keywords: extensions, lisp, tools
;; License: GPL v2. Tell your friends to buy a copy.
;; URL: http://xahlee.info/emacs/emacs/elisp_get-selection-or-unit.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides the following functions

;; xah-get-pos-of-block ()
;; xah-get-pos-of-block-or ()
;; xah-get-pos-of-filepath ()
;; xah-get-pos-of-filepath-or ()
;; xah-get-pos-of-glyphs ()
;; xah-get-pos-of-glyphs-or ()
;; xah-get-pos-of-url ()
;; xah-get-pos-of-url-or ()

;; xah-get-bounds-of-thing (Unit)
;; xah-get-bounds-of-thing-or-region (Unit)

;; xah-get-bounds-of-block ()
;; xah-get-bounds-of-block-or-region ()

;; xah-get-thing-at-point (Unit)
;; xah-get-thing-or-region (Unit)

;; names starting with xah-get-pos return a vector, so you can use it with seq-setq and seq-let.
;; names starting with xah-get-thing return a cons pair, compatible with thing-at-point.

;; This package is similar to emac's builtin `thing-at-point' in package thingatpt.el.

;; The main differences are:

;; • Is not based on syntax table. So the returned syntactic unit are predicable in any major mode.
;; • provides the 'block, which is similar to emacs's 'paragraph, but strictly defined by between blank lines.
;; • functions names ending in -or -or-region return the boundary of region, if active.
;; • Thing 'url and 'filepath, are different from how thingatpt.el determines them, and, again, is not based on syntax table, but based on regex of likely characters. Also, result is never modified version of what's in the buffer. For example, if 'url, the http prefix is not automatically added if it doesn't exist in buffer.
;; • Thing 'line never includes newline character. This avoid inconsistency when line is last line.

;; Home page: http://xahlee.info/emacs/emacs/elisp_get-selection-or-unit.html

;;; Install:

;; To install manually, place this file in the directory ~/.emacs.d/lisp/
;; Then, add the following in your emacs lisp init:
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; Then, in elisp code where you want to use it, add
;; (require 'xah-get-thing)

;;; HISTORY

;; xah-get-thing-at-cursor (deprecated), xah-get-thing-or-selection (deprecated)
;; 2015-05-22 changes won't be logged here anymore, unless incompatible ones.
;; version 1.0, 2015-05-22 was {unit-at-cursor, get-selection-or-unit} from xeu_elisp_util.el

;; HHHH---------------------------------------------------
;;; Code:

(defun xah-get-bounds-of-thing (Unit)
  "Return the boundary of Unit under cursor.
Return a cons cell (START . END).
Unit can be:
• 'word → sequence of 0 to 9, A to Z, a to z, and hyphen.
• 'glyphs → sequence of visible glyphs. Useful for file name, URL, …, anything doesn't have white spaces in it.
• 'line → delimited by “\\n”. (captured text does not include “\\n”.)
• 'block → delimited by empty lines or beginning/end of buffer. Lines with just spaces or tabs are also considered empty line. (captured text does not include a ending “\\n”.)
• 'buffer → whole buffer. (respects `narrow-to-region')
• 'filepath → delimited by chars that's usually not part of filepath.
• 'url → delimited by chars that's usually not part of URL.
• 'inDoubleQuote → between double quote chars.
• 'inSingleQuote → between single quote chars.
• a vector [beginRegex endRegex] → The elements are regex strings used to determine the beginning/end of boundary chars. They are passed to `skip-chars-backward' and `skip-chars-forward'. For example, if you want paren as delimiter, use [\"^(\" \"^)\"]

This function is similar to `bounds-of-thing-at-point'.
The main difference are:

• This function's behavior does not depend on syntax table. e.g. for Units 「'word」, 「'block」, etc.
• 'line always returns the line without end of line character, avoiding inconsistency when the line is at end of buffer.
• Support certain “thing” such as 'glyphs that's a sequence of chars. Useful as file path or url in html links, but do not know which before hand.
• Some “thing” such 'url and 'filepath considers strings that at usually used for such. The algorithm that determines this is different from thing-at-point.

Created: 2017-05-27
Version: 2022-07-24"
  (let ((xp0 (point)) xbeg xend)
    (save-excursion
      (cond
       ((eq Unit 'block)
        (progn
          (setq xbeg (if (re-search-backward "\n[ \t]*\n" nil "move")
                        (goto-char (match-end 0))
                      (point)))
          (setq xend (if (re-search-forward "\n[ \t]*\n" nil "move")
                        (match-beginning 0)
                      (point)))))

       ((eq Unit 'filepath)
        (let ((xdelimiters "^  \t\n\"`'|[]{}<>‘’“”「」〔〕〈〉《》【】〖〗«»‹›·。\\`"))
          (skip-chars-backward xdelimiters)
          (setq xbeg (point))
          (goto-char xp0)
          (skip-chars-forward xdelimiters)
          (setq xend (point))))

       ((eq Unit 'url)
        (let ((xdelimiters "^  \t\n\"`'|()[]{}<>‘’“”。\\"))
          (skip-chars-backward xdelimiters)
          (setq xbeg (point))
          (goto-char xp0)
          (skip-chars-forward xdelimiters)
          (setq xend (point))))

       ((eq Unit 'inDoubleQuote)
        (progn
          (skip-chars-backward "^\"")
          (setq xbeg (point))
          (goto-char xp0)
          (skip-chars-forward "^\"")
          (setq xend (point))))

       ((eq Unit 'inSingleQuote)
        (progn
          (skip-chars-backward "^\"")
          (setq xbeg (point))
          (goto-char xp0)
          (skip-chars-forward "^\"")
          (setq xend (point))))

       ((vectorp Unit)
        (progn
          (skip-chars-backward (elt Unit 0))
          (setq xbeg (point))
          (goto-char xp0)
          (skip-chars-forward (elt Unit 1))
          (setq xend (point))))

       ((eq Unit 'word)
        (let ((wordcharset "-A-Za-z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ"))
          (skip-chars-backward wordcharset)
          (setq xbeg (point))
          (goto-char xp0)
          (skip-chars-forward wordcharset)
          (setq xend (point))))

       ((eq Unit 'glyphs)
        (progn
          (skip-chars-backward "[:graph:]")
          (setq xbeg (point))
          (skip-chars-forward "[:graph:]")
          (setq xend (point))))

       ((eq Unit 'buffer)
        (progn
          (setq xbeg (point-min))
          (setq xend (point-max))))

       ((eq Unit 'line)
        (progn
          (setq xbeg (line-beginning-position))
          (setq xend (line-end-position))))))
    (cons xbeg xend)))

(defun xah-get-bounds-of-thing-or-region (Unit)
  "If region is active, return its boundary, else same as `xah-get-bounds-of-thing'.
Created: 2016-10-18
Version: 2021-08-11"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-thing Unit)))

(defun xah-get-thing-or-region (Unit)
  "If region is active, return its boundary, else return the thing at point.
See `xah-get-bounds-of-thing' for Unit.
Version: 2021-08-11"
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((xbds (xah-get-bounds-of-thing Unit)))
      (buffer-substring-no-properties (car xbds) (cdr xbds)))))

(defun xah-get-thing-at-point (Unit)
  "Return the thing at point.
See `xah-get-bounds-of-thing' for Unit.
Created: 2016-10-18
Version: 2021-08-11"
  (let ((xbds (xah-get-bounds-of-thing Unit)))
    (buffer-substring-no-properties (car xbds) (cdr xbds))))

(defun xah-get-bounds-of-block ()
  "Return the boundary (START . END) of current block.
Version: 2021-08-12"
  (let (xbeg xend (xblankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq xbeg (if (re-search-backward xblankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq xend (if (re-search-forward xblankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons xbeg xend)))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else same as `xah-get-bounds-of-block'.
Version: 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))

;; HHHH---------------------------------------------------
;; new 2024-03-23

(defun xah-get-pos-of-block ()
  "Return the begin end positions of current text block.
Return value is a `vector'.
Text block is group of lines separated by blank lines.

URL `http://xahlee.info/emacs/emacs/elisp_get_text_block.html'
Created: 2024-03-23
Version: 2024-10-07"
  (let (xbeg xend (xp (point)))
    (save-excursion
      (setq xbeg (if (re-search-backward "\n[ \t]*\n" nil 1) (match-end 0) (point)))
      (goto-char xp)
      (setq xend (if (re-search-forward "\n[ \t]*\n" nil 1) (match-beginning 0) (point))))
    (vector xbeg xend)))

(defun xah-get-pos-of-block-or ()
  "Return the begin end positions of current text block or active region.
Return value is a `vector'.

URL `http://xahlee.info/emacs/emacs/elisp_get_text_block.html'
Created: 2024-03-23
Version: 2024-03-23"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-block)))

;; HHHH---------------------------------------------------

(defun xah-get-pos-of-word ()
  "Return the [begin end] positions of any word.
Word is a to z plus hyphen. the a to z may be any accented unicode char.
Return value is a `vector'.
Created: 2024-03-29
Version: 2024-05-03"
  (let (xbeg xend (xword "-A-Za-zßáàâäāǎãåąăạảảấầẩẫậắằẳặæçčćéèêëēěęẹẻẽếềểễệíìîïīǐỉịñňńóòôöõǒøōồơọỏốổỗộớờởợúùûüūũưụủứừửữựýÿỳỷỹþďðđĩľĺłřŕšśťžźż"))
    (save-excursion
      (progn
        (skip-chars-backward xword)
        (setq xbeg (point))
        (skip-chars-forward xword)
        (setq xend (point))))
    (vector xbeg xend)))

(defun xah-get-pos-of-word-or ()
  "If region is active, return its [begin end] positions, else same as `xah-get-pos-of-word'.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-03-23"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-word)))

;; HHHH---------------------------------------------------

(defun xah-get-pos-of-symbol ()
  "Return the [begin end] positions of symbol.
Symbol is identifier chars, defined in current major mode's syntax table.
Return value is a `vector'.
Created: 2024-03-30
Version: 2024-04-04"
  (let ((xbd (bounds-of-thing-at-point 'symbol)))
    (if xbd
        (vector (car xbd) (cdr xbd))
      (vector (point) (point)))))

(defun xah-get-pos-of-symbol-or ()
  "If region is active, return its [begin end] positions, else same as `xah-get-pos-of-symbol'.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-03-30"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-symbol)))

;; HHHH---------------------------------------------------

(defun xah-get-pos-of-glyphs ()
  "Return the [begin end] positions of any visible glyphs sequence.
This is similar to word, but include all visible graphemes.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-03-23"
  (let (xbeg xend (xglyphs "[:graph:]"))
    (save-excursion
      (progn
        (skip-chars-backward xglyphs)
        (setq xbeg (point))
        (skip-chars-forward xglyphs)
        (setq xend (point))))
    (vector xbeg xend)))

(defun xah-get-pos-of-glyphs-or ()
  "If region is active, return its [begin end] positions, else same as `xah-get-pos-of-glyphs'.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-03-23"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-glyphs)))

;; HHHH---------------------------------------------------

(defun xah-get-pos-of-url ()
  "Return the [begin end] positions of URL.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-09-14"
  (let (xbeg xend (xp0 (point)) (xdelimiters "^  \t\n\"`'|()[]{}<>‘’“”「」〔〕〈〉《》【】〖〗«»‹›❮❯ ❰❱\\"))
    (save-excursion
      (progn
        (skip-chars-backward xdelimiters)
        (setq xbeg (point))
        (goto-char xp0)
        (skip-chars-forward xdelimiters)
        (setq xend (point))))
    (vector xbeg xend)))

(defun xah-get-pos-of-url-or ()
  "If region is active, return its [begin end] positions, else same as `xah-get-pos-of-url'.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-03-23"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-url)))

;; HHHH---------------------------------------------------

(defun xah-get-pos-of-filepath ()
  "Return the [begin end] positions of filepath.
Return value is a `vector'.

Created: 2024-03-23
Version: 2024-09-01"
  (let (xbeg xend (xp0 (point)) (xdelimiters "^  \t\n\"`'|[]{}<>‘’“”「」〔〕〈〉《》【】〖〗«»‹›❮❯❰❱·。\\`"))
    (save-excursion
      (progn
        (skip-chars-backward xdelimiters)
        (setq xbeg (point))
        (goto-char xp0)
        (skip-chars-forward xdelimiters)
        (setq xend (point))))
    (vector xbeg xend)))

(defun xah-get-pos-of-filepath-or ()
  "If region is active, return its [begin end] positions, else same as `xah-get-pos-of-filepath'.
Return value is a `vector'.
Created: 2024-03-23
Version: 2024-03-23"
  (if (region-active-p)
      (vector (region-beginning) (region-end))
    (xah-get-pos-of-filepath)))

;; HHHH---------------------------------------------------

(provide 'xah-get-thing)

;;; xah-get-thing.el ends here
