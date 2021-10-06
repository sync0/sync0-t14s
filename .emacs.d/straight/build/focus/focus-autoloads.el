;;; focus-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "focus" "focus.el" (0 0 0 0))
;;; Generated autoloads from focus.el

(autoload 'focus-mode "focus" "\
("Dim the font color of text in surrounding sections.

  If called interactively, enable Focus mode if ARG is positive,
  and disable it if ARG is zero or negative.  If called from
  Lisp, also enable the mode if ARG is omitted or nil, and toggle
  it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" 53 55 (face default) 118 120 (face default) 179 181 (face default) 245 247 (face default)) t nil)

(autoload 'focus-read-only-mode "focus" "\
("A read-only mode optimized for `focus-mode'.

  If called interactively, enable Focus-Read-Only mode if ARG is
  positive, and disable it if ARG is zero or negative.  If called
  from Lisp, also enable the mode if ARG is omitted or nil, and
  toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" 46 48 (face default) 111 113 (face default) 177 179 (face default) 241 243 (face default)) t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "focus" '("focus-")))

;;;***

(provide 'focus-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; focus-autoloads.el ends here
