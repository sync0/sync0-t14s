;;; guess-language-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "guess-language" "guess-language.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from guess-language.el

(autoload 'guess-language-mode "guess-language" "\
Toggle guess-language mode.

If called interactively, enable Guess-Language mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Guess-language is a buffer-local minor mode.  It guesses the
language of the current paragraph when flyspell detects an
incorrect word and changes ispell's dictionary and typo-mode
accordingly.  If the language settings change, flyspell is rerun
on the current paragraph.  If the paragraph is shorter than
`guess-language-min-paragraph-length', none of the above happens
because there is likely not enough text to guess the language
correctly.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "guess-language" '("guess-language")))

;;;***

(provide 'guess-language-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; guess-language-autoloads.el ends here
