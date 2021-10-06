;;; smart-quotes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "smart-quotes" "smart-quotes.el" (0 0 0 0))
;;; Generated autoloads from smart-quotes.el

(defvar smart-quotes-mode nil "\
Toggle smart-quotes-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `smart-quotes-mode'.")

(custom-autoload 'smart-quotes-mode "smart-quotes" nil)

(autoload 'smart-quotes-mode "smart-quotes" "\
Minor mode that makes the ' and \" keys insert left and right
quotation marks automatically according to the context before point;
see `smart-quotes-insert-single' and `smart-quotes-insert-double'.
With no argument, this command toggles Smart Quotes mode.
With a prefix argument ARG, turn Smart Quotes minor mode on if ARG
is positive, otherwise turn it off.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smart-quotes "smart-quotes" "\
Unconditionally turn on Smart Quotes mode." nil nil)

(autoload 'turn-off-smart-quotes "smart-quotes" "\
Unconditionally turn off Smart Quotes mode." nil nil)

(autoload 'smart-quotes-smarten "smart-quotes" "\
Turn quotes into smart quotes in region or buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "smart-quotes" '("smart-quotes-")))

;;;***

(provide 'smart-quotes-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-quotes-autoloads.el ends here
