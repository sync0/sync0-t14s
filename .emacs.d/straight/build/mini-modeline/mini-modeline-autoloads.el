;;; mini-modeline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "mini-modeline" "mini-modeline.el" (0 0 0 0))
;;; Generated autoloads from mini-modeline.el

(defvar mini-modeline-mode nil "\
Non-nil if Mini-Modeline mode is enabled.
See the `mini-modeline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mini-modeline-mode'.")

(custom-autoload 'mini-modeline-mode "mini-modeline" nil)

(autoload 'mini-modeline-mode "mini-modeline" "\
Enable modeline in minibuffer.

If called interactively, enable Mini-Modeline mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mini-modeline" '("mini-modeline-")))

;;;***

(provide 'mini-modeline-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mini-modeline-autoloads.el ends here
