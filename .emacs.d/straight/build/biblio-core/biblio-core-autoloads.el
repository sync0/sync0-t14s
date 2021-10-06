;;; biblio-core-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "biblio-core" "biblio-core.el" (0 0 0 0))
;;; Generated autoloads from biblio-core.el

(autoload 'biblio-lookup "biblio-core" "\
Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`biblio-backends'.  Returns the buffer in which results will be
inserted.

\(fn &optional BACKEND QUERY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-core" '("biblio-")))

;;;***

(provide 'biblio-core-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; biblio-core-autoloads.el ends here
