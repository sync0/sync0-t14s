(defmacro sync0-redefine (symbol value)
  `(setf ,symbol ,value))

(defmacro sync0-nullify-variable (var)
  "Make target variable nil"
  `(setf ,var nil))

(defun sync0-nullify-variable-list (varlist)
  "Set all variables from varlist nil"
  (mapc #'(lambda (a) (set a nil)) varlist))

(provide 'sync0-macros)
