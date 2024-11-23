(require 'abbrev)

;; ;; Define minor modes for each language with ispell dictionary and input method
;; (defvar english-mode-abbrev-table nil "Abbreviation table for English.")
;; (defvar spanish-mode-abbrev-table nil "Abbreviation table for Spanish.")
;; (defvar portuguese-mode-abbrev-table nil "Abbreviation table for Portuguese.")
;; (defvar french-mode-abbrev-table nil "Abbreviation table for French.")
;; (defvar italian-mode-abbrev-table nil "Abbreviation table for Italian.")

;; (defvar sync0-language-active-table nil "Abbreviation table.")

;; (defvar sync0-language-configs
;;   '((:code "en" :mode "english-mode" :name "english" :input-method nil :ispell-dict "en_US-large" :abbrev-table english-mode-abbrev-table)
;;     (:code "es" :mode "spanish-mode" :name "spanish" :input-method "spanish-prefix" :ispell-dict "es" :abbrev-table spanish-mode-abbrev-table)
;;     (:code "pt" :mode "portuguese-mode" :name "portuguese" :input-method "portuguese-prefix" :ispell-dict "pt_BR" :abbrev-table portuguese-mode-abbrev-table)
;;     (:code "fr" :mode "french-mode" :name "french" :input-method "french-postfix" :ispell-dict "fr_FR" :abbrev-table french-mode-abbrev-table)
;;     (:code "it" :mode "italian-mode" :name "italian" :input-method "italian-postfix" :ispell-dict "it_IT" :abbrev-table italian-mode-abbrev-table)
;;     (:code "de" :mode "german-mode" :name "german" :input-method "german-prefix" :ispell-dict "de_DE" :abbrev-table german-mode-abbrev-table))
;;   "Alist of language configurations.")

;; (defun sync0-choose-language-action (lang)
;;   "Change language settings for the selected LANG."
;;   (when-let ((lang-config (cl-find-if (lambda (config)
;;                                        (string= (plist-get config :name) lang))
;;                                      sync0-language-configs)))
;;     (let ((code (plist-get lang-config :code))
;;           (input-method (plist-get lang-config :input-method))
;;           (ispell-dict (plist-get lang-config :ispell-dict))
;;           (lang-minor-mode (plist-get lang-config :mode))
;;           (abbrev-table (plist-get lang-config :abbrev-table)))
      
;;       ;; Disable all language minor modes
;;       (dolist (config sync0-language-configs)
;;         (let ((mode (plist-get config :mode)))
;;           (when (and (boundp (intern (concat (symbol-name mode) "-mode")))
;;                      (symbol-value (intern (concat (symbol-name mode) "-mode"))))
;;             (funcall (intern (concat (symbol-name mode) "-mode")) -1))))
      
;;       ;; Enable the selected language's minor mode
;;       (funcall (intern (concat (symbol-name lang-minor-mode) "-mode")) t)
      
;;       ;; Set the rest of the language settings
;;       (setq guess-language-current-language code)
;;       (setq sync0-language-active lang)
;;       (set-input-method input-method)
;;       (ispell-change-dictionary ispell-dict)
;;       (message "Language switched to %s." lang))))

;; (defun sync0-choose-language-action (lang)
;;   "Change language settings for the selected LANG."
;;   (when-let ((lang-config (cl-find-if (lambda (config)
;; 					(string= (plist-get config :name) lang))
;;                                       sync0-language-configs)))
;;     (let ((code (plist-get lang-config :code))
;;           (input-method (plist-get lang-config :input-method))
;;           (ispell-dict (plist-get lang-config :ispell-dict))
;;           (lang-minor-mode (plist-get lang-config :mode))
;;           (abbrev-table (plist-get lang-config :abbrev-table)))
;;       (setq guess-language-current-language code)
;;       (setq sync0-language-active lang)
;;       (set lang-minor-mode t)
;;       (set-input-method input-method)
;;       (ispell-change-dictionary ispell-dict)
;;       (message "Language switched to %s." lang))))

;; (defun sync0-choose-language-action (lang)
;;   "Change language settings for the selected LANG."
;;   (let ((lang-mode (intern (concat lang "-mode"))))
;;     ;; Disable all language minor modes
;;     (dolist (mode '(english-mode spanish-mode portuguese-mode french-mode italian-mode german-mode))
;;       (when (boundp mode)
;;         (funcall mode -1)))  ;; Disable each minor mode

;;     ;; Enable the selected language's minor mode
;;     (when (fboundp lang-mode)
;;       (funcall lang-mode 1))

;;     (message "Language switched to %s." lang)))


(defun sync0-ispell-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

;; (defun sync0-ispell-word-then-abbrev ()
;;   "Call `ispell-word', then create an abbrev for it.
;;       With prefix P, create local abbrev. Otherwise it will
;;       be global.
;;       If there's nothing wrong with the word at point, keep
;;       looking for a typo until the beginning of buffer. You can
;;       skip typos you don't want to fix with `SPC', and you can
;;       abort completely with `C-g'."
;;   (interactive)
;;   (if (bound-and-true-p sync0-language-active-table)
;;       (let (bef aft)
;; 	(save-excursion
;; 	  (while (if (setq bef (sync0-ispell-get-word))
;;                      ;; Word was corrected or used quit.
;;                      (if (ispell-word nil 'quiet)
;; 			 nil ; End the loop.
;;                        ;; Also end if we reach `bob'.
;;                        (not (bobp)))
;; 		   ;; If there's no word at point, keep looking
;; 		   ;; until `bob'.
;; 		   (not (bobp)))
;;             (backward-word)
;;             (backward-char))
;; 	  (setq aft (sync0-ispell-get-word)))
;; 	;;     (unless
;; 	;;         (save-excursion
;; 	;;           (with-temp-buffer
;; 	;;             (insert-file-contents company-ispell-dictionary)
;; 	;;             (goto-char (point-min))
;; 	;;             (re-search-forward (concat "^" aft) nil t 1)))
;; 	;;       (write-region (concat aft "\n") nil company-ispell-dictionary 'append))
;; 	(unless (equal aft bef)
;; 	  (let* ((aft (downcase aft))
;; 		 (bef (downcase bef))
;; 		 (mode sync0-language-active-table)
;; 		 (mode-table (concat mode "-abbrev-table"))
;; 		 (mode-cache (assoc mode-table sync0-abbrev-cache))
;; 		 (new-abbrev (list name expansion nil :count 0)))
;; 	    ;; Check if the mode's abbrev cache exists, otherwise create it
;; 	    (unless mode-cache
;; 	      (setq sync0-abbrev-cache
;; 		    (cons (cons mode-table nil) sync0-abbrev-cache))
;; 	      (setq mode-cache (assoc mode-table sync0-abbrev-cache)))  ;; Re-fetch the mode cache after it's created
;; 	    ;; Replace or add the abbrev in the cache
;; 	    (if (assoc bef (cdr mode-cache))  ;; If abbrev already exists, replace it
;; 		(setcdr mode-cache
;; 			(cons new-abbrev (assq-delete-all bef (cdr mode-cache))))
;; 	      ;; Otherwise, add the new abbrev to the cache
;; 	      (setcdr mode-cache (cons new-abbrev (cdr mode-cache))))
;; 	    ;; Define or redefine the abbrev in the local-abbrev-table
;; 	    (define-abbrev local-abbrev-table bef aft)
;; 	    (message "\"%s\" now expands to \"%s\" in %s." bef aft mode-table))))
;;     (error "No local abbrev table active.")))



(add-hook 'guess-language-after-detection-functions #'sync0-language-change)

(provide 'sync0-guess-language)
