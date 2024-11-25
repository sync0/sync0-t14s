(require 'ispell)

(defvar sync0-language-active-table nil "Abbreviation table for language minor modes.")

(define-minor-mode english-mode
  "Minor mode for English language."
  :lighter " en"
  :keymap nil
  (if english-mode
      (progn
        (setq-local local-abbrev-table english-mode-abbrev-table)
        (setq sync0-language-active-code "en")
        (setq sync0-language-active-table "english-mode")
        (set-input-method nil)               ;; No input method needed for English
        (ispell-change-dictionary "en_US-large"))
    ;; Reset changes when mode is disabled
    (kill-local-variable 'local-abbrev-table)
    (kill-local-variable 'sync0-language-active-code)
    (kill-local-variable 'sync0-language-active-table)))

(define-globalized-minor-mode global-english-mode
  english-mode
  (lambda ()
    (when (derived-mode-p 'text-mode) ;; Only activate in text-based modes
      (english-mode 1))))

;; Define Spanish Mode
(define-minor-mode spanish-mode
  "Minor mode for Spanish language."
  :lighter " es"
  :keymap nil
  (if spanish-mode
      (progn
        (setq-local local-abbrev-table spanish-mode-abbrev-table)
        (setq sync0-language-active-code "es")
        (setq sync0-language-active-table "spanish-mode")
        (set-input-method "spanish-prefix")
        (ispell-change-dictionary "es"))
    (kill-local-variable 'local-abbrev-table)
    (kill-local-variable 'sync0-language-active-code)
    (kill-local-variable 'sync0-language-active-table)))

(define-globalized-minor-mode global-spanish-mode
  spanish-mode
  (lambda ()
    (when (derived-mode-p 'text-mode)
      (spanish-mode 1))))

;; Define Portuguese Mode
(define-minor-mode portuguese-mode
  "Minor mode for Portuguese language."
  :lighter " pr"
  :keymap nil
  (if portuguese-mode
      (progn
        (setq-local local-abbrev-table portuguese-mode-abbrev-table)
        (setq sync0-language-active-code "pt")
        (setq sync0-language-active-table "portuguese-mode")
        (set-input-method "portuguese-prefix")
        (ispell-change-dictionary "pt_BR"))
    (kill-local-variable 'local-abbrev-table)
    (kill-local-variable 'sync0-language-active-code)
    (kill-local-variable 'sync0-language-active-table)))

(define-globalized-minor-mode global-portuguese-mode
  portuguese-mode
  (lambda ()
    (when (derived-mode-p 'text-mode)
      (portuguese-mode 1))))

;; Define French Mode
(define-minor-mode french-mode
  "Minor mode for French language."
  :lighter " fr"
  :keymap nil
  (if french-mode
      (progn
        (setq-local local-abbrev-table french-mode-abbrev-table)
        (setq sync0-language-active-code "fr")
        (setq sync0-language-active-table "french-mode")
        (set-input-method "french-postfix")
        (ispell-change-dictionary "fr_FR"))
    (kill-local-variable 'local-abbrev-table)
    (kill-local-variable 'sync0-language-active-code)
    (kill-local-variable 'sync0-language-active-table)))

(define-globalized-minor-mode global-french-mode
  french-mode
  (lambda ()
    (when (derived-mode-p 'text-mode)
      (french-mode 1))))

;; Define Italian Mode
(define-minor-mode italian-mode
  "Minor mode for Italian language."
  :lighter " it"
  :keymap nil
  (if italian-mode
      (progn
        (setq-local local-abbrev-table italian-mode-abbrev-table)
        (setq sync0-language-active-code "it")
        (setq sync0-language-active-table "italian-mode")
        (set-input-method "italian-postfix")
        (ispell-change-dictionary "it_IT"))
    (kill-local-variable 'local-abbrev-table)
    (kill-local-variable 'sync0-language-active-code)
    (kill-local-variable 'sync0-language-active-table)))

(define-globalized-minor-mode global-italian-mode
  italian-mode
  (lambda ()
    (when (derived-mode-p 'text-mode)
      (italian-mode 1))))

;; Define German Mode
(define-minor-mode german-mode
  "Minor mode for German language."
  :lighter " de"
  :keymap nil
  (if german-mode
      (progn
        (setq-local local-abbrev-table german-mode-abbrev-table)
        (setq sync0-language-active-code "de")
        (setq sync0-language-active-table "german-mode")
        (set-input-method "german-prefix")
        (ispell-change-dictionary "de_DE"))
    (kill-local-variable 'local-abbrev-table)
    (kill-local-variable 'sync0-language-active-code)
    (kill-local-variable 'sync0-language-active-table)))

(define-globalized-minor-mode global-german-mode
  german-mode
  (lambda ()
    (when (derived-mode-p 'text-mode)
      (german-mode 1))))

(setq abbrev-minor-mode-table-alist '(("english-mode" .   english-mode-abbrev-table)
                                      ("french-mode" .   french-mode-abbrev-table)
                                      ("portuguese-mode" .   portuguese-mode-abbrev-table)
                                      ("french-mode" .   french-mode-abbrev-table)
                                      ("italian-mode" .   italian-mode-abbrev-table)
                                      ("german-mode" .   german-mode-abbrev-table)))

;; (defun sync0-choose-language-action (lang-code)
;;   "Change language settings for the selected LANG-CODE."
;;   (let* ((language-map '(("en" . "english")
;;                         ("fr" . "french")
;;                         ("es" . "spanish")
;;                         ("de" . "german")
;;                         ("it" . "italian")
;;                         ("pt" . "portuguese")))
;;          (lang-name (cdr (assoc lang-code language-map)))  ;; Get language name from lang-code
;;          (lang-mode (intern (concat lang-name "-mode"))))  ;; Construct the mode name (e.g., "english-mode")

;;     ;; Disable all language minor modes
;;     (dolist (mode '(english-mode spanish-mode portuguese-mode french-mode italian-mode german-mode))
;;       (when (boundp mode)
;;         (funcall mode -1)))  ;; Disable each minor mode

;;     ;; Enable the selected language's minor mode
;;     (when (fboundp lang-mode)
;;       (funcall lang-mode 1))

;;     (message "Language switched to %s." lang-name)))

;; (defun sync0-choose-current-language ()
;;   "Let the user choose a language interactively and perform the corresponding action."
;;   (interactive)
;;   (let ((chosen-language (completing-read
;;                            "Choose language: "
;;                            (list "en" "fr" "es" "de" "it" "pt")
;;                            nil t)))  ;; t for require match, nil for case insensitive
;;     (sync0-choose-language-action chosen-language)))

(defun sync0-choose-language-action (lang-code)
  "Change language settings for the selected LANG-CODE."
  (let* ((language-map '(("en" . "english")
                        ("fr" . "french")
                        ("es" . "spanish")
                        ("de" . "german")
                        ("it" . "italian")
                        ("pt" . "portuguese")))
         (lang-name (cdr (assoc lang-code language-map)))  ;; Get language name from lang-code
         (global-lang-mode (intern (concat "global-" lang-name "-mode"))))  ;; Construct the global mode name (e.g., "global-english-mode")

    ;; Disable all global language minor modes
    (dolist (mode '(global-english-mode global-spanish-mode global-portuguese-mode global-french-mode global-italian-mode global-german-mode))
      (when (boundp mode)
        (funcall mode -1)))  ;; Disable each global minor mode

    ;; Enable the selected language's global minor mode
    (when (fboundp global-lang-mode)
      (funcall global-lang-mode 1))

    (message "Language switched to %s." lang-name)))

(defun sync0-choose-current-language ()
  "Let the user choose a language interactively and perform the corresponding action."
  (interactive)
  (let ((chosen-language (completing-read
                           "Choose language: "
                           (list "en" "fr" "es" "de" "it" "pt")
                           nil t)))  ;; t for require match, nil for case insensitive
    (sync0-choose-language-action chosen-language)))


(defun sync0-language-change (lang beginning end)
  "Set of functions to run after a different language is detected."
  (unless (string-equal guess-language-current-language lang)
(sync0-choose-language-action lang)))

(defun sync0-ispell-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun sync0-ispell-word-then-abbrev ()
  "Call `ispell-word`, then create an abbrev for it.
   With prefix P, create local abbrev. Otherwise, it will be global.
   If there's nothing wrong with the word at point, keep looking for a typo until the beginning of buffer."
  (interactive)
  (if (bound-and-true-p sync0-language-active-table)
      (let (name expansion)
        (save-excursion
          (while  (if (setq name (sync0-ispell-get-word))
		      ;; Check the word with `ispell`
		      (if (ispell-word nil 'quiet)
			  nil  ; End the loop if word is corrected
			(not (bobp)))
		    (not (bobp)))
            (backward-word)  ;; Keep searching for typos until beginning of buffer
            (backward-char))
          ;; Get the word after spell check
          (setq expansion (sync0-ispell-get-word)))
        ;; Proceed only if the word has changed (i.e., there was an actual typo)
        (unless (equal expansion name)
          (let* ((name (downcase name))
                 (expansion (downcase expansion))
                 (mode sync0-language-active-table)
                 (mode-table (concat mode "-abbrev-table"))
                 (mode-cache (assoc mode-table sync0-abbrev-cache))
                 (new-abbrev (list name expansion nil :count 0)))
            ;; Check if the mode's abbrev cache exists, otherwise create it
            (unless mode-cache
              (setq sync0-abbrev-cache (cons (cons mode-table nil) sync0-abbrev-cache))
              (setq mode-cache (assoc mode-table sync0-abbrev-cache)))  ;; Re-fetch the mode cache after it's created
            ;; Replace or add the abbrev in the cache
            (if (assoc name (cdr mode-cache))  ;; If abbrev already exists, replace it
                (setcdr mode-cache (cons new-abbrev (assq-delete-all name (cdr mode-cache))))
              ;; Otherwise, add the new abbrev to the cache
              (setcdr mode-cache (cons new-abbrev (cdr mode-cache))))
            ;; Define or redefine the abbrev in the local-abbrev-table
            (define-abbrev local-abbrev-table name expansion)
            (message "\"%s\" now expands to \"%s\" in %s." name expansion mode-table))))
    (error "No local abbrev table active.")))

(global-set-key (kbd "M-i") 'sync0-ispell-word-then-abbrev)
(evil-leader/set-key "L" 'sync0-ispell-word-then-abbrev)

(defun sync0-lookup-word (word)
  "Search an online dictionary for the word at point according
            to the active language minor mode."
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (cond  ((string-equal guess-language-current-language "en") 
          (browse-url (format "https://www.merriam-webster.com/dictionary/%s" word)))
         ((string-equal guess-language-current-language "de") 
          (browse-url (format "https://www.duden.de/rechtschreibung/%s" word)))
         ((string-equal guess-language-current-language "it") 
          (browse-url (format "https://www.duden.de/rechtschreibung/%s" word)))
         ((string-equal guess-language-current-language "pt") 
          (browse-url (format "https://www.dicio.com.br/%s" word)))
         ((string-equal guess-language-current-language "fr") 
          (browse-url (format "https://dictionnaire.lerobert.com/definition/%s#definitions" word)))
         ((string-equal guess-language-current-language "es") 
          (browse-url (format "https://dle.rae.es/?w=%s" word)))
         (t "No language minor mode specified")))

(defun sync0-lookup-conjugation (word)
  "Search an online dictionary for the word at point according
            to the active language minor mode."
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (cond  ((string-equal guess-language-current-language "en") 
          (browse-url (format "https://www.merriam-webster.com/dictionary/%s" word)))
         ((string-equal guess-language-current-language "de") 
          (browse-url (format "https://www.verbformen.de/konjugation/?w=%s" word)))
         ((string-equal guess-language-current-language "it") 
          (browse-url (format "https://www.verbformen.de/konjugation/?w=%s" word)))
         ((string-equal guess-language-current-language "pt") 
          (browse-url (format "https://www.conjugacao.com.br/verbo-%s/" word)))
         ((string-equal guess-language-current-language "fr") 
          (browse-url (format "http://la-conjugaison.nouvelobs.com/du/verbe/%s.php" word)))
         ((string-equal guess-language-current-language "es") 
          (browse-url (format "http://conjugador.reverso.net/conjugacion-espanol-verbo-%s.html" word)))
         (t "No language minor mode specified")))

(defun sync0-lookup-thesaurus (word)
  "Search an online dictionary for the word at point according
            to the active language minor mode."
  (interactive (list (save-excursion (car (ispell-get-word nil)))))
  (cond  ((string-equal guess-language-current-language "en") 
          (browse-url (format "https://www.merriam-webster.com/thesaurus/%s" word)))
         ((string-equal guess-language-current-language "fr") 
          (browse-url (format "https://dictionnaire.lerobert.com/definition/%s#synonymes" word)))
         ((string-equal guess-language-current-language "de") 
          (browse-url (format "https://www.duden.de/rechtschreibung/%s#synonyme" word)))
         ((string-equal guess-language-current-language "it") 
          (browse-url (format "https://www.duden.de/rechtschreibung/%s#synonyme" word)))
         ((string-equal guess-language-current-language "pt") 
          (browse-url (format "https://www.dicio.com.br/%s" word)))
         ((string-equal guess-language-current-language "es") 
          (browse-url (format "http://conjugador.reverso.net/conjugacion-espanol-verbo-%s.html" word)))
         (t "No language minor mode specified")))

(defun sync0-guess-language-set-parts-of-speech ()
  "Choose parts of speech according to active language"
  (let* ((parts-list (list ()))
         (lang (prin1-to-string guess-language-current-language)))
    (cond ((string-equal lang "es")
           (progn
             (setq parts-list sync0-spanish-parts-speech)
             (ivy-completing-read "Elija uno: " parts-list)))
          ((string-equal lang "pt")
           (progn
             (setq parts-list sync0-portuguese-parts-speech)
             (ivy-completing-read "Escolha um: " parts-list)))
          ((string-equal lang "it")
           (progn
             (setq parts-list sync0-portuguese-parts-speech)
             (ivy-completing-read "Escolha um: " parts-list)))
          ((string-equal lang "fr")
           (progn
             (setq parts-list sync0-french-parts-speech)
             (ivy-completing-read "Choississez un : " parts-list)))
          ((string-equal lang "en")
           (progn
             (setq parts-list sync0-english-parts-speech)
             (ivy-completing-read "Choose one: " parts-list)))
          (t "No language minor mode specified"))))

(defhydra sync0-hydra-language-functions (:color amaranth :hint nil :exit t)
  "
     ^Language functions^
     ^^^------------------------
     Show _d_efinition
     Show _c_onjugation
     Show in _t_hesaurus

     _q_uit
        "
  ;; Quickly work with bookmarks
  ("d" sync0-lookup-word)
  ;; ("i" sync0-ispell-word-then-abbrev)
  ("c" sync0-lookup-conjugation)
  ("t" sync0-lookup-thesaurus)
  ("q"  nil :color blue))

(evil-leader/set-key
  "l" 'sync0-hydra-language-functions/body)

(provide 'sync0-language)
