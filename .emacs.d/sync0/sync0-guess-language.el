
(defun sync0-language-change (lang beginning end)
  "Set of functions to run after a different language is detected."
  (unless (string-equal guess-language-current-language lang)
    (cond 
     ((string-equal lang "es")
      (progn
        (setq sync0-language-active "spanish")
        (setq local-abbrev-table spanish-mode-abbrev-table)
        (set-input-method "spanish-prefix")
        (ispell-change-dictionary "es")))
     ((string-equal lang "de")
      (progn
        (setq sync0-language-active "german")
        (setq local-abbrev-table german-mode-abbrev-table)
        (set-input-method "german-prefix")
        (ispell-change-dictionary "de_DE")))
     ((string-equal lang "pt")
      (progn
        (setq sync0-language-active "portuguese")
        (setq local-abbrev-table portuguese-mode-abbrev-table)
        (set-input-method "portuguese-prefix")
        (ispell-change-dictionary "pt_BR")))
     ((string-equal lang "fr")
      (progn
        (setq sync0-language-active "french")
        (setq local-abbrev-table french-mode-abbrev-table)
        (set-input-method "french-postfix")
        (ispell-change-dictionary "fr_FR")))
     ((string-equal lang "it")
      (progn
        (setq sync0-language-active "italian")
        (setq local-abbrev-table italian-mode-abbrev-table)
        (set-input-method "italian-postfix")
        (ispell-change-dictionary "it_IT")))
     ((string-equal lang "en")
      (progn
        (setq sync0-language-active "english")
        (setq local-abbrev-table english-mode-abbrev-table)
        (set-input-method nil)
        (ispell-change-dictionary "en_US-large"))))))

(defvar sync0-change-language-actions-alist
  '((?1 "en" (lambda ()
               (progn
                 (setq  guess-language-current-language 'en)
                 (setq sync0-language-active "english")
                 (setq local-abbrev-table english-mode-abbrev-table)
                 (set-input-method nil)
                 (ispell-change-dictionary "en_US-large"))))
    (?2 "es" (lambda ()
               (progn
                 (setq  guess-language-current-language 'es)
                 (setq sync0-language-active "spanish")
                 (setq local-abbrev-table spanish-mode-abbrev-table)
                 (set-input-method "spanish-prefix")
                 (ispell-change-dictionary "es"))))
    (?3 "pt" (lambda ()
               (progn
                 (setq  guess-language-current-language 'pt)
                 (setq sync0-language-active "portuguese")
                 (setq local-abbrev-table portuguese-mode-abbrev-table)
                 (set-input-method "portuguese-prefix")
                 (ispell-change-dictionary "pt_BR"))))
    (?4 "fr" (lambda ()
               (progn
                 (setq  guess-language-current-language 'fr)
                 (setq sync0-language-active "french")
                 (setq local-abbrev-table french-mode-abbrev-table)
                 (set-input-method "french-postfix")
                 (ispell-change-dictionary "fr_FR"))))
    (?5 "it" (lambda ()
               (progn
                 (setq  guess-language-current-language 'it)
                 (setq sync0-language-active "italian")
                 (setq local-abbrev-table italian-mode-abbrev-table)
                 (set-input-method "italian-postfix")
                 (ispell-change-dictionary "it_IT"))))
    (?6 "de" (lambda ()
               (progn
                 (message "Deutsch ist die aktuelle Sprache")
                 (setq  guess-language-current-language 'de)
                 (setq sync0-language-active "german")
                 (setq local-abbrev-table german-mode-abbrev-table)
                 (set-input-method "german-prefix")
                 (ispell-change-dictionary "de_DE")))))
  "List that associates number letters to descriptions and actions.")

(defun sync0-change-current-language ()
  "Lets the user choose the animal and takes the corresponding action.
    Returns whatever the action returns."
  (interactive)
  (let ((choice
         (read-char-choice
          (mapconcat
           (lambda (item) (format "[%c] %s" (car item) (cadr item)))
           sync0-change-language-actions-alist " ")
          (mapcar #'car sync0-change-language-actions-alist))))
    (funcall (nth 2 (assoc choice sync0-change-language-actions-alist)))))

(defun sync0-ispell-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun sync0-ispell-word-then-abbrev ()
  "Call `ispell-word', then create an abbrev for it.
      With prefix P, create local abbrev. Otherwise it will
      be global.
      If there's nothing wrong with the word at point, keep
      looking for a typo until the beginning of buffer. You can
      skip typos you don't want to fix with `SPC', and you can
      abort completely with `C-g'."
  (interactive)
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (sync0-ispell-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (sync0-ispell-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          ;; (unless
          ;;  (save-excursion
          ;;   (with-temp-buffer
          ;;    (insert-file-contents company-ispell-dictionary)
          ;;    (goto-char (point-min))
          ;;    (re-search-forward (concat "^" aft) nil t 1)))
          ;;    (write-region (concat aft "\n") nil company-ispell-dictionary 'append))
          (define-abbrev local-abbrev-table bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft "loc"))
      (user-error "No typo at or before point"))))

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
  ("i" sync0-ispell-word-then-abbrev)
  ("c" sync0-lookup-conjugation)
  ("t" sync0-lookup-thesaurus)
  ("q"  nil :color blue))

(evil-leader/set-key
  "L" 'sync0-ispell-word-then-abbrev
  "l" 'sync0-hydra-language-functions/body)

(add-hook 'guess-language-after-detection-functions #'sync0-language-change)

(provide 'sync0-guess-language)
