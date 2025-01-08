
(defvar sync0-zkn-all-properties-list
  '("ZETTEL_TYPE" "BIBLATEX_TYPE" "ZETTEL_FUNCTION" "FICHE_TYPE" "PROJECT_TITLE" "ANNOTATION_REFS" "ROAM_REFS" "ROAM_ALIASES" "CROSSREF" "PARENT" "WEBSITE") 
  "List of zettel properties")

(defvar sync0-zkn-properties-list
  '("PROJECT_TITLE") 
  "List of zettel properties")

(defvar sync0-zkn-excluded-candidates
  '("journal" "fiche" "etc" "project" "todo" "reference"))

(defvar sync0-zkn-project-dirs '("project" "todo"))

(defvar sync0-zkn-zettel-types 
  '()
  "List of projects in my Zkn.")

(defvar sync0-zkn-projects 
  '()
  "List of projects in my Zkn.")

(defvar sync0-zkn-zettel-functions 
  '() 
  "List of possible functions for a Zettel.")

(defvar sync0-zkn-fiche-types 
  '() 
  "List of fiche types.")

(defvar sync0-language-active-code nil)

(defvar sync0-func-dir
  (concat (getenv "HOME") "/.emacs.d/sync0/")
  "Directory with all my custom functions.")

(defvar sync0-vars-dir
  (concat (getenv "HOME") "/.emacs.d/sync0-vars/")
  "Directory with all my custom variables.")

(defvar sync0-databases-dir
  (concat (getenv "HOME") "/Gdrive/databases/")
  "Directory with all my custom variables.")

(defvar sync0-alpha "abcdefghijkmnpqrstuvwxyz"
  "Possible alphabetic characters that can appear in BibLaTeX keys.
Use format of base58 encoding.")

(defvar sync0-bibtex-timeday
  (format-time-string "%y%-j")
  "Timeday for newly created bibkeys and such")

(defvar sync0-date-year
  (format-time-string "%Y")
  "Timeday for newly created bibkeys and such")

(defvar sync0-zkn-variables-list
  '((sync0-zkn-projects . "~/.emacs.d/sync0-vars/projects.txt")
;;     (sync0-zkn-zettel-types . "~/.emacs.d/sync0-vars/zettel-types.txt")
    (sync0-zkn-zettel-functions . "~/.emacs.d/sync0-vars/zettel-functions.txt")
    (sync0-zkn-fiche-types . "~/.emacs.d/sync0-vars/fiche-types.txt")))
;; define the rest

(defconst sync0-zkn-zettel-types-alist '(("main" . ("annotation" "atom" "block" "bibliography" "chronology" "citation" "excalidraw" "lexis" "list" "question" "structure" "snippet" "node" "nil"))
					       ("fiche" . ("character" "concept" "event" "dewey" "institution" "keyword" "lexicon" "maxim" "oeuvre" "people" "place" "publication" "reference" "nil"))
					       ("index" . ("dashboard" "bibliography" "list" "misc" "meta" "query" "tasks" "nil"))
					       ("project" . ("log" "supervision" "note" "prompt" "nil"))
					       ("writing" . ("collage" "draft" "freewriting" "mini" "plan" "nil"))))

(defconst sync0-zkn-zettel-types
  (mapcar 'car sync0-zkn-zettel-types-alist)
  "A list of Zettel types for sync0's Zettelkasten system.")


(defconst sync0-zkn-doctypes
  (list "article" "atom" "book" "blog" "chapter" "conference" "composant" "outline" "paragraph" "part" "report" "review" "scene" "section" "subsection" "subsubsection" "speech" "structure" "summary" "synthesis"))


(setq sync0-cloud-dir (concat (getenv "HOME") "/Gdrive/")
      sync0-zkn-dir (concat sync0-cloud-dir "obsidian/")
      sync0-bibliographies-dir (concat sync0-cloud-dir "bibliographies/")
      sync0-goodreads-dir (concat sync0-cloud-dir "goodreads/")
      sync0-bibnotes-dir (concat sync0-cloud-dir "bibnotes/")
      sync0-zkn-references-dir (concat sync0-zkn-dir "references/")
      ;; sync0-obsidian-dir (concat (getenv "HOME") "/Gdrive/obsidian/")
      sync0-zkn-dir-sans (concat (getenv "HOME") "/Gdrive/obsidian")
      sync0-zkn-attachments-dir (concat (getenv "HOME") "/Gdrive/cabinet/")
      ;; sync0-exported-pdfs-dir (concat (getenv "HOME") "/Gdrive/cabinet/")
      sync0-zkn-exported-pdfs-dir sync0-zkn-attachments-dir
      sync0-emacs-dir (concat (getenv "HOME") "/.emacs.d/sync0/")
      sync0-inbox-dir (concat (getenv "HOME") "/Inbox/")
      sync0-dir-teaching (concat sync0-cloud-dir "projects/teaching/" sync0-date-year "/")
      sync0-current-year (format-time-string "%Y")
      sync0-current-month (format-time-string "%B")
      sync0-current-month-downcase (downcase (format-time-string "%B"))
      sync0-current-day (format-time-string "%d")
      sync0-english-parts-speech '("noun" "intransitive verb" "transitive verb" "verb" "conjunction" "adjective" "adverb")
      sync0-french-parts-speech '("nom féminin" "nom masculin" "verbe intransitif" "verbe transitif" "verbe" "conjonction" "adjectif" "adverbe")
      sync0-portuguese-parts-speech '("sustantivo femenino" "sustantivo masculino" "verbo intransitivo" "verbo transitivo" "verbo" "conjunção" "adjetivo" "advérbio")
      sync0-spanish-parts-speech '("sustantivo femenino" "sustantivo masculino" "verbo intransitivo" "verbo transitivo" "verbo" "conjunción" "adjectivo" "adverbio"))

(defvar sync0-default-file-associations
   '(("epub" . "zathura")
     ("odt" . "libreoffice")
     ("doc" . "libreoffice")
     ("docx" . "libreoffice")
     ("ppt" . "libreoffice")
     ("mp4" . "vlc")
     ("mp3" . "vlc"))
"My default file associations")

(defvar sync0-last-file nil
  "Stores the path of the last file used.")

(defvar sync0-last-dir nil
  "Stores the path of the last directory used.")

(defun sync0-set-variable-from-files (varlist)
  "From a list of pairs of variable and files, define all of them
  with a loop"
  (dolist (element varlist) 
    (let ((var (car element))
          (file (cdr element))
          x)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; (keep-lines "contexts" (point-min) (point-max)) 
        (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
          (push (match-string-no-properties 1) x)))
      (set var (reverse x)))))

(sync0-set-variable-from-files sync0-zkn-variables-list)

(defvar sync0-org-roam-zettel-title nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-path nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-subtitle nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-author nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-type nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-subtype nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-doctype nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-date nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-version nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-last-version nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-project nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-origin nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-parent nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-aliases nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-filename nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-archive nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-up nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-down nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-right nil
  "Dummy var that stores the title of the Zettel to create.")

(defvar sync0-org-roam-zettel-left nil
  "Dummy var that stores the title of the Zettel to create.")

(defun sync0-org-roam-reset-zettel-properties ()
  "Reset all sync0-org-roam-zettel properties to nil."
  (setq sync0-org-roam-zettel-title nil
        sync0-org-roam-zettel-subtitle nil
        sync0-org-roam-zettel-author nil
        sync0-org-roam-zettel-path nil
        sync0-org-roam-zettel-date nil
        sync0-org-roam-zettel-archive nil
        sync0-org-roam-zettel-origin nil
        sync0-org-roam-zettel-up nil
        sync0-org-roam-zettel-left nil
        sync0-org-roam-zettel-right nil
        sync0-org-roam-zettel-down nil
        sync0-org-roam-zettel-type nil
        sync0-org-roam-zettel-subtype nil
        sync0-org-roam-zettel-doctype nil
        sync0-org-roam-zettel-version nil
        sync0-org-roam-zettel-last-version nil
        sync0-org-roam-zettel-project nil
        sync0-org-roam-zettel-parent nil
        sync0-org-roam-zettel-aliases nil
        sync0-org-roam-zettel-filename nil))


(provide 'sync0-vars)

