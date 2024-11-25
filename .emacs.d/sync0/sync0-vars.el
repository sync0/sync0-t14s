
(defvar sync0-zettelkasten-all-properties-list
  '("ZETTEL_TYPE" "BIBLATEX_TYPE" "ZETTEL_FUNCTION" "FICHE_TYPE" "PROJECT_TITLE" "ANNOTATION_REFS" "ROAM_REFS" "ROAM_ALIASES" "CROSSREF" "PARENT" "WEBSITE") 
  "List of zettel properties")

(defvar sync0-zettelkasten-properties-list
  '("PROJECT_TITLE") 
  "List of zettel properties")

(defvar sync0-zettelkasten-excluded-candidates
  '("journal" "fiche" "etc" "project" "todo" "reference"))

(defvar sync0-zettelkasten-project-directories '("project" "todo"))

(defvar sync0-zettelkasten-zettel-types 
  '()
  "List of projects in my Zettelkasten.")

(defvar sync0-zettelkasten-projects 
  '()
  "List of projects in my Zettelkasten.")

(defvar sync0-zettelkasten-zettel-functions 
  '() 
  "List of possible functions for a Zettel.")

(defvar sync0-zettelkasten-fiche-types 
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

(defvar sync0-zettelkasten-variables-list
  '((sync0-zettelkasten-projects . "~/.emacs.d/sync0-vars/projects.txt")
;;     (sync0-zettelkasten-zettel-types . "~/.emacs.d/sync0-vars/zettel-types.txt")
    (sync0-zettelkasten-zettel-functions . "~/.emacs.d/sync0-vars/zettel-functions.txt")
    (sync0-zettelkasten-fiche-types . "~/.emacs.d/sync0-vars/fiche-types.txt")))
;; define the rest

(defvar sync0-zettelkasten-zettel-types-alist '(("main" . ("annotation" "atom" "block" "bibliography" "chronology" "citation" "excalidraw" "lexis" "list" "question" "structure" "snippet" "node" "nil"))
					       ("fiche" . ("character" "concept" "event" "dewey" "institution" "keyword" "lexicon" "maxim" "oeuvre" "people" "place" "publication" "reference" "nil"))
					       ("index" . ("dashboard" "bibliography" "list" "misc" "meta" "query" "tasks" "nil"))
					       ("project" . ("log" "supervision" "note" "prompt" "nil"))
					       ("writing" . ("collage" "draft" "freewriting" "mini" "plan" "nil"))))

(defvar sync0-zettelkasten-zettel-types
      (mapcar 'car sync0-zettelkasten-zettel-types-alist))

(defvar sync0-zettelkasten-doctypes
  (list "article" "atom" "book" "blog" "chapter" "conference" "composant" "outline" "paragraph" "part" "report" "review" "scene" "section" "subsection" "subsubsection" "speech" "structure" "summary" "synthesis"))


(setq sync0-cloud-directory (concat (getenv "HOME") "/Gdrive/")
      sync0-zettelkasten-directory (concat sync0-cloud-directory "obsidian/")
      sync0-bibliographies-directory (concat sync0-cloud-directory "bibliographies/")
      sync0-goodreads-directory (concat sync0-cloud-directory "goodreads/")
      sync0-bibnotes-directory (concat sync0-cloud-directory "bibnotes/")
      sync0-zettelkasten-references-directory (concat sync0-zettelkasten-directory "references/")
      ;; sync0-obsidian-directory (concat (getenv "HOME") "/Gdrive/obsidian/")
      sync0-zettelkasten-directory-sans (concat (getenv "HOME") "/Gdrive/obsidian")
      sync0-zettelkasten-attachments-directory (concat (getenv "HOME") "/Gdrive/cabinet/")
      ;; sync0-exported-pdfs-directory (concat (getenv "HOME") "/Gdrive/cabinet/")
      sync0-zettelkasten-exported-pdfs-directory sync0-zettelkasten-attachments-directory
      sync0-emacs-directory (concat (getenv "HOME") "/.emacs.d/sync0/")
      sync0-inbox-directory (concat (getenv "HOME") "/Inbox/")
      sync0-directory-teaching (concat sync0-cloud-directory "projects/teaching/" sync0-date-year "/")
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

(defvar sync0-last-directory nil
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

(sync0-set-variable-from-files sync0-zettelkasten-variables-list)

(provide 'sync0-vars)

