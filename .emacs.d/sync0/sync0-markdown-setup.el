(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  :config
  (require 'sync0-yaml))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :custom
  (indent-tabs-mode t)
  (markdown-enable-wiki-links t)
  (markdown-enable-math t)
  (markdown-coding-system 'utf-8)
  (markdown-asymmetric-header t)
  (markdown-hide-markup t)
  ;; (markdown-hide-markup nil)
  (markdown-header-scaling t)
  ;; ? scale
  ;;    (markdown-header-scaling-values '(1.953 1.563 1.25 1.0 0.8 0.64))
  ;; major second scale
  (markdown-header-scaling-values '(1.602 1.424 1.266 1.125 1.0 1.0))
  ;; ?
  ;; (markdown-header-scaling-values '(2.074 1.728 1.44 1.2 1.0 0.833))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :config
  (require 'sync0-markdown)

  (evil-define-key 'visual markdown-mode-map
    "z" 'markdown-insert-italic)

  (evil-define-key 'normal markdown-mode-map
    (kbd "<tab>") 'markdown-cycle
    (kbd "C-<") 'markdown-backward-same-level
    (kbd "C->") 'markdown-forward-same-level
    (kbd "C-k") 'markdown-move-up
    (kbd "C-j") 'markdown-move-down
    (kbd "C-h") 'markdown-promote
    (kbd "C-l") 'markdown-demote
    "H" 'markdown-promote
    "L" 'markdown-demote
;;     "K" 'markdown-move-up
;;     "J" 'markdown-move-down
    "k" 'evil-previous-visual-line
    "j" 'evil-next-visual-line
    "$" 'evil-end-of-visual-line
    "^" 'evil-beginning-of-visual-line
    "[" 'evil-backward-sentence-begin
    "]" 'evil-forward-sentence-begin
    "{" 'markdown-backward-paragraph
    "}" 'markdown-forward-paragraph)

  :bind ((:map markdown-mode-map
               ("M-<right>" . markdown-demote)
               ("M-<left>" . markdown-promote))))

(use-package obsidian
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "inbox")
  ;; Create missing files in inbox? - when clicking on a wiki link
  ;; t: in inbox, nil: next to the file with the link
  ;; default: t
  ;; (obsidian-wiki-link-create-file-in-inbox nil)
  ;; The directory for daily notes (file name is YYYY-MM-DD.md)
  (obsidian-daily-notes-directory "journal")
  ;; Directory of note templates, unset (nil) by default
  (obsidian-templates-directory "templates")
  ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
  ;; (obsidian-daily-note-template "Daily Note Template.md")
  :config
  (obsidian-specify-path sync0-zettelkasten-directory)
  ;; (global-obsidian-mode t)

  (require 'sync0-markdown)
  (require 'markdown-toc)

  (defun sync0-obsidian-ignore-unparseable-files (orig-fun &rest args)
    "Advice to skip files that cannot be parsed by YAML parser."
    (condition-case err
        (apply orig-fun args)
      (error (message "Skipping unparseable file: %s" (car args)) nil)))

  (advice-add 'obsidian-find-yaml-front-matter :around #'sync0-obsidian-ignore-unparseable-files))

;; Override obsidian.el functions with my custom version of them
(with-eval-after-load 'obsidian

  (defun obsidian--update-from-front-matter (file)
    "Takes FILE, parse front matter then update anything that needs to be updated.
At the moment updates only `obsidian--aliases-map' with found aliases."
    ;; Check if file is in "references" folder
    (unless (string-match-p "/references/" file)
      (let* ((dict (obsidian--file-front-matter file)))
        (if dict
            (let* ((aliases (gethash 'aliases dict))
                   (alias (gethash 'alias dict))
                   (all-aliases (-filter #'identity (append aliases (list alias)))))
              ;; Update aliases
              (-map (lambda (al) (if al (progn
                                          (obsidian--add-alias (format "%s" al) file)))) all-aliases))))))

  ;; Step 2: Redefine `obsidian-update` to check cache age and conditionally update
  (defun obsidian-update ()
    "Update everything in obsidian.el (tags, links, etc.), with cache checks."
    (interactive)
    ;; Load the cache if it exists
    (sync0-obsidian-load-cache)
    ;; Define an age threshold (in seconds) for the cache; here, 1 day
    (let ((cache-age-threshold (* 24 60 60))  ; 1 day in seconds
          (current-time (float-time)))
      (if (and obsidian-cache-timestamp
               (< (- current-time obsidian-cache-timestamp) cache-age-threshold))
          ;; If the cache is recent enough, skip updating
          (message "Using recent cache; skipping update.")
        ;; Otherwise, prompt the user
        (if (y-or-n-p "Cache is outdated. Do you want to use it anyway?")
            (message "Using outdated cache.")
          ;; If the user wants a fresh cache, proceed with updating
          (obsidian-reset-cache)
          (obsidian-update-tags-list)
          (obsidian--update-all-from-front-matter)
          ;; Save the updated cache
          (sync0-obsidian-save-cache)
          (message "Cache updated and saved.")))))

  (defun obsidian-tag-find ()
    "Find all notes with a tag."
    (interactive)
    ;;   (obsidian-update-tags-list)
    (let* ((tag (completing-read "Select tag: "
                                 (->> obsidian--tags-list (-map 's-downcase) -distinct (-sort 'string-lessp))))
           (results (obsidian--grep tag))
           (choice (completing-read "Select file: " results)))
      (obsidian-find-file choice)))
  )

(provide 'sync0-markdown-setup)
