(setq user-full-name "Carlos Alberto Rivera Carreño"
      ;; Define my Gdrive location
      sync0-cloud-directory "~/Gdrive/"
      user-mail-address "carc.sync0@gmail.com")

 ;; Bookmarks directory
 (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
       bookmark-save-flag 1)

 (setq auto-save-interval 100
       auto-save-timeout 60)

(provide 'sync0-user)
