;; Taken from
;; https://readingworldmagazine.com/emacs/2020-08-24-emacs-web-mode/
                                        ;web-mode hydra
;; (defvar sync0-web-mode-title (with-octicon "globe" "Web Mode Control"))
;generate hydra
;; (pretty-hydra-define Web-Mode (:title sync0-web-mode-title :quit-key "q" :color blue )
;; (pretty-hydra-define Web-Mode (:quit-key "q" :color blue )
;; ("Mode"
;; (
;;     ("1" beginning-of-buffer "Go To Top Of Page")
;;     ("0" end-of-buffer "Go To Bottom Of Page")
;;     ("n" web-mode-element-end "Go Element End")
;;     ("p" web-mode-element-beginning "Go Element Beginning")
;;     ("F" web-mode-element-children-fold-or-unfold "Fold/Unfold Element Children")
;;     ("f" web-mode-fold-or-unfold "Fold/Unfold")
;;     ("&" web-mode-whitespaces-show "Toggle Show Whitespace" :toggle t)
;;     ("s" web-mode-element-content-select "Select Content")
;;     ("k" web-mode-element-clone "Clone Element" :color red)

;; );end mode
;; "Action"
;; (
;;     ("r" web-mode-element-wrap "Wrap Element In Tag" )
;;     ("m" web-mode-mark-and-expand "Mark/Expand Region" :color red )
;;     ("E" web-mode-dom-errors-show "Show Errors" )
;;     ("N" web-mode-dom-normalize "Normalize HTML" )
;;     ("B" web-beautify-html "Beautify HTML")
;; ;   ("I" highlight-indent-guides-mode  "Show Indent Guides" :toggle t )
;; );end action

;; "Other"
;; (
;;      ("i" yas-insert-snippet "Insert Snippet" )
;;      ;; ("e" helm-emmet "Insert Emmet Snippet")
;;      ;; ("T" company-try-hard "Cycle Completion Backends")
;;      ("t" indent-for-tab-command "Style Html Region Indent")
;;      ;; ("c" Css-Mode/body      "Css-Mode Interface" :color blue)
;;      ;; ( "R" Javascripts/body "Javascript Interface" :color blue)
;;      ;; ( "w" Web-Development/body "Web Dev Interface" :color blue)
;;      ;; ("h" hydra-helm/body "Return To Helm" :color blue )
;;      ("<SPC>" nil "Quit" :color blue)

;; );end other
;; );end hydra body
;; );end pretty-hydra-web-development
;; (bind-key "<C-m> v" 'Web-Mode/body)

(provide 'sync0-web-mode-functions)
