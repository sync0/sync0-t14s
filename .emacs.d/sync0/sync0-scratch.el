(defun sync0-create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun sync0-create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(setq initial-scratch-message ";; 
;; « Ces bonnes gens qui dorment tranquilles, c'est drôle!
;; Patience! un nouveau 89 se prépare! On est las de constitutions,
;; de chartes, de subtilités, de mensonges! Ah! si j'avais un
;; journal ou une tribune, comme je vous secouerais tout cela! Mais,
;; pour entreprendre n'importe quoi, il faut de l'argent! Quelle
;; malédiction que d'être le fils d'un cabaretier et de perdre sa
;; jeunesse à la quête de son pain! »
;;
;; Gustave Flaubert
;; L'éducation sentimentale (1885)
;; "
)

(provide 'sync0-scratch)
