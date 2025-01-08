(require 'sync0-zettelkasten)
(require 'sync0-bibtex-utils)

(defvar sync0-ivy-bibtex-cache nil
  "Cache variable for sync0-ivy-bibtex.")

;; (defun sync0-ivy-bibtex-update-cache ()
;;   "Force update sync0-ivy-bibtex-cache to reflect changes."
;;   (interactive)
;;   (bibtex-completion-init)
;;   (setq sync0-ivy-bibtex-cache (bibtex-completion-candidates)))

;; (defun sync0-bibtex-completion-get-value (field entry)
;;   "Fix a redefinition of bibtex-completion-get-value because it
;; outputs the bibtex fields with the braces {}."
;;       (bibtex-completion-get-value field entry))

;; (defun consult-bibtex--candidates (&optional cands)
;;   "Convert `bibtex-completion' candidates to `completing-read' candidates.
;; CANDS is an optional subset of candidates to convert. When omitted CANDS
;; defaults to all the candidates configured by `bibtex-completion'."
;;   (when (null sync0-ivy-bibtex-cache)
;;     (sync0-ivy-bibtex-update-cache))
;;   (or cands
;;       (setq cands sync0-ivy-bibtex-cache))  ;; Use cached candidates here
;;   (cl-loop
;;    for cand in cands
;;    with cand-str = nil
;;    do (setq cand-str
;;             (concat (citar-get-value "=type=" cand) " "
;;                     (bibtex-completion-format-entry (cdr cand) (1- (frame-width)))))
;;    ;; Add a `consult--type' property for narrowing support.
;;    do (add-text-properties 0 1
;;                            `(consult--type
;;                              ,(or
;;                                (when-let ((type (bibtex-completion-get-value "=type=" cand)))
;;                                  (car (rassoc (capitalize type) consult-bibtex-narrow)))
;;                                (car (rassoc "Other" consult-bibtex-narrow)))
;;                              ;; Symbols are more performant than strings for most situations.
;;                              bib-type ,(intern (capitalize (bibtex-completion-get-value "=type=" cand)))
;;                              consult--candidate ,(bibtex-completion-get-value "=key=" cand)
;;                              has-pdf ,(not (not (bibtex-completion-get-value "=has-pdf=" cand)))
;;                              has-note ,(not (not (bibtex-completion-get-value "=has-note=" cand))))
;;                            ;; The trailing type text is there for matching, it'll be removed by consult.
;;                            cand-str)
;;    collect cand-str))

;; (defun consult-bibtex--read-entry (&optional arg)
;;   "Read a bibtex entry.
;; Optional argument CANDS is the same as for `consult-bibtex--candidates'. ARG
;; causes `bibtex-completion' re-read all bibtex entries from your bibtex files."
;;   ;; (when arg
;;   ;;   (bibtex-completion-clear-cache))
;;   ;; (bibtex-completion-init)
;;   (let* ((candidates (consult-bibtex--candidates))
;;          (preselect
;;           (when-let ((key (bibtex-completion-key-at-point)))
;;             (cl-find-if (lambda (cand)
;;                           (string-equal key (get-text-property 0 'consult--candidate cand)))
;;                         candidates))))
;;     (consult--read candidates
;;                    :prompt "BibTeX entries: "
;;                    :require-match t
;;                    :category 'bibtex-completion
;;                    :lookup 'consult--lookup-candidate
;;                    :default preselect
;;                    :group
;;                    ;; (consult--type-title consult-bibtex-narrow)
;;                    (lambda (cand transform)
;;                      (if transform
;;                          (substring cand (1+ (length (symbol-name (get-text-property 0 'bib-type cand)))))
;;                        (symbol-name (get-text-property 0 'bib-type cand))))
;;                    :narrow
;;                    ;; Allow narrowing on PDFs and notes, alongside just `consult--type'.
;;                    (let ((type-narrow (plist-get (consult--type-narrow consult-bibtex-narrow) :predicate)))
;;                      (list :predicate
;;                            (lambda (cand)
;;                              (when consult--narrow
;;                                (cond
;;                                 ((eq consult--narrow consult-bibtex-pdf-narrow-key)
;;                                  (get-text-property 0 'has-pdf cand))
;;                                 ((eq consult--narrow consult-bibtex-note-narrow-key)
;;                                  (get-text-property 0 'has-note cand))
;;                                 (t (funcall type-narrow cand)))))
;;                            :keys
;;                            `(,@(and consult-bibtex-pdf-narrow-key
;;                                     `((,consult-bibtex-pdf-narrow-key . "With PDFs")))
;;                              ,@(and consult-bibtex-note-narrow-key
;;                                     `((,consult-bibtex-note-narrow-key . "With Notes")))
;;                              ,@consult-bibtex-narrow)))
;;                    :history consult-bibtex-history)))

;; (defun my-consult-bibtex--read-entry (&optional prompt)
;;   "Read a bibtex entry.
;; Optional argument CANDS is the same as for `consult-bibtex--candidates'. ARG
;; causes `bibtex-completion' re-read all bibtex entries from your bibtex files."
;;   (let* ((candidates (consult-bibtex--candidates))
;;          (preselect
;;           (when-let ((key (bibtex-completion-key-at-point)))
;;             (cl-find-if (lambda (cand)
;;                           (string-equal key (get-text-property 0 'consult--candidate cand)))
;;                         candidates)))
;; 	 (my-prompt (or prompt "BibTeX entries: ")))
;;     (consult--read candidates
;;                    :prompt my-prompt
;;                    :require-match t
;;                    :category 'bibtex-completion
;;                    :lookup 'consult--lookup-candidate
;;                    :default preselect
;;                    :group
;;                    ;; (consult--type-title consult-bibtex-narrow)
;;                    (lambda (cand transform)
;;                      (if transform
;;                          (substring cand (1+ (length (symbol-name (get-text-property 0 'bib-type cand)))))
;;                        (symbol-name (get-text-property 0 'bib-type cand))))
;;                    :narrow
;;                    ;; Allow narrowing on PDFs and notes, alongside just `consult--type'.
;;                    (let ((type-narrow (plist-get (consult--type-narrow consult-bibtex-narrow) :predicate)))
;;                      (list :predicate
;;                            (lambda (cand)
;;                              (when consult--narrow
;;                                (cond
;;                                 ((eq consult--narrow consult-bibtex-pdf-narrow-key)
;;                                  (get-text-property 0 'has-pdf cand))
;;                                 ((eq consult--narrow consult-bibtex-note-narrow-key)
;;                                  (get-text-property 0 'has-note cand))
;;                                 (t (funcall type-narrow cand)))))
;;                            :keys
;;                            `(,@(and consult-bibtex-pdf-narrow-key
;;                                     `((,consult-bibtex-pdf-narrow-key . "With PDFs")))
;;                              ,@(and consult-bibtex-note-narrow-key
;;                                     `((,consult-bibtex-note-narrow-key . "With Notes")))
;;                              ,@consult-bibtex-narrow)))
;;                    :history consult-bibtex-history)))

;; (defun sync0-consult-bibtex-batch-action (keys)
;;   "Perform a batch action on a list of BibTeX KEYS."
;;   (let* ((action-name (completing-read "Choose action: " (mapcar #'car sync0-consult-bibtex-action-map)))
;;          (action-fn (cdr (assoc action-name sync0-consult-bibtex-action-map))))
;;     (if action-fn
;;         (funcall action-fn keys)
;;       (message "No valid action selected"))))

;; (defun sync0-consult-bibtex-multi-select (&optional cands)
;;   "Allow multiple selection of BibTeX entries using consult-bibtex--read-entry and return their keys.
;; CANDS is an optional list of candidates, which defaults to the list generated by
;; `consult-bibtex--candidates`."
;;   (interactive)
;;   (let ((candidates (or cands (consult-bibtex--candidates)))  ;; Get all candidates
;;         (selected-keys '())  ;; List to hold selected keys
;;         (done nil))          ;; Flag to indicate when to stop
;;     (while (not done)
;;       (let ((selected (consult-bibtex--read-entry)))
;;         (if selected
;;             (progn
;;               (push selected selected-keys)  ;; Add selected entry to the list
;;               (message "Selected entry: %s" selected))
;;           (setq done t)))  ;; Exit the loop if the user presses C-g or cancels
;;       (when (not (yes-or-no-p "Continue selecting? "))
;;         (setq done t)))  ;; Exit the loop if the user decides not to continue
;;     (message "Final selection: %s" selected-keys)
;;     (sync0-consult-bibtex-batch-action selected-keys)))

;; (defun sync0-bibtex-completion-choose-key (&optional unique pointer query-message use-cache)
;;   "Allow multiple selection of BibTeX entries using consult-bibtex--read-entry and return their keys.
;; CANDS is an optional list of candidates, which defaults to the list generated by
;; `consult-bibtex--candidates`."
;;   (interactive)
;;   (let* ((entry (when (or pointer
;;                           (string= major-mode "bibtex-mode"))
;;                   (save-excursion
;;                     (bibtex-beginning-of-entry)
;;                     (bibtex-parse-entry))))
;;          (preselect (if entry
;; 			(cdr (assoc "=key=" entry))
;; 		      sync0-bibtex-choose-key-cache))
;;          (candidates (if use-cache
;;                          sync0-bibtex-completion-candidate-cache
;;                        (setq sync0-bibtex-completion-candidate-cache (bibtex-completion-candidates))))
;; 	 (my-message (or query-message "Choose BibTeX key: ")))
;;     (if unique
;;         (let ((selection (my-consult-bibtex--read-entry my-message)))
;; 	  (progn 
;; 	    (message "Selected key: %s" selection)
;; 	    (setq sync0-bibtex-choose-key-cache selection)))
;;       (let ((selected-keys '())  ;; List to hold selected keys
;;             (done nil))          ;; Flag to indicate when to stop
;; 	(while (not done)
;; 	  (let ((selected (my-consult-bibtex--read-entry my-message)))
;;             (if selected
;; 		(progn
;; 		  (push selected selected-keys)  ;; Add selected entry to the list
;; 		  (message "Selected entry: %s" selected))
;;               (setq done t)))  ;; Exit the loop if the user presses C-g or cancels
;; 	  (when (not (yes-or-no-p "Continue selecting? "))
;;             (setq done t)))  ;; Exit the loop if the user decides not to continue
;; 	(message "Selected keys: %s" selected-keys)
;; 	selected-keys))))

;; (defun sync0-update-bibtex-authors ()
;;   (interactive)
;;   (let ((current-authors)
;;         (final-list)
;;         (bibtex-authors  (append
;;                           (mapcar #'(lambda (x) (cdr (assoc "editor" x)))
;;                                   (bibtex-completion-candidates))
;;                           (mapcar #'(lambda (x) (cdr (assoc "author" x)))
;;                                   (bibtex-completion-candidates)))))
;;     (with-temp-buffer
;;       (insert-file-contents "~/.emacs.d/sync0-vars/bibtex-authors.txt")
;;       (goto-char (point-min))
;;       ;; (keep-lines "contexts" (point-min) (point-max)) 
;;       (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
;;         (push  (match-string 1) current-authors)))
;;     (with-temp-file
;;         "~/.emacs.d/sync0-vars/bibtex-authors.txt"
;;       (setq final-list (delete-dups (append current-authors bibtex-authors)))
;;       (sync0-insert-elements-of-list final-list))))


(provide 'sync0-ivy-bibtex)
