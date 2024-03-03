(require 'sync0-bibtex-vars)

(defun sync0-bibtex-update-var (field)
    "Update variables used for completion based on the information
   provided by the new entry."
     (when-let* ((my-list (assoc field sync0-bibtex-completion-variables-list))
                (new-object  (eval (cadr my-list)))
                (completion-var  (caddr my-list))
                (completion-list  (symbol-value completion-var))
                (completion-file   (cadddr my-list)))
      (cond ((member field sync0-bibtex-people-fields) 
             (let (x)
               (cond ((string-match " and " new-object)
                      ;; create a list with parts 
                      (setq x (append (split-string new-object " and ") x)))
                     ;; check when author is an organization
                     ((string-match "^{" new-object)
                      (push  (substring new-object 1 -1) x))
                     ;; other cases
                     (t (push new-object x)))
               (dolist (element x)
                 ;; Check whether the item to be added is already present.
                 (unless (member  element   completion-list)
                   ;; Send the element to the list.
                   (push element completion-list)
                   ;; Update the variable with the bigger list
                   (set completion-var completion-list)
                   ;; Send the element to the file.
                   (append-to-file (concat element "\n") nil completion-file)))))
            ((member field sync0-bibtex-string-multiple-fields)
             (let (x)
               (if (string-match ", " new-object)
                   ;; create a list with parts 
                   (setq x (append (split-string new-object ", ") x))
                 ;; other cases
                 (push new-object x))
               (dolist (element x)
                 (unless (member  element completion-list)
                   ;; Send the element to the list.
                   (push element completion-list)
                   ;; Update the variable with the bigger list
                   (set completion-var completion-list)
                   ;; Send the element to the file.
                   (append-to-file (concat element "\n") nil completion-file)))))
            (t (unless (member  new-object  completion-list)
                 ;; Send the element to the list.
                 (push new-object completion-list)
                 ;; Update the variable with the bigger list
                 (set completion-var completion-list)
                 ;; Send the element to the file.
                 (append-to-file (concat new-object "\n") nil completion-file))))))

  ;; (defun sync0-bibtex-update-vars (seqlists)
  ;;   "Update variables used for completion based on the information
  ;;    provided by the new entry."
  ;;   (dolist (element seqlists)
  ;;     ;; Check whether the item to be added is empty.
  ;;     (unless (or (sync0-null-p  (cadr element))
  ;;                  ;; Check whether the item to be added is already present.
  ;;                  (member (eval (cadr element))  (eval (caddr element))))
  ;;       ;; Send the element to the list.
  ;;       (push (cadr element) (caddr element))
  ;;       ;; Send the element to the file.
  ;;       (append-to-file (concat (eval (cadr element)) "\n") nil (cadddr element)))))

  ;; FIX!
  ;; (defun sync0-bibtex-update-completion-files (seqlists)
  ;;   (interactive)
  ;;   (dolist (element seqlists)
  ;;     (let ((current-elements)
  ;;           (bib-elements (delete-duplicates 
  ;;                          (cond ((string= (car element) "author")
  ;;                                 (let ((my-list (bibtex-completion-candidates))
  ;;                                       aggregate)
  ;;                                   (dolist (current-author my-list aggregate)
  ;;                                     (when-let ((author  (cdr (assoc  "author" current-author))))
  ;;                                       (cond ((string-match " and " author)
  ;;                                              ;; create a list with parts 
  ;;                                             (setq aggregate (append (split-string author " and ") aggregate)))
  ;;                                             ;; check when author is an organization
  ;;                                             ((string-match "^{" author)
  ;;                                              (push  (substring author 1 -1) aggregate))
  ;;                                             ;; other cases
  ;;                                             (t (push author aggregate)))))))
  ;;                                ((string= (car element) "keywords")
  ;;                                 (let ((my-list (bibtex-completion-candidates))
  ;;                                       aggregate)
  ;;                                   (dolist (current-key my-list aggregate)
  ;;                                     (when-let ((keywords  (cdr (assoc  "keywords" current-key))))
  ;;                                       (if (string-match ", " keywords)
  ;;                                           ;; create a list with parts 
  ;;                                          (setq aggregate (append (split-string keywords ", ") aggregate))
  ;;                                         ;; other cases
  ;;                                            (push keywords aggregate))))))
  ;;                                (t (mapcar #'(lambda (x) (cdr (assoc (car element) x)))
  ;;                                           (bibtex-completion-candidates)))))))
  ;;       (with-temp-buffer
  ;;         (insert-file-contents (cadddr element))
  ;;         (goto-char (point-min))
  ;;         ;; (keep-lines "contexts" (point-min) (point-max)) 
  ;;         (while (re-search-forward "^\\([[:print:]]+\\)\n" (point-max) t)
  ;;           (push  (match-string 1) current-elements)))
  ;;       (with-temp-file (cadddr element)
  ;;         (if (string= (car element) "keywords")
  ;;             (set (caddr element) (delete-dups (append current-elements bib-elements bu-keywords-values)))
  ;;           (set (caddr element) (delete-dups (append current-elements bib-elements))))
  ;;         (sync0-insert-elements-of-list (eval (caddr element)))))))

(provide 'sync0-bibtex-var-functions)
