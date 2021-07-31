
(setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "_")
        (case-fn . downcase)))

(setq deft-strip-summary-regexp
      (concat "\\("
              "[\n\t]" ;; blank
              "\\|^:[[:ascii:]]+:.*$" ;; org-mode properties string
              "\\|^#\\+[[:alpha:]_]+.*$" ;; org-mode metadata
              "\\|^Origin:.*$" ;; Origin string
              "\\|^\\[\\[file:.*$" ;; org-mode inline-images
              ;; org-mode properties  
              ;; "\\|:PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; 
              "\\)"))

(defun deft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((key (file-name-nondirectory file))
           (text (deft-file-contents file))
           (title (deft-file-title file))
           (summary (deft-file-summary file))
           (fixed (replace-regexp-in-string  "\\[\\[[[:alnum:][:space:]-:]+\\]\\[\\([[:graph:][:space:]]+\\)\\]\\]" "\\1" summary)) 
           (reduced (if (> (length fixed) 500)
                        (substring-no-properties fixed nil 500)
                      fixed))
           (new-string (string-join (split-string-every reduced 70) "\n"))
           (mtime (when deft-time-format
                    (format-time-string deft-time-format (deft-file-mtime file))))
           (mtime-width (deft-string-width mtime))
           (line-width (- deft-window-width mtime-width))
           (title-width (min line-width (deft-string-width title)))
           (summary-width (min (deft-string-width summary)
                               (- line-width
                                  (length deft-separator)))))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face 'deft-title-face
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
                     :notify (lambda (widget &rest ignore)
                               (deft-open-file (widget-get widget :tag)))
                     (if title (truncate-string-to-width title title-width)
                       deft-empty-file-title))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'deft-time-face)))
      (when (> summary-width 0)
        (widget-insert "\n")
        (widget-insert (propertize new-string 
                                   'face 'deft-summary-face)))
      (widget-insert "\n"))))

(defhydra sync0-hydra-deft-functions (:color amaranth :hint nil :exit t)
  "
        ^Deft^
        ^------------------
        _n_: New file
        _f_: Filter
        _c_: Clear filter
        _d_: Delete file
                                                   
        [q] Quit
             "
  ("f" deft-filter)
  ("c" deft-filter-clear)
  ("n" deft-new-file)
  ("d" deft-delete-file)
  ("q" nil :color blue))

(evil-leader/set-key-for-mode 'deft-mode "z" 'sync0-hydra-deft-functions/body)

(provide 'sync0-deft)
