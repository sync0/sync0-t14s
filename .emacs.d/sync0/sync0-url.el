
(use-package browse-url
  :straight nil
  :custom
  (browse-url-browser-function 'browse-url-default-browser)
  (browse-url-chrome-program "google-chrome-stable")
  (browse-url-generic-program "google-chrome-stable"))

(defun sync0-visit-urls-in-browser (urls)
  "Visit each URL in the default web browser."
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (dolist (url urls)
      (browse-url url))))

(defun sync0-visit-urls-in-browser-from-region-or-buffer ()
  "Visit URLs from the active region or the buffer in the default web browser."
  (interactive)
  (let ((urls (if (use-region-p)
                  (split-string (buffer-substring (region-beginning) (region-end)) "\n" t)
                (split-string (buffer-string) "\n" t))))
    (sync0-visit-urls-in-browser urls)))

(provide 'sync0-url)
