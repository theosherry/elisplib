;; Daily planning

;; Use this command to save the current window state to the specfied file
;; (with-temp-file
;;   (locate-user-emacs-file "my-libraries/daily_planning.winstate")
;;   (print (window-state-get (frame-root-window) t) (current-buffer)))

(setq --theo-lib-path (file-name-directory load-file-name))

(defun my-start-daily-planning ()
  "Restore window state for daily planning."
  (interactive)
  (find-file (expand-file-name "projects.org" *org-directory*))
  (find-file (expand-file-name "someday.org" *org-directory*))
  (find-file (expand-file-name "tickler.org" *org-directory*))
  (find-file (expand-file-name "inbox.org" *org-directory*))
  (find-file (expand-file-name "reference.org" *org-directory*))
  (org-agenda-list)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "daily_planning.winstate" --theo-lib-path))
    (window-state-put (read (current-buffer))) (frame-root-window)))

(provide 'my-window-states-library)
