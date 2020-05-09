;; Daily planning

;; Use this command to save the current window state to the specfied file
;; (with-temp-file
;;   "~/.emacs.d/my_libraries/daily_planning.winstate"
;;   (print (window-state-get (frame-root-window) t) (current-buffer)))

(defun my-start-daily-planning ()
  "Restore window state for daily planning."
  (interactive)
  (find-file "~/Dropbox/workspaces/emacs/Organization/projects.org")
  (find-file "~/Dropbox/workspaces/emacs/Organization/someday.org")
  (find-file "~/Dropbox/workspaces/emacs/Organization/one_offs.org")
  (find-file "~/Dropbox/workspaces/emacs/Organization/daily_planning.org")
  (org-agenda-list)
  (with-temp-buffer
    (insert-file-contents
     (locate-user-emacs-file "my-libraries/daily_planning.winstate"))
    (window-state-put (read (current-buffer))) (frame-root-window)))

(provide 'my-window-states-library)
