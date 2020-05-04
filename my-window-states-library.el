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
     (locate-user-emacs-file "my_libraries/daily_planning.winstate"))
    (window-state-put (read (current-buffer))) (frame-root-window)))


(defvar --winstack-stack '()
  "A Stack holding window configurations.
Use `my-winstack-push' and
`my-winstack-pop' to modify it.")

(defun my-winstack-push()
  "Push the current window configuration onto `--winstack-stack'."
  (interactive)
  (if (and (window-configuration-p (car --winstack-stack))
           (compare-window-configurations (car --winstack-stack)
					  (current-window-configuration)))
      (message "Current config already pushed")
    (progn (push (current-window-configuration) --winstack-stack)
           (message (concat "pushed "
			    (number-to-string
                             (length (window-list (selected-frame))))
			    " frame config")))))

(defun my-winstack-pop()
  "Pop the last window configuration off `--winstack-stack' and apply it."
  (interactive)
  (if (car --winstack-stack)
      (progn (set-window-configuration (pop --winstack-stack))
             (message "popped"))
    (message "End of window stack")))

(provide 'my-window-states-library)
