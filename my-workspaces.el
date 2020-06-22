(require 'workgroups2)

(defvar --my-saved-workgroup)

(defun my-save-session-org-link ()
  "Save session for specified workgroup, allowing the insertion of an org link
using `my-insert-session-org-link'."
  (interactive)
  ;; Close treemacs first to avoid any problems with double side windows
  (if (and (fboundp 'treemacs-get-local-window) (treemacs-get-local-window)) (treemacs))
  (let ((workgroup-name (if (y-or-n-p "Use an existing workgroup?")
			    (wg-read-workgroup-name)
			  (wg-read-new-workgroup-name))))

    (wg-get-workgroup-create workgroup-name)
    (setq --my-saved-workgroup workgroup-name)
    (wg-save-session)
    (message (format "Workgroup session saved for workgroup %s." workgroup-name))))

(defun my-insert-session-org-link ()
  "Insert an org link that will load the workgroup session that was active when
`my-save-session-org-link' was called."
  (interactive)
  (let ((link-label "Load session")
	(link-action (format "(progn (wg-get-workgroup \"%s\") (wg-open-session (locate-user-emacs-file \".emacs_workgroups\")))" --my-saved-workgroup)))
    (insert (format "[[elisp:%s][%s]]" link-action link-label))))

(provide 'my-workspaces)
