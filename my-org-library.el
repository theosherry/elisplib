(require 'my-util-library  "my-util-library.el")

(defun my-time-note (beginning end title)
  "In org-mode, insert a heading with a specified title and current timestamp.
If a region is selected, this function will wrap that region in a header."
  (interactive "r\nMNote title: ")
  (org-insert-heading)
  (insert title " -- " (current-time-string))
  (newline)
  (if (use-region-p)
      (progn 
	(insert (my-cut-selected-text beginning end))
	(deactivate-mark)
	(org-previous-visible-heading 1)
	(org-cycle)
	(beginning-of-line))
    (end-of-line)))

(defun my-time-stamp ()
  "Insert a 'Date added' time stamp."
  (interactive)
  (insert "Date added: " (current-time-string)))

(defun my-mobile-org-sync ()
  "Pull and then push to org-mobile."
  (message "Starting mobile-org sync...")
  (org-agenda-list)
  (org-mobile-pull)
  (org-mobile-push))

(provide 'my-org-library)
