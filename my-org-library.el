;;; -*- lexical-binding: t -*-
(require 'dash)
(require 's)

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

(defun --my-set-and-update-parent-todo-statistics ()
  "Create statistics cookie if doesn't exist, then update."
  (outline-back-to-heading)
  (let* ((heading-title (nth 4 (org-heading-components)))
         (cleaned-heading-title (replace-regexp-in-string "\\[.*\\]" "" heading-title))
         (cookied-title (replace-regexp-in-string "$" " [0/0]" cleaned-heading-title)))
    (org-edit-headline cookied-title))
  (org-update-statistics-cookies nil))

(defun my-convert-to-checkbox (beginning end)
  "Convert a bullet list under region to a checkbox list."
  (interactive "r")
  (replace-regexp "^\\( *-\\)" "\\1 [ ]" nil beginning end)
  (--my-set-and-update-parent-todo-statistics))

(defun my-mass-estimate (beginning end)
  "Set the effort of multiple headings."
  (interactive "r")
  (let* ((allowed (org-property-get-allowed-values nil org-effort-property t))
         (must-match
	  (and allowed
	       (not (get-text-property 0 'org-unrestricted
				       (caar allowed)))))
         (effort (completing-read "Effort: " allowed nil must-match)))
    (save-excursion
      (org-map-entries (lambda ()
                         (org-set-effort nil effort)
                         (outline-hide-subtree))
                       t 'region-start-level)))
  ;; go back to beginning of region
  (goto-char (min beginning end)))

(defun my-clock-in ()
  "Clock in and set task to active."
  (interactive)
  (org-clock-in)
  ;; Record the state we'll want to switch back to when marking task
  ;; inactive (but not done)
  (org-set-property "Pre-active-state" (org-get-todo-state))
  (org-todo "ACTIVE"))

(defun my-clock-out ()
  "Clock out and set task to previously active state."
  (interactive)
  (org-clock-out)
  ;; If previous state is set and not an empty string, use that, otherwise use TODO
  (let* ((previous-state (org-entry-get nil "Pre-active-state"))
         (todo-state (or (and (not (-contains? '(nil "") previous-state))
                              previous-state)
                         "TODO")))
    (org-todo todo-state)))

(defun my-make-percent-progress-checklist ()
  "Insert percent progress checklist under heading at point."
  (interactive)
  (end-of-line)
  (let ((checklist (--reduce-from (concat acc (format "- [ ] %s%%\n" it))
                                  "\n" '(10 20 30 40 50 60 70 80 90 100))))
    (insert (s-chomp checklist))
    (--my-set-and-update-parent-todo-statistics)))

(provide 'my-org-library)

