;;; -*- lexical-binding: t -*-
(require 'dash)
(require 'helm)
(require 'helm-org)
(require 'org-id)

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

(defun --my-heading-from-components (components)
  "Return a basic todo heading from (org-heading-components) result."
  (let* ((level-text (make-string (nth 0 components) ?*))
         (todo (nth 2 components))
         (todo-text (if todo (format "%s " todo) ""))
         (text (nth 4 components)))
    (format "%s %s%s" level-text todo-text text)))

(defun my-gather-org-headings (&optional match id)
  "Gather the org headings in the active buffer. Returns a list of cons,
(--my-heading-from-components . point).

Because this will set potentially multiple ids, it may be slow the first time
it's run on a set of headings.

ARGS

If `match` is provided, only select the headlines specified by the match pattern
(see org-agenda matching).

If `id` is non-nil, return the ID property (which will be set if not present)
instead of point.
"
  (org-map-entries (lambda ()
                     (let ((identifier (if id (org-id-get-create) (point))))
                       (cons (--my-heading-from-components (org-heading-components))
                             identifier)))
                   match nil))

(pp (org-link-escape "T1%20finish%20this"))

(defun --my-org-id-link-from-id (id)
  "Make id link to heading from id. Replaces spaces with underscores to avoid
problems with `org-entry-put-multivalued-property`, which makes use of spaces."
  (org-id-goto id)
  (let ((name  (replace-regexp-in-string "\s" "_"
                                         (nth 4 (org-heading-components)))))
    (format "[[id:%s][%s]]" id name)))

(defvar --my-waiting-ids-prop "MY-WAITING-IDS")
(defvar --my-waiting-links-prop "MY-WAITING-LINKS")
(defvar --my-blocking-ids-prop "MY-BLOCKING-IDS")
(defvar --my-blocking-links-prop "MY-BLOCKING-LINKS")

(defun --my-set-props-on-blocking (blocking-id waiting-id waiting-link)
  "Add the necessary props on the blocking heading when waiting/blocking relationship is created."
  (let* ((pom (cdr (org-id-find blocking-id)))
         (waiting-ids (org-entry-get-multivalued-property pom --my-waiting-ids-prop))
         (new-waiting-ids (-union waiting-ids (list waiting-id)))
         (waiting-links (org-entry-get-multivalued-property pom --my-waiting-links-prop))
         (new-waiting-links (-union waiting-links (list waiting-link))))
    (apply #'org-entry-put-multivalued-property pom --my-waiting-ids-prop new-waiting-ids)
    (apply #'org-entry-put-multivalued-property pom --my-waiting-links-prop new-waiting-links)))

(defun --my-set-props-on-waiting (blocking-id waiting-id)
  "Add the necessary props to the waiting heading when waiting/blocking relationship is created."
  (let* ((pom (cdr (org-id-find waiting-id)))
         (blocking-ids (org-entry-get-multivalued-property pom --my-blocking-ids-prop))
         (new-blocking-ids (-union blocking-ids (list blocking-id)))
         (blocking-link (--my-org-id-link-from-id blocking-id))
         (blocking-links (org-entry-get-multivalued-property pom --my-blocking-links-prop))
         (new-blocking-links (-union blocking-links (list blocking-link))))
    (apply #'org-entry-put-multivalued-property pom --my-blocking-ids-prop new-blocking-ids)
    (apply #'org-entry-put-multivalued-property pom --my-blocking-links-prop new-blocking-links)))

(defun --my-set-heading-to-blocking-factory(waiting-id)
  "Make a function that will handle setting up blocking/waiting relationship for
any blockers + waiting-id."
  (let ((waiting-link (--my-org-id-link-from-id waiting-id))) ;; Create waiting link here to avoid making it multiple times for each invocation of the lambda below.
    (lambda (blocking-id)
      (--my-set-props-on-blocking blocking-id waiting-id waiting-link)
      (--my-set-props-on-waiting blocking-id waiting-id))))

(defun --my-resolve-props-on-waiting (blocking-id waiting-id)
  "Resolve the necessary props on waiting heading when waiting/blocking relationship is resolved."
  (let* ((pom (cdr (org-id-find waiting-id)))
         (blocking-ids (org-entry-get-multivalued-property pom --my-blocking-ids-prop))
         (blocking-links (org-entry-get-multivalued-property pom --my-blocking-links-prop))
         (new-blocking-ids (-remove-item blocking-id blocking-ids))
         (new-blocking-links (--remove-first (string-match blocking-id it) blocking-links)))
    (apply #'org-entry-put-multivalued-property pom --my-blocking-ids-prop new-blocking-ids)
    (apply #'org-entry-put-multivalued-property pom --my-blocking-links-prop new-blocking-links)

    ;; If waiting TODO is no longer being blocked, handle that
    (if (not new-blocking-ids)
        (let ((waiting-text (nth 4 (org-heading-components))))
          (goto-char pom)
          (org-todo "TODO")
          (message (format "Task \"%s\" was waiting on the task just completed and is now ready to be started."
                           (truncate-string-to-width waiting-text 20 nil nil t)))))))

(defun --my-resolve-props-on-blocking (blocking-id waiting-id)
  "Resolve the necessary props on blocking heading when waiting/blocking relationship is resolved."
  (let* ((pom (cdr (org-id-find blocking-id)))
         (waiting-ids (org-entry-get-multivalued-property pom --my-waiting-ids-prop))
         (waiting-links (org-entry-get-multivalued-property pom --my-waiting-links-prop))
         (new-waiting-ids (-remove-item waiting-id waiting-ids))
         (new-waiting-links (--remove-first (string-match waiting-id it) waiting-links)))
    (apply #'org-entry-put-multivalued-property pom --my-waiting-ids-prop new-waiting-ids)
    (apply #'org-entry-put-multivalued-property pom --my-waiting-links-prop new-waiting-links)))


(defun --my-resolve-blocking-to-done-factory(blocking-id)
  "Make a function that wlil handle resolving a blocking/waiting relationship for waiters specified
by waiting-id and blocker specified by blocking-id."
  (lambda (waiting-id)
    (--my-resolve-props-on-waiting blocking-id waiting-id)
    (--my-resolve-props-on-blocking blocking-id waiting-id)))

(defun --my-set-blocking-headings ()
  "Allow user to select blocking tasks, and setup blocking/waiting relationships."
  (let ((waiting-id (org-id-get-create)))
    (cl-flet ((heading-to-blocking (--my-set-heading-to-blocking-factory waiting-id)))
      (let* ((files (list (current-buffer)))
             (sources (helm-org-build-sources files nil nil))
             (candidates (my-gather-org-headings
                          "LEVEL=1|TODO=\"TODO\"|TODO=\"RECURRING\""
                          t))
             (source-helm (helm-build-sync-source "Wait Upon Candidates"
                            :candidates candidates
                            :action (lambda (_)
                                      ;; We want to iterate over all selected candidates
                                      (let ((marked-cands (helm-marked-candidates)))
                                        (dolist (cand marked-cands)
                                          (heading-to-blocking cand)))))))
        (helm :sources source-helm
              :buffer "*helm org inbuffer*")))))

(defun --my-handle-blocking-heading-to-done ()
  "Check if heading marked DONE is blocking, and if so resolve its relationship with
the waiting heading(s)."
  (let* ((id (org-id-get-create))
         (pom (point))
         (waiting-ids (org-entry-get-multivalued-property pom --my-waiting-ids-prop))
         (waiting-links (org-entry-get-multivalued-property pom --my-waiting-links-prop)))
    (if waiting-ids
        (cl-flet ((blocking-to-done (--my-resolve-blocking-to-done-factory id)))
          (-each waiting-ids (lambda (waiting-id)
                               (blocking-to-done waiting-id)))

          ))))

(defun --my-on-heading-to-waiting ()
  (if (y-or-n-p "Select headings to wait upon?")
      (--my-set-blocking-headings)))

(defun --my-on-heading-to-done ()
  (--my-handle-blocking-heading-to-done))

(defun --my-on-todo-change()
  (cond ((equal org-state "WAITING") (funcall '--my-on-heading-to-waiting))
        ((equal org-state "DONE") (funcall '--my-on-heading-to-done))))

(defun my-waiting-init ()
  (add-hook 'org-after-todo-state-change-hook
            '--my-on-todo-change))

(provide 'my-org-library)

