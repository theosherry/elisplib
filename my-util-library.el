;; Keep my utility functions here.

(defun my-cut-selected-text (beginning end)
  "Cut the selected region and return it."
  (interactive "r")
  (if (use-region-p)
      (let ((region (buffer-substring beginning end)))
	(delete-region beginning end)
	region)
     ""))

(provide 'my-util-library)
