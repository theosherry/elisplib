;; Keep my utility functions here.

(defun my-cut-selected-text (beginning end)
  "Cut the selected region and return it."
  (interactive "r")
  (if (use-region-p)
      (let ((region (buffer-substring beginning end)))
	(delete-region beginning end)
	region)
    ""))

(defun my-tag-word-or-region (text-begin text-end)
  "Surround current word or region with given text. Taken from https://emacs.stackexchange.com/a/3500/26829"
  (interactive "sStart tag: \nsEnd tag: ")
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (progn
          (goto-char (region-end))
          (insert text-end)
          (goto-char (region-beginning))
          (insert text-begin))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (goto-char (cdr bds))
        (insert text-end)
        (goto-char (car bds))
        (insert text-begin))))) 

(provide 'my-util-library)
