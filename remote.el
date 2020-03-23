(defun my-ssh (alias)
  "ssh into the machine specified by the alias. It is assumed that the alias is fully
configured in ~/.ssh/config"
  (interactive "MRemote alias: ")
  (let ((default-directory (format "/ssh:%s:" alias))
	(explicit-shell-file-name "/bin/bash"))
    (shell)))

(provide 'my-ssh)
