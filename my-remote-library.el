(defun my-ssh (alias)
  "ssh into the machine specified by the alias. It is assumed that the alias is fully
configured in ~/.ssh/config"
  (interactive "MRemote alias: ")
  (let ((default-directory (format "/ssh:%s:" alias))
	(explicit-shell-file-name "/bin/bash"))
    (shell)))

(defun my-ssh-sudo-open (alias)
  "Open a remote file on machine specified by alias and its configuration in ~.ssh/config."
  (interactive "MRemote alias: ")
  (let ((prepend-s (format "/ssh:%s|sudo::" alias)))
    (minibuffer-with-setup-hook
	(lambda ()
	  (delete-minibuffer-contents)
	  (insert prepend-s))
      (call-interactively #'find-file))))
    
(provide 'my-remote-library)
