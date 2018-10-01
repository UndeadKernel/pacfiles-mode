;;; pacfiles-utils.el --- common utilities of pacfiles-mode -*- lexical-binding: t; -*-

;;; Code:
;;; Commentary:

(defun pacfiles--calculate-merge-file (file path)
  "File name associated to the merge file tied to FILE located in PATH."
  (concat path (substring (secure-hash 'md5 file) 0 10) ".pacmerge"))

(defun pacfiles--add-sudo-maybe (file-path permission)
  "Add \"/sudo::\" to FILE-PATH if the file does not meet the PERMISSION.
FILE-PATH is a variable pointing to a file name.
PERMISSION is either \":read\" or \":write\""
  (let ((predicate (cond ((eq permission :read) #'file-readable-p)
                         ((eq permission :write) #'file-writable-p)
                         (t (user-error "Unknown keyword"))))
        (apt-path file-path))
    (unless (funcall predicate apt-path)
	  (setq apt-path (concat "/sudo::" apt-path))
	  (unless (funcall predicate apt-path)
	    (error "Could not %s \"%s\"" (if (eq permission :read) "read" "write") file-path)))
    apt-path))


(provide 'pacfiles-utils)
;;; pacfiles-utils.el ends here
