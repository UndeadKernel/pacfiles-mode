;;; pacfiles-utils.el --- common utilities of pacfiles-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility functions used throughout pacfiles-mode
;;
;;; Code:

(require 'cl-lib)
(require 'tramp)

(defun pacfiles--calculate-merge-file (file path)
  "Compute the merge file name associated with FILE and place it under PATH."
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
      (setq apt-path
            ;; use different sudo styles depending on wheter the file is local or remote
            (if (file-remote-p apt-path)
                (with-parsed-tramp-file-name apt-path pf
                  (tramp-make-tramp-file-name "sudo" "root" pf-domain pf-host pf-port pf-localname
                                              ;; use the users' METHOD as hop to sudo
                                              (cl-case pf-user
                                                ((nil) (format "%s:%s|" pf-method pf-host))
                                                (t (format "%s:%s@%s|" pf-method pf-user pf-host)))))
              (concat "/sudo::" apt-path)))
      (unless (funcall predicate apt-path)
        (error "Could not %s \"%s\"" (if (eq permission :read) "read" "write") file-path)))
    apt-path))

(defun pacfiles--set-remote-path-maybe (file-path)
  "Change FILE-PATH to be a remote file if `default-directory' is a remote path.

Use the same tramp method used by the user as the remote path."
  (if (file-remote-p default-directory)
      (with-parsed-tramp-file-name default-directory pf
        ;; TODO: adapt the calling convention of this next function
        (tramp-make-tramp-file-name pf-method pf-user pf-domain pf-host pf-port file-path))
    file-path))

(defun pacfiles--var-to-cons (var)
  "Create a cons of the VAR symbol and the VAR value."
  `(,var . ,(symbol-value var)))

(defun pacfiles--cons-to-var (cons)
  "Set the `car' of CONS to the `cdr' of CONS."
  (let ((var (car cons)))
    (set var (cdr cons))))

(provide 'pacfiles-utils)
;;; pacfiles-utils.el ends here
