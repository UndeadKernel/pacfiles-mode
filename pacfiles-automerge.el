;;; pacfiles-automerge.el --- Auto-Merging related functions -*- lexical-binding: t; -*-
;;; Commentary:
;; Functions to manage the Auto-Merging functions of pacfiles
;;
;;; Code:

(require 'pacfiles-utils)
(require 'pacfiles-win)

(declare-function pacfiles-revert-buffer-no-confirm "pacfiles-mode" ())

(defvar pacfiles--automerge-alist
  '((:regexp "/etc/pacman\\.d/mirrorlist\\.pacnew$"
     :mergefn #'pacfiles--automerge-mirrorlist)))

;;;###autoload
(defun pacfiles--automerge-available (update-file)
  "Return the function that knows how to merge UPDATE-FILE if it exists.

UPDATE-FILE searches for matches in `pacfiles--automerge-alist'. If a match is
found, return a function, otherwise return nil."
  (let ((case-fold-search nil) ; do not ignore case with `string-match-p'
        (automerge-alist pacfiles--automerge-alist)
        (merge-available nil))
    (while (and automerge-alist (not merge-available))
      (when (string-match-p (plist-get (car automerge-alist) :regexp) update-file)
        (setq merge-available (plist-get (car automerge-alist) :mergefn)))
      (setq automerge-alist (cdr automerge-alist)))
    merge-available))

;; -----------------------------------------------------------------------
;; --- Functions that auto-merge the files in `pacfiles--automerge-alist'.

;;;###autoload
(defun pacfiles--automerge-mirrorlist (base-file update-file)
  "Automatically merge the mirrorlist in BASE-FILE into UPDATE-FILE."
  (let ((base-buffer (find-file-noselect base-file))
        (update-buffer (find-file-noselect update-file)))
    ;; Check if reflector was used to generate mirrorlist.
    (if (with-current-buffer base-buffer
          (goto-char (point-min))
          (search-forward "generated by Reflector" nil t))
        (message "Automerge not possible. Use reflector to recreate the mirrorlist.")
      ;; Do the merging of the mirrorlist.
      (let ((servers (pacfiles--find-mirrorlist-servers base-buffer)))
        (if (not servers)
            ;; No servers selected, copy the update as is
            (progn
              (with-current-buffer (pacfiles--create-mirrorlist-merge-buffer
                                    update-file update-buffer)
                (save-buffer))
              (message "There are no mirrorlist servers to merge. Update copied over."))
          ;; Servers found, use them in a new file
          (pacfiles--merge-mirrorlist-servers servers update-file update-buffer))
        (pacfiles-revert-buffer-no-confirm)))))

;;;###autoload
(defun pacfiles--find-mirrorlist-servers (base-buffer)
  "Find the activated servers (i.e., those uncommented) in BASE-BUFFER."
  (let ((servers '()))
    (with-current-buffer base-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\([^#\n].*\\)\n" nil t)
          (push (match-string 1) servers))))
    (reverse servers)))

;;;###autoload
(defun pacfiles--create-mirrorlist-merge-buffer (mirrorlist-update-file update-buffer)
  "Ready up the merge buffer for the mirrorlist.

The MIRRORLIST-UPDATE-FILE is the path of the original mirrorlist. UPDATE-BUFFER
is the buffer with the updated mirrorlist. Return the buffer of the merged file."
    (let* ((merge-file
            (pacfiles--set-remote-path-maybe
             (pacfiles--calculate-merge-file mirrorlist-update-file
                                             pacfiles-merge-file-tmp-location)))
           (merge-buffer (find-file-noselect merge-file t)))
      (with-current-buffer merge-buffer
        (erase-buffer)
        (insert-buffer-substring update-buffer)
        merge-buffer)))

;;;###autoload
(defun pacfiles--merge-mirrorlist-servers (servers mirrorlist-update-file update-buffer)
  "Automatically merge the mirrorlist update in MIRRORLIST-UPDATE-FILE.

Merge the uncommented lines in SERVERS that exist in UPDATE-BUFFER."
  (let ((servers-status-alist '()))
    ;; Search for the status of the old servers in the update mirrorlist.
    (with-current-buffer update-buffer
      (save-excursion
        (while servers
          (let ((server (pop servers)))
            (goto-char (point-min))
            (push `(,server . ,(re-search-forward server nil t)) servers-status-alist)))))
    (setq servers-status-alist (reverse servers-status-alist))
    ;; Merge the servers into a merge file.
    (with-current-buffer (pacfiles--create-mirrorlist-merge-buffer
                          mirrorlist-update-file update-buffer)
      (erase-buffer)
      (insert-buffer-substring update-buffer)
      (goto-char (point-min))
      ;; Insert the existing servers after the first blank line.
      (if (not (seq-some (lambda (s) (cdr s)) servers-status-alist))
          (message "No valid servers were found")
        (re-search-forward "^[[:blank:]]*$")
        (open-line 2)
        (forward-line)
        (insert "## pacfiles: Automerged servers\n")
        (insert
         (string-join
          (mapcar #'car (seq-filter (lambda (s) (cdr s)) servers-status-alist))
          "\n")))
      ;; Report about non-existing servers after the first blank line.
      (when (seq-some (lambda (s) (not (cdr s))) servers-status-alist)
        (re-search-forward "^[[:blank:]]*$")
        (open-line 2)
        (forward-line)
        (insert "## pacfiles: Servers no longer available\n")
        (insert
         (string-join
          (mapcar (lambda (s) (concat "#" (car s)))
                  (seq-remove (lambda (s) (cdr s)) servers-status-alist))
          "\n")))
      (save-buffer))))

(provide 'pacfiles-automerge)
;;; pacfiles-automerge.el ends here
