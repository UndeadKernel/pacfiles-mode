;;; pacfiles-buttons.el --- the buttons of pacfiles-mode --- -*- lexical-binding: t; -*-

;;; Commentary:
;; Definitions that deal with buttons and their fonts.
;;
;;; Code:

(defgroup pacfiles-button-faces nil
  "Faces for the buttons used in pacfiles-mode."
  :group 'pacfiles)

(defface pacfiles--apply-all
  '((t (:inherit 'button :height 1.3)))
  "Face for the Apply All button."
  :group 'pacfiles-button-faces)

(defface pacfiles--discard-all
  '((t (:inherit 'button :height 1.3)))
  "Face for the Apply All button."
  :group 'pacfiles-button-faces)

(defface pacfiles--discard
  '((t (:inherit 'warning :weight bold :underline t)))
  "Face for the Apply All button."
  :group 'pacfiles-button-faces)

(defface pacfiles--delete
  '((t (:inherit 'error :weight bold :underline t)))
  "Face for the Apply All button."
  :group 'pacfiles-button-faces)


(define-button-type 'pacfiles--button-apply-all
  'face 'pacfiles--apply-all
  'follow-link t)

(define-button-type 'pacfiles--button-discard-all
  'face 'pacfiles--discard-all
  'follow-link t)

(define-button-type 'pacfiles--button-apply
  'face 'button
  'follow-link t)

(define-button-type 'pacfiles--button-discard
  'face 'pacfiles--discard
  'follow-link t)

(define-button-type 'pacfiles--button-delete
  'face 'pacfiles--delete
  'follow-link t)

(define-button-type 'pacfiles--button-generic
  'face 'button
  'follow-link t)


(defvar pacfiles-activate-no-confirm nil
  "Do not ask for user input when applying or discarding a merged file.")

(defvar pacfiles--inhibit-button-revert nil
  "Clicking a button does not revert the pacfiles list buffer.")

(defun pacfiles--insert-merge-button (file-pair)
  "Insert a button to merge FILE-PAIR.

To determine the file-pair against which FILE will be merged, the extension of
FILE is removed."
  (let* ((update-file (car file-pair))
         (base-file (file-name-sans-extension update-file)))
    (if (file-exists-p base-file)
        (progn
          ;; Insert button that merges two files.
          (insert-text-button "[merge]"
                              'help-echo (format "Start merging '%s' and '%s'."
                                                 (file-name-nondirectory update-file)
                                                 (file-name-nondirectory base-file))
                              'action `(lambda (_)
                                         (ediff-merge-files ,update-file ,base-file nil
                                                            ;; location of the merged file-pair
                                                            ,(cdr file-pair)))
                              'type 'pacfiles--button-generic)
          (insert " "))
      ;; The base file doesn't exist.
      ;; Insert button that just copies the update to the merge file.
      (insert-text-button "[merge]"
                            'help-echo (format "Merge '%s'."
                                               (file-name-nondirectory update-file))
                            'action `(lambda (_)
                                       (when (y-or-n-p
                                              (format "Base file '%s' not found. Use '%s' as is? "
                                                      ,base-file ,update-file))
                                         (copy-file ,update-file ,(cdr file-pair))
                                         (when (not pacfiles--inhibit-button-revert) (revert-buffer t t))))
                            'type 'pacfiles--button-generic)
      (insert " "))))

(defun pacfiles--insert-view-merge-button (file-pair)
  "Insert a button that displays the merge in FILE-PAIR."
  (let* ((file-update (car file-pair))
         (file-base (file-name-sans-extension file-update))
         (file-merge (cdr file-pair)))
    (insert-text-button "[view]"
                        'help-echo (format "View the merge of '%s' with '%s'."
                                           (file-name-nondirectory file-update)
                                           (file-name-nondirectory file-base))
                        'action `(lambda (_)
                                   (let ((window (split-window-right)))
                                     (select-window window)
                                     (set-window-buffer window
                                      (pacfiles--create-view-buffer
                                       (file-name-nondirectory ,file-base) ,file-merge))))
                        'type 'pacfiles--button-generic)
    (insert " ")))

(defun pacfiles--insert-diff-button (file-update)
  "Insert a button that displays a diff of the update FILE-UPDATE and its base file."
  (let ((file-base (file-name-sans-extension file-update)))
    (if (file-exists-p file-base)
        (progn
          (insert-text-button "[diff]"
                              'help-echo (format "Diff '%s' with '%s'."
                                                 (file-name-nondirectory file-update)
                                                 (file-name-nondirectory file-base))
                              'action `(lambda (_) (ediff-files ,file-update ,file-base))
                              'type 'pacfiles--button-generic)
          (insert " "))
      ;; Replace the diff button with spaces
      (insert "       "))))

(defun pacfiles--insert-apply-button (file-pair)
  "Insert a button that copies the `cdr' of FILE-PAIR to its `car'."
  (let* ((merge-file (cdr file-pair))
        (update-file (car file-pair))
        (destination-file (file-name-sans-extension update-file)))
    (insert-text-button "[apply]"
                        'help-echo (format "Apply the merge of '%s' and '%s' to the file system."
                                           (file-name-nondirectory update-file)
                                           (file-name-sans-extension (file-name-nondirectory update-file)))
                        'action `(lambda (_)
                                   (when (or pacfiles-activate-no-confirm
                                             (y-or-n-p (format "Apply the merge and overwrite '%s'? "
                                                               ,destination-file)))
                                     ;; Copy and keep the destination file's permissions and user/group
                                     (let* ((dst-file (pacfiles--add-sudo-maybe ,destination-file :write))
                                            (dst-attrs (file-attributes dst-file 'integer))
                                            (dst-uid (file-attribute-user-id dst-attrs))
                                            (dst-gid (file-attribute-group-id dst-attrs))
                                            (dst-mode (file-modes dst-file)))
                                       (copy-file ,merge-file dst-file t)
                                       (set-file-modes dst-file dst-mode)
                                       (tramp-set-file-uid-gid dst-file dst-uid dst-gid))
                                     ;; Delete the merge and update files
                                     (delete-file (pacfiles--add-sudo-maybe ,merge-file :write))
                                     (delete-file (pacfiles--add-sudo-maybe ,update-file :write))
                                     (when (not pacfiles--inhibit-button-revert) (revert-buffer t t))
                                     (message "Merge applied!")))
                        'type 'pacfiles--button-apply)
    (insert " ")))

(defun pacfiles--insert-discard-button (file-pair)
  "Insert button that deletes the `cdr' of FILE-PAIR from the file system."
  (let ((merge-file (cdr file-pair))
        (update-file (car file-pair)))
    (insert-text-button "[discard]"
                        'help-echo (format "Delete the merge of '%s' from the file system."
                                           (file-name-sans-extension (file-name-nondirectory update-file)))
                        'action `(lambda (_)
                                   (let ((del-file (pacfiles--add-sudo-maybe ,merge-file :write)))
                                     (when (or pacfiles-activate-no-confirm
                                               (y-or-n-p (format  "Discard the merge between '%s' and '%s'? "
                                                                  ,update-file
                                                                  ,(file-name-sans-extension update-file))))
                                       (delete-file del-file)
                                       (message "Merge discarded!")))
                                   (when (not pacfiles--inhibit-button-revert) (revert-buffer t t)))
                        'type 'pacfiles--button-discard)
    (insert " ")))

(defun pacfiles--insert-delete-button (file-pair)
  "Insert a button that deletes the file in the `car' of FILE-PAIR."
  (let ((update-file (car file-pair)))
    (insert-text-button "[delete]"
                        'help-echo (format "Delete '%s' from the file system."
                                           (file-name-nondirectory update-file))
                        'action `(lambda (_)
                                   (when (y-or-n-p (format "Delete '%s' permanently? "
                                                           ,update-file))
                                     (delete-file (pacfiles--add-sudo-maybe ,update-file :write))
                                     (message "File deleted!"))
                                   (when (not pacfiles--inhibit-button-revert) (revert-buffer t t)))
                        'type 'pacfiles--button-delete)
    (insert " ")))

(defun pacfiles--insert-footer-buttons ()
  "Insert the `apply all' and `discard all' buttons."
  (insert-text-button "[Apply All]"
                      'help-echo "Write all merged files into the system."
                      'action (lambda (_)
                                (pacfiles--activate-all-buttons 'pacfiles--button-apply "apply"))
                      'type 'pacfiles--button-apply-all)
  (insert "  ")
  (insert-text-button "[Discard All]"
                      'help-echo "Discard all merged files."
                      'action (lambda (_)
                                (pacfiles--activate-all-buttons 'pacfiles--button-discard "discard"))
                      'type 'pacfiles--button-discard-all))

(defun pacfiles--activate-all-buttons (activate-type action-name)
  "Find all buttons with button type ACTIVATE-TYPE and activate them.
Use ACTION-NAME to display an appropriate warning message."
  (when (y-or-n-p (concat (capitalize action-name) " all merged files? "))
    (save-excursion
      (goto-char (point-min))
      ;; Catch errors that `forward-button' might throw.
      (condition-case nil
          (let* ((pacfiles-activate-no-confirm t) ; do not ask the user
                 (pacfiles--inhibit-button-revert t)
                 (button (forward-button 1 nil nil))
                 (type (button-type button)))
            ;; Iterate until we find the first footer button.
            (while (not (eq type 'pacfiles--button-apply-all))
              (when (eq type activate-type)
                (button-activate button))
              (setq button (forward-button 1 nil nil)
                    type (button-type button))))))
    (message "Done!")
    (revert-buffer t t)))


(provide 'pacfiles-buttons)
;;; pacfiles-buttons.el ends here
