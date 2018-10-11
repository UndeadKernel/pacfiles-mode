;;; pacfiles-mode.el --- pacnew and pacsave merging tool -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Carlos G. Cordero
;;
;; Author: Carlos G. Cordero <http://github/UndeadKernel>
;; Maintainer: Carlos G. Cordero <pacfiles@binarycharly.com>
;; Created: Oct 11, 2018
;; Modified: Oct 11, 2018
;; Version: 1.0
;; Keywords: files pacman arch pacnew pacsave update linux
;; URL: https://github.com/UndeadKernel/pacfiles-mode
;; Package-Requires: ((emacs "26") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `pacfiles-mode' is an Emacs major mode to manage `.pacnew` and `.pacsave`
;; files left by Arch's pacman. To merge files, *pacfiles-mode* automatically
;; creates an Ediff merge session that a user can interact with. After finishing
;; the Ediff merge session, *pacfiles-mode* cleans up the mess that Ediff leaves
;; behind. *pacfiles-mode* also takes care of keeping the correct permissions of
;; merged files, and requests passwords (with TRAMP) to act as root when needed.
;;
;; Start the major mode using the command `pacfiles' or `pacfiles/start'.
;;
;;; Code:

(require 'pacfiles-buttons)
(require 'pacfiles-utils)
(require 'pacfiles-win)

(require 'cl-seq)
(require 'ediff)
(require 'outline)
(require 'time-date)

(defgroup pacfiles nil "Faces for the buttons used in pacfiles-mode."
  :group 'tools)

(defvar pacfiles-updates-search-command "find /etc -name '*.pacnew' -o -name '*.pacsave' 2>/dev/null"
  "Command to find .pacnew files.")

(defvar pacfiles--merge-search-command
  (concat "find " pacfiles-merge-file-tmp-location " -name '*.pacmerge' 2>/dev/null")
  "Command to search for temporarily merged files.")

(defvar pacfiles--ediff-conf '()
  "Alist that stores ediff variables and its values.")


;;;###autoload
(defalias 'pacfiles 'pacfiles-start)

;;;###autoload
(defun pacfiles-start ()
  "Find and manage pacman backup files in an Arch-based GNU/Linux system."
  (interactive)
  ;; Save the current window configuration so that it can be restored when we are finished.
  (pacfiles--push-window-conf)
  ;; Save ediff varaibles that we modify to later restore them to the uers's value.
  (pacfiles--save-ediff-conf)
  (let ((buffer (get-buffer-create pacfiles--files-buffer-name)))
    (display-buffer buffer '(pacfiles--display-buffer-fullscreen))
    (with-current-buffer buffer
      (pacfiles-mode)
      (pacfiles-revert-buffer t t))))

(defun pacfiles-quit ()
  "Quit ‘pacfiles-mode’ and restore the previous window and ediff configuration."
  (interactive)
  (pacfiles--restore-ediff-conf)
  ;; Kill buffers we create which start with '*pacfiles:'
  (kill-matching-buffers "^\\*pacfiles:.*" t t)
  (pacfiles--pop-window-conf))

;; Main function that displays the contents of the PACFILES buffer.
(defun pacfiles-revert-buffer (&optional _ignore-auto noconfirm)
  "Populate the ‘pacfiles-mode’ buffer with .pacnew and .pacsave files.

Ignore IGNORE-AUTO but take into account NOCONFIRM."
  (interactive)
  (with-current-buffer (get-buffer-create pacfiles--files-buffer-name)
    (when (or noconfirm
              (y-or-n-p (format "Reload list of backup pacman files? ")))
      (run-hooks 'before-revert-hook)
      ;; The actual revert mechanism starts here
      (let ((inhibit-read-only t)
            (files (split-string (shell-command-to-string pacfiles-updates-search-command) "\n" t))
            (merged-files (split-string (shell-command-to-string pacfiles--merge-search-command) "\n" t))
            (pacnew-alist (list))
            (pacsave-alist (list)))
        (delete-region (point-min) (point-max))
        (insert "* PACFILES MODE" "\n")
        ;; Split .pacnew and .pacsave files
        (dolist (file files)
          ;; Associate each FILE in FILES with a file to hold the merge
          (let ((merge-file (pacfiles--calculate-merge-file file pacfiles-merge-file-tmp-location)))
            (cond
             ((string-match-p ".pacnew" file)
              (push (cons file merge-file) pacnew-alist))
             ((string-match-p ".pacsave" file)
              (push (cons file merge-file) pacsave-alist))
             (t (user-error (format "Cannot process file %s" file))))))
        ;; --- Process .pacnew files ---
        (insert "\n\n" "** PACNEW files" "\n")
        (insert "\n" "*** pending" "\n")
        ;; Display the .pacnew files that need merging
        (pacfiles--insert-pending-files pacnew-alist merged-files)
        (insert "\n" "*** merged" "\n")
        ;; Display .pacnew files that have an associated merge file.
        (pacfiles--insert-merged-files pacnew-alist merged-files)
        ;; --- Process .pacsave files ---
        (insert "\n\n" "** PACSAVE files" "\n")
        (insert "\n" "*** pending" "\n")
        ;; Display the .pacsave files that need merging
        (pacfiles--insert-pending-files pacsave-alist merged-files)
        (insert "\n" "*** merged" "\n")
        (pacfiles--insert-merged-files pacsave-alist merged-files)
        (insert "\n\n")
        (pacfiles--insert-footer-buttons))))
  (goto-char 0))

;;;###autoload
(defun pacfiles-revert-buffer-no-confirm ()
    "Revert the pacfiles list buffer without asking for confirmation."
    (interactive)
    (pacfiles-revert-buffer t t))

(defun pacfiles--insert-pending-files (files-alist merged-files)
  "Insert files in FILES-ALIST if their `cdr' is not in MERGED-FILES.

The FILE-TYPE specifies which type of update file we are processing."
  ;; Keep files in FILES-ALIST which don't have a cdr in MERGED-FILES.
  (let ((pending-alist (cl-remove-if (lambda (i) (member (cdr i) merged-files)) files-alist)))
    (if (null pending-alist)
        (insert (propertize "--- no pending files ---\n" 'font-lock-face 'font-lock-comment-face))
      (dolist (file-pair pending-alist)
        (pacfiles--insert-merge-button file-pair)
        (pacfiles--insert-diff-button (car file-pair))
        (pacfiles--insert-delete-button file-pair)
        (insert (car file-pair) " ")
        (pacfiles--insert-days-old (car file-pair))
        (insert "\n")))))

(defun pacfiles--insert-merged-files (files-alist merged-files)
  "Insert files in FILES-ALIST that have an associated file in MERGED-FILES."
  (let ((merged-alist (cl-remove-if-not (lambda (i) (member (cdr i) merged-files)) files-alist)))
    (if (null merged-alist)
        (insert (propertize "--- no merged files ---\n" 'font-lock-face 'font-lock-comment-face))
      (dolist (file-pair merged-alist)
        (pacfiles--insert-apply-button file-pair)
        (pacfiles--insert-view-merge-button file-pair)
        (pacfiles--insert-discard-button file-pair)
        (insert (car file-pair) " ")
        ;; calculate how many days old is the merged file
        (pacfiles--insert-days-created (cdr file-pair))
        (insert "\n")))))

(defun pacfiles--insert-days-old (file)
  "Insert how many days passed between FILE and FILE without its extension.

If REVERSE-ORDER is non-nil, calculate the time difference as
\(FILE without extension\) - FILE."
  (let* ((base-file (file-name-sans-extension file))
         (time-base-file
          (time-to-seconds (file-attribute-modification-time (file-attributes base-file))))
         (time-file
          (time-to-seconds (file-attribute-modification-time (file-attributes file))))
         (reverse-time (< time-file time-base-file)))
    (when (file-exists-p base-file)
        (insert
         (propertize
          (format "(%.1f %s)"
                  (time-to-number-of-days
                   (cond (reverse-time (time-subtract time-base-file time-file))
                         (t            (time-subtract time-file time-base-file))))
                  (if reverse-time "day[s] old" "day[s] ahead"))
          'font-lock-face 'font-lock-warning-face)))))

(defun pacfiles--insert-days-created (file)
  "Insert the number of days since FILE was created."
  (if (file-exists-p file)
      (let ((time-file (file-attribute-modification-time (file-attributes file))))
        (insert
         (propertize
          (format "(%.1f day[s] since created)" (time-to-number-of-days (time-since time-file)))
          'font-lock-face 'font-lock-string-face)))
    (error "File '%s' dosn't exist" file)))

(defun pacfiles--save-ediff-conf ()
  "Save ediff variables we modify with the user's current values.
We restore the saved variables after ‘pacfiles-mode’ quits."
  (require 'ediff)
  (let ((vars-to-save
         '(ediff-autostore-merges ediff-keep-variants ediff-window-setup-function
           ediff-before-setup-hook ediff-quit-hook ediff-cleanup-hook ediff-quit-merge-hook
           ediff-quit-hook ediff-split-window-function)))
    (dolist (var vars-to-save)
            (push (pacfiles--var-to-cons var) pacfiles--ediff-conf))))

(defun pacfiles--change-ediff-conf ()
  "Change ediff's configuration variables to fit ‘pacfiles-mode’."
  (setq ediff-autostore-merges nil
        ediff-keep-variants t
        ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally)
  (add-hook 'ediff-before-setup-hook #'pacfiles--push-window-conf)
  (add-hook 'ediff-quit-hook #'pacfiles--pop-window-conf t)
  (add-hook 'ediff-cleanup-hook #'pacfiles--clean-after-ediff)
  (remove-hook 'ediff-quit-merge-hook #'ediff-maybe-save-and-delete-merge)
  (add-hook 'ediff-quit-hook (lambda () (pacfiles-revert-buffer  t t))))

(defun pacfiles--restore-ediff-conf ()
    "Restore the ediff variables saved by `pacfiles--save-ediff-conf'."
    (dolist (pair pacfiles--ediff-conf)
      (pacfiles--cons-to-var pair))
    (setq pacfiles--ediff-conf '()))

(defvar pacfiles-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")       #'pacfiles-quit)
    (define-key map (kbd "g")       #'pacfiles-revert-buffer-no-confirm)
    (define-key map (kbd "r")       #'pacfiles-revert-buffer-no-confirm)
    (define-key map (kbd "TAB")     #'outline-toggle-children)
    (define-key map (kbd "C-c C-p") #'outline-previous-heading)
    (define-key map (kbd "C-c C-n") #'outline-next-heading)
    (define-key map (kbd "n")       #'forward-button)
    (define-key map (kbd "p")       #'backward-button)
    map)
  "Keymap for ‘pacfiles-mode’.")

;; Tell emacs that, when creating new buffers, pacfiles-mode should not be used
;; ... as the major mode.
(put 'pacfiles-mode 'mode-class 'special)

;;;###autoload
(define-derived-mode pacfiles-mode outline-mode "pacfiles"
  :syntax-table nil
  :abbrev-table nil
  "Major mode for managing .pacnew and .pacsave files."
  ;; If the buffer is not the one we create, do nothing and error out.
  (unless (string= (buffer-name) pacfiles--files-buffer-name)
    (user-error "Use the command `pacfiles' instead of `pacfiles-mode' to start pacfiles-mode"))
  ;; The buffer shall not be edited.
  (read-only-mode)
  ;; No edits... no undo.
  (buffer-disable-undo)
  ;; Disable showing parents locally by letting the mode think it's disabled.
  (setq-local show-paren-mode nil)
  (setq show-trailing-whitespace nil)
  ;; Disable lines numbers.
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  ;; Set the function used when reverting pacfile-mode buffers.
  (setq-local revert-buffer-function #'pacfiles-revert-buffer)
  ;; configure ediff
  (pacfiles--change-ediff-conf)
  ;; configure outline-mode
  (setq-local outline-blank-line t))


(provide 'pacfiles-mode)
;;; pacfiles-mode.el ends here
