;;; pacfiles-mode.el --- Definition of the pacfiles Major mode -*- lexical-binding: t; -*-

;;; Commentary:
;; The following coding conventions are used:
;;   pacfiles/  : User facing (public) function
;;   pacfiles-  : User facing (public) variable
;;   pacfiles-- : Private function or variable
;;; Code:

(require 'pacfiles-win)
(require 'pacfiles-diff)

(defvar pacfiles-search-command "find /etc -name '*.pacnew' -o -name '*.pacsave' 2>/dev/null"
  "Command to find .pacnew files.")

(defvar pacfiles--merge-search-command
  (concat "find " pacfiles--merge-file-tmp-location "-name '*.pacmerge 2>/dev/null'")
  "Command to search for temporarily merged files.")


(defalias 'pacfiles 'pacfiles/start)
(defun pacfiles/start ()
  "Find and manage pacman backup files in an Arch-based GNU/Linux system."
  (interactive)
  ;; Save the current window configuration so that it can be restored when we are finished.
  (pacfiles--save-window-conf)
  (let ((buffer (get-buffer-create pacfiles--files-buffer-name)))
    (display-buffer buffer '(pacfiles--display-buffer-fullscreen))
    (with-current-buffer buffer
      (pacfiles-mode)
      (pacfiles/revert-buffer t t))))

(defun pacfiles/quit ()
  "Quit pacfiles-mode and restore the previous window configuration."
  (interactive)
  (let ((buffer (get-buffer pacfiles--files-buffer-name)))
    (pacfiles--restore-window-conf)
    (when buffer
      (kill-buffer buffer))))

;; Main function that displays the contents of the PACFILES buffer.
(defun pacfiles/revert-buffer (&optional ignore-auto noconfirm)
  "Populate the pacfiles-mode buffer with .pacnew and .pacsave files.

Ignore IGNORE-AUTO but take into account NOCONFIRM."
  (interactive)
  (with-current-buffer (get-buffer-create pacfiles--files-buffer-name)
    (when (or noconfirm
              (y-or-n-p (format "Reload list of backup pacman files? ")))
      (run-hooks 'before-revert-hook)
      ;; The actual revert mechanism starts here
      (let ((inhibit-read-only t)
            (files (split-string (shell-command-to-string pacfiles-search-command) "\n" t))
            (merged-files (split-string (shell-command-to-string pacfiles--merge-search-command) "\n" t))
            (pacnew-files (list))
            (pacsave-files (list)))
        (delete-region (point-min) (point-max))
        (insert "* PACFILES MODE" "\n")
        ;; Deal With .pacnew and .pacsave files separately
        ;; Split into .pacnew and .pacsave files
        ;; Associate each file with a hash of the file to merge
        (dolist (file files)
          (cond
           ((string-match-p ".pacnew" file) (push file pacnew-files))
           ((string-match-p ".pacsave" file) (push file pacsave-files))
           (t (user-error (format "Cannot process file %s" file)))))
        ;; Process .pacnew files
        (insert "\n\n" "** PACNEW files" "\n")
        (insert "\n" "*** pending merge" "\n")
        ;; Display the .pacnew files that need merging
        (pacfiles--insert-pending-files pacnew-files :pacnew)
        (insert "\n" "*** merged" "\n")
        (pacfiles--insert-merged-files '() :pacnew)
        ;; Process .pacsave files
        (insert "\n\n" "** PACSAVE files" "\n")
        (insert "\n" "*** pending merge" "\n")
        ;; Display the .pacsave files that need merging
        (pacfiles--insert-pending-files pacsave-files :pacsave)
        (insert "\n" "*** merged" "\n")
        (pacfiles--insert-merged-files '() :pacsave)
        (insert "\n\n")
        (pacfiles--insert-footer-buttons))))
  (goto-char 0))

(defun pacfiles--insert-pending-files (pending-file-list file-type)
  "Insert files in PENDING-FILE-LIST that can be merged of the type FILE-TYPE.

The FILE-TYPE argument can be either `:pacnew' or `:pacsave'."
  (if (null pending-file-list)
      (insert (propertize "--- no files found ---" 'font-lock-face 'font-lock-comment-face))
    (dolist (file pending-file-list)
      ;; calculate days old
      (pacfiles--insert-merge-button file)
      (insert " " file " ")
      (pacfiles--insert-days-old file (if (eq file-type :pacnew) nil t))
      (insert "\n"))))

(defun pacfiles--insert-merged-files (merged-file-list file-type)
  "Insert files in MERGED-FILE-LIST that can be applied of type FILE-TYPE."
  (if (null merged-file-list)
      (insert (propertize "--- no files found ---" 'font-lock-face 'font-lock-comment-face))
    (dolist (file merged-file-list)
      ;; calculate days old
      (pacfiles--insert-apply-button file "file.lol")
      (insert " " file " ")
      (pacfiles--insert-days-created file)
      (insert "\n"))))

(defun pacfiles--insert-merge-button (file)
  "Insert a button to merge FILE.

To determine the file against which FILE will be merged, the extension of FILE is removed."
  (let ((base-file (file-name-sans-extension file)))
    (insert-text-button "[merge]"
                        'help-echo (format "Start merging '%s' and `%s'."
                                           (file-name-nondirectory file)
                                           (file-name-nondirectory base-file))
                        'action `(lambda (_) (ediff-merge-files ,file ,base-file nil
                                               ;; location of the merged file
                                               (concat ,pacfiles--merge-file-tmp-location
                                                       ,(file-name-nondirectory base-file)
                                                       ".pacfiles")))
                        'face 'font-lock-keyword-face
                        'follow-link t)))

(defun pacfiles--insert-days-old (file &optional reverse-order)
  "Insert how many days passed between FILE and FILE without its extension.

If REVERSE-ORDER is non-nil, calculate the time difference as
\(FILE without extension\) - FILE."
  (let* ((base-file (file-name-sans-extension file))
         (time-base-file
          (time-to-seconds (file-attribute-modification-time (file-attributes base-file))))
         (time-file
          (time-to-seconds (file-attribute-modification-time (file-attributes file)))))
    (when (file-exists-p base-file)
        (insert
         (propertize
          (format "(%.1f %s)"
                  (time-to-number-of-days
                   (cond ((bound-and-true-p reverse-order) (time-subtract time-base-file time-file))
                         (t (time-subtract time-file time-base-file))))
                  (if reverse-order "day[s] old" "day[s] ahead"))
          'font-lock-face 'font-lock-warning-face)))))

(defun pacfiles--insert-apply-button (merge-file destination-file)
  "Insert a button that copies MERGE-FILE to DESTINATION-FILE."
  (let ((merge-name (file-name-sans-extension merge-file))
        (destination-name (file-name-sans-extension destination-file)))
    (insert-text-button "[merge]"
                        'help-echo (format "Apply `%s' to the file system."
                                           (file-name-nondirectory merge-file))
                        'action `(lambda (_) (message "TODO: Copy `%s' to `%s'" ,merge-name ,destination-name nil))
                        'face 'font-lock-keyword-face
                        'follow-link t)))

(defun pacfiles--insert-days-created (file)
  "Insert the number of days since FILE was created."
  (if (file-exists-p file)
      (let ((time-file (file-attribute-modification-time (file-attributes file))))
        (insert
         (propertize
          (format "(%.1f day[s] since created)" (time-to-number-of-days (time-since time-file)))
          'font-lock-face 'font-lock-string-face)))
    (error "File `%s' dosn't exist" file)))

(defun pacfiles--insert-footer-buttons ()
  "Insert the `apply all' and `discard all' buttons."
  (insert-text-button "[Apply All]"
                      'help-echo "Write all merged files into the system."
                      'follow-link t
                      'face 'font-lock-keyword-face
                      'action (lambda (_) (message "TODO: implement me")))
  (insert "  ")
  (insert-text-button "[Discard All]"
                      'help-echo "Discard all merged files."
                      'follow-link t
                      'face 'font-lock-keyword-face)
  'action (lambda (_) (message "TODO: implement me")))

(defvar pacfiles-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")       #'pacfiles/quit)
    (define-key map (kbd "TAB")     #'outline-toggle-children)
    (define-key map (kbd "C-c C-p") #'outline-previous-heading)
    (define-key map (kbd "C-c C-n") #'outline-next-heading)
    (define-key map (kbd "n")       #'forward-button)
    (define-key map (kbd "p")       #'backward-button)
    map)
  "Keymap for pacfiles-mode.")

(define-derived-mode pacfiles-mode outline-mode "pacfiles"
  :syntax-table nil
  :abbrev-table nil
  "Major mode for managing .pacnew and. pacsave files."
  ;; If the buffer is not the one we create, do nothing and error out.
  (when (not (string= (buffer-name) pacfiles--files-buffer-name))
    (user-error "Use the command `pacfiles' instead of `pacfiles-mode' to start pacfiles-mode")
    (return))
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
  (setq revert-buffer-function #'pacfiles/revert-buffer)
  ;; configure outline-mode
  (setq-local outline-blank-line t))


(provide 'pacfiles-mode)
;;; pacfiles-mode.el ends here
