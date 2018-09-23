;;; pacfiles-mode.el --- Definition of the pacfiles Major mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'pacfiles-win)
(require 'pacfiles-diff)

(defvar pacfiles-search-command "find /etc -name  '*.pacnew' -o -name '*.pacsave' 2>/dev/null"
  "Command to find .pacnew files.")

(defface pacfiles-header
  '((t :foreground "blue"
       :background "aquamarine"
       :weight bold
       :underline nil
       ))
  "Face used for headers."
  :group 'pacfiles-mode)

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

;; Main function that
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
            (files (split-string (shell-command-to-string pacfiles-search-command) "\n" t)))
        (delete-region (point-min) (point-max))
        (insert "* PACFILES MODE")
        ;; Deal With .pacnew and .pacsave files separately
        (let ((pacnew-files (list))
              (pacsave-files (list)))
          ;; Split into .pacnew and .pacsave files
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
          ;; Process .pacsave files
          (insert "\n\n" "** PACSAVE files" "\n")
          (insert "\n" "*** pending merge" "\n")
          ;; Display the .pacsave files that need merging
          (pacfiles--insert-pending-files pacsave-files :pacsave)
          (insert "\n" "*** merged" "\n")))))
  (goto-char 0))

(defun pacfiles--insert-pending-files (file-list file-type)
  "Display the files in FILE-LIST that can be merged of the type FILE-TYPE.

The FILE-TYPE argument can be either `:pacnew' or `:pacsave'."
  (dolist (file file-list)
    ;; calculate days old
    (pacfiles--insert-merge-button file)
    (insert " " file " ")
    (pacfiles--insert-days-old file (if (eq file-type :pacnew) nil t))
    (insert "\n")))

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
  (setq revert-buffer-function #'pacfiles/revert-buffer))


(provide 'pacfiles-mode)
;;; pacfiles-mode.el ends here
