;;; pacfiles-win.el --- Window related functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to manage the windows of pacfiles-mode
;;
;;; Code:

(require 'subr-x)
(require 'ediff)

(defvar pacfiles--files-buffer-name "*pacfiles:file-list*"
  "Name of the window that holds the list of pacman files.")

(defcustom pacfiles-merge-file-tmp-location "/tmp/"
  "Location of temporary merged filed."
  :type '(string)
  :group 'pacfiles)

(defvar pacfiles--previous-window-confs '()
  "The window configuration before `pacfiles' is called.")

(defvar pacfiles--empty-buffer-name "*pacfiles:empty-buffer*"
  "Empty buffer meant to replace buffers killed in EDIFF windows.
Doing this replacement avoids having multiple windows open with the same buffer.
Having the same buffer open in multiple windows might break the proper killing
of EDIFF windows.")

(defun pacfiles--display-buffer-fullscreen (buffer alist)
  "Display BUFFER fullscreen taking ALIST into account."
  (when-let (window (or (display-buffer-reuse-window buffer alist)
                        (display-buffer-same-window buffer alist)
                        (display-buffer-pop-up-window buffer alist)
                        (display-buffer-use-some-window buffer alist)))
    (delete-other-windows window)
    window))

(defun pacfiles--push-window-conf ()
  "Push the current window configuration to later be restored
by `pacfiles--restore-window-conf'."
  (let ((win-conf (current-window-configuration)))
    (push win-conf pacfiles--previous-window-confs)))

(defun pacfiles--pop-window-conf ()
  "Restore the first window configuration found
in `pacfiles--previous-window-confs'."
  (if pacfiles--previous-window-confs
    (condition-case nil
        (progn
          (let ((win-conf (pop pacfiles--previous-window-confs)))
            (set-window-configuration win-conf)))
      (error "Window configuration could not be restored"))
    (error "No window configurations to restore")))

(defun pacfiles--clean-after-ediff ()
  "Kill buffers that ediff has left behind. Ask user if merged file is modified."
  (let ((window-a ediff-window-A)
        (window-b ediff-window-B)
        (window-c ediff-window-C)
        (empty-buffer (get-buffer-create pacfiles--empty-buffer-name)))
    ;; Save the merged buffer if the user wants and kill it
    (when window-c ; window-c is nil when diffing instead or merging
      (save-excursion
        (select-window window-c t) ; buffer-c is made current
        (when (and (buffer-modified-p)
                   (y-or-n-p (format "'%s' was modified. Save before killing? " (buffer-name))))
          (with-file-modes #o700
            (save-buffer)))
        (set-buffer-modified-p nil) ; buffer set to "not modified" to kill it without asking the user.
        (kill-buffer)
        (switch-to-buffer empty-buffer)))
    ;; Kill file-a and file-b always. We want to explicitly set the current buffer
    ;; ... to make sure that no function in `kill-buffer-query-functions' stops us.
    (save-excursion
      (select-window window-a t) ; this makes buffer-a current
      (switch-to-buffer empty-buffer))
    (save-excursion
      (select-window window-b t) ; this makes buffer-b current
      (switch-to-buffer empty-buffer))))

(defun pacfiles--create-view-buffer (name file)
  "Show FILE in a new buffer named NAME that pacfiles will manage."
  (let ((view-buffer
         (get-buffer-create (concat "*pacfiles:merge-" name "*"))))
    (with-current-buffer view-buffer
      (insert-file-contents file nil nil nil t)
      (read-only-mode)
      view-buffer)))

(provide 'pacfiles-win)
;;; pacfiles-win.el ends here
