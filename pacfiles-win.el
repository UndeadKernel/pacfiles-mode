;;; pacfiles-win.el --- Window related functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'subr-x)

(defvar pacfiles--files-buffer-name "*pacfiles:file-list*"
  "Name of the window that holds the list of pacman files.")

(defvar pacfiles--merge-file-tmp-location "/tmp/"
  "Location of temporary merged filed.")

(defvar pacfiles--previous-window-confs '()
  "The window configuration before `pacfiles' is called.")

(defun pacfiles--display-buffer-fullscreen (buffer alist)
  "Display BUFFER fullscreen taking ALIST into account."
  (when-let (window (or (display-buffer-reuse-window buffer alist)
                        (display-buffer-same-window buffer alist)
                        (display-buffer-pop-up-window buffer alist)
                        (display-buffer-use-some-window buffer alist)))
    (delete-other-windows window)
    window))

(defun pacfiles--push-window-conf ()
  "Push the current window configuration to later be restored by `pacfiles--restore-window-conf'."
  (let ((win-conf (current-window-configuration)))
    (push win-conf pacfiles--previous-window-confs)))

(defun pacfiles--pop-window-conf ()
  "Restore the first window configuration found in `pacfiles--previous-window-confs'."
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
        (window-c ediff-window-C))
    ;; Save the merged buffer some times and kill it
    (save-excursion
      (select-window window-c t) ; buffer-c is made current
      (when (and (buffer-modified-p)
                 (y-or-n-p (format "'%s' was modified. Save before killing?" (buffer-name))))
        (save-buffer))
      (set-buffer-modified-p nil) ; Set buffer to not modified to not ask user
      (kill-buffer))    ;; Kill file-a and file-b always. We want to explicitly set the current buffer
    ;; ... to make sure that no function in `kill-buffer-query-functions' stops us.
    (save-excursion
      (select-window window-a t) ; buffer-a is made current
      (message "killa %s" (kill-buffer)))
    (save-excursion
      (select-window window-b t) ; buffer-b is made current
      (message "killb %s" (kill-buffer)))))

(provide 'pacfiles-win)
;;; pacfiles-win.el ends here
