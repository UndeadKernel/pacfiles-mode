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

(provide 'pacfiles-win)
;;; pacfiles-win.el ends here
