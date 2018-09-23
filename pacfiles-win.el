;;; pacfiles-win.el --- Window related functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'subr-x)

(defvar pacfiles--files-buffer-name "*pacfiles:file-list*"
  "Name of the window that holds the list of pacman files.")

(defvar pacfiles--merge-file-tmp-location "/tmp/"
  "Location of temporary merged filed.")

(defvar pacfiles--previous-window-conf nil
  "The window configuration before `pacfiles' is called.")

(defun pacfiles--display-buffer-fullscreen (buffer alist)
  "Display BUFFER fullscreen taking ALIST into account."
  (when-let (window (or (display-buffer-reuse-window buffer alist)
                        (display-buffer-same-window buffer alist)
                        (display-buffer-pop-up-window buffer alist)
                        (display-buffer-use-some-window buffer alist)))
    (delete-other-windows window)
    window))

(defun pacfiles--save-window-conf ()
  "Save the current window configuration to later be restored by `pacfiles//restore-window-conf'."
  (unless pacfiles--previous-window-conf
    (setq pacfiles--previous-window-conf (current-window-configuration))))

(defun pacfiles--restore-window-conf ()
  "Bury or kill pacfiles' buffers according to KILL-BUFFER and restore the previous window configuration."
  (when pacfiles--previous-window-conf
    (condition-case nil
        (progn
          (set-window-configuration pacfiles--previous-window-conf)
          (setq pacfiles--previous-window-conf nil))
      (error (error "Window configuration could not be restored")))))

(provide 'pacfiles-win)
;;; pacfiles-win.el ends here
