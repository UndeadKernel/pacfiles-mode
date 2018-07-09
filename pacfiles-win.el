;;; pacfiles-win.el --- Window related functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defvar pacfiles--files-buffer-name "*pacfiles-file-list*"
  "Name of the window that holds the list of pacman files.")

(defvar pacfiles--previous-window-conf nil
  "The window configuration before having started `pacfiles'.")

(defun pacfiles//display-buffer-fullscreen (buffer alist)
  "Display BUFFER fullscreen taking ALIST into account."
  (when-let (window (or (display-buffer-reuse-window buffer alist)
                        (display-buffer-same-window buffer alist)
                        (display-buffer-pop-up-window buffer alist)
                        (display-buffer-use-some-window buffer alist)))
    (delete-other-windows window)
    window))

(defun pacfiles//save-window-conf ()
  "Save the current window configuration to later be restored by `pacfiles--restore-windows-conf'."
  (unless (get-buffer-window pacfiles--files-buffer-name (selected-frame))
    (setq pacfiles--previous-window-conf (current-window-configuration))))

(defun pacfiles//restore-window-conf (&optional kill-buffer)
  "Bury or kill pacfiles' buffers according to KILL-BUFFER and restore the previous window configuration."
  (let ((winconf pacfiles--previous-window-conf)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq pacfiles--previous-window-conf nil))))))

(provide 'pacfiles-win)
;;; pacfiles-win.el ends here
