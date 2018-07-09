;;; pacfiles.el --- Definition of the pacfiles Major mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'pacfiles-win)

(defun pacfiles ()
  "Find and manage pacman backup files in an Arch-based GNU/Linux system."
  ;; Save the current window configuration
  (pacfiles//save-window-conf)
  (let ((buffer (get-buffer-create pacfiles--files-buffer-name)))
    (display-buffer buffer '(pacfiles//display-buffer-fullscreen))
    (with-current-buffer buffer
      (pacfiles-mode))))

(define-derived-mode pacfiles-mode special-mode "pacfiles"
  :syntax-table nil
  :abbrev-table nil
  "Major mode for managing .pacnew and. pacsave files.")


(provide 'pacfiles)
;;; pacfiles.el ends here
