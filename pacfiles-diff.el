;;; pacfiles-diff.el --- Diff handling -*- lexical-binding: t; -*-

(defun pacfiles/diff-current-line ()
  "Diff the files in the line where the point is."
  (interactive)
  (let ((file-a pacfiles/get-current-file)
        (file-b pacfiles/get-current-file ""))))

(provide 'pacfiles-diff)
;;; pacfiles-diff.el ends here
