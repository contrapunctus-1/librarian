;;; librarian.el --- generate function references for Emacs Lisp code -*- lexical-binding: t; -*-

;;; Commentary:
;;

(defun librarian ()
  (let* ((project-name  (file-name-nondirectory
                         (directory-file-name default-directory)))
         (project-files (directory-files default-directory nil
                                         (format "^%s \\.el$" project-name))))
    (list project-name project-files)))

(provide 'librarian)

;;; librarian.el ends here
