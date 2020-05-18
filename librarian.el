;;; librarian.el --- generate function reference documentation for Emacs Lisp code -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: docs,lisp,tools
;; Homepage: https://github.com/contrapunctus-1/librarian
;; Package-Requires: ((emacs "25.1") (cask "0.8.4"))
;; Version: 0.1.0

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;; This is something for which one always thinks, "Oh, that can be
;; done with <hack with existing tool>." And then you realize, no, not
;; robustly or reliably, it can't.

;;; Code:

(require 'cask)

(defvar librarian-project-files-function #'librarian-project-files)

(defun librarian-project-files (project-dir)
  "Return files from PROJECT-DIR.
It makes certain assumptions - the name of the directory is also
the prefix of the files."
  (let* ((project-name  (file-name-nondirectory
                         (directory-file-name default-directory)))
         (project-files (thread-last
                            (directory-files default-directory nil
                                             (format "^%s.*\\.el$" project-name))
                          (--mapcar (or (string-match-p "-autoloads\\.el" it)
                                        (string-match-p "-pkg\\.el" it))))))))

(defun librarian-project-files-cask (project-dir)
  "Return files for project from PROJECT-DIR by using Cask."
  (cask-files
   (cask-setup default-directory)))

(defun librarian ()
  "Find definitions"
  (let*
    (list project-name project-files)))

(provide 'librarian)

;;; librarian.el ends here
