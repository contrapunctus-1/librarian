;;; librarian.el --- generate reference documentation for Emacs Lisp code -*- lexical-binding: t; -*-

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
;;; Commentary:
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;; This is something for which one always thinks, "Oh, that can be
;; done with <hack with existing tool>." And then you realize, no, not
;; robustly or reliably, it can't.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dash)

(defun | (var functions)
  "Assuming FUNCTIONS is a list of functions (FN-1 FN-2 ... FN-N),
return the result of (FN-N ... (FN-2 (FN-1 VAR)))"
  (dolist (fn functions)
    (setq var (funcall fn var)))
  var)

(defun librarian-org-print-form (index type form)
  "Return a string using INDEX, TYPE, and FORM.
FORM is a Lisp form."
  (let ((elt-2 (nth 2 form)))
    (format "%s. %s - %s %s\n"
            index
            type
            (nth 1 form)
            (pcase (car form)
              ('defun (if elt-2 elt-2 "()"))
              (_ "")))))

(defun librarian-backend-org (alist)
  "Convert LIST to documentation in Org markup.
LIST should be a list returned by `librarian-file'."
  (let ((buffer (get-buffer-create "*librarian-output-org*")))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (cl-loop
        with index = 1
        for (type form) in alist
        initially do (insert (format "* Reference\n"))
        do (->> (librarian-org-print-form index type form)
                (insert))
        (cl-incf index))
      (org-mode))))

(defvar librarian-current-backend #'librarian-backend-org
  "Function to be used to transform code to documentation.")

(defun librarian-default-filter (form)
  "Default filter run with each FORM from a source file.
Return non-nil if the FORM is to be documented."
  ;; Remove `require', `provide', `declare-function'...
  (pcase (car form)
    ((or 'require 'provide 'declare-function 'autoload)
     nil)
    ;; ...and `defvar' without a value
    ((and 'defvar
          (guard (= 2 (length form))))
     nil)
    (_ form)))

(defvar librarian-filter-list '(librarian-default-filter)
  "Filters to be run on each form in source file.
The form returned is what is used in the output, so this function
can modify the forms, too.")

(defun librarian-default-sort (form1 form2)
  "Return t if `car' of FORM1 is less than `car' of FORM2 in lexicographic order."
  (string-lessp (symbol-name (car form1))
                (symbol-name (car form2))))

(defvar librarian-sort-function #'librarian-default-sort)

(defun librarian-form-internal-p (form)
  "Return non-nil if FORM is an internal definition.
Currently, this is decided by the presence of -- in its name."
  (string-match-p "--" (symbol-name (nth 1 form))))

(defun librarian-form-category (form)
  "Return definition category of FORM as a keyword.
Possible return values are :internal-functions, :command,
:functions, :constants, :internal-variables, :variables, and
:custom-variables."
  (pcase (car form)
    ('defun
        (cond ((librarian-form-internal-p form) "Internal Function")
              ((seq-find (lambda (subform)
                           (and (consp subform)
                                (eq (car subform) 'interactive)))
                         form)
               "Command")
              (t "Function")))
    ('defconst "Constant")
    ('defvar
      (if (librarian-form-internal-p form)
          "Internal Variable"
        "Variable"))
    ('defcustom "Custom Variable")
    ('define-derived-mode "Mode")
    (_ "Misc")))

(defun librarian-file (&optional file)
  "Return Lisp forms from a file, filtered through `librarian-filters'.
Return value is a list, where each element is a list with a
string (as returned by `librarian-form-category') as the first
element and the Lisp form as the second element."
  (let ((buffer (if file (find-file-noselect file) (current-buffer)))
        (table  (make-hash-table)))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (--> (cl-loop with expr
               while (setq expr (ignore-errors (read buffer)))
               when (| expr librarian-filter-list)
               collect (cons (librarian-form-category it) (list it))))))))

(provide 'librarian)

;;; librarian.el ends here
