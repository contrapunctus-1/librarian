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

(defun librarian-form-to-org (form))

(defun librarian-backend-org (forms)
  "Convert FORMS to documentation in Org markup.
FORMS should be a list of s-expressions.")

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
        (cond ((librarian-form-internal-p form) :internal-functions)
              ((seq-find (lambda (subform)
                           (and (consp subform)
                                (eq (car subform) 'interactive)))
                         form)
               :commands)
              (t :functions)))
    ('defconst :constants)
    ('defvar
      (if (librarian-form-internal-p form)
          :internal-variables
        :variables))
    ('defcustom :custom-variables)
    (_ :misc)))

(defun librarian-file (&optional file)
  "Return Lisp forms from a file, filtered through `librarian-filters'.
Return value is a hash table, with keywords (as returned by
`librarian-form-category') as keys and lists of Lisp forms as
values."
  (let ((buffer (if file (find-file-noselect file) (current-buffer)))
        (table  (make-hash-table)))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (--> (cl-loop with expr
               while (setq expr (ignore-errors (read buffer)))
               when (| expr librarian-filter-list)
               collect it)
             (cl-loop for form in it
               do (let* ((kw        (librarian-form-category form))
                         (old-forms (gethash kw table)))
                    (puthash kw
                             (if old-forms
                                 (append old-forms
                                         (list form))
                               (list form))
                             table))
               finally return table))))))

(provide 'librarian)

;;; librarian.el ends here
