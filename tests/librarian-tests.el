;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'librarian)

(describe
 "librarian-file"
 (it "filters out require, provide, and declare-function calls"
     (expect (length (librarian-file "librarian.el"))
             :to-equal 11)
     (expect (seq-count (lambda (form)
                          (equal (car form) "Function"))
                        (librarian-file "librarian.el"))
             :to-equal 8)
     (expect (seq-count (lambda (form)
                          (equal (car form) "Variable"))
                        (librarian-file "librarian.el"))
             :to-equal 3)))

;; Local Variables:
;; nameless-current-name: "contrasync"
;; End:
