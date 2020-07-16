;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'librarian)

(describe
 "librarian-file"
 (it "filters out require, provide, and declare-function calls"
     (expect (hash-table-count (librarian-file "librarian.el"))
             :to-equal 2)
     (expect (length (gethash :functions (librarian-file "librarian.el")))
             :to-equal 9)
     (expect (length (gethash :variables (librarian-file "librarian.el")))
             :to-equal 3)))

;; Local Variables:
;; nameless-current-name: "contrasync"
;; End:
