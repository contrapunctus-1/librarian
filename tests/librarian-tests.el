;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'librarian)

(describe
 "librarian-file"
 (it "filters out require, provide, and declare-function calls"
     (expect (length (librarian-file "librarian.el"))
             :to-equal 14)))

;; Local Variables:
;; nameless-current-name: "contrasync"
;; End:
