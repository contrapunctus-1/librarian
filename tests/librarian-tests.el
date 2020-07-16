;;; -*- lexical-binding: t; -*-
(require 'buttercup)
(require 'librarian)

(describe
 "librarian-file"
 (it "filters out require, provide, and declare-function calls"
     (expect (length (librarian-file "librarian.el"))
             :to-equal 4)
     (expect (length (plist-get (librarian-file "librarian.el") :functions))
             :to-equal 9)
     (expect (length (plist-get (librarian-file "librarian.el") :variables))
             :to-equal 3)))

;; Local Variables:
;; nameless-current-name: "contrasync"
;; End:
