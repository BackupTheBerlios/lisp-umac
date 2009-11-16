
(cl:in-package :cl-user)

(in-package :denest)

;Since it is entirely equivalent to making a macro, it is scanned as such.
; except you only use it as macro with use-denest-macro.
(expr-scan:add-scanner-fun 'def-denest-macro #'fun-scanner)
