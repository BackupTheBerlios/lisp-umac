;;Author: Jasper den Ouden
;;This file is in public domain.

(load "scope.lisp")

(load "unlisp.lisp")

(use-package '#:unlisp)

(macroexpand '[a *{b := 3 b}+3])

;Enable some stuff.
(fancy :curly-hooks t)
(fancy := t) (fancy '= t)

(macroexpand '{top
	        x := 6
	        x = 7
	        (sqr x) := { [x*x] }
	        (lala 1)})
