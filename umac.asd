
;;Author: Jasper den Ouden
;;This file is in public domain.
(defpackage :umac-asd
  (:use :common-lisp :asdf))

(in-package :umac-asd)

;;TODO testing?
(defsystem :umac
    :description "Extendable looping/accumulating/declaring macro."
    :components ((:file "umac")
		 (:file "umac-basic" :depends-on ("umac"))))

(defsystem :scope
    :description "Scoped variables/functions."
    :components ((:file "scope")))

(defsystem :unlisp
    :description "Reader macros, which don't make sense. And a macro for binary function notation."
    :components ((:file "unlisp" :depends-on (:scope))))

(defsystem :generic
    :description "General purpose utility macros/functions."
    :components ((:file "generic")))
