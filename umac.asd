
;;Author: Jasper den Ouden
;;This file is in public domain.

(defpackage :umac-asd
  (:use :common-lisp :asdf))

(in-package :umac-asd)

(defsystem :generic
    :description "General purpose utility macros/functions."
    :components ((:file "generic")))

(defsystem :denest
    :description "Get unneeded nesting out of your code.\
 Turns out to also be very useful in use in the same sense that LOOP,\
 Iterate or umac-like macros. (Much better then LOOP at least)

Also has some 'internal' macros that make use of the keywords. These can\
 be used directly with USE-DENEST-MACRO."
    :components ((:file "denest")))

(defsystem :umac
    :description "Extendable looping/accumulating/declaring macro.
In my opinion :denest is much better! Use that!"
    :components ((:file "umac")
		 (:file "umac-basic" :depends-on ("umac"))))

(defsystem :scope
    :description "Scoped variables/functions.
Denest can do it somewhat similarly."
    :components ((:file "scope")))

(defsystem :unlisp
    :description "Reader macros, most of which don't make sense to use. And\
 a macro for binary function notation."
    :components ((:file "unlisp" :depends-on (:scope))))

