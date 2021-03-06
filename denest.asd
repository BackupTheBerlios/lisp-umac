
(use-package '(:asdf))

(defsystem :denest
    :description "Get unneeded nesting out of your code.\
 Turns out to also be very useful in use in the same sense that LOOP,\
 Iterate or umac-like macros. (Much better then LOOP at least)

Also has some 'internal' macros that make use of the keywords. These can\
 be used directly with USE-DENEST-MACRO."
    :components ((:file "denest")))
