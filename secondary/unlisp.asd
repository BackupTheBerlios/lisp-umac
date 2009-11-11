
(use-package '(:asdf))

(defsystem :unlisp
    :description "Reader macros, most of which don't make sense to use. And\
 a macro for binary function notation. Mostly made for the heck of it."
    :components ((:file "unlisp" :depends-on (:scope))))
