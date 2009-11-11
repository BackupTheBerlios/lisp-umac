
(use-package '(:asdf))

(defsystem :vect
    :description "Vectors made with classes, rather silly scheme in\
 retrospect, depending how much you trust simple-array to be efficiently\
 implemented. It is a little slow to load/compile.
It is cute how matrices are an extension of vectors with vector components,
 and where inproducts are matrix multiplication though."
    :components ((:file "vect" :depends-on (:iterate))
		 (:file "vect-number") (:file "vect-define")
		 (:file "specific") (:file "matrix")))
