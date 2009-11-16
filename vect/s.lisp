(load "load.lisp")

(in-package #:vect)

(setf *vector-types* nil)

(print 'a)

(in-package #:vect)
(progn
  (setf *vector-types* nil)
  (define-vect vect2d double-float 0d0 2 :fun-maker mk-vect2d))

(vect-as-list(v* (mk-vect2i 2.0 1.0) 1 2 4))

(vect-as-list(v* (mk-vect 2d0 1d0) 1 2 4))

(vect-as-list(v+ (mk-vect (mk-vect 4d0 0d0) (mk-vect 0d0 0d0))
		 (mk-vect (mk-vect 0d0 1d-2) (mk-vect 0d0 0d0))))


(all-combinations '(a b c d e f g) 4 (lambda (list) (print list)))