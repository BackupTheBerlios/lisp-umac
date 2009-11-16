;;Author: Jasper den Ouden
;;This file is in public domain.

(in-package #:vect)

;;Matrices from vectors.
(define-vect matrix22d vect2d (mk-vect2d 0d0 0d0) 2
	     :fun-maker mk-matrix22d)
(define-vect matrix23d vect3d (mk-vect3d 0d0 0d0 0d0) 2
	       :fun-maker mk-matrix23d)
(define-vect matrix32d vect2d (mk-vect2d 0d0 0d0) 3
	       :fun-maker mk-matrix32d)
(define-vect matrix33d vect3d (mk-vect3d 0d0 0d0 0d0) 3
	       :fun-maker mk-matrix33d)

(define-vect matrix22f vect2f (mk-vect2f 0.0 0.0) 2
	     :fun-maker mk-matrix22f)
(define-vect matrix23f vect3f (mk-vect3f 0.0 0.0 0.0) 2
	       :fun-maker mk-matrix23d)
(define-vect matrix32f vect2f (mk-vect2f 0.0 0.0) 3
	       :fun-maker mk-matrix32d)
(define-vect matrix33f vect3f (mk-vect3f 0.0 0.0 0.0) 3
	       :fun-maker mk-matrix33f)

;Index matrices are silly?