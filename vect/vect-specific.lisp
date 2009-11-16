;;Author: Jasper den Ouden
;;This file is in public domain.

(in-package #:vect)

;;Matrices from vectors.
(define-vect vect2d double-float 0d0 2 :fun-maker mk-vect2d)
(define-vect vect2f single-float 0.0 2 :fun-maker mk-vect2f)
(define-vect vect2i fixnum       0   2 :fun-maker mk-vect2i)

(define-vect vect3d double-float 0d0 3 :fun-maker mk-vect3d)
(define-vect vect3f single-float 0.0 3 :fun-maker mk-vect3f)
(define-vect vect3i fixnum       0   3 :fun-maker mk-vect3i)
