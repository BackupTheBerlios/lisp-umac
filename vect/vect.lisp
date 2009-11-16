;;Author: Jasper den Ouden
;;This file is in public domain.

(defpackage #:vect
  (:use #:common-lisp #:iterate)
  (:export mk-vect define-vect u v w x y z
	   v+ v- v* v/ inpr crosspr lensqr len distsqr dist
	   v-angle angle-v2 angle-v3
	   vect-as-list
	  ;Some specific ones
	   vect2d vect2f vect2i vect3d vect3f vect3i
	   matrix22d matrix32d matrix23d matrix33d
	   matrix22f matrix32f matrix23f matrix33f

	   gl-vertex)
  (:documentation "Vectors/matrices with numbers in them."))

(in-package #:vect)

(defgeneric r* (a b)
  (:documentation "Multiplies two numbers. v* is the macro that users use.\
 There is no r/ it is done via r*."))

(defgeneric r+2 (a b)
  (:documentation "Adds two numbers. v+ is the macro that users use. It 
leads to r+n methods."))
(defgeneric r-2 (a b)
  (:documentation "subtracts two numbers. v+ is the macro that users use.\
 It leads to r+n methods."))

(defgeneric r+3 (a b c) (:documentation "See r+2"))
(defgeneric r+4 (a b c d) (:documentation "See r+2"))
(defgeneric r+5 (a b c d e) (:documentation "See r+2"))
(defgeneric r+6 (a b c d e f) (:documentation "See r+2"))

(defgeneric r-3 (a b c) (:documentation "See r+2"))
(defgeneric r-4 (a b c d) (:documentation "See r+2"))
(defgeneric r-5 (a b c d e) (:documentation "See r+2"))
(defgeneric r-6 (a b c d e f) (:documentation "See r+2"))

(defgeneric mk-vect2 (x y)
  (:documentation "Makes two-dimensional vector, with type inferred from\
 arguments."))

(defgeneric mk-vect3 (x y z)
  (:documentation "Makes three-dimensional vector, with type inferred from\
 arguments."))

(defgeneric mk-vect4 (w x y z)     (:documentation "See mk-vect2"))
(defgeneric mk-vect5 (v w x y z)   (:documentation "See mk-vect2"))
(defgeneric mk-vect6 (u v w x y z) (:documentation "See mk-vect2"))

(defun vect-maker-name (n)
  (case n (2 'mk-vect2) (3 'mk-vect3) (4 'mk-vect4)
	  (5 'mk-vect5) (6 'mk-vect6)))
  
(defmacro mk-vect (&rest args)
  `(,(vect-maker-name (length args)) ,@args))

(defmacro v* (vect &rest factors)
  "Multiplies vect with factors."
  (if (null factors) vect
      `(r* ,vect (v* ,@factors))))

(defmacro v/ (vect &rest dividers)
  "Divides vect with dividers."
  (if (null dividers) vect
      `(r* ,vect (/ (v* ,@dividers)))))

(defmacro v+ (&rest add)
  "Addition macro. Chooses from the r+n functions, chains them if more then\
 6."
  (case (length add)
    (1 (car add))    (2 `(r+2 ,@add)); (3 `(r+3 ,@add))
;    (4 `(r+4 ,@add)) (5 `(r+5 ,@add)) (6 `(r+6 ,@add))
    (t `(v+ (r+2 ,@(subseq add 0 2)) ,@(subseq add 2)))))

(defmacro v- (&rest add)
  "Substraction macro. Chooses from the r-n functions, chains them if\
 more then 6."
  (case (length add)
    (1 (car add))    (2 `(r-2 ,@add)); (3 `(r-3 ,@add))
    (t `(v- (r-2 ,@(subseq add 0 2)) ,@(subseq add 2)))))

;    (4 `(r-4 ,@add)) (5 `(r-5 ,@add)) (6 `(r-6 ,@add))
;    (t `(v- (v-6 ,@(subseq add 0 6)) ,@(subseq add 6)))))


(defgeneric r-inpr (a b) (:documentation "Inproduct between two things.
Used through inpr macro."))

(defgeneric crosspr (a b) (:documentation "Cross product between two\
 things."))

(defmacro inpr (&rest args)
  "Inproduct between things. Usually two."
  (if (null (cddr args))
    `(r-inpr ,@args)
    `(r-inpr ,(car args) (inpr ,@(cdr args)))))

(declaim (inline len lensqr distsqr dist))
(defun lensqr (vect)
  "Length squared of a vector."
  (r-inpr vect vect))

(defun len (vect)
  "Length of a vector."
  (sqrt (lensqr vect)))

(defgeneric distsqr (a b)
  (:documentation "Distance squared between two vectors."))
(defmethod distsqr (a b)
  (lensqr (v- a b)))

(defun dist (a b)
  "Distance between two vectors."
  (sqrt (distsqr a b)))

(defgeneric v-angle (vect) (:documentation "Angles from a vector."))

(defun angle-v2 (angle)
  "Vector from a angle."
  (mk-vect (cos angle) (sin angle)))

(defun angle-v3 (angle up-angle)
  "Vector from two angles, so three-dimensional.
 (Not the usual illogical polar coordinates, height is sin up-angle.)"
  (mk-vect (* (cos angle) (cos up-angle)) (* (sin angle) (cos up-angle))
	   (sin up-angle)))

(defgeneric vect-as-list (vec) (:documentation "Converts a vector to a list\
 for printing and such."))

(when (find-package :gl)
  (defgeneric gl-vertex (vec)
    (:documentation "Enters vertex in GL as a position.")))    
