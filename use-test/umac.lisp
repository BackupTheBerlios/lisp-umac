;;Author: Jasper den Ouden
;;This file is in public domain.

(defpackage :umac-test
  (:use :common-lisp :umac))

(in-package :umac-test)

;(in-package :umac)
;Euler problem 6; difference between sum of squares and square of sums.
(defun sqr (x) (* x x))

(defun euler-prob-6 (&optional (n 100))
  (multiple-value-bind (sum sum-sqr)
      (umac ((:sum) (:sum-into val-1)
	     (:for-interval i 1 n))
	(summing i)
	(summing-into val-1 (sqr i)))
    (- (sqr sum) sum-sqr)))

(euler-prob-6)

;Converting to list. (Much better with MAKE-ARRAY though)
(umac ((:for-vect el (vector 1 2 3 4 5 6)) (:sum-into val-1)
       (:list))
  (collecting (summing-into val-1 el)))

(umac ((:sum-into val-1) (:for-interval i 0 10))
  (summing-into val-1 i))
