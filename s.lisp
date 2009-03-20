;;This file is in public domain.

(load "umac.lisp")
(load "umac-basic.lisp")

(in-package #:umac)

(umac ((:return) (:list) (:sum val-0))
  (until (> val-0 10))
  (summing 1)
  (fix-list)
  (collecting val-0 'q) (appending '(a b)))
(umac ((:for i 0 (+ i 1)) (:return) (:list))
  (collecting i)
  (until (> i 10)))

(umac ((:list) (:for-list el (list 1 2 3 4 5 6 7 8)))
  (collecting (+ el 10)))

;This one should fail; extension miauw doesn't exist.
; (Unless you defined it.)
(umac ((:miauw el (list 1 2 3 4 5 6 7 8)))
  (collecting (+ el 10))) ;See comment.


