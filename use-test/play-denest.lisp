
(load "denest.lisp")

(use-package '(:denest))

(denest (let ((a 2) (b 5)))
        (flet ((sqr (x) (* x x)))
	  (+ a (sqr b))))

(denest* (let (a 2) (b 5))
         (:* flet ((sqr (x) (* x x)))
	     (+ a (sqr b))))

(denest (collecting (nil list)) (summing (0 a))
	(:return (values a list)) ;Do the below, finally returning this.
	(dolist (el (list 1 2 3 4))
	  (summing el)
	  (collecting a)))

(denest 
 (return-accumulate* ((collecting) 1 (summing)))
 (dolist (el (list 1 2 3 4))
   (summing el)
   (collecting (+ el 3))))

(denest
  (collecting ())
  (:integer-block ((i 0 10) (j 0 10)))
  (:until ((and (= i 5) (= j 5))))
  (collecting (list i j)))

(denest (minimizing ())
        (dolist (el (list 1 2 -5 23 -4 5 364 -224 46))
	  (minimizing el)))

(denest (summing ())
        (:on-list ((a '(1 2 3 4)) (b '(5 6 7 8)))
	  (summing a b)))

(let ((a 1)) ;You can do it a little crazy too!
  (denest
    (setf a) (+ a)
    (* 2 2))
  a)

(maximizing ()
  (dolist (a '(1 2 3 4 5 6 7 214  577  74 1 1 23 5 32 64 73))
    (maximizing a)))



