;;Author: Jasper den Ouden
;;This file is in public domain.

(in-package #:vect)

;;Regular number version.
(defmethod r* ((x number) (y number))
  (* x y))

;Addition and substraction.
(eval (let*((vars '(v1 v2 v3 v4 v5 v6))
	     (args (iter (for v in vars)
			 (collect `(,v number)))))
	 `(progn ,@(iter (for n from 2)
			 (for r+ in '(r+2 r+3 r+4 r+5 r+6))
			 (for r- in '(r-2 r-3 r-4 r-5 r-6))
			 (appending `((defmethod ,r+ (,@(subseq args 0 n))
					(+ ,@(subseq vars 0 n)))
				      (defmethod ,r- (,@(subseq args 0 n))
					(+ ,@(subseq vars 0 n)))))))))
;Inproduct.
(defmethod r-inpr ((a number) (b number))
  (* a b))
