(load "scope.lisp")

(scope (var out)
       (fun collect (x) (setf out `(,@out ,x)))
       (do ((i 0 (+ i 1))) ((> i 10) out)
	 (collect i)))
