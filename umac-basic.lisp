(in-package #:umac)

;;Accumulation-like
(def-umac :list (&optional (list-into 'ret) initial)
  "Listing stuff; collecting, appending"
  `((:let (,list-into ,initial))
    (:flet (collecting (&rest collected)
	     (setf- append ,list-into collected))
           (appending (&rest appended)
	     (dolist (a appended)
	       (setf- append ,list-into a))))))

(def-umac :sum (&optional (sum-onto 'ret) (initial 0))
  "Summing onto a variable; summing"
  `((:let  (,sum-onto ,initial))
    (:mlet (summing (&rest added)
	     `(setf- + ,',sum-onto ,@added)))))

(def-umac :ops (&optional (onto 'ret) initial)
  "Changing stuff with any operation."
  `((:let (,onto ,initial))
    (:mlet (op (op-name &rest args)
	     `(setf- ,op-name ,,onto ,@args)))))

;;End condition
(def-umac :return ()
  "Returning; until, while. WARNING uses (return), will behave such!"
  `((:mlet (force-return (returned)
	     `(setf ret ,returned))
           (until (&rest and)
	     `(when (and ,@and) (return)))
	   (while (&rest and)
	     `(unless (and ,@and) (return))))))

(def-umac :once ()
  "Return after single run of umac.(Overrides others that say loop.)"
  `((:post (return))))

;;Iteration.
(def-umac :for (iter start change)
  `((:let (,iter ,start)) (:post (setf ,iter ,change))))

(def-umac :for-interval (i from to)
  `((:for ,i ,from (+ ,i 1)) (:post (unless (< i ,to) (return)))))

(def-umac :repeat (count &optional (i (gensym)))
  `((:for-interval ,i 0 ,count)))

(def-umac :for-list (var list &optional (end-cond :stop) (iter (gensym)))
  "An iterator over a list. Set end-cond to :continue to not stop when \
list runs out."
  `((:for ,iter ,list (cdr ,iter))
    (:smlet (,var (car ,iter)))
    ,@(case end-cond
	(:continue nil)
	(:stop `((:post (when (null ,iter) (return))))))))

(def-umac :for-vect (var array &optional (i (gensym)))
  (let ((arr (gensym)))
    `((:let (,arr ,array))
      (:repeat ,(length arr) ,i)
      (:smlet (,var (aref ,arr ,i))))))

