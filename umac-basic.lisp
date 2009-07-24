;;Author: Jasper den Ouden
;;This file is in public domain.

(in-package #:umac)

;;NOTE some stuff is build in:
; :let, :flet, :mlet(macrolet), :smlet (symbol-macrolet),
; :initially, :finally, :force-return.

;;Accumulation-like
(def-umac :list (&optional (into-list 'ret) initial)
  "Listing stuff; collecting, appending. After this, you may only\
 manipulate the list with these functions!w"
  (let ((last (gensym)))
  `((:let (,into-list ,initial) ,last)
    (:flet (collecting (&rest collected)
	     (if (null ,into-list)
	       (setf ,into-list collected  ,last (last ,into-list))
	       (setf (cdr ,last) collected ,last (last ,last))))
	   (appending (&rest appended)
	     (dolist (list appended)
	       (dolist (el list)
		 (collecting el))))
	   (fix-list ()
	     (setf ,last (last ,last)))))))

(def-umac :sum (&optional (sum-onto 'ret) (initial 0))
  "Summing onto a variable; summing"
  `((:let  (,sum-onto ,initial))
    (:mlet (summing (&rest added)
	     `(setf- + ,',sum-onto ,@added)))))

(def-umac :sum-into (&optional (sum-onto 'val-1) (initial 0))
  "Summing onto a variable; summing"
  `((:let  (,sum-onto ,initial))
    (:mlet (summing-into (into &rest added)
	     `(setf- + ,into ,@added)))))

(def-umac :op (&optional (by-name 'op) op (onto 'ret) initial)
  "Changing stuff with any operation."
  `((:let (,onto ,initial))
    (:mlet (,by-name (op-name &rest args)
	     `(setf- ,,op ,,onto ,@args)))))

;;End condition
(def-umac :return ()
  "Returning; until, while. WARNING uses (return), will behave such!"
  `((:mlet (return-val (returned)
	     `(progn (setf ret ,returned)
		     (return)))
	   (set-returned (returned)
	     `(setf ret ,returned))
           (until (&rest and)
	     `(when (and ,@and) (return)))
	   (while (&rest and)
	     `(unless (and ,@and) (return))))))

(def-umac :once ()
  "Return after single run of umac.(Overrides any others that say loop.)
Useful for function callbacks."
  `((:post (return))))

;;Iteration.
(def-umac :for (iter start change)
  "DO-like iteration."
  `((:let (,iter ,start)) (:post (setf ,iter ,change))))

(def-umac :for-interval (i from to)
  "Does something for intager inside an interval assumes from<=to."
  `((:for ,i ,from (+ ,i 1)) (:post (unless (< ,i ,to) (return)))))

(def-umac :repeat (count &optional (i (gensym)))
  "Repeats some statement."
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
  "Iterator over a vector."
  (let ((arr (gensym)))
    `((:let (,arr ,array))
      (:repeat (length ,arr) ,i)
      (:smlet (,var (aref ,arr ,i))))))
