;;Author: Jasper den Ouden
;;This file is in public domain.

(in-package #:vect)

(defun components-from-integer (dimension)
  "Uses standard component names."
  (case dimension (1 '(x)) (2 '(x y)) (3 '(x y z))))

(defmacro do-components (fun maker components &rest args)
  "Macro, does function with the arguments from all the arguments. 
Components is the number of elements, or their names."
  (when (integerp components)
    (setf components (components-from-integer)))
  `(,maker
    ,@(iter (for c in components)
	    (collect
		`(,fun
		  ,@(iter (for a in args)
			 (collect `(,c ,a))))))))

(defmacro make-do-components-method (method fun maker components type
				     arg-cnt)
  "Makes a method from the components."
  (let ((gs (iter (repeat arg-cnt)
		  (collect (gensym)))))
    `(defmethod ,method (,@(iter (for g in gs)
				 (collect `(,g ,type))))
       (do-components ,fun ,maker ,components ,@gs))))

(defun any-eql (eql list)
  "Any of list eql to eql."
  (dolist (el list) (when (eql eql el) (return el))))

(defun all-combinations (list n fun &optional args)
  (dolist (el list)
    (if (= n 1)
      (funcall fun (cons el args))
      (all-combinations list (- n 1) fun (cons el args)))))

(defun r+n-name (n) (case n (2 'r+2) (3 'r+3) (4 'r+4) (5 'r+5) (6 'r+6)))
(defun r-n-name (n) (case n (2 'r-2) (3 'r-3) (4 'r-4) (5 'r-5) (6 'r-6)))

(defvar *vector-types* nil "Types that already exist.")

(defmacro define-vect (name of-type initform components &key maker
		       fun-maker v+ v-)
  "Defines vector type with components of given type.
Also makes all methods."
  (when (integerp components)
    (setf components (components-from-integer components)))
  
  (unless maker 
    (setf maker (vect-maker-name (length components))))
  
  (when (any-eql name *vector-types*)
    (warn "Vector type with this name already exists.")
    (return-from define-vect))
  
 ;Set v+ and v- based on info. This cuts back on stuff defmethod has to do. 
  (unless v+
    (setf v+ (if (or (subtypep of-type 'number) (subtypep of-type 'complex))
		 '+ 'v+)))
  (unless v-
    (setf v- (if (or (subtypep of-type 'number) (subtypep of-type 'complex))
		 '- 'v-)))
  
  (push name *vector-types*)
  
  `(progn
   ;The class.
     (defclass ,name ()
       (,@(iter (for c in components)
		(collect `(,c :initarg ,c :accessor ,c
			      :type ,of-type :initform ,initform))))
       (:documentation ,(format nil "A ~D-dimensional vector of type ~D."
				(length components) of-type)))
   ;Creation.
     (defmethod ,maker (,@(iter (for c in components)
				(collect `(,c ,of-type))))
       (make-instance ',name ,@(iter (for c in components)
				     (appending `(',c ,c)))))
      ,@(when fun-maker
	 `((defun ,fun-maker (,@components)
	     (make-instance ',name ,@(iter (for c in components)
					   (appending `(',c ,c)))))))
   ;Multiplication. (Dividing based on it.)
     (defmethod r* ((vect ,name) (scalar number))
       (mk-vect ,@(iter (for c in components)
			(collect `(* (,c vect) scalar)))))
   ;Adding/substracting, different counts of arguments.
     ,@(iter (for n from 2) (while (<= n 2))
	     (all-combinations *vector-types* n
		 (lambda (tps)
		   (when (any-eql name tps)
		     (let*((vars (iter (repeat n) (collect (gensym))))
			   (args (iter (for v in vars)
				       (for tp in tps)
				       (collect `(,v ,tp)))))
		       (appending
			`((defmethod ,(r+n-name n) (,@args)
			    (do-components ,v+ mk-vect ,components
					   ,@vars))
			  (defmethod ,(r-n-name n) (,@args)
			    (do-components ,v- mk-vect ,components
					   ,@vars)))))))))
   ;Other stuff.
     ,@(iter
	(all-combinations *vector-types* 2
	  (lambda (tps)
	    (when (any-eql name tps)
	      (destructuring-bind (tp-a tp-b) tps
		(appending
		 `((defmethod r-inpr ((a ,tp-a) (b ,tp-b)) ;Inproduct
		     (,v+ ,@(iter (for c in components)
				 (collect `(r-inpr (,c a) (,c b))))))
		   ,(case (length components)
		      (2 ;Cross product of 2 dimensional is 1 dimensional.
		       (destructuring-bind (x y) components
			 `(defmethod crosspr  ((a ,tp-a) (b ,tp-b))
			    (- (* (,y a) (,x b)) (* (,x a) (,y b))))))
		      (3
		       (destructuring-bind (x y z) components
			 `(defmethod crosspr  ((a ,tp-a) (b ,tp-b))
			    (mk-vect 
			     (- (* (,z a) (,y b)) (* (,y a) (,z b)))
			     (- (* (,x a) (,z b)) (* (,z a) (,x b)))
			     (- (* (,y a) (,x b)) (* (,x a) (,y b))))))))))))))
	(finish))
     ;Listing
     (defmethod vect-as-list ((vec ,name))
       ,(if (any-eql of-type (cdr *vector-types*))
	    `(iter (for el in (do-components identity list ,components vec))
		   (collect (vect-as-list el)))
	    `(do-components identity list ,components vec)))
     
     ,(case (length components)
        (2
	 (destructuring-bind (x y) components
	   `(defmethod v-angle ((vec ,name))
	      (with-slots (,x ,y) vec
		(cond ((> (abs ,x) (abs ,y))
		       (atan ,y ,x))
		      ((and (= (abs ,x) 0) (= (abs ,y) 0))
		       0d0)
		      (t
		       (- (/ pi 2) (atan ,x ,y)))))))))
	;TODO more
	
     ;Entering in GL if available.
     (when (find-package :gl)
       (defmethod gl-vertex ((vec ,name))
	 (with-slots (,@components) vec
	   (gl:vertex ,@components))))
     ))

