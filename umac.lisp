(defpackage #:umac
  (:use #:common-lisp)
  (:export umac def-umac
	   values-default values-d
	   collecting appending summing until while force-return))

(in-package #:umac)

(defvar *umac-hash* (make-hash-table))

(defun first-match (list eql-to &optional (match-fun #'eql))
  (dolist (el list)
    (when (funcall match-fun el eql-to)
      (return el))))

(defun append-nonmatching (list appended &optional (match-fun #'eql))
  "Append elements of appended if match-fun returns false."
  (let (left)
    (dolist (a appended)
      (unless (first-match list a match-fun)
	(setf left `(,@left ,a))))
    (append list left)))

(defun delist (x) (if (listp x) (car x) x))

(defmacro setf- (change set &rest args)
  `(setf ,set (,change ,set ,@args)))

(defmacro umac ((&rest rest) &body body)
  "Umac allows you to make variables and functions/macros manipulating them
in one sentence."
  (let (got-let got-smlet got-flet got-mlet got-post force-return)
    (do ((iter rest iter)) ((null iter) nil)
      (let ((el (car iter)))
	(flet ((got-more (list) ;Add stuff and iterate forward.
		 (setf- cdr iter)
		 (append-nonmatching list (cdr el)
		   (lambda (a b) (eql (delist a) (delist b))))))
	  (case (car el)
	  ;Not implemented as extensions because extensions should wield the 
	  ;power through these anyway, this way the extensions take less args.
	    (:let   (setf- got-more got-let))
	    (:smlet (setf- got-more got-smlet))
	    (:flet  (setf- got-more got-flet))
	    (:mlet  (setf- got-more got-mlet))
	    (:post  (setf- got-more got-post))
	    (:force-return ;Coerces what is returned.
	     (setf force-return (cadr el))
	     (setf- cdr iter))
	    (t ;A umac from extension. Uses iterator as a 'stack' here.
	     (let ((got-fun (gethash (car el) *umac-hash*)))
	       (if got-fun
		 (setf iter (append (funcall got-fun el) (cdr iter)))
		 (error "You tried to use a non-existent extension."))))))))
    `(let (,@got-let) ;Put it all together. Here be roles of above variables.
     (symbol-macrolet (,@got-smlet)
     (flet ((values-default ()
	      ,(flet ((get-var (name)
			(when (first-match got-let name
				(lambda (el eql-to) (eql (delist el) eql-to)))
			  name)))
		 `(values ,(get-var 'ret)   ,(get-var 'val-0) ,(get-var 'val-1)
			  ,(get-var 'val-2) ,(get-var 'val-3) ,(get-var 'val-4)
			  ,(get-var 'val-5) ,(get-var 'val-6) ,(get-var 'val-7))))
            ,@got-flet)
     (macrolet ((values-d () '(values-default))
		,@got-mlet)
       (do () (nil nil)
	 ,@body
	 ,@got-post)
       ,(if force-return force-return '(values-default))))))))

(defmacro def-umac (name (&rest arguments) &body body)
  "Defines a umac for you."
  (let ((args (gensym)) (self (gensym)) (gname (gensym))
	(docstr (when (stringp (car body)) (list (car body)))))
  `(let ((,gname ,name))
     (setf (gethash ,gname *umac-hash*)
	   (lambda (,args)
	     ,@docstr
	     (destructuring-bind (,self ,@arguments) ,args
	       (unless (eql ,self ,gname)
		 (error "(Internal error)First argument not repeat of the 
extension name."))
	       ,@(if docstr (cdr body) body)))))))
