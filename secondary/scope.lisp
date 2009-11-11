;;Author: Jasper den Ouden
;;This file is in public domain.

(defpackage #:scope
  (:use #:common-lisp)
  (:export scope))

(in-package #:scope)

(defun symbol-name-= (a b)
  (flet ((desym (x)
	   (cond
	     ((symbolp x) (symbol-name x))
	     ((stringp x) x)
	     (t nil))))
    (let ((a (desym a)) (b (desym b)))
      (when (or a b)
	(string= (desym a) (desym b))))))

(defun scope-fun (body &optional top-level)
  "Turned out to be easier to do it via functions."
  (reverse
   (do ((b body (cdr b)) (out nil out))
       ((null b) out)
     (flet ((through-let (let-name elements)
	      (cond
		(top-level
		 (dolist (el elements)
		   (case let-name
		     (let      (push `(defvar ,@el) out))
		     (flet     (push `(defun ,@el) out))
		     (macrolet (push `(defmacro ,@el) out)))))
		(t
		 (push `(,let-name (,@elements)
		           ,@(scope-fun (cdr b)))
		       out)
		 (return out))))
	    (collect (el)
	      (push el out)))
       (if (not (listp (car b)))
	 (collect (car b))
	 (destructuring-bind (name &rest set) (car b)
	   (macrolet ((sym-name-through-let (t-body &rest stuff)
			`(cond
			   ,@(loop for el in stuff collect
			       (destructuring-bind
				     (sym-name let-name set-to) el
				 `((symbol-name-= name ,sym-name)
				   (through-let ',let-name ,set-to))))
			   (t ,t-body))))
	     (sym-name-through-let (collect (car b))
	      ("VARS" let set)        ("VAR" let (list set))
	      ("FUNS" flet set)       ("FUN" flet (list set))
	      ("MACROS" macrolet set) ("MACRO" macrolet (list set))
	      ("SYMBOLMACROS" symbol-macrolet set)
	      ("SYMBOLMACRO" symbol-macrolet (list set))))))))))

(defmacro scope (&body body)
  "Allows variables, functions, and (symbol)macros to be created and within\
 scope."
  (let ((top-level (when (symbol-name-= (car body) "TOP")
		     (setf body (cdr body))
		     t)))
    `(progn ,@(scope-fun body top-level))))
