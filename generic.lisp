;;Author: Jasper den Ouden
;;This file is in public domain.

(cl:in-package :cl-user)

(defpackage #:generic
  (:nicknames #:gen)
  (:use #:common-lisp)
  (:export sqr delist

	   curry curry-l
	   
           with-gensyms for-more setf-
	   if-let if-use when-let case-let
	   ift when-do
	   string-case
	   clamp

	   with-mod-slots
	   with-access with-mod-access

	   setf-defun
	   
	   var-changer def-changable-var)
  (:documentation "Assortment of little useful macros/functions."))

(in-package #:generic)

(defun sqr(x)
  "Square of a value."
  (* x x))

(defun delist (x)
  "If list, return car, otherwise itself."
  (if (listp x) (car x) x))

(defmacro with-gensyms ((&rest vars)&body body)
"Makes you some variables with gensyms output in them."
  `(let (,@(mapcar (lambda (v) `(,v (gensym))) vars))
     ,@body))

(defmacro for-more (macroname &rest args)
  "Applies a series of different arguments to same function."
  (cons 'progn
     (loop for el in args
	collect (cons macroname el))))

(defmacro setf- (operator set &rest args)
  "Changes 'set argument with setf using given operator, and extra\
 arguments. WARNING/TODO: abstraction leak if set has sideeffects."
  `(setf ,set (,operator ,set ,@args)))

(defmacro if-let (var cond if-t &optional (if-f nil))
  "Makes a variable var set by cond, and them does if-t if non-nil and
 (optionally)if-f else."
  `(let ((,var ,cond))
    (if ,var ,if-t ,if-f)))

(defmacro if-use (&rest conds)
  "Returns the first one that returns non-nil."
  (if (null(cdr conds))
    (car conds)
    (with-gensyms (var)
      `(if-let ,var ,(car conds) ,var
	 (if-use ,@(cdr conds))))))

(defmacro when-let (var cond &body body)
  "When, but with the condition, var available."
  `(if-let ,var ,cond (progn ,@body) nil))

(defmacro case-let (var is &rest cases)
  "Case, but makes a variable for you."
  `(let ((,var ,is))
     (case ,var ,@cases)))

(defmacro ift (manner self &rest with)
  "Returns itself if `(,manner ,self ,@with) true."
  (with-gensyms (s)
    `(let ((,s ,self))
      (when (,manner ,s ,@with)
	,s))))

(defmacro when-do (cond &rest do)
  "return-from the condition, if the condition is true."
  (with-gensyms (s)
    `(when-let ,s ,cond
       (,@do ,s))))

(defmacro string-case (string &rest cases)
  "A case for strings."
  (cons 'cond
    (loop for el in cases
      collect (if (eql (car el) t)
		`(t ,(cadr el))
		`((string= ,string ,(car el)) ,(cadr el))))))

(defmacro let-from-list ((&rest vars) list)
  "Sets given variables vars according to list, as far as possible."
  (with-gensyms (tmp)
    `(let ((,tmp ,list))
     (let 
       ,(loop for var in vars 
	      for i from 0
	  collect `(,var (nth ,i ,tmp)))))))

(defun clamp (clamped from to)
  "Clamp between two values."
  (cond ((< clamped from) from)
	((> clamped to)   to)
	(t                clamped)))

(defmacro with-mod-slots (mod (&rest slots) object &body body)
  "WITH-SLOTS, but requires something to be prepended to the\
 SYMBOL-MACROLET's, this allows you to use two or more objects with\
 convenient symbols at the same time."
  (with-gensyms (obj)
    `(let ((,obj ,object))
       (symbol-macrolet
	   (,@(mapcar (lambda (slot)
			`(,(intern (format nil "~D~D" mod slot))
			   (slot-value ,obj ',slot)))
		      slots))
	 ,@body))))

(defmacro with-mod-access (mod (&rest accessors) object &body body)
  "Access objects. Lists on accessors/readers or plain functions are seen
 as (function &rest args-after) 
Mod adds some name previously so you can work with multiple of the same.\
 (Similar to with-mod-slots.)"
  (with-gensyms (obj)
    `(let ((,obj ,object))
       (symbol-macrolet
	   (,@(mapcar 
	       (lambda (a)
		 `(,(if mod (intern (format nil "~D~D" mod (delist a)))
			    (delist a))
		    (,(delist a) ,obj ,@(when (listp a) (cdr a)))))
	       accessors))
	 ,@body))))

(defmacro with-access ((&rest accessors) object &body body)
  "Access objects. Lists on accessors/readers/ plain functions  are seen as
 (function &rest args-after)."
  `(with-mod-access nil (,@accessors) ,object ,@body))

(defun curry (fun &rest curried)
  "Curry to the right; add arguments to the end of the function.
Note: uses apply.. Hope it will optimize."
  (lambda (&rest args)
    (apply fun (append args curried))))

(defun curry-l (fun &rest curried)
  "Curry to the left; add arguments to the start of the function.
Note: uses apply.. Hope it will optimize."
  (lambda (&rest args)
    (apply fun (append curried args))))

(defmacro setf-defun (name (&rest args) &body body)
  "Make a defun and a setter at the same time.
TODO see though things."
  (with-gensyms (to)
    `(progn (defun ,name (,@args) ,@body)
	    (defun (setf ,name) (,to ,@args)
	      ,(car body)
	      (setf ,@(last body) ,to)))))

(defmacro var-changer (var-name
	           &key (doc "Changes a variable, see the variable doc."))
  "Makes a variable changer for a given variable."
  `(defmacro ,var-name (,var-name &body body)
     ,doc
     (append (list 'let (list (list ,var-name ,var-name)))
	     body)))

(defmacro def-changable-var (var-name &key init doc (changer-doc doc)
			 (name var-name))
  "Makes variable, then makes it changable"
  `(progn (defvar ,var-name ,init ,doc)
	  (var-changer ,var-name :doc ,changer-doc)))
