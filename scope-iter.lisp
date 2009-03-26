(require :iterate)

(defpackage #:scope
  (:use #:common-lisp #:iterate)
  (:documentation "Scope via the #:iterate package. Unfortunately "))

(in-package #:scope)

(set-macro-character #\{ #'curly-hook-reader)

(defmacro setf- (op var &rest rest)
  `(setf ,var (,op ,var ,@rest)))

(defun symbol-name-= (sym str)
  (when (symbolp sym)
    (string= (symbol-name sym) str)))

(defmacro iter-once (&body body)
  `(iter ,@body (finish)))

(defmacro scope (&body body)
  "Special progn, scans for definitions first."
  (let ((iter (when (symbol-name-= (car body) "ITER")
		(setf- cdr body))))
    `(,(if iter 'iter-once 'iter)
      ,@body)))
