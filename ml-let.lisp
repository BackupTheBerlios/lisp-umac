;; Author Jasper den Ouden
;; This file is placed in the public domain.

(defmacro if-let (var cond if-t &optional (if-f nil))
  "Makes a variable var set by cond, and them does if-t if non-nil and
 (optionally)if-f else."
  `(let ((,var ,cond))
    (if ,var ,if-t ,if-f)))

(defun ml-let-fn (args body)
  "Function for ml-let macro."
  (when (null args)
    (return-from ml-let-fn body))
  (if-let pos (position '= args)
    (case pos
      (0 (error "ml-let cannot make variables named =\
 (Did you forget the variable name?"))
      (1 (destructuring-bind (var = value &rest rest) args
	   (unless (eql = '=) (error "Function is erronous, = not found\
 where POSITION indicated it to be."))
	   `((let ((,(car args) ,(caddr args)))
	       ,@(ml-let-fn rest body)))))
      (t
         `((flet ((,(car args) (,@(subseq args 1 pos))
		    ,(nth (+ pos 1) args)))
	     ,@(ml-let-fn (nthcdr (+ pos 2) args) body)))))
    (error "ml-let needs an = in there somewhere.")))

(defmacro ml-let ((&rest args) &body body)
  "ML-like variable and function creation."
  (car(ml-let-fn args body)))
