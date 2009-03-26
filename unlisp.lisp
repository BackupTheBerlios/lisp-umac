
(defpackage #:unlisp
  (:use #:common-lisp #:scope)
  (:export fancy scope-fancy binary)
  (:documentation "Assortment of fancy notation."))

(in-package #:unlisp)

;;Switches to turn fancy stuff on/off.
(defvar *fancy* '(= nil := nil :curly-hooks nil :square-hooks t)
  "Plist showing what fancy stuff is enabled.")

;;Scope fancy stuff.
(defmacro scope-fancy (&body body)
  "Adds fancy operators to define variables/functions to scope."
  (let ((top-level (when (symbol-name-= (car body) "TOP")
		     (setf body (cdr body))
		     t)))
    (when (getf *fancy* :separators)
      (setf body (do ((b body (cdr b))
		      (out nil (if (eql (car b) 'sep)
				   `(,@out ())
				   `(,@(cdr out) (,@(car out) ,(car b))))))
		     ((null b) out))))	
    `(scope ,@(when top-level '(top))
       ,@(reverse
	  (do ((b body (cdr b))
	       (out nil out))
	      ((null b) out)
	    (flet ((collect (el) (push el out)))
	      (cond
		((null (cddr b))
		 (collect (car b)))
		((and (eql (cadr b) :=) (getf *fancy* :=))
		 (if (listp (car b))
		     (collect `(fun ,(caar b) (,@(cdar b))
				    ,(caddr b)))
		     (collect `(var ,(car b) ,(caddr b))))
		 (setf b (cddr b)))
		((and (symbol-name-= (cadr b) "=") (getf *fancy* '=))
		 (collect `(setf ,(car b) ,(caddr b)))
		 (setf b (cddr b)))
		(t
		 (collect (car b))))))))))

(defun symbol-name-= (a b)
  (flet ((desym (x)
	   (cond
	     ((symbolp x) (symbol-name x))
	     ((stringp x) x)
	     (t nil))))
    (let ((a (desym a)) (b (desym b)))
      (when (or a b)
	(string= (desym a) (desym b))))))

(defun in-string (char str)
  (loop for ch across str when (char= ch char) return t))

(defun space-around (these-str str)
  "Puts space round characters in these-str, so they are separate \
characters."
  (if (null these-str) str
      (coerce  ;TODO a little blunt?
       (loop for ch across str
	  when (in-string ch these-str) collect #\Space
	  collect ch
	  when (in-string ch these-str) collect #\Space)
       'string)))

(defun curly-hook-reader (to-macro &optional (hook #\{) (unhook #\})
			                     space-around)
  "Outputs a function that 'binds' the curly hooks to a macro.\
 (can also do other hooks with optional arguments.)"
  (lambda (stream char)
    "Binds curly hooks to some macro."
    (declare (ignore char))
    (do ((ch #\Space (read-char stream nil unhook))
	 (n 1 (cond ((char= ch unhook) (- n 1))
		    ((char= ch hook)   (+ n 1))
		    (t                 n)))
	 (str "" (if (and (= n 1) (char= ch unhook)) str
		     (concatenate 'string str (list ch)))))
	((= n 0)
	 (progn (unread-char ch stream) ;TODO where did i lose it?
	 `(,to-macro ,@(read-from-string
			(format nil "(~D)"
				(space-around space-around str)))))))))

(set-macro-character #\{ (curly-hook-reader 'scope-fancy))

;;Binary functions.

(defvar *binary-funs* '(* % / + -)
  "Symbols of binary functions and their order.(Latter is critical!)
Note that '=' is not defaultly provided. (And the = of scope-fancy will 
never meet with the binary funs.)")

(defun binary-fancy-fun (code &optional (bin-funs *binary-funs*))
  "See the binary macro."
  (if (or (null bin-funs) (null code))
    (if (null (cdr code)) (car code)
      (error "Malformed binary notation! All values must be separated by\
 binary functions!"))
    (let ((bin-name (if (listp (car bin-funs))
			(caar bin-funs)
			(car bin-funs)))
	  (bin-fun  (if (listp (car bin-funs))
			(cadar bin-funs)
			(car bin-funs))))
      (multiple-value-bind (pre-bf post-bf)
	  (do ((i 0 (+ i 1))
	       (c code (cdr c)))
	      ((or (null c) (symbol-name-= (car c) bin-name))
	       (values (subseq code 0 i) (cdr c))))
	(if (null post-bf) ;Not this one.
	    (binary-fancy-fun code (cdr bin-funs))
	    `(,bin-fun ,(binary-fancy-fun pre-bf)
		       ,(binary-fancy-fun post-bf)))))))

(defmacro binary (&rest code)
  "Fancy binary notation. (NOTE: much more inefficient then it could be,\
 on plus side, all compile-time.)"
  (binary-fancy-fun code))

(defvar *space-around* "*%/+-"
  "characters seen as single tokens by adding spaces around them.")
(set-macro-character #\[
  (curly-hook-reader 'binary #\[ #\] *space-around*))

(defun reader-symbol-response (name)
  (lambda (stream char)
    (declare (ignore stream char))
    name))

;;Enabling and disabling fancy stuff.

(defun fancy (what &optional (how t))
  "Disable/enable unlisps fancy stuff, t enables default, others have 
other behaviors."
  (setf (getf *fancy* what) how)
  (case what
    (:curly-hooks
     (set-macro-character #\{
       (cond ((eql how t) (curly-hook-reader 'scope-fancy))
	     (t           (curly-hook-reader how)))))
    (:square-hooks
     (set-macro-character #\[
       (when how (curly-hook-reader 'binary #\[ #\] *space-around*))))))

