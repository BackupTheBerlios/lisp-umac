(load "scope.lisp")

(load "unlisp.lisp")
(load "umac.lisp")

(load

(require :cl-graph)

(asdf-install:install :cl-containers)

(tinaa:document-system :package :scope "doc2")

(asdf-install:install "/home/jasper/lisp-util/albert-0.4.10.tar.gz")

(require :albert)



(require :asdf-install)

(documentation-template:create-template
  '#:umac :target "doc/doc-src/autodoc/umac.html"
	  :subtitle "Inferior to iterate")
(documentation-template:create-template
  '#:scope :target "doc/doc-src/autodoc/scope.html"
	   :subtitle "Scoped variables, functions and (symbol)macros.")
(documentation-template:create-template
  '#:unlisp :target "doc/doc-src/autodoc/unlisp.html"
  :subtitle "Not so lispy constructs.")

(in-package #:unlisp)

(disable-fancy :sep-mac-char)

(set-macro-character #\: nil)

(progn
  (defvar *keyword* :keyword "Without this, keywords are gone forever.\
 (Unless you restart.)")
  (set-macro-character
   #\: (lambda (stream char)
	 "I could make separators and stoppers if the damn characters \
weren't taken. This one not a good one for it either, even with hack."
	 (let ((rch (read-char stream))) ;Peek if it is a keyword.
	   (case rch
	     ((#\Space #\Tab #\Newline #\) #\;) ;It's not, make sep.
	      (unread-char rch stream)
	      'sep)
	     (t
	      (do*((ch rch (read-char stream)) ;It's a keyword, collect it.
		   (str "" str))
		  ((case ch
		     ((#\Space #\Newline #\Tab #\; 
		       #\' #\` #\, #\))
		      t)
		     (t
		      (setf str (concatenate 'string str (list ch)))
		      nil))
		   (intern (string-upcase str) *keyword*)))))))))

(progn
  (defvar *old-semicolum* (get-macro-character #\;)
    "Holds the semicolon readmacro, when read at this point.")
  ;Even worse then #\:..
  (set-macro-character
   #\; (if how (lambda (stream char)
		 (case (peek-char nil stream)
		   (#\Newline 'sep)
		   (t  	      (funcall *old-semicolum* stream char))))
	   *old-semicolum*)))

(require :iterate)

(print 'a)

(load "program-to-class.lisp")

(in-package #:program-to-class)

(defmacro blabla (&rest stuff)
  nil)

(print(macroexpand-1 '
(program-to-class
    ();(#:first (:use #:common-lisp) (:export meh beep))
    (first-class first-main)
  (defvar *kaka* 'kaka)
  (print *kaka*)
  (defun meh ()
    (format nil "~D~D" *kaka* 3))
  (defun beep (bleh)
    (format nil "~D-~D-~D" bleh 'beep (meh))))))

(first-main)

(sb-ext:enable-package-locks)

(iterate (repeat 4) (collect 2))



(defmacro machook (x y) `(/ (+ ,x ,y) 2))

(let (cookie)
  (flet ((hook (expander form env)
	   (push form cookie)
	   (funcall expander form env)))
    (let ((*macroexpand-hook* #'hook))
      (full-macroexpand '(progn (machook 1 2)))))
  cookie)


(defstruct blab
  (*mah* 65))


(mah)

(with-slots (*mah*) (make-blab)
  (mah))

(defparameter *meh* -1)

(defun mah ()
  (print (setf- +  *mah* 1)))

(let ((*mah* -5))
  (mah))

(intern "LALA" :keyword)

(load "class-var.lisp")

(in-package #:class-var)

(defparameter *meh* -1)
(defparameter *mah* 1)

(class-using-var moeh (*mah* *meh*))

(defvar *inst* (make-instance 'moeh))
(defvar *inst2* (make-instance 'moeh))

(defun meh () (print (setf *meh* (+ *meh* 1))))
(defun mah () (print (setf *mah* (+ *mah* 1))))

(class-being-vars (*inst* *mah* *meh*)
  (meh) (mah))

(print *mah*)

(require :asdf-install)
(require :asdf)

(asdf-install:install "/home/jasper/lisp-util/misc-extensions_1.2.0.tar.gz")

(require :misc-extensions)
(in-package #:lexical-contexts)

(defcontext vector-context (x y)
  (deflex z 0 "z")
  (defun lensqr () (+ (sqr x) (sqr y) (sqr z))))

(with-context (vector-context 4 6)
  (lensqr))



(let ((n 0))
  (defun n+1 () (setf n (+ n 1))))

(asdf-install:install :cl-graphviz)

(let ((n -5))
  (n+1))

(asdf-install:install )

(asdf-install:install "/home/jasper/lisp-util/metabang-bind_latest.tar.gz")

(asdf-install:install "/home/jasper/lisp-util/tinaa_latest.tar.gz")

(require :tinaa)

(setf bck asdf:*central-registry*)

(push #p"/home/jasper/lisp-util/cl-graphviz/" asdf:*central-registry*)

(require :cl-graph)
(in-package #:cl-graph)

(require :iterate)

(use-package :iterate)

(let ((g (make-container 'dot-graph :default-edge-type :undirected)))
  (iter (for (a b) in '((a b) (b c) (b d) (d e) (e f) (d f)))
	(add-edge-between-vertexes g a b))
  (layout-graph-with-graphviz g)
  (format t "~A" (graph->dot g nil)))
