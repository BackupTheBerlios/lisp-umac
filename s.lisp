(load "scope.lisp")

(load "unlisp.lisp")
(load "umac.lisp")

(require :asdf)

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
