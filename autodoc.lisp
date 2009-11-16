
;Make sure autodoc is available.

(asdf:operate 'asdf:load-op :autodoc)

;Document.

;Generic
(autodoc:document-system :generic :directory "doc/autodoc/"
 :scan-recurse-cnt 10 :write-recurse-cnt 10)

;Since it is entirely equivalent to making a macro, it is scanned as such.
; except you only use it as macro with use-denest-macro.
(expr-scan:add-scanner-fun 'denest:def-denest-macro 
			   (lambda (expr)
			     (print expr)
			     (expr-scan:fun-scanner expr)))

;Denest, as it is separate.
(autodoc:document-system :denest :directory "doc/autodoc/"
 :scan-recurse-cnt 10 :write-recurse-cnt 10
 :order '(("Variables" (defvar defparameter))
	  ("Functions" (defun defmacro))
;TODO damned thing won't do it autodoc scans for them wrong..
	  ("Denest macros" (denest:def-denest-macro)))) 
