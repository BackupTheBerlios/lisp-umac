(in-package #:umac)

(def-umac loop () (&rest series)
  "Loops a series until (end ',loop-name) is called."
  (series-stuff nil nil series))

(def-umac repeat () (count &rest series)
  "Repeat something some number of times. If count a list, it is the 
counting index, upto-value and then the name you can umac-end-to with."
  (argumentize-list (i upto repeat-name)
      (if (listp count) count (list (gensym) count))
    (umac- `(loop
	       (while (< ,i ,upto))
	       ,@series
	       (sum ,i 1)))))

(def-umac for (:pass-first 55) (name start change)
  (when (eql change name)
    (error "For is only for changing variables. (Use var)"))
  (add-var `(,name ,start)))

(def-umac for-on-list (:pass-first t) (el list key)
  (add-var `(,el ,list))
  (cond
    ((not key)
     `(progn
	(setf ,el (cdr ,el))
	(when (null ,el)
	  (return nil))))
    ((eql key :no-stop)
     `(setf ,el (cdr ,el)))
    (t
     (error "You gave a bad key for umac for-on-list."))))

(def-umac for-range (:pass-first t) (i from to incr)
  (add-var `(i ,from))
  `(progn
     (setf ,i (+ ,i ,(if-use incr 1)))
     (when (>= ,i ,to) (return))))
