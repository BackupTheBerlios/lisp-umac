;;Author: Jasper den Ouden
;;This file is in public domain.

(in-package #:umac)

(defclass ustream ()
  ((str :initform "" :type string :initarg str)
   (getstr :initform (lambda()"") :type function :initarg :getstr)))

(defun make-stream (&key (str "") (from (lambda () "")))
  (make-instance 'ustream :str str
    :getstr (cond ((functionp from) from)
		  ((streamp from) (lambda () (read-line stream))))))

(defun subseq* (str start &optional end)
  "Subseq without bounds errors."
  (if (> (length str) start)
      (subseq str start (if end (min end (length str)) (length str)))
      ""))

(defun first-read-eql (ustream cases)
  "Returns if the first part of the stream is"
  (let ((test (string)
	  (with-slots (str) ustream
	    (string= (subseq* str 0 (length string)) str))))
    (if (listp cases)
	(umac (loop (for-on-list case-str cases)
		 (when (test case-str)
		   (set-return case-str)
		   (finish))))
	(when (test cases) cases))))

(defun is-whitespace (ch)
  (case ch ((#\Newline #\Space #\Tab) t)))


(defun xor (x y) (and (or x y) (not (and x y))))

(defun count-to-stop (str stopchar &optional negate)
  (loop for el across str ;Iterate until ending of symbol.
        for i from 0
     when (xor (funcall stopchar el) negate)
     return i))

(defun get-token (str &optional (stopchar #'is-whitespace) negate)
  "Gets a token from a string. Stopchar are characters it stops for, must \
are characters that must be in there, or it will also stop.
Returns: the string token, the ending index, the element it stopped at."
  (when-with i (count-to-stop str stopchar negate)
    (values (subseq str 0 i) i (when (< i (length str)) (aref str i)))
    (values str (length str) nil)))

(defun stopchar-to-fun (stopchar)
  (cond
    ((functionp stopchar)
     stopchar)
    ((listp stopchar)
     (lambda (ch)
       (umac (loop (for-on-list el ,rship)
		(when (eql ch (car el)) (part-return t))))))
    ((stringp stopchar)
     (lambda (ch)
       (umac
	(loop (for-range (i 0 (length stopchar)))
	   (when (eql ch (aref stopchar i))
	     (part-return t))))))))

(defun skip-string (ustream string)
  "Skips a string."
  (with-slots (str getstr) ustream
    (setf- subseq* str (length ,string))
    (when (= (length str) 0)
      (setf str (funcall getstr)))))

(def-umac skip () (ustream skip)
  `(let ((rskip ,skip))
     (cond
       ((stringp rskip)
	(skip-string ,ustream rskip))
       (t
	(let ((skipfun (stopchar-to-fun rskip) skipfun))
	  (with-slots (str getstr) ,ustream
	    (do () ((if (= (length str) 0) (setf str (funcall getstr))
			t) t)
	      (setf- subseq* str (count-to-skip str skipfun)))))))))
    
(def-umac skip-string () (ustream string)
  "Skips a string."
  `(skip-string ,ustream ,string))

(def-umac when-read ((ustream cases &key (str (gensym))) &rest series)
  (umac- `(when-with ,str (first-read-eql ,ustream ,cases)
	    (skip-string ,str)
	    ,@series)))

(def-umac case-read *umac-macs* () (ustream &rest case-clauses)
  "Multiple of when-read in sequence."
  `(cond
     ,@(loop for c in case-clauses collect
	 (argumentize-list (cases &rest body) c
	   `((first-eql ,str ,getstr ,cases)
	      (read ,str ,getstr (,@body)))))))
