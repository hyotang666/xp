(defpackage :pxp.pool (:use :cl)
  (:export #:defpool))
(in-package :pxp.pool)

(declaim (optimize speed))

(defmacro defpool (name &key constructor destructor resetter)
  ;; Trivial syntax check.
  (check-type name symbol)
  (assert constructor () "CONSTRUCTOR is required.")
  ;; The body
  (let ((?pool (gensym (format nil "*~A*" name))))
  `(progn
     (defvar ,?pool ()
       ,(format nil "Pooling the ~A objects." name))
     (defmacro ,(intern (format nil "WITH-POOLED-~A" name))
       ((var &rest args) &body body)
       `(let ((,var (pop ,',?pool)))
	  (setq ,var (if (null ,var)
		       (,',constructor ,@args)
		       ,,@(if resetter
			    `(`(,',resetter ,var ,@args))
			    '(`,var))))
	  (unwind-protect (progn ,@body)
	    ,,@(when destructor
		 `(`(,',destructor ,var)))
	    (and ,var (pushnew ,var ,',?pool)))))
     ',name)))

(defun pprint-defpool (output exp)
  (funcall (formatter #.(concatenate 'string
				     "~:<" ; pprint-logical-block.
				     "~W~^ ~:_" ; operator.
				     "~W~^~1I ~_" ; name.
				     "~@{~W~^~3I ~@_~W~^~1I ~:@_~}" ; each k-v clause.
				     "~:>"))
	   output exp))

(set-pprint-dispatch '(cons (member defpool)) 'pprint-defpool)
