(in-package :cl-user)
(defpackage :pxp.dispatch (:use :cl)
  (:shadow . #0=(pprint-dispatch copy-pprint-dispatch *print-pprint-dispatch*
				 set-pprint-dispatch pprint-dispatch))
  (:export . #0#)
  (:export
    #:*current-level*
    #:*IPD* ; initial pprint dispatch.
    #:get-printer ; reader
    #:pprint-dispatch-entries ; reader
    #:non-pretty-print ; printer
    #:show ; entry printer
    #:initialize
    #:structure-type-p
    ))
(in-package :pxp.dispatch)

(declaim (optimize speed))

(defun structure-type-p (x)
  (and (symbolp x)
       (typep (find-class x nil) 'structure-class)))

(defvar *print-pprint-dispatch* t ;see initialization at end of file.
  "controls pretty printing of output")

(defvar *IPD* nil ;see initialization at end of file.
  "initial print dispatch table.")

(declaim (type (mod #.most-positive-fixnum) *current-level*))
(defvar *current-level* 0
  "current depth in logical blocks.")

(declaim (ftype (function (t) (values (eql t) &optional))
		always-true))
(defun always-true (x) (declare (ignore x)) T)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *preds-for-specs*
  '((T always-true) (cons consp)
    (null null) (symbol symbolp) (atom atom) (cons consp)
    (list listp) (number numberp) (integer integerp)
    (rational rationalp) (float floatp) (complex complexp)
    (character characterp) (string stringp) (bit-vector bit-vector-p)
    (vector vectorp) (simple-vector simple-vector-p)
    (simple-string simple-string-p) (simple-bit-vector simple-bit-vector-p)
    (array arrayp) (package packagep) (function functionp)
    (compiled-function compiled-function-p))))

(defstruct (pprint-dispatch (:conc-name nil) (:copier nil))
  (conses-with-cars (make-hash-table :test #'eq) :type hash-table)
  (structures (make-hash-table :test #'eq) :type hash-table)
  (others nil :type list))

(defstruct (entry (:conc-name nil))
  (test nil)        ;predicate function or count of higher priority others.
  (fn nil)          ;pprint function
  (full-spec nil))  ;list of priority and type specifier

(declaim (ftype (function (&optional (or null pprint-dispatch)) (values pprint-dispatch &optional))
		copy-pprint-dispatch))
(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (let* ((table (or table *IPD*))
	 (new-conses-with-cars
           (make-hash-table :test #'eq
	     :size (max (hash-table-count (conses-with-cars table)) 32)))
	 (new-structures
	   (make-hash-table :test #'eq
	     :size (max (hash-table-count (structures table)) 32))))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-conses-with-cars) (copy-entry value)))
	     (conses-with-cars table))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-structures) (copy-entry value)))
	     (structures table))
    (make-pprint-dispatch
      :conses-with-cars new-conses-with-cars
      :structures new-structures
      :others (copy-list (others table)))))

(declaim (ftype (function ((or symbol cons))
			  (values (member :cons-with-car :structure-type :other) &optional))
		specifier-category))
(defun specifier-category (spec)
  (cond ((and (consp spec)
	      (eq (car spec) 'cons)
	      (consp (cdr spec))
	      (null (cddr spec))
	      (consp (cadr spec))
	      (eq (caadr spec) 'member)
	      (consp (cdadr spec))
	      (null (cddadr spec)))
	 :cons-with-car)
	((and (symbolp spec) (structure-type-p spec)) :structure-type)
	(T :other)))

(declaim (ftype (function ((or symbol cons)) (values (cons (eql lambda)) &optional))
		<specifier-fn>))
(defun <specifier-fn> (spec)
  (labels ((convert-body (spec)
             (cond ((atom spec)
		    (locally (declare (symbol spec))
                      (let ((pred (cadr (assoc spec *preds-for-specs*))))
                        (if pred `(,pred x) `(typep x ',spec)))))
                   ((member (car spec) '(and or not))
                    (cons (car spec) (mapcar #'convert-body (cdr spec))))
                   ((eq (car spec) 'member)
                    `(member x ',(copy-list (cdr spec))))
                   ((eq (car spec) 'cons)
                    `(and (consp x)
                          ,@(if (cdr spec) `((let ((x (car x)))
                                               ,(convert-body (cadr spec)))))
                          ,@(if (cddr spec) `((let ((x (cdr x)))
                                                ,(convert-body (caddr spec)))))))
                   ((eq (car spec) 'satisfies)
                    `(funcall (function ,(cadr spec)) x))
                   (T `(typep x ',(copy-tree spec))))))
    `(lambda (x) ,(convert-body spec))))

(declaim (ftype (function (real real) (values boolean &optional)) priority->))
(defun priority-> (x y)
  #+sbcl ; due to real.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (> x y))

(declaim (ftype (function (pprint-dispatch real fixnum) (values null &optional)) adjust-counts))
(defun adjust-counts (table priority delta)
  (maphash #'(lambda (key value)
	         (declare (ignore key))
	       (if (priority-> priority (car (full-spec value)))
		   (incf (the fixnum (test value)) delta)))
	   (conses-with-cars table))
  (maphash #'(lambda (key value)
	         (declare (ignore key))
	       (if (priority-> priority (car (full-spec value)))
		   (incf (the fixnum (test value)) delta)))
	   (structures table)))

(declaim (ftype (function ((or symbol cons) (or symbol function)
			     &optional real (or null pprint-dispatch))
			  (values null &optional))
		set-pprint-dispatch))
(defun set-pprint-dispatch (type-specifier function
			    &optional (priority 0) (table *print-pprint-dispatch*))
  #-(or sbcl cmu)
  (when (or (not (numberp priority)) (complexp priority))
    (error "invalid PRIORITY argument ~A to SET-PPRINT-DISPATCH" priority))
  (let* ((category (specifier-category type-specifier))
	 (entry
	   (if function
	     (make-entry :test (if (not (eq category :other)) nil
				 (let ((pred (<specifier-fn> type-specifier)))
				   (if (and (consp (caddr pred))
					    (symbolp (caaddr pred))
					    (equal (cdaddr pred) '(x)))
				     (symbol-function (caaddr pred))
				     (compile nil pred))))
			 :fn function
			 :full-spec (list priority type-specifier)))))
    (ecase category
      (:cons-with-car
	(if (null entry)
	    (remhash (cadadr type-specifier) (conses-with-cars table))
	    (setf (test entry)
		    (count-if #'(lambda (e)
				  (priority-> (car (full-spec e)) priority))
			      (others table))
		  (gethash (cadadr type-specifier) (conses-with-cars table))
		     entry)))
      (:structure-type
	(if (null entry)
	    (remhash type-specifier (structures table))
	    (setf (test entry)
		    (count-if #'(lambda (e)
				  (priority-> (car (full-spec e)) priority))
			      (others table))
	          (gethash type-specifier (structures table))
		    entry)))
      (:other
	 (let ((old (car (member type-specifier (others table) :test #'equal
				 :key #'(lambda (e) (cadr (full-spec e)))))))
	   (when old
	     (setf (others table) (delete old (others table)))
	     (adjust-counts table (car (full-spec old)) -1)))
	 (when entry
	   (let ((others (cons nil (others table))))
	      (do ((l others (cdr l)))
		  ((null (cdr l)) (rplacd l (list entry)))
		(when (priority-> priority (car (full-spec (cadr l))))
		  (rplacd l (cons entry (cdr l)))
		  (return nil)))
	      (setf (others table) (cdr others)))
	   (adjust-counts table priority 1)))))
  nil)

(declaim (ftype (function (t entry) (values boolean &optional)) fits))
(defun fits (obj entry) (and (funcall (coerce (test entry) 'function) obj) t))

(declaim (ftype (function (t pprint-dispatch) (values (or null (or symbol function)) &optional))
		get-printer))
(defun get-printer (object table)
  (let* ((entry (if (consp object)
		    (gethash (car object) (conses-with-cars table))
		    (gethash (type-of object) (structures table)))))
    (if (not entry)
	(setq entry (find object (others table) :test #'fits))
	(loop :for count :of-type fixnum :below (test entry)
	      :for o :in (others table)
	      :when (fits object o)
	        :do (setq entry o) (loop-finish)))
    (when entry (fn entry))))

(defun non-pretty-print (object s)
  (write object
	 :level (if *print-level*
		  (- (the fixnum *print-level*) *current-level*))
	 :pretty nil
	 :stream s))

(declaim (ftype (function (t &optional (or null pprint-dispatch))
			  (values (or symbol function) boolean &optional))
		pprint-dispatch))
(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (let* ((table (or table *IPD*))
	 (fn (get-printer object table)))
    (values (or fn #'non-pretty-print) (not (null fn)))))

(setq *IPD* (make-pprint-dispatch))

(defun pprint-dispatch-entries (table)
  (let ((stuff (copy-list (others table))))
    (maphash #'(lambda (key val) (declare (ignore key))
		       (push val stuff))
	     (conses-with-cars table))
    (maphash #'(lambda (key val) (declare (ignore key))
		       (push val stuff))
	     (structures table))
    (sort stuff #'priority-> :key #'(lambda (x) (car (full-spec x))))))

(defun show (entry output)
  (funcall (formatter "~{~_P=~4D ~W~} F=~W ")
	   output (full-spec entry) (fn entry)))

(defun initialize ()
  (when (eq *print-pprint-dispatch* T)
    (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))))

