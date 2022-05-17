(cl:in-package :cl-user)
(defpackage :pxp.queue (:use :cl)
  (:export
    #:queue ; type name.
    #:initialize
    #:show
    ;;; Type
    #:Qindex
    #:newline-kind
    #:indent-kind
    #:newline-fresh-kind
    ;;; Queue columns.
    ;#:Qtype
    ;#:Qkind
    #:Qpos
    ;#:Qdepth
    ;#:Qend
    ;#:Qoffset
    ;#:Qarg
    ;;; accessor
    #:Qleft
    ;#:Qright
    ;;;
    ;#:Qnext
    #:enqueue
    #:for-each ; Iterator.
    #:flush
    #:fresh-newline-p
    ))
(in-package :pxp.queue)

(declaim (optimize speed))

;;;; Use vector as table.
;;;; Each row has seven columns.

(eval-when (:execute :load-toplevel :compile-toplevel)
  ;; not used at run time.
  ;; In other words, used in read time, so eval-when is needed.
  (defconstant queue-entry-size 7) ; columns

  (defconstant queue-min-size (* 75. queue-entry-size))) ; minimum rows.

(deftype Qindex () 'pxp.adjustable-vector:index)

(deftype newline-fresh-kind () '(member :unconditional :fresh))
(deftype newline-kind () '(member :mandatory :miser :fill :linear))
(deftype indent-kind () '(member :current :block))

;;;; The queue column types.
(deftype Qtype () '(member :newline :ind :start-block :end-block))
(deftype Qkind ()
  '(or newline-kind newline-fresh-kind indent-kind))
(deftype Qpos ()
  "Total position corresponding to this entry"
  'pxp.adjustable-vector:index)
(deftype Qdepth ()
  "Depth in blocks of this entry."
  '(mod #.array-total-size-limit))
(deftype Qend ()
  "Offset to entry marking end of section this entry starts. (NIL until known.)
  Only :start-block and non-literal :newline entries can start sections."
  '(or null (mod #.array-total-size-limit)))
(deftype Qoffset ()
  "Offset to :END-BLOCK for :START-BLOCK (NIL until known)."
  '(or null (mod #.array-total-size-limit)))
(deftype Qarg ()
  "QARG for :IND indentation delta
       for :START-BLOCK suffix in the block if any.
                        or if per-line-prefix then cons of suffix and
                        per-line-prefix.
       for :END-BLOCK suffix for the block if any."
  '(or string cons))
(deftype Qentry ()
  "The queue entries have several parts."
  '(or Qtype Qkind Qpos Qdepth Qend Qoffset Qarg))

(defmacro Qtype   (queue index) `(pxp.adjustable-vector:ref (queue ,queue) ,index))
(defmacro Qkind   (queue index) `(pxp.adjustable-vector:ref (queue ,queue) (1+ ,index)))
(defmacro Qpos    (queue index) `(pxp.adjustable-vector:ref (queue ,queue) (+ (the (mod #.(- array-total-size-limit queue-entry-size)) ,index) 2)))
(defmacro Qdepth  (queue index) `(the Qdepth (pxp.adjustable-vector:ref (queue ,queue) (+ (the (mod #.(- array-total-size-limit queue-entry-size)) ,index) 3))))
(defmacro Qend    (queue index) `(the Qend (pxp.adjustable-vector:ref (queue ,queue) (+ (the (mod #.(- array-total-size-limit queue-entry-size)) ,index) 4))))
(defmacro Qoffset (queue index) `(the Qoffset (pxp.adjustable-vector:ref (queue ,queue) (+ (the (mod #.(- array-total-size-limit queue-entry-size)) ,index) 5))))
(defmacro Qarg    (queue index) `(pxp.adjustable-vector:ref (queue ,queue) (+ (the (mod #.(- array-total-size-limit queue-entry-size)) ,index) 6)))

(define-compiler-macro queue (queue)
  `(the (pxp.adjustable-vector:adjustable-vector Qentry) (slot-value ,queue 'queue)))

(define-compiler-macro Qleft (queue)
  `(the fixnum (slot-value ,queue 'Qleft)))

(define-compiler-macro Qright (queue)
  `(the fixnum (slot-value ,queue 'Qright)))

(defclass queue ()
  ((queue :initform (pxp.adjustable-vector:new #.queue-min-size :element-type 'Qentry)
          :initarg :queue :accessor queue
          :documentation
          "This holds a queue of action descriptors.")
   (Qleft :initform nil :initarg :qleft :accessor Qleft
          :documentation "Point to the next entry to dequeue.")
   (Qright :initform nil :initarg :qright :accessor Qright
           :documentation "Point to the last entry enqueued.")))

(defun initialize (xp)
  (setf (Qleft xp) 0)
  (setf (Qright xp) #.(- queue-entry-size)))

(defun Qemptyp (queue)
  (> (Qleft queue) (Qright queue)))

(defmacro Qnext (index) `(the (mod #.(- array-total-size-limit queue-entry-size))
			      (+ ,index #.queue-entry-size)))

(defmacro for-each (((ptr &rest column+) <queue> &key consume) &body body)
  "(for-each ((Ptr Column+) Queue &key Consume) Body)
Ptr := Variable
Columns := [ Qtype | Qkind | Qpos | Qdepth | Qend | Qoffset | Qarg ]
Consume := Boolean, not evaluated.
Iterate over <QUEUE> entries.
If Consume is true, Qentries are destructively consumed.
BLOCK NIL is implicitly achieved."
  (check-type consume boolean)
  (let ((?ptr ptr)
	(?queue (gensym "QUEUE")))
    `(do* ((,?queue ,<queue>)
	   (,?ptr (Qleft ,?queue) ,(if consume
				     `(QLeft ,?queue)
				     `(Qnext ,?ptr))))
       (,(if consume
	   `(Qemptyp ,?queue)
	   `(not (< ,?ptr (Qright ,?queue))))
	 ,@(when consume `((initialize ,?queue))))
       (declare (fixnum ,?ptr))
       (symbol-macrolet ,(mapcar (lambda (column)
				   (let ((accessor (uiop:find-symbol* (symbol-name column) :pxp.queue)))
				     ;; Trivial-syntax-check
				     #-(or abcl)
				     ;; abcl fail when ACCESSOR is (member Qpos Qdepth Qarg).
				     (assert (subtypep accessor 'Qentry) ()
		                       "~S must be Qentry." accessor)
				     `(,column (,accessor ,?queue ,?ptr))))
				 column+)
         ,@body)
       ,@(when consume
	   `((setf (Qleft ,?queue) (Qnext (Qleft ,?queue))))))))

(declaim (ftype (function (queue fixnum) (values null &optional)) sync-depth))
(defun sync-depth (xp depth)
  (for-each ((ptr Qend Qdepth Qtype) xp)
    (when (and (null Qend)
	       (not (< Qdepth depth))
	       (member Qtype '(:newline :start-block)))
      (setf Qend (- (Qright xp) ptr)))))

(declaim (ftype (function (queue fixnum) (values null &optional)) sync-offset))
(defun sync-offset (xp depth)
  (for-each ((ptr Qdepth Qtype Qoffset) xp)
    (when (and (= Qdepth depth)
	       (eq Qtype :start-block)
	       (null Qoffset))
      (setf Qoffset (- (the (mod #.(- array-total-size-limit queue-entry-size)) (Qright xp)) ptr))
      (return nil))))

(declaim (ftype (function (queue Qtype (or null Qkind)
				 &key (:arg (or null t))
				 (:position Qpos)
				 (:depth Qdepth)
				 (:sync (member nil :offset :depth)))
			  (values &optional))
		enqueue))
(defun enqueue (xp type kind &key arg depth position sync)
  (incf (the fixnum (Qright xp)) #.queue-entry-size)
  (when (> (Qright xp) #.(- queue-min-size queue-entry-size))
    (replace (queue xp) (queue xp) :start2 (Qleft xp) :end2 (Qright xp))
    (setf (Qright xp) (the fixnum (- (Qright xp) (Qleft xp))))
    (setf (Qleft xp) 0))
  (pxp.adjustable-vector:overflow-protect (queue xp (Qright xp)
						 :entry-size #.queue-entry-size
						 :min-size #.queue-min-size))
  (setf (Qtype xp (Qright xp)) type
	(Qkind xp (Qright xp)) kind
	(Qpos xp (Qright xp)) position
	(Qdepth xp (Qright xp)) depth
	(Qend xp (Qright xp)) nil
	(Qoffset xp (Qright xp)) nil
	(Qarg xp (Qright xp)) arg)
  (case sync
    (:depth (sync-depth xp depth))
    (:offset (sync-offset xp depth)))
  (values))

(defun show (xp s)
  (unless (Qemptyp xp)
    (funcall (formatter "~&ptr type         kind           pos depth end offset arg") s)
    (for-each ((p Qtype Qkind Qpos Qend Qoffset Qarg) xp)
      (funcall (formatter "~&~4A~13A~15A~4A~6A~4A~7A~A") s
	      (/ (the fixnum (- p (Qleft xp))) #.queue-entry-size)
	      Qtype
	      (if (member Qtype '(:newline :ind)) Qkind "")
	      (pxp.buffer:buffer-position<-total-position xp Qpos)
	      (Qdepth xp p)
	      (if (not (member Qtype '(:newline :start-block))) ""
		(and Qend
		     (/ (the fixnum (- (+ p Qend) (Qleft xp))) #.queue-entry-size)))
	      (if (not (eq Qtype :start-block)) ""
		(and Qoffset
		     (/ (the fixnum (- (+ p Qoffset) (Qleft xp))) #.queue-entry-size)))
	      (if (not (member Qtype '(:ind :start-block :end-block))) ""
		QarG)))))

(defun flush (queue)
  (setf (Qleft queue) (Qnext (Qright queue))))

(declaim (ftype (function (queue (mod #.array-total-size-limit)) (values boolean &optional))
		fresh-newline-p))
(defun fresh-newline-p (queue index)
  (typep (Qkind queue index) 'newline-fresh-kind))
