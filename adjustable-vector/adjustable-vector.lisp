(in-package :cl-user)
(defpackage :pxp.adjustable-vector (:use :cl)
  (:shadow write)
  (:export
    #:new
    #:overflow-protect
    #:write
    #:ref
    #:index ; type
    #:adjustable-vector ; type
    ;; Re-Export from common-lisp as abstraction barrier.
    #:fill
    #:replace
    #:subseq
    #:position
    ))
(in-package :pxp.adjustable-vector)

(declaim (optimize speed))

;; Type

(deftype index () '(mod #.array-total-size-limit))

(deftype adjustable-vector (&optional (type t))
  `(simple-array ,type (*)))

;; CREATE

(defun new (size &key (element-type t))
  #+sbcl ; Due to ELEMENT-TYPE is unknown in compile time.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (make-array size :element-type element-type))

;; REFER

(setf (symbol-function 'ref) #'aref)

;; UPDATE

(if (ignore-errors (fdefinition '(setf aref)))
  (setf (fdefinition '(setf ref)) (fdefinition '(setf aref)))
  (defun (setf ref) (new adjustable-vector index)
    #+sbcl ; Due to upgraded element type is unknown at compile time.
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (setf (aref adjustable-vector index) new)))

;; OVERFLOW-PROTECT

(defmacro overflow-protect ((accessor <object> <next-index>
				      &key (entry-size 1)
				      (min-size 256))
			    &body body &environment env)
  (declare ((unsigned-byte 32) min-size entry-size))
  ;; KLUDGE:
  (assert (constantp entry-size env))
  (assert (constantp min-size env))
  ;; Trivial-syntax-check.
  (eval-when (:load-toplevel)
    ;; CLHS allow to use-then-define style code.
    ;; So the accessor may not exist in compile time.
    ;; We check accessor existence only in load-time.
    (assert (fboundp accessor) ()
            "ACCESSOR must be the function name that can read adjustable-vector from ~S, but ~S."
            <object> accessor))
  ;; Binding.
  (let* ((?next-index (gensym "NEXT-INDEX"))
	 (?object (gensym "OBJECT")))
    ;; The Body
    `(let ((,?next-index ,<next-index>)
	   (,?object ,<object>))
       #+sbcl ; Due to array element type is unknown.
       (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (when (and (< ,(- min-size entry-size) ,?next-index) ; seldom happens
		  (< (- (length (,accessor ,?object)) ,entry-size)
		     ,?next-index))
	 (let* ((old (,accessor ,?object))
		(new (make-array (+ ,?next-index ,(if (= entry-size 1)
						    50
						    (* 10 entry-size)))
				 :element-type (array-element-type old))))
	   (replace new old)
	   (setf (,accessor ,?object) new)))
       ,@body)))

;;; As abstraction barrier.

(declaim (ftype (function (vector stream &key (:start (or null (mod #.array-total-size-limit)))
				  (:end (or null (mod #.array-total-size-limit)))))
		write))
(defun write (adjustable-vector output &key start end)
  (cond
    ((stringp adjustable-vector)
     (write-string adjustable-vector output :start (or start 0) :end end))
    ((and (or (null start) (= 0 start)) (null end))
     (cl:write adjustable-vector :stream output))
    (t
      (cl:write (subseq adjustable-vector (or start 0) end) :stream output))))
