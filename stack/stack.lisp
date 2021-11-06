(in-package :cl-user)
(defpackage :pxp.stack (:use :cl)
  (:shadow block)
  (:export
    #:stack
    #:initialize
    ;; Printer
    #:show-detail
    #:show-ptr
    #:show-section-start
    ;;; Accessor
    #:block-stack
    #:prefix-stack
    #:prefix
    #:suffix
    ;;; PTR
    #:block-stack-ptr
    #:non-blank-prefix-ptr
    #:prefix-stack-ptr
    #:initial-prefix-ptr
    #:prefix-ptr
    #:suffix-ptr
    ;;; Stack.
    #:push-block-stack
    #:pop-block-stack
    #:push-prefix-stack
    #:pop-prefix-stack
    ;;;
    #:set-reverse-suffix
    #:depth-in-blocks
    #:set-indentation-prefix
    #:set-prefix
    #:set-suffix
    #:section-start-line
    #:section-start
    ))
(in-package :pxp.stack)

(eval-when (:execute :load-toplevel :compile-toplevel) ; used at compile time.
  (defvar block-stack-entry-size 1)
  (defvar prefix-stack-entry-size 5))

(eval-when (:execute :load-toplevel :compile-toplevel) ;not used at run time.
  (defvar block-stack-min-size #.(* 35. block-stack-entry-size))
  (defvar prefix-stack-min-size #.(* 30. prefix-stack-entry-size)))

;;;; Prefix-stack entries.
(deftype pointer () '(mod #.array-total-size-limit))
(deftype prefix-ptr () "Current length of PREFIX." 'pointer)
(deftype suffix-ptr () "Current length of pending suffix." 'pointer)
(deftype non-blank-prefix-ptr () "Current length of non-blank prefix." 'pointer)
(deftype initial-prefix-ptr () "Prefix-ptr at the start of this block." 'pointer)
(deftype section-start-line ()
  "Line-no value at last non-literal break at this level."
  'pointer)
(deftype prefix-stack-entry ()
  '(or prefix-ptr suffix-ptr non-blank-prefix-ptr initial-prefix-ptr section-start-line))

(defclass stack ()
  ((depth-in-blocks
     :initform nil :initarg :depth-in-blocks
     :accessor depth-in-blocks
     :documentation "Number of logical blocks at QRIGHT that are started but not ended.")
   (block-stack
     :initform (pxp.adjustable-vector:new #.block-stack-min-size) :initarg :block-stack
     :accessor block-stack
     :documentation
     #.(format nil "~@{~A~^~%~}"
       	   "This stack is pushed and popped in accordance with the way blocks are"
       	   "nested at the moment they are entered into the queue.  It contains the"
       	   "following block specific value."
       	   "SECTION-START total position where the section (see AIM-1102)"
       	   "that is rightmost in the queue started."))
   (block-stack-ptr :initform nil :initarg :block-stack-ptr
       	     :accessor block-stack-ptr)
   (prefix :initform (pxp.adjustable-vector:new #.pxp.buffer:buffer-min-size :element-type 'character)
           :initarg :prefix :accessor prefix
           :documentation
           "Stores the prefix that should be used at the start of the line")
   (prefix-stack
     :initform (pxp.adjustable-vector:new #.prefix-stack-min-size :element-type 'prefix-stack-entry)
     :initarg :prefix-stack :accessor prefix-stack
     :documentation
     "This stack is pushed and popped in accordance with the way blocks
     are nested at the moment things are taken off the queue and printed.")
   (prefix-stack-ptr :initform nil :initarg :prefix-stack-ptr
       	      :accessor prefix-stack-ptr)
   (suffix :initform (pxp.adjustable-vector:new #.pxp.buffer:buffer-min-size :element-type 'character)
           :initarg :suffix :accessor suffix
           :documentation
           "Stores the suffixes that have to be printed to close of the current
           open blocks.  For convenient in popping, the whole suffix
           is stored in reverse order.")))

(defmacro prefix-ptr (stack)
  `(pxp.adjustable-vector:ref (prefix-stack ,stack) (prefix-stack-ptr ,stack)))
(defmacro suffix-ptr (stack)
  `(pxp.adjustable-vector:ref (prefix-stack ,stack) (+ (prefix-stack-ptr ,stack) 1)))
(defmacro non-blank-prefix-ptr (stack)
  `(pxp.adjustable-vector:ref (prefix-stack ,stack) (+ (prefix-stack-ptr ,stack) 2)))
(defmacro initial-prefix-ptr (stack)
  `(pxp.adjustable-vector:ref (prefix-stack ,stack) (+ (prefix-stack-ptr ,stack) 3)))
(defmacro section-start-line (stack)
  `(pxp.adjustable-vector:ref (prefix-stack ,stack) (+ (prefix-stack-ptr ,stack) 4)))
(defmacro section-start (stack)
  `(pxp.adjustable-vector:ref (block-stack ,stack) (block-stack-ptr ,stack)))

(defun initialize (stack)
  (setf (depth-in-blocks stack) 0)
  (setf (block-stack-ptr stack) 0)
  (setf (section-start stack) 0)
  (setf (prefix-stack-ptr stack) #.(- prefix-stack-entry-size)))

(defmacro check-size (xp vect ptr)
  (let* ((min-size
	   (symbol-value
	     (intern (concatenate 'string (string vect) "-MIN-SIZE")
		     (find-package :pxp))))
	 (entry-size
	   (symbol-value
	     (intern (concatenate 'string (string vect) "-ENTRY-SIZE")
		     (find-package :pxp)))))
    `(when (and (> ,ptr ,(- min-size entry-size)) ;seldom happens
		(> ,ptr (- (length (,vect ,xp)) ,entry-size)))
       (let* ((old (,vect ,xp))
	      (new (make-array (+ ,ptr ,(if (= entry-size 1) 50
					    (* 10 entry-size)))
			       :element-type (array-element-type old))))
	 (replace new old)
	 (setf (,vect ,xp) new)))))

(declaim (ftype (function (stack) (values &optional))
		push-block-stack))
(defun push-block-stack (stack)
  (incf (block-stack-ptr stack) #.block-stack-entry-size)
  (pxp.adjustable-vector:overflow-protect (block-stack stack (block-stack-ptr stack)
						       :min-size #.block-stack-min-size))
  (values))

(declaim (ftype (function (stack) (values &optional))
		pop-block-stack))
(defun pop-block-stack (stack)
  (decf (block-stack-ptr stack) #.block-stack-entry-size)
  (values))

(defun show-detail (stack output)
  (unless (minusp (prefix-stack-ptr stack))
    (format output "~&prefix= ")
    (pxp.adjustable-vector:write (prefix stack) output :end (max (prefix-ptr stack) 0))
    (format output "~&suffix= ")
    (pxp.adjustable-vector:write (suffix stack) output :end (max (suffix-ptr stack) 0))))
(declaim (ftype (function (stack pointer) (values &optional)) set-indentation-prefix))
(defun set-indentation-prefix (stack new-position)
  (let ((new-ind (max (non-blank-prefix-ptr stack) new-position)))
    (setf (prefix-ptr stack) (initial-prefix-ptr stack))
    (pxp.adjustable-vector:overflow-protect (prefix stack new-ind)
      (when (> new-ind (prefix-ptr stack))
        (pxp.adjustable-vector:fill (prefix stack) #\space :start (prefix-ptr stack) :end new-ind))
      (setf (prefix-ptr stack) new-ind)))
  (values))

(declaim (ftype (function (stack string) (values &optional)) set-prefix))
(defun set-prefix (stack prefix-string)
  (pxp.adjustable-vector:replace (prefix stack) prefix-string
				 :start1 (- (prefix-ptr stack) (length prefix-string)))
  (setf (non-blank-prefix-ptr stack) (prefix-ptr stack))
  (values))
(declaim (ftype (function (stack)
			  (values &optional))
		push-prefix-stack
		pop-prefix-stack))
(defun push-prefix-stack (stack)
  (let ((old-prefix 0) (old-suffix 0) (old-non-blank 0))
    (when (not (minusp (prefix-stack-ptr stack)))
      (setq old-prefix (prefix-ptr stack)
	    old-suffix (suffix-ptr stack)
	    old-non-blank (non-blank-prefix-ptr stack)))
    (incf (prefix-stack-ptr stack) #.prefix-stack-entry-size)
    (pxp.adjustable-vector:overflow-protect (prefix-stack stack (prefix-stack-ptr stack)
							  :entry-size #.prefix-stack-entry-size
							  :min-size #.prefix-stack-min-size)
      (setf (prefix-ptr stack) old-prefix
            (suffix-ptr stack) old-suffix
            (non-blank-prefix-ptr stack) old-non-blank)))
  (values))

(defun pop-prefix-stack (stack)
  (decf (prefix-stack-ptr stack) #.prefix-stack-entry-size)
  (values))

(defun show-ptr (stack output)
  (unless (minusp (prefix-stack-ptr stack))
    (format output "~&initial-prefix-ptr prefix-ptr suffix-ptr non-blank start-line")
    (do ((save (prefix-stack-ptr stack)))
      ((minusp (prefix-stack-ptr stack)) (setf (prefix-stack-ptr stack) save))
      (format output "~& ~19A~11A~11A~10A~A"
		 (initial-prefix-ptr stack) (prefix-ptr stack) (suffix-ptr stack)
		 (non-blank-prefix-ptr stack) (section-start-line stack))
      (pop-prefix-stack stack))))

(defun show-section-start (stack output)
  (unless (minusp (block-stack-ptr stack))
    (format output "~&section-start")
    (do ((save (block-stack-ptr stack)))
      ((minusp (block-stack-ptr stack)) (setf (block-stack-ptr stack) save))
      (format output " ~D" (section-start stack))
      (pop-block-stack stack))))

(defun set-reverse-suffix (stack)
  (reverse-string-in-place (suffix stack) 0 (suffix-ptr stack)))

(declaim (ftype (function (string (mod #.array-total-size-limit)
				  (mod #.array-total-size-limit))
			  (values string &optional))
		reverse-string-in-place))
(defun reverse-string-in-place (string start end)
  (do ((i start (1+ i)) (j (1- end) (1- j))) ((not (< i j)) string)
    (rotatef (pxp.adjustable-vector:ref string i)
	     (pxp.adjustable-vector:ref string j))))

(declaim (ftype (function (stack string) (values &optional)) set-suffix))
(defun set-suffix (stack suffix-string)
  (let* ((end (length suffix-string))
	 (new-end (+ (suffix-ptr stack) end)))
    (pxp.adjustable-vector:overflow-protect (suffix stack new-end)
      (loop :for i :downfrom (1- new-end)
            :for j :upfrom 0 :below end
            :do (setf (pxp.adjustable-vector:ref (suffix stack) i)
		      (pxp.adjustable-vector:ref suffix-string j)))
      (setf (suffix-ptr stack) new-end)))
  (values))
