(cl:in-package :cl-user)
(defpackage :pxp.buffer (:use :cl)
  (:shadow write)
  (:export
    #:buffer ; class name.
    #:buffer-ptr
    #:buffer-offset
    ;; printer
    #:show
    #:show-detail
    #:write
    ;;
    #:initialize
    #:skip-to
    #:flush
    #:last-non-blank
    #:left-most-p
    #:too-large-p
    #:add-char
    #:add-string
    #:prefix
    #:shift
    #:BP<-TP
    #:LP<-BP
    #:TP<-BP
    ;;
    #:buffer-min-size
    #:buffer-entry-size
    ))
(in-package :pxp.buffer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant buffer-min-size 256.)
  (defconstant buffer-entry-size 1)
  )

#-(or :franz-inc)
(defun output-position  (&optional (s *standard-output*)) (declare (ignore s)) nil)

#+:franz-inc
(defun output-position  (&optional (s *standard-output*)) (excl::charpos s))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; PXP.ADJUSTABLE-VECTOR:OVERFLOW-PROTECT needs this eval-when.
(defclass buffer ()
  ((buffer
     :initform (pxp.adjustable-vector:new #.buffer-min-size :element-type 'character)
     :initarg :buffer :accessor buffer
     :documentation
     #.(cl:format nil "~@{~A~^~%~}"
       	   "This is a vector of characters (eg a string) that builds up the"
       	   "line images that will be printed out."))
   (charpos
     :initform nil :initarg :charpos :accessor charpos
     :documentation
     #.(cl:format nil "~@{~A~^~%~}"
       	   "The output character position of the first character in the buffer"
       	   "(non-zero only if a partial line has been output)."))
   (buffer-ptr
     :initform nil :initarg :buffer-ptr :accessor buffer-ptr
     :documentation
     "The buffer position where the next character should be inserted in the string.")
   (buffer-offset
     :initform nil :initarg :buffer-offset :accessor buffer-offset
     :documentation
     #.(cl:format nil "~@{~A~^~%~}"
       	   "Used in computing total lengths."
       	   "It is changed to reflect all shifting and insertion of prefixes so that"
       	   "total length computes things as they would be if they were"
       	   "all on one line.  Positions are kept three different ways"
       	   "Buffer position (eg BUFFER-PTR)"
       	   "Line position (eg (+ BUFFER-PTR CHARPOS)).  Indentations are stored in this form."
       	   "Total position if all on one line (eg (+ BUFFER-PTR BUFFER-OFFSET))"
       	   " Positions are stored in this form.")))))

(defun write (buffer output &key (end (buffer-ptr buffer)))
  (pxp.adjustable-vector:write (buffer buffer) output :end end))

(defun show (buffer output)
  (fresh-line output)
  (write-string "buffer= " output)
  (write buffer output :end (max (buffer-ptr buffer) 0)))

(defun show-detail (buffer output)
  (cl:format output "charpos= ~D buffer-ptr= ~D buffer-offset= ~D"
	     (charpos buffer) (buffer-ptr buffer) (buffer-offset buffer)))

;;;; CHARPOS

(defun LP<-BP (buffer &optional (ptr (buffer-ptr buffer)))
  (+ ptr (charpos buffer)))

(defun left-most-p (buffer)
  (zerop (LP<-BP buffer)))

(defun too-large-p (buffer &key max)
  (< max (LP<-BP buffer)))

(defun BP<-LP (buffer ptr) (- ptr (charpos buffer)))

(defun initialize (buffer stream)
  (setf (charpos buffer) (cond ((output-position stream)) (T 0))
	(buffer-offset buffer) (charpos buffer)
	(buffer-ptr buffer) 0)
  )

(defun inc-ptr (buffer)
  (incf (charpos buffer) (buffer-ptr buffer)))

;;;; BUFFER-PTR

(defun TP<-BP (buffer) (+ (buffer-ptr buffer) (buffer-offset buffer)))

;We don't use adjustable vectors or any of that, because we seldom have
;to actually extend and non-adjustable vectors are a lot faster in
;many Common Lisps.

(defun add-char (char buffer)
  (let ((new-buffer-end (1+ (buffer-ptr buffer))))
    (pxp.adjustable-vector:overflow-protect (buffer buffer new-buffer-end)
      (setf (pxp.adjustable-vector:ref (buffer buffer) (buffer-ptr buffer)) char
	    (buffer-ptr buffer) new-buffer-end))))

(defun add-string (string buffer &key start end mode)
  (let ((new-buffer-end (+ (buffer-ptr buffer) (- end start))))
    (pxp.adjustable-vector:overflow-protect (buffer buffer new-buffer-end)
      (loop :with buf = (buffer buffer)
            :for i :upfrom (buffer-ptr buffer)
            :for j :upfrom start :below end
            :do (setf (pxp.adjustable-vector:ref buf i) (funcall mode (char string j))))
      (setf (buffer-ptr buffer) new-buffer-end))))

(defun skip-to (length buffer)
  (let ((end (+ (buffer-ptr buffer) length)))
    (pxp.adjustable-vector:overflow-protect (buffer buffer end)
      (pxp.adjustable-vector:fill (buffer buffer) #\space :start (buffer-ptr buffer) :end end)
      (setf (buffer-ptr buffer) end))))

(defun prefix (buffer &key rewind)
  (pxp.adjustable-vector:subseq (buffer buffer)
				(- (buffer-ptr buffer) rewind)
				(buffer-ptr buffer)))

;;;; BUFFER-OFFSET

(defun BP<-TP (buffer ptr)
  (- ptr (buffer-offset buffer)))

(defun flush (buffer)
  (incf (buffer-offset buffer) (buffer-ptr buffer))
  (inc-ptr buffer)
  (setf (buffer-ptr buffer) 0))

(defun last-non-blank (buffer &key end)
  (pxp.adjustable-vector:position #\space (buffer buffer) :test-not #'char= :from-end T :end end))

(defun shift (buffer change &key prefix prefix-end out-point)
  (flet ((set-prefix! (adjustable-vector prefix end2)
           (pxp.adjustable-vector:replace adjustable-vector prefix :end2 end2))
	 (shift! (adjustable-vector start1 start2 end2)
           (pxp.adjustable-vector:replace adjustable-vector adjustable-vector
					  :start1 start1
					  :start2 start2
					  :end2 end2)))
    (setf (charpos buffer) 0)
    (when (plusp change) ; almost never happens
      (pxp.adjustable-vector:overflow-protect (buffer buffer (+ (buffer-ptr buffer) change))))
    (shift! (buffer buffer) prefix-end out-point (buffer-ptr buffer))
    (set-prefix! (buffer buffer) prefix prefix-end)
    (incf (buffer-ptr buffer) change)
    (decf (buffer-offset buffer) change)))
