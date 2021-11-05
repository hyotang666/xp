(cl:in-package :cl-user)
(defpackage :pxp.buffer (:use :cl)
  (:export
    #:buffer ; class name.
    #:buffer-ptr
    #:buffer-offset
    ;; printer
    #:show
    #:show-detail
    #:write-to
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

(defclass buffer ()
  ((buffer
     :initform (make-array #.buffer-min-size :element-type 'character)
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
       	   " Positions are stored in this form."))))

(defun show (buffer output)
  (fresh-line output)
  (write-string "buffer= " output)
  (write-string (buffer buffer) output :start 0 :end (max (buffer-ptr buffer) 0)))

(defun show-detail (buffer output)
  (cl:format output "charpos= ~D buffer-ptr= ~D buffer-offset= ~D"
	     (charpos buffer) (buffer-ptr buffer) (buffer-offset buffer)))

(defun write-to (output buffer &key (end (buffer-ptr buffer)))
  (write-string (buffer buffer) output :end end))

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

(defmacro may-resize (buffer vect ptr)
  (let* ((min-size
	   (symbol-value
	     (uiop:find-symbol* (concatenate 'string (string vect) "-MIN-SIZE")
				:pxp.buffer)))
	 (entry-size
	   (symbol-value
	     (uiop:find-symbol* (concatenate 'string (string vect) "-ENTRY-SIZE")
				:pxp.buffer))))
    `(when (and (> ,ptr ,(- min-size entry-size)) ;seldom happens
		(> ,ptr (- (length (,vect ,buffer)) ,entry-size)))
       (let* ((old (,vect ,buffer))
	      (new (make-array (+ ,ptr ,(if (= entry-size 1) 50
					    (* 10 entry-size)))
			       :element-type (array-element-type old))))
	 (replace new old)
	 (setf (,vect ,buffer) new)))))

(defun add-char (char buffer)
  (let ((new-buffer-end (1+ (buffer-ptr buffer))))
    (may-resize buffer buffer new-buffer-end)
    (setf (char (buffer buffer) (buffer-ptr buffer)) char
	  (buffer-ptr buffer) new-buffer-end)))

(defun add-string (string buffer &key start end mode)
  (let ((new-buffer-end (+ (buffer-ptr buffer) (- end start))))
    (may-resize buffer buffer new-buffer-end)
    (loop :with buf = (buffer buffer)
	  :for i :upfrom (buffer-ptr buffer)
	  :for j :upfrom start :below end
	  :do (setf (char buf i) (funcall mode (char string j))))
    (setf (buffer-ptr buffer) new-buffer-end)))

(defun skip-to (length buffer)
  (let ((end (+ (buffer-ptr buffer) length)))
    (may-resize buffer buffer end)
    (fill (buffer buffer) #\space :start (buffer-ptr buffer) :end end)
    (setf (buffer-ptr buffer) end)))

(defun prefix (buffer &key rewind)
  (subseq (buffer buffer) (- (buffer-ptr buffer) rewind)
	  (buffer-ptr buffer)))

;;;; BUFFER-OFFSET

(defun BP<-TP (buffer ptr)
  (- ptr (buffer-offset buffer)))

(defun flush (buffer)
  (incf (buffer-offset buffer) (buffer-ptr buffer))
  (inc-ptr buffer)
  (setf (buffer-ptr buffer) 0))

(defun last-non-blank (buffer &key end)
  (position #\space (buffer buffer) :test-not #'char= :from-end T :end end))

(defun shift (buffer change &key prefix prefix-end out-point)
  (setf (charpos buffer) 0)
  (when (plusp change)                  ;almost never happens
    (may-resize buffer buffer (+ (buffer-ptr buffer) change)))
  (replace (buffer buffer) (buffer buffer) :start1 prefix-end
	   :start2 out-point :end2 (buffer-ptr buffer))
  (replace (buffer buffer) prefix :end2 prefix-end)
  (incf (buffer-ptr buffer) change)
  (decf (buffer-offset buffer) change))
