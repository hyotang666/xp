(defpackage :pxp.stream (:use :cl)
  (:shadow *print-right-margin* *print-lines* *print-miser-width*)
  (:export #:xp-structure ;;;; Type name.
	   #:xp-structure-p ; Predicate.
	   #:tab-kind ; type name.
	   ;;;; Configure
	   #:*circularity-hash-table*
	   #:*parents*
	   #:*last-abbreviated-printing*
	   #:*abbreviation-happened*
	   *print-right-margin*
	   *print-lines*
	   *print-miser-width*
	   ;;;; Helpers
	   ;; WRITERS ++ suffixed.
	   #:write-char++
	   #:write-string++
	   ;; WRITERS + suffixed.
	   #:pprint-indent+
	   #:pprint-newline+
	   #:pprint-tab+
	   #:write-string+
	   ;; DSL Macros.
	   #:push-char-mode
	   #:pop-char-mode
	   #:with-block
	   ;; Printings.
	   #:attempt-to-output
	   #:call-with-xp-stream
	   #:circularity-process
	   #:maybe-print-fast
	   ;; Miscellaneous utilities.
	   #:decode-stream-arg
	   ))
(in-package :pxp.stream)

(declaim (optimize speed))

;;;; CONFIGURATION VARIABLES.

(declaim (type (or null (mod #.array-total-size-limit))
	       *print-right-margin* *print-miser-width*))
(defvar *print-right-margin* nil
  "+#/nil the right margin for pretty printing")

(defvar *print-lines* nil
  "+#/nil truncates printing after # lines")

(defvar *print-miser-width* 40.
  "+#/nil miser format starts when there is less than this width left")

(declaim (type (mod #.array-total-size-limit) *default-right-margin*))
(defvar *default-right-margin* 70.
  "controls default line length; must be a non-negative integer")

(defvar *print-shared* nil)

(defvar *last-abbreviated-printing*
	#'(lambda (&optional stream) (declare (ignore stream)) nil)
  "funcalling this redoes the last xp printing that was abbreviated.")

(defvar *abbreviation-happened* nil
  "t if current thing being printed has been abbreviated.")

(declaim (type (or null (mod #.most-positive-fixnum)) *locating-circularities*))
(defvar *locating-circularities* nil
  "Integer if making a first pass over things to identify circularities.
   Integer used as counter for #n= syntax.")

;;;; TYPE

(deftype char-mode ()
  '(member nil :up :down :cap0 :cap1 :capw))

;;;; XP-STRUCTURE

(define-compiler-macro char-mode-counter (xp)
  `(the (mod #.array-total-size-limit) (slot-value ,xp 'char-mode-counter)))

(define-compiler-macro linel (xp)
  `(the (mod #.array-total-size-limit) (slot-value ,xp 'linel)))

(define-compiler-macro line-no (xp)
  `(the (mod #.array-total-size-limit) (slot-value ,xp 'line-no)))

(defclass xp-structure (trivial-gray-streams:fundamental-character-output-stream
			 pxp.buffer:buffer pxp.stack:stack pxp.queue:queue)
  ((base-stream :initform nil :initarg :base-stream
		:type (or null stream) :accessor base-stream
		:documentation "The stream io eventually goes to.")
   (linel :accessor linel
          :documentation "The line length to use for formatting." )
   (line-limit :initform nil :initarg :line-limit :accessor line-limit
	       :documentation "If non-NIL the max number of lines to print.")
   (line-no :accessor line-no
            :documentation "number of next line to be printed.")
   (char-mode :type char-mode :initform nil :accessor char-mode)
   (char-mode-counter
     :accessor char-mode-counter
     :documentation "depth of nesting of ~(...~)")))

;;;; CONSTRUCTOR

(declaim (ftype (function (&optional stream) (values (or null (mod #.array-total-size-limit)) &optional))
		output-width))
;default (bad) definitions for the non-portable functions

#-(or :franz-inc)(eval-when (:execute :load-toplevel :compile-toplevel)
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil))


;Definitions for FRANZ Common Lisp. (Only verified for the version 1.3
;(5/31/87) currently running on suns at MIT.)

#+:franz-inc(eval-when (:execute :load-toplevel :compile-toplevel)
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil))

(defmethod shared-initialize :after ((o xp-structure) slot-names &key stream)
  (with-slots (base-stream linel line-limit line-no char-mode char-mode-counter) o
    (setf base-stream stream
	  linel (max 0 (or *print-right-margin*
			   (output-width stream)
			   *default-right-margin*))
	  line-limit *print-lines*
	  line-no 1
	  char-mode nil
	  char-mode-counter 0)))

;;;; PRINTER

(declaim (type boolean *describe-xp-streams-fully*))
(defvar *describe-xp-streams-fully* nil "Set to T to see more info.")

(defmethod print-object ((xp xp-structure) s)
  (print-unreadable-object (xp s :type t :identity nil)
    (funcall (formatter "stream ") s)
    (if (not (base-stream xp))
        (funcall (formatter "not currently in use") s)
        (funcall (formatter "outputting to ~S") s (base-stream xp)))
    (when (base-stream xp)
      (pxp.buffer:show xp s)
      (when (not *describe-xp-streams-fully*) (cl:princ " ..." s))
      (when *describe-xp-streams-fully*
        (funcall (formatter "~&   pos   _123456789_123456789_123456789_123456789") s)
        (funcall (formatter "~&depth-in-blocks= ~D linel= ~D line-no= ~D line-limit= ~D") s
                     (pxp.stack:depth-in-blocks xp) (linel xp) (line-no xp) (line-limit xp))
        (when (or (char-mode xp) (not (zerop (char-mode-counter xp))))
          (funcall (formatter "~&char-mode= ~S char-mode-counter= ~D") s
                       (char-mode xp) (char-mode-counter xp)))
	(pxp.stack:show-section-start xp s)
        (funcall (formatter "~&linel= ~D") s (linel xp))
	(pxp.buffer:show-detail xp s)
	(pxp.stack:show-detail xp s)
	(pxp.queue:show xp s)
	(pxp.stack:show-ptr xp s))))
  (values))

;;;; PREDICATE

(defun xp-structure-p (arg)
  (typep arg 'xp-structure))

;;;; SLOT HELPERS

(declaim (ftype (function (xp-structure character)
			  (values character &optional))
		handle-char-mode))
(defun handle-char-mode (xp char)
  #+sbcl ; due to type uncertainty about character or base-char.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (ecase (char-mode xp)
    (:CAP0 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :DOWN) (char-upcase char))))
    (:CAP1 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :CAPW) (char-upcase char))))
    (:CAPW (cond ((alphanumericp char) (char-downcase char))
		 (T (setf (char-mode xp) :CAP1) char)))
    (:UP (char-upcase char))
    (:DOWN (char-downcase char))))

(declaim (ftype (function (xp-structure char-mode) (values &optional))
		push-char-mode))
(defun push-char-mode (xp new-mode)
  (when (zerop (char-mode-counter xp))
    (setf (char-mode xp) new-mode))
  (incf (char-mode-counter xp))
  (values))

(declaim (ftype (function (xp-structure) (values &optional)) pop-char-mode))
(defun pop-char-mode (xp)
  (decf (char-mode-counter xp))
  (when (zerop (char-mode-counter xp))
    (setf (char-mode xp) nil))
  (values))

(declaim (ftype (function (xp-structure (or null string)
					boolean
					(or null string))
			  (values &optional))
		start-block))
(defun start-block (xp prefix-string on-each-line? suffix-string
		       &aux (prefix-string prefix-string)) ; Rebinding to muffle sbcl compiler.
  (when prefix-string (write-string++ prefix-string xp 0 (length prefix-string)))
  (when (and (char-mode xp) on-each-line?)
    (setq prefix-string
	  (pxp.buffer:prefix xp :rewind
			     (locally
			       #+sbcl ; due to type uncertainty.
			       (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
			       (length prefix-string)))))
  (pxp.stack:push-block-stack xp)
  (pxp.queue:enqueue xp :start-block nil
		     :arg (if on-each-line? (cons suffix-string prefix-string) suffix-string)
		     :position (pxp.buffer:total-position<-buffer-position xp)
		     :depth (pxp.stack:depth-in-blocks xp))
  (incf (pxp.stack:depth-in-blocks xp))	      ;must be after enqueue
  (setf (pxp.stack:section-start xp) (pxp.buffer:total-position<-buffer-position xp))
  (values))

(declaim (ftype (function (xp-structure (or null string)) (values &optional)) end-block))
(defun end-block (xp suffix)
  (unless (eq *abbreviation-happened* '*print-lines*)
    (when suffix (write-string+ suffix xp 0 (length suffix)))
    (decf (pxp.stack:depth-in-blocks xp))
    (pxp.queue:enqueue xp :end-block nil
		       :arg suffix
		       :position (pxp.buffer:total-position<-buffer-position xp)
		       :depth (pxp.stack:depth-in-blocks xp)
		       :sync :offset)
    (pxp.stack:pop-block-stack xp))
  (values))

(defmacro with-block ((xp prefix-string on-each-line? suffix-string) &body body)
  (let ((?xp (gensym "XP"))
	(?suffix-string (gensym "SUFFIX-STRING")))
    `(let ((,?xp ,xp)
	   (,?suffix-string ,suffix-string))
       (start-block ,?xp ,prefix-string ,on-each-line? ,?suffix-string)
       (unwind-protect (progn ,@body)
	 (end-block ,?xp ,?suffix-string)))))

(defmacro line-position<-total-position (xp ptr)
  `(pxp.buffer:line-position<-buffer-position ,xp (pxp.buffer:buffer-position<-total-position ,xp ,ptr)))

;;;; WRITERS suffixed +++
;; These functions are never doing output, just extending the buffer.

(declaim (ftype (function (string xp-structure
			  (mod #.array-total-size-limit)
			  (mod #.array-total-size-limit))
		  (values &optional))
	write-string+++))
; never forces output; therefore safe to call from within output-line.
(defun write-string+++ (string xp start end)
  (pxp.buffer:add-string string xp :start start :end end
			 :mode (if (char-mode xp)
				   (lambda (char) (handle-char-mode xp char))
				   #'identity))
  (values))

;;;; WRITERS suffixed ++.
;; Theese functions does FORCE-SOME-OUTPUT if needed.

(declaim (ftype (function (character xp-structure) (values &optional)) write-char++))
;note this checks (> BUFFER-PTR LINEL) instead of (> (line-position<-buffer-position) LINEL)
;this is important so that when things are longer than a line they
;end up getting printed in chunks of size LINEL.
(defun write-char++ (char xp &aux (char char)) ; To muffle sbcl compiler.
  (when (> (pxp.buffer:buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (pxp.buffer:add-char (if (char-mode xp)
			   (handle-char-mode xp char)
			   char)
		       xp)
  (values))

(declaim (ftype (function (string xp-structure
			  (mod #.array-total-size-limit)
			  (mod #.array-total-size-limit))
		  (values &optional))
	write-string++))
(defun write-string++ (string xp start end)
  (when (> (pxp.buffer:buffer-ptr xp) (linel xp))
    (force-some-output xp))
  (write-string+++ string xp start end))

;;;; WRITERS suffixed +.
;; These functions expects the STREAM is XP-STRUCTURE.

(declaim (ftype (function ((or pxp.queue:newline-kind (member :fresh :unconditional))
			   xp-structure)
			  (values &optional))
		pprint-newline+))
(defun pprint-newline+ (kind xp)
  (pxp.queue:enqueue xp :newline kind
		     :position (pxp.buffer:total-position<-buffer-position xp)
		     :depth (pxp.stack:depth-in-blocks xp)
		     :sync :depth)
  (setf (pxp.stack:section-start xp) (pxp.buffer:total-position<-buffer-position xp))
  (when (and (member kind '(:fresh :unconditional)) (char-mode xp))
    (handle-char-mode xp #\newline))
  (when (member kind '(:fresh :unconditional :mandatory))
    (attempt-to-output xp T nil))
  (values))

(declaim (ftype (function (character xp-structure) (values &optional))
	write-char+))
(defun write-char+ (char xp)
  (if (eql char #\newline)
      (pprint-newline+ :unconditional xp)
      (write-char++ char xp))
  (values))

(declaim (ftype (function (string xp-structure (mod #.array-total-size-limit)
			  (mod #.array-total-size-limit))
		  (values null &optional))
	write-string+))
(defun write-string+ (string xp start end)
  (loop :for s = start :then (1+ sub-end)
	:for next-newline = (position #\newline string :test #'char= :start s :end end)
	:for sub-end = (or next-newline end)
	:do (write-string++ string xp s sub-end)
	    (when (null next-newline)
	      (loop-finish))
	    (pprint-newline+ :unconditional xp)))

(deftype tab-kind ()
'(member :section :line-relative :section-relative :line))

(declaim (ftype (function (tab-kind
			    (unsigned-byte 32)
			    (unsigned-byte 32)
			    xp-structure)
			  (values &optional))
	pprint-tab+))
(defun pprint-tab+ (kind colnum colinc xp)
  (let ((indented? nil) (relative? nil))
    (case kind
      (:section (setq indented? T))
      (:line-relative (setq relative? T))
      (:section-relative (setq indented? T relative? T)))
    (let* ((current
	     (the pxp.adjustable-vector:index
		  (if (not indented?)
		    (pxp.buffer:line-position<-buffer-position xp)
		    (- (pxp.buffer:total-position<-buffer-position xp)
		       (pxp.stack:section-start xp)))))
	   (new
	     (the pxp.adjustable-vector:index
		  (cond
		    ((zerop colinc)
		     (if relative? (+ current colnum) (max colnum current)))
		    (relative?
		      (* colinc (floor (the (unsigned-byte 32) (+ current colnum colinc -1))
				       colinc)))
		    ((> colnum current) colnum)
		    (T (+ colnum
			  (* colinc
			     (floor (the (unsigned-byte 32) (+ current (- colnum) colinc))
				    colinc)))))))
	   (length (- new current)))
      (when (plusp length)
	(when (char-mode xp) (handle-char-mode xp #\space))
	(pxp.buffer:skip-to length xp))))
  (values))

(declaim (ftype (function (pxp.queue:indent-kind fixnum xp-structure) (values &optional))
		pprint-indent+))
(defun pprint-indent+ (kind n xp)
  (pxp.queue:enqueue xp :ind kind
		     :arg n
		     :position (pxp.buffer:total-position<-buffer-position xp)
		     :depth (pxp.stack:depth-in-blocks xp)))

;;;; ATTEMPT-TO-OUTPUT

(declaim (ftype (function (xp-structure pxp.queue:Qindex)
			  (values &optional)) output-line))
(defun output-line (xp Qentry)
  (let* ((out-point (pxp.buffer:buffer-position<-total-position xp (pxp.queue:Qpos xp Qentry)))
	 (last-non-blank (pxp.buffer:last-non-blank xp :end out-point))
	 (end (cond ((pxp.queue:fresh-newline-p xp Qentry) out-point)
		    (last-non-blank (1+ last-non-blank))
		    (T 0)))
	 (line-limit-exit (and (line-limit xp) (not (> (the pxp.adjustable-vector:index (line-limit xp))
						       (line-no xp))))))
    (when line-limit-exit
      (setf (pxp.buffer:buffer-ptr xp) end)          ;truncate pending output.
      (write-string+++ " .." xp 0 3)
      (pxp.stack:set-reverse-suffix xp)
      (write-string+++ (pxp.stack:suffix xp) xp 0 (pxp.stack:suffix-ptr xp))
      (pxp.queue:flush xp)
      (setq *abbreviation-happened* '*print-lines*)
      (throw 'line-limit-abbreviation-exit T))
    (incf (line-no xp))
    (unless *locating-circularities*
      (pxp.buffer:write xp (base-stream xp) :end end)
      (cl:terpri (base-stream xp))))
  (values))

(declaim (ftype (function (xp-structure pxp.queue:Qindex) (values &optional)) setup-for-next-line))
(defun setup-for-next-line (xp Qentry)
  (let* ((out-point (pxp.buffer:buffer-position<-total-position xp (pxp.queue:Qpos xp Qentry)))
	 (prefix-end
	   (cond ((pxp.queue:fresh-newline-p xp Qentry)
		  (pxp.stack:non-blank-prefix-ptr xp))
		 (T (pxp.stack:prefix-ptr xp)))))
    (pxp.buffer:shift xp
		      :prefix (pxp.stack:prefix xp)
		      :prefix-end prefix-end
		      :out-point out-point)
    (when (not (pxp.queue:fresh-newline-p xp Qentry))
      (setf (pxp.stack:section-start-line xp) (line-no xp))))
  (values))

(declaim (ftype (function (xp-structure) (values &optional)) flush))
(defun flush (xp)
  (unless *locating-circularities*
    (pxp.buffer:write xp (base-stream xp)))
  (pxp.buffer:flush xp)
  (values))

(declaim (ftype (function (xp-structure boolean boolean)
			  (values &optional))
		attempt-to-output))
(defun attempt-to-output (xp force-newlines? flush-out?)
  (macrolet ((maybe-too-large (xp Qentry)
              `(let ((limit (- (linel ,xp)
			       (if (eql (line-limit ,xp) (line-no ,xp))
				   (+ 2 (if (minusp (pxp.stack:prefix-stack-ptr ,xp))
					  0
					  (pxp.stack:suffix-ptr ,xp)))
				   0))))
		 (declare (pxp.adjustable-vector:index limit))
		 (cond (Qend
			(> (line-position<-total-position ,xp (pxp.queue:Qpos ,xp (+ ,Qentry Qend)))
			   limit))
		       ((or force-newlines? (pxp.buffer:too-large-p xp :max limit)) T)
		       (T (return nil))))) ; wait until later to decide.
	     (misering? (xp)
               `(and *print-miser-width*
		     (<= (- (linel ,xp) (pxp.stack:initial-prefix-ptr ,xp)) *print-miser-width*))))
    (pxp.queue:for-each ((ptr Qtype Qkind Qarg Qpos Qoffset Qend) xp :consume t)
      (case Qtype
	(:ind
	  (unless (misering? xp)
	    (pxp.stack:set-indentation-prefix xp
	      (case Qkind
		(:block (the fixnum (+ (pxp.stack:initial-prefix-ptr xp) (the fixnum Qarg))))
		(T ; :current
		  (the fixnum (+ (line-position<-total-position xp Qpos)
				 (the fixnum Qarg))))))))
	(:start-block
	  (cond ((maybe-too-large xp ptr)
		 (pxp.stack:push-prefix-stack xp)
		 (setf (pxp.stack:initial-prefix-ptr xp) (pxp.stack:prefix-ptr xp))
		 (pxp.stack:set-indentation-prefix xp (line-position<-total-position xp Qpos))
		 (let ((arg Qarg))
		   (when (consp arg) (pxp.stack:set-prefix xp (cdr arg)))
		   (setf (pxp.stack:initial-prefix-ptr xp) (pxp.stack:prefix-ptr xp))
		   (cond ((not (listp arg)) (pxp.stack:set-suffix xp arg))
			 ((car arg) (pxp.stack:set-suffix xp (car arg)))))
		 (setf (pxp.stack:section-start-line xp) (line-no xp)))
		(T (incf (the pxp.adjustable-vector:index (pxp.queue:Qleft xp)) Qoffset))))
	(:end-block (pxp.stack:pop-prefix-stack xp) )
	(T ; :newline
	  (when (case Qkind
		  (:fresh (not (pxp.buffer:left-most-p xp)))
		  (:miser (misering? xp))
		  (:fill (or (misering? xp)
			     (> (line-no xp) (pxp.stack:section-start-line xp))
			     (maybe-too-large xp ptr)))
		  (T T)) ;(:linear :unconditional :mandatory)
	    (output-line xp ptr)
	    (setup-for-next-line xp ptr)))))
	    ;;;;
    (when flush-out? (flush xp))
    (values)))

(declaim (ftype (function (xp-structure) (values &optional)) force-some-output))
(defun force-some-output (xp)
  (attempt-to-output xp nil nil)
  (when (> (pxp.buffer:buffer-ptr xp) (linel xp)) ;only if printing off end of line
    (attempt-to-output xp T T))
  (values))

(declaim (ftype (function (xp-structure fixnum) (values &optional)) print-fixnum))
(defun print-fixnum (xp fixnum)
  (multiple-value-bind (digits d)
      (truncate fixnum 10)
    (unless (zerop digits)
      (print-fixnum xp digits))
    (write-char++ (code-char (+ #.(char-code #\0) d)) xp)))

;;;; MAYBE-PRINT-FAST

(declaim (ftype (function (string) (values boolean &optional)) no-escapes-needed))
(defun no-escapes-needed (s)
  (let ((n (length s)))
    (and (not (zerop n))
	 (let ((c (schar s 0)))
	   (or (and (alpha-char-p c) (upper-case-p c)) (find c "*<>")))
	 (loop :for i :upfrom 1 :below n
	       :for c = (schar s i)
	       :unless (not (or (digit-char-p c)
				(and (alpha-char-p c) (upper-case-p c))
				(find c "*+<>-")))
	         :return nil
	       :finally (return t)))))

(defun maybe-print-fast (xp object)
  (cond ((stringp object)
	 (cond ((null *print-escape*) (write-string+ object xp 0 (length object)) T)
	       ((every #'(lambda (c) (not (or (char= c #\") (char= c #\\))))
		       object)
		(write-char++ #\" xp)
		(write-string+ object xp 0 (length object))
		(write-char++ #\" xp) T)))
	((typep object 'fixnum)
	 (when (and (null *print-radix*) (= *print-base* 10.))
	   (when (minusp object)
	     (write-char++ #\- xp)
	     (setq object (- object)))
	   (print-fixnum xp object) T))
	((symbolp object)
	 (let ((s (symbol-name object))
	       (is-key (keywordp object))
	       (mode (case *print-case*
		       (:downcase :down)
		       (:capitalize :cap1)
		       (T nil)))) ;note no-escapes-needed requires all caps
	   (cond ((and (or is-key (eq (symbol-package object) *package*)
			   (eq object (find-symbol s)))
		       (no-escapes-needed s))
		  (when (and is-key *print-escape*)
		    (write-char++ #\: xp))
		  (when mode (push-char-mode xp mode))
		  (write-string++ s xp 0 (length s))
		  (when mode (pop-char-mode xp)) T))))))

;;;; CIRCULARITY-PROCESS

(declaim (type (or null hash-table) *circularity-hash-table*))
(defvar *circularity-hash-table* nil
  "Contains hash table used for locating circularities, or a stack.")

(declaim (type list *parents*))
(defvar *parents* nil "used when *print-shared* is nil")

(declaim (ftype (function (xp-structure t boolean)
			  (values (member nil :subsequent :first) &optional))
		circularity-process))
(defun circularity-process (xp object interior-cdr?)
  "Determine circularity process type.
  Returns
  NIL - Printing should just continue on.
  :FIRST - Object is first occurrence of a DUPLICATE.
  :SUBSEQUENT - Second or later occurrence."
  (unless (or (numberp object)
	      (characterp object)
	      (and (symbolp object)	;Reader takes care of sharing.
		   (or (null *print-gensym*) (symbol-package object))))
    (let ((id (gethash object *circularity-hash-table*)))
      (declare ((or null fixnum) id))
      (if *locating-circularities*
	  (cond ((null id)	;never seen before
		 (when *parents* (push object *parents*))
		 (setf (gethash object *circularity-hash-table*) 0)
		 nil)
		((zerop id) ;possible second occurrence
		 (cond ((or (null *parents*) (member object *parents*))
			(setf (gethash object *circularity-hash-table*)
			      (incf *locating-circularities*))
			:subsequent)
		       (T nil)))
		(T :subsequent));third or later occurrence
	  (cond ((or (null id)	;never seen before (note ~@* etc. conses)
		     (zerop id));no duplicates
		 nil)
		((plusp id)
		 (cond (interior-cdr?
			(decf pxp.dispatch:*current-level*)
			(write-string++ ". #" xp 0 3))
		       (T (write-char++ #\# xp)))
		 ;;;;
		 (print-fixnum xp id)
		 (write-char++ #\= xp)
		 (setf (gethash object *circularity-hash-table*) (- id))
		 :first)
		(T (if interior-cdr?
		       (write-string++ ". #" xp 0 3)
		       (write-char++ #\# xp))
		   (print-fixnum xp (- id))
		   (write-char++ #\# xp)
		   :subsequent))))))

;;;; XP PRINTINGS

(pxp.pool:defpool xp
  :constructor (lambda (&rest args)
		 (apply #'make-instance 'xp-structure args))
  :resetter (lambda (instance &rest args)
	      (apply #'reinitialize-instance instance args))
  :destructor (lambda (instance) (setf (base-stream instance) nil)))

(declaim (ftype (function (function stream list) (values t &optional)) do-xp-printing))
(defun do-xp-printing (fn stream args)
  (with-pooled-xp (xp :stream stream)
    (let ((pxp.dispatch:*current-level* 0)
          (result nil))
      (catch 'line-limit-abbreviation-exit
        (with-block (xp nil nil nil)
          (setq result (apply fn xp args))))
      (when (and *locating-circularities*
                 (zerop *locating-circularities*)	;No circularities.
                 (= (line-no xp) 1)	     	;Didn't suppress line.
                 (zerop (pxp.buffer:buffer-offset xp)))	;Didn't suppress partial line.
        (setq *locating-circularities* nil))	;print what you have got.
      (when (catch 'line-limit-abbreviation-exit
              (attempt-to-output xp nil T) nil)
        (attempt-to-output xp T T))
      result)))

(deftype stream-designator ()
  '(or boolean stream))

(declaim (ftype (function (stream-designator) (values stream &optional)) decode-stream-arg))
(defun decode-stream-arg (stream)
  "Decode stream-designator to apropreate stream."
  (etypecase stream
    ((eql t) *terminal-io*)
    (null *standard-output*)
    (stream stream)))

(defun xp-print (fn stream args)
  (setq *result* (do-xp-printing fn stream args))
  (when *locating-circularities*
    (setq *locating-circularities* nil)
    (setq *abbreviation-happened* nil)
    (setq *parents* nil)
    (setq *result* (do-xp-printing fn stream args))))

(pxp.pool:defpool circularity-hash-table
  :constructor (lambda (&rest args)
		 (and *print-circle*
		      (apply #'make-hash-table :test #'eq args)))
  :destructor (lambda (table) (and table (clrhash table))))

(defvar *result* nil "used to pass back a value")

(declaim (ftype (function (function stream &rest t) (values t &optional)) call-with-xp-stream))
(defun call-with-xp-stream (fn stream &rest args)
  (if (xp-structure-p stream)
      (apply fn stream args)
      (with-pooled-circularity-hash-table (*circularity-hash-table*)
        (let ((*abbreviation-happened* nil)
              (*locating-circularities* (if *print-circle* 0 nil))
              (*parents* (when (not *print-shared*) (list nil)))
              (*result* nil))
          (xp-print fn (decode-stream-arg stream) args)
          (when *abbreviation-happened*
            (setq *last-abbreviated-printing*
                  (let ((list (copy-list args))) ; to avoid inner looping.
                    (lambda (&optional (stream stream))
                      (let ((*package* *package*))
                        (apply #'call-with-xp-stream
                               fn stream list))))))
          *result*))))

;;;; STREAMS

(defmethod trivial-gray-streams:stream-line-column ((output xp-structure)) nil)

(defmethod trivial-gray-streams:stream-write-char ((output xp-structure) char)
  (write-char+ char output)
  char)

(defmethod trivial-gray-streams:stream-finish-output ((output xp-structure))
  (attempt-to-output output T T)
  nil)

(defmethod trivial-gray-streams:stream-force-output ((output xp-structure))
  (attempt-to-output output T T)
  nil)

(defmethod trivial-gray-streams:stream-clear-output ((output xp-structure))
  (let ((*locating-circularities* 0)) ;hack to prevent visible output
    (attempt-to-output output T T))
  nil)

