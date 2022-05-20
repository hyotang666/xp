;-*-syntax:COMMON-LISP;Package:(XP :use "COMMON-LISP" :colon-mode :external)-*-

(in-package :cl-user)

;This is the November, 26 1991 version of
;Richard C. Waters' XP pretty printer.

;The standard version of this program is available by anonymous FTP
;from MERL.COM in the files /pub/xp/xp*.  If you have gotten the file
;from somewhere else, or copied the files a long time ago, you might
;consider copying them from MERL.COM now to obtain the latest version.

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------

;The functions in this file are documented in Chapter 27 of Common Lisp:
;the Language Second Edition, Guy L. Steele Jr, Digital press, 1990,
;and in even greater detail in
;  MIT/AIM-1102a, July 1989.
;This report can be obtained by writing to

;              Publications
;	       MIT AI Laboratory
;	       545 Tech. Sq.
;	       Cambridge MA 02139

;This file attempts to be as compatible with pure Common Lisp as possible.
;It has been tested on the following Common Lisps to date.

;The companion file "XPTEST.LISP" contains a set of 600+ tests.  You should
;run these tests after the first time you compile this file on a new system.

;The companion file "XPDOC.TXT" contains brief documentation.

#.(macrolet ((reexport (from &rest syms)
	       `'((:shadowing-import-from ,from ,@syms)
		  (:export ,from ,@syms))))
    `(defpackage :pxp (:use :cl)
       (:shadow write print prin1 princ pprint format write-to-string princ-to-string
		prin1-to-string write-line write-string fresh-line
		defstruct)
       ,@(reexport :pxp.dispatch copy-pprint-dispatch pprint-dispatch
		   set-pprint-dispatch *print-pprint-dispatch*)
       (:shadow formatter pprint-fill pprint-linear pprint-tabular
		pprint-logical-block pprint-pop pprint-exit-if-list-exhausted
		pprint-newline pprint-indent pprint-tab
		*print-right-margin* *print-miser-width* *print-lines*)
       (:export formatter pprint-fill pprint-linear pprint-tabular
		pprint-logical-block pprint-pop pprint-exit-if-list-exhausted
		pprint-newline pprint-indent pprint-tab
		*print-right-margin* #:*default-right-margin*
		*print-miser-width* *print-lines*
		#:*last-abbreviated-printing*
		*print-shared*)))

(in-package :pxp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Defpackage xp-user needs this eval-when.
(defvar *xp-printing-functions*
	'(write print prin1 princ pprint format write-to-string princ-to-string
	  prin1-to-string write-line write-string fresh-line
	  defstruct)
  "printing functions redefined by xp."))

#.(let ((pxp-symbols (append *xp-printing-functions*
			     (loop :for s :being :each :external-symbol :of :pxp
				   :collect s))))
    `(defpackage :xp-user (:use)
       (:import-from :pxp ,@pxp-symbols)
       (:import-from :cl ,@(loop :for s :being :each :external-symbol :of :cl
				 :unless (member s pxp-symbols :test #'string=)
				 :collect s))
       (:export
	 ,@(union
	     (loop :for s :being :each :external-symbol :of :cl
			:collect s)
	     (loop :for s :being :each :external-symbol :of :pxp
		   :collect s)))))

;must do the following in common lisps not supporting *print-shared*

(defvar *print-shared* nil)

(defvar *print-right-margin* nil
  "+#/nil the right margin for pretty printing")
(defvar *print-miser-width* 40.
  "+#/nil miser format starts when there is less than this width left")
(defvar *print-lines* nil
  "+#/nil truncates printing after # lines")
(defvar *default-right-margin* 70.
  "controls default line length; must be a non-negative integer")
(defvar *last-abbreviated-printing*
	#'(lambda (&optional stream) (declare (ignore stream)) nil)
  "funcalling this redoes the last xp printing that was abbreviated.")

(defvar *current-length* 0
  "current position in logical block.")
(defvar *abbreviation-happened* nil
  "t if current thing being printed has been abbreviated.")
(defvar *result* nil "used to pass back a value")

;default (bad) definitions for the non-portable functions

#-(or :franz-inc)(eval-when (:execute :load-toplevel :compile-toplevel)
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil))


;Definitions for FRANZ Common Lisp. (Only verified for the version 1.3
;(5/31/87) currently running on suns at MIT.)

#+:franz-inc(eval-when (:execute :load-toplevel :compile-toplevel)
(defun output-width     (&optional (s *standard-output*)) (declare (ignore s)) nil))


(declaim (type (or null integer) *locating-circularities*))
(defvar *locating-circularities* nil
  "Integer if making a first pass over things to identify circularities.
   Integer used as counter for #n= syntax.")
(declaim (type list *parents*))
(defvar *parents* nil "used when *print-shared* is nil")

(declaim (type (or null hash-table) *circularity-hash-table*))
(defvar *circularity-hash-table* nil
  "Contains hash table used for locating circularities, or a stack.")
;When an entry is first made it is zero.
;If a duplicate is found, a positive integer tag is assigned.
;After the first time the object is printed out, the tag is negated.

(pxp.pool:defpool circularity-hash-table
  :constructor (lambda (&rest args)
		 (and *print-circle*
		      (apply #'make-hash-table :test #'eq args)))
  :destructor (lambda (table) (and table (clrhash table))))

;               ---- XP STRUCTURES, AND THE INTERNAL ALGORITHM ----

(deftype char-mode ()
  '(member nil :up :down :cap0 :cap1 :capw))

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

(defmethod shared-initialize :after ((o xp-structure) slot-names &key stream)
  (setf (base-stream o) stream
        (linel o) (max 0 (cond (*print-right-margin*)
                                ((output-width stream))
                                (T *default-right-margin*)))
        (line-limit o) *print-lines*
        (line-no o) 1
        (char-mode o) nil
        (char-mode-counter o) 0))

(defun xp-structure-p (arg)
  (typep arg 'xp-structure))

;This does not tell you the line position you were at when the total-position
;was set, unless there have been no newlines or indentation output
;between ptr and the current output point.
(defmacro line-position<-total-position (xp ptr)
  `(pxp.buffer:line-position<-buffer-position ,xp (pxp.buffer:buffer-position<-total-position ,xp ,ptr)))

;we shift the queue over rather than using a circular queue because
;that works out to be a lot faster in practice.  Note, short printout
;does not ever cause a shift, and even in long printout, the queue is
;shifted left for free every time it happens to empty out.


(declaim (type boolean *describe-xp-streams-fully*))
(defvar *describe-xp-streams-fully* nil "Set to T to see more info.")

(defmethod print-object ((xp xp-structure) s)
  (print-unreadable-object (xp s :type t :identity nil)
    (cl:format s "stream ")
    (if (not (base-stream xp))
        (cl:format s "not currently in use")
        (cl:format s "outputting to ~S" (base-stream xp)))
    (when (base-stream xp)
      (pxp.buffer:show xp s)
      (when (not *describe-xp-streams-fully*) (cl:princ " ..." s))
      (when *describe-xp-streams-fully*
        (cl:format s "~&   pos   _123456789_123456789_123456789_123456789")
        (cl:format s "~&depth-in-blocks= ~D linel= ~D line-no= ~D line-limit= ~D"
                     (pxp.stack:depth-in-blocks xp) (linel xp) (line-no xp) (line-limit xp))
        (when (or (char-mode xp) (not (zerop (char-mode-counter xp))))
          (cl:format s "~&char-mode= ~S char-mode-counter= ~D"
                       (char-mode xp) (char-mode-counter xp)))
	(pxp.stack:show-section-start xp s)
        (cl:format s "~&linel= ~D" (linel xp))
	(pxp.buffer:show-detail xp s)
	(pxp.stack:show-detail xp s)
	(pxp.queue:show xp s)
	(pxp.stack:show-ptr xp s))))
  (values))

;This maintains a list of XP structures.  We save them
;so that we don't have to create new ones all of the time.
;We have separate objects so that many can be in use at once.

;(Note should really be doing some locking here, but CL does not have the
;primitives for it.  There is a tiny probability here that two different
;processes could end up trying to use the same xp-stream)

(pxp.pool:defpool xp
  :constructor (lambda (&rest args)
		 (apply #'make-instance 'xp-structure args))
  :resetter (lambda (instance &rest args)
	      (apply #'reinitialize-instance instance args))
  :destructor (lambda (instance) (setf (base-stream instance) nil)))

;The char-mode stuff is a bit tricky.
;one can be in one of the following modes:
;NIL no changes to characters output.
;:UP CHAR-UPCASE used.
;:DOWN CHAR-DOWNCASE used.
;:CAP0 capitalize next alphanumeric letter then switch to :DOWN.
;:CAP1 capitalize next alphanumeric letter then switch to :CAPW
;:CAPW downcase letters.  When a word break letter found, switch to :CAP1.
;It is possible for ~(~) to be nested in a format string, but note that
;each mode specifies what should happen to every letter.  Therefore, inner
;nested modes never have any effect.  You can just ignore them.


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

;Assumes is only called when char-mode is non-nil

(declaim (ftype (function (xp-structure character)
			  (values character &optional))
		handle-char-mode))
(defun handle-char-mode (xp char)
  (ecase (char-mode xp)
    (:CAP0 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :DOWN) (char-upcase char))))
    (:CAP1 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :CAPW) (char-upcase char))))
    (:CAPW (cond ((alphanumericp char) (char-downcase char))
		 (T (setf (char-mode xp) :CAP1) char)))
    (:UP (char-upcase char))
    (:DOWN (char-downcase char))))

;All characters output are passed through the handler above.  However, it must
;be noted that on-each-line prefixes are only processed in the context of the
;first place they appear.  They stay the same later no matter what.  Also
;non-literal newlines do not count as word breaks.



;;;  The next function scans the queue looking for things it can do.
;;; it keeps outputting things until the queue is empty, or it finds
;;; a place where it cannot make a decision yet.

;;; If flush-out? is T and force-newlines? is NIL then the buffer,
;;; prefix-stack, and queue will be in an inconsistent state after the call.
;;; You better not call it this way except as the last act of outputting.


(declaim (ftype (function (xp-structure boolean boolean)
			  (values &optional))
		attempt-to-output))
(defun attempt-to-output (xp force-newlines? flush-out?)
  (macrolet ((maybe-too-large (xp Qentry)
              `(let ((limit (linel ,xp)))
		 (when (eql (line-limit ,xp) (line-no ,xp)) ;prevents suffix overflow
		   (decf limit 2) ;3 for " .." minus 1 for space (heuristic)
		   (when (not (minusp (pxp.stack:prefix-stack-ptr ,xp)))
		     (decf limit (pxp.stack:suffix-ptr ,xp))))
		 (cond (Qend
			(> (line-position<-total-position ,xp (pxp.queue:Qpos ,xp (+ ,Qentry Qend))) limit))
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
		(:block (+ (pxp.stack:initial-prefix-ptr xp) Qarg))
		(T ; :current
		  (+ (line-position<-total-position xp Qpos)
		     Qarg))))))
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
		(T (incf (pxp.queue:Qleft xp) Qoffset))))
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
    (when flush-out? (flush xp))
    (values)))


(declaim (ftype (function (xp-structure) (values &optional)) force-some-output))
(defun force-some-output (xp)
  (attempt-to-output xp nil nil)
  (when (> (pxp.buffer:buffer-ptr xp) (linel xp)) ;only if printing off end of line
    (attempt-to-output xp T T))
  (values))

;This handles the basic outputting of characters.  note + suffix means that
;the stream is known to be an XP stream, all inputs are mandatory, and no
;error checking has to be done.
;;;; +++ suffix functions.

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

;;;; ++ suffix functions.
;;  Suffix ++ additionally means that the output is guaranteed not to contain a newline char.

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

;;;; + suffix functions.

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
		   (integer 0 *)
		   (integer 0 *)
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
	     (if (not indented?)
	         (pxp.buffer:line-position<-buffer-position xp)
	         (- (pxp.buffer:total-position<-buffer-position xp) (pxp.stack:section-start xp))))
	   (new
	     (if (zerop colinc)
	         (if relative? (+ current colnum) (max colnum current))
	         (cond (relative?
			 (* colinc (floor (+ current colnum colinc -1) colinc)))
		       ((> colnum current) colnum)
		       (T (+ colnum
			     (* colinc
				(floor (+ current (- colnum) colinc) colinc)))))))
	   (length (- new current)))
      (when (plusp length)
	(when (char-mode xp) (handle-char-mode xp #\space))
	(pxp.buffer:skip-to length xp))))
  (values))

;note following is smallest number >= x that is a multiple of colinc
;  (* colinc (floor (+ x (1- colinc)) colinc))

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

(declaim (ftype (function (pxp.queue:indent-kind fixnum xp-structure) (values &optional))
		pprint-indent+))
(defun pprint-indent+ (kind n xp)
  (pxp.queue:enqueue xp :ind kind
		     :arg n
		     :position (pxp.buffer:total-position<-buffer-position xp)
		     :depth (pxp.stack:depth-in-blocks xp)))

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
	  (pxp.buffer:prefix xp :rewind (length prefix-string))))
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

;this can only be called last!


(declaim (ftype (function (xp-structure) (values &optional)) flush))
(defun flush (xp)
  (unless *locating-circularities*
    (pxp.buffer:write xp (base-stream xp)))
  (pxp.buffer:flush xp)
  (values))

;This prints out a line of stuff.


(declaim (ftype (function (xp-structure pxp.queue:Qindex)
			  (values &optional)) output-line))
(defun output-line (xp Qentry)
  (let* ((out-point (pxp.buffer:buffer-position<-total-position xp (pxp.queue:Qpos xp Qentry)))
	 (last-non-blank (pxp.buffer:last-non-blank xp :end out-point))
	 (end (cond ((pxp.queue:fresh-newline-p xp Qentry) out-point)
		    (last-non-blank (1+ last-non-blank))
		    (T 0)))
	 (line-limit-exit (and (line-limit xp) (not (> (line-limit xp) (line-no xp))))))
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

;		   ---- BASIC INTERFACE FUNCTIONS ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking of fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

;;; Circularity process

;;; It is vital that this function be called EXACTLY once for each occurrence of
;;;   each thing in something being printed.
;;; Returns nil if printing should just continue on.
;;;   Either it is not a duplicate, or we are in the first pass and do not know.
;;; returns :FIRST if object is first occurrence of a DUPLICATE.
;;;   (This can only be returned on a second pass.)
;;;   After an initial code (printed by this routine on the second pass)
;;;   printing should continue on for the object.
;;; returns :SUBSEQUENT if second or later occurrence.
;;;   Printing is all taken care of by this routine.

;;; Note many (maybe most) lisp implementations have characters and small numbers
;;; represented in a single word so that the are always eq when they are equal and the
;;; reader takes care of properly sharing them (just as it does with symbols).
;;; Therefore, we do not want circularity processing applied to them.  However,
;;; some kinds of numbers (e.g., bignums) undoubtedly are complex structures that
;;; the reader does not share.  However, they cannot have circular pointers in them
;;; and it is therefore probably a waste to do circularity checking on them.  In
;;; any case, it is not clear that it easy to tell exactly what kinds of numbers a
;;; given implementation of CL is going to have the reader automatically share.


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

(deftype stream-designator ()
  '(or boolean stream))

(declaim (ftype (function (stream-designator) (values stream &optional)) decode-stream-arg))
(defun decode-stream-arg (stream)
  "Decode stream-designator to apropreate stream."
  (etypecase stream
    ((eql t) *terminal-io*)
    (null *standard-output*)
    (stream stream)))

(defun write+ (object xp)
  (let ((*parents* *parents*))
    (unless (and *circularity-hash-table*
		(eq (circularity-process xp object nil) :subsequent))
      (when (and *circularity-hash-table* (consp object))
	;;avoid possible double check in handle-logical-block.
	(setq object (cons (car object) (cdr object))))
      (let ((printer (if *print-pretty*
		         (pxp.dispatch:get-printer object pxp.dispatch:*print-pprint-dispatch*)
			 nil))
	    type)
	(cond (printer (funcall printer xp object))
	      ((maybe-print-fast xp object))
	      ((and *print-pretty*
		    (symbolp (setq type (type-of object)))
		    (setq printer (get type 'structure-printer))
		    (not (eq printer :none)))
	       (funcall printer xp object))
	      ((and *print-pretty* *print-array* (arrayp object)
		    (not (stringp object)) (not (bit-vector-p object))
		    (not (pxp.dispatch:structure-type-p (type-of object))))
	       (pretty-array xp object))
	      (T (let ((stuff
			 (with-output-to-string (s)
			   (pxp.dispatch:non-pretty-print object s))))
		   (write-string+ stuff xp 0 (length stuff)))))))))


(declaim (ftype (function (function stream list) (values t &optional)) do-xp-printing))
(defun do-xp-printing (fn stream args)
  (with-pooled-xp (xp :stream stream)
    (let ((pxp.dispatch:*current-level* 0)
          (result nil))
      (catch 'line-limit-abbreviation-exit
        (start-block xp nil nil nil)
        (setq result (apply fn xp args))
        (end-block xp nil))
      (when (and *locating-circularities*
                 (zerop *locating-circularities*)	;No circularities.
                 (= (line-no xp) 1)	     	;Didn't suppress line.
                 (zerop (pxp.buffer:buffer-offset xp)))	;Didn't suppress partial line.
        (setq *locating-circularities* nil))	;print what you have got.
      (when (catch 'line-limit-abbreviation-exit
              (attempt-to-output xp nil T) nil)
        (attempt-to-output xp T T))
      result)))

(defun xp-print (fn stream args)
  (setq *result* (do-xp-printing fn stream args))
  (when *locating-circularities*
    (setq *locating-circularities* nil)
    (setq *abbreviation-happened* nil)
    (setq *parents* nil)
    (setq *result* (do-xp-printing fn stream args))))


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


(declaim (ftype (function (t stream) (values t &optional)) basic-write))
(defun basic-write (object stream)
  (cond ((xp-structure-p stream) (write+ object stream))
	(*print-pretty* (call-with-xp-stream
			  #'(lambda (s o) (write+ o s)) stream object))
	(T (cl:write object :stream stream))))

(defun write (object &rest pairs &key
		     (stream *standard-output*)
		     ((:escape *print-escape*) *print-escape*)
		     ((:radix *print-radix*) *print-radix*)
		     ((:base *print-base*) *print-base*)
		     ((:circle *print-circle*) *print-circle*)
		     ((:pretty *print-pretty*) *print-pretty*)
		     ((:level *print-level*) *print-level*)
		     ((:length *print-length*) *print-length*)
		     ((:case *print-case*) *print-case*)
		     ((:gensym *print-gensym*) *print-gensym*)
		     ((:array *print-array*) *print-array*)
		     ((:pprint-dispatch pxp.dispatch:*print-pprint-dispatch*)
		      pxp.dispatch:*print-pprint-dispatch*)
		     ((:right-margin *print-right-margin*)
		      *print-right-margin*)
		     ((:lines *print-lines*) *print-lines*)
		     ((:miser-width *print-miser-width*)
		      *print-miser-width*))
  (setq stream (decode-stream-arg stream))
  (cond ((or (xp-structure-p stream) *print-pretty*)
	 (basic-write object stream))
	#+(or ccl clisp ecl)
	((and (null *print-pretty*)
	      ;; as (typep object '(cons (member quote function) (cons * null)))
	      ;; due to ecl does not support cons-type-specifier.
	      (and (consp object)
		   (member (car object) '(quote function))
		   (consp (cdr object))
		   (null (cddr object))))
	 (funcall (cl:formatter "~:<~W ~W~:>") stream object))
	(T
	  (apply #'cl:write object pairs)))
  object)

;This prints a few very common, simple atoms very fast.
;Pragmatically, this turns out to be an enormous savings over going to the
;standard printer all the time.  There would be diminishing returns from making
;this work with more things, but might be worth it.

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


(declaim (ftype (function (xp-structure fixnum) (values &optional)) print-fixnum))
(defun print-fixnum (xp fixnum)
  (multiple-value-bind (digits d)
      (truncate fixnum 10)
    (unless (zerop digits)
      (print-fixnum xp digits))
    (write-char++ (code-char (+ #.(char-code #\0) d)) xp)))

;just wants to succeed fast in a lot of common cases.
;assumes no funny readtable junk for the characters shown.


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

(defun print (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (terpri stream)
  (let ((*print-escape* T))
    (basic-write object stream))
  (write-char #\space stream)
  object)

(defun prin1 (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-escape* T))
    (basic-write object stream))
  object)

(defun princ (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (let ((*print-escape* nil))
    (basic-write object stream))
  object)

(defun pprint (object &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (terpri stream)
  (let ((*print-escape* T) (*print-pretty* T))
    (basic-write object stream))
  (values))

(defun write-to-string (object &rest pairs &key &allow-other-keys)
  (with-output-to-string (s)
    (apply #'write object :stream s pairs)))

(defun prin1-to-string (object)
  (with-output-to-string (stream)
    (let ((*print-escape* T))
      (basic-write object stream))))

(defun princ-to-string (object)
  (with-output-to-string (stream)
    (let ((*print-escape* nil))
      (basic-write object stream))))

;Any format string that is converted to a function is always printed
;via an XP stream (See formatter).


(defmethod trivial-gray-streams:stream-line-column ((output xp-structure)) nil)
(defmethod trivial-gray-streams:stream-write-char ((output xp-structure) char)
  (write-char+ char output)
  char)

(locally
  ;; To muffle &OPTIONAL and &KEY is used at once.
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
(defun write-string (string &optional (stream *standard-output*)
		     &key (start 0) (end (length string)))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (write-string+ string stream start end)
      (cl:write-string string stream :start start :end end))
  string)

(defun write-line (string &optional (stream *standard-output*)
		   &key (start 0) (end (length string)))
  (setq stream (decode-stream-arg stream))
  (if (xp-structure-p stream)
      (progn (write-string+ string stream start end)
	     (pprint-newline+ :unconditional stream))
      (cl:write-line string stream :start start :end end))
  string) )

;This has to violate the XP data abstraction and fool with internal
;stuff, in order to find out the right info to return as the result.

(defun fresh-line (&optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (cond ((xp-structure-p stream)
	 (attempt-to-output stream T T) ;ok because we want newline
	 (when (not (pxp.buffer:left-most-p stream))
	   (pprint-newline+ :fresh stream)
	   T))
	(T (and (cl:fresh-line stream) t))))

;Each of these causes the stream to be pessimistic and insert
;newlines wherever it might have to, when forcing the partial output
;out.  This is so that things will be in a consistent state if
;output continues to the stream later.

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

;note we are assuming that if a structure is defined using xp::defstruct,
;then its print-function (if any) will be defined using xp::print etc.

(defmacro defstruct (name &body body)
  (let* ((struct-name (if (consp name) (car name) name))
	 (printer (cadr (safe-assoc :print-function name)))
	 (xp-print-fn
	   (intern (concatenate 'string
		     "PRINT-" (string (package-name
					(symbol-package struct-name)))
		     ":" (string struct-name))
		   (find-package :pxp))))
    (cond (printer
	   `(eval-when (:execute :load-toplevel :compile-toplevel)
	      (cl:defstruct ,name ,@ body)
	      (defun ,xp-print-fn (xp obj)
		(funcall (function ,printer) obj xp pxp.dispatch:*current-level*))
	      (setf (get ',struct-name 'structure-printer) #',xp-print-fn)
	      ',(if (consp name) (car name) name)))
	  ((and (not (safe-assoc :type name))
		(not (safe-assoc :include name)))
	   (let* ((conc-name-spec (safe-assoc :conc-name name))
		  (conc-name (cond ((null conc-name-spec)
				    (concatenate 'string (string struct-name) "-"))
				   ((null (cadr conc-name-spec)) "")
				   (T (string (cadr conc-name-spec)))))
		  (slots (mapcar #'(lambda (x) (if (consp x) (car x) x)) body)))
	     `(eval-when (:execute :load-toplevel :compile-toplevel)
		(cl:defstruct ,name ,@ body)
		(defun ,xp-print-fn (xp obj)
		  (funcall (formatter "~@<#S(~;~W ~:I~@_~@{:~A ~W~^ ~:_~}~;)~:>") xp
			   ',struct-name
			   ,@(mapcan #'(lambda (slot)
					 `(,(string slot)
					    (,(intern (concatenate 'string
					                 conc-name (string slot)))
					      obj)))
				     slots)))
		(setf (get ',struct-name 'structure-printer) #',xp-print-fn)
		',(if (consp name) (car name) name))))
	  (T `(eval-when (:execute :load-toplevel :compile-toplevel)
		(setf (get ',struct-name 'structure-printer) :none)
		(cl:defstruct ,name ,@ body))))))

(defun safe-assoc (item list)
  (do ((l list (cdr l))) ((not (consp l)) nil)
    (when (and (consp (car l)) (eq (caar l) item))
      (return (car l)))))

;           ---- FUNCTIONAL INTERFACE TO DYNAMIC FORMATTING ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking or fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

(defmacro pprint-logical-block ((stream-symbol list
				 &key (prefix nil) (per-line-prefix nil)
				      (suffix ""))
				&body body)
  (cond ((eq stream-symbol nil) (setq stream-symbol '*standard-output*))
	((eq stream-symbol T) (setq stream-symbol '*terminal-io*)))
  (when (not (symbolp stream-symbol))
    (warn "STREAM-SYMBOL arg ~S to PPRINT-LOGICAL-BLOCK is not a bindable symbol"
	  stream-symbol)
    (setq stream-symbol '*standard-output*))
  (when (and prefix per-line-prefix)
    (warn "prefix ~S and per-line-prefix ~S cannot both be specified ~
           in PPRINT-LOGICAL-BLOCK" prefix per-line-prefix)
    (setq per-line-prefix nil))
  `(call-with-xp-stream
     #'(lambda (,stream-symbol)
	 (let ((+l ,list)
	       (+p ,(or prefix per-line-prefix ""))
	       (+s ,suffix))
	   (pprint-logical-block+
	     (,stream-symbol +l +p +s ,(not (null per-line-prefix)) T nil)
	     ,@ body nil)))
     (decode-stream-arg ,stream-symbol)))

;Assumes var and args must be variables.  Other arguments must be literals or variables.

(defmacro pprint-logical-block+ ((var args prefix suffix per-line? circle-check? atsign?)
				 &body body)
   (when (and circle-check? atsign?)
     (setq circle-check? 'not-first-p))
  `(let ((pxp.dispatch:*current-level* (1+ pxp.dispatch:*current-level*))
	 (*current-length* -1)
	 (*parents* *parents*)
	 ,@(when (and circle-check? atsign?)
	     `((not-first-p (plusp *current-length*)))))
     (unless (check-block-abbreviation ,var ,args ,circle-check?)
       (block logical-block
	 (start-block ,var ,prefix ,per-line? ,suffix)
	 (unwind-protect
	   (macrolet ((pprint-pop () `(pprint-pop+ ,',args ,',var))
		      (pprint-exit-if-list-exhausted ()
			`(when (null ,',args)
			   (return-from logical-block nil))))
	     ,@ body)
	   (end-block ,var ,suffix))))))

(defun pprint-newline (kind &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (check-type kind pxp.queue:newline-kind)
  (when (xp-structure-p stream)
    (pprint-newline+ kind stream))
  nil)

(defun pprint-indent (relative-to n &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (check-type relative-to pxp.queue:indent-kind)
  (when (xp-structure-p stream)
    (pprint-indent+ relative-to n stream))
  nil)

(defun pprint-tab (kind colnum colinc &optional (stream *standard-output*))
  (setq stream (decode-stream-arg stream))
  (check-type kind tab-kind)
  (when (xp-structure-p stream)
    (pprint-tab+ kind colnum colinc stream))
  nil)

;                        ---- COMPILED FORMAT ----

;Note that compiled format strings always print through xp streams even if
;they don't have any xp directives in them.  As a result, the compiled code
;can depend on the fact that the stream being operated on is an xp
;stream not an ordinary one.

 (eval-when (:execute :load-toplevel :compile-toplevel)

(declaim (special *string* *used-args* *used-outer-args* *used-initial*
		  *get-arg-carefully* *inner-end* *outer-end* *at-top*))

(defvar *fn-table* (make-hash-table) "used to access fns for commands")

;;;; CONDITION

(define-condition failed-to-compile (warning)
  ((id :initarg :id :reader error-id
       :documentation "Used to identify error point in the test.")
   (control-string :initarg :control-string :reader control-string)
   (error-point :initarg :error-point :reader error-point
		:documentation "The index where the error is occured.")
   (message :initarg :message :reader error-message))
  (:report (lambda (this output)
	     (funcall (cl:formatter "XP: ~A~%~S~%~V@T|")
		      output
		      (error-message this)
		      (control-string this)
		      (1+ (error-point this))))))

(defvar *xp-condition* 'failed-to-compile
  "The default condition for ERR.")

(defun failed-to-compile (id msg control-string i)
  (error (make-condition *xp-condition*
			 :id id
			 :control-string control-string
			 :message msg
			 :error-point i)))

;; MEMO: FIXME(?) Seems to not be used. Should be removed?
(defun position-in (set start)
  (position-if #'(lambda (c) (find c set)) *string* :start start))

(defun position-not-in (string set &key start)
  (position-if-not #'(lambda (c) (find c set)) string :start start))


(declaim (ftype (function (string &key (:start (mod #.array-total-size-limit)))
			  (values (mod #.array-total-size-limit) &optional))
		params-end))
(defun params-end (control-string &key start) ;start points just after ~
  "Return an end position of the format directive parameters."
  (let ((end (length control-string)))
    (labels ((rec (position)
	       (cond
	         ((null position)
		  (failed-to-compile 1 "missing directive" control-string (1- start)))
	         ((not (char= (aref control-string position) #\'))
		  position)
	         ((= (1+ position) end)
		  (failed-to-compile 2 "No character after '" control-string position))
	         (t
		   (rec (position-not-in control-string "+-0123456789,Vv#:@" :start (+ 2 position)))))))
      (rec (position-not-in control-string "+-0123456789,Vv#:@" :start start)))))


(declaim (ftype (function (string &key
				  (:start (mod #.array-total-size-limit))
				  (:end (mod #.array-total-size-limit)))
			  (values (or null (mod #.array-total-size-limit))
				  (or null (mod #.array-total-size-limit))
				  &optional))
		next-directive1))
(defun next-directive1 (control-string &key start end)
  "Return two values.
1: Start position of param i.e. tilda, or nil.
2: End position of param, or nil."
  (let ((i (position #\~ control-string :start start :end end)))
    (if (null i)
        (values nil nil)
        (let ((j (params-end control-string :start (1+ i))))
	  (if (not (char= (aref control-string j) #\/))
	      (values i j)
	      (values i (or (position #\/ control-string :start (1+ j) :end end)
			    (failed-to-compile 3 "Matching / missing"
					       control-string (position #\/ control-string :start start)))))))))

(declaim (ftype (function (string (mod #.array-total-size-limit))
			  (values boolean &optional))
		colonp))
(defun colonp (control-string j) ;j points to directive name
  (or (char= (aref control-string (1- j)) #\:)
      (and (char= (aref control-string (1- j)) #\@)
	   (char= (aref control-string (- j 2)) #\:))))

(declaim (ftype (function (string (mod #.array-total-size-limit))
			  (values boolean &optional))
		atsignp))
(defun atsignp (control-string j) ;j points to directive name
  (or (char= (aref control-string (1- j)) #\@)
      (and (char= (aref control-string (1- j)) #\:)
	   (char= (aref control-string (- j 2)) #\@))))

(declaim (ftype (function (string) (values boolean &optional)) fancy-directives-p))
(defun fancy-directives-p (control-string)
  (let ((end (length control-string)))
    (labels ((rec (i j)
               (when i
                 (let ((c (aref control-string j)))
                   (if (or (find c "_Ii/Ww")
                           (and (find c ">Tt")
                                (colonp control-string j)))
                       t
                       (multiple-value-call #'rec
                         (next-directive1 control-string :start j :end end)))))))
      (multiple-value-call #'rec (next-directive1 control-string :start 0 :end end)))))

(declaim (ftype (function (string boolean) (values (or string function) &optional))
		maybe-compile-format-string))
(defun maybe-compile-format-string (string force-fn?)
  (if (not (or force-fn? (fancy-directives-p string)))
      string
      (eval `(formatter ,string))))

(defvar *format-string-cache* T)

(declaim (ftype (function ((or string function) boolean)
			  (values (or string function) &optional))
		process-format-string))
(defun process-format-string (string-or-fn force-fn?)
  (cond ((not (stringp string-or-fn)) string-or-fn) ;called from ~? too.
	((not *format-string-cache*)
	 (maybe-compile-format-string string-or-fn force-fn?))
	(T (when (not (hash-table-p *format-string-cache*))
	     (setq *format-string-cache* (make-hash-table :test #'eq)))
	   (let ((value (gethash string-or-fn *format-string-cache*)))
	     (when (or (not value) (and force-fn? (stringp value)))
	       (setq value (maybe-compile-format-string string-or-fn force-fn?))
	       (setf (gethash string-or-fn *format-string-cache*) value))
	     value))))

(defun format (stream string-or-fn &rest args)
  (cond ((stringp stream)
	 (cl:format stream "~A"
		      (with-output-to-string (stream)
			(apply #'format stream string-or-fn args)))
	 nil)
	((null stream)
	 (with-output-to-string (stream)
	   (apply #'format stream string-or-fn args)))
	(T (when (eq stream T)
	     (setq stream *standard-output*))
	   (when (stringp string-or-fn)
	     (setq string-or-fn (process-format-string string-or-fn nil)))
	   (cond ((not (stringp string-or-fn))
		  (apply string-or-fn stream args))
		 ((xp-structure-p stream)
		  (apply #'using-format stream string-or-fn args))
		 (T (apply #'cl:format stream string-or-fn args)))
	   nil)))


;Each of these functions expect to get called with two arguments
;start and end.  Start points to the first character after the ~
;marking the command.  End points to the first character after the
;command.  This includes the matching end command for paired commands.

(defmacro def-format-handler (char args &body body)
  (let ((name (intern (cl:format nil "FORMAT-~A" char) (find-package :pxp))))
    `(eval-when (:execute :load-toplevel :compile-toplevel)
       (defun ,name ,args ,@ body)
       (setf (gethash (char-upcase ,char) *fn-table*) (function ,name))
       (setf (gethash (char-downcase ,char) *fn-table*) (function ,name)))))

;Definitions of the forms used in the code created by PARSE.
;Note these functions assume the stream is in the var XP and is an xp stream,

; INITIAL holds the initial value of ARGS (for ~@*).
;Initial is always bound to (args) if it is bound at all.
;Note this uses args, but only when actually binding


(declaim (ftype (function () (values (eql init) &optional)) initial))
(defun initial () (setq *used-initial* T) 'init)

(defmacro bind-initial (&body code)
  `(let* ((*used-initial* nil)
	  (body (progn ,@ code)))
     (if *used-initial* (make-binding 'init (args) body) body)))

; ARGS holds the current argument list
;The val bound to args must always be computed (to use it up) even if args is not used.


(declaim (ftype (function () (values (eql args) &optional)) args))
(defun args () (setq *used-args* T) 'args)

(defmacro bind-args (doit? val &body code)
  (if (eq doit? T)
      `(let* ((val ,val)
	      (*used-args* nil)
	      (body (progn ,@ code)))
	 (if *used-args* (make-binding 'args val body) (cons val body)))
      `(flet ((code () ,@ code))
	 (if (not ,doit?)
	     (code) ;important bindings not done if not doit?
	     (let* ((val ,val)
		    (*used-args* nil)
		    (body (code)))
	       (if *used-args* (make-binding 'args val body) (cons val body)))))))


(declaim (ftype (function () (values (eql outer-args) &optional)) outer-args))
(defun outer-args () (setq *used-outer-args* T) 'outer-args)

(defmacro bind-outer-args (&body code)
  `(let* ((*used-outer-args* nil)
	  (body (progn ,@ code)))
     (if *used-outer-args* (make-binding 'outer-args (args) body) body)))

(defmacro maybe-bind (doit? var val &body code)
  `(let ((body (progn ,@ code)))
     (if ,doit? (make-binding ,var ,val body) body)))

(defun make-binding (var value body)
  `((let ((,var ,value)) ,@ body)))

(defmacro pprint-pop+ (args xp)
  `(if (pprint-pop-check+ ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))


(declaim (ftype (function (t xp-structure) (values boolean &optional)) pprint-pop-check+))
(defun pprint-pop-check+ (args xp)
  (incf *current-length*)
  (cond ((not (listp args))  ;must be first so supersedes length abbrev
	 (write-string++ ". " xp 0 2)
	 (write+ args xp)
	 T)
	((and *print-length* ;must supersede circle check
	      (not (< *current-length* *print-length*)))
	 (write-string++ "..." xp 0 3)
	 (setq *abbreviation-happened* T)
	 T)
	((and *circularity-hash-table* (not (zerop *current-length*)))
	 (case (circularity-process xp args T)
	   (:first ;; note must inhibit rechecking of circularity for args.
		   (write+ (cons (car args) (cdr args)) xp) T)
	   (:subsequent T)
	   (T nil)))))

(defmacro pprint-pop+top (args xp)
  `(if (pprint-pop-check+top ,args ,xp)
       (return-from logical-block nil)
       (pop ,args)))


(declaim (ftype (function (t xp-structure) (values boolean &optional)) pprint-pop-check+top))
(defun pprint-pop-check+top (args xp)
  (incf *current-length*)
  (cond ((not (listp args))  ;must be first so supersedes length abbrev
	 (write-string++ ". " xp 0 2)
	 (write+ args xp)
	 T)
	((and *print-length* ;must supersede circle check
	      (not (< *current-length* *print-length*)))
	 (write-string++ "..." xp 0 3)
	 (setq *abbreviation-happened* T)
	 T)))


(declaim (ftype (function ((mod #.array-total-size-limit) (mod #.array-total-size-limit))
			  (values t &optional))
		literal))
(defun literal (start end &aux (start start))
  (let ((forms
	  (loop :for s = start :then (1+ sub-end)
		:for next-newline = (position #\newline *string* :start s :end end)
		:for sub-end = (or next-newline end)
		:when (< s sub-end)
		:collect (if (= s (1- sub-end))
			   `(write-char++ ,(aref *string* s) xp)
			   `(write-string++ ,(subseq *string* s sub-end) xp
					    ,0 ,(- sub-end s)))
		:when (null next-newline)
		  :do (loop-finish)
		:collect `(pprint-newline+ :unconditional xp))))
    (if (null (cdr forms))
        (car forms)
	(cons 'progn forms))))

;This is available for putting on #".

(defvar *default-package*)


(declaim (ftype (function (stream character t) (values cons &optional)) format-string-reader))
(defun format-string-reader (stream sub-char arg)
    (declare (ignore arg))
  (unread-char sub-char stream)
  `(function
    (lambda (s &rest args)
      (formatter-in-package ,(read stream) ,(package-name *package*)))))

(defmacro formatter-in-package (string reader-package)
  (formatter-fn string reader-package))

(defmacro formatter (string)
  `(function
    (lambda (s &rest args)
      (formatter-in-package ,string ,(package-name *package*)))))


(declaim (ftype (function (string &key
				  (:start (mod #.array-total-size-limit))
				  (:end (mod #.array-total-size-limit)))
			  (values (or null character)
				  (or null (mod #.array-total-size-limit))
				  (or null (mod #.array-total-size-limit))
				  &optional))
		next-directive))
(defun next-directive (control-string &key start end)
  "Return three values.
1: Next directive character or nil.
2: Start position of params i.e. tilda, or nil.
3: End position of params, or nil."
  (multiple-value-bind (i j) (next-directive1 control-string :start start :end end)
    (if (null i)
      ;; No directives in format control string.
      (values nil nil nil)
      ;; Format control string has directives.
      (let* ((directive (aref control-string j))
	     (close (cdr (assoc directive '((#\( . #\)) (#\[ . #\]) (#\< . #\>) (#\{ . #\}))))))
	(if (null close)
	  ;; Single directive. e.g. ~A, ~S etc.
	  (values directive i j)
	  ;; Paired directives.
	  (labels ((rec (ii k count)
		     (cond
		       ((null ii)
		        (failed-to-compile 4 "No matching close directive" control-string j))
		       ((char= (aref control-string k) directive)
			;; Nest into.
		        (multiple-value-call #'rec
		          (next-directive1 control-string :start k :end end)
		          (1+ count)))
		       ((char= (aref control-string k) close)
			;; Nest back.
		        (if (minusp (1- count))
			  ;; Got directive pair.
		          (values directive i k)
			  ;; Still not get its pair yet.
		          (multiple-value-call #'rec
		            (next-directive1 control-string :start k :end end)
		            (1- count))))
		       (t ;; Met single directive, ignore.
			 (multiple-value-call #'rec
		            (next-directive1 control-string :start k :end end)
		            count)))))
	    (multiple-value-call #'rec
	      (next-directive1 control-string :start j :end end)
	      0)))))))

;This gets called with start pointing to the character after the ~ that
;starts a command.  Defaults, is a list of default values for the
;parameters.  Max is the maximum number of parameters allowed.  Nocolon,
;noatsign, nocolonatsign can be used to specify what colon atsign
;combinations are permitted. Parse params returns three values, colon?,
;atsign? and a list of code chunks that correspond to the parameters
;specified.


(declaim (ftype (function () (values t &optional)) get-arg))
(defun get-arg ()
  (if *get-arg-carefully*
      (if *at-top* `(pprint-pop+top ,(args) xp) `(pprint-pop+ ,(args) xp))
      `(pop ,(args))))

(defun num-args () `(length ,(args)))


(declaim (ftype (function ((mod #.array-total-size-limit)
			   list
			   &key (:max (or null (integer 0 *)))
			   (:nocolon boolean)
			   (:noatsign boolean)
			   (:nocolonatsign boolean))
			  (values boolean boolean list &optional))
		parse-params))
(defun parse-params (start defaults &key (max (length defaults))
		     (nocolon nil) (noatsign nil) (nocolonatsign nil))
  (let ((colon nil) (atsign nil) (params nil) (i start) j c)
    (loop
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (push (get-arg) params) (incf i))
	    ((char= c #\#) (push (num-args) params) (incf i))
	    ((char= c #\') (incf i) (push (aref *string* i) params) (incf i))
	    ((char= c #\,) (push nil params))
	    (T (setq j (position-not-in *string* "+-0123456789" :start i))
	       (when (= i j)
		 (return nil))
	       (push (parse-integer *string* :start i :end j :radix 10.) params)
	       (setq i j)))
      (if (char= (aref *string* i) #\,) (incf i) (return nil)))
    (setq params (nreverse params))
    (do ((ps params (cdr ps))
	 (ds defaults (cdr ds))
	 (nps nil))
	((null ds) (setq params (nreconc nps ps)))
      (push (cond ((or (null ps) (null (car ps))) (car ds))
		  ((not (consp (car ps))) (car ps))
		  (T `(cond (,(car ps)) (T ,(car ds)))))
	    nps))
    (when (and max (< max (length params)))
      (failed-to-compile 6 "Too many parameters" *string* i))
    (loop
      (setq c (aref *string* i))
      (cond ((char= c #\:)
	     (when colon
	       (failed-to-compile 7 "Two colons specified" *string* i))
	     (setq colon T))
	    ((char= c #\@)
	     (when atsign
	       (failed-to-compile 8 "Two atsigns specified" *string* i))
	     (setq atsign T))
	    (T (return nil)))
      (incf i))
    (when (and colon nocolon)
      (failed-to-compile 9 "Colon not permitted" *string* i))
    (when (and atsign noatsign)
      (failed-to-compile 10 "Atsign not permitted" *string* i))
    (when (and colon atsign nocolonatsign)
      (failed-to-compile 11 "Colon and atsign together not permitted" *string* i))
    (values colon atsign params)))


(declaim (ftype (function ((mod #.array-total-size-limit)
			   (mod #.array-total-size-limit))
			  (values list &optional))
		compile-format))
(defun compile-format (start end)
  (let ((start start)
	(result nil))
    (prog (c i j fn)
     L(multiple-value-setq (c i j) (next-directive *string* :start start :end end))
      (when (if (null c) (< start end) (< start i))
	(push (literal start (or i end)) result))
      (when (null c) (return (nreverse result)))
      (when (char= c #\newline)
	(multiple-value-bind (colon atsign)
	    (parse-params (1+ i) nil :nocolonatsign T)
	  (when atsign (push `(pprint-newline+ :unconditional xp) result))
	  (incf j)
	  (when (not colon)
	    (setq j (position-if-not
		      #'(lambda (c)
			  (or (char= c #\tab) (char= c #\space)))
		      *string* :start j :end end))
	    (when (null j) (setq j end)))
	  (setq start j)
	  (go L)))
      (setq fn (gethash c *fn-table*))
      (when (null fn) (failed-to-compile 5 "Unknown format directive" *string* j))
      (incf j)
      (push (funcall fn (1+ i) j) result)
      (setq start j)
      (go L))))

(declaim (ftype (function (string string) (values cons &optional)) formatter-fn))
(defun formatter-fn (*string* *default-package*)
  (handler-case `(apply (function call-with-xp-stream)
			(function
			  (lambda (xp &rest args)
			    ,@(bind-initial
				`((block top
					 ,@(let ((*get-arg-carefully* nil)
						 (*at-top* t)
						 (*inner-end* 'top)
						 (*outer-end* 'top))
					     (compile-format 0 (length *string*))))))
			    (when ,(args)
			      (copy-list ,(args))))) ;needed by symbolics.
			s args)
    (failed-to-compile (c)
      (warn c)
      `(let ((*string* ,*string*))
	 (apply #'format s *string* args)))))

;The business with the catch above allows many (formatter "...") errors to be
;reported in a file without stopping the compilation of the file.


;COMPILE-FORMAT gets called to turn a bit of format control string into code.

;Only called after correct parse is known.


(declaim (ftype (function ((mod #.array-total-size-limit))
			  (values (mod #.array-total-size-limit) &optional))
		params-start))
(defun directive-start (end) ;end points at characters after params
  (loop
    (setq end (position #\~ *string* :end end :from-end T))
    (when (or (zerop end) (not (char= (aref *string* (1- end)) #\')))
      (return end))
    (decf end)))

;breaks things up at ~; directives.


(declaim (ftype (function ((mod #.array-total-size-limit)
			   (mod #.array-total-size-limit))
			  (values list &optional))
		chunk-up))
(defun chunk-up (start end)
  (let ((positions (list start)) (spot start))
    (loop
      (multiple-value-bind (c i j) (next-directive *string* :start spot :end end)
	(declare (ignore i))
	(when (null c) (return (nreverse (cons end positions))))
	(when (char= c #\;) (push (1+ j) positions))
	(setq spot j)))))



(declaim (ftype (function ((mod #.array-total-size-limit) &optional boolean)
			  (values (or null (mod #.array-total-size-limit)) &optional))
		num-args-in-args))
(defun num-args-in-args (start &optional (errorp nil))
  (let ((n 0) (i (1- start)) c)
    (loop
      (setq i (position-not-in *string* "+-0123456789," :start (1+ i)))
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (incf n))
	    ((char= c #\#)
	     (when errorp
	       (failed-to-compile 21 "# not allowed in ~~<...~~> by (formatter \"...\")" *string* start))
	     (return nil))
	    ((char= c #\') (incf i))
	    (T (return n))))))

;Both these only called if correct parse already known.


(def-format-handler #\/ (start end)
  (multiple-value-bind (colon atsign params) (parse-params start nil :max nil)
    (let* ((whole-name-start (1+ (params-end *string* :start start)))
	   (colon-pos (position #\: *string* :start whole-name-start :end (1- end)))
	   (pkg (uiop:find-package*
		  (if colon-pos
		      (string-upcase (subseq *string* whole-name-start colon-pos))
		      *default-package*)))
	   (name-start (cond ((null colon-pos) whole-name-start)
			     ((and (< colon-pos (1- end))
				   (char= #\: (aref *string* (1+ colon-pos))))
			      (+ colon-pos 2))
			     (T (1+ colon-pos))))
	   (fn (intern (string-upcase (subseq *string* name-start (1- end))) pkg)))
      (if (not (find-if #'consp params))
	  `(funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ params)
	  (let ((vars (mapcar #'(lambda (arg)
				  (declare (ignore arg))
				  (gensym))
			      params)))
	    `(let ,(mapcar #'list vars params)
	       (funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ vars)))))))

(def-format-handler #\A (start end)
  (if (not (= end (1+ start)))
      (simple-directive start end)
      `(let ((*print-escape* nil))
	 (write+ ,(get-arg) XP))))

(def-format-handler #\S (start end)
  (if (not (= end (1+ start)))
      (simple-directive start end)
      `(let ((*print-escape* T))
	 (write+ ,(get-arg) XP))))

;The basic Format directives "DBOXRCFEG$".  The key thing about all of
;these directives is that they just get a single arg and print a chunk of
;stuff.  Further they are complex enough that I just call the standard
;definition of FORMAT to get the work done.  What should really be being
;called is the internal routine that FORMAT uses to do the corresponding
;work.  However, this cannot be done in a portable way.

(def-format-handler #\D (start end) (simple-directive start end))
(def-format-handler #\B (start end) (simple-directive start end))
(def-format-handler #\O (start end) (simple-directive start end))
(def-format-handler #\X (start end) (simple-directive start end))
(def-format-handler #\R (start end) (simple-directive start end))
(def-format-handler #\C (start end) (simple-directive start end))
(def-format-handler #\F (start end) (simple-directive start end))
(def-format-handler #\E (start end) (simple-directive start end))
(def-format-handler #\G (start end) (simple-directive start end))
(def-format-handler #\$ (start end) (simple-directive start end))

(defun simple-directive (start end)
  (let ((n (num-args-in-args start)))
    (if n `(using-format xp ,(subseq *string* (1- start) end)
			 ,@ (copy-tree (make-list (1+ n) :initial-element (get-arg))))
	(multiple-value-bind (colon atsign params)
	    (parse-params start nil :max 8)
	  (let* ((arg-str (subseq "v,v,v,v,v,v,v,v" 0
				  (max 0 (1- (* 2 (length params))))))
		 (str (concatenate 'string "~"
				   arg-str
				   (if colon ":" "")
				   (if atsign "@" "")
				   (subseq *string* (1- end) end))))
	    `(using-format xp ,str ,@ params ,(get-arg)))))))

(defun using-format (xp string &rest args)
  (let ((result (apply #'cl:format nil string args)))
    (write-string+ result xp 0 (length result))))

;Format directives that get open coded "P%&~|T*?^"

(def-format-handler #\P (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
  (let ((arg (if colon `(car (backup-in-list 1 ,(initial) ,(args))) (get-arg))))
    (if atsign
	`(if (not (eql ,arg 1)) (write-string++ "ies" xp 0 3) (write-char++ #\y xp))
	`(when (not (eql ,arg 1))
	   (write-char++ #\s XP))))))

(def-format-handler #\% (start end) (declare (ignore end))
  (multiple-newlines start :unconditional))

(def-format-handler #\& (start end) (declare (ignore end))
  (multiple-newlines start :fresh))

(defun multiple-newlines (start kind)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon T :noatsign T)
      (declare (ignore colon atsign))
    (if (eql (car params) 1)
        `(pprint-newline+ ,kind xp)
	`(multiple-newlines1 xp ,kind ,(car params)))))

(defun multiple-newlines1 (xp kind num)
  (loop :repeat num
	:do (pprint-newline+ kind xp)
	    (setq kind :unconditional)))

(def-format-handler #\| (start end) (declare (ignore end))
  (multiple-chars start #.(aref (cl:format nil "~|") 0)))

(def-format-handler #\~ (start end) (declare (ignore end))
  (multiple-chars start #\~))

(defun multiple-chars (start char)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon t :noatsign t)
      (declare (ignore colon atsign))
    (if (eql (car params) 1)
        `(write-char++ ,char xp)
	`(multiple-chars1 xp ,(car params) ,char))))

(defun multiple-chars1 (xp num char)
  (loop :repeat num :do (write-char++ char xp)))

(def-format-handler #\T (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params) (parse-params start '(1 1))
    `(pprint-tab+ ,(if colon
		       (if atsign :section-relative :section)
		       (if atsign :line-relative :line))
		  ,(pop params) ,(pop params) xp)))

(def-format-handler #\* (start end) (declare (ignore end))
  (if (atsignp *string* (params-end *string* :start start))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(0) :nocolon t)
	  (declare (ignore colon atsign))
	`(setq args (backup-to ,(car params) ,(initial) ,(args))))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(1))
	  (declare (ignore atsign))
	`(setq args
	       ,(if colon
		    `(backup-in-list ,(car params) ,(initial) ,(args))
		    `(nthcdr ,(car params) ,(args)))))))

;fancy stuff here, so will not get spurious indications of circularity.

(defun backup-in-list (num list some-tail)
  (backup-to (- (tail-pos list some-tail) num) list some-tail))


(declaim (ftype (function ((mod #.array-total-size-limit)
			   list list)
			  (values list &optional))
		backup-to))
(defun backup-to (num list some-tail)
  (if (not *circularity-hash-table*)
      (nthcdr num list)
      (multiple-value-bind (pos share) (tail-pos list some-tail)
	  (declare (ignore pos))
	(if (not (< num share))
	    (nthcdr num list)
	    (do ((L (nthcdr num list) (cdr L))
		 (n (- share num) (1- n))
		 (R nil (cons (car L) R)))
		((zerop n) (nreconc R L)))))))

;because of backup-to, a prefix of some-tail may have been copied (in which
;case it cannot share anything with list), but there is a cons in some-tail
;that is in list.  This can be used to determine the position of some-tail
;relative to list.  However, we have to be careful, because they both could
;be cdr recursive.


(declaim (ftype (function (list list)
			  (values (mod #.array-total-size-limit)
				  (mod #.array-total-size-limit)
				  &optional))
		tail-pos))
(defun tail-pos (list some-tail)
  (block outer
    (do ((n 0 (1+ n))
	 (L list (cdr L)))
	(nil)
      (do ((m n (1- m))
	   (ST some-tail (cdr ST)))
	  (nil)
	(when (minusp m)
	  (return nil))
	(when (eq ST L)
	  (return-from outer (values m n)))))))

(def-format-handler #\? (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil :nocolon t)
      (declare (ignore colon))
    (if (not atsign)
        `(apply #'format xp ,(get-arg) ,(get-arg))
	`(let ((fn (process-format-string ,(get-arg) T)))
	   (setq args (apply fn xp ,(args)))))))

(def-format-handler #\^ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 3 :noatsign t)
      (declare (ignore atsign))
    `(when ,(cond ((null params) `(null ,(if colon `(cdr ,(outer-args)) (args))))
		  (t `(do-complex-^-test ,@ params)))
       (return-from ,(if colon *outer-end* *inner-end*) nil))))

(defun do-complex-^-test (a1 &optional (a2 nil) (a3 nil))
  (cond (a3 (and (<= a1 a2) (<= a2 a3)))
	(a2 (= a1 a2))
	(t (= 0 a1))))

;delimited pairs of format directives. "(){}[]<>;"

(def-format-handler #\[ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 1 :nocolonatsign T)
    (setq start (1+ (params-end *string* :start start)))
    (let* ((chunks (chunk-up start end))
	   (innards (loop :for n :in chunks
			  :for m :in (cdr chunks)
			  :collect (compile-format n (directive-start m)))))
      (cond (colon (when (not (= (length innards) 2))
		     (failed-to-compile 13 "Wrong number of clauses in ~~:[...~~]" *string* (1- start)))
		   `(cond ((null ,(get-arg)) ,@ (car innards))
			  (T ,@ (cadr innards))))
	    (atsign (when (not (= (length innards) 1))
		      (failed-to-compile 14 "Too many clauses in ~~@[...~~]" *string* (1- start)))
		    `(cond ((car args) ,@ (car innards)) (T ,(get-arg))))
	    (T (let* ((j -1) (len (- (length chunks) 2))
		      (else? (colonp *string* (1- (nth len chunks)))))
		 `(case ,(if params (car params) (get-arg))
		    ,@(mapcar #'(lambda (unit)
				  (incf j)
				  `(,(if (and else? (= j len)) T j) ,@ unit))
			      innards))))))))

(def-format-handler #\( (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end *string* :start start)))
    (setq end (directive-start end))
    `(progn (push-char-mode xp ,(cond ((and colon atsign) :UP)
				      (colon :CAP1)
				      (atsign :CAP0)
				      (T :DOWN)))
	    ,@(compile-format start end)
	    (pop-char-mode xp))))

(def-format-handler #\; (start end) (declare (ignore start))
  (failed-to-compile 15 "~~; appears out of context" *string* (1- end)))
(def-format-handler #\] (start end) (declare (ignore start))
  (failed-to-compile 16 "Unmatched closing directive" *string* (1- end)))
(def-format-handler #\) (start end) (declare (ignore start))
  (failed-to-compile 17 "Unmatched closing directive" *string* (1- end)))
(def-format-handler #\> (start end) (declare (ignore start))
  (failed-to-compile 18 "Unmatched closing directive" *string* (1- end)))
(def-format-handler #\} (start end) (declare (ignore start))
  (failed-to-compile 19 "Unmatched closing directive" *string* (1- end)))

(def-format-handler #\{ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(-1) :max 1)
    (let* ((force-once (colonp *string* (1- end)))
	   (n (car params))
	   (bounded (not (eql n -1))))
      (setq start (1+ (params-end *string* :start start)))
      (setq end (directive-start end))
      (car (maybe-bind bounded 'N n ;must be outermost if is V or #
	     (maybe-bind (not (> end start)) 'FN  ;must be second
			 `(process-format-string ,(get-arg) T)
	       (bind-args (not atsign) (get-arg)
		 `((prog () ,@(when force-once
				'((go S)))
		       L (when (null ,(args))
			   (return nil))
		       ,@(when force-once
			   '(S))
			 ,@(when bounded
			     '((if (= N 0) (return nil) (decf N))))
			 ,@(bind-outer-args
			     (bind-args colon (get-arg)
			       (bind-initial
				 (let ((*get-arg-carefully*
					 (and *get-arg-carefully* atsign))
				       (*at-top* (and *at-top* atsign))
				       (*outer-end* nil)
				       (*inner-end* nil))
				   (if (not colon)
				       (if (not (> end start))
					   `((setq args (apply FN xp ,(args))))
					   (compile-format start end))
				       (let ((*inner-end* 'inner))
					 `((block inner
					     ,@(if (not (> end start))
						   `((setq args (apply FN xp ,(args))))
						   (compile-format start end))))))))))
			 (go L))))))))))

(def-format-handler #\< (start end)
  (if (colonp *string* (1- end))
      (handle-logical-block start end)
      (handle-standard-< start end)))

(defun handle-standard-< (start end)
  `(using-format xp ,(subseq *string* (1- start) end)
		 ,@ (copy-tree (make-list (num-args-in-directive start end)
					  :initial-element (get-arg)))))

(defun num-args-in-directive (start end)
  (let ((n 0) c i j)
    (incf n (num-args-in-args start T))
    (multiple-value-setq (j i) (next-directive1 *string* :start start :end end))
    (loop
      (multiple-value-setq (c i j) (next-directive *string* :start j :end end))
      (when (null c) (return n))
      (cond ((eql c #\;)
	     (when (colonp *string* j)
	       (failed-to-compile 22 "~~:; not supported in ~~<...~~> by (formatter \"...\")." *string* j)))
	    ((find c "*[^<_IiWw{Tt")
	     (failed-to-compile 23 "~~<...~~> too complicated to be supported by (formatter \"...\")." *string* j))
	    ((eql c #\() (incf n (num-args-in-directive (1+ i) j)))
	    ((find c "%&\|~") (incf n (num-args-in-args (1+ i) T)))
	    ((eql c #\?)
	     (when (atsignp *string* j)
	       (failed-to-compile 23 "~~<...~~> too complicated to be supported by (formatter \"...\")." *string* j))
	     (incf n 2))
	    ((find c "AaSsDdBbOoXxRrCcFfEeGg$Pp")
	     (incf n (1+ (num-args-in-args (1+ i) T))))))))

;The pretty-printing directives. "_IW<:>"

(def-format-handler #\_ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    `(pprint-newline+ ,(cond ((and colon atsign) :mandatory)
			     (colon :fill)
			     (atsign :miser)
			     (T :linear)) XP)))

(def-format-handler #\I (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0) :noatsign T)
      (declare (ignore atsign))
    `(pprint-indent+ ,(if colon :current :block) ,(car params) XP)))

(def-format-handler #\W (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (cond ((not (or colon atsign)) `(write+ ,(get-arg) XP))
	  (T `(let (,@(when colon
			'((*print-pretty* T)))
		    ,@(when atsign
			'((*print-level* nil) (*print-length* nil))))
		(write+ ,(get-arg) XP))))))

(defun handle-logical-block (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end *string* :start start)))
    (let* ((chunks (chunk-up start end))
	   (on-each-line?
	     (and (cddr chunks) (atsignp *string* (1- (cadr chunks)))))
	   (prefix
	     (cond ((cddr chunks) (pop chunks)
		    (subseq *string* start (directive-start (car chunks))))
		   (colon "(")))
	   (suffix
	     (cond ((cddr chunks)
		    (subseq *string* (cadr chunks)
			    (directive-start (caddr chunks))))
		   (colon ")"))))
      (when (cdddr chunks) (failed-to-compile 24 "Too many subclauses in ~~<...~~:>" *string* (1- start)))
      (when (and prefix (or (find #\~ prefix) (find #\newline prefix)))
	(failed-to-compile 25 "Prefix in ~~<...~~:> must be a literal string without newline" *string* start))
      (when (and suffix (or (find #\~ suffix) (find #\newline suffix)))
	(failed-to-compile 26 "Suffix in ~~<...~~:> must be a literal string without newline"
	     *string* (cadr chunks)))
      (car (bind-args T (if atsign `(prog1 ,(args) (setq ,(args) nil)) (get-arg))
	     (bind-initial
	       `((pprint-logical-block+ (xp ,(args) ,prefix ,suffix ,on-each-line?
					    ,(not (and *at-top* atsign)) ,atsign)
		   ,@(fill-transform (atsignp *string* (1- end))
		       (let ((*get-arg-carefully* T)
			     (*at-top* (and *at-top* atsign))
			     (*inner-end* 'logical-block)
			     (*outer-end* 'logical-block))
			 (compile-format (car chunks)
					 (directive-start (cadr chunks)))))))))))))


(declaim (ftype (function (xp-structure t boolean)
			  (values boolean &optional))
		check-block-abbreviation))
(defun check-block-abbreviation (xp args circle-check?)
  (cond ((not (listp args)) (write+ args xp) T)
	((and *print-level* (> pxp.dispatch:*current-level* *print-level*))
	 (write-char++ #\# XP) (setq *abbreviation-happened* T) T)
	((and *circularity-hash-table* circle-check?
	      (eq (circularity-process xp args nil) :subsequent)) T)
	(T nil)))


(declaim (ftype (function (boolean list) (values list &optional)) fill-transform))
(defun fill-transform (doit? body)
  (if (not doit?)
      body
      (mapcan #'(lambda (form)
		  (cond ((eq (car form) 'write-string++)
			 (fill-transform-literal (cadr form)))
			((eq (car form) 'write-char++)
			 (fill-transform-char (cadr form)))
			(T (list form))))
	      body)))


(declaim (ftype (function (character) (values cons &optional)) fill-transorm-char))
(defun fill-transform-char (char)
  (if (or (char= char #\space) (char= char #\tab))
      (list `(write-char++ ,char xp) '(pprint-newline+ :fill xp))
      `((write-char++ ,char xp))))


(declaim (ftype (function (string) (values list &optional)) fill-transform-literal))
(defun fill-transform-literal (string)
  (flet ((white-space (c) (or (char= c #\space) (char= c #\tab))))
    (do ((index 0 end) (result) (end nil nil)) (nil)
      (let ((white (position-if #'white-space string :start index)))
	(when white
	  (setq end (position-if-not #'white-space string :start (1+ white))))
	(when (null end)
	  (setq end (length string)))
	(push `(write-string++ ,(subseq string index end) xp ,0 ,(- end index))
	      result)
	(when white
	  (push '(pprint-newline+ :fill xp) result))
	(when (null white)
	  (return (nreverse result)))))))

 ) ;end of eval when for all (formatter "...") stuff.

;                ---- PRETTY PRINTING FORMATS ----

(defun pretty-array (xp array)
  (cond ((vectorp array) (pretty-vector xp array))
	((zerop (array-rank array))
	 (write-string++ "#0A " xp 0 4)
	 (write+ (aref array) xp))
	(T (pretty-non-vector xp array))))

(defun pretty-vector (xp v)
  (pprint-logical-block (xp nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
	(loop (pprint-pop)
	      (write+ (aref v i) xp)
	      (when (= (incf i) end)
		(return nil))
	      (write-char++ #\space xp)
	      (pprint-newline+ :fill xp))))))

(defvar *prefix*)

(defun pretty-non-vector (xp array)
  (let* ((bottom (1- (array-rank array)))
	 (indices (make-list (1+ bottom) :initial-element 0))
	 (dims (array-dimensions array))
	 (*prefix* (cl:format nil "#~DA(" (1+ bottom))))
    (labels ((pretty-slice (slice)
	       (pprint-logical-block (xp nil :prefix *prefix* :suffix ")")
		 (let ((end (nth slice dims))
		       (spot (nthcdr slice indices)) ; to avoid inner looping.
		       (i 0)
		       (*prefix* "("))
		   (when (plusp end)
		     (loop (pprint-pop)
			   (setf (car spot) i)
			   (if (= slice bottom)
			       (write+ (apply #'aref array indices) xp)
			       (pretty-slice (1+ slice)))
			   (when (= (incf i) end)
			     (return nil))
			   (write-char++ #\space xp)
			   (pprint-newline+ (if (= slice bottom) :fill :linear) xp)))))))
      (pretty-slice 0))))

;Must use pprint-logical-block (no +) in the following three, because they are
;exported functions.

(defun pprint-linear (s list &optional (colon? T) atsign?)
     (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :linear s))))

(defun pprint-fill (s list &optional (colon? T) atsign?)
    (declare (ignore atsign?))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-newline+ :fill s))))

(defun pprint-tabular (s list &optional (colon? T) atsign? (tabsize nil))
  (declare (ignore atsign?))
  (when (null tabsize) (setq tabsize 16))
  (pprint-logical-block (s list :prefix (if colon? "(" "")
			        :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space s)
	  (pprint-tab+ :section-relative 0 tabsize s)
	  (pprint-newline+ :fill s))))

(defun fn-call (xp list)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list))

;Although idiosyncratic, I have found this very useful to avoid large
;indentations when printing out code.

(defun alternative-fn-call (xp list)
  (if (> (length (symbol-name (car list))) 12)
      (funcall (formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list)
      (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list)))

(defun bind-list (xp list &rest args)
    (declare (ignore args))
  (if (do ((i 50 (1- i))
	   (ls list (cdr ls))) ((null ls) t)
	(when (or (not (consp ls)) (not (symbolp (car ls))) (minusp i))
	  (return nil)))
      (pprint-fill xp list)
      (funcall (formatter "~:<~@{~:/pxp:pprint-fill/~^ ~_~}~:>") xp list)))

(defun block-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

(defun defun-like (xp list &rest args)
    (declare (ignore args))
  (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/pxp:pprint-fill/~^~@{ ~_~W~^~}~:>")
	   xp list))

(defun print-fancy-fn-call (xp list template)
  (let ((i 0) (in-first-section T))
    (pprint-logical-block+ (xp list "(" ")" nil T nil)
      (write+ (pprint-pop) xp)
      (pprint-indent+ :current 1 xp)
      (loop
	(pprint-exit-if-list-exhausted)
	(write-char++ #\space xp)
	(when (eq i (car template))
	  (pprint-indent+ :block (cadr template) xp)
	  (setq template (cddr template))
	  (setq in-first-section nil))
	(pprint-newline (cond ((and (zerop i) in-first-section) :miser)
			      (in-first-section :fill)
			      (T :linear))
			xp)
	(write+ (pprint-pop) xp)
	(incf i)))))

(defun maybelab (xp item &rest args)
    (declare (ignore args) (special need-newline indentation))
  (when need-newline (pprint-newline+ :mandatory xp))
  (cond ((and item (symbolp item))
	 (write+ item xp)
	 (setq need-newline nil))
	(T (pprint-tab+ :section indentation 0 xp)
	   (write+ item xp)
	   (setq need-newline T))))

(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))


;THE FOLLOWING STUFF SETS UP THE DEFAULT *PRINT-PPRINT-DISPATCH*

;This is an attempt to specify a correct format for every form in the CL book
;that does not just get printed out like an ordinary function call
;(i.e., most special forms and many macros).  This of course does not
;cover anything new you define.

(defun let-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~/pxp:bind-list/~^~@{ ~_~W~^~}~:>") xp obj))

(defun cond-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~:/pxp:pprint-linear/~^ ~_~}~:>") xp obj))

(defun dmm-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun defsetf-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun do-print (xp obj)
  (funcall
 (formatter "~:<~W~^ ~:I~@_~/pxp:bind-list/~^ ~_~:/pxp:pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
           xp obj))


(defun flet-print (xp obj)
  (funcall (formatter "~:<~1I~W~^ ~@_~:<~@{~/pxp:block-like/~^ ~_~}~:>~^~@{ ~_~W~^~}~:>")
	   xp obj))

(defun function-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "#'~W") xp (cadr list))
      (fn-call xp list)))

(defun mvb-print (xp list)
  (print-fancy-fn-call xp list '(1 3 2 1)))

(defun prog-print (xp list)
  (let ((need-newline T) (indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~:/pxp:pprint-fill/~^ ~@{~/pxp:maybelab/~^ ~}~:>")
	     xp list)))

(defun setq-print (xp obj)
  (funcall (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~:_~W~^ ~_~}~:>") xp obj))

(defun quote-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (formatter "'~W") xp (cadr list))
      (pprint-fill xp list)))

(defun tagbody-print (xp list)
  (let ((need-newline (and (consp (cdr list))
			   (symbolp (cadr list)) (cadr list)))
	(indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (formatter "~:<~W~^ ~@{~/pxp:maybelab/~^ ~}~:>") xp list)))

(defun up-print (xp list)
  (print-fancy-fn-call xp list '(0 3 1 1)))

;here is some simple stuff for printing LOOP

;The challange here is that we have to effectively parse the clauses of the
;loop in order to know how to print things.  Also you want to do this in a
;purely incremental way so that all of the abbreviation things work, and
;you wont blow up on circular lists or the like.  (More aesthic output could
;be produced by really parsing the clauses into nested lists before printing them.)

;The following program assumes the following simplified grammar of the loop
;clauses that explains how to print them.  Note that it does not bare much
;resemblence to the right parsing grammar, however, it produces half decent
;output.  The way to make the output better is to make the grammar more
;detailed.
;
;loop == (LOOP {clause}*)      ;one clause on each line.
;clause == block | linear | cond | finally
;block == block-head {expr}*   ;as many exprs as possible on each line.
;linear == linear-head {expr}* ;one expr on each line.
;finally == FINALLY [DO | DOING | RETURN] {expr}* ;one expr on each line.
;cond == cond-head [expr]
;          clause
;	   {AND clause}*       ;one AND on each line.
;        [ELSE
;          clause
;	   {AND clause}*]      ;one AND on each line.
;        [END]
;block-head == FOR | AS | WITH | AND
;              | REPEAT | NAMED | WHILE | UNTIL | ALWAYS | NEVER | THEREIS | RETURN
;              | COLLECT | COLLECTING | APPEND | APPENDING | NCONC | NCONCING | COUNT
;              | COUNTING | SUM | SUMMING | MAXIMIZE | MAXIMIZING | MINIMIZE | MINIMIZING
;linear-head == DO | DOING | INITIALLY
;var-head == FOR | AS | WITH
;cond-head == IF | WHEN | UNLESS
;expr == <anything that is not a head symbol>

;Note all the string comparisons below are required to support some
;existing implementations of LOOP.

(defun token-type (token &aux string)
  (cond ((not (symbolp token)) :expr)
	((string= (setq string (string token)) "FINALLY") :finally)
	((member string '("IF" "WHEN" "UNLESS") :test #'string=) :cond-head)
	((member string '("DO" "DOING" "INITIALLY") :test #'string=) :linear-head)
	((member string '("FOR" "AS" "WITH" "AND" "END" "ELSE"
			  "REPEAT" "NAMED" "WHILE" "UNTIL" "ALWAYS" "NEVER"
			  "THEREIS" "RETURN" "COLLECT" "COLLECTING" "APPEND"
			  "APPENDING" "NCONC" "NCONCING" "COUNT" "COUNTING"
			  "SUM" "SUMMING" "MAXIMIZE" "MAXIMIZING"
			  "MINIMIZE" "MINIMIZING")
		 :test #'string=)
	 :block-head)
	(T :expr)))

(defun pretty-loop (xp loop)
  (if (not (and (consp (cdr loop)) (symbolp (cadr loop)))) ; old-style loop
      (fn-call xp loop)
      (pprint-logical-block (xp loop :prefix "(" :suffix ")")
	(let (token type)
	  (labels ((next-token ()
		     (pprint-exit-if-list-exhausted)
		     (setq token (pprint-pop))
		     (setq type (token-type token)))
		   (print-clause (xp)
		     (case type
		       (:linear-head (print-exprs xp nil :mandatory))
		       (:cond-head (print-cond xp))
		       (:finally (print-exprs xp T :mandatory))
		       (otherwise (print-exprs xp nil :fill))))
		   (print-exprs (xp skip-first-non-expr newline-type)
		     (let ((first token))
		       (next-token)	;so always happens no matter what
		       (pprint-logical-block (xp nil)
			 (write first :stream xp)
			 (when (and skip-first-non-expr (not (eq type :expr)))
			   (write-char #\space xp)
			   (write token :stream xp)
			   (next-token))
			 (when (eq type :expr)
			   (write-char #\space xp)
			   (pprint-indent :current 0 xp)
			   (loop (write token :stream xp)
				 (next-token)
				 (when (not (eq type :expr)) (return nil))
				 (write-char #\space xp)
				 (pprint-newline newline-type xp))))))
		   (print-cond (xp)
		     (let ((first token))
		       (next-token)	;so always happens no matter what
		       (pprint-logical-block (xp nil)
			 (write first :stream xp)
			 (when (eq type :expr)
			   (write-char #\space xp)
			   (write token :stream xp)
			   (next-token))
			 (write-char #\space xp)
			 (pprint-indent :block 2 xp)
			 (pprint-newline :linear xp)
			 (print-clause xp)
			 (print-and-list xp)
			 (when (and (symbolp token)
				    (string= (string token) "ELSE"))
			   (print-else-or-end xp)
			   (write-char #\space xp)
			   (pprint-newline :linear xp)
			   (print-clause xp)
			   (print-and-list xp))
			 (when (and (symbolp token)
				    (string= (string token) "END"))
			   (print-else-or-end xp)))))
		   (print-and-list (xp)
		     (loop (when (not (and (symbolp token)
					   (string= (string token) "AND")))
				 (return nil))
			   (write-char #\space xp)
			   (pprint-newline :mandatory xp)
			   (write token :stream xp)
			   (next-token)
			   (write-char #\space xp)
			   (print-clause xp)))
		   (print-else-or-end (xp)
		     (write-char #\space xp)
		     (pprint-indent :block 0 xp)
		     (pprint-newline :linear xp)
		     (write token :stream xp)
		     (next-token)
		     (pprint-indent :block 2 xp)))
	    (pprint-exit-if-list-exhausted)
	    (write (pprint-pop) :stream xp)
	    (next-token)
	    (write-char #\space xp)
	    (pprint-indent :current 0 xp)
	    (loop (print-clause xp)
		  (write-char #\space xp)
		  (pprint-newline :linear xp)))))))

;Backquote is a big problem we MUST do all this reconsing of structure in
;order to get a list that will trigger the right formatting functions to
;operate on it.  On the other side of the coin, we must use a non-list structure
;for the little backquote printing markers to ensure that they will always
;print out the way we want no matter what the code printers say.
;  Note that since it is sometimes possible to write the same
;backquote form in several ways, this might not necessarily print out a
;form in exactly the way you wrote it.  For example '`(a .,b) and '`(a ,@b)
;both print out as `'(a .,b), because the backquote reader produces the
;same code in both cases.

;; NOTE [by SATO Shinichi]
;; ".,hoge" is not familier in 2021, so now xp generates ",@hoge" notation.
;; Some coner case tests may needed.

(defvar *bq-list* (list
  #+cmu 'lisp::backq-list
  #+abcl 'system::backq-list
  #.(if (find-package :fare-quasiquote)
      `',(uiop:find-symbol* 'list '#:fare-quasiquote)
      (values))))
(defvar *bq-list** (list
  #+cmu 'lisp::backq-list*
  #+abcl 'system::backq-list*
  #.(if (find-package :fare-quasiquote)
      `',(uiop:find-symbol* 'list* '#:fare-quasiquote)
      (values))))
(defvar *bq-cons* (list
  #+cmu 'lisp::backq-cons
  #+abcl 'system::backq-cons
  #.(if (find-package :fare-quasiquote)
      `',(uiop:find-symbol* 'cons '#:fare-quasiquote)
      (values))))
(defvar *bq-append* (list
  #+cmu 'lisp::backq-append
  #+abcl 'system::backq-append
  #.(if (find-package :fare-quasiquote)
      `',(uiop:find-symbol* 'append '#:fare-quasiquote)
      (values))))
(defvar *bq-nconc* (list
  #+cmu 'lisp::backq-nconc
  #+abcl 'system::backq-nconc
  #.(if (find-package :fare-quasiquote)
      `',(uiop:find-symbol* 'nconc '#:fare-quasiquote)
      (values))))

(defun bq-print (xp obj)
  (funcall (formatter "`~W") xp (bqtify obj)))

(defvar *bq-vector* (list
  #+cmu 'lisp::backq-vector
  #+abcl 'system::backq-vector))
(defvar *bq-list-to-vector* (list
  #+(or cmu abcl) '#:no-such)) ;turned off

(defun bq-vector-print (xp obj)
  (funcall (pxp:formatter "`#~W") xp (car (bqtify obj))))

(cl:defstruct bq-struct code data)

(defun bq-struct-print (xp obj)
  ;; We must print out the string as a string, in order to prevent
  ;; circularity testing
  (let ((code (bq-struct-code obj)))
    (declare (simple-string code))
    (write-string++ code xp 0 (length code))
    (write+ (bq-struct-data obj) xp)))

;Convert the backquote form to a list resembling what the user typed in,
;with calls to printers for ",", ",@", etc.

(defun bqtify (exp)
  (cond ((or (numberp exp) (eq exp t) (null exp) (stringp exp)) exp)
	((symbolp exp) (make-bq-struct :code "," :data exp))
	((bq-struct-p exp)
	 (make-bq-struct :code "," :data exp))
	((atom exp) exp)
	((eq (car exp) 'quote) (cadr exp))
	((member (car exp) *bq-list*)
	 (mapcar 'bqtify (cdr exp)))
	((member (car exp) *bq-cons*)
	 (cons (bqtify (cadr exp)) (bqtify-inline (cddr exp) nil)))
	((member (car exp) *bq-list**)
	 (nconc (mapcar 'bqtify (butlast (cdr exp)))
		(bqtify-inline (last exp) nil)))
	((member (car exp) *bq-append*)
	 (mapcon #'(lambda (x) (bqtify-inline x t)) (cdr exp)))
	((member (car exp) *bq-nconc*)
	 (mapcon #'(lambda (x) (bqtify-inline x nil)) (cdr exp)))
	((member (car exp) *bq-vector*)
	 (list (mapcar 'bqtify (cdr exp))))
	((member (car exp) *bq-list-to-vector*)
	 (mapcar 'bqtify (cdr exp)))
	(t (make-bq-struct :code "," :data exp))))

;Convert a thing in a bq-form which is being expanded into the list, not
;just being made an element.  The argument is the list whose car is the
;form, and the value is stuff to be appended into the resulting code list.

(defun bqtify-inline (loc copy-p)
  (cond ((atom (cdr loc))
	 (let ((tem (bqtify (car loc))))
	   (cond ((and (bq-struct-p tem) (equal (bq-struct-code tem) ","))
		  (list (make-bq-struct :code ",@" :data (car loc))))
		 (t tem))))
	((and (listp (car loc))
	      (eq (caar loc) 'quote)
	      (listp (cadar loc)))
	 (cadar loc))
	(t (list (make-bq-struct :code (cond (copy-p ",@") (T ",."))
				 :data (car loc))))))


;; BACKQUOTE.
#+sbcl
(eval-when (:load-toplevel :execute)
  (flet ((print-comma (output comma &rest noise)
           (declare (ignore noise))
	   (funcall (formatter "~[,~;,.~;,@~]~W")
		    output (sb-int:comma-kind comma)
		    (sb-int:comma-expr comma)))
	 (print-backquote (output backquote &rest noise)
           (declare (ignore noise))
	   (funcall (formatter "`~W")
		    output (cadr backquote))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member sb-int:quasiquote)) #'print-backquote 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch 'sb-impl::comma #'print-comma 0 pxp.dispatch:*IPD*)))

#+clisp
(eval-when (:load-toplevel :execute)
  (flet ((printer (prefix)
           (lambda (output exp &rest noise)
	     (declare (ignore noise))
	     (format output "~A~W" prefix (cadr exp)))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::backquote)) (printer "`") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::unquote)) (printer ",") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::nsplice)) (printer ",.") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::splice)) (printer ",@") 0 pxp.dispatch:*IPD*)))

#+ecl
(eval-when (:load-toplevel :execute)
  (flet ((printer (prefix)
           (lambda (output exp &rest noise)
	     (declare (ignore noise))
	     (format output "~A~W" prefix (cadr exp)))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:quasiquote)) (printer "`") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:unquote)) (printer ",") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:unquote-nsplice)) (printer ",.") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:unquote-splice)) (printer ",@") 0 pxp.dispatch:*IPD*)))

#+allegro
(eval-when (:load-toplevel :execute)
  (flet ((printer (prefix)
           (lambda (output exp &rest noise)
	     (declare (ignore noise))
	     (format output "~A~W" prefix (cadr exp)))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member excl::backquote)) (printer "`") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member excl::bq-comma)) (printer ",") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member excl::bq-comma-dot)) (printer ",.") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member excl::bq-comma-atsign)) (printer ",@") 0 pxp.dispatch:*IPD*)))

#+(or :cmu :abcl #.(cl:if (cl:find-package :fare-quasiquote) '(and) '(or)))
(eval-when (:load-toplevel :execute)
(pxp.dispatch:set-pprint-dispatch 'bq-struct #'bq-struct-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-cons*)) #'bq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-list*)) #'bq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-list**)) #'bq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-append*)) #'bq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-nconc*)) #'bq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-vector*)) #'bq-vector-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch `(cons (member ,@*bq-list-to-vector*)) #'bq-vector-print 0 pxp.dispatch:*IPD*)
) ; Eval-when.

(pxp.dispatch:set-pprint-dispatch '(satisfies function-call-p) #'fn-call -5 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch 'cons #'pprint-fill -10 pxp.dispatch:*IPD*)

(pxp.dispatch:set-pprint-dispatch '(cons (member defstruct)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member block)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member case)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member catch)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member ccase)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member cond)) #'cond-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member ctypecase)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member defconstant)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member define-setf-expander)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member defmacro)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member define-modify-macro)) #'dmm-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member defparameter)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member defsetf)) #'defsetf-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member cl:defstruct)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member deftype)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member defun)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member defvar)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member do)) #'do-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member do*)) #'do-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member do-all-symbols)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member do-external-symbols)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member do-symbols)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member dolist)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member dotimes)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member ecase)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member etypecase)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member eval-when)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member flet)) #'flet-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member function)) #'function-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member labels)) #'flet-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member lambda)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member let)) #'let-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member let*)) #'let-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member locally)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member loop)) #'pretty-loop 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member macrolet)) #'flet-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member multiple-value-bind)) #'mvb-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member multiple-value-setq)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member prog)) #'prog-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member prog*)) #'prog-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member progv)) #'defun-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member psetf)) #'setq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member psetq)) #'setq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member quote)) #'quote-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member return-from)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member setf)) #'setq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member setq)) #'setq-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member tagbody)) #'tagbody-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member throw)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member typecase)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member unless)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member unwind-protect)) #'up-print 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member when)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member with-input-from-string)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member with-open-file)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member with-open-stream)) #'block-like 0 pxp.dispatch:*IPD*)
(pxp.dispatch:set-pprint-dispatch '(cons (member with-output-to-string)) #'block-like 0 pxp.dispatch:*IPD*)

(defun pprint-dispatch-print (xp table)
  (let ((stuff (pxp.dispatch:pprint-dispatch-entries table)))
    (pprint-logical-block (xp stuff :prefix "#<" :suffix ">")
      (format xp (formatter "pprint dispatch table containing ~A entries: ")
	      (length stuff))
      (loop (pprint-exit-if-list-exhausted)
	    (pxp.dispatch:show (pprint-pop) xp)))))

(setf (get 'pxp.dispatch:pprint-dispatch 'structure-printer) #'pprint-dispatch-print)

(pxp.dispatch:set-pprint-dispatch 'pxp.dispatch:pprint-dispatch #'pprint-dispatch-print 0 pxp.dispatch:*IPD*)


;so only happens first time is loaded.
(pxp.dispatch:initialize)

(defun install (&key (package *package*) (macro nil) (shadow T) (remove nil))
  (when (not (packagep package)) (setq package (find-package package)))
  (when (not remove)
    (when macro
      (set-dispatch-macro-character #\# #\" #'format-string-reader))
    (when (not (eq package (find-package :pxp)))
      (use-package :pxp package)
      (when shadow (shadowing-import *xp-printing-functions* package))))
  (when (and remove (member (find-package :pxp) (package-use-list package)))
    (unuse-package :pxp package)
    (dolist (sym (intersection *xp-printing-functions*
			       (package-shadowing-symbols package)))
      (unintern sym package)))
  T)

;changes since last documentation.
;~/fn/ only refers to global function values, not lexical.

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------
