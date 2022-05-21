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
       ,@(reexport :pxp.dispatch copy-pprint-dispatch pprint-dispatch
		   set-pprint-dispatch *print-pprint-dispatch*)
       ,@(reexport :pxp.stream *print-right-margin* *print-miser-width* *print-lines*
		   *print-shared* #:*last-abbreviated-printing*)
       ,@(reexport :pxp.printer pprint-pop pprint-exit-if-list-exhausted pprint-logical-block
		   write print prin1 princ pprint write-to-string prin1-to-string princ-to-string
		   write-string write-line fresh-line
		   pprint-tab pprint-newline pprint-indent pprint-fill pprint-linear pprint-tabular)
       ,@(reexport :pxp.format format formatter defstruct)
       (:export #:*default-right-margin*)))

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

;When an entry is first made it is zero.
;If a duplicate is found, a positive integer tag is assigned.
;After the first time the object is printed out, the tag is negated.

;               ---- XP STRUCTURES, AND THE INTERNAL ALGORITHM ----

;This does not tell you the line position you were at when the total-position
;was set, unless there have been no newlines or indentation output
;between ptr and the current output point.
;we shift the queue over rather than using a circular queue because
;that works out to be a lot faster in practice.  Note, short printout
;does not ever cause a shift, and even in long printout, the queue is
;shifted left for free every time it happens to empty out.


;This maintains a list of XP structures.  We save them
;so that we don't have to create new ones all of the time.
;We have separate objects so that many can be in use at once.

;(Note should really be doing some locking here, but CL does not have the
;primitives for it.  There is a tiny probability here that two different
;processes could end up trying to use the same xp-stream)

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



;Assumes is only called when char-mode is non-nil

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


;This handles the basic outputting of characters.  note + suffix means that
;the stream is known to be an XP stream, all inputs are mandatory, and no
;error checking has to be done.
;;;; +++ suffix functions.

;;;; ++ suffix functions.
;;  Suffix ++ additionally means that the output is guaranteed not to contain a newline char.
;;;; + suffix functions.

;note following is smallest number >= x that is a multiple of colinc
;  (* colinc (floor (+ x (1- colinc)) colinc))

;this can only be called last!

;This prints out a line of stuff.



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


;This prints a few very common, simple atoms very fast.
;Pragmatically, this turns out to be an enormous savings over going to the
;standard printer all the time.  There would be diminishing returns from making
;this work with more things, but might be worth it.


;just wants to succeed fast in a lot of common cases.
;assumes no funny readtable junk for the characters shown.


;Any format string that is converted to a function is always printed
;via an XP stream (See formatter).

;This has to violate the XP data abstraction and fool with internal
;stuff, in order to find out the right info to return as the result.

;Each of these causes the stream to be pessimistic and insert
;newlines wherever it might have to, when forcing the partial output
;out.  This is so that things will be in a consistent state if
;output continues to the stream later.

;note we are assuming that if a structure is defined using xp::defstruct,
;then its print-function (if any) will be defined using xp::print etc.

;           ---- FUNCTIONAL INTERFACE TO DYNAMIC FORMATTING ----

;The internal functions in this file, and the (formatter "...") expansions
;use the '+' forms of these functions directly (which is faster) because,
;they do not need error checking or fancy stream coercion.  The '++' forms
;additionally assume the thing being output does not contain a newline.

;Assumes var and args must be variables.  Other arguments must be literals or variables.

;                        ---- COMPILED FORMAT ----

;Note that compiled format strings always print through xp streams even if
;they don't have any xp directives in them.  As a result, the compiled code
;can depend on the fact that the stream being operated on is an xp
;stream not an ordinary one.


;                ---- PRETTY PRINTING FORMATS ----

;Must use pprint-logical-block (no +) in the following three, because they are
;exported functions.

(defun fn-call (xp list)
  (funcall (pxp.format:formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list))

;Although idiosyncratic, I have found this very useful to avoid large
;indentations when printing out code.

(defun alternative-fn-call (xp list)
  (if (> (length (symbol-name (car list))) 12)
      (funcall (pxp.format:formatter "~:<~1I~@{~W~^ ~_~}~:>") xp list)
      (funcall (pxp.format:formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>") xp list)))

(defun bind-list (xp list &rest args)
    (declare (ignore args))
  (if (do ((i 50 (1- i))
	   (ls list (cdr ls))) ((null ls) t)
	(when (or (not (consp ls)) (not (symbolp (car ls))) (minusp i))
	  (return nil)))
      (pxp.printer:pprint-fill xp list)
      (funcall (pxp.format:formatter "~:<~@{~:/pxp:pprint-fill/~^ ~_~}~:>") xp list)))

(defun block-like (xp list &rest args)
    (declare (ignore args))
  (funcall (pxp.format:formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>") xp list))

(defun defun-like (xp list &rest args)
    (declare (ignore args))
  (funcall (pxp.format:formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/pxp:pprint-fill/~^~@{ ~_~W~^~}~:>")
	   xp list))

(defun print-fancy-fn-call (xp list template)
  (let ((i 0) (in-first-section T))
    (pxp.printer::pprint-logical-block+ (xp list "(" ")" nil T nil)
      (pxp.printer:write+ (pxp.printer::pprint-pop) xp)
      (pxp.stream:pprint-indent+ :current 1 xp)
      (loop
	(pxp.printer::pprint-exit-if-list-exhausted)
	(pxp.stream:write-char++ #\space xp)
	(when (eq i (car template))
	  (pxp.stream:pprint-indent+ :block (cadr template) xp)
	  (setq template (cddr template))
	  (setq in-first-section nil))
	(pxp.printer:pprint-newline (cond ((and (zerop i) in-first-section) :miser)
			      (in-first-section :fill)
			      (T :linear))
			xp)
	(pxp.printer:write+ (pxp.printer::pprint-pop) xp)
	(incf i)))))

(defun maybelab (xp item &rest args)
    (declare (ignore args) (special need-newline indentation))
  (when need-newline (pxp.stream:pprint-newline+ :mandatory xp))
  (cond ((and item (symbolp item))
	 (pxp.printer:write+ item xp)
	 (setq need-newline nil))
	(T (pxp.stream:pprint-tab+ :section indentation 0 xp)
	   (pxp.printer:write+ item xp)
	   (setq need-newline T))))

(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))


;THE FOLLOWING STUFF SETS UP THE DEFAULT *PRINT-PPRINT-DISPATCH*

;This is an attempt to specify a correct format for every form in the CL book
;that does not just get printed out like an ordinary function call
;(i.e., most special forms and many macros).  This of course does not
;cover anything new you define.

(defun let-print (xp obj)
  (funcall (pxp.format:formatter "~:<~1I~W~^ ~@_~/pxp:bind-list/~^~@{ ~_~W~^~}~:>") xp obj))

(defun cond-print (xp obj)
  (funcall (pxp.format:formatter "~:<~W~^ ~:I~@_~@{~:/pxp:pprint-linear/~^ ~_~}~:>") xp obj))

(defun dmm-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun defsetf-print (xp list)
  (print-fancy-fn-call xp list '(3 1)))

(defun do-print (xp obj)
  (funcall
 (pxp.format:formatter "~:<~W~^ ~:I~@_~/pxp:bind-list/~^ ~_~:/pxp:pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
           xp obj))


(defun flet-print (xp obj)
  (funcall (pxp.format:formatter "~:<~1I~W~^ ~@_~:<~@{~/pxp:block-like/~^ ~_~}~:>~^~@{ ~_~W~^~}~:>")
	   xp obj))

(defun function-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (pxp.format:formatter "#'~W") xp (cadr list))
      (fn-call xp list)))

(defun mvb-print (xp list)
  (print-fancy-fn-call xp list '(1 3 2 1)))

(defun prog-print (xp list)
  (let ((need-newline T) (indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (pxp.format:formatter "~:<~W~^ ~:/pxp:pprint-fill/~^ ~@{~/pxp:maybelab/~^ ~}~:>")
	     xp list)))

(defun setq-print (xp obj)
  (funcall (pxp.format:formatter "~:<~W~^ ~:I~@_~@{~W~^ ~:_~W~^ ~_~}~:>") xp obj))

(defun quote-print (xp list)
  (if (and (consp (cdr list)) (null (cddr list)))
      (funcall (pxp.format:formatter "'~W") xp (cadr list))
      (pxp.printer:pprint-fill xp list)))

(defun tagbody-print (xp list)
  (let ((need-newline (and (consp (cdr list))
			   (symbolp (cadr list)) (cadr list)))
	(indentation (1+ (length (symbol-name (car list))))))
    (declare (special need-newline indentation))
    (funcall (pxp.format:formatter "~:<~W~^ ~@{~/pxp:maybelab/~^ ~}~:>") xp list)))

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
      (pxp.printer::pprint-logical-block (xp loop :prefix "(" :suffix ")")
	(let (token type)
	  (labels ((next-token ()
		     (pxp.printer::pprint-exit-if-list-exhausted)
		     (setq token (pxp.printer::pprint-pop))
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
		       (pxp.printer::pprint-logical-block (xp nil)
			 (pxp.printer::write first :stream xp)
			 (when (and skip-first-non-expr (not (eq type :expr)))
			   (write-char #\space xp)
			   (pxp.printer::write token :stream xp)
			   (next-token))
			 (when (eq type :expr)
			   (write-char #\space xp)
			   (pxp.printer:pprint-indent :current 0 xp)
			   (loop (pxp.printer::write token :stream xp)
				 (next-token)
				 (when (not (eq type :expr)) (return nil))
				 (write-char #\space xp)
				 (pxp.printer:pprint-newline newline-type xp))))))
		   (print-cond (xp)
		     (let ((first token))
		       (next-token)	;so always happens no matter what
		       (pxp.printer::pprint-logical-block (xp nil)
			 (pxp.printer::write first :stream xp)
			 (when (eq type :expr)
			   (write-char #\space xp)
			   (pxp.printer::write token :stream xp)
			   (next-token))
			 (write-char #\space xp)
			 (pxp.printer:pprint-indent :block 2 xp)
			 (pxp.printer:pprint-newline :linear xp)
			 (print-clause xp)
			 (print-and-list xp)
			 (when (and (symbolp token)
				    (string= (string token) "ELSE"))
			   (print-else-or-end xp)
			   (write-char #\space xp)
			   (pxp.printer:pprint-newline :linear xp)
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
			   (pxp.printer:pprint-newline :mandatory xp)
			   (pxp.printer::write token :stream xp)
			   (next-token)
			   (write-char #\space xp)
			   (print-clause xp)))
		   (print-else-or-end (xp)
		     (write-char #\space xp)
		     (pxp.printer:pprint-indent :block 0 xp)
		     (pxp.printer:pprint-newline :linear xp)
		     (pxp.printer::write token :stream xp)
		     (next-token)
		     (pxp.printer:pprint-indent :block 2 xp)))
	    (pxp.printer::pprint-exit-if-list-exhausted)
	    (pxp.printer::write (pxp.printer::pprint-pop) :stream xp)
	    (next-token)
	    (write-char #\space xp)
	    (pxp.printer:pprint-indent :current 0 xp)
	    (loop (print-clause xp)
		  (write-char #\space xp)
		  (pxp.printer:pprint-newline :linear xp)))))))

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
  (funcall (pxp.format:formatter "`~W") xp (bqtify obj)))

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
    (pxp.stream:write-string++ code xp 0 (length code))
    (pxp.printer:write+ (bq-struct-data obj) xp)))

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
	   (funcall (pxp.format:formatter "~[,~;,.~;,@~]~W")
		    output (sb-int:comma-kind comma)
		    (sb-int:comma-expr comma)))
	 (print-backquote (output backquote &rest noise)
           (declare (ignore noise))
	   (funcall (pxp.format:formatter "`~W")
		    output (cadr backquote))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member sb-int:quasiquote)) #'print-backquote 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch 'sb-impl::comma #'print-comma 0 pxp.dispatch:*IPD*)))

#+clisp
(eval-when (:load-toplevel :execute)
  (flet ((printer (prefix)
           (lambda (output exp &rest noise)
	     (declare (ignore noise))
	     (pxp.format:format output "~A~W" prefix (cadr exp)))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::backquote)) (printer "`") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::unquote)) (printer ",") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::nsplice)) (printer ",.") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member system::splice)) (printer ",@") 0 pxp.dispatch:*IPD*)))

#+ecl
(eval-when (:load-toplevel :execute)
  (flet ((printer (prefix)
           (lambda (output exp &rest noise)
	     (declare (ignore noise))
	     (pxp.format:format output "~A~W" prefix (cadr exp)))))
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:quasiquote)) (printer "`") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:unquote)) (printer ",") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:unquote-nsplice)) (printer ",.") 0 pxp.dispatch:*IPD*)
    (pxp.dispatch:set-pprint-dispatch '(cons (member si:unquote-splice)) (printer ",@") 0 pxp.dispatch:*IPD*)))

#+allegro
(eval-when (:load-toplevel :execute)
  (flet ((printer (prefix)
           (lambda (output exp &rest noise)
	     (declare (ignore noise))
	     (pxp.format:format output "~A~W" prefix (cadr exp)))))
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
(pxp.dispatch:set-pprint-dispatch 'cons #'pxp.printer:pprint-fill -10 pxp.dispatch:*IPD*)

(pxp.dispatch:set-pprint-dispatch '(cons (member pxp.format:defstruct)) #'block-like 0 pxp.dispatch:*IPD*)
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
    (pxp.printer::pprint-logical-block (xp stuff :prefix "#<" :suffix ">")
      (pxp.format:format xp (pxp.format:formatter "pprint dispatch table containing ~A entries: ")
	      (length stuff))
      (loop (pxp.printer::pprint-exit-if-list-exhausted)
	    (pxp.dispatch:show (pxp.printer::pprint-pop) xp)))))

(setf (get 'pxp.dispatch:pprint-dispatch 'pxp.printer::structure-printer) #'pprint-dispatch-print)

(pxp.dispatch:set-pprint-dispatch 'pxp.dispatch:pprint-dispatch #'pprint-dispatch-print 0 pxp.dispatch:*IPD*)


;so only happens first time is loaded.
(pxp.dispatch:initialize)

(defun install (&key (package *package*) (macro nil) (shadow T) (remove nil))
  (when (not (packagep package)) (setq package (find-package package)))
  (when (not remove)
    (when macro
      (set-dispatch-macro-character #\# #\" #'pxp.format::format-string-reader))
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
