(defpackage :pxp.format (:use :cl)
  (:shadow format formatter defstruct)
  (:export format formatter defstruct))
(in-package :pxp.format)

;;;; SPECIAL VARIABLES.

(declaim (special *string* *used-args* *used-outer-args* *used-initial*
		  *get-arg-carefully* *inner-end* *outer-end* *at-top*)
	 (optimize speed)
	 (string *string*))

(defvar *xp-condition* 'failed-to-compile
  "The default condition for ERR.")

(defvar *format-string-cache* T)

;;;; DSL

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The macro DEF-FORMAT-HANDLER needs this eval-when.
  (defvar *fn-table* (make-hash-table) "used to access fns for commands. as { character:function }"))

(defmacro def-format-handler (char args &body body)
  (let ((name (intern (cl:format nil "FORMAT-~A" char) (find-package :pxp.format))))
    `(eval-when (:execute :load-toplevel :compile-toplevel)
       (defun ,name ,args
	 (declare (fixnum ,@args))
	 ,@body)
       (setf (gethash (char-upcase ,char) *fn-table*) #',name)
       (setf (gethash (char-downcase ,char) *fn-table*) #',name))))

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
		      (1+ (the (unsigned-byte 8) (error-point this)))))))

(defun failed-to-compile (id msg control-string i)
  (error (make-condition *xp-condition*
			 :id id
			 :control-string control-string
			 :message msg
			 :error-point i)))

;;;; UTILITIES.

(defmacro ref (string index)
  `(locally
     #+sbcl
     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (char ,string ,index)))

(declaim (ftype (function (string simple-string &key (:start (mod #.array-total-size-limit)))
			  (values (or null (mod #.array-total-size-limit)) &optional))
		position-not-in))
(defun position-not-in (string set &key start)
  #+sbcl ; Due to upgraded element type is unknown.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (position-if-not (lambda (c) (find c set)) string :start start))

;;;;

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
	         ((not (char= #\' (ref control-string position)))
		  position)
	         ((= (1+ position) end)
		  (failed-to-compile 2 "No character after '" control-string position))
	         (t
		   (rec (position-not-in control-string "+-0123456789,Vv#:@"
					 :start (+ 2 position)))))))
      (rec (position-not-in control-string "+-0123456789,Vv#:@" :start start)))))

(declaim (ftype (function (string (mod #.array-total-size-limit))
			  (values boolean &optional))
		colonp))
(defun colonp (control-string j) ;j points to directive name
  (or (char= #\: (ref control-string (1- j)))
      (and (char= #\@ (ref control-string (1- j)))
	   (char= #\: (ref control-string (- j 2))))))

(declaim (ftype (function (string (mod #.array-total-size-limit))
			  (values boolean &optional))
		atsignp))
(defun atsignp (control-string j) ;j points to directive name
  (or (char= #\@ (ref control-string (1- j)))
      (and (char= #\: (ref control-string (1- j)))
	   (char= #\@ (ref control-string (- j 2))))))

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
	  (if (not (char= #\/ (ref control-string j)))
	      (values i j)
	      (values i (or (position #\/ control-string :start (1+ j) :end end)
			    (failed-to-compile 3 "Matching / missing"
					       control-string
					       (position #\/ control-string :start start)))))))))

(declaim (ftype (function (string) (values boolean &optional)) fancy-directives-p))
(defun fancy-directives-p (control-string)
  (let ((end (length control-string)))
    (labels ((rec (i j)
               (when i
                 (let ((c (ref control-string j)))
                   (or (cond
			 ((find c "_Ii/Ww") nil)
			 ((find c ">Tt") (colonp control-string j))
			 (t nil))
                       (multiple-value-call #'rec
                         (next-directive1 control-string :start j :end end)))))))
      (multiple-value-call #'rec (next-directive1 control-string :start 0 :end end)))))

(declaim (ftype (function () (values (eql init) &optional)) initial))
(defun initial () (setq *used-initial* T) 'init)

(defun make-binding (var value body)
  `((let ((,var ,value)) ,@ body)))

(declaim (ftype (function () (values (eql args) &optional)) args))
(defun args () (setq *used-args* T) 'args)

(defmacro bind-initial (&body code)
  `(let* ((*used-initial* nil)
	  (body (progn ,@ code)))
     (if *used-initial* (make-binding 'init (args) body) body)))

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
      (let* ((directive (ref control-string j))
	     (close (cdr (assoc directive '((#\( . #\)) (#\[ . #\]) (#\< . #\>) (#\{ . #\}))))))
	(if (null close)
	  ;; Single directive. e.g. ~A, ~S etc.
	  (values directive i j)
	  ;; Paired directives.
	  (labels ((rec (ii k count)
		     (cond
		       ((null ii)
		        (failed-to-compile 4 "No matching close directive" control-string j))
		       ((char= (ref control-string k)
			       directive)
			;; Nest into.
		        (multiple-value-call #'rec
		          (next-directive1 control-string :start k :end end)
		          (1+ count)))
		       ((char= (ref control-string k) close)
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
	    (declare (ftype (function ((or null (mod #.array-total-size-limit))
				       (or null (mod #.array-total-size-limit))
				       fixnum))
			    rec))
	    (multiple-value-call #'rec
	      (next-directive1 control-string :start j :end end)
	      0)))))))

(declaim (ftype (function ((mod #.array-total-size-limit) (mod #.array-total-size-limit))
			  (values t &optional))
		literal))
(defun literal (start end &aux (start start))
  "Return an expression that write literal string."
  (let ((forms
	  (loop :for s = start :then (1+ sub-end)
		:for next-newline = (position #\newline *string* :start s :end end)
		:for sub-end = (or next-newline end)
		:when (< s sub-end)
		:collect (if (= s (1- sub-end))
			   `(pxp.stream:write-char++ ,(ref *string* s) xp)
			   `(pxp.stream:write-string++ ,(subseq *string* s sub-end) xp
					    ,0 ,(- sub-end s)))
		:when (null next-newline)
		  :do (loop-finish)
		:collect `(pxp.stream:pprint-newline+ :unconditional xp))))
    (if (null (cdr forms))
        (car forms)
	(cons 'progn forms))))

(declaim (ftype (function ((mod #.array-total-size-limit)
			   (mod #.array-total-size-limit))
			  (values list &optional))
		compile-format))
(defun compile-format (start end)
  "Return compiled S-Expression forms."
  (let ((start start)
	(result nil))
    (prog (c i j)
     label
     ;; Update vars.
     (multiple-value-setq (c i j) (next-directive *string* :start start :end end))
     ;; Update result.
     (when (if c (< start i) (< start end))
       (push (literal start (or i end)) result))
     (cond
       ;; Termination test.
       ((null c) (return (nreverse result)))
       ((char= c #\newline)
	(multiple-value-bind (colon atsign)
	  (parse-params (1+ i) nil :nocolonatsign T)
	  ;; Update result.
	  (when atsign (push `(pxp.stream:pprint-newline+ :unconditional xp) result))
	  ;; Update J.
	  (incf j)
	  (unless colon
	    (setq j (or (locally
			  #+sbcl
			  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
			  (position-if-not
			    (lambda (c) (or (char= c #\tab) (char= c #\space)))
			    *string* :start j :end end))
			end)))
	  ;; Update start.
	  (setq start j)))
       (t
	 (let ((fn (or (gethash c *fn-table*)
		       (failed-to-compile 5 "Unknown format directive" *string* j))))
	   ;; Update result.
	   (push (funcall (coerce fn 'function) (1+ i) (incf j)) result)
	   ;; Update START.
	   (setq start j))))
     ;; Looping
     (go label))))

(declaim (ftype (function (string string) (values cons &optional)) <formatter-fn>))
(defun <formatter-fn> (*string* *default-package*)
  (handler-case `(apply #'pxp.stream:call-with-xp-stream
			  (lambda (xp &rest args)
			    ,@(bind-initial
				`((block top
					 ,@(let ((*get-arg-carefully* nil)
						 (*at-top* t)
						 (*inner-end* 'top)
						 (*outer-end* 'top))
					     (compile-format 0 (length *string*))))))
			    (when ,(args)
			      (copy-list ,(args)))) ;needed by symbolics.
			s args)
    (failed-to-compile (c)
      (warn c)
      `(let ((*string* ,*string*))
	 (apply #'format s *string* args)))))

(defmacro formatter (string)
  `(lambda (s &rest args)
     ,(<formatter-fn> string (package-name *package*))))

(declaim (ftype (function (string boolean) (values (or string function) &optional))
		maybe-compile-format-string))
(defun maybe-compile-format-string (string force-fn?)
  (if (not (or force-fn? (fancy-directives-p string)))
      string
      (eval `(formatter ,string))))

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
		  (apply (coerce string-or-fn 'function) stream args))
		 ((pxp.stream:xp-structure-p stream)
		  (apply #'using-format stream string-or-fn args))
		 (T (apply #'cl:format stream string-or-fn args)))
	   nil)))


;Each of these functions expect to get called with two arguments
;start and end.  Start points to the first character after the ~
;marking the command.  End points to the first character after the
;command.  This includes the matching end command for paired commands.

;Definitions of the forms used in the code created by PARSE.
;Note these functions assume the stream is in the var XP and is an xp stream,

; INITIAL holds the initial value of ARGS (for ~@*).
;Initial is always bound to (args) if it is bound at all.
;Note this uses args, but only when actually binding


; ARGS holds the current argument list
;The val bound to args must always be computed (to use it up) even if args is not used.

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

;This is available for putting on #".

(defvar *default-package*)

(declaim (ftype (function (stream character t) (values cons &optional)) format-string-reader))
(defun format-string-reader (stream sub-char arg)
    (declare (ignore arg))
  (unread-char sub-char stream)
  `(lambda (s &rest args)
     ,(<formatter-fn> (read stream) (package-name *package*))))


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
      (if *at-top* `(pxp.printer::pprint-pop+top ,(args) xp) `(pxp.printer::pprint-pop+ ,(args) xp))
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
  #+sbcl ; Maybe due to &key.
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((colon nil) (atsign nil) (params nil) (i start) j)
    (flet ((parse-params ()
	     (loop :collect (case (ref *string* i)
			      ((#\V #\v) (incf i) (get-arg))
			      ((#\#) (incf i) (num-args))
			      ((#\') (incf i 2) (ref *string* (1- i)))
			      ((#\,) nil)
			      (otherwise
				(if (= i (setq j (position-not-in *string* "+-0123456789" :start i)))
				  (loop-finish))
				(prog1 (parse-integer *string* :start i :end j :radix 10.)
				  (setq i j))))
		   :if (char= (ref *string* i) #\,)
		     :do (incf i)
		     :else :do (loop-finish))))
      (do ((ps (parse-params) (cdr ps))
	   (ds defaults (cdr ds))
	   (nps nil))
	((null ds) (setq params (nreconc nps ps)))
	(push (cond ((or (null ps) (null (car ps))) (car ds))
		    ((not (consp (car ps))) (car ps))
		    (T `(cond (,(car ps)) (T ,(car ds)))))
	      nps)))
    ;; Error checks.
    (when (and max (< max (list-length params)))
      (failed-to-compile 6 "Too many parameters" *string* i))
    (loop (case (ref *string* i)
	    ((#\:) (when colon
		     (failed-to-compile 7 "Two colons specified" *string* i))
		   (setq colon T))
	    ((#\@) (when atsign
		     (failed-to-compile 8 "Two atsigns specified" *string* i))
		   (setq atsign T))
	    (otherwise (return nil)))
	  (incf i))
    (when (and colon nocolon)
      (failed-to-compile 9 "Colon not permitted" *string* i))
    (when (and atsign noatsign)
      (failed-to-compile 10 "Atsign not permitted" *string* i))
    (when (and colon atsign nocolonatsign)
      (failed-to-compile 11 "Colon and atsign together not permitted" *string* i))
    ;; Return values.
    (values colon atsign params)))


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
    (when (or (zerop end) (not (char= (ref *string* (1- end)) #\')))
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
    (declare ((mod #.array-total-size-limit) n))
    (loop
      (setq i (position-not-in *string* "+-0123456789," :start (1+ i)))
      (setq c (ref *string* i))
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
				   (char= #\: (ref *string* (1+ colon-pos))))
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

(defun using-format (xp string &rest args)
  (let ((result (apply #'cl:format nil string args)))
    (declare (string result))
    (pxp.stream:write-string+ result xp 0 (length result))))

(defun simple-directive (start end)
  (declare (fixnum start end))
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

(def-format-handler #\A (start end)
  (if (not (= end (1+ start)))
      (simple-directive start end)
      `(let ((*print-escape* nil))
	 (pxp.printer:write+ ,(get-arg) XP))))

(def-format-handler #\S (start end)
  (if (not (= end (1+ start)))
      (simple-directive start end)
      `(let ((*print-escape* T))
	 (pxp.printer:write+ ,(get-arg) XP))))

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

;Format directives that get open coded "P%&~|T*?^"

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
	  (declare (fixnum m))
	(when (minusp m)
	  (return nil))
	(when (eq ST L)
	  (return-from outer (values m n)))))))

(declaim (ftype (function ((mod #.array-total-size-limit)
			   list list)
			  (values list &optional))
		backup-to))
(defun backup-to (num list some-tail)
  (if (not pxp.stream:*circularity-hash-table*)
      (nthcdr num list)
      (multiple-value-bind (pos share) (tail-pos list some-tail)
	  (declare (ignore pos))
	(if (not (< num share))
	    (nthcdr num list)
	    (do ((L (nthcdr num list) (cdr L))
		 (n (- share num) (1- n))
		 (R nil (cons (car L) R)))
		((zerop n) (nreconc R L))
		(declare (fixnum n)))))))

(declaim (ftype (function ((mod #.array-total-size-limit) list list)
			  (values list &optional))
		backup-in-list))
(defun backup-in-list (num list some-tail)
  (backup-to (- (tail-pos list some-tail) num) list some-tail))

(def-format-handler #\P (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
  (let ((arg (if colon `(car (backup-in-list 1 ,(initial) ,(args))) (get-arg))))
    (if atsign
	`(if (not (eql ,arg 1)) (pxp.stream:write-string++ "ies" xp 0 3) (pxp.stream:write-char++ #\y xp))
	`(when (not (eql ,arg 1))
	   (pxp.stream:write-char++ #\s XP))))))

(declaim (ftype (function (stream t (mod #.most-positive-fixnum))
			  (values null &optional))
		multiple-newlines1))
(defun multiple-newlines1 (xp kind num)
  (loop :for i :of-type fixnum :below num
	:do (pxp.stream:pprint-newline+ kind xp)
	    (setq kind :unconditional)))

(defun multiple-newlines (start kind)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon T :noatsign T)
      (declare (ignore colon atsign))
    (if (eql (car params) 1)
        `(pxp.stream:pprint-newline+ ,kind xp)
	`(multiple-newlines1 xp ,kind ,(car params)))))

(def-format-handler #\% (start end) (declare (ignore end))
  (multiple-newlines start :unconditional))

(def-format-handler #\& (start end) (declare (ignore end))
  (multiple-newlines start :fresh))

(declaim (ftype (function (stream fixnum character)
			  (values null &optional))
		multiple-chars1))
(defun multiple-chars1 (xp num char)
  (loop :repeat num :do (pxp.stream:write-char++ char xp)))

(defun multiple-chars (start char)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon t :noatsign t)
      (declare (ignore colon atsign))
    (if (eql (car params) 1)
        `(pxp.stream:write-char++ ,char xp)
	`(multiple-chars1 xp ,(car params) ,char))))

(def-format-handler #\| (start end) (declare (ignore end))
  (multiple-chars start #.(ref (cl:format nil "~|") 0)))

(def-format-handler #\~ (start end) (declare (ignore end))
  (multiple-chars start #\~))

(def-format-handler #\T (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params) (parse-params start '(1 1))
    `(pxp.stream:pprint-tab+ ,(if colon
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

;because of backup-to, a prefix of some-tail may have been copied (in which
;case it cannot share anything with list), but there is a cons in some-tail
;that is in list.  This can be used to determine the position of some-tail
;relative to list.  However, we have to be careful, because they both could
;be cdr recursive.


(def-format-handler #\? (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil :nocolon t)
      (declare (ignore colon))
    (if (not atsign)
        `(apply #'format xp ,(get-arg) ,(get-arg))
	`(let ((fn (process-format-string ,(get-arg) T)))
	   (setq args (apply fn xp ,(args)))))))

(declaim (ftype (function ((mod #.array-total-size-limit)
			   &optional
			   (or null (mod #.array-total-size-limit))
			   (or null (mod #.array-total-size-limit)))
			  (values boolean &optional))
		do-complex-^-test))
(defun do-complex-^-test (a1 &optional (a2 nil) (a3 nil))
  (cond (a3 (and (<= a1 a2) (<= a2 a3)))
	(a2 (= a1 a2))
	(t (= 0 a1))))

(def-format-handler #\^ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 3 :noatsign t)
      (declare (ignore atsign))
    `(when ,(cond ((null params) `(null ,(if colon `(cdr ,(outer-args)) (args))))
		  (t `(do-complex-^-test ,@ params)))
       (return-from ,(if colon *outer-end* *inner-end*) nil))))

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
		      (else? (colonp *string* (1- (the fixnum (nth len chunks))))))
		 (declare (fixnum j))
		 `(case ,(if params (car params) (get-arg))
		    ,@(mapcar #'(lambda (unit)
				  (incf j)
				  `(,(if (and else? (= j len)) T j) ,@ unit))
			      innards))))))))

(def-format-handler #\( (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end *string* :start start)))
    (setq end (directive-start end))
    `(progn (pxp.stream:push-char-mode xp ,(cond ((and colon atsign) :UP)
				      (colon :CAP1)
				      (atsign :CAP0)
				      (T :DOWN)))
	    ,@(compile-format start end)
	    (pxp.stream:pop-char-mode xp))))

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

(defun num-args-in-directive (start end)
  (let ((n 0) c i j)
    (declare (fixnum n))
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

(defun handle-standard-< (start end)
  (declare (fixnum start end))
  `(using-format xp ,(subseq *string* (1- start) end)
		 ,@ (copy-tree (make-list (num-args-in-directive start end)
					  :initial-element (get-arg)))))

(declaim (ftype (function (boolean list) (values list &optional)) fill-transform))
(defun fill-transform (doit? body)
  (if (not doit?)
      body
      (mapcan #'(lambda (form)
		  (cond ((eq (car form) 'pxp.stream:write-string++)
			 (fill-transform-literal (cadr form)))
			((eq (car form) 'pxp.stream:write-char++)
			 (fill-transform-char (cadr form)))
			(T (list form))))
	      body)))

(defun handle-logical-block (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end *string* :start start)))
    (let* ((chunks (chunk-up start end))
	   (on-each-line?
	     (and (cddr chunks) (atsignp *string* (1- (the fixnum (cadr chunks))))))
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
	       `((pxp.printer:pprint-logical-block+ (xp ,(args) ,prefix ,suffix ,on-each-line?
					    ,(not (and *at-top* atsign)) ,atsign)
		   ,@(fill-transform (atsignp *string* (1- end))
		       (let ((*get-arg-carefully* T)
			     (*at-top* (and *at-top* atsign))
			     (*inner-end* 'pxp.printer::logical-block)
			     (*outer-end* 'pxp.printer::logical-block))
			 (compile-format (car chunks)
					 (directive-start (cadr chunks)))))))))))))

(def-format-handler #\< (start end)
  (if (colonp *string* (1- end))
      (handle-logical-block start end)
      (handle-standard-< start end)))

;The pretty-printing directives. "_IW<:>"

(def-format-handler #\_ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    `(pxp.stream:pprint-newline+ ,(cond ((and colon atsign) :mandatory)
			     (colon :fill)
			     (atsign :miser)
			     (T :linear)) XP)))

(def-format-handler #\I (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0) :noatsign T)
      (declare (ignore atsign))
    `(pxp.stream:pprint-indent+ ,(if colon :current :block) ,(car params) XP)))

(def-format-handler #\W (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (cond ((not (or colon atsign)) `(pxp.printer:write+ ,(get-arg) XP))
	  (T `(let (,@(when colon
			'((*print-pretty* T)))
		    ,@(when atsign
			'((*print-level* nil) (*print-length* nil))))
		(pxp.printer:write+ ,(get-arg) XP))))))




(declaim (ftype (function (character) (values cons &optional)) fill-transorm-char))
(defun fill-transform-char (char)
  (if (or (char= char #\space) (char= char #\tab))
      (list `(pxp.stream:write-char++ ,char xp) '(pxp.stream:pprint-newline+ :fill xp))
      `((pxp.stream:write-char++ ,char xp))))


(declaim (ftype (function (string) (values list &optional)) fill-transform-literal))
(defun fill-transform-literal (string)
  (flet ((white-space (c) (or (char= c #\space) (char= c #\tab))))
    (do ((index 0 end) (result) (end nil nil)) (nil)
      (let ((white (locally
		     #+sbcl
		     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
		     (position-if #'white-space string :start index))))
	(when white
	  (setq end (locally
		      #+sbcl
		      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
		      (position-if-not #'white-space string :start (1+ white)))))
	(when (null end)
	  (setq end (length string)))
	(push `(pxp.stream:write-string++ ,(subseq string index end) xp ,0 ,(- end index))
	      result)
	(when white
	  (push '(pxp.stream:pprint-newline+ :fill xp) result))
	(when (null white)
	  (return (nreverse result)))))))

;;;; DEFSTRUCT

(defun safe-assoc (item list)
  (do ((l list (cdr l))) ((not (consp l)) nil)
    (when (and (consp (car l)) (eq (caar l) item))
      (return (car l)))))

(defmacro defstruct (name &body body)
  (let* ((struct-name (if (consp name) (car name) name))
	 (printer (cadr (safe-assoc :print-function name)))
	 (xp-print-fn
	   (intern (concatenate 'string
		     "PRINT-" (package-name (symbol-package struct-name))
		     ":" (string struct-name))
		   (find-package :pxp))))
    (cond (printer
	   `(eval-when (:execute :load-toplevel :compile-toplevel)
	      (cl:defstruct ,name ,@ body)
	      (defun ,xp-print-fn (xp obj)
		(funcall #',printer obj xp pxp.dispatch:*current-level*))
	      (setf (get ',struct-name 'pxp.printer::structure-printer) #',xp-print-fn)
	      ',(if (consp name) (car name) name)))
	  ((and (not (safe-assoc :type name))
		(not (safe-assoc :include name)))
	   (let* ((conc-name-spec (safe-assoc :conc-name name))
		  (conc-name (cond ((null conc-name-spec)
				    (concatenate 'string (string struct-name) "-"))
				   ((null (cadr conc-name-spec)) "")
				   (T (locally
					#+sbcl
					(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
					(string (cadr conc-name-spec))))))
		  (slots (mapcar #'(lambda (x) (if (consp x) (car x) x)) body)))
	     `(eval-when (:execute :load-toplevel :compile-toplevel)
		(cl:defstruct ,name ,@ body)
		(defun ,xp-print-fn (xp obj)
		  (funcall (formatter "~@<#S(~;~W ~:I~@_~@{:~A ~W~^ ~:_~}~;)~:>") xp
			   ',struct-name
			   ,@(mapcan #'(lambda (slot)
					 `(,(symbol-name slot)
					    (,(intern (concatenate 'string
					                 conc-name (symbol-name slot)))
					      obj)))
				     slots)))
		(setf (get ',struct-name 'pxp.printer::structure-printer) #',xp-print-fn)
		',(if (consp name) (car name) name))))
	  (T `(eval-when (:execute :load-toplevel :compile-toplevel)
		(setf (get ',struct-name 'pxp.printer::structure-printer) :none)
		(cl:defstruct ,name ,@ body))))))

