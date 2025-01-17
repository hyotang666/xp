; vim: ft=lisp
(in-package :asdf)

(defsystem "pxp"
  :depends-on
  (
   "uiop"			; Utilities implicitly depends on via asdf.
   "trivial-gray-streams"	; Wrapper of gray-streams.
   ;; "structure-ext"		; Macro converting defstruct to defclass.
   (:feature :ccl "fare-quasiquote-extras") ; Portable backquote.
   )
  :components
  ((:module "adjustable-vector"
	    :pathname "adjustable-vector"
	    :components ((:file "adjustable-vector")))
   (:module "dispatch"
	    :pathname "dispatch"
	    :components ((:file "dispatch")))
   (:module "buffer"
	    :pathname "buffer"
	    :depends-on ("adjustable-vector")
	    :components ((:file "buffer")))
   (:module "queue"
	    :pathname "queue"
	    :depends-on ("adjustable-vector" "buffer")
	    :components ((:file "queue")))
   (:module "stack"
	    :pathname "stack"
	    :depends-on ("adjustable-vector")
	    :components ((:file "stack")))
   (:module "pool" :pathname "pool"
	    :components ((:file "pool")))
   (:module "stream" :pathname "stream"
	    :depends-on ("buffer" "stack" "queue" "dispatch" "pool")
	    :components ((:file "stream")))
   (:module "printer" :pathname "printer"
	    :depends-on ("stream" "dispatch" "buffer" "queue")
	    :components ((:file "printer")))
   (:module "format" :pathname "format"
	    :depends-on ("printer")
	    :components ((:file "format")))
   (:file "xp-code" :depends-on ("buffer" "dispatch" "queue" "stream")))
  :in-order-to ((test-op (test-op "xp-test"))))

