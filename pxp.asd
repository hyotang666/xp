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
   (:module "buffer"
	    :pathname "buffer"
	    :depends-on ("adjustable-vector")
	    :components ((:file "buffer")))
   (:module "stack"
	    :pathname "stack"
	    :depends-on ("adjustable-vector")
	    :components ((:file "stack")))
   (:file "xp-code" :depends-on ("buffer")))
  :in-order-to ((test-op (test-op "xp-test"))))

