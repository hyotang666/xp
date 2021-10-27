; vim: ft=lisp
(in-package :asdf)

(defsystem "xp"
  :depends-on
  (
   "uiop"			; Utilities implicitly depends on via asdf.
   "trivial-gray-streams"	; Wrapper of gray-streams.
   ;; "structure-ext"		; Macro converting defstruct to defclass.
   )
  :components ((:file "xp-code"))
  :in-order-to ((test-op (test-op "xp-test"))))
