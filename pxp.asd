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
  :components ((:file "xp-code"))
  :in-order-to ((test-op (test-op "xp-test"))))

