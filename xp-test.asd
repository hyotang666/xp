; vim: ft=lisp
(in-package :asdf)

(defsystem :xp-test
  :depends-on
  (
   "xp"	; Pretty-Printer.
   )
  :components ((:file "xp-test"))
  :perform (test-op (o c) (symbol-call :xp-test :do-tests)))

