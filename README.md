This repository is farked of [yitzchak/xp](https://github.com/yitzchak/xp).
The goal is to support asdf and current lisp implementations e.g. sbcl, ccl, clisp, ecl...

In order to support abcl who has a package named "XP" already,
the name is changed to "PXP" which stands in "Portable XP".
This is a kludge, so may rename again in the future.

## TODO
NOTE: Not "Critical bug" is the bug about indentation and newlining.
Such bugs do not break read/print identity.

### SBCL/2.2.4
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### CCL/1.12.1
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [ ] Passing the test.

#### Issue:
[fare-quasiquote] is supported.
Evaluate `(named-readtables:in-readtable :fare-quasiquote)` then
`(asdf:test-system :pxp :force t)`, all tests are passed.

[fare-quasiquote]: https://github.com/fare/fare-quasiquote

### CLISP/2.49
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### ECL/21.2.1
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### Allegro/10.1
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### CMUCL/21D
* [x] Loadable.
* [x] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.

#### Issue

```lisp
(lambda ()
  (let ((*print-escape* t))
    (let ((*print-escape* nil))
      (cl:write-to-string "foo"))))
#<Interpreted Function (LAMBDA () ...)>

(funcall *) => "foo"

(compile nil **) => #<Function "LAMBDA NIL" {...}>

(funcall *) => "\"foo\""
```

### ABCL/1.9.0
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### CLASP
* [x] Loadable.
* [ ] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.
