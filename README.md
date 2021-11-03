This repository is farked of [yitzchak/xp](https://github.com/yitzchak/xp).
The goal is to support asdf and current lisp implementations e.g. sbcl, ccl, clisp, ecl...

In order to support abcl who has a package named "XP" already,
the name is changed to "PXP" which stands in "Portable XP".
This is a kludge, so may rename again in the future.

## TODO
NOTE: Not "Critical bug" is the bug about indentation and newlining.
Such bugs do not break read/print identity.

### SBCL
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### CCL
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [ ] Passing the test.

#### Issue:
CCL's backquote is a reader macro.
And it generates an ordinary lisp operator instead of e.g. `lisp::backq-list` in cmucl.
With the original XP code, we can not support this ccl behavior.

For maximum portability, we may have to use [fare-quasiquote].

[fare-quasiquote]: https://github.com/fare/fare-quasiquote

### CLISP
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### ECL
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### Allegro
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [x] Passing the test.

### CMUCL
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

### ABCL
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [ ] Passing the test.

#### Issue
ABCL prints package prefix even if the symbol is accessible.
For details, see [issue](https://github.com/armedbear/abcl/issues/408).

### CLASP
* [ ] Loadable.
* [ ] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.
