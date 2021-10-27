This repository is farked of [yitzchak/xp](https://github.com/yitzchak/xp).
The goal is to support asdf and current lisp implementations e.g. sbcl, ccl, clisp, ecl...

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

* Issue: CCL is seemed to be not able to print list `(function foo)`.

```lisp
(cl:write '(function foo) :pretty t) => #'FOO
(cl:write '(function foo) :pretty nil) => #'FOO
```

### CLISP
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [ ] Passing the test.

* Issue: CLISP is seemed to be not able to print list `(function foo)`.

```lisp
(cl:write '(function foo) :pretty t) => #'FOO
(cl:write '(function foo) :pretty nil) => #'FOO
```

### ECL
* [x] Loadable.
* [x] Testable.
* [x] Fixing critical bugs.
* [ ] Passing the test.

* Issue: ECL is seemed to be not able to print list `(function foo)`.

```lisp
(cl:write '(function foo) :pretty t) => #'FOO
(cl:write '(function foo) :pretty nil) => #'FOO
```

### Allegro
* [ ] Loadable.
* [ ] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.

### CMUCL
* [x] Loadable.
* [x] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.

* Issue

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
* Issue: ABCL already have the package named "XP".
* [ ] Loadable.
* [ ] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.

### CLASP
* [ ] Loadable.
* [ ] Testable.
* [ ] Fixing critical bugs.
* [ ] Passing the test.
