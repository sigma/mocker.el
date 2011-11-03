Mocker.el is a mocking framework for Emacs lisp.

Its single entry point, `mocker-let` provides an `let` like interface to
defining mock objects. Actually, `mocker-let` is a wrapper around `flet`, which
can be seen as a way to manually generate mocks.

## Usage

Let's start with a simple example:

```lisp
(mocker-let ((foo (x y z)
                  ((:input '(1 2 3) :output 4)
                   (:input '(4 5 6) :output 10)))
             (bar (x)
                  ((:input '(42) :output 4))))
  (+ (foo 1 2 3)
     (foo 4 5 6)
     (bar 42)))
```

Each mock is defined in a function-style, and is associated with a set of
"records" that map expected inputs to desired outputs.

By default, the order of definition within a mock has to be respected by the
wrapped code, so that in this situation it would be an error to observe `(foo
4 5 6)` before `(foo 1 2 3)`.

```lisp
(mocker-let ((foo (x y z)
                  ((:input '(1 2 3) :output 4)
                   (:input '(4 5 6) :output 10)))
             (bar (x)
                  ((:input '(42) :output 4))))
  (+ (foo 4 5 6)
     (foo 1 2 3)
     (bar 42)))
```

In such a situation, you'll get a typed error with a message like
```
(mocker-record-error "Violated record while mocking `foo'. Expected input like: `(1 2 3)', got: `(4 5 6)' instead")
...
```

If order is not important, you can obtain the same effect as before by
specifying it:

```lisp
(mocker-let ((foo (x y z)
                  :ordered nil
                  ((:input '(1 2 3) :output 4)
                   (:input '(4 5 6) :output 10)))
             (bar (x)
                  ((:input '(42) :output 4))))
  (+ (foo 4 5 6)
     (foo 1 2 3)
     (bar 42)))
```


## Extensibility

Each record definition actually builds a `mocker-record` object, that's
responsible for checking the actual behavior. By providing alternative
implementations of those records, one can adapt the mocking to special needs.

As a quick proof of concept, an implementation of a stub is provided with the
class `mocker-stub-record` which casualy ignores any input and always emits the
same output:

```lisp
(mocker-let ((foo (x)
                  ((:record-cls 'mocker-stub-record :output 42))))
  (foo 12345))
```

Customized classes can be provided, that can even introduce a mini-language for
describing the stub. This can be achieved by overloading
`mocker-read-record` correctly.

In case the customized record class is meant to be used in many tests, it might
be more convenient to use a pattern like:

```lisp
(let ((mocker-mock-default-record-cls 'mocker-stub-record))
  (mocker-let ((foo (x)
                    ((:output 42)))
               (bar (x y)
                    ((:output 1))))
    (+ (foo 12345)
       (bar 5 14))))
```

## Comparison to other mocking solutions

* el-mock.el (http://www.emacswiki.org/emacs/EmacsLispMock)

  * el-mock.el uses a small DSL for recording behavior, which is great for
    conciseness. mocker.el instead uses regular lisp as much as possible, which
    is more flexible.

  * el-mock.el does not allow recording multiple behaviors (the same call will
    always return the same value).

## Examples

```lisp
;;; automatically answer some `y-or-n-p' questions
(mocker-let ((y-or-n-p (prompt)
                       ((:input '("Really?") :output t)
                        (:input '("I mean... for real?") :output nil))))
  ...)
```

```lisp
;;; blindly accept all `yes-or-no-p' questions
(mocker-let ((yes-or-no-p (prompt)
                          ((:record-cls mocker-stub-record :output t))))
  ...)
```

```lisp
;;; make `foo' generate the fibonacci suite, no matter how it's called
(mocker-let ((foo (x)
                  ((:input-matcher (lambda (x) t)
                    :output-generator (lexical-let ((x 0) (y 1))
                                        (lambda (any)
                                          (let ((z (+ x y)))
                                            (setq x y y z))))
                    :max-occur nil))))
  ...)
```
