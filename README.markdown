Mocker.el is a mocking framework for Emacs lisp.

Its single entry point, `mocker-let` provides an `let` like interface to
defining mock objects. Actually, `mocker-let` is a wrapper around `flet`, which
can be seen as a way to manually generate mocks.

## Usage

## Extensibility

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
  ...
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
