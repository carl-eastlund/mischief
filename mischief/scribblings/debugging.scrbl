#lang scribble/manual

@(require mischief/examples)

@title[#:style '(toc)]{Structured Debugging}

The @racketmodname[debug] collection provides debugging utilities that
automatically trace flow into and out of debugged portions of the program and
format debugging output to reflect that call stack.  The collection includes
the languages @racketmodname[debug/racket] and @racketmodname[debug/mischief]
that automatically add debugging to primitive forms.  The
@racketmodname[no-debug] collection includes variants of everything in
@racket[debug] that execute identically but disable debugging output, so
turning debugging on or off simply means replacing @racket[(require debug)]
with @racket[(require no-debug)] or vice-versa.

@examples/evaluator[mischief #:requires [debug]
(define/debug (sum xs)
  (debug* cond
    [(empty? (rest xs)) (first xs)]
    [else (debug + (first xs) (sum (rest xs)))]))
(debug sum (build-list 3 add1))
(debug sum (build-list 0 add1))
]

@include-section["debug.scrbl"]
@include-section["no-debug.scrbl"]
