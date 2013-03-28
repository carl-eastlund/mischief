#lang scribble/manual

@(require mischief/examples)

@(define-example-form function-examples mischief)

@title[#:tag "function"]{@racketmodname[mischief/function]: Higher Order Functions}

@defmodule[mischief/function]

@deftogether[(
@defproc[(arg+ [proc procedure?] [arg any/c] ...) procedure?]
@defproc[(arg+-right [proc procedure?] [arg any/c] ...) procedure?]
)]{

Partially applies @racket[proc] to the given @racket[arg]s, which may include
keyword arguments.  The @racket[arg]s are added as either the leftmost or
rightmost arguments to @racket[proc], respectively.

@function-examples[
(define one-two (arg+ list 1 2))
(one-two 3 4)
(define sort< (arg+-right sort < #:cache-keys? #true))
(sort< '("aunt" "anteater" "and" "an" "ant") #:key string-length)
]

}

@defproc[(call [proc procedure?] [arg-or-keyword-arg any/c] ...) any]{

Applies @racket[proc] to the given @racket[arg-or-keyword-arg]s.
Equivalent to @racket[(proc arg-or-keyword-arg ...)].

@function-examples[
(map call (list add1 sub1 - /) (list 1 2 3 4))
(call sort '(one two three) string<? #:key symbol->string)
]

}

@defproc[
(keyword-call
  [proc procedure?]
  [ks (listof keyword?)]
  [vs list?]
  [x any/c]
  ...)
any
]{

Applies @racket[proc] to the positional arguments @racket[x ...] and keyword
arguments @racket[ks] with values @racket[vs].  Equivalent to
@racket[(keyword-apply proc ks vs (list x ...))].

@function-examples[
(keyword-call
  sort
  '(#:cache-keys? key)
  (list #false first)
  '([2 a] [1 b] [3 c])
  <)
]

}

@deftogether[(
@defproc[(conjoin [proc procedure?] ...) procedure?]
@defproc[(disjoin [proc procedure?] ...) procedure?]
)]{

Produce the conjunction or disjunction, respectively, of the given predicates.

@function-examples[
(define positive-integer? (conjoin positive? integer?))
(positive-integer? 1)
(positive-integer? -1)
(positive-integer? 1.5)
(define <=? (disjoin < =))
(<=? 1 2)
(<=? 1 1)
(<=? 2 1)
(define always-true (disjoin (const #true) error))
(always-true #:irrelevant 'keyword)
(define never-true (conjoin (const #false) error))
(never-true #:irrelevant 'keyword)
]

}

@defproc[
(dynamic-wrap-procedure
  [proc procedure?]
  [wrap (-> (-> any) any)])
procedure?
]{

Creates a procedure that invokes @racket[proc] with its given arguments, but in
a dynamic context established by @racket[wrap], such as the dynamic extent of
@racket[parameterize].

@function-examples[
(define (silent proc)
  (dynamic-wrap-procedure proc
    (lambda {thunk}
      (parameterize {[current-output-port (open-output-nowhere)]}
        (thunk)))))
(define silent-write (silent write))
(silent-write '(1 2 3))
(silent-write '(1 2 3) (current-output-port))
(with-output-to-string
  (lambda {}
    (silent-write "good night moon")))
]

}

@defproc[
(normalize-procedure-arity [arity procedure-arity?])
procedure-arity?
]{

Produces a canonical form for the representation of a procedure's arity.

@function-examples[
(normalize-procedure-arity (list 1 (arity-at-least 3) 0 4))
]

}

@defform[(eta proc-expr)]{

Eta-expands @racket[proc-expr]; in other words, produces a procedure that
behaves identically to @racket[proc-expr], except that @racket[proc-expr] is
not evaluated until the first time it is called.

Strictly speaking, eta-expansion would evaluate @racket[proc-expr] every time
it is called; the @racket[eta] macro memoizes @racket[proc-expr] itself.  It
does not memoize the results of the procedure @racket[proc-expr] returns.

@function-examples[
(define is-a-list?
  (disjoin empty?
    (conjoin cons?
      (compose (eta is-a-list?) rest))))
(is-a-list? '(1 2 3))
(is-a-list? '(1 2 3 . more))
]

}

@deftogether[(
@defform[(define/keywords (name-id keys-id vals-id . formals) body ...+)]
@defform[(lambda/keywords {keys-id vals-id . formals} body ...+)]
)]{

Define or construct, respectively, a procedure that accepts arbitrary keyword
arguments, binding the list of keywords to @racket[keys-id] and their
respective values to @racket[vals-id].

@function-examples[
(define/keywords (f keys vals . others)
  (list keys vals others))
(f #:x 1 2 3 #:y 4)
]

}
