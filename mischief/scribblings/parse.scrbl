#lang scribble/manual

@(require mischief/examples)

@(define-example-form parse-examples mischief (for-syntax mischief))

@title[#:tag "parse"]{@racketmodname[mischief/parse]: Tools for
@racket[syntax-parse]}
@defmodule[mischief/parse]

@section{Shorthand Macros}

@defform[(|@| attribute-id)]{
An alias for @racket[attribute].
}

@defform[(syntax-matches? expr pat ...)]{
Returns @racket[#true] if @racket[expr] produces syntax matched by any of the
patterns @racket[pat], or @racket[#false] otherwise.
}

@defform[(syntax-matcher pat ...)]{
Produces a predicate that recognizes syntax matched by any of the patterns
@racket[pat].
}

@defform[(syntax-class/c class-id)]{
Produces a contract that accepts syntax belonging to the syntax class named by
@racket[class-id].
}

@defform[(syntax-parse/c pat ...)]{
Produces a contract that accepts syntax matched by any of the patterns
@racket[pat].
}

@section{Syntax Classes}

@defidform[formals]{

A syntax class that parses formal arguments as used by @racket[#%plain-lambda]
and @racket[case-lambda].  The class has 6 attributes: @racketid[arg-id],
@racketid[rest], @racketid[rest-id?], @racketid[rest-id], @racketid[formal-id],
and @racketid[call].

The attribute @racketid[arg-id] has a depth of 1 and contains all the
positional, non-rest formal argument identifiers.

The attribute @racketid[rest] has a depth of 0 and contains the tail of the
(possibly improper) list of arguments: either a rest argument or @racket[()].

The attribute @racketid[rest-id?] has a depth of 0 and contains the rest
argument identifier if one is present, or @racket[#false] otherwise.

The attribute @racketid[rest-id] has a depth of 1.  If there is a rest argument,
the attribute contains just that identifier.  Otherwise the attribute is empty.

The attribute @racketid[formal-id] has a depth of 1 and contains the result of
appending @racketid[arg-id] with @racketid[rest-id].

The attribute @racketid[call] has a depth of 0.  It is bound to @racket[#%app]
if @racketid[rest-id] is empty and @racket[apply] otherwise.

@parse-examples[
(define (recursive stx)
  (syntax-parse stx
    [({~literal define} (name:id . args:formals) . _)
     #'(args.call name args.formal-id ...)]))
(recursive #'(define (print x port) ---etc---))
(recursive #'(define (printf fmt . args) ---etc---))
]
}

@defidform[kw-formals]{

A syntax class that parses formal arguments including keyword arguments and
optional arguments as used by @racket[lambda] and @racket[define].  The class
has 13 attributes: @racketid[req-id], @racketid[opt-id], @racketid[opt-expr],
@racketid[req-kw], @racketid[req-kw-id], @racketid[opt-kw],
@racketid[opt-kw-id], @racketid[opt-kw-expr], @racketid[rest],
@racketid[rest-id?], @racketid[rest-id], @racketid[formal-id], and
@racketid[call].

@parse-examples[
(syntax-parse
  '{a b [c "one"] [d "two"] #:w e #:x f #:y [g "three"] #:z [h "four"] . i}
  [args:kw-formals
   (stylish-println
     (list
       (|@| args.req-id)
       (|@| args.opt-id)
       (|@| args.opt-expr)
       (|@| args.req-kw)
       (|@| args.req-kw-id)
       (|@| args.opt-kw)
       (|@| args.opt-kw-id)
       (|@| args.opt-kw-expr)
       (|@| args.rest)
       (|@| args.rest-id?)
       (|@| args.rest-id)
       (|@| args.formal-id)
       (|@| args.call)))])
]

}

@defidform[for-clauses]{
Parses a sequence of clauses for a @racket[for]-like macro.  Has no attributes.

@parse-examples[
(define (f x)
  (syntax-parse x
    [c:for-clauses #'c]))
(f #'{[(k v) (in-dict (hash))] #:when (eq? k v)})
(f #'{[(k v) (in-dict (hash))] [k (in-naturals)]})
(f #'{[(k v) (in-dict (hash))] #:else "something"})
]

See @racket[for-body] for a more practical example.
}

@defidform[for-body]{
Parses the body of a @racket[for]-like macro.  Has two attributes:
@racketid[head] and @racketid[tail]. The attribute @racketid[head] has a depth
of 1 and contains the interleaved definitions, expressions, and break clauses
that form most of the body.  The attribute @racketid[tail] has a depth of 0 and
contains the final expression of the body.

@parse-examples[
(define-syntax (for/string-set! stx)
  (syntax-parse stx
    [(_ target:expr clauses:for-clauses . body:for-body)
     #'(let {[s target]}
         (for clauses
           body.head ...
           (define-values {i c} body.tail)
           (string-set! s i c)))]))
(define s (string-copy "fox"))
(for/string-set! s
    {[i (in-naturals)]
     [c (in-string "abcdefghijklmnopqrstuvwxyz")]}
  #:break (>= i (string-length s))
  (values i c))
s
]
}

@section{Literal Sets}
