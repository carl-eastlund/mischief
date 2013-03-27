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
and @racket[case-lambda].  The class has five attributes: @racketid[arg-id],
@racketid[rest-id?], @racketid[rest-id], @racketid[formal-id], and
@racketid[call].

The attribute @racketid[arg-id] has a depth of 1 and contains all the
positional, non-rest formal argument identifiers.

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

@section{Literal Sets}
