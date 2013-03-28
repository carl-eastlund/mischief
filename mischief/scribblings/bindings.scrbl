#lang scribble/manual

@(require (for-label mischief))

@title{Definitions and Binding Forms}

@section{@racketmodname[mischief/define]: Definition Forms}
@require[(for-label mischief/define)]
@defmodule[mischief/define]

@defform[(at-end body:expr ...)]{

Lifts the @racket[body] definitions and expressions to the end of the enclosing
module.

}

@defform[(define-single-definition define-one-id define-many-id)]{

Defines @racket[define-one-id] in terms of @racket[define-many-id], where
@racket[define-many-id] defines multiple bindings similar to
@racket[define-values] and @racket[define-syntaxes], and
@racket[define-one-id] defines a single binding with shorthand for functions.

}

@deftogether[(
@defform*[(
(define-if-unbound id expr)
(define-if-unbound (id . formals) body ...+)
)]
@defform[(define-values-if-unbound {id ...} expr)]
@defform*[(
(define-syntax-if-unbound id expr)
(define-syntax-if-unbound (id . formals) body ...+)
)]
@defform[(define-syntaxes-if-unbound {id ...} expr)]
)]{

Variants of @racket[define], @racket[define-values], @racket[define-syntax],
and @racket[define-syntaxes] that define the given @racket[id]s if they have no
existing binding, and do nothing if the @racket[id]s are already bound.

}

@defform*[(
(define-provide-pre-syntax id expr)
(define-provide-pre-syntax (id . formals) body ...+)
)]{

Defines provide pre-transformers using @racket[define-syntax] and
@racket[make-provide-pre-transformer].

}

@defform[(define-unimplemented id ...)]{

Use this macro to define names that will be needed but are not yet
implemented.  Defines each @racket[id] as a macro such that any reference
raises an exception at run-time.

}

@defidform[unimplemented]{

A macro that expands any references to an expression that raises an exception
at run-time.

}

@defform[(unimplemented-out id ...)]{

A @racket[provide] form that exports each @racket[id] as a macro defined by
@racket[define-unimplemented].

}

@section{@racketmodname[mischief/match]: Pattern Matching}
@require[(for-label mischief/match)]
@defmodule[mischief/match]

@deftogether[(
@defform[(match! expr [pat body ...+] ...)]
@defform[(match*! {expr ...} [{pat ...} body ...+] ...)]
)]{

Variants of @racket[match] and @racket[match*] that include source locations in
their error messages if no clauses match the inputs.

}
