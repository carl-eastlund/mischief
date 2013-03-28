#lang scribble/manual

@(require
   mischief
   (submod mischief reflection)
   (for-label mischief))

@(define (mischief-phase-1)
   (modules->pre-content phase-1-exports))

@(define (mischief-phase-0)
   (modules->pre-content phase-0-exports))

@(define (modules->pre-content mods)
   (list->pre-content
     (map module->pre-content mods)))

@(define (module->pre-content mod)
   (racketmodname #,mod))

@(define (list->pre-content pcs)
   (match! pcs
     [(list one) one]
     [(list one two) (list one " and " two)]
     [(list head ... tail)
      (for/fold
          {[result (list "and " tail)]}
          {[pc (in-list (reverse head))]}
        (list* pc ", " result))]))

@title{Mischief: a Racketeer's Toolkit}

@author{Carl Eastlund}

@emph{Mischief} is a set of general-purpose utilities for programming and
meta-programming in Racket.

@defmodulelang[mischief]

The module @racketmodname[mischief] can be used as either a module language or
via @racket[require].

@margin-note{
The @racketmodname[mischief] library combines
@mischief-phase-0[] at phase 0 and
@mischief-phase-1[] at phase 1.
}

@include-section["debugging.scrbl"]

@include-section["printing.scrbl"]

@include-section["metaprogramming.scrbl"]

@section{Modules}

@subsection{@racketmodname[mischief/require]: Importing from Modules}
@require[(for-label mischief/require)]
@defmodule[mischief/require]

@defform[(dynamic-in mod-id)]{

Equivalent to @racket[(quote mod-id)] when used in @racket[require].

}

@defform[(require/provide require-spec ...)]{

Requires the @racket[require-spec]s and provides all the resulting bindings.
When each @racket[require-spec] is a module path, equivalent to:
@racketblock[
(begin
  (require require-spec ...)
  (provide (all-from-out require-spec ...)))
]

}

@defform[(quote-require require-spec ...)]{

Produces a list of symbols representing the names imported by the
@racket[require-spec]s.

}

@subsection{@racketmodname[mischief/module]: Module Paths and Names}
@require[(for-label mischief/module)]
@defmodule[mischief/module]

@defproc[
(resolve-module-path [mod-path module-path?]
  [#:namespace namespace namespace? (current-namespace)]
  [#:module-name-resolver resolver
   (-> module-path? 
       (or/c resolved-module-path? #false)
       (or/c syntax? #false)
       boolean?
       resolved-module-path?)
   (current-module-name-resolver)]
  [#:relative-to relative-to (or/c resolved-module-path? #false) #false]
  [#:syntax-context syntax-context (or/c syntax? #false) #false]
  [#:load? load? boolean? #true])
resolved-module-path?
]{

Resolves @racket[mod-path].  Equivalent to:
@racketblock[
(parameterize {[current-namespace namespace]}
  (resolver mod-path relative-to syntax-context load?))
]

}

@defproc[(module-name? [x any/c]) boolean?]{

Recognizes values that are valid results for
@racket[resolved-module-path-name].

}

@section{Definitions and Binding Forms}

@subsection{@racketmodname[mischief/define]: Definition Forms}
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

@subsection{@racketmodname[mischief/match]: Pattern Matching}
@require[(for-label mischief/match)]
@defmodule[mischief/match]

@deftogether[(
@defform[(match! expr [pat body ...+] ...)]
@defform[(match*! {expr ...} [{pat ...} body ...+] ...)]
)]{

Variants of @racket[match] and @racket[match*] that include source locations in
their error messages if no clauses match the inputs.

}

@section[#:style '(toc)]{First-Order Datatypes}

@subsection{list}
@require[(for-label mischief/list)]
@defmodule[mischief/list]

@subsection{string}
@require[(for-label mischief/string)]
@defmodule[mischief/string]

@subsection{symbol}
@require[(for-label mischief/symbol)]
@defmodule[mischief/symbol]

@subsection{keyword}
@require[(for-label mischief/keyword)]
@defmodule[mischief/keyword]

@subsection{boolean}
@require[(for-label mischief/boolean)]
@defmodule[mischief/boolean]

@subsection{struct}
@require[(for-label mischief/struct)]
@defmodule[mischief/struct]

@subsection{dict}
@require[(for-label mischief/dict)]
@defmodule[mischief/dict]

@subsection{maybe}
@require[(for-label mischief/maybe)]
@defmodule[mischief/maybe]

@subsection{quotation}
@require[(for-label mischief/quotation)]
@defmodule[mischief/quotation]

@subsection{sort}
@require[(for-label mischief/sort)]
@defmodule[mischief/sort]

@section[#:style '(toc)]{Functions and Flow Control}

@subsection{function}
@require[(for-label mischief/function)]
@defmodule[mischief/function]

@subsection{contract}
@require[(for-label mischief/contract)]
@defmodule[mischief/contract]

@subsection{values}
@require[(for-label mischief/values)]
@defmodule[mischief/values]

@subsection{for}
@require[(for-label mischief/for)]
@defmodule[mischief/for]

@subsection{memoize}
@require[(for-label mischief/memoize)]
@defmodule[mischief/memoize]

@subsection{fold}
@require[(for-label mischief/fold)]
@defmodule[mischief/fold]

@subsection{visitor}
@require[(for-label mischief/visitor)]
@defmodule[mischief/visitor]

@section[#:style '(toc)]{Scribble and Documentation}

@subsection{examples}
@require[(for-label mischief/examples)]
@defmodule[mischief/examples]

@section[#:style '(toc)]{Web Content}

@subsection{web}
@require[(for-label mischief/web)]
@defmodule[mischief/web]
