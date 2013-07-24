#lang scribble/manual

@(require (for-label mischief syntax/kerncase))

@title[#:tag "scope"]{@racketmodname[mischief/scope]: Managing Internal Definitions}

@defmodule[mischief/scope]

@section{Scope Datatype}

@defproc[(scope? [x any/c]) boolean?]{

Recognizes scope values, which encapsulate pairs of internal definition
contexts and expansion contexts.

}

@defproc[
(scope-definition-context [sc scope?])
(or/c internal-definition-context? #false)
]{
Produces the internal definition context encapsulated by a scope.
}

@defproc[
(scope-expansion-context [sc scope?])
(or/c 'expression 'top-level 'module 'module-begin list?)
]{
Produces the expansion context encapsulated by a scope.
}

@defparam[current-scope sc scope?]{
The current scope used for expansion and local bindings.
}

@defproc[(new-scope [#:scope sc scope? (current-scope)]) scope?]{

Produces a scope encapsulating a fresh internal definition context and
expansion context, extending the ones in @racket[sc].

}

@defproc[(close-scope! [#:scope sc scope? (current-scope)]) scope?]{

Seals the internal definition context encapsulated by @racket[sc].

}

@defproc[
(call-with-new-scope
  [#:scope? sc scope? (current-scope)]
  [proc (-> scope? any)])
any
]{

Calls @racket[proc] with the result of
@racket[(new-scope #:scope sc)], and calls
@racket[(close-scope! #:scope sc)] when @racket[proc] returns.

}

@defform[(with-new-scope body ...+)]{

Executes the @racket[body] forms with @racket[current-scope] set to a fresh
scope.

}

@section{Local Bindings}

@deftogether[(

@defproc[
(scope-bind-value!
  [id identifier?]
  [#:scope sc scope? (current-scope)])
void?
]

@defproc[
(scope-bind-values!
  [ids (listof identifier?)]
  [#:scope sc scope? (current-scope)])
void?
]

@defproc[
(scope-bind-syntax!
  [id identifier?]
  [val any/c]
  [#:scope sc scope? (current-scope)])
void?
]

@defproc[
(scope-bind-syntaxes!
  [ids (listof identifier?)]
  [vals list?]
  [#:scope sc scope? (current-scope)])
void?
]

@defproc[
(scope-bind-syntax/eval!
  [id identifier?]
  [expr syntax?]
  [#:scope sc scope? (current-scope)])
void?
]

@defproc[
(scope-bind-syntaxes/eval!
  [ids (listof identifier?)]
  [expr syntax?]
  [#:scope sc scope? (current-scope)])
void?
]

)]{

Bind one or more identifiers in @racket[sc].  The procedures
@racket[scope-bind-value!] and @racket[scope-bind-values!] bind the names of
values, while @racket[scope-bind-syntax!] and @racket[scope-bind-syntaxes!]
bind names as syntax to the provided values.  The procedures
@racket[scope-bind-syntax/eval!] and @racket[scope-bind-syntaxes/eval!] bind
names as syntax to one or more values produced by evaluating the provided
syntax object.

}

@defproc[
(scope-static-binding? [id identifier?] [#:scope sc scope? (current-scope)])
boolean?
]{

Reports whether @racket[id] is bound as syntax in @racket[sc].

}

@defproc[
(scope-static-value [id identifier?]
  [#:rename? rename? boolean? #true]
  [#:scope sc scope? (current-scope)]
  [#:success success (-> any/c any) identity]
  [#:failure failure (-> any)
   (lambda {}
     (wrong-syntax id "expected an identifier with a syntax binding"))])
any
]{

Extracts the value associated with @racket[id] in @racket[sc], if @racket[sc]
is bound as syntax.  Recursively follows rename transformers if
@racket[rename?] is @racket[#true].  If @racket[id] is bound as syntax, passes
its value to @racket[success]; otherwise invokes @racket[failure].

}

@section{Lexical Context}

@defproc[
(in-scope [stx syntax?] [#:scope sc scope? (current-scope)])
syntax?
]{

Adds the bindings encapsulated by @racket[sc] to the context of @racket[stx].

}

@defproc[
(out-of-scope [stx syntax?] [#:scope sc scope? (current-scope)])
syntax?
]{

Removes the bindings encapsulated by @racket[sc] from the context of
@racket[stx].

}

@defproc[
(scope-delta-introducer [id identifier?] [#:scope sc scope? (current-scope)])
(-> syntax? syntax?)
]{

Produces a delta introducer that applies any marks on @racket[id] that were not
on its original definition.

}

@section{Local Expansion and Evaluation}

@defproc[
(expand-in-scope [stx syntax?]
  [#:scope sc scope? (current-scope)]
  [#:stop-at stop (or/c (listof identifier?) #false) 
   (if (memq (scope-expansion-context sc)
         '(expression module-begin))
       #false
       '())])
syntax?
]{

Locally expands @racket[stx] using the definition and expansion
contexts encapsulated by @racket[sc].  If @racket[stop] is a list, expansion
stops when it encounters any identifier in @racket[stop] or any identifier in
@racket[(kernel-form-identifier-list)].  If @racket[stop] is @racket[#false],
expansion does not stop, and recurs into subforms.

@emph{Note:} the interpretation of @racket['()] and @racket[#false] in
@racket[stop] are reversed from that used in the third argument to
@racket[local-expand].  This change is intended to make the behavior of
@racket[stop] more uniform: any list results in head-expansion, while
@racket[#false] results in recursive expansion.

}

@defproc[
(expand-expression-in-scope [stx syntax?]
  [#:scope sc scope? (current-scope)]
  [#:stop-at stop (or/c (listof identifier?) #false) #false])
syntax?
]{

Like @racket[expand-in-scope], but sets the expansion context to
@racket['expression] while expanding.

}

@defproc[
(expand-in-scope-for-syntax [stx syntax?]
  [#:scope sc scope? (current-scope)]
  [#:stop-at stop (or/c (listof identifier?) #false) #false])
syntax?
]{

Like @racket[expand-in-scope], but expands compile-time expressions.

}

@defproc[
(eval-in-scope [stx syntax?] [#:scope sc scope? (current-scope)])
any
]{
Evaluates @racket[stx] at compile-time using the bindings in @racket[sc].
}
