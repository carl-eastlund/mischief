#lang scribble/manual

@(require (for-label mischief))

@title[#:tag "dye-pack"]{@racketmodname[mischief/dye-pack]: Protecting Syntax Objects}

@defmodule[mischief/dye-pack]

@defparam[current-dye-packs stx syntax?]{

Stores the syntax object used for arming and disarming dye packs in the current
syntax transformer.

}

@defproc[
(call-with-disarmed-dye-packs
  [stx syntax?]
  [proc (-> syntax? any)]
  [#:inspector inspector (or/c inspector? #false) #false])
any
]{

Calls @racket[proc] with a version of @racket[stx] whose dye packs are disarmed
using @racket[insp].  Stores the original @racket[stx] in
@racket[current-dye-packs] during the execution of @racket[proc].

Equivalent to:
@racketblock[
(parameterize {[current-dye-packs stx]}
  (proc (syntax-disarm stx inspector)))
]

}

@defform[
(with-disarmed-dye-packs name-id stx-expr body ...+)
]{

Executes @racket[body] with @racket[name-id] bound to a disarmed version of
@racket[stx-expr] using the declaration inspector for the enclosing module.
Stores the value of the original @racket[stx-expr] in
@racket[current-dye-packs] during the execution of @racket[proc].

Equivalent to:
@racketblock[
(call-with-disarmed-dye-packs stx-expr
  #:inspector (module-code-inspector)
  (lambda {name-id} body ...))
]

}

@defproc[
(rearm-dye-packs [stx syntax?]
  [#:dye-packs dye-packs syntax? (current-dye-packs)]
  [#:mode mode any/c #false])
syntax?
]{

Produces a version of @racket[stx] in which the dye packs on @racket[dye-packs]
are rearmed.  If @racket[mode] is @racket[#false], always rearms the dye packs
for the whole syntax object.  If @racket[mode] is a non-symbol true value, the
dye packs are pushed to nested syntax objects in accordance with the
@racket['taint-mode] syntax property.  If @racket[mode] is a symbol, sets the
@racket['taint-mode] property of @racket[stx] to @racket[mode] and then rearms
according to it.

}

@defform[
(module-code-inspector)
]{

Produces the declaration inspector for the enclosing module.  Equivalent to:
@racketblock[
(variable-reference->module-declaration-inspector
  (#%variable-reference))
]

}
