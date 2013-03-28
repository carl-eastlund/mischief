#lang scribble/manual

@(require (for-label mischief))

@title[#:tag "contract"]{@racketmodname[mischief/contract]: Contracts}

@defmodule[mischief/contract]

This module re-exports @racket[predicate/c] from
@racketmodname[racket/contract].

@defform[(define-flat-rec-contract name-id contract-expr ...)]{

Defines a flat, recursive contract named @racket[name-id] that accepts any of
the @racket[contract-expr] variants.

}

@defproc[(dict/c [key/c contract?] [val/c contract?]) contract?]{

Contract for dictionaries with keys contracted by @racket[key/c] and values
contracted by @racket[val/c].  Currently implemented as an impersonator
contract regardless of the values of @racket[key/c] and @racket[val/c].

}

@defthing[nat/c flat-contract?]{

An alias for @racket[natural-number/c].

}

@defproc[
(type-predicate/c
  [input/c contract? any/c]
  [#:strict? strict? boolean? #true]
  [#:super super (or/c predicate/c #false)])
contract?
]{

Produces a contract for predicate functions.  With no arguments, equivalent to
@racket[predicate/c].  The predicate's argument is contracted by
@racket[input/c].  If @racket[strict?] is @racket[#true], its result must be a
boolean; otherwise it can be any value.  If @racket[super] is a predicate, then
the contracted predicate must only accept values that @racket[super] also
accepts.

}
