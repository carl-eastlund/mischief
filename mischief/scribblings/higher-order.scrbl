#lang scribble/manual

@(require (for-label mischief))

@title[#:style '(toc)]{Functions and Flow Control}

@include-section["function.scrbl"]

@include-section["contract.scrbl"]

@section[#:tag "values"]{@racketmodname[mischief/values]: Multiple Values}
@require[(for-label mischief/values)]
@defmodule[mischief/values]

@defform[(values-of e)]{

Produces the values returned by the expression @racket[e] as a list.

}

@include-section["for.scrbl"]

@include-section["memoize.scrbl"]

@include-section["fold.scrbl"]

@include-section["visitor.scrbl"]
