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

@section{memoize}
@require[(for-label mischief/memoize)]
@defmodule[mischief/memoize]

@section{fold}
@require[(for-label mischief/fold)]
@defmodule[mischief/fold]

@section{visitor}
@require[(for-label mischief/visitor)]
@defmodule[mischief/visitor]
