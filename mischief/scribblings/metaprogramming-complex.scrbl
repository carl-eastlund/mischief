#lang scribble/manual

@title[#:style '(toc)]{Advanced Tools for Writing Macros}

The modules in this section provide tools for writing complex macros, such as
those that use @racket[local-expand].

@include-section["scope.scrbl"]

@include-section["dye-pack.scrbl"]

@include-section["id-table.scrbl"]

@include-section["kernel-syntax.scrbl"]

@section[#:tag "preserve-expensive-metadata"]{
@racketmodname[mischief/preserve-expensive-metadata]:
Compiled Syntax Objects with Source Locations and Syntax Properties}
@require[(for-label mischief/preserve-expensive-metadata)]
@defmodule[mischief/preserve-expensive-metadata]

@defform[(quote-syntax/preserve-expensive-metadata e)]{

Similar to @racket[(quote-syntax e)], but preserves the source location and
syntax properties of the original syntax even when compiled.

}
