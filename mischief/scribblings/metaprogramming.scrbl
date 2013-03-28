#lang scribble/manual

@require[(for-label mischief)]

@title[#:style '(toc)]{Macros and Syntax Transformers}

There are a lot of different tools for writing macros in the
@racketmodname[mischief] collection.
@itemlist[

@item{For short definitions for simple macros,
@racketmodname[mischief/shorthand] provides a set of convenient definition
forms.}

@item{
For writing macros of moderate complexity, there are two modules:
@itemlist[

@item{@racketmodname[mischief/transform] provides functions that operate on
syntax objects; and}

@item{@racketmodname[mischief/parse] provides syntax classes and macros for use
with @racket[syntax-parse].}

]
}

@item{
For more complicated macros, there are some new abstractions:

@itemlist[

@item{@racketmodname[mischief/scope] provides tools for automatic management of
local expansion and internal definition contexts;}

@item{@racketmodname[mischief/dye-pack] provides tools for protecting syntax
objects;}

@item{@racketmodname[mischief/id-table] provides a comparison and dictionaries
 for identifiers based on their symbolic name and marks, but irrespective of
 their bindings;}

@item{@racketmodname[mischief/kernel-syntax] provides functions that operate on
fully expanded programs; and}

@item{@racketmodname[mischief/preserve-expensive-metadata] provides an
alternative to @racket[quote-syntax] that preserves source locations and
properties that are normally discarded by compilation.}
]
}

@item{
For debugging macros, @racketmodname[mischief/stepper] gives a programmatic
interface to the @seclink["top" #:doc '(lib
"macro-debugger/macro-debugger.scrbl")]{Macro Stepper}.
}

]

@include-section["metaprogramming-simple.scrbl"]

@include-section["metaprogramming-moderate.scrbl"]

@include-section["metaprogramming-complex.scrbl"]

@section{stepper}
@require[(for-label mischief/stepper)]
@defmodule[mischief/stepper]
