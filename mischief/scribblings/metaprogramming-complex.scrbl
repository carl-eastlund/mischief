#lang scribble/manual

@title[#:style '(toc)]{Advanced Tools for Writing Macros}

The modules in this section provide tools for writing complex macros, such as
those that use @racket[local-expand].

@include-section["scope.scrbl"]

@section{dye-pack}
@require[(for-label mischief/dye-pack)]
@defmodule[mischief/dye-pack]

@section{id-table}
@require[(for-label mischief/id-table)]
@defmodule[mischief/id-table]

@section{kernel-syntax}
@require[(for-label mischief/kernel-syntax)]
@defmodule[mischief/kernel-syntax]

@section{preserve-expensive-metadata}
@require[(for-label mischief/preserve-expensive-metadata)]
@defmodule[mischief/preserve-expensive-metadata]

