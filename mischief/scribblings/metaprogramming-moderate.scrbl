#lang scribble/manual

@title[#:style '(toc)]{General-Purpose Tools for Writing Macros}

The modules in this section provide tools that are useful for writing many
kinds of macros that grow beyond simple one-liners.

@include-section["transform.scrbl"]

@section{parse}
@require[(for-label mischief/parse)]
@defmodule[mischief/parse]

@section{srcloc}
@require[(for-label mischief/srcloc)]
@defmodule[mischief/srcloc]

@section{location}
@require[(for-label mischief/location)]
@defmodule[mischief/location]
