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

@include-section["modular.scrbl"]

@include-section["bindings.scrbl"]

@include-section["datatypes.scrbl"]

@include-section["higher-order.scrbl"]

@include-section["typesetting.scrbl"]
