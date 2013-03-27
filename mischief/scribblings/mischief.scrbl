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

@section[#:style '(toc)]{Modules}

@subsection{require}
@require[(for-label mischief/require)]
@defmodule[mischief/require]

@subsection{module}
@require[(for-label mischief/module)]
@defmodule[mischief/module]

@section[#:style '(toc)]{Definitions and Binding Forms}

@subsection{define}
@require[(for-label mischief/define)]
@defmodule[mischief/define]

@subsection{match}
@require[(for-label mischief/match)]
@defmodule[mischief/match]

@section[#:style '(toc)]{First-Order Datatypes}

@subsection{list}
@require[(for-label mischief/list)]
@defmodule[mischief/list]

@subsection{string}
@require[(for-label mischief/string)]
@defmodule[mischief/string]

@subsection{symbol}
@require[(for-label mischief/symbol)]
@defmodule[mischief/symbol]

@subsection{keyword}
@require[(for-label mischief/keyword)]
@defmodule[mischief/keyword]

@subsection{boolean}
@require[(for-label mischief/boolean)]
@defmodule[mischief/boolean]

@subsection{struct}
@require[(for-label mischief/struct)]
@defmodule[mischief/struct]

@subsection{dict}
@require[(for-label mischief/dict)]
@defmodule[mischief/dict]

@subsection{maybe}
@require[(for-label mischief/maybe)]
@defmodule[mischief/maybe]

@subsection{quotation}
@require[(for-label mischief/quotation)]
@defmodule[mischief/quotation]

@subsection{sort}
@require[(for-label mischief/sort)]
@defmodule[mischief/sort]

@section[#:style '(toc)]{Functions and Flow Control}

@subsection{function}
@require[(for-label mischief/function)]
@defmodule[mischief/function]

@subsection{contract}
@require[(for-label mischief/contract)]
@defmodule[mischief/contract]

@subsection{values}
@require[(for-label mischief/values)]
@defmodule[mischief/values]

@subsection{for}
@require[(for-label mischief/for)]
@defmodule[mischief/for]

@subsection{memoize}
@require[(for-label mischief/memoize)]
@defmodule[mischief/memoize]

@subsection{fold}
@require[(for-label mischief/fold)]
@defmodule[mischief/fold]

@subsection{visitor}
@require[(for-label mischief/visitor)]
@defmodule[mischief/visitor]

@section[#:style '(toc)]{Scribble and Documentation}

@subsection{examples}
@require[(for-label mischief/examples)]
@defmodule[mischief/examples]

@section[#:style '(toc)]{Web Content}

@subsection{web}
@require[(for-label mischief/web)]
@defmodule[mischief/web]
