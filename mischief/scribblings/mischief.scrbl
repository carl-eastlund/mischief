#lang scribble/manual

@(require
   mischief
   (submod mischief reflection)
   (for-template mischief))

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

@section{General Purpose}

@subsection{define}
@defmodule[mischief/define]

@subsection{values}
@defmodule[mischief/values]

@subsection{error}
@defmodule[mischief/error]

@subsection{dict}
@defmodule[mischief/dict]

@subsection{for}
@defmodule[mischief/for]

@subsection{list}
@defmodule[mischief/list]

@subsection{contract}
@defmodule[mischief/contract]

@subsection{match}
@defmodule[mischief/match]

@subsection{function}
@defmodule[mischief/function]

@subsection{boolean}
@defmodule[mischief/boolean]

@subsection{maybe}
@defmodule[mischief/maybe]

@subsection{symbol}
@defmodule[mischief/symbol]

@subsection{keyword}
@defmodule[mischief/keyword]

@subsection{quotation}
@defmodule[mischief/quotation]

@subsection{phrase}
@defmodule[mischief/phrase]

@subsection{stylish}
@defmodule[mischief/stylish]

@subsection{string}
@defmodule[mischief/string]

@subsection{struct}
@defmodule[mischief/struct]

@subsection{visitor}
@defmodule[mischief/visitor]

@subsection{memoize}
@defmodule[mischief/memoize]

@subsection{require}
@defmodule[mischief/require]

@subsection{module}
@defmodule[mischief/module]

@subsection{sort}
@defmodule[mischief/sort]

@subsection{scope}
@defmodule[mischief/scope]

@subsection{parse}
@defmodule[mischief/parse]

@subsection{pack}
@defmodule[mischief/dye-pack]

@subsection{transform}
@defmodule[mischief/transform]

@subsection{shorthand}
@defmodule[mischief/shorthand]

@subsection{srcloc}
@defmodule[mischief/srcloc]

@subsection{location}
@defmodule[mischief/location]

@subsection{fold}
@defmodule[mischief/fold]

@subsection{table}
@defmodule[mischief/id-table]
