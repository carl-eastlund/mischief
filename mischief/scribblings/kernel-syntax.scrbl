#lang scribble/manual

@(require mischief/examples)

@title[#:tag "kernel-syntax"]{@racketmodname[mischief/kernel-syntax]:
Manipulating Fully Expanded Syntax}

@defmodule[mischief/kernel-syntax]

@(define-example-form kernel-syntax-examples mischief mischief/kernel-syntax)

@defproc[
(sort-kernel-syntax [stxs (listof syntax?)]
  [#:cycles-ok? cycles-ok? boolean? #false])
(listof syntax?)
]{

Sorts the fully expanded definitions and expressions in @racket[stxs]
topologically such that definitions occur before corresponding references.  The
sort is stable, meaning that where possible the elements are left in the same
order as they occur in @racket[stxs].  When @racket[cycles-ok?] is
@racket[#true], mutually recursive definitions are kept in their original order
with respect to each other.  When @racket[cycles-ok?] is @racket[#false],
@racket[sort-kernel-syntax] reports an error if @racket[stxs] contains mutually
recursive definitions.

@kernel-syntax-examples[
(sort-kernel-syntax
  (list
    #'(define-values {flatten}
        (#%plain-lambda {x}
          (#%plain-app flatten/append x (quote ()))))
    #'(define-values {flatten/append}
        (#%plain-lambda {x tail}
          (if (#%plain-app empty? x)
            tail
            (if (#%plain-app cons? x)
              (#%plain-app flatten/append (#%plain-app first x)
                (#%plain-app flatten/append (#%plain-app rest x)
                  tail))
              (#%plain-app cons x tail)))))
    #'(#%plain-app flatten
        (#%plain-app list 2 (#%plain-app list 2 3) 4))))
]

}

@defproc[
(kernel-syntax-bindings [stx syntax?]
  [#:among among (or/c (listof identifier?) #false) #false])
(listof identifier?)
]{

Returns the list of unique names defined by the fully expanded expression or
definition @racket[stx].  If @racket[among] is a list, the result is restricted
to names that are @racket[free-identifier=?] to elements of @racket[among].

@kernel-syntax-examples[
(kernel-syntax-bindings
  #'(define-values {flatten}
      (#%plain-lambda {x}
        (#%plain-app flatten/append x (quote ())))))
(kernel-syntax-bindings
  #'(define-values {flatten/append}
      (#%plain-lambda {x tail}
        (if (#%plain-app empty? x)
          tail
          (if (#%plain-app cons? x)
            (#%plain-app flatten/append (#%plain-app first x)
              (#%plain-app flatten/append (#%plain-app rest x)
                tail))
            (#%plain-app cons x tail)))))
  #:among (list #'sir-not-appearing-in-this-definition))
(kernel-syntax-bindings
  #'(#%plain-app flatten
      (#%plain-app list 2 (#%plain-app list 2 3) 4)))
]

}

@defproc[
(kernel-syntax-references [stx syntax?]
  [#:among among (or/c (listof identifier?) #false) #false])
(listof identifier?)
]{

Returns the list of unique names referred to by the fully expanded expression
or definition @racket[stx], not including built-in special forms or names
defined by or locally bound within @racket[stx].  If @racket[among] is a list,
the result is restricted to names that are @racket[free-identifier=?] to
elements of @racket[among].

@kernel-syntax-examples[
(kernel-syntax-references
  #'(define-values {flatten}
      (#%plain-lambda {x}
        (#%plain-app flatten/append x (quote ())))))
(kernel-syntax-references
  #'(define-values {flatten/append}
      (#%plain-lambda {x tail}
        (if (#%plain-app empty? x)
          tail
          (if (#%plain-app cons? x)
            (#%plain-app flatten/append (#%plain-app first x)
              (#%plain-app flatten/append (#%plain-app rest x)
                tail))
            (#%plain-app cons x tail))))))
(kernel-syntax-references
  #'(#%plain-app flatten
      (#%plain-app list 2 (#%plain-app list 2 3) 4))
  #:among (list #'flatten #'flatten/append))
]

}
