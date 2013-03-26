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

@title{Mischief: Tools for Racketeers}

@author{Carl Eastlund}

@defmodulelang[mischief]

@margin-note{
The @racketmodname[mischief] library combines
@mischief-phase-0[] at phase 0 and
@mischief-phase-1[] at phase 1.
}
