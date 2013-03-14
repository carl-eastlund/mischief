#lang racket/base

(provide
  (contract-out
    [sort-kernel-syntax
     (->* {(listof syntax?)} {#:cycles-ok? boolean?}
       (listof syntax?))]
    [kernel-syntax-bindings
     (->* {syntax?} {#:among (or/c #false (listof identifier?))}
       (listof identifier?))]
    [kernel-syntax-references
     (->* {syntax?} {#:among (or/c #false (listof identifier?))}
       (listof identifier?))]))

(require
  racket/contract
  racket/dict
  syntax/id-table
  syntax/parse
  data/queue)

(define (sort-kernel-syntax stxs #:cycles-ok? [cycles-ok? #false])

  (define defns (make-free-id-table))
  (define status (make-hasheq))
  (define output (make-queue))

  (for {[stx (in-list stxs)]}
    (for {[id (in-list (kernel-syntax-bindings stx))]}
      (dict-set! defns id stx)))

  (define names (dict-keys id~>stx))

  (define (visit stx [stack empty])
    (match (dict-ref status stx 'todo)
      ['done (void)]))

  (for-each visit stxs)
  (queue->list queue))

(define (kernel-syntax-references stx #:among [among #false])

  (define (visit stx)
    (syntax-parse stx
      #:literal-sets {kernel-literals}
      [(begin ~! e ...) (for-each visit (attribute e))]
      [(define-values ~! {x ...} _) (for-each record! (attribute x))]
      [(define-syntaxes ~! {x ...} _) (for-each record! (attribute x))]
      [_ (void)]))

  (visit stx))
