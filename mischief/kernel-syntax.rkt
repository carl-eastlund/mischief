#lang racket/base

(require
  racket/contract
  racket/list
  racket/dict
  racket/match
  syntax/id-table
  syntax/parse
  data/queue
  mischief/parse
  mischief/for
  mischief/dict)

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

(define (sort-kernel-syntax stxs #:cycles-ok? [cycles-ok? #false])

  ;; Definitions and names:

  (define name~>syntax
    (for*/dict! (make-free-id-table)
        {[stx (in-list stxs)]
         [id (in-list (kernel-syntax-bindings stx))]}
      (values id stx)))

  (define names
    (dict-keys name~>syntax))

  (define name->syntax
    (dict->procedure name~>syntax))

  ;; Sorting sub-sequences of the input:

  (define syntax~>index
    (for/hasheq! {[stx (in-list stxs)] [i (in-naturals)]}
      (values stx i)))

  (define syntax->index
    (dict->procedure syntax~>index))

  ;; Traversing the dependency graph:

  (define syntax~>status (make-hasheq))

  (define output (make-queue))

  (define (visit stx [stack empty])
    (match (dict-ref syntax~>status stx 'todo)
      ['done (void)]
      ['pending (report-cycle! stx stack)]
      ['todo
       (dict-set! syntax~>status stx (if cycles-ok? 'done 'pending))
       (define ref-ids (kernel-syntax-references stx #:among names))
       (define triples
         (for/list {[id (in-list ref-ids)]}
           (define stx (name->syntax id))
           (define index (syntax->index stx))
           (list index id stx)))
       (define sorted-triples
         (sort triples < #:key first))
       (for {[triple (in-list sorted-triples)]}
         (define id (second triple))
         (define stx* (third triple))
         (visit stx* (cons (list id stx) stack)))
       (dict-set! syntax~>status stx 'done)
       (enqueue! output stx)]))

  (define (report-cycle! stx stack)
    (define cycle
      (for/list {[frame (in-list stack)]}
        (define id (first frame))
        (define stx* (second frame))
        #:final (eq? stx stx*)
        (syntax-e id)))
    (raise-syntax-error 'sort-kernel-syntax
      (format "found a cycle among the definitions of: ~s" cycle)
      stx))

  ;; Run:

  (for-each visit stxs)

  (queue->list output))

(define (kernel-syntax-bindings stx #:among [among #false])

  (define seen (make-free-id-table))

  (define (unseen? id)
    (not (dict-has-key? seen id)))

  (define keep
    (cond
      [among (list->free-id-table among)]
      [else #false]))

  (define (keep? id)
    (cond
      [among (dict-has-key? keep id)]
      [else #true]))

  (define output (make-queue))

  (define (record! id)
    (when (and (keep? id) (unseen? id))
      (dict-set! seen id #true)
      (enqueue! output id)))

  (define (visit stx)
    (syntax-parse stx
      #:literal-sets {kernel-literals}
      [(begin ~! e ...) (for-each visit (attribute e))]
      [(define-values ~! {x:id ...} . _) (for-each record! (attribute x))]
      [(define-syntaxes ~! {x:id ...} . _) (for-each record! (attribute x))]
      [_ (void)]))

  (visit stx)

  (queue->list output))

(define (kernel-syntax-references stx #:among [among #false])

  (define seen (make-free-id-table))

  (define (unseen? id)
    (not (dict-has-key? seen id)))

  (define keep
    (cond
      [among (list->free-id-table among)]
      [else #false]))

  (define (keep? id)
    (cond
      [among (dict-has-key? keep id)]
      [else #true]))

  (define output (make-queue))

  (define (extend d ids)
    (for/dict d {[id (in-list ids)]}
      (values id #true)))

  (define (visit stx [bound (make-immutable-free-id-table)])

    (define (unbound? id)
      (not (dict-has-key? bound id)))

    (define (record! id)
      (when (and (keep? id) (unbound? id) (unseen? id))
        (dict-set! seen id #true)
        (enqueue! output id)))

    (syntax-parse stx
      #:literal-sets {kernel-literals}

      [(begin ~! e ...)
       (for {[stx (in-list (attribute e))]}
         (visit stx bound))]

      [(define-values ~! {x:id ...} e)
       (visit (attribute e)
         (extend bound (attribute x)))]

      [(#%expression ~! e) (visit (attribute e) bound)]

      [(#%plain-lambda ~! args:formals body:expr ...+)
       (define bound* (extend bound (attribute args.formal-id)))
       (for {[stx (in-list (attribute body))]}
         (visit stx bound*))]

      [(case-lambda ~! [args:formals body:expr ...+] ...)
       (for {[ids (in-list (attribute args.formal-id))]
             [stxs (in-list (attribute body))]}
         (define bound* (extend bound ids))
         (for {[stx (in-list stxs)]}
           (visit stx bound*)))]

      [(if ~! test then else)
       (visit (attribute test) bound)
       (visit (attribute then) bound)
       (visit (attribute else) bound)]

      [(begin0 e0 e ...)
       (visit (attribute e0) bound)
       (for {[stx (in-list (attribute e))]}
         (visit stx bound))]

      [(let-values ~! {[(lhs ...) rhs] ...} body ...+)
       (for {[stx (in-list (attribute rhs))]}
         (visit stx bound))
       (define bound* (extend bound (append* (attribute lhs))))
       (for {[stx (in-list (attribute body))]}
         (visit stx bound*))]

      [(letrec-values ~! {[(lhs ...) rhs] ...} body ...+)
       (define bound* (extend bound (append* (attribute lhs))))
       (for {[stx (in-list (attribute rhs))]}
         (visit stx bound*))
       (for {[stx (in-list (attribute body))]}
         (visit stx bound*))]

      [(set! ~! x:id e)
       (record! (attribute x))
       (visit (attribute e) bound)]

      [(with-continuation-mark e1 e2 e3)
       (visit (attribute e1) bound)
       (visit (attribute e2) bound)
       (visit (attribute e3) bound)]

      [(#%plain-app e0 e ...)
       (visit (attribute e0) bound)
       (for {[stx (in-list (attribute e))]}
         (visit stx bound))]

      [(#%variable-reference x:id)
       (record! (attribute x))]

      [x:id (record! (attribute x))]

      [_ (void)]))

  (visit stx)

  (queue->list output))

(define (list->free-id-table ids)
  (for/dict! (make-free-id-table)
      {[id (in-list ids)]}
    (values id #true)))
