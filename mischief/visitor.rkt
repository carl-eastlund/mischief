#lang racket/base

(provide
  visit
  visitor?
  visitor-combine
  make-default-visitor
  make-leaf-visitor
  make-visitor
  make-wrapper-visitor
  make-uniform-default-visitor
  make-uniform-leaf-visitor
  make-uniform-visitor
  make-uniform-wrapper-visitor
  make-memoizing-visitor
  map-visitor
  for-each-visitor
  map/reduce-visitor
  make-map-visitor
  make-for-each-visitor
  make-map/reduce-visitor)

(require
  racket/list
  racket/block
  racket/function
  racket/set
  mischief/list
  mischief/function
  mischief/struct
  mischief/memoize)

(define/keywords (visit ks vs the-visitor the-value . xs)

  (define (handle the-handlers the-value ks vs xs)
    (cond
      [(empty? the-handlers)
       (error 'visit "no handler for value: ~v" the-value)]
      [else
       (define/keywords (resume ks vs the-value . xs)
         (handle (rest the-handlers) the-value ks vs xs))
       (define/keywords (recur ks vs the-value . xs)
         (handle (visitor-handlers the-visitor) the-value ks vs xs))
       (keyword-apply (first the-handlers) ks vs resume recur the-value xs)]))

  (handle (visitor-handlers the-visitor) the-value ks vs xs))

;; (Visitor X Y ... Z) = (visitor (Listof (Handler X Y ... Z)))
;; (Handler X Y ... Z) = ((X Y ... -> Z) (X Y ... -> Z) X Y ... -> Z)
(struct visitor [handlers]
  #:property prop:procedure
  (procedure-rename visit 'visitor))

(define empty-visitor
  (visitor empty))

(define (visitor-combine . visitors)
  (visitor (append* (map visitor-handlers visitors))))

(define (make-wrapper-visitor handler)
  (visitor (list handler)))

(define (make-visitor predicate handler)
  (make-wrapper-visitor
    (lambda/keywords {ks vs resume recur value . xs}
      (if (predicate value)
        (keyword-apply handler ks vs recur value xs)
        (keyword-apply resume ks vs value xs)))))

(define (make-leaf-visitor predicate handler)
  (make-visitor predicate
    (lambda/keywords {ks vs recur value . xs}
      (keyword-apply handler ks vs value xs))))

(define (make-default-visitor handler)
  (make-leaf-visitor (const #true) handler))

(define (make-uniform-wrapper-visitor handler)
  (make-wrapper-visitor
    (lambda/keywords {ks vs resume recur value . xs}
      (block
        (define (resume/uniform value)
          (keyword-apply resume ks vs value xs))
        (define (recur/uniform value)
          (keyword-apply recur ks vs value xs))
        (handler resume/uniform recur/uniform value)))))

(define (make-uniform-visitor predicate handler)
  (make-uniform-wrapper-visitor
    (lambda {resume recur value}
      (if (predicate value)
        (handler recur value)
        (resume value)))))

(define (make-uniform-leaf-visitor predicate handler)
  (make-uniform-visitor predicate
    (lambda {recur value}
      (handler value))))

(define (make-uniform-default-visitor handler)
  (make-uniform-leaf-visitor (const #true) handler))

(define atom?
  (disjoin
    null?
    number?
    symbol?
    keyword?
    string?
    bytes?
    char?
    boolean?
    void?
    eof-object?))

(define map-visitor
  (visitor-combine
    (make-uniform-leaf-visitor atom? identity)
    (make-uniform-visitor pair?
      (lambda {recur pair}
        (cons (recur (car pair)) (recur (cdr pair)))))
    (make-uniform-visitor vector?
      (lambda {recur vec}
        (apply (vector-reconstructor vec)
          (map recur (vector->list vec)))))
    (make-uniform-visitor hash?
      (lambda {recur ht}
        ((hash-reconstructor ht)
         (for/list {[(k v) (in-hash ht)]}
           (cons (recur k) (recur v))))))
    (make-uniform-visitor set?
      (lambda {recur s}
        (apply (set-reconstructor s)
          (map recur (set->list s)))))
    (make-uniform-visitor box?
      (lambda {recur b}
        ((box-reconstructor b) (recur (unbox b)))))
    (make-uniform-visitor mpair?
      (lambda {recur mpair}
        (mcons (recur (mcar mpair)) (recur (mcdr mpair)))))
    (make-uniform-visitor syntax?
      (lambda {recur stx}
        (datum->syntax stx (recur (syntax-e stx)) stx stx stx)))
    (make-uniform-visitor prefab?
      (lambda {recur x}
        (apply prefab (prefab-key x)
          (map recur (prefab-fields x)))))
    (make-uniform-visitor transparent-struct?
      (lambda {recur x}
        (apply (transparent-struct-constructor x)
          (map recur (transparent-struct-fields x)))))))

(define for-each-visitor
  (visitor-combine
    (make-uniform-leaf-visitor atom? void)
    (make-uniform-visitor pair?
      (lambda {recur pair}
        (recur (car pair))
        (recur (cdr pair))))
    (make-uniform-visitor vector?
      (lambda {recur vec}
        (for {[x (in-vector vec)]}
          (recur x))))
    (make-uniform-visitor hash?
      (lambda {recur ht}
        (for {[(k v) (in-hash ht)]}
          (recur k) (recur v))))
    (make-uniform-visitor set?
      (lambda {recur s}
        (for {[x (in-set s)]}
          (recur x))))
    (make-uniform-visitor box?
      (lambda {recur b}
        (recur (unbox b))))
    (make-uniform-visitor mpair?
      (lambda {recur mpair}
        (recur (mcar mpair))
        (recur (mcdr mpair))))
    (make-uniform-visitor syntax?
      (lambda {recur stx}
        (recur (syntax-e stx))))
    (make-uniform-visitor prefab?
      (lambda {recur x}
        (for-each recur
          (prefab-fields x))))
    (make-uniform-visitor transparent-struct?
      (lambda {recur x}
        (for-each recur (transparent-struct-fields x))))))

(define map/reduce-visitor
  (visitor-combine
    (make-leaf-visitor atom?
      (lambda/keywords {ks vs atom map reduce}
        (keyword-call map ks vs atom)))
    (make-visitor pair?
      (lambda/keywords {ks vs recur pair map reduce}
        (keyword-call reduce ks vs
          (keyword-call recur ks vs (car pair) map reduce)
          (keyword-call recur ks vs (cdr pair) map reduce))))
    (make-visitor vector?
      (lambda/keywords {ks vs recur vec map reduce}
        (keyword-apply reduce ks vs
          (for/list {[x (in-vector vec)]}
            (keyword-call recur ks vs x map reduce)))))
    (make-visitor hash?
      (lambda/keywords {ks vs recur ht map reduce}
        (keyword-apply reduce ks vs
          (for/list {[(k v) (in-hash ht)]}
            (keyword-call reduce ks vs
              (keyword-call recur ks vs k map reduce)
              (keyword-call recur ks vs v map reduce))))))
    (make-visitor set?
      (lambda/keywords {ks vs recur s map reduce}
        (keyword-apply reduce ks vs
          (for/list {[x (in-set s)]}
            (keyword-call recur ks vs x map reduce)))))
    (make-visitor box?
      (lambda/keywords {ks vs recur b map reduce}
        (keyword-call recur ks vs (unbox b) map reduce)))
    (make-visitor mpair?
      (lambda/keywords {ks vs recur mpair map reduce}
        (keyword-call reduce ks vs
          (keyword-call recur ks vs (mcar mpair) map reduce)
          (keyword-call recur ks vs (mcdr mpair) map reduce))))
    (make-visitor syntax?
      (lambda/keywords {ks vs recur stx map reduce}
        (keyword-call recur ks vs (syntax-e stx) map reduce)))
    (make-visitor prefab?
      (lambda/keywords {ks vs recur x map reduce}
        (keyword-apply reduce ks vs
          (for/list {[x (in-list (prefab-fields x))]}
            (keyword-call recur ks vs x map reduce)))))
    (make-visitor transparent-struct?
      (lambda/keywords {ks vs recur x map reduce}
        (keyword-apply reduce ks vs
          (for/list {[x (in-list (transparent-struct-fields x))]}
            (keyword-call recur ks vs x map reduce)))))))

(define (make-map-visitor predicate constructor . accessors)
  (make-uniform-visitor predicate
    (lambda {recur x}
      (apply constructor
        (for/list {[f (in-list accessors)]}
          (recur (f x)))))))

(define (make-for-each-visitor predicate . accessors)
  (make-uniform-visitor predicate
    (lambda {recur x}
      (for {[f (in-list accessors)]}
        (recur (f x))))))

(define (make-map/reduce-visitor predicate . accessors)
  (make-visitor predicate
    (lambda/keywords {ks vs recur x map reduce}
      (keyword-apply reduce ks vs
        (for/list {[f (in-list accessors)]}
          (keyword-call recur ks vs (f x) map reduce))))))

(define (make-memoizing-visitor [memo (make-memo-table)])
  (make-wrapper-visitor
    (lambda/keywords {ks vs resume recur value . xs}
      (keyword-apply call/memoize ks vs memo resume value xs))))

(define (vector-reconstructor v)
  (if (immutable? v) vector-immutable vector))

(define (hash-reconstructor h)
  (cond
    [(immutable? h)
     (cond
       [(hash-eq? h) make-immutable-hasheq]
       [(hash-eqv? h) make-immutable-hasheqv]
       [(hash-equal? h) make-immutable-hash])]
    [(hash-weak? h)
     (cond
       [(hash-eq? h) make-weak-hasheq]
       [(hash-eqv? h) make-weak-hasheqv]
       [(hash-equal? h) make-weak-hash])]
    [else
     (cond
       [(hash-eq? h) make-hasheq]
       [(hash-eqv? h) make-hasheqv]
       [(hash-equal? h) make-hash])]))

(define (set-reconstructor s)
  (cond
    [(set-eq? s) seteq]
    [(set-eqv? s) seteqv]
    [(set-equal? s) set]))

(define (box-reconstructor b)
  (if (immutable? b) box-immutable box))
