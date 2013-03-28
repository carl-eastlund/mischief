#lang racket/base

(provide
  datum-fold
  default-fold-short-circuit
  default-fold-list
  default-fold-list*
  default-fold-vector
  default-fold-box
  default-fold-prefab
  default-fold-hash
  default-fold-syntax
  default-fold-other)

(require
  racket/promise
  mischief/for
  mischief/function)

(define (datum-fold x
          #:short-circuit [fold-short-circuit default-fold-short-circuit]
          #:list [fold-list default-fold-list]
          #:list* [fold-list* default-fold-list*]
          #:vector [fold-vector default-fold-vector]
          #:box [fold-box default-fold-box]
          #:prefab [fold-prefab default-fold-prefab]
          #:hash [fold-hash default-fold-hash]
          #:hash-eq [fold-hash-eq (arg+ fold-hash (hasheq))]
          #:hash-eqv [fold-hash-eqv (arg+ fold-hash (hasheqv))]
          #:hash-equal [fold-hash-equal (arg+ fold-hash (hash))]
          #:syntax [fold-syntax default-fold-syntax]
          #:other [fold-other default-fold-other])

  (define (outer-loop x)
    (define p (delay (inner-loop x)))
    (fold-short-circuit x (lambda () (force p))))

  (define (inner-loop x)
    (cond
      [(list? x) (fold-list (map outer-loop x))]
      [(pair? x)
       (let loop {[x x] [xs null]}
         (if (pair? x)
           (loop (cdr x) (cons (outer-loop (car x)) xs))
           (fold-list* (reverse xs) (outer-loop x))))]
      [(vector? x)
       (fold-vector
         (for/list {[y (in-vector x)]}
           (outer-loop y)))]
      [(box? x) (fold-box (outer-loop (unbox x)))]
      [(prefab-struct-key x) =>
       (lambda (key)
         (fold-prefab key
           (map outer-loop
             (cdr (vector->list (struct->vector x))))))]
      [(hash? x)
       (define/for/lists {ks vs} {[(k v) (in-hash x)]}
         (values k (outer-loop v)))
       (cond
         [(hash-eq? x) (fold-hash-eq ks vs)]
         [(hash-eqv? x) (fold-hash-eqv ks vs)]
         [(hash-equal? x) (fold-hash-equal ks vs)])]
      [(syntax? x)
       (fold-syntax
         (datum->syntax x (gensym) x x x)
         (outer-loop (syntax-e x)))]
      [else (fold-other outer-loop x)]))

  (outer-loop x))

(define (default-fold-short-circuit x f) (f))
(define (default-fold-list xs) xs)
(define (default-fold-list* xs y) (append xs y))
(define (default-fold-vector xs) (list->vector xs))
(define (default-fold-box x) (box x))
(define (default-fold-prefab k vs) (apply make-prefab-struct k vs))
(define (default-fold-hash ht ks vs) (foldl hash-set ht ks vs))
(define (default-fold-syntax stx v) (datum->syntax stx v stx stx stx))
(define (default-fold-other f x) x)
