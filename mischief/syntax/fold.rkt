#lang racket/base

(provide
  datum-fold)

(require
  racket/promise)

(define (datum-fold x
          #:short-circuit [process-short-circuit default-process-short-circuit]
          #:list [process-list default-process-list]
          #:list* [process-list* default-process-list*]
          #:vector [process-vector default-process-vector]
          #:box [process-box default-process-box]
          #:prefab [process-prefab default-process-prefab]
          #:hash [process-hash default-process-hash]
          #:hash-eq [process-hash-eq (lambda (x) (process-hash (hasheq) x))]
          #:hash-eqv [process-hash-eqv (lambda (x) (process-hash (hasheqv) x))]
          #:hash-equal [process-hash-equal (lambda (x) (process-hash (hash) x))]
          #:syntax [process-syntax default-process-syntax]
          #:other [process-other default-process-other])

  (define (outer-loop x)
    (define p (delay (inner-loop x)))
    (process-short-circuit x (lambda () (force p))))

  (define (inner-loop x)
    (cond
      [(list? x) (process-list (map outer-loop x))]
      [(pair? x)
       (let loop {[x x] [xs null]}
         (if (pair? x)
           (loop (cdr x) (cons (outer-loop (car x)) xs))
           (process-list* (reverse xs) (outer-loop x))))]
      [(vector? x)
       (process-vector
         (for/list {[y (in-vector x)]}
           (outer-loop y)))]
      [(box? x) (process-box (outer-loop (unbox x)))]
      [(prefab-struct-key x) =>
       (lambda (key)
         (process-prefab key
           (map outer-loop
             (cdr (vector->list (struct->vector x))))))]
      [(hash? x)
       (define alist
         (for/list {[(k v) (in-hash x)]}
           (cons k (outer-loop v))))
       (cond
         [(hash-eq? x) (process-hash-eq alist)]
         [(hash-eqv? x) (process-hash-eqv alist)]
         [(hash-equal? x) (process-hash-equal alist)])]
      [(syntax? x)
       (process-syntax
         (datum->syntax x (gensym) x x x)
         (outer-loop (syntax-e x)))]
      [else (process-other x)]))

  (outer-loop x))

(define (default-process-short-circuit x f) (f))
(define (default-process-list xs) xs)
(define (default-process-list* xs y) (append xs y))
(define (default-process-vector xs) (list->vector xs))
(define (default-process-box x) (box x))
(define (default-process-prefab k vs) (apply make-prefab-struct k vs))
(define (default-process-hash ht alist)
  (for/fold {[ht ht]} {[p (in-list alist)]}
    (hash-set ht (car p) (cdr p))))
(define (default-process-syntax stx v) (datum->syntax stx v stx stx stx))
(define (default-process-other x) x)
