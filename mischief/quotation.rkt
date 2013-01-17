#lang racket/base

(provide
  quotation
  custom-quoter)

(require
  racket/mpair
  racket/function
  mischief/function
  srfi/67)

(define (quotation value #:custom [custom (const #f)])

  (define (q x)
    (cond
      [(custom x) => (lambda (f) (f q x))]
      [(boolean? x) x]
      [(char? x) x]
      [(number? x) x]
      [(string? x) (if (immutable? x) x `(string-copy ,x))]
      [(bytes? x) (if (immutable? x) x `(bytes-copy ,x))]
      [(symbol? x)
       (cond
         [(symbol-interned? x) `(quote ,x)]
         [(symbol-unreadable? x)
          (list 'string->unreadable-symbol
            (symbol->string x))]
         [else
          (list 'string->uninterned-symbol
            (symbol->string x))])]
      [(keyword? x) `(quote ,x)]
      [(null? x) 'empty]
      [(list? x)
       (list* 'list
         (for/list {[y (in-list x)]}
           (q y)))]
      [(mlist? x)
       (list* 'mlist
         (for/list {[y (in-mlist x)]}
           (q y)))]
      [(pair? x)
       (list 'cons
         (q (car x))
         (q (cdr x)))]
      [(mpair? x)
       (list 'mcons
         (q (mcar x))
         (q (mcdr x)))]
      [(vector? x)
       (list* (vector-constructor x)
         (for/list {[y (in-vector x)]}
           (q y)))]
      [(box? x)
       (list (box-constructor x)
         (q (unbox x)))]
      [(prefab-struct-key x) =>
       (lambda (key)
         (list* 'make-prefab-struct (q key)
           (for/list {[y (in-vector (struct->vector x) 1)]}
             (q y))))]
      [(hash? x)
       (list (hash-constructor x)
         (list* 'list
           (sort
             (for/list {[(k v) (in-hash x)]}
               (list 'cons (q k) (q v)))
             (<? default-compare))))]
      [(regexp? x)
       (if (pregexp? x)
         `(pregexp ,(object-name x))
         `(regexp ,(object-name x)))]
      [(syntax? x)
       (list 'syntax
         (syntax->datum x))]
      [(void? x) '(void)]
      [(eof-object? x) 'eof]
      [else `(quote ,x)]))

  (q value))

(define (custom-quoter pred proc)
  (lambda (x) (if (pred x) proc #f)))

(define (box-constructor b)
  (if (immutable? b) 'box-immutable 'box))

(define (vector-constructor v)
  (if (immutable? v) 'vector-immutable 'vector))

(define (hash-constructor h)
  (string->symbol
    (string-append
      (cond
        [(immutable? h) "make-immutable-"]
        [(hash-weak? h) "make-weak-"]
        [else "make-"])
      (cond
        [(hash-eq? h) "hasheq"]
        [(hash-eqv? h) "hasheqv"]
        [else "hash"]))))
