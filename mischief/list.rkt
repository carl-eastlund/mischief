#lang racket/base

(provide
  make-alist build-alist map-map
  member? memv? memq?
  partition*
  take-while drop-while
  take-until drop-until
  sort/unique)

(require
  racket/list
  racket/match
  racket/function)

(define (partition* f . xss)
  (define-values {yes no}
    (let loop {[xss xss]}
      (cond
        [(andmap empty? xss)
         (values xss xss)]
        [(andmap cons? xss)
         (define xs (map first xss))
         (define ? (apply f xs))
         (define-values {yes no}
           (loop (map rest xss)))
         (if ?
           (values (map cons xs yes) no)
           (values yes (map cons xs no)))]
        [else (error 'partition* "given lists have different lengths")])))
  (apply values
    (append yes no)))

(define (map-map f . xs^3)
  (apply map
    (lambda xs^2
      (apply map f xs^2))
    xs^3))

(define (make-alist keys value)
  (for/list {[key (in-list keys)]}
    (cons key value)))

(define (build-alist keys key->value)
  (for/list {[key (in-list keys)]}
    (cons key (key->value key))))

(define (member? x ys [equiv? equal?])
  (for/or {[y (in-list ys)]}
    (equiv? x y)))

(define (memv? x ys)
  (for/or {[y (in-list ys)]}
    (eqv? x y)))

(define (memq? x ys)
  (for/or {[y (in-list ys)]}
    (eq? x y)))

(define (take-while pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       (cons (first xs)
         (take-while pred (rest xs)))
       empty)]))

(define (drop-while pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       (drop-while pred (rest xs))
       xs)]))

(define (take-until pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       empty
       (cons (first xs)
         (take-until pred (rest xs))))]))

(define (drop-until pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       xs
       (drop-until pred (rest xs)))]))

(define (sort/unique elements <?
          #:key [key identity]
          #:cache-keys? [cache-keys? #f])
  (let* {[elements (sort elements <? #:key key #:cache-keys? cache-keys?)]}
    (if (empty? elements)
      elements
      (let loop {[elem (first elements)]
                 [elements (rest elements)]}
        (if (empty? elements)
          (list elem)
          (if (<? elem (first elements))
            (cons elem
              (loop (first elements)
                (rest elements)))
            (loop elem (rest elements))))))))
