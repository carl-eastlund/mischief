#lang racket

(provide
  discrim-sort
  byte-discrim
  natural-discrim
  integer-discrim
  char-discrim
  string-discrim
  sexp-discrim)

(struct ordered [key value] #:transparent)

(define-syntax-rule (eta-discrim discrim)
  (lambda (ords) (discrim ords)))

(define (self-ordered x)
  (ordered x x))

(define (key-map f xs)
  (for/list {[x (in-list xs)]}
    (match x
      [(ordered k v)
       (ordered (f k) v)])))

(define (key-split x)
  (match x
    [(ordered k v)
     (ordered k (ordered k v))]))

(define (key-partition key-pred xs)
  (define (pred x)
    (match x
      [(ordered k v)
       (key-pred k)]))
  (partition pred xs))

(define (discrim-sort discrim xs)
  (append* (discrim (map self-ordered xs))))

(define (byte-discrim ords)
  (define buckets (make-vector 256 '()))
  (for {[ord (in-list ords)]}
    (match ord
      [(ordered k v)
       (vector-set! buckets k
         (cons v (vector-ref buckets k)))]))
  (map reverse
    (filter-not empty?
      (vector->list buckets))))

(define (integer-discrim ords)
  (define-values {neg pos}
    (key-partition negative? ords))
  (append
    (reverse (natural-discrim (key-map - neg)))
    (natural-discrim pos)))

(define (sequence-discrim len ref elem-discrim
          #:least-significant-first? [least-significant-first? #false])
  (define (first-discrim ords index)
    (define (ref-index k) (ref k index))
    (elem-discrim (key-map ref-index ords)))
  (define (rest-discrim ords index)
    (inner-sequence-discrim ords (add1 index)))
  (define-values {before-discrim after-discrim}
    (if least-significant-first?
      (values rest-discrim first-discrim)
      (values first-discrim rest-discrim)))
  (define (inner-sequence-discrim ords [index 0])
    (cond
      [(empty? ords) '()]
      [else
       (define (small? x) (<= (len x) index))
       (define-values {small large} (key-partition small? ords))
       (define (ref-index k) (ref k index))
       (define pass1 (before-discrim (map key-split large) index))
       (define pass2
         (append*
           (for/list {[ords (in-list pass1)]}
             (after-discrim ords index))))
       (cons (map ordered-value small) pass2)]))
  inner-sequence-discrim)

(define (cons-discrim car-discrim cdr-discrim)
  (define (inner-cons-discrim ords)
    (define pass1 (car-discrim (key-map car (map key-split ords))))
    (define pass2
      (append*
        (for/list {[ords (in-list pass1)]}
          (cdr-discrim (key-map cdr ords)))))
    pass2)
  inner-cons-discrim)

(define (singleton-discrim ords)
  (list (map ordered-value ords)))

(define (map-discrim f discrim)
  (define (inner-map-discrim ords)
    (discrim (key-map f ords)))
  inner-map-discrim)

(define union-discrim
  (case-lambda
    [()
     (define (empty-union-discrim ords)
       (unless (empty? ords)
         (error 'union-discrim
           "illegal key: ~v"
           (ordered-key (first ords))))
       '())
     empty-union-discrim]
    [(discrim)
     (define (simple-union-discrim ords)
       (discrim ords))
     simple-union-discrim]
    [(discrim pred . other-variants)
     (define others-discrim
       (apply union-discrim other-variants))
     (define (inner-union-discrim ords)
       (cond
         [(empty? ords) '()]
         [else
          (define-values {these others}
            (key-partition pred ords))
          (append (discrim these) (others-discrim others))]))
     inner-union-discrim]))

(define (sexp-discrim . atom-discrims)
  (define (atom? x)
    (and (not (empty? x)) (not (cons? x))))
  (define atom-discrim
    (apply union-discrim atom-discrims))
  (define (inner-sexp-discrim ords)
    (the-discrim ords))
  (define the-discrim
    (union-discrim
      atom-discrim
      atom?
      singleton-discrim
      empty?
      (cons-discrim inner-sexp-discrim inner-sexp-discrim)
      cons?))
  inner-sexp-discrim)

(define (natural-length n)
  (quotient (+ (integer-length n) 7) 8))

(define (natural-ref n i)
  (define lo (* i 8))
  (define hi (+ lo 8))
  (bitwise-bit-field n lo hi))

(define char-discrim
  (map-discrim char->integer integer-discrim))

(define string-discrim
  (sequence-discrim string-length string-ref char-discrim))

(define natural-discrim
  (sequence-discrim natural-length natural-ref byte-discrim
    #:least-significant-first? #true))

(module+ test
  (require rackunit)

  (check-equal?
    (discrim-sort (sexp-discrim byte-discrim) '((1) (2 1) ()))
    '(() (1) (2 1)))

  (check-equal?
    (discrim-sort byte-discrim '(6 2 8 3 1))
    '(1 2 3 6 8))

  (check-equal?
    (discrim-sort integer-discrim '(2 257 65537 65793 65792 65536 256 1))
    '(1 2 256 257 65536 65537 65792 65793))

  (check-equal?
    (discrim-sort string-discrim '("cat" "cot" "car" "bat"))
    '("bat" "car" "cat" "cot"))

  (check-equal?
    (discrim-sort
      (sexp-discrim integer-discrim integer? string-discrim string?)
      '[(3 "." 1 4 1 5)
        3
        (3 "point" (1 4))
        "point"
        1
        (3 "point" 1 4)
        (3 "point" "landing")
        "."
        (3 "." 1 4)
        4])
    '[1 3 4 "." "point"
      (3 "." 1 4)
      (3 "." 1 4 1 5)
      (3 "point" 1 4)
      (3 "point" "landing")
      (3 "point" (1 4))]))
