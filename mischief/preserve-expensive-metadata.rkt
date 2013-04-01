#lang racket/base

(provide
  quote-syntax/preserve-expensive-metadata)

(require
  (for-syntax
    racket/base
    syntax/parse
    syntax/srcloc
    mischief/transform
    mischief/fold
    mischief/boolean))

(begin-for-syntax

  (define (preserve-expensive-metadata x)
    (datum-fold x
      #:syntax preserve-syntax
      #:list preserve-list
      #:list* preserve-list*
      #:vector preserve-vector
      #:box preserve-box
      #:prefab preserve-prefab
      #:hash preserve-hash
      #:other preserve-atomic))

  (define (preserve-syntax stx e-stx)
    (preserve-properties stx
      #`(datum->syntax
          (quote-syntax #,(datum->syntax stx 'context))
          #,e-stx
          #,(quote-transformer (build-source-location-list stx)))))

  (define (preserve-properties p-stx e-stx)
    (for/fold
        {[stx e-stx]}
        {[key (in-list (syntax-property-symbol-keys p-stx))]}
      #`(syntax-property
          #,stx
          #,(quote-transformer key)
          #,(quote-transformer (syntax-property p-stx key)))))

  (define (preserve-hash ht ks vs)
    #`(#,(preserve-hash-type ht)
       #,(preserve-alist ks vs)))

  (define (preserve-hash-type ht)
    (cond!
      [(hash-eq? ht) #'make-immutable-hasheq]
      [(hash-eqv? ht) #'make-immutable-hasheqv]
      [(hash-equal? ht) #'make-immutable-hash]))

  (define (preserve-alist ks vs)
    (define/syntax-parse [k ...] ks)
    (define/syntax-parse [v ...] vs)
    #`(list (cons k v) ...))

  (define (preserve-list stxs) #`(list #,@stxs))
  (define (preserve-list* stxs stx) #`(list* #,@stxs #,stx))
  (define (preserve-vector stxs) #`(vector-immutable #,@stxs))
  (define (preserve-box stx) #`(box-immutable #,stx))
  (define (preserve-prefab k stxs) #`(make-prefab-struct (quote #,k) #,@stxs))
  (define (preserve-atomic rec x) (quote-transformer x)))

(define-syntax (quote-syntax/preserve-expensive-metadata stx)
  (syntax-parse stx
    [(_ term)
     (preserve-expensive-metadata (attribute term))]))
