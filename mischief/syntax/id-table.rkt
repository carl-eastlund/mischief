#lang racket/base

(provide
  label-id-representative
  label-identifier=?
  label-identifier-hash-code
  make-immutable-label-id-table
  make-weak-label-id-table
  make-label-id-table)

(require
  racket/dict
  mischief/syntax/transform)

(define symbol-table
  (make-weak-hasheq))

(define (label-id-representative id)
  (to-syntax #:stx id
    (hash-ref! symbol-table (syntax-e id)
      (lambda ()
        (string->uninterned-symbol
          (symbol->string
            (syntax-e id)))))))

(define (label-identifier=? one two)
  (and (eq? (syntax-e one) (syntax-e two))
    (bound-identifier=?
      (label-id-representative one)
      (label-id-representative two))))

(define (label-identifier-hash-code id [rec eq-hash-code])
  (rec (syntax-e id)))

(define (make-immutable-label-id-table [init '()])
  (define table0
    (make-immutable-custom-hash
      label-identifier=?
      label-identifier-hash-code))
  (for/fold {[table table0]} {[(key value) (in-dict init)]}
    (dict-set table key value)))

(define (make-weak-label-id-table [init '()])
  (define table
    (make-weak-custom-hash
      label-identifier=?
      label-identifier-hash-code))
  (for {[(key value) (in-dict init)]}
    (dict-set! table key value))
  table)

(define (make-label-id-table [init '()])
  (define table
    (make-custom-hash
      label-identifier=?
      label-identifier-hash-code))
  (for {[(key value) (in-dict init)]}
    (dict-set! table key value))
  table)
