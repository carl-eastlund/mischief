#lang racket/base

(provide
  check-duplicate-label
  label-id-representative
  label-identifier=?
  label-identifier-hash-code
  make-immutable-label-id-table
  make-weak-label-id-table
  make-label-id-table)

(require
  racket/dict
  mischief/syntax/transform)

(define (check-duplicate-label ids)
  (define table (make-label-id-table))
  (for/or {[id (in-list ids)]}
    (cond
      [(dict-has-key? table id) id]
      [else
       (dict-set! table id #true)
       #false])))

(define (label-id-representative original-id)
  (define unbound-id
    (to-syntax #:context id
      unique-symbol))
  (define delta
    (make-syntax-delta-introducer unbound-id #false))
  (delta
    (to-syntax
      (syntax-e original-id))))

(define unique-symbol
  (string->uninterned-symbol
    "unique-unbound-name"))

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
