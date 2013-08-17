#lang racket/base

(provide
  check-duplicate-label
  label-id-representative
  label-identifier=?
  label-identifier-hash-code
  free-identifier-hash-code
  bound-identifier-hash-code
  make-immutable-label-id-table
  make-weak-label-id-table
  make-label-id-table)

(require
  racket/list
  racket/dict
  racket/match
  mischief/define
  mischief/transform)

(define (bound-identifier-hash-code id
          [hash-code eq-hash-code]
          #:phase [phase (syntax-local-phase-level)])
  (hash-code (syntax-e id)))

(define (label-identifier-hash-code id
          [hash-code eq-hash-code]
          #:phase [phase (syntax-local-phase-level)])
  (hash-code (syntax-e id)))

(define (free-identifier-hash-code id
          [hash-code eq-hash-code]
          #:phase [phase (syntax-local-phase-level)])
  (hash-code (identifier-binding-symbol id phase)))

(define-if-unbound (identifier-binding-symbol id
                     [phase (syntax-local-phase-level)])
  (match (identifier-binding id phase)
    [(list _ sym _ _ _ _ _) sym]
    [_ (syntax-e id)]))

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
    (to-syntax #:context original-id
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
