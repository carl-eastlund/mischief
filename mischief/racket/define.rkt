#lang racket/base

(provide
  at-end
  define-single-definition
  define-if-unbound
  define-values-if-unbound
  define-syntax-if-unbound
  define-syntaxes-if-unbound)

(require
  (for-syntax
    racket/base
    racket/list
    racket/match
    racket/syntax
    syntax/parse))

(define-syntax (at-end stx)
  (syntax-case stx ()
    [(_ e ...)
     (match (syntax-local-context)
       ['module
         (begin
           (syntax-local-lift-module-end-declaration
            (syntax/loc stx (begin e ...)))
           (syntax/loc stx (begin)))]
       [ctx (wrong-syntax stx
                          "can only be used in module context; got: ~s"
                          ctx)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition Generalization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-single-definition stx)
  (syntax-parse stx
    [(define-single-definition define-one define-many)
     #'(define-syntax (define-one stx)
         (syntax-parse stx
           [(_ (head . args) . body) #'(define-one head (lambda args . body))]
           [(_ name expr) #'(define-many [name] expr)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Potentially Redundant Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-many-if-unbound stx)
  (syntax-case stx []
    [(_ def [name ...] expr)
     (let* ([ids (syntax->list #'(name ...))])
       (for ([bad (in-list ids)] #:unless (identifier? bad))
         (wrong-syntax bad "expected an identifier"))
       (let*-values ([(bound unbound) (partition identifier-binding ids)])
         (cond
          [(null? bound) (syntax/loc stx (def [name ...] expr))]
          [(null? unbound) (syntax/loc stx (def [] (values)))]
          [else (wrong-syntax
                 stx
                 "conflicting definitions for ~s; none for ~s"
                 (map syntax-e bound)
                 (map syntax-e unbound))])))]))

(define-syntax-rule (define-values-if-unbound [name ...] expr)
  (define-many-if-unbound define-values [name ...] expr))

(define-single-definition define-if-unbound define-values-if-unbound)

(define-syntax-rule (define-syntaxes-if-unbound [name ...] expr)
  (define-many-if-unbound define-syntaxes [name ...] expr))

(define-single-definition define-syntax-if-unbound define-syntaxes-if-unbound)
