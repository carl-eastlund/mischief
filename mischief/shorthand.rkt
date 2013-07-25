#lang racket/base

(provide
  define-id-shorthand
  define-shorthand
  define-aliases
  define-alias
  with-aliases)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse))

(define-syntax (define-id-shorthand stx)
  (syntax-parse stx
    [(_ name:id template)
     (syntax/loc stx
       (define-syntax name
         (let ([result #'template])
           (lambda (stx)
             (syntax-parse stx
               [(_ . args) (datum->syntax stx (cons result #'args) stx stx)]
               [_:id result])))))]))

(define-syntax (define-shorthand stx)
  (syntax-parse stx
    [(_ (name:id . pattern) template)
     (syntax/loc stx
       (define-syntax (name stx)
         (syntax-parse stx
           [(_ . pattern) (syntax/loc stx template)])))]
    [(_ name:id [pattern template] ...)
     (syntax/loc stx
       (define-syntax (name stx)
         (syntax-parse stx
           [pattern (syntax/loc stx template)]
           ...)))]))

(define-syntax (define-aliases stx)
  (syntax-parse stx
    [(_ [alias:id name:id] ...)
     (syntax/loc stx
       (define-syntaxes {alias ...}
         (values (make-rename-transformer #'name) ...)))]))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ alias:id name:id)
     (syntax/loc stx
       (define-syntax alias
         (make-rename-transformer #'name)))]))

(define-syntax (with-aliases stx)
  (syntax-parse stx
    [(_ {[alias:id name:id] ...} body:expr ...+)
     (syntax/loc stx
       (let-syntax {[alias (make-rename-transformer #'name)] ...}
         body ...))]))
