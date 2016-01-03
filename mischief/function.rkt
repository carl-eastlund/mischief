#lang racket/base

(provide
  eta
  arg+
  arg+-right
  disjoin
  conjoin
  call
  keyword-call
  define/keywords
  lambda/keywords
  dynamic-wrap-procedure
  normalize-procedure-arity)

(require
  (for-syntax
    racket/base
    syntax/name
    syntax/parse
    mischief/parse)
  mischief/define
  racket/function
  racket/promise
  racket/private/norm-arity)

(define (dynamic-wrap-procedure proc wrap)
  (make-keyword-procedure
    (lambda (ks vs . args)
      (wrap
        (lambda ()
          (keyword-apply proc ks vs args))))))

(define-syntax-rule (eta proc)
  (make-eta (lambda () (#%expression proc))))

(define normalize-procedure-arity normalize-arity)

(define-syntax (define/keywords stx)
  (syntax-parse stx
    [(_ (name:id ks:id vs:id . args:formals) body:expr ...+)
     #'(define name (lambda/keywords (ks vs . args) body ...))]))

(define-syntax (lambda/keywords stx)
  (syntax-parse stx
    [(_ (ks:id vs:id . args:formals) body:expr ...+)
     #`(make-keyword-procedure
         #,(syntax-property
             #'(lambda (ks vs . args) body ...)
             'inferred-name
             (syntax-local-infer-name stx)))]))

(define (arg+-maker r?)
  (make-keyword-procedure
    (lambda (ks1 vs1 f . xs1)
      (make-keyword-procedure
        (lambda {ks2 vs2 . xs2}
          (define kvs
            (sort
              (append
                (map cons ks1 vs1)
                (map cons ks2 vs2))
              keyword<?
              #:key car))
          (keyword-apply f
            (map car kvs)
            (map cdr kvs)
            (if r?
              (append xs2 xs1)
              (append xs1 xs2))))))))

(define arg+ (arg+-maker #false))
(define arg+-right (arg+-maker #true))

(define (keyword-call f ks vs . xs)
  (keyword-apply f ks vs xs))

(define call
  (make-keyword-procedure
    (lambda (ks vs f . xs)
      (keyword-apply f ks vs xs))))

(define (make-eta ->proc)
  (let* {[memo (delay (->proc))]}
    (make-keyword-procedure
      (lambda (ks vs . xs)
        (keyword-apply (force memo) ks vs xs)))))

(define-if-unbound (disjoin . procs)
  (make-keyword-procedure
    (lambda (ks vs . xs)
      (for/or {[proc (in-list procs)]}
        (keyword-apply proc ks vs xs)))))

(define-if-unbound (conjoin . procs)
  (make-keyword-procedure
    (lambda (ks vs . xs)
      (for/and {[proc (in-list procs)]}
        (keyword-apply proc ks vs xs)))))
