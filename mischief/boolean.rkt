#lang racket/base

(provide
  (struct-out exn:fail:cond!)
  cond!
  implies
  implies? and? or?
  xor iff
  cons/optional
  list/optional
  list*/optional)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    syntax/srcloc
    macro-debugger/emit)
  racket/bool
  racket/block
  mischief/define)

(define-syntax (cond! stx)

  (define-splicing-syntax-class cond!-default
    #:attributes {result}
    (pattern (~seq [{~literal else} ~! results:expr ...+])
      #:attr result #'(block results ...))
    (pattern (~seq)
      #:attr loc
      (datum->syntax #false
        (source-location->prefix stx))
      #:attr result
      #'(raise
          (exn:fail:cond!
            (format "~acond!: no clause succeeded" 'loc)
            (current-continuation-marks)))))

  (define-splicing-syntax-class cond!-clauses
    #:attributes {wrap}
    (pattern (~seq clause:cond!-clause ...)
      #:attr wrap (apply compose (attribute clause.wrap))))

  (define-syntax-class cond!-clause
    #:attributes {wrap}
    (pattern [test:test-expr {~literal =>} ~! fun:expr]
      #:attr var (generate-temporary)
      #:attr wrap
      (lambda (stx)
        #`(let {[var test]}
            (if var (fun var) #,stx))))
    (pattern [test:test-expr body:expr ...+]
      #:do {(when (eq? (syntax-e (attribute test)) 'else)
              (emit-remark
                "mismatched `else' literal"
                #'test
                #'else
                (format "phase: ~a" (syntax-local-phase-level))
                (if (free-identifier=? #'test #'else
                      (syntax-local-phase-level))
                  "same"
                  "diff"))
              (wrong-syntax (attribute test)
                "mismatched `else' literal"))}
      #:attr wrap
      (lambda (stx)
        #`(if test (block body ...) #,stx)))
    (pattern [test:test-expr]
      #:attr var (generate-temporary)
      #:attr wrap
      (lambda (stx)
        #`(let {[var test]}
            (if var var #,stx)))))

  (define-syntax-class test-expr
    (pattern (~and (~not {~literal else}) _:expr)))

  (syntax-parse stx
    [(_ clauses:cond!-clauses default:cond!-default)
     ((attribute clauses.wrap) (attribute default.result))]))

(struct exn:fail:cond! exn:fail [] #:transparent)

(define-syntax-if-unbound (implies stx)
  (syntax-parse stx
    [(_ a:expr b:expr)
     #'(if a b #true)]))

(define (implies? a b)
  (implies a b))

(define (and? . bs)
  (for/and {[b (in-list bs)]} b))

(define (or? . bs)
  (for/or {[b (in-list bs)]} b))

(define-if-unbound (xor a b)
  (if a
    (if b #f a)
    (if b b #f)))

(define (iff a b)
  (if a
    (if b #t #f)
    (if b #f #t)))

(define (cons/optional x xs)
  (if x (cons x xs) xs))

(define (list/optional . xs)
  (let loop {[xs xs]}
    (cond
      [(null? xs) '()]
      [else (let* {[x (car xs)]}
              (if x
                (cons x (loop (cdr xs)))
                (loop (cdr xs))))])))

(define (list*/optional arg0 . args)
  (let loop {[arg0 arg0] [args args]}
    (cond
      [(null? args) arg0]
      [else (cons/optional arg0
              (loop (car args) (cdr args)))])))
