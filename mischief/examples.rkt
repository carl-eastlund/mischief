#lang mischief

(provide
  examples/evaluator
  make-example-evaluator
  define-example-evaluator
  define-example-form)

(require
  scribble/eval
  racket/sandbox
  (for-syntax mischief))

(define-syntax (define-example-form stx)
  (syntax-parse stx
    [(_ name:id lang:module-path
        (~or spec:module-path
          ({~literal for-syntax} phase-1-spec:module-path))
        ...)
     #'(begin
         (define-example-evaluator evaluator lang
           spec ...
           (for-syntax phase-1-spec) ...)
         (... (define-shorthand (name body ...)
                (examples #:eval evaluator body ...))))]))

(define-syntax (define-example-evaluator stx)
  (syntax-parse stx
    [(_ name:id lang:module-path
        (~or spec:module-path
          ({~literal for-syntax} phase-1-spec:module-path))
        ...)
     #'(begin
         (require (for-label lang spec ... (for-syntax phase-1-spec ...)))
         (define name
           (make-example-evaluator 'lang
             #:requires '[spec ... (for-syntax phase-1-spec) ...])))]))

(define-syntax (examples/evaluator stx)
  (syntax-parse stx
    [(_ lang:module-path
        (~optional
          (~seq #:requires
            [(~or spec:module-path
               ({~literal for-syntax} phase-1-spec:module-path))
             ...])
          #:defaults {[(spec 1) empty] [(phase-1-spec 1) empty]})
        body:expr ...+)
     #'(begin
         (require (for-label lang spec ... (for-syntax phase-1-spec ...)))
         (examples
           #:eval (make-example-evaluator 'lang
                    #:requires '[spec ... (for-syntax phase-1-spec) ...])
           body ...))]))

(define make-example-evaluator
  (dynamic-wrap-procedure make-evaluator
    (lambda {proc}
      (parameterize {[sandbox-output 'string]
                     [sandbox-error-output 'string]}
        (call-with-trusted-sandbox-configuration proc)))))
