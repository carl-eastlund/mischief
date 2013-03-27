#lang mischief

(provide
  examples/evaluator
  make-example-evaluator
  define-example-evaluator)

(require
  scribble/eval
  racket/sandbox
  (for-syntax mischief))

(define-syntax (define-example-evaluator stx)
  (syntax-parse stx
    [(_ name:id lang:module-path spec:module-path ...)
     #'(begin
         (require (for-label lang spec ...))
         (define name
           (make-example-evaluator 'lang
             #:requires '[spec ...])))]))

(define-syntax (examples/evaluator stx)
  (syntax-parse stx
    [(_ lang:module-path
        (~optional (~seq #:requires [spec:module-path ...])
          #:defaults {[(spec 1) empty]})
        body:expr ...+)
     #'(begin
         (require (for-label lang spec ...))
         (examples
           #:eval (make-example-evaluator 'lang #:requires '[spec ...])
           body ...))]))

(define make-example-evaluator
  (dynamic-wrap-procedure make-evaluator
    (lambda {proc}
      (parameterize {[sandbox-output 'string]
                     [sandbox-error-output 'string]}
        (call-with-trusted-sandbox-configuration proc)))))
