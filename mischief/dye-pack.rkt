#lang racket/base

(provide
  with-disarmed-dye-packs
  call-with-disarmed-dye-packs
  rearm-dye-packs
  current-dye-packs
  module-code-inspector)

(require
  (for-syntax
    racket/base
    syntax/parse)
  racket/block)

(define-syntax (module-code-inspector stx)
  (syntax-parse stx
    [(_) #'(variable-reference->module-declaration-inspector
             (#%variable-reference))]))

(define-syntax (with-disarmed-dye-packs stx)
  (syntax-parse stx
    [(_ name:id armed:expr body:expr ...)
     #'(call-with-disarmed-dye-packs armed
         #:inspector (module-code-inspector)
         (lambda (name) body ...))]))

(define current-dye-packs
  (make-parameter (datum->syntax #false '())))

(define (call-with-disarmed-dye-packs stx proc
          #:inspector [inspector #false])
  (parameterize {[current-dye-packs stx]}
    (proc (syntax-disarm stx inspector))))

(define (rearm-dye-packs stx0
          #:dye-packs [dye-packs (current-dye-packs)]
          #:mode [mode #true])
  (define stx
    (cond
      [(symbol? mode)
       (syntax-property stx0 'taint-mode mode)]
      [else stx0]))
  (syntax-rearm stx dye-packs mode))
