#lang racket/base

(provide
  define/debug
  define-values/debug
  lambda/debug
  case-lambda/debug
  #%app/debug
  debug
  debug*
  debug-value
  debug-values
  debug-exception
  dprintf
  stylish-dprintf
  call-and-debug)

(require
  (for-syntax
    racket/base
    syntax/parse))

(define-syntax define/debug
  (make-rename-transformer #'define))

(define-syntax define-values/debug
  (make-rename-transformer #'define-values))

(define-syntax lambda/debug
  (make-rename-transformer #'lambda))

(define-syntax case-lambda/debug
  (make-rename-transformer #'case-lambda))

(define-syntax #%app/debug
  (make-rename-transformer #'#%app))

(define-syntax debug
  (make-rename-transformer #'#%app))

(define-syntax (debug* stx)
  (syntax-parse stx
    [(_ . e) #'e]))

(define dummy (make-keyword-procedure void))

(define debug-value dummy)
(define debug-values dummy)
(define debug-exception dummy)
(define debug-expr dummy)
(define dprintf dummy)
(define stylish-dprintf dummy)

(define (call-and-debug #:thunk thunk fmt . args)
  (thunk))

(define (call-with-debug-frame
          #:enter [enter #false]
          #:exit [exit #false]
          #:thunk thunk
          fmt . args)
  (thunk))
