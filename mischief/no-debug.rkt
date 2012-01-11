#lang racket/base

(provide
  define/debug
  debug
  debug*
  debug-value
  debug-values
  dprintf
  stylish-dprintf
  call-with-debug-frame
  call-and-debug)

(require
  (for-syntax
    racket/base))

(define-syntax define/debug
  (make-rename-transformer #'define))

(define-syntax debug
  (make-rename-transformer #'#%app))

(define-syntax-rule (debug* . e) e)

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
