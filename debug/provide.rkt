#lang mischief

(provide
  debug-out)

(require
  (for-syntax
    racket/base
    racket/provide-transform
    racket/syntax
    syntax/parse))

(begin-for-syntax

  (define (rename-debug-identifier id)
    (format-id id "~a/debug" id))

  (define (rename-debug-export ex)
    (define id (export-local-id ex))
    (define debug-id (rename-debug-identifier id))
    (define mode (export-mode ex))
    (cond
      [(identifier-binding debug-id mode)
       (define context
         (or (current-syntax-context)
           (export-orig-stx ex)))
       (export
         debug-id
         (export-out-sym ex)
         mode
         (export-protect? ex)
         context)]
      [else ex])))

(define-syntax debug-out
  (make-provide-transformer
    (lambda {stx modes}
      (syntax-parse stx
        [(_ spec:expr ...)
         (map rename-debug-export
           (expand-export #'(combine-out spec ...) modes))]))))
