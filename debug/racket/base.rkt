#lang mischief

(require
  ;; Base language:
  racket/base
  (for-template
    (only-in racket/base
      define-syntax
      define-syntaxes))
  ;; Debugging versions:
  debug
  (for-template debug/syntax)
  ;; Export macro:
  debug/provide)

(provide
  (debug-out
    (all-from-out racket/base)
    (for-template
      define-syntax
      define-syntaxes)))
