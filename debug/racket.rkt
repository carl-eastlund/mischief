#lang mischief

(require
  ;; Base language:
  racket
  (for-template
    (only-in racket
      define-syntax
      define-syntaxes))
  ;; Debugging versions:
  (for-syntax debug)
  debug
  debug/syntax
  (for-template debug/syntax)
  ;; Export macro:
  debug/provide)

(provide
  (debug-out
    (all-from-out racket)
    (for-template
      define-syntax
      define-syntaxes)))
