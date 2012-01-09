#lang racket/base

(provide
  values-of)

(define-syntax-rule (values-of e)
  (call-with-values
    (lambda () (#%expression e))
    list))
