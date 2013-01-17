#lang racket/base

(provide
  string-lines)

(define (string-lines str)
  (regexp-split #px"\n"
    (regexp-replace #px"\n$" str "")))
