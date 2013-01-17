#lang racket/base

(provide
  (struct-out yes)
  no)

(struct yes [value] #:transparent)

(define (no) #false)
