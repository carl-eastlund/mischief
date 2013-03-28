#lang racket/base

(provide
  maybe?
  (struct-out yes)
  no?
  no)

(require
  racket/bool)

(define (maybe? x)
  (or (yes? x) (no? x)))

(struct yes [value] #:transparent)

(define (no? x) (false? x))

(define (no) #false)
