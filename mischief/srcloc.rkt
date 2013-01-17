#lang racket/base

(provide
  source-location->suffix)

(require
  syntax/srcloc)

(define (source-location->suffix loc)
  (cond
    [(source-location-known? loc)
     (format " [~a]" (source-location->string loc))]
    [else ""]))
