#lang racket/base

(provide
  explode-syntax)

(require
  ffi/unsafe)

(define c
  (ffi-lib #f))

(define _hash
  (make-ctype _scheme
    (lambda (x)
      (unless (hash? x)
        (error '_hash
          "expected a mutable hash table, but got non-hash table ~v"
          x))
      (when (immutable? x)
        (error '_hash
          "expected a mutable hash table, but got immutable hash table ~v"
          x))
      x)
    #f))

(define scheme_explode_syntax
  (get-ffi-obj "scheme_explode_syntax" c
    (_fun _scheme _hash -> _scheme)))

(define (explode-syntax stx)
  (scheme_explode_syntax stx (make-hasheq)))
