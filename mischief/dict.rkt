#lang racket/base

(provide
  dict-ref?
  dict-update?
  dict-add
  dict-subtract
  dict-set-all
  dict-remove-all
  dict->procedure)

(require
  racket/dict
  racket/match
  racket/function)

(define (dict->procedure dict
          #:failure
          [default
           (lambda {key}
             (dict-key-not-found-error 'dict->procedure dict key))])
  (lambda {key}
    (dict-ref dict key
      (lambda {}
        (invoke default key)))))

(define (dict-ref? dict key
          #:success [success identity]
          #:failure
          [default
           (lambda {}
             (dict-key-not-found-error 'dict-ref? dict key))])
  (define result
    (let/ec return
      (define value
        (dict-ref dict key
          (lambda () (return default))))
      (lambda () (success value))))
  (invoke result))

(define (dict-update? dict key
          #:transform [f identity]
          #:success [success identity]
          #:failure
          [default
           (lambda {}
             (dict-key-not-found-error 'dict-update? dict key))])
  ;; Using dict-update directly in case its implementation
  ;; ever improves to traverse keys only once.
  ;; Otherwise this would be easier with ref and set.
  (dict-update dict key
    (lambda (value/placeholder)
      (define value
        (match value/placeholder
          [(placeholder contents) contents]
          [value (success value)]))
      (f value))
    (lambda () (placeholder (invoke default)))))

(struct placeholder [contents])

(define (dict-add base
          #:combine
          [combine #false]
          #:combine/key
          [combine/key (and combine (lambda {k v1 v2} (combine v1 v2)))]
          . dicts)
  (for*/fold
      {[base base]}
      {[dict (in-list dicts)]
       [(key value) (in-dict dict)]}
    (dict-update? base key
      #:success
      (lambda (other)
        (if combine/key
          (combine/key key other value)
          (dict-key-multiple-values-error 'dict-add base key value other)))
      #:failure (lambda () value))))

(define (dict-subtract base . dicts)
  (for*/fold
      {[base base]}
      {[dict (in-list dicts)]
       [key (in-dict-keys dict)]}
    (dict-remove base key)))

(define (dict-set-all base
          #:value key->value
          . seqs)
  (for*/fold
      {[base base]}
      {[seq (in-list seqs)]
       [key seq]}
    (dict-set base key (invoke key->value key))))

(define (dict-remove-all base . seqs)
  (for*/fold
      {[base base]}
      {[seq (in-list seqs)]
       [key seq]}
    (dict-remove base key)))

(define (invoke result . args)
  (cond
    [(procedure? result) (apply result args)]
    [else result]))

(define (dict-key-not-found-error name dict key)
  (error name
    "key ~v not found in dict ~v"
    key
    dict))

(define (dict-key-multiple-values-error name dict key . vs)
  (error name
    "key ~v given multiple values ~v"
    key
    vs))
