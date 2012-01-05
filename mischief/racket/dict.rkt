#lang racket/base

(provide
  dict-ref?
  dict-key-not-found-error)

(require
  racket/dict
  racket/function
  racket/contract)

(provide/contract
 [dict-union (->* [(and/c dict? dict-can-functional-set?)]
                  [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                  #:rest (listof dict?)
                  (and/c dict? dict-can-functional-set?))]
 [dict-union! (->* [(and/c dict? dict-mutable?)]
                   [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                   #:rest (listof dict?)
                   void?)])

(define (dict-ref? dict key
          #:success [success identity]
          #:failure [failure (lambda ()
                               (dict-key-not-found-error
                                 'dict-ref? dict key))])
  (define thunk
    (let/ec return
      (define result
        (dict-ref dict key
          (lambda () (return failure))))
      (lambda () (success result))))
  (thunk))

(define (dict-key-not-found-error name dict key)
  (error name "key ~v not found in dict ~v" key dict))

(define ((dict-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

;; Eli: If this is useful, then at least make it worth using instead of
;; writing your own code.  For example, inspect the arguments and choose
;; an efficient order for the loops, or use a temporary hash table for a
;; union of two alists, etc.  Alternatively, just write a function for
;; merging two hash tables (and call it `hash-union', of course).

;; Ryan: I prefer the names dict-add-all and dict-add-all!---no connotations
;; of symmetry, and it makes it clear that the first argument determines the
;; representation (and key constraints, etc).

(define (dict-union
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (dict-duplicate-error 'dict-union))]
         one . rest)
  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-dict two)])
    (dict-set one k (if (dict-has-key? one k)
                        (combine/key k (dict-ref one k) v)
                        v))))

(define (dict-union!
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (dict-duplicate-error 'dict-union))]
         one . rest)
  (for* ([two (in-list rest)] [(k v) (in-dict two)])
    (dict-set! one k (if (dict-has-key? one k)
                         (combine/key k (dict-ref one k) v)
                         v))))
