#lang racket/base

(provide
  memo-table?
  make-memo-table
  clear-memo-table!
  memoize
  call/memoize
  memoize-procedure
  lambda/memoize
  case-lambda/memoize
  define/memoize
  let/memoize)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/name
    syntax/parse
    mischief/parse)
  racket/list
  racket/promise
  mischief/function)

;; MemoTable = (memo-table Cache)
;; Cache = (WeakHashEq Key (Value Key))
;; (Value final-key) = (Promise Any)
;; (Value Key) = (Ephemeron Key Cache) otherwise
(struct memo-table [(cache #:mutable)])
(define final-key (gensym 'final-key))
(define keyword-key (gensym 'keyword-key))

(define (make-memo-table)
  (memo-table (make-weak-hasheq)))

(define (clear-memo-table! memo)
  (set-memo-table-cache! memo (make-weak-hasheq)))

(define (memoize memo #:compute thunk . keys)
  (let loop {[keys keys] [cache (memo-table-cache memo)]}
    (if (empty? keys)
      (force
        (hash-ref! cache final-key
          (lambda () (delay (thunk)))))
      (loop (rest keys)
        (ephemeron-value
          (hash-ref! cache (first keys)
            (lambda ()
              (make-ephemeron (first keys)
                (make-weak-hasheq)))))))))

(define call/memoize
  (make-keyword-procedure
    (lambda (ks vs memo f . xs)
      (apply memoize memo
        (append xs (cons keyword-key ks) (cons keyword-key vs))
        #:compute (lambda () (keyword-apply f ks vs xs))))
    (lambda (memo f . xs)
      (apply memoize memo xs
        #:compute (lambda () (apply f xs))))))

(define call-memoized-procedure
  (make-keyword-procedure
    (lambda (ks vs mp . xs)
      (keyword-apply call/memoize
        ks vs (memoized-table mp) (memoized-proc mp) xs))))

(struct memoized [table proc]
  #:property prop:procedure call-memoized-procedure)

(define (memoize-procedure proc)
  (memoized (make-memo-table) proc))

(define-syntax-rule (lambda/memoize args . body)
  (memoize-procedure (lambda args . body)))

(define-syntax-rule (case-lambda/memoize [args . body] ...)
  (memoize-procedure (case-lambda [args . body] ...)))

(define-syntax-rule (define/memoize (name . args) . body)
  (define name (lambda/memoize args . body)))

(define-syntax-rule (let/memoize loop {[lhs rhs] ...} . body)
  (letrec {[loop (lambda/memoize (lhs ...) . body)]} (loop rhs ...)))
