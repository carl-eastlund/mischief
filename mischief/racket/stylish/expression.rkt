#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide

  value->expression
  value-quotable?

  expr-style?
  empty-expr-style
  extend-expr-style
  set-expr-style-default-convert
  set-expr-style-preserve-cache?
  clear-expr-style-cache!

  expr-style-extension?
  expr-style-extension)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/list
  racket/promise
  mischief/racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; ExprStyle = (expr-style (Maybe (Any -> Any)) (List ExprType) ExprCache)
;; ExprCache = (expr-cache (Cache ExprType) (Cache Any) (Cache Boolean))
;; (Cache T) = (Hash Any (Promise T))
;; ExprType = {exists T (expr-type (Type T) (Convert T) (Quotable T) Boolean)}
;; (Type T) = (Any -> Boolean : T)
;; (Convert T) = (Any ExprStyle -> Any)
;; (Quotable T) = (Any ExprStyle -> Boolean)
(struct expr-style [default extensions cache])
(struct expr-cache [type-of convert quotable?] #:mutable)
(struct expr-type [type? convert quotable? prefer-quote?])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define (value->expression name x st)
  (let* {[st (set-expr-style-preserve-cache? st #true)]}
    (convert name x st)))

(define (value-quotable? name x st)
  (let* {[st (set-expr-style-preserve-cache? st #true)]}
    (quotable? name x st)))

(define empty-expr-style
  (expr-style #false empty #false))

(define (extend-expr-style st after? new-exts)
  (update expr-style st
    [extensions (if after?
                  (append extensions new-exts)
                  (append new-exts extensions))]
    [cache (and cache (fresh-cache))]))

(define (set-expr-style-default-convert st new-default)
  (update expr-style st
    [default new-default]
    [cache (and cache (fresh-cache))]))

(define (set-expr-style-preserve-cache? st preserve?)
  (update expr-style st
    [cache (and preserve? (or cache (fresh-cache)))]))

(define (clear-expr-style-cache! st)
  (cond
    [(expr-style-cache st) =>
     (lambda (cache)
       (set-expr-cache-type-of! cache (make-weak-hasheq))
       (set-expr-cache-convert! cache (make-weak-hasheq))
       (set-expr-cache-quotable?! cache (make-weak-hasheq)))]
    [else (void)]))

(define (expr-style-extension? x)
  (expr-type? x))

(define (expr-style-extension type? convert quotable? prefer-quote?)
  (expr-type type? convert quotable? prefer-quote?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

(define (convert name x st)
  (force
    (hash-ref! (expr-cache-convert (expr-style-cache st)) x
      (lambda ()
        (delay
          (if (and
                (prefer-quote? name x st)
                (quotable? name x st))
            `(quote ,x)
            (cond
              [(type-of name x st) =>
               (lambda (type)
                 ((expr-type-convert type) x st))]
              [(expr-style-default st) =>
               (lambda (default)
                 (default x))]
              [else (error name
                      "cannot convert value: ~v" x)])))))))

(define (prefer-quote? name x st)
  (cond
    [(type-of name x st) => expr-type-prefer-quote?]
    [else #false]))

(define (quotable? name x st)
  (force
    (hash-ref! (expr-cache-quotable? (expr-style-cache st)) x
      (lambda ()
        (delay
          (cond
            [(type-of name x st) =>
             (lambda (type)
               ((expr-type-quotable? type) x st))]
            [else #false]))))))

(define (type-of name x st)
  (force
    (hash-ref! (expr-cache-type-of (expr-style-cache st)) x
      (lambda ()
        (delay
          (let find {[exts (expr-style-extensions st)]}
            (if (empty? exts)
              #false
              (if ((expr-type-type? (first exts)) x)
                (first exts)
                (find (rest exts))))))))))

(define (fresh-cache)
  (expr-cache
    (make-weak-hasheq)
    (make-weak-hasheq)
    (make-weak-hasheq)))
