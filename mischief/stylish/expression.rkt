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

  expr-style-extension?
  expr-style-extension)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/list
  racket/promise
  mischief/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; ExprStyle = (expr-style (Maybe (Any -> Any)) (List ExprType))
;; ExprType = {exists T (expr-type (Type T) (Convert T) (Quotable T) Boolean)}
;; (Type T) = (Any -> Boolean : T)
;; (Convert T) = (Any ExprStyle -> Any)
;; (Quotable T) = (Any ExprStyle -> Boolean)
(struct expr-style [default extensions])
(struct expr-type [type? convert quotable? prefer-quote?])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define (value->expression name x st)
  (convert name x st))

(define (value-quotable? name x st)
  (quotable? name x st))

(define empty-expr-style
  (expr-style #false empty))

(define (extend-expr-style st after? new-exts)
  (update expr-style st
    [extensions (if after?
                  (append extensions new-exts)
                  (append new-exts extensions))]))

(define (set-expr-style-default-convert st new-default)
  (update expr-style st
    [default new-default]))

(define (expr-style-extension? x)
  (expr-type? x))

(define (expr-style-extension type? convert quotable? prefer-quote?)
  (expr-type type? convert quotable? prefer-quote?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

(define (convert name x st)
  (cond
    [(and
       (prefer-quote? name x st)
       (quotable? name x st))
     `(quote ,x)]
    [(type-of name x st) =>
     (lambda (type)
       ((expr-type-convert type) x st))]
    [(expr-style-default st) =>
     (lambda (default)
       (default x))]
    [else (error name
            "cannot convert value: ~v" x)]))

(define (prefer-quote? name x st)
  (cond
    [(type-of name x st) => expr-type-prefer-quote?]
    [else #false]))

(define (quotable? name x st)
  (cond
    [(type-of name x st) =>
     (lambda (type)
       ((expr-type-quotable? type) x st))]
    [else #false]))

(define (type-of name x st)
  (let find {[exts (expr-style-extensions st)]}
    (if (empty? exts)
      #false
      (if ((expr-type-type? (first exts)) x)
        (first exts)
        (find (rest exts))))))
