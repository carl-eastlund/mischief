#lang racket/unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Imports

(require
  racket/function
  racket/list
  racket/promise
  mischief/struct
  mzlib/pconvert
  mischief/stylish/signatures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Imports

(import)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Exports

(export expression^)

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

(define simple-expr-style
  (expr-style print-convert empty))

(define (extend-expr-style est
          #:after? [after? #false]
          . new-exts)
  (update expr-style est
    [extensions (if after?
                  (append extensions new-exts)
                  (append new-exts extensions))]))

(define (set-expr-style-default-convert st new-default)
  (update expr-style st
    [default new-default]))

(define (expr-style-extension? x)
  (expr-type? x))

(define (expr-style-extension
          type?
          convert
          [quotable? (const #false)]
          [prefer-quote? #true])
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
