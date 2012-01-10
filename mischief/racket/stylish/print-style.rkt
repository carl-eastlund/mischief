#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide
  default-print-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/list
  mischief/racket/define
  mischief/racket/struct
  mischief/racket/stylish/stylish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definition

(at-end
  (define default-print-style
    (extend-print-style
      (set-print-style-default-printer
        empty-print-style
        print-default)
      (print-style-extension boolean? print-default)
      (print-style-extension number? print-default)
      (print-style-extension string? print-default)
      (print-style-extension bytes? print-default)
      (print-style-extension char? print-default)
      (print-style-extension symbol? print-default)
      (print-style-extension regexp? print-default)
      (print-style-extension keyword? print-default)
      (print-style-extension special? print-special)
      (print-style-extension null? print-default)
      (print-style-extension pair? print-list*)
      (print-style-extension vector? print-vector)
      (print-style-extension box? print-box)
      (print-style-extension hash? print-hash)
      (print-style-extension prefab? print-prefab)
      (print-style-extension stylish-comment-expr? print-comment)
      (print-style-extension stylish-unprintable-expr? print-unprintable))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Default

(define (print-default x port)
  #;(print x port 1)
  (write x port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Special

(define (special? x)
  (and (list? x)
    (cons? x)
    (cons? (rest x))
    (empty? (rest (rest x)))
    (hash-has-key? special-keyword-table
      (special-keyword x))))

(define (print-special x port)
  (write-string (special-prefix x) port)
  (stylish-print-expr (special-contents x) port))

(define (special-keyword x) (first x))
(define (special-contents x) (second x))
(define (special-prefix x)
  (hash-ref special-keyword-table
    (special-keyword x)))

(define special-keyword-table
  (hasheq
    'quote "'"
    'quasiquote "`"
    'unquote ","
    'unquote-splicing ",@"
    'syntax "#'"
    'quasisyntax "#`"
    'unsyntax "#,"
    'unsyntax-splicing "#,@"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print ([Im]proper) Lists

(define (print-list* x port
          #:left [left "("]
          #:right [right ")"]
          [indent 1]
          [print-elem stylish-print-expr])
  (write-string left port)
  (unless (null? x)
    (call-with-stylish-port port
      (lambda (port)
        (print-elem (car x) port)
        (print-tail (cdr x) port indent print-elem))))
  (write-string right port))

(define (print-tail x port indent [print-elem stylish-print-expr])
  (cond
    [(null? x) (void)]
    [(pair? x)
     (stylish-print-separator port #:indent indent)
     (print-elem (car x) port)
     (print-tail (cdr x) port indent print-elem)]
    [else (print-dotted x port indent print-elem)]))

(define (print-pair x port
          #:left [left "("]
          #:right [right ")"]
          [indent 0]
          [print-car stylish-print-expr]
          [print-cdr stylish-print-expr])
  (write-string left port)
  (call-with-stylish-port port
    (lambda (port)
      (print-car (car x) port)
      (print-dotted (cdr x) port indent print-cdr)))
  (write-string right port))

(define (print-dotted x port indent [print-elem stylish-print-expr])
  (stylish-print-separator port #:indent indent)
  (write-string "." port)
  (stylish-print-separator port #:indent indent)
  (print-elem x port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Vector

(define (print-vector vec port [print-elem stylish-print-expr])
  (write-string "#" port)
  (print-list* #:left "[" #:right "]"
    (vector->list vec) port 0 print-elem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Box

(define (print-box b port [print-elem stylish-print-expr])
  (write-string "#&" port)
  (print-elem (unbox b) port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Hash

(define (print-hash ht port
          [print-key stylish-print-expr]
          [print-value stylish-print-expr])
  (cond
    [(hash-eq? ht) (write-string "#hasheq" port)]
    [(hash-eqv? ht) (write-string "#hasheqv" port)]
    [(hash-equal? ht) (write-string "#hash" port)])
  (print-list* #:left "{" #:right "}"
    (hash->list ht) port 0
    (lambda (kv port)
      (print-pair #:left "[" #:right "]"
        kv port 1 print-key print-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Prefab Struct

(define (print-prefab x port
          [print-key stylish-print-expr]
          [print-value stylish-print-expr])
  (write-string "#s(" port)
  (call-with-stylish-port port
    (lambda (port)
      (print-key (prefab-key x) port)
      (print-tail (prefab-fields x) port 1 print-value)))
  (write-string ")" port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Comment

(define (print-comment x port [print-expr stylish-print-expr])
  (print-expr (stylish-comment-expr-expr x) port)
  (stylish-print-separator port)
  (call-with-stylish-port port
    (lambda (port)
      (write-string "#|" port)
      (stylish-print-separator port #:indent 1)
      (write-string (stylish-comment-expr-comment x) port)
      (stylish-print-separator port #:indent 1)
      (write-string "|#" port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Unprintable

(define (print-unprintable x port)
  (write-string "#<" port)
  (display (stylish-unprintable-expr-name x) port)
  (write-string ">" port))
