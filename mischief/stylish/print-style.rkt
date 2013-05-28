#lang racket/unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Imports

(require
  racket/list
  mischief/define
  mischief/struct
  mischief/stylish/signatures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Imports

(import print^ stylish^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Exports

(export print-style^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization Dependencies

(init-depend print^ stylish^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Default

(define (print-atom x port st)
  #;(print x port 1)
  (write x port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print ([Im]proper) Lists

(define (print-list* x port st
          #:left [left "("]
          #:right [right ")"]
          [indent 0]
          [print-elem stylish-write])
  (cond
    [(special? x)
     (print-special x port st print-elem)]
    [(null? x)
     (write-string left port)
     (write-string right port)]
    [else
     (write-string left port)
     (call-with-stylish-port port
       (lambda (port)
         (print-elem (car x) port st)
         (print-tail (cdr x) port st indent print-elem)))
     (write-string right port)]))

(define (print-tail x port st
          [indent 0]
          [print-elem stylish-write])
  (cond
    [(special? x)
     (print-dotted x port st indent
       (lambda (x port st)
         (print-special x port st print-elem)))]
    [(null? x) (void)]
    [(pair? x)
     (stylish-print-separator port #:indent indent)
     (print-elem (car x) port st)
     (print-tail (cdr x) port st indent print-elem)]
    [else (print-dotted x port st indent print-elem)]))

(define (print-pair x port st
          #:left [left "("]
          #:right [right ")"]
          [indent 0]
          [print-car stylish-write]
          [print-cdr stylish-write])
  (write-string left port)
  (call-with-stylish-port port
    (lambda (port)
      (print-car (car x) port st)
      (print-dotted (cdr x) port st indent print-cdr)))
  (write-string right port))

(define (print-dotted x port st
          [indent 1]
          [print-elem stylish-write])
  (stylish-print-separator port #:indent indent)
  (write-string "." port)
  (stylish-print-separator port #:indent indent)
  (print-elem x port st))

(define (special? x)
  (and (list? x)
    (cons? x)
    (cons? (rest x))
    (empty? (rest (rest x)))
    (hash-has-key? special-keyword-table
      (special-keyword x))))

(define (print-special x port st [print-contents stylish-write])
  (write-string (special-prefix x) port)
  (print-contents (special-contents x) port st))

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
;; Print Vector

(define (print-vector vec port st
          [print-elem stylish-write])
  (write-string "#" port)
  (print-list* (vector->list vec) port st
    0 print-elem #:left "[" #:right "]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Box

(define (print-box b port st
          [print-elem stylish-write])
  (write-string "#&" port)
  (print-elem (unbox b) port st))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Hash

(define (print-hash ht port st
          [print-key stylish-write]
          [print-value stylish-write])
  (cond
    [(hash-eq? ht) (write-string "#hasheq" port)]
    [(hash-eqv? ht) (write-string "#hasheqv" port)]
    [(hash-equal? ht) (write-string "#hash" port)])
  (print-list* #:left "{" #:right "}"
    (hash->list ht) port st 0
    (lambda (kv port st)
      (print-pair kv port st
        1 print-key print-value #:left "[" #:right "]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Prefab Struct

(define (print-prefab x port st
          [print-key stylish-write]
          [print-value stylish-write])
  (write-string "#s(" port)
  (call-with-stylish-port port
    (lambda (port)
      (print-key (prefab-key x) port st)
      (print-tail (prefab-fields x) port st 1 print-value)))
  (write-string ")" port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print Comment

(define (print-comment x port st
          [print-expr stylish-write])
  (print-expr (stylish-comment-expr-expr x) port st)
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

(define (print-unprintable x port st)
  (write-string "#<" port)
  (display (stylish-unprintable-expr-name x) port)
  (write-string ">" port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define default-print-style
  (extend-print-style
    (set-print-style-default-printer
      empty-print-style
      write)
    (print-style-extension stylish-writable? generic-stylish-write)
    (print-style-extension boolean? print-atom)
    (print-style-extension number? print-atom)
    (print-style-extension string? print-atom)
    (print-style-extension bytes? print-atom)
    (print-style-extension char? print-atom)
    (print-style-extension symbol? print-atom)
    (print-style-extension regexp? print-atom)
    (print-style-extension keyword? print-atom)
    (print-style-extension null? print-atom)
    (print-style-extension pair? print-list*)
    (print-style-extension vector? print-vector)
    (print-style-extension box? print-box)
    (print-style-extension hash? print-hash)
    (print-style-extension prefab? print-prefab)
    (print-style-extension stylish-comment-expr? print-comment)
    (print-style-extension stylish-unprintable-expr? print-unprintable)))

(define current-print-style (make-parameter default-print-style))
