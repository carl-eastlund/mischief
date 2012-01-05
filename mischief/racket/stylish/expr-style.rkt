#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide
  default-expr-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/set
  racket/promise
  mischief/racket/define
  mischief/syntax/transform
  mischief/racket/struct
  mischief/racket/stylish/stylish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definition

(at-end
  (define default-expr-style
    (extend-expr-style
      (set-expr-style-default-convert
        empty-expr-style
        (lambda (x) `(quote ,x)))
      (expr-style-extension boolean?
        convert-boolean
        quotable-boolean?
        try-quote-boolean?)
      (expr-style-extension number?
        convert-number
        quotable-number?
        try-quote-number?)
      (expr-style-extension string?
        convert-string
        quotable-string?
        try-quote-string?)
      (expr-style-extension bytes?
        convert-bytes
        quotable-bytes?
        try-quote-bytes?)
      (expr-style-extension char?
        convert-char
        quotable-char?
        try-quote-char?)
      (expr-style-extension symbol?
        convert-symbol
        quotable-symbol?
        try-quote-symbol?)
      (expr-style-extension regexp?
        convert-regexp
        quotable-regexp?
        try-quote-regexp?)
      (expr-style-extension keyword?
        convert-keyword
        quotable-keyword?
        try-quote-keyword?)
      (expr-style-extension null?
        convert-null
        quotable-null?
        try-quote-null?)
      (expr-style-extension list?
        convert-list
        quotable-list?
        try-quote-list?)
      (expr-style-extension list*?
        convert-list*
        quotable-list*?
        try-quote-list*?)
      (expr-style-extension pair?
        convert-pair
        quotable-pair?
        try-quote-pair?)
      (expr-style-extension mpair?
        convert-mpair
        quotable-mpair?
        try-quote-mpair?)
      (expr-style-extension vector?
        convert-vector
        quotable-vector?
        try-quote-vector?)
      (expr-style-extension box?
        convert-box
        quotable-box?
        try-quote-box?)
      (expr-style-extension hash?
        convert-hash
        quotable-hash?
        try-quote-hash?)
      (expr-style-extension set?
        convert-set
        quotable-set?
        try-quote-set?)
      (expr-style-extension void?
        convert-void
        quotable-void?
        try-quote-void?)
      (expr-style-extension prefab?
        convert-prefab
        quotable-prefab?
        try-quote-prefab?)
      (expr-style-extension struct?
        convert-struct
        quotable-struct?
        try-quote-struct?)
      (expr-style-extension promise?
        convert-promise
        quotable-promise?
        try-quote-promise?)
      (expr-style-extension syntax?
        convert-syntax
        quotable-syntax?
        try-quote-syntax?)
      (expr-style-extension path?
        convert-path
        quotable-path?
        try-quote-path?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Boolean

(define (convert-boolean b st) b)
(define (quotable-boolean? b st) #true)
(define try-quote-boolean? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Number

(define (convert-number n st) n)
(define (quotable-number? n st) #true)
(define try-quote-number? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert String

(define (convert-string s st) (if (immutable? s) s `(string-copy ,s)))
(define (quotable-string? s st) (immutable? s))
(define try-quote-string? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Byte String

(define (convert-bytes b st) (if (immutable? b) b `(bytes-copy ,b)))
(define (quotable-bytes? b st) (immutable? b))
(define try-quote-bytes? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Char

(define (convert-char c st) c)
(define (quotable-char? c st) #true)
(define try-quote-char? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Symbol

(define (convert-symbol s st)
  (list (symbol-constructor-name s)
    (symbol->string s)))

(define (quotable-symbol? s st) (symbol-interned? s))
(define try-quote-symbol? #true)

(define (symbol-constructor-name s)
  (cond
    [(symbol-interned? s) 'string->symbol]
    [(symbol-unreadable? s) 'string->unreadable-symbol]
    [else 'string->uninterned-symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Regexp

(define (convert-regexp rx st) rx)
(define (quotable-regexp? rx st) #true)
(define try-quote-regexp? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Keyword

(define (convert-keyword kw st) `(quote ,kw))
(define (quotable-keyword? kw st) #true)
(define try-quote-keyword? #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Null

(define (convert-null mt st) 'empty)
(define (quotable-null? mt st) #true)
(define try-quote-null? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert List

(define (convert-list xs st)
  (cons 'list
    (for/list {[x (in-list xs)]}
      (stylish-value->expr x st))))

(define (quotable-list? xs st)
  (for/and {[x (in-list xs)]}
    (stylish-quotable-value? x st)))

(define try-quote-list? #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Improper List

(define (list*? x)
  (and (pair? x)
    (pair? (cdr x))))

(define (convert-list* x st)
  (cons 'list*
    (cons (stylish-value->expr (car x) st)
      (let convert-tail {[tail (cdr x)]}
        (if (pair? tail)
          (cons (stylish-value->expr (car tail) st)
            (convert-tail (cdr tail)))
          (list (stylish-value->expr tail st)))))))

(define (quotable-list*? x st)
  (and (stylish-quotable-value? (car x) st)
    (let quotable-tail? {[tail (cdr x)]}
      (if (pair? tail)
        (and (stylish-quotable-value? (car tail) st)
          (quotable-tail? (cdr tail)))
        (stylish-quotable-value? tail st)))))

(define try-quote-list*? #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Pair

(define (convert-pair p st)
  (list 'cons
    (stylish-value->expr (car p) st)
    (stylish-value->expr (cdr p) st)))

(define (quotable-pair? p st)
  (and
    (stylish-quotable-value? (car p) st)
    (stylish-quotable-value? (cdr p) st)))

(define try-quote-pair? #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Mutable Pair

(define (convert-mpair p st)
  (list 'mcons
    (stylish-value->expr (mcar p) st)
    (stylish-value->expr (mcdr p) st)))

(define (quotable-mpair? p st) #false)
(define try-quote-mpair? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Vector

(define (convert-vector vec st)
  (cons (vector-constructor-name vec)
    (for/list {[x (in-vector vec)]}
      (stylish-value->expr x st))))

(define (quotable-vector? vec st)
  (and (immutable? vec)
    (for/and {[x (in-vector vec)]}
      (stylish-quotable-value? x st))))

(define try-quote-vector? #true)

(define (vector-constructor-name vec)
  (if (immutable? vec)
    'vector-immutable
    'vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Box

(define (convert-box b st)
  (list (box-constructor-name b)
    (stylish-value->expr (unbox b) st)))

(define (quotable-box? b st)
  (and (immutable? b)
    (stylish-quotable-value? (unbox b) st)))

(define try-quote-box? #true)

(define (box-constructor-name b)
  (if (immutable? b)
    'box-immutable
    'box))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Hash

(define (convert-hash h st)
  (list (hash-constructor-name h)
    (cons 'list
      (for/list {[(k v) (in-hash h)]}
        (list 'cons
          (stylish-value->expr k st)
          (stylish-value->expr v st))))))

(define (quotable-hash? h st)
  (and (immutable? h)
    (for/and {[(k v) (in-hash h)]}
      (and
        (stylish-quotable-value? k st)
        (stylish-quotable-value? v st)))))

(define try-quote-hash? #true)

(define (hash-constructor-name h)
  (cond
    [(immutable? h)
     (cond
       [(hash-eq? h) 'make-immutable-hasheq]
       [(hash-eqv? h) 'make-immutable-hasheqv]
       [(hash-equal? h) 'make-immutable-hash])]
    [(hash-weak? h)
     (cond
       [(hash-eq? h) 'make-weak-hasheq]
       [(hash-eqv? h) 'make-weak-hasheqv]
       [(hash-equal? h) 'make-weak-hash])]
    [else
     (cond
       [(hash-eq? h) 'make-hasheq]
       [(hash-eqv? h) 'make-hasheqv]
       [(hash-equal? h) 'make-hash])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Set

(define (convert-set s st)
  (cons (set-constructor-name s)
    (for/list {[x (in-set s)]}
      (stylish-value->expr x st))))

(define (quotable-set? s st) #false)
(define try-quote-set? #false)

(define (set-constructor-name s)
  (cond
    [(set-eq? s) 'seteq]
    [(set-eqv? s) 'seteqv]
    [(set-equal? s) 'set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Void

(define (convert-void v st) '(void))
(define (quotable-void? v st) #false)
(define try-quote-void? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Prefab Struct

(define (convert-prefab x st)
  (cons 'make-prefab-struct
    (cons (stylish-value->expr (prefab-key x) st)
      (for/list {[x (in-list (prefab-fields x))]}
        (stylish-value->expr x st)))))

(define (quotable-prefab? x st)
  (and (stylish-quotable-value? (prefab-key x) st)
    (for/and {[y (in-list (prefab-fields x))]}
      (stylish-quotable-value? y st))))

(define try-quote-prefab? #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert (Transparent) Struct

(define (convert-struct x st)
  (define vec (struct->vector x '_))
  (cons (struct-constructor-name (vector-ref vec 0))
    (for/list {[y (in-vector vec 1)]}
      (stylish-value->expr y st))))

(define (quotable-struct? x st) #false)
(define try-quote-struct? #false)

(define (struct-constructor-name sym)
  (string->symbol
    (regexp-replace #px"^struct:" (symbol->string sym) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Promise

(define (convert-promise p st)
  (list 'delay
    (if (promise-forced? p)
      (stylish-value->expr (force p) st)
      '_)))

(define (quotable-promise? p st) #false)
(define try-quote-promise? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Syntax

(define (convert-syntax stx st)
  (define datum (to-datum stx))
  (if (stylish-quotable-value? datum st)
    (list 'syntax datum)
    (list 'datum->syntax '_
      (stylish-value->expr datum st))))

(define (quotable-syntax? stx st) #false)
(define try-quote-syntax? #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Path

(define (convert-path path st)
  (list 'bytes->path
    (path->bytes path)
    `(quote ,(path-convention-type path))))

(define (quotable-path? path st) #false)
(define try-quote-path? #false)
