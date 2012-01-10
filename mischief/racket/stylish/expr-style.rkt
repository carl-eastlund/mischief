#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide
  default-expr-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/set
  racket/dict
  racket/promise
  syntax/srcloc
  mischief/racket/boolean
  mischief/racket/define
  mischief/racket/struct
  mischief/racket/stylish/stylish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(at-end
  (define default-expr-style
    (extend-expr-style
      (set-expr-style-default-convert
        empty-expr-style
        (lambda (x) `(quote ,x)))
      boolean-expr-style-extension
      number-expr-style-extension
      string-expr-style-extension
      bytes-expr-style-extension
      char-expr-style-extension
      symbol-expr-style-extension
      regexp-expr-style-extension
      keyword-expr-style-extension
      null-expr-style-extension
      list-expr-style-extension
      list*-expr-style-extension
      pair-expr-style-extension
      mpair-expr-style-extension
      vector-expr-style-extension
      box-expr-style-extension
      hash-expr-style-extension
      set-expr-style-extension
      void-expr-style-extension
      prefab-expr-style-extension
      struct-expr-style-extension
      promise-expr-style-extension
      syntax-expr-style-extension
      path-expr-style-extension
      dict-expr-style-extension)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Boolean

(define (convert-boolean b st) b)
(define (quotable-boolean? b st) #true)
(define try-quote-boolean? #false)

(define boolean-expr-style-extension
  (expr-style-extension boolean?
    convert-boolean
    quotable-boolean?
    try-quote-boolean?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Number

(define (convert-number n st) n)
(define (quotable-number? n st) #true)
(define try-quote-number? #false)

(define number-expr-style-extension
  (expr-style-extension number?
    convert-number
    quotable-number?
    try-quote-number?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert String

(define (convert-string s st) (if (immutable? s) s `(string-copy ,s)))
(define (quotable-string? s st) (immutable? s))
(define try-quote-string? #false)

(define string-expr-style-extension
  (expr-style-extension string?
    convert-string
    quotable-string?
    try-quote-string?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Byte String

(define (convert-bytes b st) (if (immutable? b) b `(bytes-copy ,b)))
(define (quotable-bytes? b st) (immutable? b))
(define try-quote-bytes? #false)

(define bytes-expr-style-extension
  (expr-style-extension bytes?
    convert-bytes
    quotable-bytes?
    try-quote-bytes?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Char

(define (convert-char c st) c)
(define (quotable-char? c st) #true)
(define try-quote-char? #false)

(define char-expr-style-extension
  (expr-style-extension char?
    convert-char
    quotable-char?
    try-quote-char?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Symbol

(define (convert-symbol s st)
  (list (symbol-constructor-name s)
    (symbol->string s)))

(define (quotable-symbol? s st) (symbol-interned? s))
(define try-quote-symbol? #true)

(define (symbol-constructor-name s)
  (cond!
    [(symbol-interned? s) 'string->symbol]
    [(symbol-unreadable? s) 'string->unreadable-symbol]
    [else 'string->uninterned-symbol]))

(define symbol-expr-style-extension
  (expr-style-extension symbol?
    convert-symbol
    quotable-symbol?
    try-quote-symbol?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Regexp

(define (convert-regexp rx st) rx)
(define (quotable-regexp? rx st) #true)
(define try-quote-regexp? #false)

(define regexp-expr-style-extension
  (expr-style-extension regexp?
    convert-regexp
    quotable-regexp?
    try-quote-regexp?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Keyword

(define (convert-keyword kw st) `(quote ,kw))
(define (quotable-keyword? kw st) #true)
(define try-quote-keyword? #true)

(define keyword-expr-style-extension
  (expr-style-extension keyword?
    convert-keyword
    quotable-keyword?
    try-quote-keyword?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Null

(define (convert-null mt st) 'empty)
(define (quotable-null? mt st) #true)
(define try-quote-null? #false)

(define null-expr-style-extension
  (expr-style-extension null?
    convert-null
    quotable-null?
    try-quote-null?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert List

(define (convert-list xs st
          #:elem [convert-elem stylish-value->expr])
  (cons 'list
    (for/list {[x (in-list xs)]}
      (convert-elem x st))))

(define (quotable-list? xs st)
  (for/and {[x (in-list xs)]}
    (stylish-quotable-value? x st)))

(define try-quote-list? #true)

(define list-expr-style-extension
  (expr-style-extension list?
    convert-list
    quotable-list?
    try-quote-list?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Improper List

(define (list*? x)
  (and (pair? x)
    (pair? (cdr x))))

(define (convert-list* x st
          #:head [convert-head stylish-value->expr]
          #:tail [convert-tail stylish-value->expr])
  (cons 'list*
    (let convert-elems {[elems x]}
      (cons (convert-head (car x) st)
        (cond!
          [(pair? (cdr elems)) (convert-elems (cdr elems))]
          [else (list (convert-tail (cdr elems) st))])))))

(define (quotable-list*? x st)
  (and (stylish-quotable-value? (car x) st)
    (let quotable-tail? {[tail (cdr x)]}
      (if (pair? tail)
        (and (stylish-quotable-value? (car tail) st)
          (quotable-tail? (cdr tail)))
        (stylish-quotable-value? tail st)))))

(define try-quote-list*? #true)

(define list*-expr-style-extension
  (expr-style-extension list*?
    convert-list*
    quotable-list*?
    try-quote-list*?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Pair

(define (convert-pair p st
          #:car [convert-car stylish-value->expr]
          #:cdr [convert-cdr stylish-value->expr])
  (list 'cons
    (convert-car (car p) st)
    (convert-cdr (cdr p) st)))

(define (quotable-pair? p st)
  (and
    (stylish-quotable-value? (car p) st)
    (stylish-quotable-value? (cdr p) st)))

(define try-quote-pair? #true)

(define pair-expr-style-extension
  (expr-style-extension pair?
    convert-pair
    quotable-pair?
    try-quote-pair?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Mutable Pair

(define (convert-mpair p st
          #:mcar [convert-mcar stylish-value->expr]
          #:mcdr [convert-mcdr stylish-value->expr])
  (list 'mcons
    (convert-mcar (mcar p) st)
    (convert-mcdr (mcdr p) st)))

(define (quotable-mpair? p st) #false)
(define try-quote-mpair? #false)

(define mpair-expr-style-extension
  (expr-style-extension mpair?
    convert-mpair
    quotable-mpair?
    try-quote-mpair?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Vector

(define (convert-vector vec st
          #:elem [convert-elem stylish-value->expr])
  (cons (vector-constructor-name vec)
    (for/list {[x (in-vector vec)]}
      (convert-elem x st))))

(define (quotable-vector? vec st)
  (and (immutable? vec)
    (for/and {[x (in-vector vec)]}
      (stylish-quotable-value? x st))))

(define try-quote-vector? #true)

(define (vector-constructor-name vec)
  (if (immutable? vec)
    'vector-immutable
    'vector))

(define vector-expr-style-extension
  (expr-style-extension vector?
    convert-vector
    quotable-vector?
    try-quote-vector?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Box

(define (convert-box b st
          #:contents [convert-contents stylish-value->expr])
  (list (box-constructor-name b)
    (convert-contents (unbox b) st)))

(define (quotable-box? b st)
  (and (immutable? b)
    (stylish-quotable-value? (unbox b) st)))

(define try-quote-box? #true)

(define (box-constructor-name b)
  (if (immutable? b)
    'box-immutable
    'box))

(define box-expr-style-extension
  (expr-style-extension box?
    convert-box
    quotable-box?
    try-quote-box?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Hash

(define (convert-hash h st
          #:key [convert-key stylish-value->expr]
          #:value [convert-value stylish-value->expr])
  (convert-dict h st
    #:constructor (hash-constructor-name h)
    #:key convert-key
    #:value convert-value))

(define (quotable-hash? h st)
  (and (immutable? h)
    (for/and {[(k v) (in-hash h)]}
      (and
        (stylish-quotable-value? k st)
        (stylish-quotable-value? v st)))))

(define try-quote-hash? #true)

(define (hash-constructor-name h)
  (cond!
    [(immutable? h)
     (cond!
       [(hash-eq? h) 'make-immutable-hasheq]
       [(hash-eqv? h) 'make-immutable-hasheqv]
       [(hash-equal? h) 'make-immutable-hash])]
    [(hash-weak? h)
     (cond!
       [(hash-eq? h) 'make-weak-hasheq]
       [(hash-eqv? h) 'make-weak-hasheqv]
       [(hash-equal? h) 'make-weak-hash])]
    [else
     (cond!
       [(hash-eq? h) 'make-hasheq]
       [(hash-eqv? h) 'make-hasheqv]
       [(hash-equal? h) 'make-hash])]))

(define hash-expr-style-extension
  (expr-style-extension hash?
    convert-hash
    quotable-hash?
    try-quote-hash?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Set

(define (convert-set s st
          #:elem [convert-elem stylish-value->expr])
  (cons (set-constructor-name s)
    (for/list {[x (in-set s)]}
      (convert-elem x st))))

(define (quotable-set? s st) #false)
(define try-quote-set? #false)

(define (set-constructor-name s)
  (cond!
    [(set-eq? s) 'seteq]
    [(set-eqv? s) 'seteqv]
    [(set-equal? s) 'set]))

(define set-expr-style-extension
  (expr-style-extension set?
    convert-set
    quotable-set?
    try-quote-set?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Void

(define (convert-void v st) '(void))
(define (quotable-void? v st) #false)
(define try-quote-void? #false)

(define void-expr-style-extension
  (expr-style-extension void?
    convert-void
    quotable-void?
    try-quote-void?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Prefab Struct

(define (convert-prefab x st
          #:key [convert-key stylish-value->expr]
          #:field [convert-field stylish-value->expr])
  (cons 'make-prefab-struct
    (cons (convert-key (prefab-key x) st)
      (for/list {[x (in-list (prefab-fields x))]}
        (convert-field x st)))))

(define (quotable-prefab? x st)
  (and (stylish-quotable-value? (prefab-key x) st)
    (for/and {[y (in-list (prefab-fields x))]}
      (stylish-quotable-value? y st))))

(define try-quote-prefab? #true)

(define prefab-expr-style-extension
  (expr-style-extension prefab?
    convert-prefab
    quotable-prefab?
    try-quote-prefab?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert (Transparent) Struct

(define (convert-struct x st
          #:field [convert-field stylish-value->expr])
  (define vec (struct->vector x '_))
  (cons (struct-constructor-name (vector-ref vec 0))
    (for/list {[y (in-vector vec 1)]}
      (convert-field y st))))

(define (quotable-struct? x st) #false)
(define try-quote-struct? #false)

(define (struct-constructor-name sym)
  (string->symbol
    (regexp-replace #px"^struct:" (symbol->string sym) "")))

(define struct-expr-style-extension
  (expr-style-extension struct?
    convert-struct
    quotable-struct?
    try-quote-struct?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Promise

(define (convert-promise p st
          #:contents [convert-contents stylish-value->expr])
  (list 'delay
    (if (promise-forced? p)
      (convert-contents (force p) st)
      '_)))

(define (quotable-promise? p st) #false)
(define try-quote-promise? #false)

(define promise-expr-style-extension
  (expr-style-extension promise?
    convert-promise
    quotable-promise?
    try-quote-promise?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Syntax

(define (convert-syntax stx st
          #:shorthand? [shorthand? stylish-quotable-value?]
          #:datum [convert-datum stylish-value->expr])
  (define datum (syntax->datum stx))
  (define expr
    (cond!
      [(shorthand? datum st)
       (list 'syntax datum)]
      [else
       (list 'datum->syntax
         '(syntax ?)
         (convert-datum datum st))]))
  (cond!
    [(source-location-known? stx)
     (stylish-comment-expr (source-location->string stx) expr)]
    [else expr]))

(define (quotable-syntax? stx st) #false)
(define try-quote-syntax? #false)

(define syntax-expr-style-extension
  (expr-style-extension syntax?
    convert-syntax
    quotable-syntax?
    try-quote-syntax?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Path

(define (convert-path path st)
  (list 'bytes->path
    (path->bytes path)
    `(quote ,(path-convention-type path))))

(define (quotable-path? path st) #false)
(define try-quote-path? #false)

(define path-expr-style-extension
  (expr-style-extension path?
    convert-path
    quotable-path?
    try-quote-path?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Dictionary

(define (convert-dict dict st
          #:constructor [constructor-name unknown-dict-constructor]
          #:key [convert-key stylish-value->expr]
          #:value [convert-value stylish-value->expr])
  (list constructor-name
    (convert-alist (dict->list dict) st
      #:key convert-key
      #:value convert-value)))

(define unknown-dict-constructor
  (stylish-unprintable-expr
    'procedure:unknown-dict))

(define (convert-alist alist st
          #:key [convert-key stylish-value->expr]
          #:value [convert-value stylish-value->expr])
  (define (convert-elem p st)
    (convert-pair p st
      #:car convert-key
      #:cdr convert-value))
  (convert-list alist st #:elem convert-elem))

(define (dict-quotable? dict st) #false)

(define try-quote-dict? #false)

(define dict-expr-style-extension
  (expr-style-extension dict?
    convert-dict
    dict-quotable?
    try-quote-dict?))
