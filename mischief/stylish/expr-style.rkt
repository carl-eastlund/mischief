#lang racket/unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Imports

(require
  racket/set
  racket/dict
  racket/promise
  racket/path
  racket/undefined
  syntax/srcloc
  mischief/boolean
  mischief/define
  mischief/struct
  mischief/stylish/signatures
  mischief/for)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Imports

(import expression^ stylish^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Exports

(export expr-style^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization Dependencies

(init-depend expression^ stylish^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Boolean

(define (convert-boolean b st) b)
(define (quotable-boolean? b st) #true)
(define prefer-quote-boolean? #false)

(define boolean-expr-style-extension
  (expr-style-extension boolean?
    convert-boolean
    quotable-boolean?
    prefer-quote-boolean?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Number

(define (convert-number n st) n)
(define (quotable-number? n st) #true)
(define prefer-quote-number? #false)

(define number-expr-style-extension
  (expr-style-extension number?
    convert-number
    quotable-number?
    prefer-quote-number?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert String

(define (convert-string s st)
  (if (immutable? s) s (stylish-comment-expr "mutable" s)))
(define (quotable-string? s st) (immutable? s))
(define prefer-quote-string? #false)

(define string-expr-style-extension
  (expr-style-extension string?
    convert-string
    quotable-string?
    prefer-quote-string?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Byte String

(define (convert-bytes b st)
  (if (immutable? b) b (stylish-comment-expr "mutable" b)))
(define (quotable-bytes? b st) (immutable? b))
(define prefer-quote-bytes? #false)

(define bytes-expr-style-extension
  (expr-style-extension bytes?
    convert-bytes
    quotable-bytes?
    prefer-quote-bytes?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Char

(define (convert-char c st) c)
(define (quotable-char? c st) #true)
(define prefer-quote-char? #false)

(define char-expr-style-extension
  (expr-style-extension char?
    convert-char
    quotable-char?
    prefer-quote-char?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Symbol

(define (convert-symbol s st)
  (cond
    [(symbol-interned? s) `(quote ,s)]
    [(symbol-unreadable? s)
     (stylish-comment-expr "unreadable"
       `(quote ,s))]
    [else
     (stylish-comment-expr "uninterned"
       `(quote ,s))]))

(define (quotable-symbol? s st) (symbol-interned? s))
(define prefer-quote-symbol? #true)

(define symbol-expr-style-extension
  (expr-style-extension symbol?
    convert-symbol
    quotable-symbol?
    prefer-quote-symbol?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Regexp

(define (convert-regexp rx st) rx)
(define (quotable-regexp? rx st) #true)
(define prefer-quote-regexp? #false)

(define regexp-expr-style-extension
  (expr-style-extension regexp?
    convert-regexp
    quotable-regexp?
    prefer-quote-regexp?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Keyword

(define (convert-keyword kw st) `(quote ,kw))
(define (quotable-keyword? kw st) #true)
(define prefer-quote-keyword? #true)

(define keyword-expr-style-extension
  (expr-style-extension keyword?
    convert-keyword
    quotable-keyword?
    prefer-quote-keyword?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Null

(define (convert-null mt st) '(quote empty))
(define (quotable-null? mt st) #true)
(define prefer-quote-null? #true)

(define null-expr-style-extension
  (expr-style-extension null?
    convert-null
    quotable-null?
    prefer-quote-null?))

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

(define prefer-quote-list? #true)

(define list-expr-style-extension
  (expr-style-extension list?
    convert-list
    quotable-list?
    prefer-quote-list?))

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

(define prefer-quote-list*? #true)

(define list*-expr-style-extension
  (expr-style-extension list*?
    convert-list*
    quotable-list*?
    prefer-quote-list*?))

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

(define prefer-quote-pair? #true)

(define pair-expr-style-extension
  (expr-style-extension pair?
    convert-pair
    quotable-pair?
    prefer-quote-pair?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Mutable Pair

(define (convert-mpair p st
          #:mcar [convert-mcar stylish-value->expr]
          #:mcdr [convert-mcdr stylish-value->expr])
  (list 'mcons
    (convert-mcar (mcar p) st)
    (convert-mcdr (mcdr p) st)))

(define (quotable-mpair? p st) #false)
(define prefer-quote-mpair? #false)

(define mpair-expr-style-extension
  (expr-style-extension mpair?
    convert-mpair
    quotable-mpair?
    prefer-quote-mpair?))

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

(define prefer-quote-vector? #true)

(define (vector-constructor-name vec)
  (if (immutable? vec)
    'vector-immutable
    'vector))

(define vector-expr-style-extension
  (expr-style-extension vector?
    convert-vector
    quotable-vector?
    prefer-quote-vector?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Box

(define (convert-box b st
          #:contents [convert-contents stylish-value->expr])
  (list (box-constructor-name b)
    (convert-contents (unbox b) st)))

(define (quotable-box? b st)
  (and (immutable? b)
    (stylish-quotable-value? (unbox b) st)))

(define prefer-quote-box? #true)

(define (box-constructor-name b)
  (if (immutable? b)
    'box-immutable
    'box))

(define box-expr-style-extension
  (expr-style-extension box?
    convert-box
    quotable-box?
    prefer-quote-box?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Hash

(define (convert-hash h st
          #:key [convert-key stylish-value->expr]
          #:value [convert-value stylish-value->expr])
  (cons (hash-constructor-name h)
    (for/append {[{k v} (in-hash h)]}
      (list
        (stylish-value->expr k st)
        (stylish-value->expr v st)))))

(define (quotable-hash? h st)
  (and (immutable? h)
    (for/and {[(k v) (in-hash h)]}
      (and
        (stylish-quotable-value? k st)
        (stylish-quotable-value? v st)))))

(define prefer-quote-hash? #true)

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
    prefer-quote-hash?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Set

(define (convert-set s st
          #:elem [convert-elem stylish-value->expr])
  (cons (set-constructor-name s)
    (for/list {[x (in-set s)]}
      (convert-elem x st))))

(define (quotable-set? s st) #false)
(define prefer-quote-set? #false)

(define (set-constructor-name s)
  (cond!
    [(set-eq? s) 'seteq]
    [(set-eqv? s) 'seteqv]
    [(set-equal? s) 'set]))

(define set-expr-style-extension
  (expr-style-extension set?
    convert-set
    quotable-set?
    prefer-quote-set?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Void

(define (convert-void v st) '(void))
(define (quotable-void? v st) #false)
(define prefer-quote-void? #false)

(define void-expr-style-extension
  (expr-style-extension void?
    convert-void
    quotable-void?
    prefer-quote-void?))

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

(define prefer-quote-prefab? #true)

(define prefab-expr-style-extension
  (expr-style-extension prefab?
    convert-prefab
    quotable-prefab?
    prefer-quote-prefab?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert (Transparent) Struct

(define (convert-struct x st
          #:field [convert-field stylish-value->expr])
  (define vec (struct->vector x '_))
  (cons (struct-constructor-name (vector-ref vec 0))
    (for/list {[y (in-vector vec 1)]}
      (convert-field y st))))

(define (quotable-struct? x st) #false)
(define prefer-quote-struct? #false)

(define (struct-constructor-name sym)
  (string->symbol
    (regexp-replace #px"^struct:" (symbol->string sym) "")))

(define struct-expr-style-extension
  (expr-style-extension struct?
    convert-struct
    quotable-struct?
    prefer-quote-struct?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Promise

(define (convert-promise p st
          #:contents [convert-contents stylish-value->expr])
  (list 'delay
    (if (promise-forced? p)
      (convert-contents (force p) st)
      '_)))

(define (quotable-promise? p st) #false)
(define prefer-quote-promise? #false)

(define promise-expr-style-extension
  (expr-style-extension promise?
    convert-promise
    quotable-promise?
    prefer-quote-promise?))

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
     (stylish-comment-expr (source-location->relative-string stx) expr)]
    [else expr]))

(define (source-location->relative-string src)
  (source-location->string
    (source-location->relative-source src)))

(define (source-location->relative-source src [rel (current-directory)])
  (define source (source-location-source src))
  (cond
    [(path-string? source)
     (srcloc
       (find-relative-path rel source)
       (source-location-line src)
       (source-location-column src)
       (source-location-position src)
       (source-location-span src))]
    [else src]))

(define (quotable-syntax? stx st) #false)
(define prefer-quote-syntax? #false)

(define syntax-expr-style-extension
  (expr-style-extension syntax?
    convert-syntax
    quotable-syntax?
    prefer-quote-syntax?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Path

(define (convert-path path st)
  (list 'bytes->path
    (path->bytes path)
    `(quote ,(path-convention-type path))))

(define (quotable-path? path st) #false)
(define prefer-quote-path? #false)

(define path-expr-style-extension
  (expr-style-extension path?
    convert-path
    quotable-path?
    prefer-quote-path?))

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

(define prefer-quote-dict? #false)

(define dict-expr-style-extension
  (expr-style-extension dict?
    convert-dict
    dict-quotable?
    prefer-quote-dict?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert EOF

(define (convert-eof x st) 'eof)
(define (eof-quotable? x st) #true)
(define prefer-quote-eof? #false)

(define eof-expr-style-extension
  (expr-style-extension eof-object?
    convert-eof
    eof-quotable?
    prefer-quote-eof?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Undefined

(define (undefined? x) (eq? x undefined))

(define (convert-undefined x st) (stylish-unprintable-expr 'undefined))
(define (undefined-quotable? x st) #true)
(define prefer-quote-undefined? #false)

(define undefined-expr-style-extension
  (expr-style-extension undefined?
    convert-undefined
    undefined-quotable?
    prefer-quote-undefined?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert Printable

(define printable-expr-style-extension
  (expr-style-extension stylish-printable?
    generic-stylish-value->expr
    generic-stylish-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define default-expr-style
  (extend-expr-style
    (set-expr-style-default-convert
      empty-expr-style
      (lambda (x) `(quote ,x)))
    printable-expr-style-extension
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
    dict-expr-style-extension
    eof-expr-style-extension
    undefined-expr-style-extension))

(define current-expr-style (make-parameter default-expr-style))
