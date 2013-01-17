#lang racket/base

(provide
  (all-defined-out))

(require
  racket/pretty
  racket/list
  racket/vector
  racket/function
  racket/set
  racket/promise
  mischief/match
  mischief/function
  mischief/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value to Expression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A ReifyTable is:
;;  (reify-table [Listof ReifyExt] (Or (Any -> Any) Boolean))
;; A ReifyExt is, for some type T:
;;  (reify-extension [Pred T] [Reify T] [Compare T] [Quotable T] [Literal T])
;; A [Pred T] is (Any -> Boolean : T)
;; A [Reify T] is (T (Any -> Any) (Any -> Boolean) (Any Any -> Boolean) -> Any)
;; A [Compare T] is (T T (Any Any -> Boolean) -> Boolean)
;; A [Quotable T] is (T (Any -> Boolean) -> Boolean)
;; A [Literal T] is Boolean
(struct reify-table [extensions default])
(struct reify-extension [predicate reify compare quotable? literal])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions

(define (value->expression v [vt (current-reify-value-table)])
  (reify-value 'value->expression v vt (make-hasheq) (make-hasheq)))

(define (value-quotable? v [vt (current-reify-value-table)])
  (quotable? 'value-quotable? v vt (make-hasheq)))

(define (value-literal? v [vt (current-reify-value-table)])
  (and (quotable? 'value-literal? v vt (make-hasheq))
    (literal? 'value-literal? v vt)))

(define (reify-value-extension? x)
  (reify-extension? x))

(define (reify-value-extension predicate reify
          #:literal? [literal? (const #false)]
          #:quotable? [quotable? (const #false)]
          #:compare [compare (const #false)])
  (reify-extension predicate reify literal? quotable? compare))

(define (reify-value-table? x)
  (reify-table? x))

(define (extend-reify-value-table vt
          #:default [default (reify-table-default vt)]
          #:after? [after? #false]
          . extensions)
  (reify-table
    (if after?
      (append (reify-table-extensions vt) extensions)
      (append extensions (reify-table-extensions vt)))
    default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Functions

(define (reify-value name v vt cache:reify-value cache:is-quotable?)
  (match! vt
    [(reify-table extensions default)
     (define (<<? x y) (value<<? name extensions x y))
     (define (q? x) (quotable? name x vt cache:is-quotable?))
     (let reify/value {[v v]}
       (hash-ref! cache:reify-value v
         (lambda ()
           (if (q? v)
             (if (literal? name v vt) v `(quote ,v))
             (let reify/extensions {[extensions extensions]}
               (if (empty? extensions)
                 (if (procedure? default)
                   (default v)
                   (cannot-reify-error name v))
                 (match! (first extensions)
                   [(reify-extension type? proc _ _ _)
                    (if (type? v)
                      (proc v reify/value q? <<?)
                      (reify/extensions (rest extensions)))])))))))]))

(define (cannot-reify-error name v)
  (error name
    "cannot reify value: ~v" v))

(define (quotable? name v vt cache:is-quotable?)
  (match! vt
    [(reify-table extensions default)
     (let quotable?/value {[v v]}
       (hash-ref! cache:is-quotable? v
         (lambda ()
           (let quotable?/extensions {[extensions extensions]}
             (if (empty? extensions)
               (eq? default #true)
               (match! (first extensions)
                 [(reify-extension type? _ _ quot? _)
                  (if (type? v)
                    (quot? v quotable?/value)
                    (quotable?/extensions (rest extensions)))]))))))]))

(define (literal? name v vt)
  (match! vt
    [(reify-table extensions _)
     (let literal?/extensions {[extensions extensions]}
       (if (empty? extensions)
         #false
         (match! (first extensions)
           [(reify-extension type? _ _ _ lit?)
            (if (type? v)
              lit?
              (literal?/extensions (rest extensions)))])))]))

(define (value<<? name extensions a b)
  (let <<?/values {[a a] [b b]}
    (let <<?/extensions {[extensions extensions]}
      (if (empty? extensions)
        #false
        (match! (first extensions)
          [(reify-extension type? _ <<? _ _)
           (define a? (type? a))
           (define b? (type? b))
           (cond
             [(and a? b?)
              (<<? a b <<?/values)]
             [a? #true]
             [b? #false]
             [else (<<?/extensions (rest extensions))])])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value to Expression Extensions

;;;;;;;;;;;;;;;;;;;;
;; Comparisons

(define (simple<<? a b <<?)
  (<<? a b))

(define (flat<<? <?)
  (lambda (a b <<?)
    (<? a b)))

(define (map<<? f [base<<? simple<<?])
  (lambda (a b <<?)
    (base<<? (f a) (f b) <<?)))

(define (lex<<? . <<?s)
  (cond
    [(empty? <<?s) (const #false)]
    [(empty? (rest <<?s)) (first <<?s)]
    [else (let* {[<<1? (first <<?s)]
                 [<<2? (apply lex<<? (rest <<?s))]}
            (lambda (a b <<?)
              (cond
                [(<<1? a b <<?) #true]
                [(<<1? b a <<?) #false]
                [else (<<2? a b <<?)])))]))

(define (dispatch<<? pred true<<? false<<?)
  (lambda (a b <<?)
    (define a? (pred a))
    (define b? (pred b))
    (cond
      [(and a? b?) (true<<? a b <<?)]
      [a? #true]
      [b? #false]
      [else (false<<? a b <<?)])))

(define (compose<<? . <<?s)
  (cond
    [(empty? <<?s) simple<<?]
    [(empty? (rest <<?s)) (first <<?s)]
    [else (let* {[<<1? (first <<?s)]
                 [<<2? (apply compose<<? (rest <<?s))]}
            (lambda (a b <<?)
              (<<1? a b
                (lambda (c d)
                  (<<2? c d <<?)))))]))

(define (category<<? . preds)
  (cond
    [(empty? preds) (const #false)]
    [else (let* {[pred (first preds)]
                 [<<?* (apply category<<? (rest preds))]}
            (lambda (a b <<?)
              (define a? (pred a))
              (define b? (pred b))
              (cond
                [b? #false]
                [a? #true]
                [else (<<?* a b <<?)])))]))

(define (constant<<? . xs)
  (cond
    [(empty? xs) (const #false)]
    [else (let* {[x (first xs)]
                 [<<?* (apply constant<<? (rest xs))]}
            (lambda (a b <<?)
              (define a? (equal? a x))
              (define b? (equal? b x))
              (cond
                [b? #false]
                [a? #true]
                [else (<<?* a b <<?)])))]))

;;;;;;;;;;;;;;;;;;;;
;; Reify Null

(define reify-null-extension
  (reify-extension null?
    (const 'empty)
    (const #false)
    (const #true)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify List

(define (reify-list xs reify q? <<?)
  (cons 'list
    (map reify xs)))

(define (list-quotable? xs quotable?)
  (andmap quotable? xs))

(define empty<<?
  (const #false))

(define cons<<?
  (lex<<?
    (map<<? first)
    (map<<? rest (eta list<<?))))

(define list<<?
  (dispatch<<? empty?
    empty<<?
    cons<<?))

(define reify-list-extension
  (reify-extension list?
    reify-list
    list<<?
    list-quotable?
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Improper List

(define (list*? x)
  (and (pair? x)
    (pair? (cdr x))))

(define (reify-list* x reify q? <<?)
  (list* 'list*
    (reify (car x))
    (let reify-tail {[tail (cdr x)]}
      (if (pair? tail)
        (cons (reify (car tail))
          (reify-tail (cdr tail)))
        (list (reify tail))))))

(define (list*-quotable? x quotable?)
  (and (quotable? (car x))
    (let tail-quotable? {[tail (cdr x)]}
      (if (pair? (cdr tail))
        (list*-quotable? tail quotable?)
        (pair-quotable? tail quotable?)))))

(define list*<<?
  (lex<<?
    (map<<? car)
    (map<<? cdr
      (dispatch<<? list*?
        (eta list*<<?)
        (eta pair<<?)))))

(define reify-list*-extension
  (reify-extension list*?
    reify-list*
    list*<<?
    list*-quotable?
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Pair

(define (reify-pair pair reify q? <<?)
  (list 'cons
    (reify (car pair))
    (reify (cdr pair))))

(define (pair-quotable? pair quotable?)
  (and
    (quotable? (car pair))
    (quotable? (cdr pair))))

(define pair<<?
  (lex<<?
    (map<<? car)
    (map<<? cdr)))

(define reify-pair-extension
  (reify-extension pair?
    reify-pair
    pair<<?
    pair-quotable?
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Vector

(define (vector-constructor-name vec)
  (if (immutable? vec)
    'vector-immutable
    'vector))

(define (reify-vector vec reify q? <<?)
  (cons (vector-constructor-name vec)
    (map reify (vector->list vec))))

(define (vector-quotable? vec quotable?)
  (and (immutable? vec)
    (for/and {[x (in-vector vec)]}
      (quotable? x))))

(define vector<<?
  (map<<? vector->list list<<?))

(define reify-vector-extension
  (reify-extension vector?
    reify-vector
    vector<<?
    vector-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Hash

(define (immutable-hash-constructor-name ht)
  (cond
    [(hash-eq? ht) 'hasheq]
    [(hash-eqv? ht) 'hasheqv]
    [(hash-equal? ht) 'hash]))

(define (hash-constructor-name ht)
  (cond
    [(immutable? ht)
     (cond
       [(hash-eq? ht) 'make-immutable-hasheq]
       [(hash-eqv? ht) 'make-immutable-hasheqv]
       [(hash-equal? ht) 'make-immutable-hash])]
    [(hash-weak? ht)
     (cond
       [(hash-eq? ht) 'make-weak-hasheq]
       [(hash-eqv? ht) 'make-weak-hasheqv]
       [(hash-equal? ht) 'make-weak-hash])]
    [else
     (cond
       [(hash-eq? ht) 'make-hasheq]
       [(hash-eqv? ht) 'make-hasheqv]
       [(hash-equal? ht) 'make-hash])]))

(define (reify-hash ht reify q? <<?)
  (define alist (sort (hash->list ht) <<? #:key car))
  (list (hash-constructor-name ht)
    (cons 'list
      (for/list {[kv (in-list alist)]}
        (list 'cons
          (reify (car kv))
          (reify (cdr kv)))))))

(define (hash-quotable? ht quotable?)
  (and (immutable? ht)
    (for/and {[(k v) (in-hash ht)]}
      (and (quotable? k) (quotable? v)))))

(define alist<<?
  (compose<<? list<<? pair<<?))

(define (sorted-alist<<? a b <<?)
  (define sa (sort a <<? #:key car))
  (define sb (sort b <<? #:key cdr))
  (alist<<? a b <<?))

(define hash<<?
  (lex<<?
    (category<<? immutable? hash-weak?)
    (category<<? hash-eq? hash-eqv? hash-equal?)
    (map<<? hash->list sorted-alist<<?)))

(define reify-hash-extension
  (reify-extension hash?
    reify-hash
    hash<<?
    hash-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Set

(define (set-constructor-name s)
  (cond
    [(set-eq? s) 'seteq]
    [(set-eqv? s) 'seteqv]
    [(set-equal? s) 'setequal]))

(define (reify-set s reify q? <<?)
  (cons (set-constructor-name s)
    (map reify
      (sort (set->list s) <<?))))

(define (sorted-list<<? a b <<?)
  (list<<? (sort a <<?) (sort b <<?) <<?))

(define set<<?
  (lex<<?
    (category<<? set-eq? set-eqv? set-equal?)
    (map<<? set->list sorted-list<<?)))

(define reify-set-extension
  (reify-extension set?
    reify-set
    set<<?
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Mutable Pair

(define (reify-mpair mpair reify q? <<?)
  (list 'mcons
    (reify (mcar mpair))
    (reify (mcdr mpair))))

(define mpair<<?
  (lex<<?
    (map<<? mcar)
    (map<<? mcdr)))

(define reify-mpair-extension
  (reify-extension mpair?
    reify-mpair
    mpair<<?
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Box

(define (box-constructor-name b)
  (if (immutable? b)
    'box-immutable
    'box))

(define (reify-box b reify q? <<?)
  (list (box-constructor-name b)
    (reify (unbox b))))

(define (box-quotable? b quotable?)
  (and (immutable? b)
    (quotable? (unbox b))))

(define box<<?
  (lex<<?
    (category<<? immutable?)
    (map<<? unbox)))

(define reify-box-extension
  (reify-extension box?
    reify-box
    box<<?
    box-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Syntax

(define (reify-syntax stx reify q? <<?)
  (define x (syntax->datum stx))
  (if (q? x)
    (list 'syntax x)
    (list 'datum->syntax '#f x)))

(define syntax<<?
  (map<<? syntax->datum))

(define reify-syntax-extension
  (reify-extension syntax?
    reify-syntax
    syntax<<?
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Prefab Struct

(define (reify-prefab x reify q? <<?)
  (list* 'make-prefab-struct
    (reify (prefab-key x))
    (map reify (prefab-fields x))))

(define (prefab-quotable? x quotable?)
  (and (quotable? (prefab-key x))
    (andmap quotable? (prefab-fields x))))

(define prefab<<?
  (lex<<?
    (map<<? prefab-key)
    (map<<? prefab-fields list<<?)))

(define reify-prefab-extension
  (reify-extension prefab?
    reify-prefab
    prefab<<?
    prefab-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Struct

(define (reify-struct x reify q? <<?)
  (match! x
    [(app struct->vector
       (vector (? symbol?
                  (app symbol->string
                    (regexp #rx"(?s:^struct:\\(.*\\)$)"
                      (list _ name))))
         es ...))
     (cons (string->symbol name)
       (map reify es))]))

(define struct<<?
  (map<<? struct->vector vector<<?))

(define reify-struct-extension
  (reify-extension struct?
    reify-struct
    struct<<?
    (const #false)
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Byte String

(define (reify-bytes b reify q? <<?)
  (if (immutable? b)
    b
    `(bytes-copy
       ,(bytes->immutable-bytes b))))

(define (bytes-quotable? b quotable?)
  (immutable? b))

(define bytes<<?
  (flat<<? bytes<?))

(define reify-bytes-extension
  (reify-extension bytes?
    reify-bytes
    bytes<<?
    bytes-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify String

(define (reify-string str reify q? <<?)
  (if (immutable? str)
    str
    (list 'string-copy str)))

(define (string-quotable? str quotable?)
  (immutable? str))

(define string<<?
  (flat<<? string<?))

(define reify-string-extension
  (reify-extension string?
    reify-string
    string<<?
    string-quotable?
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Symbol

(define (reify-symbol sym reify q? <<?)
  (cond
    [(symbol-interned? sym) `(quote ,sym)]
    [(symbol-unreadable? sym)
     (list 'string->unreadable-symbol
       (symbol->string sym))]
    [else (list 'string->uninterned-symbol
            (symbol->string sym))]))

(define (symbol-quotable? sym quotable?)
  (symbol-interned? sym))

(define symbol<<?
  (map<<? symbol->string string<<?))

(define reify-symbol-extension
  (reify-extension symbol?
    reify-symbol
    symbol<<?
    symbol-quotable?
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Keyword

(define (reify-keyword kwd reify q? <<?)
  `(quote ,kwd))

(define keyword<<?
  (map<<? keyword->string string<<?))

(define reify-keyword-extension
  (reify-extension keyword?
    reify-keyword
    keyword<<?
    (const #true)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Number

(define (reify-number n reify q? <<?) n)

(define zero<<?
  (constant<<? -0.0 0 +0.0))

(define real<<?
  (lex<<?
    (flat<<? <)
    (dispatch<<? zero?
      zero<<?
      (category<<? exact? inexact?))))

(define complex<<?
  (lex<<?
    (map<<? real-part real<<?)
    (map<<? imag-part real<<?)))

(define number<<?
  (dispatch<<? real? real<<? complex<<?))

(define reify-number-extension
  (reify-extension number?
    reify-number
    number<<?
    (const #true)
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Character

(define (reify-char c reify q? <<?) c)

(define char<<?
  (flat<<? char<?))

(define reify-char-extension
  (reify-extension char?
    reify-char
    char<<?
    (const #true)
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Boolean

(define (reify-boolean b reify q? <<?) b)

(define boolean<<?
  (constant<<? #false #true))

(define reify-boolean-extension
  (reify-extension boolean?
    reify-boolean
    boolean<<?
    (const #true)
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify PRegexp

(define (reify-regexp rx reify q? <<?)
  (if (pregexp? rx)
    (list 'pregexp
      (reify (object-name rx)))
    (list 'regexp
      (reify (object-name rx)))))

(define regexp<<?
  (lex<<?
    (category<<? pregexp?)
    (map<<? object-name string<<?)))

(define reify-regexp-extension
  (reify-extension regexp?
    reify-regexp
    regexp<<?
    (const #true)
    #true))

;;;;;;;;;;;;;;;;;;;;
;; Reify Void

(define reify-void-extension
  (reify-extension void?
    (const '(void))
    (const #false)
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Undefined

(define (undefined)
  (letrec {[x x]} x))

(define (undefined? x)
  (eq? x (undefined)))

(define reify-undefined-extension
  (reify-extension undefined?
    (const '(letrec {[undefined undefined]} undefined))
    (const #false)
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify EOF

(define reify-eof-extension
  (reify-extension eof-object?
    (const 'eof)
    (const #false)
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Path

(define (reify-path path reify q? <<?)
  (list 'bytes->path
    (reify (path->bytes path))
    (reify (path-convention-type path))))

(define path<<?
  (lex<<?
    (map<<? path-convention-type (constant<<? 'unix 'windows))
    (map<<? path->bytes bytes<<?)))

(define reify-path-extension
  (reify-extension path?
    reify-path
    path<<?
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;
;; Reify Promise

(define (reify-promise pr reify q? <<?)
  (list 'delay
    (reify (force pr))))

(define (promise<<? a b <<?)
  (<<? (force a) (force b)))

(define reify-promise-extension
  (reify-extension promise?
    reify-promise
    promise<<?
    (const #false)
    #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value to Expression Tables

(define empty-reify-value-table
  (reify-table empty #false))

(define default-reify-value-table
  (reify-table
    (list
      reify-null-extension
      reify-list-extension
      reify-list*-extension
      reify-pair-extension
      reify-vector-extension
      reify-hash-extension
      reify-set-extension
      reify-mpair-extension
      reify-box-extension
      reify-syntax-extension
      reify-prefab-extension
      reify-struct-extension
      reify-symbol-extension
      reify-keyword-extension
      reify-string-extension
      reify-bytes-extension
      reify-number-extension
      reify-char-extension
      reify-boolean-extension
      reify-regexp-extension
      reify-void-extension
      reify-undefined-extension
      reify-eof-extension
      reify-path-extension
      reify-promise-extension)
    #true))

(define current-reify-value-table
  (make-parameter default-reify-value-table))
