#lang racket/base

(require
  racket/contract
  (for-syntax racket/base)
  (for-template racket/base))

(provide
  (contract-out
    [to-syntax
     (->*
         {any/c}
         {#:stx (or/c syntax? #false)
          #:source source-location?
          #:context (or/c syntax? #false)
          #:prop (or/c syntax? #false)}
       syntax?)]
    [to-datum (-> any/c (not/c syntax?))]
    [fresh
     (->*
         {}
         {(or/c string? symbol? identifier? keyword? char? number?)
          #:add-suffix? boolean?
          #:source source-location?}
       identifier?)]
    [format-fresh
     (->*
         {string?}
         {#:add-suffix? boolean?
          #:source source-location?}
       #:rest (listof (or/c string? symbol? identifier? keyword? char? number?))
       identifier?)]
    [id-transform (-> syntax? (or/c syntax? (-> identifier? syntax?)) syntax?)]
    [fresh-mark (-> (-> syntax? syntax?))]
    [identifier-upcase (-> identifier? identifier?)]
    [identifier-downcase (-> identifier? identifier?)]
    [identifier-titlecase (-> identifier? identifier?)]
    [identifier-foldcase (-> identifier? identifier?)]
    [syntax-local-variable-reference (-> variable-reference?)]
    [check-missing-identifier
     (-> (listof identifier?) (listof identifier?) (or/c identifier? #false))])
  quote-transformer
  rename-transformers
  rename-transformer
  set!-transformer
  id-transformer
  (rename-out [wrong-syntax syntax-error]))

(require
  racket/function
  racket/bool
  racket/dict
  racket/syntax
  syntax/parse
  syntax/srcloc
  syntax/id-table
  mischief/fold
  mischief/visitor
  mischief/scope
  (for-template
    racket/base))

(define (check-missing-identifier actual expected)
  (define table (make-free-id-table))
  (for {[id (in-list expected)]}
    (dict-set! table id #true))
  (for/first {[id (in-list actual)]
              #:unless (dict-has-key? table id)}
    id))

(define (syntax-local-variable-reference)
  (syntax-local-eval
    #'(#%variable-reference)))

(define (to-syntax x #:stx [stx #f]
          #:context [context stx]
          #:source [source stx]
          #:prop [prop stx]
          #:cert [cert stx])
  (check-syntax/false! '|to-syntax [context argument]| context)
  (check-source-location! '|to-syntax [source argument]| source)
  (check-syntax/false! '|to-syntax [properties argument]| prop)
  (check-syntax/false! '|to-syntax [(obsolete) certificates argument]| cert)
  (datum->syntax context x (build-source-location-list source) prop))

(define (check-syntax/false! name x)
  (unless (or (syntax? x) (false? x))
    (raise-type-error name "syntax or #false" x)))

(define (to-datum x)
  (define ht (make-weak-hasheq))
  (datum-fold x
    #:short-circuit (lambda (v f) (hash-ref! ht v f))
    #:syntax (lambda (stx datum) datum)))

(define (quote-transformer x)
  #`(quasiquote #,(to-syntax ((quote-visitor) x))))

(define (quote-visitor)
  (visitor-combine
    (make-memoizing-visitor)
    (make-uniform-leaf-visitor syntax?
      (lambda (stx)
        #`(unquote (quote-syntax #,(to-syntax stx)))))
    (make-uniform-leaf-visitor path?
      (lambda (path)
        #`(unquote
            (bytes->path
              (quote #,(path->bytes path))
              (quote #,(path-convention-type path))))))
    (make-visitor hash?
      (lambda (rec ht)
        #`(unquote
            (#,(hash-maker ht)
             (quasiquote #,(rec (hash->list ht)))))))
    map-visitor))

(define (hash-maker ht)
  (cond
    [(immutable? ht)
     (cond
       [(hash-equal? ht) #'make-immutable-hash]
       [(hash-eqv? ht) #'make-immutable-hasheqv]
       [(hash-eq? ht) #'make-immutable-hasheq])]
    [(hash-weak? ht)
     (cond
       [(hash-equal? ht) #'make-weak-hash]
       [(hash-eqv? ht) #'make-weak-hasheqv]
       [(hash-eq? ht) #'make-weak-hasheq])]
    [else
     (cond
       [(hash-equal? ht) #'make-hash]
       [(hash-eqv? ht) #'make-hasheqv]
       [(hash-eq? ht) #'make-hasheq])]))

(define (fresh-mark)
  (make-syntax-introducer))

(define (identifier-rename rename id0)
  (define sym0 (syntax-e id0))
  (define str0 (symbol->string sym0))
  (define str (rename str0))
  (define sym (string->symbol str))
  (define id (to-syntax #:stx id0 sym))
  id)

(define (identifier-upcase id)
  (identifier-rename string-upcase id))

(define (identifier-downcase id)
  (identifier-rename string-downcase id))

(define (identifier-titlecase id)
  (identifier-rename string-titlecase id))

(define (identifier-foldcase id)
  (identifier-rename string-foldcase id))

(define (fresh [x 'fresh]
          #:add-suffix? [add-suffix? #true]
          #:source [source (if (syntax? x) x #false)])
  (format-fresh "~a" x
    #:add-suffix? add-suffix?
    #:source source))

(define (format-fresh
          #:add-suffix? [add-suffix? #true]
          #:source [source #false]
          fmt . args)
  (define sym0 (apply format-symbol fmt args))
  (define sym
    (cond
      [add-suffix?
       (define n (add1 (hash-ref fresh:base->count sym0 0)))
       (begin0 (format-symbol "~a~~~a" sym0 n)
         ;; update the number of times we've seen this symbol
         (hash-set! fresh:base->count sym0 n)
         ;; keep the base in the table as long as its derivatives exist
         (hash-set! fresh:sym->base sym sym0))]
      [else sym0]))
  ((fresh-mark) (to-syntax sym #:source source)))

(define (unintern sym)
  (string->uninterned-symbol
    (symbol->string sym)))

(define fresh:base->count (make-weak-hasheq))
(define fresh:sym->base (make-weak-hasheq))

(define (transform stx f)
  (parameterize {[current-syntax-context stx]}
    (f stx)))

(define (self-transform f stx)
  (syntax-parse stx
    [x:id (f #'x)]
    [((~literal set!) x:id e:expr)
     (to-syntax #:stx stx
       (list #'set! (f #'x) #'e))]
    [(x:id . args)
     (to-syntax #:stx stx
       (list* '#%app (f #'x) #'args))]))

(define (self-transformer f)
  (lambda (stx)
    (self-transform f stx)))

(define (do-macro-transform mt stx)
  (transform stx
    (set!-transformer-proc mt)))

(define (do-id-transform it stx)
  (transform stx
    (self-transformer
      (id-transformer-proc it))))

(define (do-rename-transform rt stx)
  (transform stx
    (self-transformer
      (const (rename-transformer-target rt)))))

(define (rename-transformers . ids)
  (apply values
    (map rename-transformer ids)))

(define (id-transform stx proc/stx)
  (define proc
    (cond
      [(procedure? proc/stx) proc/stx]
      [(syntax? proc/stx) (const proc/stx)]
      [else (error 'id-transform
              "expected second argument to be a procedure or syntax, got ~v"
              proc/stx)]))
  (self-transform proc stx))

(struct set!-transformer [proc]
  #:omit-define-syntaxes
  #:property prop:procedure do-macro-transform
  #:property prop:set!-transformer do-macro-transform)

(struct id-transformer [proc]
  #:omit-define-syntaxes
  #:property prop:procedure do-id-transform
  #:property prop:set!-transformer do-id-transform)

(struct rename-transformer [target]
  #:omit-define-syntaxes
  #:property prop:procedure do-rename-transform
  #:property prop:rename-transformer 0)
