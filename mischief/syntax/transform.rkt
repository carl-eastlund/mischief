#lang racket/base

(provide
  quote-transformer
  rename-transformers
  rename-transformer
  macro-transformer
  id-transformer
  to-syntax
  to-datum
  fresh
  format-fresh
  fresh-mark
  syntax-local-variable-reference)

(require
  racket/function
  racket/bool
  racket/syntax
  syntax/parse
  syntax/srcloc
  (for-template
    racket/base)
  mischief/racket/visitor)

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
  ((datum-visitor) x))

(define (datum-visitor)
  (visitor-combine
    (make-memoizing-visitor)
    (make-uniform-visitor syntax?
      (lambda (recur stx)
        (recur (syntax-e stx))))
    map-visitor))

(define (quote-transformer x)
  #`(quasiquote #,(to-syntax ((quote-visitor) x))))

(define (quote-visitor)
  (visitor-combine
    (make-memoizing-visitor)
    (make-uniform-leaf-visitor syntax?
      (lambda (stx)
        #`(unquote (quote-syntax #,stx))))
    map-visitor
    (make-uniform-visitor path?
      (lambda (rec path)
        (with-syntax {[b (rec (path->bytes path))]
                      [t (rec (path-convention-type path))]}
          #'(unquote
              (bytes->path
                (quasiquote b)
                (quasiquote t))))))))

(define (fresh-mark)
  (make-syntax-introducer))

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
  (define n (add1 (hash-ref fresh:base->count sym0 0)))
  (define sym (if add-suffix? (format-symbol "~a~~~a" sym0 n) sym0))
  (define mark (make-syntax-introducer))
  (begin0 (mark (to-syntax sym #:source source))
    ;; update the number of times we've seen this symbol
    (hash-set! fresh:base->count sym0 n)
    ;; keep the base in the table as long as its derivatives exist
    (hash-set! fresh:sym->base sym sym0)))

(define fresh:base->count (make-weak-hasheq))
(define fresh:sym->base (make-weak-hasheq))

(define (transform stx f)
  (parameterize {[current-syntax-context stx]}
    (f stx)))

(define (self-transformer f)
  (lambda (stx)
    (syntax-parse stx
      [x:id (f #'x)]
      [((~literal set!) x:id e:expr) #`(set! #,(f #'x) e)]
      [(x:id . args) #`(#,(to-syntax '#%app #:stx stx) #,(f #'x) . args)])))

(define (macro-transform mt stx)
  (transform stx
    (macro-transformer-proc mt)))

(define (id-transform it stx)
  (transform stx
    (self-transformer
      (id-transformer-proc it))))

(define (rename-transform rt stx)
  (transform stx
    (self-transformer
      (const (rename-transformer-target rt)))))

(define (rename-transformers . ids)
  (apply values
    (map rename-transformer ids)))

(struct macro-transformer [proc]
  #:omit-define-syntaxes
  #:property prop:procedure macro-transform
  #:property prop:set!-transformer macro-transform)

(struct id-transformer [proc]
  #:omit-define-syntaxes
  #:property prop:procedure id-transform
  #:property prop:set!-transformer id-transform)

(struct rename-transformer [target]
  #:omit-define-syntaxes
  #:property prop:procedure rename-transform
  #:property prop:rename-transformer 0)
