#lang racket/base

(provide
  at-end
  define-single-definition
  define-if-unbound
  define-values-if-unbound
  define-syntax-if-unbound
  define-syntaxes-if-unbound
  define-provide-pre-syntax
  define-unimplemented
  unimplemented-out
  unimplemented)

(require
  (for-syntax
    racket/base
    racket/list
    racket/match
    racket/syntax
    syntax/parse
    racket/provide-transform)
  syntax/location
  syntax/srcloc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Provide Pre-Transformer Definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-provide-pre-syntax stx)
  (syntax-parse stx
    [(_ name:id proc:expr)
     #'(define-syntax name
         (make-provide-pre-transformer proc))]
    [(_ (name:id . args) . body)
     #'(define-syntax name
         (make-provide-pre-transformer
           (lambda args . body)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition Placeholders
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-unimplemented stx)
  (syntax-parse stx
    [(_ name:id ...)
     #`(define-syntaxes {name ...}
         (unimplemented-transformers #:source (quote-syntax #,stx)
           (quote-syntax name) ...))]))

(define-provide-pre-syntax (unimplemented-out stx modes)
  (syntax-parse stx
    [(_ name:id ...)
     (define/syntax-parse [temp ...]
       (generate-temporaries (attribute name)))
     (syntax-local-lift-module-end-declaration
       (syntax/loc stx (define-unimplemented temp ...)))
     #'(rename-out [temp name] ...)]))

(begin-for-syntax
  (define (unimplemented-transformers #:source [source #false] . name-stxs)
    (apply values
      (for/list {[name-stx (in-list name-stxs)]}
        (define source-stx
          (or source name-stx))
        (make-set!-transformer
          (lambda {stx}
            (define ref-stx
              (syntax-parse stx
                [(~or ({~literal set!} ~! ref:id _:expr) (ref:id . _) ref:id)
                 #:fail-unless (free-identifier=? #'ref name-stx)
                 (format "expected a reference to ~s" (syntax-e name-stx))
                 #'ref]))
            #`(raise-unimplemented-error
                (quote #,ref-stx)
                #:source (quote-srcloc #,stx)
                #:original-name (quote #,name-stx)
                #:original-source (quote-srcloc #,source-stx))))))))

(define (raise-unimplemented-error name
          #:source [source #false]
          #:original-name [original-name #false]
          #:original-source [original-source #false])
  (raise
    (exn:fail
      (format "~a: ~s~a~a~a"
        "cannot execute an unimplemented name"
        name
        (if source
          (format "\n  reference occurs at: ~a"
            (source-location->string source))
          "")
        (if (and original-name
              (not (eq? original-name name)))
          (format "\n  originally defined by name: ~a"
            original-name)
          "")
        (if original-source
          (format "\n  originally defined at: ~a"
            (source-location->string original-source))
          ""))
      (current-continuation-marks))))

(define-unimplemented unimplemented)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition Reordering
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (at-end stx)
  (syntax-case stx ()
    [(_ e ...)
     (match (syntax-local-context)
       ['module
         (begin
           (syntax-local-lift-module-end-declaration
            (syntax/loc stx (begin e ...)))
           (syntax/loc stx (begin)))]
       [ctx (wrong-syntax stx
                          "can only be used in module context; got: ~s"
                          ctx)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition Generalization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-single-definition stx)
  (syntax-parse stx
    [(define-single-definition define-one define-many)
     #'(define-syntax (define-one stx)
         (syntax-parse stx
           [(_ (head . args) . body) #'(define-one head (lambda args . body))]
           [(_ name expr) #'(define-many [name] expr)]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Potentially Redundant Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-many-if-unbound stx)
  (syntax-case stx []
    [(_ def [name ...] expr)
     (let* ([ids (syntax->list #'(name ...))])
       (for ([bad (in-list ids)] #:unless (identifier? bad))
         (wrong-syntax bad "expected an identifier"))
       (let*-values ([(bound unbound) (partition identifier-binding ids)])
         (cond
          [(null? bound) (syntax/loc stx (def [name ...] expr))]
          [(null? unbound) (syntax/loc stx (def [] (values)))]
          [else (wrong-syntax
                 stx
                 "conflicting definitions for ~s; none for ~s"
                 (map syntax-e bound)
                 (map syntax-e unbound))])))]))

(define-syntax-rule (define-values-if-unbound [name ...] expr)
  (define-many-if-unbound define-values [name ...] expr))

(define-single-definition define-if-unbound define-values-if-unbound)

(define-syntax-rule (define-syntaxes-if-unbound [name ...] expr)
  (define-many-if-unbound define-syntaxes [name ...] expr))

(define-single-definition define-syntax-if-unbound define-syntaxes-if-unbound)
