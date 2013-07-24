#lang racket/base

(provide
  @
  $
  $!
  syntax-matcher
  syntax-matches?
  syntax-class/c
  syntax-parse/c
  formals
  kw-formals
  for-clauses
  for-body
  block-body
  self-quoting
  datum-literal
  literal
  module-path
  temp-id
  bound-id
  static-id
  static-binding
  struct-binding
  struct-binding/known
  struct-binding/check
  define-literals/ids
  require/define-literals/ids)

(require
  (for-syntax
    racket/base
    racket/list
    racket/match
    racket/require-transform
    racket/syntax
    syntax/parse)
  racket/contract
  racket/function
  racket/list
  racket/bool
  racket/syntax
  racket/struct-info
  syntax/parse
  syntax/parse/experimental/specialize
  syntax/parse/experimental/template
  mischief/list
  mischief/boolean
  mischief/maybe
  mischief/scope
  (only-in srfi/1 list-index))

(define-syntax @
  (make-rename-transformer #'attribute))

(define-syntax $
  (make-rename-transformer #'template))

(define-syntax $!
  (make-rename-transformer #'quote-syntax))

(define-syntax (syntax-class/c stx)
  (syntax-parse stx
    [(_ class:id)
     #'(flat-named-contract '(syntax-class/c class)
         (lambda (x)
           (and (syntax? x)
             (syntax-matches? x (~var _ class)))))]))

(define-syntax (syntax-parse/c stx)
  (syntax-parse stx
    [(_ pat ...)
     #'(flat-named-contract '(syntax-parse/c pat ...)
         (lambda (x)
           (and (syntax? x)
             (syntax-matches? x pat ...))))]))

(define-syntax (syntax-matcher stx)
  (syntax-parse stx
    [(_ pat ...)
     #'(syntax-parser [pat #true] ... [_ #false])]))

(define-syntax (syntax-matches? stx)
  (syntax-parse stx
    [(_ e:expr pat ...)
     #'(syntax-parse e [pat #true] ... [_ #false])]))

(define-syntax (define-literals/ids stx)
  (syntax-parse stx
    [(_ base-name:id opt ... [ref:id ...])
     (define/syntax-parse literals-name
       (format-id #'base-name "~a-literals" #'base-name))
     (define/syntax-parse ids-name
       (format-id #'base-name "~a-ids" #'base-name))
     #'(begin
         (define-literal-set literals-name opt ... [ref ...])
         (define ids-name (list (quote-syntax/prune ref) ...)))]))

(define-syntax (require/define-literals/ids stx)
  (syntax-parse stx
    [(_ base-name:id require-spec)
     (define-values [imports sources]
       (expand-import #'require-spec))
     (define phases
       (map import-mode imports))
     (define phase
       (cond
         [(empty? phases) 0]
         [(empty? (rest phases)) (first phases)]
         [else (let* {[p0 (first phases)]}
                 (for/first {[p (in-list (rest phases))]
                             #:unless (eqv? p p0)}
                   (raise-syntax-error #f
                     (format "conflicting phases in imports: ~v vs. ~v" p p0)
                     stx))
                 p0)]))
     (define/syntax-parse [ref ...]
       (map import-local-id imports))
     #`(begin
         (require require-spec)
         (define-literals/ids base-name
           #:phase #,phase
           [ref ...]))]))

(define-syntax-class self-quoting
  (pattern (~not (~or _:id _:keyword (_ . _)))))

(define-syntax-class formals
  (pattern (arg-id:id ... . (~and rest (~or rest-id?:id ())))
    #:attr [rest-id 1]
    (cond
      [(attribute rest-id?) => list]
      [else empty])
    #:attr [formal-id 1]
    (syntax->list
      #'[arg-id ... rest-id ...])
    #:attr call
    (cond
      [(not (attribute rest-id?)) #'#%app]
      [else #'apply])))

(define-syntax-class kw-formals
  (pattern ((~or
              req-id:id
              [opt-id:id opt-expr:expr]
              (~seq req-kw:keyword req-kw-id:id)
              (~seq opt-kw:keyword [opt-kw-id:id opt-kw-expr:expr]))
            ...
            . (~and rest (~or rest-id?:id ())))
    #:attr [rest-id 1]
    (cond
      [(attribute rest-id?) => list]
      [else empty])
    #:attr [formal-id 1]
    (syntax->list
      #'[req-id ... req-kw-id ... opt-id ... opt-kw-id ... rest-id ...])
    #:attr call
    (cond
      [(not (attribute rest-id?)) #'#%app]
      [(and (empty? (attribute req-kw)) (empty? (attribute opt-kw)) #'apply)]
      [else #'keyword-apply])))

(define-syntax-class for-clauses
  #:attributes {}
  (pattern {clause:for-clause ...}
    #:fail-when
    (check-duplicate-identifier (append* (@ clause.name)))
    "duplicate identifier"))

(define-splicing-syntax-class for-clause
  #:attributes {[name 1]}
  (pattern [var:id _:expr] #:attr [name 1] (list (@ var)))
  (pattern [(name:id ...) _:expr])
  (pattern (~seq #:when _:expr) #:attr [name 1] '())
  (pattern (~seq #:unless _:expr) #:attr [name 1] '())
  (pattern _:break-clause #:attr [name 1] '()))

(define-splicing-syntax-class break-clause
  (pattern (~seq #:break _:expr))
  (pattern (~seq #:final _:expr)))

(define-syntax-class for-body
  (pattern
    ((~and (~seq head ...)
       (~seq (~or _:break-clause _:expr) ...))
     tail:expr)))

(define-syntax-class block-body
  (pattern (_:expr ...+)))

(define-syntax-class temp-id
  #:attributes {temp}
  (pattern x #:attr temp (generate-temporary #'x))
  (pattern _ #:attr temp (generate-temporary)))

(define-syntax-class (datum-literal datum-pred datum-desc)
  #:description datum-desc
  #:attributes {value}
  (pattern x
    #:attr value (syntax->datum #'x)
    #:when (datum-pred (attribute value))))

(define-syntax-class/specialize (literal v)
  (datum-literal (lambda {x} (equal? x v)) (format "~s" v)))

(define-syntax-class/specialize module-path
  (datum-literal module-path? "a module path"))

(define-syntax-class bound-id
  #:description "a bound identifier"
  #:attributes {}
  (pattern x:id
    #:fail-unless (identifier-binding #'x)
    "expected a bound identifier, but found an unbound identifier"))

(define-syntax-class (static-binding static-pred static-desc)
  #:description static-desc
  #:attributes {value delta}
  (pattern x
    #:fail-unless (identifier? (@ x))
    (format "expected ~a, but found ~a"
      static-desc "a non-identifier")
    #:fail-unless (identifier-binding (@ x))
    (format "expected ~a, but found ~a"
      static-desc "an unbound identifier")
    #:do {(define maybe (scope-static-value (@ x) #:success yes #:failure no))}
    #:fail-unless (yes? maybe)
    (format "expected ~a, but found ~a"
      static-desc "an identifier bound as a value")
    #:attr value (yes-value maybe)
    #:fail-unless (static-pred (attribute value))
    (format "expected ~a, but found ~a"
      static-desc
      (format "an identifier bound as syntax to ~v"
        (attribute value)))
    #:attr delta
    ;; do not compute delta until we need it
    (let* {[proc #false]
           [scope (current-scope)]}
      (lambda (stx)
        (unless proc
          (set! proc
            (scope-delta-introducer (@ x)
              #:scope scope)))
        (proc stx)))))

(define-syntax-class/specialize static-id
  (static-binding (const #true) "an identifier bound as syntax"))

(define-syntax-class (struct-binding/check
                       #:all [all #false]
                       #:descriptor [known-descriptor? all]
                       #:constructor [known-constructor? all]
                       #:predicate [known-predicate? all]
                       #:fields [known-accessors? all]
                       #:super [known-super? all]
                       #:mutable [known-mutators? #false])
  #:attributes {value
                descriptor-id
                constructor-id
                predicate-id
                [accessor-id 1]
                [mutator-id 1]
                super-id
                known-fields?}
  (pattern (~var type (static-binding struct-info? "struct type name"))

    #:do {(define info (extract-struct-info (attribute type.value)))}

    #:attr value info
    #:attr descriptor-id (first info)
    #:attr constructor-id (second info)
    #:attr predicate-id (third info)
    #:attr [accessor-id 1] (reverse (take-while identifier? (fourth info)))
    #:attr [mutator-id 1] (reverse (take (fifth info)
                                     (length (attribute accessor-id))))
    #:attr super-id (sixth info)
    #:attr known-fields? (= (length (fourth info))
                            (length (attribute accessor-id)))

    #:fail-unless (implies known-descriptor? (attribute descriptor-id))
    "struct type descriptor name unknown"
    #:fail-unless (implies known-constructor? (attribute constructor-id))
    "struct constructor name unknown"
    #:fail-unless (implies known-predicate? (attribute predicate-id))
    "struct predicate name unknown"
    #:fail-unless (implies (or known-accessors? known-mutators?)
                    (attribute known-fields?))
    "struct may have unknown fields"
    #:fail-unless (implies known-mutators?
                    (andmap identifier? (attribute mutator-id)))
    (format "struct has missing or unknown mutator for field ~s"
      (syntax-e
        (list-ref (attribute accessor-id)
          (list-index false? (attribute mutator-id)))))))

(define-syntax-class/specialize struct-binding
  (struct-binding/check #:all #false))

(define-syntax-class/specialize struct-binding/known
  (struct-binding/check #:all #true))
