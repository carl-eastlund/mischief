#lang racket/base

(provide
  update
  prefab?
  prefab-of-key?
  prefab
  prefab-key
  prefab-ref
  prefab-type-name
  prefab-fields
  normalize-prefab-key
  transparent-struct?
  transparent-struct-type
  transparent-struct-predicate
  transparent-struct-constructor
  transparent-struct-ref
  transparent-struct-type-name
  transparent-struct-fields)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    mischief/parse)
  racket/match
  racket/list
  mischief/list)

(define-syntax update
  (syntax-parser
    [(form-id:id
       (~var struct-id
         (struct-binding/check
           #:predicate #t
           #:constructor #t
           #:fields #t))
       value-expr:expr [lhs-id:id rhs-expr:expr] ...)

     #:attr [lhs-get-id 1]
     (for/list {[id-stx (in-list (attribute lhs-id))]}
       (format-id id-stx "~a-~a" #'struct-id id-stx))

     #:fail-when
     (for/first
         {[lhs-stx (in-list (attribute lhs-id))]
          [lhs-get-stx (in-list (attribute lhs-get-id))]
          #:unless
          (for/or {[get-stx (in-list (attribute struct-id.accessor-id))]}
            (free-identifier=? lhs-get-stx get-stx))}
       lhs-stx)
     "struct has no corresponding field"

     (define/syntax-parse value-id
       (generate-temporary #'struct-id))

     (define/syntax-parse [field-expr ...]
       (for/list {[get-stx (in-list (attribute struct-id.accessor-id))]}
         (or
           (for/first {[lhs-get-stx (in-list (attribute lhs-get-id))]
                       [rhs-stx (in-list (attribute rhs-expr))]
                       #:when (free-identifier=? get-stx lhs-get-stx)}
             rhs-stx)
           (with-syntax {[get-id get-stx]}
             #'(get-id value-id)))))

     (with-syntax {[struct-str (symbol->string (syntax-e #'struct-id))]}
       #'(let* {[value-id value-expr]}
           (unless (struct-id.predicate-id value-id)
             (raise-type-error 'form-id 'struct-str value-id))
           (let {[lhs-id (lhs-get-id value-id)] ...}
             (struct-id.constructor-id field-expr ...))))]))

(define (prefab? x)
  (if (prefab-struct-key x) #t #f))

(define (prefab-of-key? k x)
  (equal?
    (normalize-prefab-key k)
    (prefab-struct-key x)))

(define-match-expander prefab
  (syntax-parser
    [(_ key fd ...)
     #'(and
         (app prefab-struct-key
           (and (not #f) key))
         (app struct->vector
           (vector _ fd ...)))])
  (syntax-parser
    [x:id #'make-prefab]
    [(~and app (x:id . args))
     (datum->syntax #'app
       (cons #'make-prefab #'args))]))

(define (make-prefab key . fields)
  (apply make-prefab-struct key fields))

(define (prefab-key x)
  (if (prefab? x)
    (prefab-struct-key x)
    (error 'prefab-key
      "expected a prefab struct, got: ~v" x)))

(define (prefab-ref x i)
  (vector-ref (struct->vector x) (add1 i)))

(define (prefab-type-name x)
  (if (prefab? x)
    (let* {[k (prefab-key x)]}
      (if (symbol? k) k (first k)))
    (error 'prefab-type-name
      "expected a prefab struct, got: ~v" x)))

(define (prefab-fields x)
  (if (prefab? x)
    (for/list {[x (in-vector (struct->vector x) 1)]} x)
    (error 'prefab-fields
      "expected a prefab struct, got: ~v" x)))

(define (normalize-prefab-key key)
  (prefab-key (prefab key)))

(define (expose-transparent-struct x inspector success failure)
  (parameterize {[current-inspector inspector]}
    (define unique (gensym))
    (define vec (struct->vector x unique))
    (define fields (for/list {[e (in-vector vec 1)]} e))
    (define-values {type skipped?} (struct-info x))
    (if (and type (not skipped?) (not (memq? unique fields)))
      (success type fields)
      (failure))))

(define (transparent-struct? x
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields) #true)
    (lambda () #false)))

(define (transparent-struct-type x
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields) type)
    (lambda ()
      (error 'transparent-struct-type
        "cannot determine precise struct type of: ~v" x))))

(define (transparent-struct-predicate x
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields) (struct-type-make-predicate type))
    (lambda ()
      (error 'transparent-struct-predicate
        "cannot determine precise struct type of: ~v" x))))

(define (transparent-struct-constructor x
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields) (struct-type-make-constructor type))
    (lambda ()
      (error 'transparent-struct-constructor
        "cannot determine precise struct type of: ~v" x))))

(define (transparent-struct-ref x i
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields) (list-ref fields i))
    (lambda ()
      (error 'transparent-struct-constructor
        "cannot determine precise field contents of: ~v" x))))

(define (transparent-struct-type-name x
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields)
      (define-values {name inits autos get set imms super skipped?}
        (struct-type-info type))
      name)
    (lambda ()
      (error 'transparent-struct-type-name
        "cannot determine precise struct type of: ~v" x))))

(define (transparent-struct-fields x
          #:inspector [inspector (current-inspector)])
  (expose-transparent-struct x inspector
    (lambda (type fields) fields)
    (lambda ()
      (error 'transparent-struct-fields
        "cannot determine precise field contents of: ~v" x))))
