#lang racket/base

(provide
  define-component-description
  define-generic-component
  define-component-instance
  define-component
  declare-component
  declare-contracted
  declare-contracted-values
  declare)

(require
  (for-syntax
    racket/base
    racket/match
    racket/function
    racket/syntax
    syntax/parse
    syntax/parse/experimental/specialize
    mischief/function
    mischief/for
    mischief/list
    mischief/parse
    mischief/scope
    mischief/transform)
  mischief/shorthand)

(begin-for-syntax

  (struct static-description
    [desc-name
     stx-labels
     stx-names
     stx-bodies
     val-labels
     val-names
     ctc-fun])

  (struct static-component
    [stx-name
     val-name
     description])

  (struct static-generic
    [stx-name
     val-name
     domains
     range])

  (define (expand-declarations stxs)
    (for/append {[stx (in-list stxs)]}
      (expand-declaration stx)))

  (define (expand-declaration stx)
    (let* {[stx (head-expand-declaration stx)]}
      (syntax-parse stx
        [((~literal define-syntaxes) {name:id ...} body:expr)
         (define/syntax-parse expanded:expr
           (expand-in-scope-for-syntax (@ body)))
         (scope-bind-syntaxes/eval! (@ name) (@ expanded))
         (list stx)]
        [((~literal declare-contracted-values) [name:id ctc:expr] ...)
         (scope-bind-values! (@ name))
         (define/syntax-parse [expanded:expr ...]
           (map expand-expression-in-scope (@ ctc)))
         (list
           (to-syntax #:stx stx
             (list* #'declare-contracted-values
               (map list (@ name) (@ expanded)))))]
        [((~literal begin) body:expr ...)
         (expand-declarations (@ body))])))

  (define (bind-component-syntax desc #:dotted [prefix #false])

    (define label->id
      (cond
        [prefix (arg+ dotted prefix)]
        [else identity]))

    (match desc
      [(static-description
         _
         stx-labels
         stx-names
         stx-bodies
         val-labels
         val-names
         _)

       (define/syntax-parse {[val-name ...] ...} val-names)
       (define/syntax-parse {[stx-name ...] ...} stx-names)
       (define/syntax-parse {[val-label ...] ...}
         (map-map label->id val-labels))
       (define/syntax-parse {[stx-label ...] ...}
         (map-map label->id stx-labels))
       (define/syntax-parse {stx-body ...} stx-bodies)

       #'(begin
           (define-syntaxes {val-name ... ... stx-name ... ...}
             (rename-transformers #'val-label ... ... #'stx-label ... ...))
           (define-syntaxes {stx-label ...} stx-body) ...)]))

  (define (quote-description desc)
    (match desc
      [(static-description
         desc-name
         stx-labels
         stx-names
         stx-bodies
         val-labels
         val-names
         ctc-fun)
       (define/syntax-parse name desc-name)
       (define/syntax-parse {[stx-label ...] ...} stx-labels)
       (define/syntax-parse {[stx-name ...] ...} stx-names)
       (define/syntax-parse {stx-body ...} stx-bodies)
       (define/syntax-parse {[val-label ...] ...} val-labels)
       (define/syntax-parse {[val-name ...] ...} val-names)
       (define/syntax-parse fun ctc-fun)
       #'(static-description
           #'name
           (list (list #'stx-label ...) ...)
           (list (list #'stx-name ...) ...)
           (list stx-body ...)
           (list (list #'val-label ...) ...)
           (list (list #'val-name ...) ...)
           #'fun)]))

  (define (head-expand-declaration stx)
    (expand-in-scope stx
      #:stop-at
      (list
        #'begin
        #'define-syntaxes
        #'declare-contracted-values)))

  (define (dotted base-id label-id)
    (format-id label-id #:source base-id "~a.~a" base-id label-id))

  (define-splicing-syntax-class generic-formals
    #:attributes {[name 1] [domain 1]}
    (pattern (~seq arg-name #:: desc:description-spec)
      #:attr [name 1] (list (@ arg-name))
      #:attr [domain 1] (list (@ desc.value)))
    (pattern (~seq [name:id #:: desc:description-spec] ...)
      #:attr [domain 1] (@ desc.value)))

  (define-splicing-syntax-class description-spec
    #:attributes {value}
    (pattern :description-id))

  (define-syntax-class/specialize description-id
    (static-binding static-description?
      "the name of a component description"))

  (define-syntax-class/specialize component-id
    (static-binding static-component?
      "the name of a component"))

  (define-syntax-class/specialize generic-id
    (static-binding static-generic?
      "the name of a generic component"))

  (define (declaration-transformer stx)
    (wrong-syntax stx
      "must only be used inside define-component-description")))

(define-syntax declare-component declaration-transformer)
(define-syntax declare-contracted-values declaration-transformer)

(define-shorthand (declare-contracted name:id ctc:expr)
  (declare-contracted-values [name ctc]))

(define-shorthand (declare name:id ...)
  (declare-contracted-values [name any/c] ...))

(define-syntax (define-component-description stx)
  (syntax-parse stx
    [(_ desc-name:id body:expr ...)
     (with-new-scope

       (define expanded
         (map syntax-local-introduce
           (expand-declarations
             (map syntax-local-introduce
               (@ body)))))

       (syntax-parse expanded
         [{(~or
             ((~literal define-syntaxes) {stx-name:id ...} stx-body:expr)
             ((~literal declare-contracted-values)
              [val-name:id val-ctc:expr]
              ...))
           ...}
          (define/syntax-parse {[stx-label ...] ...}
            (map-map out-of-scope (@ stx-name)))
          (define/syntax-parse {[val-label ...] ...}
            (map-map out-of-scope (@ val-name)))
          #'(begin
              (define (build-contracts val-name ... ...)
                (define-syntaxes {stx-name ...} stx-body) ...
                (values val-ctc ... ...))
              (define-syntax desc-name
                (static-description
                  (quote-syntax desc-name)
                  (list (list (quote-syntax stx-label) ...) ...)
                  (list (list (quote-syntax stx-name) ...) ...)
                  (list (quote-syntax stx-body) ...)
                  (list (list (quote-syntax val-label) ...) ...)
                  (list (list (quote-syntax val-name) ...) ...)
                  #'build-contracts)))]))]))

(define-syntax (define-component stx)
  (syntax-parse stx
    [(_ comp-name:id #:: desc:description-spec body:expr ...)
     (with-new-scope
       (define static-desc (@ desc.value))
       (define/syntax-parse desc-name
         (syntax-local-introduce
           (quote-description static-desc)))
       (define/syntax-parse internal
         (syntax-local-introduce
           (bind-component-syntax static-desc)))
       (define/syntax-parse external
         (syntax-local-introduce
           (bind-component-syntax static-desc
             #:dotted (syntax-local-introduce #'comp-name))))

       (define/syntax-parse {[label ...] ...}
         (map-map syntax-local-introduce
           (static-description-val-labels static-desc)))
       (define/syntax-parse {[comp-name.label ...] ...}
         (map-map (arg+ dotted #'comp-name) (@ label)))

       #'(begin
           (define-syntax comp-name
             (static-component
               #'comp-name
               #'comp-value
               quoted-desc))
           (define comp-value
             (block
               internal
               body ...
               (make-component label ... ...)))
           (define-values {comp-name.label ... ...}
             (component->values comp-value))
           external))]))

(define-syntax (define-component-instance stx)
  (syntax-parse stx
    [(_ inst-name:id (gen-name:generic-id arg-name:component-id ...))

     (define args (@ arg-name.value))
     (define gen (@ gen-name.value))
     (define range (static-generic-range gen))

     (define/syntax-parse gen-value (static-generic-val-name gen))
     (define/syntax-parse [arg-value ...] (map static-component-val-name args))
     (define/syntax-parse quoted-range
       (quote-description range))
     (define/syntax-parse {[inst-name.label ...] ...}
       (map-map (arg+ dotted #'inst-name)
         (map-map syntax-local-introduce
           (static-description-val-labels range))))
     (define/syntax-parse external
       (bind-component-syntax range
         #:dotted #'inst-name))

     #'(begin
         (define-syntax inst-name
           (static-component
             #'inst-name
             #'inst-value
             quoted-range))
         (define inst-value
           (gen-value arg-value ...))
         (define-values {inst-name.label ... ...}
           (component->values inst-value))
         external)]))

(define-syntax (define-generic-component stx)
  (syntax-parse stx
    [(_ (gen-name:id args:generic-formals)
        #:: range:description-spec
        body:expr ...)

     (define/syntax-parse [arg-value ...] (map fresh (@ args.name)))
     (define/syntax-parse [quoted-domain ...]
       (map syntax-local-introduce
         (map quote-description (@ args.domain))))
     (define/syntax-parse quoted-range
       (syntax-local-introduce
         (quote-description (@ range.value))))

     (define/syntax-parse {[val-label ...] ...}
       (map-map syntax-local-introduce
         (static-description-val-labels (@ range.value))))

     (define/syntax-parse {[(arg-name.label ...) ...] ...}
       (for/list {[name-stx (in-list (@ args.name))]
                  [domain (in-list (@ args.domain))]}
         (map-map (arg+ dotted name-stx)
           (map-map syntax-local-introduce
             (static-description-val-labels domain)))))

     (define/syntax-parse [import ...]
       (for/list {[name-stx (in-list (@ args.name))]
                  [domain (in-list (@ args.domain))]}
         (syntax-local-introduce
           (bind-component-syntax domain #:dotted name-stx))))

     (define/syntax-parse internal
       (syntax-local-introduce
         (bind-component-syntax (@ range.value))))

     #'(begin
         (define-syntax gen-name
           (static-generic
             #'gen-name
             #'gen-value
             (list quoted-domain ...)
             quoted-range))
         (define (gen-value arg-value ...)
           (define-syntaxes {args.name ...}
             (values
               (static-component
                 #'args.name
                 #'arg-value
                 quoted-domain)
               ...))
           (define-values {arg-name.label ... ...}
             (component->values arg-value))
           ...
           import
           ...
           internal
           body
           ...
           (make-component val-label ... ...)))]))

(module* test mischief

  (require (submod "..") rackunit)

  (define-component-description TYPE
    (declare-contracted c contract?))

  (define-component-description EQUALITY-TYPE #:extends TYPE
    (declare-contracted =? (-> c c boolean?)))

  (define-component-description SET #:extends TYPE
    (declare-component Elem TYPE)
    (declare-contracted empty (-> c))
    (declare-contracted singleton (-> Elem.c c))
    (declare-contracted union (-> c c c))
    (declare-contracted member? (-> c Elem.c boolean?))
    (define-shorthand make
      [(_) (empty)]
      [(_ x) (singleton x)]
      [(_ x . rest) (union (make x) (make . rest))]))

  (define-generic-component (Set-of E #:: TYPE) #:: SET #:where Elem #:= E
    (define c (listof E.c))
    (define empty '())
    (define (singleton s) (list x))
    (define (union s1 s2) (append s1 s2))
    (define (member? x s) (for/or {[y (in-list s)]} (E.=? x y))))

  (define-component Int #:: EQUALITY-TYPE
    (define c integer?)
    (define =? =))

  (define-component-instance Set-of-Int (Set-of Int))

  (check-equal? (Set-of-Int.make 1 2 3) (list 1 2 3))
  (check-exn exn:fail:contract?
    (lambda {} (Set-of-Int.make 1/2)))
  (check-exn exn:fail:contract?
    (lambda {} (Set-of-Int.member? 1/2 (Set-of-Int.make 1 2 3)))))
