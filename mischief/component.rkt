#lang racket/base

(provide
  define-component-description
  define-generic-component
  define-component-instance
  define-component
  declare-component
  declare-contracted
  declare-contracted-values
  declare
  declare-values)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    mischief/for
    mischief/list
    mischief/parse
    mischief/scope
    mischief/transform))

(begin-for-syntax

  (struct static-description
    [desc-name
     stx-labels
     stx-names
     stx-bodies
     val-labels
     val-names
     ctc-fun])

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

  (define (head-expand-declaration stx)
    (expand-in-scope stx
      #:stop-at
      (list
        #'begin
        #'define-syntaxes
        #'declare-contracted-values))))

(define-syntax (declare-contracted-values stx)
  (wrong-syntax stx
    "must only be used inside define-component-description"))

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
            (map-map (out-of-scope (@ stx-name))))
          (define/syntax-parse {[val-label ...] ...}
            (map-map (out-of-scope (@ val-name))))
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

(define-syntax (define-component-instance stx)
  (syntax-parse stx
    [(_ comp-name:id #:: desc:description body:expr ...)
     (with-new-scope
       (match (@ desc.value)
         [(static-description _ slabs sids sexprs vlabs vids cfun)
          (define/syntax-parse {[stx-label ...] ...} slabs)
          (define/syntax-parse {[stx-name ...] ...} sids)
          (define/syntax-parse {[stx-body ...] ...} sexprs)
          (define/syntax-parse {[val-label ...] ...} vlabs)
          (define/syntax-parse {[val-name ...] ...} vids)
          (define/syntax-parse ctc-fun cfun)
          ]))]))

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
