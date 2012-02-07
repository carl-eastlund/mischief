#lang racket/base

(provide
  for/filter for*/filter
  for/filter-lists for*/filter-lists
  for/append for*/append
  for/append-lists for*/append-lists
  for/partition for*/partition
  for/partition* for*/partition*
  for/fold/lists for*/fold/lists
  for/fold/filter-lists for*/fold/filter-lists
  for/fold/append-lists for*/fold/append-lists
  define/for/fold define/for*/fold
  define/for/lists define/for*/lists
  define/for/filter-lists define/for*/filter-lists
  define/for/append-lists define/for*/append-lists
  define/for/partition define/for*/partition
  define/for/partition* define/for*/partition*
  define/for/fold/lists define/for*/fold/lists
  define/for/fold/filter-lists define/for*/fold/filter-lists
  define/for/fold/append-lists define/for*/fold/append-lists)

(require
  (for-syntax
    racket/base
    racket/block
    racket/syntax
    syntax/parse
    syntax/parse/experimental/specialize
    mischief/syntax/parse)
  racket/list
  racket/block
  mischief/syntax/shorthand)

(define-syntax (define-loops stx)
  (syntax-parse stx
    [(_ (name:id loop:id . pat) var-tem loop-tem)
     (define/syntax-parse
         {for/name for*/name define/for/name define/for*/name}
       (for/list {[prefix (in-list '(for for* define/for define/for*))]}
         (format-id #'name "~a/~a" prefix #'name)))
     #'(define-syntaxes
           {for/name for*/name define/for/name define/for*/name}
         (block
           (define-syntax-class (args loop-id)
             #:attributes {loop-body define-body}
             (pattern pat
               #:attr define-body
               (with-syntax {[loop loop-id]}
                 #'(define-values var-tem loop-tem))
               #:attr loop-body
               #'(block define-body (values . var-tem))))
           (define-syntax-class/specialize args/for (args #'for/fold))
           (define-syntax-class/specialize args/for* (args #'for*/fold))
           (values
             (syntax-parser [(_ . :args/for) #'loop-body])
             (syntax-parser [(_ . :args/for*) #'loop-body])
             (syntax-parser [(_ . :args/for) #'define-body])
             (syntax-parser [(_ . :args/for*) #'define-body]))))]))

(define-shorthand
  (for/filter clauses:fold-clauses . body:block-body)
  (for/filter-lists {xs} clauses . body))

(define-shorthand
  (for*/filter clauses:fold-clauses . body:block-body)
  (for*/filter-lists {xs} clauses . body))

(define-loops
  (filter-lists loop/fold {(~and xs:id x:temp-id rxs:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {xs ...}
  (block
    (define-values {rxs.temp ...}
      (loop/fold {[rxs.temp '()] ...} clauses
        (define-values {x.temp ...} (block . body))
        (values (if x.temp (cons x.temp rxs.temp) rxs.temp) ...)))
    (values (reverse rxs.temp) ...)))

(define-shorthand
  (for/append clauses:fold-clauses . body:block-body)
  (for/append-lists {xs} clauses . body))

(define-shorthand
  (for*/append clauses:fold-clauses . body:block-body)
  (for*/append-lists {xs} clauses . body))

(define-loops
  (append-lists loop/fold {(~and xs:id xs0:temp-id rxss:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {xs ...}
  (block
    (define-values {rxss.temp ...}
      (loop/fold {[rxss.temp '()] ...} clauses
        (define-values {xs0.temp ...} (block . body))
        (values (cons xs0.temp rxss.temp) ...)))
    (values
      (for/fold {[xs '()]} {[xs0.temp (in-list rxss.temp)]}
        (append xs0.temp xs))
      ...)))

(define-shorthand
  (for/partition clauses:fold-clauses . body:block-body)
  (for/partition* {[yes #true] [no #false]} clauses
    (define-values {? x} (block . body))
    (values (if ? #true #false) x)))

(define-shorthand
  (for*/partition clauses:fold-clauses . body:block-body)
  (for*/partition* {[yes #true] [no #false]} clauses
    (define-values {? x} (block . body))
    (values (if ? #true #false) x)))

(define-shorthand
  (define/for/partition {yes:id no:id} clauses:fold-clauses . body:block-body)
  (define/for/partition* {[yes #true] [no #false]} clauses
    (define-values {? x} (block . body))
    (values (if ? #true #false) x)))

(define-shorthand
  (define/for*/partition {yes:id no:id} clauses:fold-clauses . body:block-body)
  (define/for*/partition* {[yes #true] [no #false]} clauses
    (define-values {? x} (block . body))
    (values (if ? #true #false) x)))

(define-loops
  (partition* loop/fold {[(~and xs:id rxs:temp-id) key ...] ...}
    clauses:fold-clauses . body:block-body)
  {xs ...}
  (block
    (define rxs.temp '()) ...
    (loop/fold {} clauses
      (define-values {k v} (block . body))
      (case k
        [(key ...) (set! rxs.temp (cons v rxs.temp))]
        ...)
      (values))
    (values (reverse rxs.temp) ...)))

(define-shorthand
  (define/for/lists {xs:id ...} clauses:fold-clauses . body:block-body)
  (define/for/list-values {xs ...} clauses . body))

(define-shorthand
  (define/for*/lists {xs:id ...} clauses:fold-clauses . body:block-body)
  (define/for*/list-values {xs ...} clauses . body))

(define-loops
  (list-values loop/fold {(~and xs:id x:temp-id rxs:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {xs ...}
  (block
    (define-values {rxs.temp ...}
      (loop/fold {[rxs.temp '()] ...} clauses
        (define-values {x.temp ...} (block . body))
        (values (cons x.temp rxs.temp) ...)))
    (values (reverse rxs.temp) ...)))

(define-shorthand
  (define/for/fold {[x:id e:expr] ...} clauses:fold-clauses . body:block-body)
  (define/for/fold-values {[x e] ...} clauses . body))

(define-shorthand
  (define/for*/fold {[x:id e:expr] ...} clauses:fold-clauses . body:block-body)
  (define/for*/fold-values {[x e] ...} clauses . body))

(define-loops
  (fold-values loop/fold {[x:id e:expr] ...}
    clauses:fold-clauses . body:block-body)
  {x ...}
  (loop/fold {[x e] ...} clauses . body))

(define-loops
  (fold/lists loop/fold
      {[(~and x:id x*:temp-id) e:expr] ...}
      {(~and ys:id y:temp-id rys:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {x ... ys ...}
  (block
    (define-values {x ... rys.temp ...}
      (loop/fold {[x e] ... [rys.temp '()] ...} clauses
        (define-values {x*.temp ... y.temp ...} (block . body))
        (values x*.temp ... (cons y.temp rys.temp) ...)))
    (define-values {ys ...}
      (values (reverse rys.temp) ...))
    (values x ... ys ...)))

(define-loops
  (fold/filter-lists loop/fold
      {[(~and x:id x*:temp-id) e:expr] ...}
      {(~and ys:id y:temp-id rys:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {x ... ys ...}
  (block
    (define-values {x ... rys.temp ...}
      (loop/fold {[x e] ... [rys.temp '()] ...} clauses
        (define-values {x*.temp ... y.temp ...} (block . body))
        (values x*.temp ... (if y.temp (cons y.temp rys.temp) rys.temp) ...)))
    (define-values {ys ...}
      (values (reverse rys.temp) ...))
    (values x ... ys ...)))

(define-loops
  (fold/append-lists loop/fold
      {[(~and x:id x*:temp-id) e:expr] ...}
      {(~and ys:id ys0:temp-id ryss:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {x ... ys ...}
  (block
    (define-values {x ... ryss.temp ...}
      (loop/fold {[x e] ... [ryss.temp '()] ...} clauses
        (define-values {x*.temp ... ys0.temp ...} (block . body))
        (values x*.temp ... (cons ys0.temp ryss.temp) ...)))
    (define-values {ys ...}
      (values
        (for/fold {[ys '()]} {[ys0.temp (in-list ryss.temp)]}
          (append ys0.temp ys))
        ...))
    (values x ... ys ...)))
