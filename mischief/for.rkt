#lang racket/base

(provide
  for/filter for*/filter
  for/filter-lists for*/filter-lists
  for/append for*/append
  for/append-lists for*/append-lists
  for/partition for*/partition
  for/partition-lists for*/partition-lists
  for/fold/lists for*/fold/lists
  for/fold/filter-lists for*/fold/filter-lists
  for/fold/append-lists for*/fold/append-lists
  define/for/fold define/for*/fold
  define/for/lists define/for*/lists
  define/for/filter-lists define/for*/filter-lists
  define/for/append-lists define/for*/append-lists
  define/for/partition define/for*/partition
  define/for/partition-lists define/for*/partition-lists
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
    mischief/parse)
  racket/list
  racket/block
  mischief/shorthand)

(module+ test
  (require rackunit mischief/values)
  (define-syntax (check-same stx)
    (syntax-parse stx
      [(_ actual:expr expected:expr)
       #`(test-begin
           (define expected-results
             (values-of expected))
           (define actual-results #false)
           #,(syntax/loc stx
               (check-not-exn
                 (lambda {}
                   (set! actual-results
                     (values-of actual)))))
           #,(syntax/loc stx
               (check-equal?
                 (length actual-results)
                 (length expected-results)
                 "different number of values in result"))
           (for {[a (in-list actual-results)]
                 [b (in-list expected-results)]
                 [i (in-naturals 1)]}
             #,(syntax/loc stx
                 (check-equal? a b
                   (format "value ~a of result differs (indexed from 1)"
                     i)))))])))

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
               #:attr loop-body
               (with-syntax {[loop loop-id]} #'loop-tem)
               #:attr define-body
               #'(define-values var-tem loop-body)))
           (define-syntax-class/specialize args/for (args #'for/fold))
           (define-syntax-class/specialize args/for* (args #'for*/fold))
           (values
             (syntax-parser [(_ . :args/for) #'loop-body])
             (syntax-parser [(_ . :args/for*) #'loop-body])
             (syntax-parser [(_ . :args/for) #'define-body])
             (syntax-parser [(_ . :args/for*) #'define-body]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filter Lists

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

(define-loops
  (fold/filter-lists loop/fold
      {[(~and x:id x*:temp-id) (~and e:expr e*:temp-id)] ...}
      {(~and ys:id y:temp-id rys:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {x ... ys ...}
  (let {[e* e] ...}
    (block
      (define-values {x ... rys.temp ...}
        (loop/fold {[x e*] ... [rys.temp '()] ...} clauses
          (define-values {x*.temp ... y.temp ...} (block . body))
          (values x*.temp ... (if y.temp (cons y.temp rys.temp) rys.temp) ...)))
      (define-values {ys ...}
        (values (reverse rys.temp) ...))
      (values x ... ys ...))))

(module+ test
  (check-same
    (let {[x '(1 2 #f 4)]
          [y '(5 #f 7 8)]}
      (for/filter {[x (in-list x)]
                   [y (in-list y)]}
        (and x y (+ x y))))
    '(6 12))
  (check-same
    (let {[x '(1 2 #f 4)]
          [y '(5 #f 7 8)]}
      (for*/filter {[x (in-list x)]
                    [y (in-list y)]}
        (and x y (+ x y))))
    '(6 8 9 7 9 10 9 11 12))
  (check-same
    (let {[x '(1 2 #f 4)]
          [y '(5 #f 7 8)]}
      (for/filter-lists {x y} {[x (in-list x)] [y (in-list y)]}
        (values (and x (add1 x)) (and y (sub1 y)))))
    (values '(2 3 5) '(4 6 7)))
  (check-same
    (let {[x '(1 2 #f 4)]
          [y '(5 #f 7 8)]}
      (for*/filter-lists {x y} {[x (in-list x)] [y (in-list y)]}
        (values (and x (add1 x)) (and y (sub1 y)))))
    (values
      '(2 2 2 2 3 3 3 3 5 5 5 5)
      '(4 6 7 4 6 7 4 6 7 4 6 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Append Lists

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

(define-loops
  (fold/append-lists loop/fold
      {[(~and x:id x*:temp-id) (~and e:expr e*:temp-id)] ...}
      {(~and ys:id ys0:temp-id ryss:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {x ... ys ...}
  (let {[e* e] ...}
    (block
      (define-values {x ... ryss.temp ...}
        (loop/fold {[x e*] ... [ryss.temp '()] ...} clauses
          (define-values {x*.temp ... ys0.temp ...} (block . body))
          (values x*.temp ... (cons ys0.temp ryss.temp) ...)))
      (define-values {ys ...}
        (values
          (for/fold {[ys '()]} {[ys0.temp (in-list ryss.temp)]}
            (append ys0.temp ys))
          ...))
      (values x ... ys ...))))

(module+ test
  (check-same
    (let {[x '(1 2 3)]
          [y '(a b c)]}
      (for/append {[x (in-list x)]
                   [y (in-list y)]}
        (make-list x y)))
    '(a b b c c c))
  (check-same
    (let {[x '(1 2 3)]
          [y '(a b c)]}
      (for*/append {[x (in-list x)]
                   [y (in-list y)]}
        (make-list x y)))
    '(a b c a a b b c c a a a b b b c c c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partition Lists

(define-shorthand
  (for/partition clauses:fold-clauses . body:block-body)
  (for/partition-lists {[yes no]} clauses . body))

(define-shorthand
  (for*/partition clauses:fold-clauses . body:block-body)
  (for*/partition-lists {[yes no]} clauses . body))

(define-shorthand
  (define/for/partition {yes:id no:id} clauses:fold-clauses . body:block-body)
  (define/for/partition-lists {[yes no]} clauses . body))

(define-shorthand
  (define/for*/partition {yes:id no:id} clauses:fold-clauses . body:block-body)
  (define/for*/partition-lists {[yes no]} clauses . body))

(define-loops
  (partition-lists loop/fold
      {(~and v:temp-id
         [(~and xs:id rxs:temp-id)
          (~and ys:id rys:temp-id)])
       ...}
    clauses:fold-clauses . body:block-body)
  {xs ... ys ...}
  (block
    (define-values {rxs.temp rys.temp}
      (values '() '()))
    ...
    (loop/fold {} clauses
      (define-values {then v.temp ...} (block . body))
      (cond
        [then (set! rxs.temp (cons v.temp rxs.temp)) ...]
        [else (set! rys.temp (cons v.temp rys.temp)) ...])
      (values))
    (values
      (reverse rxs.temp) ...
      (reverse rys.temp) ...)))

(module+ test
  (check-same
    (let {[x '(1 2 3)]
          [y '(3 2 1)]}
      (for/partition {[x (in-list x)]
                      [y (in-list y)]}
        (values (< x y) (list x y))))
    (values '([1 3]) '([2 2] [3 1])))
  (check-same
    (let {[x '(1 2 3)]
          [y '(3 2 1)]}
      (for*/partition {[x (in-list x)]
                       [y (in-list y)]}
        (values (< x y) (list x y))))
    (values
      '([1 3] [1 2] [2 3])
      '([1 1] [2 2] [2 1] [3 3] [3 2] [3 1]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists

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

(define-loops
  (fold/lists loop/fold
      {[(~and x:id x*:temp-id) (~and e:expr e*:temp-id)] ...}
      {(~and ys:id y:temp-id rys:temp-id) ...}
    clauses:fold-clauses . body:block-body)
  {x ... ys ...}
  (let {[e* e] ...}
    (block
      (define-values {x ... rys.temp ...}
        (loop/fold {[x e*] ... [rys.temp '()] ...} clauses
          (define-values {x*.temp ... y.temp ...} (block . body))
          (values x*.temp ... (cons y.temp rys.temp) ...)))
      (define-values {ys ...}
        (values (reverse rys.temp) ...))
      (values x ... ys ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Values

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
