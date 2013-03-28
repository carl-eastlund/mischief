#lang racket/base

(provide
  for/dict for*/dict
  for/dict! for*/dict!
  for/hash! for*/hash!
  for/hasheq! for*/hasheq!
  for/hasheqv! for*/hasheqv!
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
  racket/dict
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

(define-syntax (define-derived-loops stx)
  (syntax-parse stx
    [(_ (derived-suffix:id . pat) (original-suffix:id . tem))
     (define/syntax-parse
         {for/derived for*/derived define/for/derived define/for*/derived}
       (for/list {[prefix (in-list '(for for* define/for define/for*))]}
         (format-id #'derived-suffix "~a/~a" prefix #'derived-suffix)))
     (define/syntax-parse
         {for/original for*/original define/for/original define/for*/original}
       (for/list {[prefix (in-list '(for for* define/for define/for*))]}
         (format-id #'original-suffix "~a/~a" prefix #'original-suffix)))
     #'(define-syntaxes
           {for/derived for*/derived define/for/derived define/for*/derived}
         (block
           (define-syntax-class args #:attributes {body}
             (pattern pat #:attr body #'tem))
           (values
             (syntax-parser [(_ . :args) #'(for/original . body)])
             (syntax-parser [(_ . :args) #'(for*/original . body)])
             (syntax-parser [(_ . :args) #'(define/for/original . body)])
             (syntax-parser [(_ . :args) #'(define/for*/original . body)]))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Immutable Dictionaries

(define-shorthand
  (for/dict d0:expr clauses:for-clauses . body:for-body)
  (for/fold {[d d0]} clauses
    body.head ...
    (define-values {k v} body.tail)
    (dict-set d k v)))

(define-shorthand
  (for*/dict d0:expr clauses:for-clauses . body:for-body)
  (for*/fold {[d d0]} clauses
    body.head ...
    (define-values {k v} body.tail)
    (dict-set d k v)))

(define-shorthand
  (for/dict! d0:expr clauses:for-clauses . body:for-body)
  (let {[d d0]}
    (for clauses
      body.head ...
      (define-values {k v} body.tail)
      (dict-set! d k v))
    d))

(define-shorthand
  (for*/dict! d0:expr clauses:for-clauses . body:for-body)
  (let {[d d0]}
    (for* clauses
      body.head ...
      (define-values {k v} body.tail)
      (dict-set! d k v))
    d))

(define-shorthand
  (for/hash! clauses:for-clauses . body:for-body)
  (for/dict! (make-hash) clauses . body))

(define-shorthand
  (for*/hash! clauses:for-clauses . body:for-body)
  (for*/dict! (make-hash) clauses . body))

(define-shorthand
  (for/hasheq! clauses:for-clauses . body:for-body)
  (for/dict! (make-hasheq) clauses . body))

(define-shorthand
  (for*/hasheq! clauses:for-clauses . body:for-body)
  (for*/dict! (make-hasheq) clauses . body))

(define-shorthand
  (for/hasheqv! clauses:for-clauses . body:for-body)
  (for/dict! (make-hasheqv) clauses . body))

(define-shorthand
  (for*/hasheqv! clauses:for-clauses . body:for-body)
  (for*/dict! (make-hasheqv) clauses . body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filter Lists

(define-shorthand
  (for/filter clauses:for-clauses . body:for-body)
  (for/filter-lists {xs} clauses . body))

(define-shorthand
  (for*/filter clauses:for-clauses . body:for-body)
  (for*/filter-lists {xs} clauses . body))

(define-derived-loops
  (filter-lists {xs:id ...} clauses:for-clauses . body:for-body)
  (fold/filter-lists {} {xs ...} clauses . body))

(define-loops
  (fold/filter-lists loop/fold
      {[(~and x:id x*:temp-id x**:temp-id) (~and e:expr e*:temp-id)] ...}
      {(~and ys:id ys*:temp-id y:temp-id rys:temp-id) ...}
    clauses:for-clauses . body:for-body)
  {x ... ys ...}
  (let {[e*.temp e] ...}
    (block
      (define-values {x**.temp ... rys.temp ...}
        (loop/fold {[x e*.temp] ... [rys.temp '()] ...} clauses
          body.head ...
          (define-values {x*.temp ... y.temp ...} body.tail)
          (values x*.temp ... (if y.temp (cons y.temp rys.temp) rys.temp) ...)))
      (define-values {ys*.temp ...}
        (values (reverse rys.temp) ...))
      (values x**.temp ... ys*.temp ...))))

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
    (let {[n 0]
          [x '(1 2 #f 4)]
          [y '(5 #f 7 8)]}
      (for/fold/filter-lists
          {[n n]}
          {lst}
          {[x (in-list x)]
           [y (in-list y)]}
        (values (add1 n) (and x y (+ x y)))))
    (values 4 '(6 12)))
  (check-same
    (let {[n 0]
          [x '(1 2 #f 4)]
          [y '(5 #f 7 8)]}
      (for*/fold/filter-lists
          {[n n]}
          {lst}
          {[x (in-list x)]
           [y (in-list y)]}
        (values (add1 n) (and x y (+ x y)))))
    (values 16 '(6 8 9 7 9 10 9 11 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Append Lists

(define-shorthand
  (for/append clauses:for-clauses . body:for-body)
  (for/append-lists {xs} clauses . body))

(define-shorthand
  (for*/append clauses:for-clauses . body:for-body)
  (for*/append-lists {xs} clauses . body))

(define-derived-loops
  (append-lists {xs:id ...} clauses:for-clauses . body:for-body)
  (fold/append-lists {} {xs ...} clauses . body))

(define-loops
  (fold/append-lists loop/fold
      {[(~and x:id x*:temp-id x**:temp-id) (~and e:expr e*:temp-id)] ...}
      {(~and ys:id ys*:temp-id ys**:temp-id ys0:temp-id ryss:temp-id) ...}
    clauses:for-clauses . body:for-body)
  {x ... ys ...}
  (let {[e*.temp e] ...}
    (define-values {x**.temp ... ryss.temp ...}
      (loop/fold {[x e*.temp] ... [ryss.temp '()] ...} clauses
        body.head ...
        (define-values {x*.temp ... ys0.temp ...} body.tail)
        (values x*.temp ... (cons ys0.temp ryss.temp) ...)))
    (define-values {ys*.temp ...}
      (values
        (for/fold {[ys**.temp '()]} {[ys0.temp (in-list ryss.temp)]}
          (append ys0.temp ys**.temp))
        ...))
    (values x**.temp ... ys*.temp ...)))

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
    '(a b c a a b b c c a a a b b b c c c))
  (check-same
    (let {[n 0] [x '(1 2 3)] [y '(a b c)]}
      (for/fold/append-lists
          {[n n]}
          {lst}
          {[x (in-list x)] [y (in-list y)]}
        (values
          (+ x n)
          (make-list x y))))
    (values 6 '(a b b c c c)))
  (check-same
    (let {[n 0] [x '(1 2 3)] [y '(a b c)]}
      (for*/fold/append-lists
          {[n n]}
          {lst}
          {[x (in-list x)] [y (in-list y)]}
        (values
          (+ x n)
          (make-list x y))))
    (values 18 '(a b c a a b b c c a a a b b b c c c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partition Lists

(define-shorthand
  (for/partition clauses:for-clauses . body:for-body)
  (for/partition-lists {[yes no]} clauses . body))

(define-shorthand
  (for*/partition clauses:for-clauses . body:for-body)
  (for*/partition-lists {[yes no]} clauses . body))

(define-shorthand
  (define/for/partition {yes:id no:id} clauses:for-clauses . body:for-body)
  (define/for/partition-lists {[yes no]} clauses . body))

(define-shorthand
  (define/for*/partition {yes:id no:id} clauses:for-clauses . body:for-body)
  (define/for*/partition-lists {[yes no]} clauses . body))

(define-loops
  (partition-lists loop/fold
      {(~and v:temp-id
         [(~and xs:id rxs:temp-id)
          (~and ys:id rys:temp-id)])
       ...}
    clauses:for-clauses . body:for-body)
  {xs ... ys ...}
  (block
    (define-values {rxs.temp rys.temp}
      (values '() '()))
    ...
    (loop/fold {} clauses
      body.head ...
      (define-values {then v.temp ...} body.tail)
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
  (define/for/lists {xs:id ...} clauses:for-clauses . body:for-body)
  (define/for/list-values {xs ...} clauses . body))

(define-shorthand
  (define/for*/lists {xs:id ...} clauses:for-clauses . body:for-body)
  (define/for*/list-values {xs ...} clauses . body))

(define-derived-loops
  (list-values {xs:id ...} clauses:for-clauses . body:for-body)
  (fold/lists {} {xs ...} clauses . body))

(define-loops
  (fold/lists loop/fold
      {[(~and x:id x*:temp-id x**:temp-id) (~and e:expr e*:temp-id)] ...}
      {(~and ys:id ys*:temp-id y:temp-id rys:temp-id) ...}
    clauses:for-clauses . body:for-body)
  {x ... ys ...}
  (let {[e*.temp e] ...}
    (block
      (define-values {x**.temp ... rys.temp ...}
        (loop/fold {[x e*.temp] ... [rys.temp '()] ...} clauses
          body.head ...
          (define-values {x*.temp ... y.temp ...} body.tail)
          (values x*.temp ... (cons y.temp rys.temp) ...)))
      (define-values {ys*.temp ...}
        (values (reverse rys.temp) ...))
      (values x**.temp ... ys*.temp ...))))

(module+ test
  (check-same
    (let {[n 0]
          [x '(1 2 3)]
          [y '(a b c)]}
      (for/fold/lists {[n n]} {lst} {[x (in-list x)] [y (in-list y)]}
        (values (add1 n) (make-list x y))))
    (values 3 '([a] [b b] [c c c])))
  (check-same
    (let {[n 0]
          [x '(1 2 3)]
          [y '(a b c)]}
      (for*/fold/lists {[n n]} {lst} {[x (in-list x)] [y (in-list y)]}
        (values (add1 n) (make-list x y))))
    (values 9 '([a] [b] [c] [a a] [b b] [c c] [a a a] [b b b] [c c c]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Values

(define-shorthand
  (define/for/fold {[x:id e:expr] ...} clauses:for-clauses . body:for-body)
  (define/for/fold-values {[x e] ...} clauses . body))

(define-shorthand
  (define/for*/fold {[x:id e:expr] ...} clauses:for-clauses . body:for-body)
  (define/for*/fold-values {[x e] ...} clauses . body))

(define-loops
  (fold-values loop/fold {[x:id e:expr] ...}
    clauses:for-clauses . body:for-body)
  {x ...}
  (loop/fold {[x e] ...} clauses . body))

(module+ test
  (check-same
    (let {[n 0]
          [x '(1 2 3)]
          [y '(10 100 1000)]}
      (for/fold-values {[n n]} {[x (in-list x)] [y (in-list y)]}
        (+ n (* x y))))
    3210)
  (check-same
    (let {[n 0]
          [x '(1 2 3)]
          [y '(10 100 1000)]}
      (for*/fold-values {[n n]} {[x (in-list x)] [y (in-list y)]}
        (+ n (* x y))))
    6660))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive folds

(module+ test
  (check-same
    (let {[n 0]
          [x '(1 2 3)]
          [y '(10 100 1000)]}
      (for/fold {[n n]} {[x (in-list x)] [y (in-list y)]}
        (+ n (* x y))))
    3210)
  (check-same
    (let {[n 0]
          [x '(1 2 3)]
          [y '(10 100 1000)]}
      (for*/fold {[n n]} {[x (in-list x)] [y (in-list y)]}
        (+ n (* x y))))
    6660))
