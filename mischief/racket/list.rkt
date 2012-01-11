#lang racket/base

(provide
  make-alist build-alist
  member? memv? memq?
  take-while drop-while
  take-until drop-until
  topological-sort
  sort/unique)

(require
  racket/list
  racket/match
  racket/function
  data/queue)

(define (make-alist keys value)
  (for/list {[key (in-list keys)]}
    (cons key value)))

(define (build-alist keys key->value)
  (for/list {[key (in-list keys)]}
    (cons key (key->value key))))

(define (member? x ys [equiv? equal?])
  (for/or {[y (in-list ys)]}
    (equiv? x y)))

(define (memv? x ys)
  (for/or {[y (in-list ys)]}
    (eqv? x y)))

(define (memq? x ys)
  (for/or {[y (in-list ys)]}
    (eq? x y)))

(define (take-while pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       (cons (first xs)
         (take-while pred (rest xs)))
       empty)]))

(define (drop-while pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       (drop-while pred (rest xs))
       xs)]))

(define (take-until pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       empty
       (cons (first xs)
         (take-until pred (rest xs))))]))

(define (drop-until pred xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs)
     (if (pred (first xs))
       xs
       (drop-until pred (rest xs)))]))

(define (sort/unique elements <?
          #:key [key identity]
          #:cache-keys? [cache-keys? #f])
  (let* {[elements (sort elements <? #:key key #:cache-keys? cache-keys?)]}
    (if (empty? elements)
      elements
      (let loop {[elem (first elements)]
                 [elements (rest elements)]}
        (if (empty? elements)
          (list elem)
          (if (<? elem (first elements))
            (cons elem
              (loop (first elements)
                (rest elements)))
            (loop elem (rest elements))))))))

(define (topological-sort todo elem->deps)

  (define elem~>status (make-hasheq))
  (define done (make-queue))

  (define (visit elem [seen '()])
    (match (hash-ref elem~>status elem 'todo)
      ['done (void)]
      ['seen
       (error 'topological-sort
         "cycle detected: ~v"
         (cons elem
           (take-until
             (lambda (x)
               (eq? x elem))
             seen)))]
      ['todo
       (hash-set! elem~>status elem 'seen)
       (let* {[seen (cons elem seen)]}
         (for {[dep (in-list (elem->deps elem))]}
           (visit elem seen)))
       (hash-set! elem~>status elem 'done)]))

  (for {[elem (in-list todo)]}
    (visit elem))

  (queue->list done))
