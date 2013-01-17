#lang racket/base

(provide
  topological-sort)

(require
  data/queue
  mischief/match
  mischief/list)

(define (topological-sort todo elem->deps
                #:cycle [cycle cycle-error])

  (define elem~>status (make-hasheq))
  (define done (make-queue))

  (define (visit elem [seen '()])
    (match! (hash-ref elem~>status elem 'todo)
      ['done (void)]
      ['seen
       (cycle
         (cons elem
           (take-until
             (lambda (x)
               (eq? x elem))
             seen)))]
      ['todo
       (hash-set! elem~>status elem 'seen)
       (define seen+elem (cons elem seen))
       (for {[dep (in-list (elem->deps elem))]}
         (visit dep seen+elem))
       (enqueue! done elem)
       (hash-set! elem~>status elem 'done)]))

  (for {[elem (in-list todo)]}
    (visit elem))

  (queue->list done))

(define (cycle-error elems)
  (error 'topological-sort
    "cycle detected: ~v"
    elems))
