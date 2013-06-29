#lang racket/base

(provide parallel-map-reduce)

(require racket/future racket/place racket/list racket/stream)

(define (parallel-map-reduce make-place init combine inputs)
  (define workers
    (for/list {[i (in-range (processor-count))]}
      (make-place)))
  (let loop {[inputs inputs] [workers workers] [busy '()] [result init]}
    (cond
      [(and
         (not (empty? workers))
         (not (stream-empty? inputs)))
       (define i (stream-first inputs))
       (define w (first workers))
       (place-channel-put w i)
       (loop
         (stream-rest inputs)
         (rest workers)
         (cons w busy)
         result)]
      [(not (empty? busy))
       (sync-workers busy
         (lambda {value worker still-busy}
           (loop
             inputs
             (list worker)
             still-busy
             (combine value result))))]
      [else
       (for-each place-kill workers)
       result])))

(define (sync-workers workers proc)
  (apply sync
    (let loop {[others '()] [workers workers]}
      (cond
        [(empty? workers) '()]
        [else
         (define w (first workers))
         (define ws (rest workers))
         (define evt
           (handle-evt w
             (lambda {x}
               (proc x w (foldl cons ws others)))))
         (cons evt
           (loop (cons w others) ws))]))))
