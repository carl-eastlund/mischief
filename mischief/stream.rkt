#lang racket/base

(provide
  define-stream
  stream-interleave
  stream-interleave*
  stream*
  stream-delay
  stream-cross-product
  stream-cross-product*
  stream-take
  stream-zip)

(require
  (for-syntax
    racket/base
    syntax/parse)
  racket/list
  racket/stream
  racket/promise
  mischief/define
  mischief/match
  mischief/boolean
  mischief/shorthand)

(define (stream-zip stream0 . streams0)
  (define streams (cons stream0 streams0))
  (stream-delay
    (cond!
      [(ormap stream-empty? streams) empty-stream]
      [else (stream-cons (map stream-first streams)
              (apply stream-zip (map stream-rest streams)))])))

(define (stream-interleave . streams)
  (match! streams
    ['() empty-stream]
    [(list st) st]
    [_ (let loop ([streams streams] [rests '()])
         (cond!
           [(empty? streams)
            (apply stream-interleave (reverse rests))]
           [(stream-empty? (first streams))
            (loop (rest streams) rests)]
           [else
            (define st (first streams))
            (stream-cons (stream-first st)
              (loop (rest streams)
                (cons (stream-rest st) rests)))]))]))

(define (stream-interleave* streams [n0 1])
  (cond!
    [(stream-empty? streams) empty-stream]
    [else
     (let loop ([n n0] [streams streams] [rests '()])
       (cond!
         [(or (zero? n) (stream-empty? streams))
          (stream-interleave*
            (for/fold {[streams streams]} {[st (in-list rests)]}
              (stream-cons st streams))
            (add1 n0))]
         [(stream-empty? (stream-first streams))
          (loop (sub1 n) (stream-rest streams) rests)]
         [else
          (define st (stream-first streams))
          (stream-cons (stream-first st)
            (loop (sub1 n) (stream-rest streams)
              (cons (stream-rest st) rests)))]))]))

(define-syntax-if-unbound stream*
  (syntax-parser
    [(_ st) #'st]
    [(_ x . ys) #'(stream-cons x (stream* . ys))]))

(define-syntax (define-stream stx)
  (syntax-parse stx
    [(_ name:id st:expr)
     (quasisyntax/loc stx
       (define name
         (delayed
           (unsyntax
             (syntax/loc stx
               (delay
                 (let {[s st]}
                   (check-stream! 'define-stream s)
                   s)))))))]))

(define-syntax (stream-delay stx)
  (syntax-parse stx
    [(_ st:expr)
     (quasisyntax/loc stx
       (delayed
         (unsyntax
           (syntax/loc stx
             (delay
               (let {[s st]}
                 (check-stream! 'stream-delay s)
                 s))))))]))

(define (check-stream! name st)
  (unless (stream? st)
    (raise-argument-error name "stream?" st)))

(define (delayed-empty? st) (stream-empty? (force (delayed-promise st))))
(define (delayed-first st) (stream-first (force (delayed-promise st))))
(define (delayed-rest st) (stream-rest (force (delayed-promise st))))

(struct delayed [promise]
  #:methods gen:stream
    {(define stream-empty? delayed-empty?)
     (define stream-first delayed-first)
     (define stream-rest delayed-rest)})

(define (stream-cross-product* f xs ys)
  (stream-map
    (lambda (y)
      (stream-map (lambda (x) (f x y)) xs))
    ys))

(define (stream-cross-product f xs ys)
  (stream-interleave*
    (stream-cross-product* f xs ys)))

(define-if-unbound (stream-take st n)
  (for/list {[x (in-stream st)] [i (in-range n)]}
    x))
