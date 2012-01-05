#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide
  rack?
  rack-append
  list->rack rack->list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/list
  racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; (Rack T) = (Items{0-3} T) or (Shelf T)
;; (Items0 T) = (none)
;; (Items1 T) = (one T)
;; (Items2 T) = (two T)
;; (Items3 T) = (three T T T)
;; (Shelf T) = (shelf (Items{1-3} T) (FRack (Items{2-3} T)) (Items{1-3} T))
;; (FRack T) = (Frame (Rack (Frame T)))
;; (Frame T) = (frame Dir Nat T)
;; Dir = 'forward or 'reverse

(struct rack [])

(struct items rack [])
(struct none items [])
(struct one items [first])
(struct two items [first second])
(struct three items [first second third])

(struct shelf rack [first between last])

(struct frame [size direction contents])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define (rack-length r)
  (size 0 r))

(define (rack-reverse r)
  (rev 0 r))

(define (rack-append . rs)
  (for/fold {[r0 (none)]} {[r (in-list rs)]}
    (rack-cat 0 r0 r)))

(define (list->rack xs)
  (match xs
    [(list) (none)]
    [(list a) (one a)]
    [(list a b) (two a b)]
    [(list a b c) (three a b c)]
    [(list* a b cs)
     (let loop {[r (two a b)] [xs cs]}
       (match xs
         [(list) r]
         [(list a) (rack-cat 0 r (one a))]
         [(list a b) (rack-cat 0 r (two a b))]
         [(list a b c) (rack-cat 0 r (three a b c))]
         [(list* a b cs) (loop (rack-cat 0 r (two a b)) cs)]))]))

(define (rack->list r)
  (rack->list* 0 r empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

(define (rack->list* depth r xs)
  (match r
    [(none) xs]
    [(one a) (rcons depth a)]
    [(two a b) (rcons depth a (rcons depth b xs))]
    [(three a b c) (rcons depth a (rcons depth b (rcons depth c xs)))]
    [(shelf << == >>)
     (rcons depth <<
       (rack->list* (add1 depth) (frame-contents ==)
         (rcons depth >> xs)))]))

(define (rcons depth x xs)
  (if (zero? depth)
    (cons x xs)
    (rack->list* (sub1 depth) (frame-contents x) xs)))

(define (rack-cat depth ra rb)
  (cond
    [(none? ra) rb]
    [(none? rb) ra]
    [(and (shelf? ra) (shelf? rb)) (shelf+shelf depth ra rb)]
    [(shelf? ra) (shelf+items depth ra rb)]
    [(shelf? rb) (items+shelf depth ra rb)]
    [else (items+items depth ra rb)]))

(define (shelf+shelf depth sa sb)
  (match* {sa sb}
    [{(shelf <ia ra ia>) (shelf <ib rb ib>)}
     (define rab
       (items-cat ia> <ib
         (lambda (iab)
           (rack-cat (add1 depth)
             (frame+one depth ra iab)
             rb))
         (lambda (ia ib)
           (rack-cat (add1 depth)
             (frame+one depth ra ia)
             (one+frame depth ib rb)))))
     (shelf <ia rab ib>)]))

(define (shelf+items depth sa ib)
  (match sa
    [(shelf <ia ra ia>)
     (items-cat ia> ib
       (lambda (iab>)
         (shelf <ia ra iab>))
       (lambda (ia ib>)
         (shelf <ia (frame+one depth ra ia) ib>)))]))

(define (items+shelf depth ia sb)
  (match sb
    [(shelf <ib rb ib>)
     (items-cat ia <ib
       (lambda (<iab)
         (shelf <iab rb ib>))
       (lambda (<ia ib)
         (shelf <ia (one+frame depth ib rb) ib>)))]))

(define (items+items depth ia ib)
  (items-cat ia ib
    (lambda (iab) iab)
    (lambda (ia ib)
      (shelve depth ia ib))))

(define (one+frame depth x fr)
  (match fr
    [(frame n 'forward r)
     (frame (+ (size depth x) n) 'forward
       (one+rack (add1 depth) (build depth 'forward x) r))]
    [(frame n 'reverse r)
     (frame (+ (size depth x) n) 'reverse
       (rack+one (add1 depth) r (build depth 'reverse x)))]))

(define (frame+one depth fr x)
  (match fr
    [(frame n 'forward r)
     (frame (+ n (size depth x)) 'forward
       (rack+one (add1 depth) r (build depth 'forward x)))]
    [(frame n 'reverse r)
     (frame (+ n (size depth x)) 'reverse
       (one+rack (add1 depth) (build depth 'reverse x) r))]))

(define (one+rack depth x r)
  (match r
    [(none) (one x)]
    [(one a) (two x a)]
    [(two a b) (three x a b)]
    [(three a b c) (shelve depth (two x a) (two b c))]
    [(shelf << == >>)
     (match <<
       [(one a) (shelf (two x a) == >>)]
       [(two a b) (shelf (three x a b) == >>)]
       [(three a b c) (shelf
                        (two x a)
                        (one+frame (add1 depth) (two b c) ==)
                        >>)])]))

(define (rack+one depth r x)
  (match r
    [(none) (one x)]
    [(one a) (two a x)]
    [(two a b) (three a b x)]
    [(three a b c) (shelve depth (two a b) (two c x))]
    [(shelf << == >>)
     (match >>
       [(one a) (shelf << == (two a x))]
       [(two a b) (shelf << == (three a b x))]
       [(three a b c) (shelf
                        <<
                        (frame+one (add1 depth) == (two a b))
                        (two c x))])]))

(define (items-cat ia ib f1 f2)
  (match* {ia ib}
    [{(one a) (one b)} (f1 (two a b))]
    [{(one a) (two b c)} (f1 (three a b c))]
    [{(two a b) (one c)} (f1 (three a b c))]
    [{(one a) (three b c d)} (f2 (two a b) (two c d))]
    [{(three a b c) (one d)} (f2 (two a b) (two c d))]
    [{_ _} (f2 ia ib)]))

(define (shelve depth ia ib)
  (shelf ia (build depth 'forward (none)) ib))

(define (build depth dir i)
  (frame (size depth i) dir i))

(define (size depth i)
  (if (zero? depth)
    (shallow-size i)
    (deep-size i)))

(define (shallow-size i)
  (match i
    [(none) 0]
    [(one _) 1]
    [(two _ _) 2]
    [(three _ _ _) 3]
    [(shelf a b c) (+ (shallow-size a)
                      (frame-size b)
                      (shallow-size c))]))

(define (deep-size i)
  (match i
    [(none) 0]
    [(one a) (frame-size a)]
    [(two a b) (+ (frame-size a) (frame-size b))]
    [(three a b c) (+ (frame-size a) (frame-size b) (frame-size c))]
    [(shelf a b c) (+ (deep-size a) (frame-size b) (deep-size c))]))

(define (rev depth i)
  (if (zero? i)
    (shallow-rev i)
    (deep-rev i)))

(define (shallow-rev i)
  (match i
    [(none) (none)]
    [(one a) (one a)]
    [(two a b) (two b a)]
    [(three a b c) (three c b a)]
    [(shelf a b c) (shelf
                     (shallow-rev c)
                     (frame-rev b)
                     (shallow-rev a))]))

(define (deep-rev i)
  (match i
    [(none) (none)]
    [(one a) (one (frame-rev a))]
    [(two a b) (two (frame-rev b) (frame-rev a))]
    [(three a b c) (three (frame-rev c) (frame-rev b) (frame-rev a))]
    [(shelf a b c) (shelf (deep-rev c) (frame-rev b) (deep-rev c))]))

(define (frame-rev f)
  (match f
    [(frame n 'forward r) (frame n 'reverse r)]
    [(frame n 'reverse r) (frame n 'forward r)]))
