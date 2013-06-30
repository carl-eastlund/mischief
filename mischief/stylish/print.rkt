#lang racket/unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Imports

(require
  racket/list
  racket/port
  racket/promise
  racket/block
  data/queue
  mischief/struct
  mischief/boolean
  mischief/stylish/signatures
  no-debug/low-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Imports

(import)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Exports

(export print^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; PrintStyle = (print-style PrintDefault (List PrintType))
;; PrintDefault = (Maybe (Any OutputPort -> Any))
;; PrintType = {exists T (print-type (Type T) (Print T))}
;; (Type T) = (Any -> Boolean : T)
;; (Print T) = (T StylishPort -> Void)
(struct print-style [default extensions])
(struct print-type [type? printer])

;; Delimited = (delimited Nat (List Token))
;; Token = (Or String Separator Delimited)
;; Separator = (separator Nat Boolean)
(struct delimited [length contents])
(struct separator [indent wide?])

;; StylishPort = (stylish-port StringOutputPort (Queue Token))
(struct stylish-port [string-port contents]
  #:property prop:output-port 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define (print-to-stylish-port name port0 left right cols proc)
  (define string-port (open-output-string 'stylish))
  (define contents (make-queue))
  (define port (stylish-port string-port contents))
  (begin0 (proc port)
    (block
      (stylish-port-flush! port)
      (close-output-port port)
      (define d (delimit-tokens (queue->list contents)))
      (cond!
        [(stylish-port? port0) (stylish-port-enqueue! port0 d)]
        [else (render-delimited d port0 left right cols)]))))

(define (print-expression name e st port)
  (cond!
    [(low-level-debug type-of name e st) =>
     (lambda (type)
       (low-level-debug (low-level-debug print-type-printer type) e port st))]
    [(print-style-default st) =>
     (lambda (default)
       (low-level-debug default e port))]
    [else (error name "cannot print expression: ~v" e)]))

(define (type-of name x st)
  (let find {[exts (print-style-extensions st)]}
    (if (empty? exts)
      #false
      (if ((print-type-type? (first exts)) x)
        (first exts)
        (find (rest exts))))))

(define (print-separator name port indent wide?)
  (stylish-port-enqueue! port
    (separator indent wide?)))

(define empty-print-style
  (print-style #false empty))

(define simple-print-style
  (print-style write empty))

(define (extend-print-style pst
          #:after? [after? #false]
          . new-exts)
  (update print-style pst
    [extensions (if after?
                  (append extensions new-exts)
                  (append new-exts extensions))]))

(define (set-print-style-default-printer st new-default)
  (update print-style st
    [default new-default]))

(define (print-style-extension? x)
  (print-type? x))

(define (print-style-extension type? printer)
  (print-type type? printer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

(define (stylish-port-enqueue! port token)
  (stylish-port-flush! port)
  (stylish-port-primitive-enqueue! port token))

(define (stylish-port-flush! port)
  (define str (get-output-string (stylish-port-string-port port)))
  (flush-output-string! (stylish-port-string-port port))
  (stylish-port-primitive-enqueue! port str))

(define (stylish-port-primitive-enqueue! port token)
  (enqueue! (stylish-port-contents port) token))

(define (flush-output-string! port)
  (void (get-output-bytes port #true 0 0)))

(define (delimit-tokens contents)
  (delimited
    (content-length contents)
    (add-separators contents)))

(define (content-length contents)
  (let loop {[contents contents] [len 0]}
    (cond!
      [(empty? contents) len]
      [else (loop (rest contents)
              (+ len (token-length (first contents))))])))

(define (token-length token)
  (cond!
    [(string? token) (string-length token)]
    [(delimited? token) (delimited-length token)]
    [(separator? token) (if (separator-wide? token) 1 0)]))

(define (add-separators contents [seen-delimited? #false])
  (cond!
    [(empty? contents) empty]
    [(cons? contents)
     (define token (first contents))
     (cond!
       [(string? token)
        (cons token
          (add-separators (rest contents) seen-delimited?))]
       [(separator? token)
        (cons token
          (add-separators (rest contents) #false))]
       [(delimited? token)
        (cons/optional (and seen-delimited? (separator 1 #false))
          (cons token
            (add-separators (rest contents) #true)))])]))

(define (render-delimited d port left right cols)
  (if (<= (delimited-length d) (- (columns->number cols) left right))
    (render-one-line d port)
    (render-many-lines d port left right cols)))

(define (columns->number c)
  (if (number? c) c +inf.0))

(define (render-many-lines d port left right cols)
  (render-contents (delimited-contents d) port left right cols))

(define (render-contents contents port left right cols)
  (unless (empty? contents)
    (define-values {indent tokens len remaining}
      (take-line contents))
    (render-line indent tokens port left (+ len right) cols)
    (render-contents remaining port left right cols)))

(define (take-line contents)
  (define-values {indent remaining}
    (if (separator? (first contents))
      (values (separator-indent (first contents)) (rest contents))
      (values #false contents)))
  (let loop {[rev-tokens empty] [len 0] [remaining remaining]}
    (cond!
      [(or (empty? remaining) (separator? (first remaining)))
       (values indent (reverse rev-tokens) len remaining)]
      [else (loop
              (cons (first remaining) rev-tokens)
              (+ len (token-length (first remaining)))
              (rest remaining))])))

(define (render-line indent tokens port left right cols)
  (when indent
    (newline port)
    (space port left)
    (space port indent))
  (render-tokens tokens port (+ left (or indent 0)) right cols))

(define (render-tokens tokens port left right cols)
  (unless (empty? tokens)
    (define token (first tokens))
    (define len (token-length token))
    (render-token (first tokens) port left (- right len) cols)
    (render-tokens (rest tokens) port (+ left len) (- right len) cols)))

(define (render-token token port left right cols)
  (cond!
    [(string? token) (write-string token port)]
    [(delimited? token) (render-delimited token port left right cols)]))

(define (render-one-line d port)
  (for {[token (in-list (delimited-contents d))]}
    (cond!
      [(string? token) (write-string token port)]
      [(separator? token) (when (separator-wide? token) (space port))]
      [(delimited? token) (render-one-line token port)])))

(define (space port [n 1])
  (for {[i (in-range n)]}
    (write-char #\space port)))
