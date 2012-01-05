#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide

  print-expression

  print-delimited
  print-separator

  stylish-port?
  stylish-port-print-style

  print-style?
  empty-print-style
  extend-print-style
  set-print-style-default-printer
  set-print-style-preserve-cache?
  clear-print-style-cache!

  print-style-extension?
  print-style-extension)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/list
  racket/port
  racket/promise
  data/queue
  mischief/racket/struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; PrintStyle = (print-style PrintDefault (List PrintType) (Maybe PrintCache))
;; PrintDefault = (Maybe (Any OutputPort -> Any))
;; PrintCache = (print-cache (Cache PrintType) (Cache Delimited))
;; (Cache T) = (Hash Any (Promise T))
;; PrintType = {exists T (print-type (Type T) (Print T))}
;; (Type T) = (Any -> Boolean : T)
;; (Print T) = (Any StylishPort -> Void)
(struct print-style [default extensions cache])
(struct print-cache [type-of delim-expr] #:mutable)
(struct print-type [type? printer])

;; Delimited = (delimited Nat (List Token))
;; Token = (Or String Separator Delimited)
;; Separator = (separator Nat Boolean)
(struct delimited [length contents])
(struct separator [indent wide?])

;; StylishPort = (stylish-port StringOutputPort (List Partial))
;; Partial = (stylish-partial PrintStyle (Queue Token))
(struct stylish-port [string-port (stack #:mutable)]
  #:property prop:output-port 0)
(struct stylish-partial [print-style (contents #:mutable)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define (print-expression name st e port left right cols)
  (let* {[st (set-print-style-preserve-cache? st #true)]}
    (print-expr name st e port left right cols)))

(define (print-delimited name port thunk)
  (print-delim name port thunk))

(define (print-separator name indent wide? port)
  (print-sep name indent wide? port))

(define (stylish-port-print-style port)
  (stylish-partial-print-style
    (first (stylish-port-stack port))))

(define empty-print-style
  (print-style #false empty #false))

(define (extend-print-style st after? new-exts)
  (update print-style st
    [extensions (if after?
                  (append extensions new-exts)
                  (append new-exts extensions))]
    [cache (and cache (fresh-cache))]))

(define (set-print-style-default-printer st new-default)
  (update print-style st
    [default new-default]
    [cache (and cache (fresh-cache))]))

(define (set-print-style-preserve-cache? st preserve?)
  (update print-style st
    [cache (and preserve? (or cache (fresh-cache)))]))

(define (clear-print-style-cache! st)
  (cond
    [(print-style-cache st) =>
     (lambda (cache)
       (set-print-cache-type-of! cache (make-weak-hasheq))
       (set-print-cache-delim-expr! cache (make-weak-hasheq)))]
    [else (void)]))

(define (print-style-extension? x)
  (print-type? x))

(define (print-style-extension type? printer)
  (print-type type? printer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging Definition

(define-syntax-rule (log-debugf fmt arg ...)
  (log-debug (format fmt arg ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Definitions

(define (print-expr name st e port left right cols)
  (let* {[stylish? (stylish-port? port)]
         [sp (if stylish? port (make-stylish-port))]
         [d (delim-expr name st e sp)]}
    (unless stylish?
      (log-debugf "\n===== print-expr =====\n")
      (log-debugf "Expression:\n~e\n" d)
      (log-debugf "Delimited:\n~e\n" d))
    (if stylish?
      (stylish-port-enqueue! port d)
      (begin (close-output-port sp)
        (render d port left right cols)))))

(define (print-delim name port thunk)
  (if (stylish-port? port)
    (stylish-port-enqueue! port
      (delim (stylish-port-print-style port) port thunk))
    (thunk)))

(define (print-sep name indent wide? port)
  (if (stylish-port? port)
    (stylish-port-enqueue! port
      (separator indent wide?))
    (when wide? (space port))))

(define (delim st port thunk)
  (let* {[st (set-print-style-preserve-cache? st #true)]}
    (stylish-port-push! port (stylish-partial st (make-queue)))
    (thunk)
    (stylish-port-pop! port)))

(define (delim-expr name st e port)
  (force
    (hash-ref! (print-cache-delim-expr (print-style-cache st)) e
      (lambda ()
        (delay
          (delim st port
            (lambda ()
              (cond
                [(type-of name e st) =>
                 (lambda (type)
                   ((print-type-printer type) e port))]
                [(print-style-default st) =>
                 (lambda (default)
                   (default e port))]
                [else (error name
                        "cannot print expression: ~v")]))))))))

(define (type-of name x st)
  (force
    (hash-ref! (print-cache-type-of (print-style-cache st)) x
      (lambda ()
        (delay
          (let find {[exts (print-style-extensions st)]}
            (if (empty? exts)
              #false
              (if ((print-type-type? (first exts)) x)
                (first exts)
                (find (rest exts))))))))))

(define (render d port left right cols)
  (if (<= (delimited-length d) (- cols left right))
    (render-one-line d port)
    (render-many-lines d port left right cols)))

(define (render-many-lines d port left right cols)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'render-many-lines d)
  (render-contents (delimited-contents d) port left right cols))

(define (render-contents contents port left right cols)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'render-contents contents)
  (unless (empty? contents)
    (define-values {indent tokens len remaining}
      (take-line contents))
    (render-line indent tokens port left (+ len right) cols)
    (render-contents remaining port left right cols)))

(define (take-line contents)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'take-line contents)
  (define-values {indent remaining}
    (if (separator? (first contents))
      (values (separator-indent (first contents)) (rest contents))
      (values #false contents)))
  (let loop {[rev-tokens empty] [len 0] [remaining remaining]}
    (cond
      [(or (empty? remaining) (separator? (first remaining)))
       (values indent (reverse rev-tokens) len remaining)]
      [else (loop
              (cons (first remaining) rev-tokens)
              (+ len (token-length (first remaining)))
              (rest remaining))])))

(define (render-line indent tokens port left right cols)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'render-line indent)
  (when indent
    (newline port)
    (space port left)
    (space port indent))
  (render-tokens tokens port (+ left (or indent 0)) right cols))

(define (render-tokens tokens port left right cols)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'render-tokens tokens)
  (unless (empty? tokens)
    (define token (first tokens))
    (define len (token-length token))
    (render-token (first tokens) port left (- right len) cols)
    (render-tokens (rest tokens) port (+ left len) (- right len) cols)))

(define (render-token token port left right cols)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'render-token token)
  (cond
    [(string? token) (write-string token port)]
    [(delimited? token) (render token port left right cols)]))

(define (render-one-line d port)
  (log-debugf "\n===== ~a =====\nInput:\n~e\n"
    'render-one-line d)
  (for {[token (in-list (delimited-contents d))]}
    (cond
      [(string? token) (write-string token port)]
      [(separator? token) (when (separator-wide? token) (space port))]
      [(delimited? token) (render-one-line token port)])))

(define (finish-partial partial)
  (define contents
    (queue->list (stylish-partial-contents partial)))
  (delimited
    (content-length contents)
    (add-separators contents)))

(define (add-separators contents)
  (let loop {[seen-delimited? #false]
             [contents contents]}
    (if (empty? contents)
      empty
      (let* {[token (first contents)]}
        (cond
          [(string? token)
           (cons token
             (loop seen-delimited? (rest contents)))]
          [(separator? token)
           (cons token
             (loop #false (rest contents)))]
          [(delimited? token)
           (if seen-delimited?
             (cons (separator 0 #f)
               (cons token
                 (loop #true (rest contents))))
             (cons token
               (loop #true (rest contents))))])))))

(define (content-length contents)
  (let loop {[contents contents] [len 0]}
    (cond
      [(empty? contents) len]
      [else (loop (rest contents)
              (+ len (token-length (first contents))))])))

(define (token-length token)
  (cond
    [(string? token) (string-length token)]
    [(delimited? token) (delimited-length token)]
    [(separator? token) (if (separator-wide? token) 1 0)]))

(define (make-stylish-port)
  (stylish-port (open-output-string 'stylish) empty))

(define (stylish-port-push! port partial)
  (stylish-port-flush! port)
  (set-stylish-port-stack! port
    (cons partial (stylish-port-stack port))))

(define (stylish-port-pop! port)
  (stylish-port-flush! port)
  (define stack (stylish-port-stack port))
  (set-stylish-port-stack! port (rest stack))
  (finish-partial (first stack)))

(define (stylish-port-enqueue! port token)
  (stylish-port-flush! port)
  (stylish-port-primitive-enqueue! port token))

(define (stylish-port-flush! port)
  (unless (empty? (stylish-port-stack port))
    (define str (get-output-string (stylish-port-string-port port)))
    (flush-output-string! (stylish-port-string-port port))
    (stylish-port-primitive-enqueue! port str)))

(define (stylish-port-primitive-enqueue! port token)
  (enqueue!
    (stylish-partial-contents
      (first (stylish-port-stack port)))
    token))

(define (fresh-cache)
  (print-cache
    (make-weak-hasheq)
    (make-weak-hasheq)))

(define (flush-output-string! port)
  (void (get-output-bytes port #true 0 0)))

(define (space port [n 1])
  (for {[i (in-range n)]}
    (write-char #\space port)))
