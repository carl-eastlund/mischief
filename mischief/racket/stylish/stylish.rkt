#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide

  stylish-print
  stylish-println
  stylish-value->string

  stylish-print-expr
  stylish-println-expr
  stylish-expr->string
  stylish-print-delimited
  stylish-print-separator

  stylish-quotable-value?
  stylish-value->expr

  print-style?
  empty-print-style
  current-print-style
  set-print-style-default-printer
  set-print-style-preserve-cache?
  print-style-extension?
  current-stylish-print-columns

  (struct-out
    stylish-comment-expr)

  expr-style?
  empty-expr-style
  current-expr-style
  set-expr-style-default-convert
  set-expr-style-preserve-cache?
  expr-style-extension?

  (rename-out
    [stylish-extend-print-style extend-print-style]
    [stylish-extend-expr-style extend-expr-style]
    [stylish-expr-style-extension expr-style-extension]
    [stylish-print-style-extension print-style-extension]
    [stylish-clear-print-style-cache! clear-print-style-cache!]
    [stylish-clear-expr-style-cache! clear-expr-style-cache!]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/function
  racket/port
  mischief/racket/stylish/expression
  mischief/racket/stylish/print)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging Definition

(define-syntax-rule (log-debugf fmt arg ...)
  (log-debug (format fmt arg ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(struct stylish-comment-expr [comment expr] #:transparent)

(define (stylish-print v [port (current-output-port)]
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (define e (value->expression 'stylish-print v est))
  (log-debugf "\n===== stylish-print =====\n")
  (log-debugf "Convert:\n~e\n" v)
  (log-debugf "Print:\n~e\n" e)
  (print-to-stylish-port 'stylish-print port left right columns
    (lambda (port)
      (print-expression 'stylish-print e pst port))))

(define (stylish-println v [port (current-output-port)]
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:columns [columns (current-stylish-print-columns)])
  (define e (value->expression 'stylish-println v est))
  (log-debugf "\n===== stylish-println =====\n")
  (log-debugf "Convert:\n~e\n" v)
  (log-debugf "Print:\n~e\n" e)
  (print-to-stylish-port 'stylish-println port left 0 columns
    (lambda (port)
      (print-expression 'stylish-println e pst port)))
  (newline port))

(define (stylish-value->string v
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (define e (value->expression 'stylish-print v est))
  (define s
    (call-with-output-string
      (lambda (port)
        (print-to-stylish-port 'stylish-value->string port left right columns
          (lambda (port)
            (print-expression 'stylish-value->string e pst port))))))
  (log-debugf "\n===== stylish-value->string =====\n")
  (log-debugf "Convert:\n~e\n" v)
  (log-debugf "Print:\n~e\n" e)
  (log-debugf "Return:\n~e\n" s)
  s)

(define (stylish-print-expr e [port (current-output-port)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (print-to-stylish-port 'stylish-print-expr port left right columns
    (lambda (port)
      (print-expression 'stylish-print-expr e pst port))))

(define (stylish-println-expr e [port (current-output-port)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:columns [columns (current-stylish-print-columns)])
  (print-to-stylish-port 'stylish-println-expr port left 0 columns
    (lambda (port)
      (print-expression 'stylish-println-expr e pst port)))
  (newline port))

(define (stylish-expr->string e
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (call-with-output-string
    (lambda (port)
      (print-to-stylish-port 'stylish-expr->string port left right columns
        (lambda (port)
          (print-expression 'stylish-expr->string e pst port))))))

(define (stylish-print-delimited port proc)
  (print-to-stylish-port 'stylish-print-delimited port 0 0
    (current-stylish-print-columns)
    proc))

(define (stylish-print-separator port [indent 0] [wide? #t])
  (print-separator 'stylish-print-separator port indent wide?))

(define (stylish-quotable-value? v [est (current-expr-style)])
  (value-quotable? 'stylish-quotable-value? v est))

(define (stylish-value->expr v [est (current-expr-style)])
  (value->expression 'stylish-value->expr v est))

(define current-print-style (make-parameter empty-print-style))
(define current-expr-style (make-parameter empty-expr-style))
(define current-stylish-print-columns (make-parameter 80))

(define (stylish-extend-print-style pst #:after? [after? #false] . exts)
  (extend-print-style pst after? exts))

(define (stylish-extend-expr-style est #:after? [after? #false] . exts)
  (extend-expr-style est after? exts))

(define (stylish-print-style-extension type? printer)
  (print-style-extension type? printer))

(define (stylish-expr-style-extension
          type? convert [quotable? (const #false)] [try-quote? #true])
  (expr-style-extension type? convert quotable? try-quote?))

(define (stylish-clear-print-style-cache! [pst (current-print-style)])
  (clear-print-style-cache! pst))

(define (stylish-clear-expr-style-cache! [est (current-expr-style)])
  (clear-expr-style-cache! est))
