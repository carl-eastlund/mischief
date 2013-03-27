#lang racket/unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Imports

(require
  racket/function
  racket/port
  mischief/stylish/signatures)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Imports

(import expression^ expr-style^ print^ print-style^ format^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Exports

(export stylish^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging Definition

(define-syntax-rule (log-debugf fmt arg ...)
  (log-debug (format fmt arg ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(struct stylish-comment-expr [comment expr] #:transparent)
(struct stylish-unprintable-expr [name] #:transparent)

(define (stylish-print-handler v)
  (unless (void? v)
    (stylish-println v)))

(define (stylish-printf
          #:port [port (current-output-port)]
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)]
          fmt . args)
  (print-to-stylish-port 'stylish-printf port left right columns
    (lambda (port)
      (print-formatted 'stylish-printf est pst port fmt args))))

(define (stylish-format
          #:expr-style [est (current-expr-style)]
          #:print-style [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)]
          fmt . args)
  (call-with-output-string
    (lambda (port)
      (print-to-stylish-port 'stylish-format port left right columns
        (lambda (port)
          (print-formatted 'stylish-format est pst port fmt args))))))

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
      (print-expression 'stylish-print e pst port)
      (void))))

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

(define (stylish-print-as-string v
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

(define (stylish-write e
          [port (current-output-port)]
          [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (print-to-stylish-port 'stylish-print-expr port left right columns
    (lambda (port)
      (print-expression 'stylish-print-expr e pst port)
      (void))))

(define (stylish-writeln e
          [port (current-output-port)]
          [pst (current-print-style)]
          #:left [left 0]
          #:columns [columns (current-stylish-print-columns)])
  (print-to-stylish-port 'stylish-println-expr port left 0 columns
    (lambda (port)
      (print-expression 'stylish-println-expr e pst port)))
  (newline port))

(define (stylish-write-as-string e
          [pst (current-print-style)]
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (call-with-output-string
    (lambda (port)
      (print-to-stylish-port 'stylish-expr->string port left right columns
        (lambda (port)
          (print-expression 'stylish-expr->string e pst port))))))

(define (call-with-stylish-port port proc
          #:left [left 0]
          #:right [right 0]
          #:columns [columns (current-stylish-print-columns)])
  (print-to-stylish-port 'call-with-stylish-port port left right columns
    proc))

(define-syntax-rule (with-stylish-port body ...)
  (call-with-stylish-port (current-output-port)
    (lambda (port)
      (parameterize {[current-output-port port]}
        body ...))))

(define (stylish-print-separator port
          #:indent [indent 0]
          #:wide? [wide? #t])
  (print-separator 'stylish-print-separator port indent wide?))

(define (stylish-quotable-value? v [est (current-expr-style)])
  (value-quotable? 'stylish-quotable-value? v est))

(define (stylish-value->expr v [est (current-expr-style)])
  (value->expression 'stylish-value->expr v est))

(define current-stylish-print-columns (make-parameter 80))
