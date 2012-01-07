#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide
  print-formatted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/port
  racket/list
  racket/match
  mischief/racket/stylish/expression
  mischief/racket/stylish/print
  mischief/racket/boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define (print-formatted name est pst port fmt args0)

  (define n (string-length fmt))

  (define (print-from-index i args)
    (cond!
      [(= i n)
       (unless (empty? args)
         (error name "unused arguments at ~a in ~a" i (reconstruct)))]
      [(< i n)
       (define ch (string-ref fmt i))
       (cond!
         [(char=? ch #\~)
          (escape-from-index (add1 i) args)]
         [else
          (write-char ch port)
          (print-from-index (add1 i) args)])]))

  (define (escape-from-index i args)
    (cond!
      [(= i n) (error name "unterminated escape at ~a in ~a" i (reconstruct))]
      [(< i n)
       (match (string-ref fmt i)
         [#\~ (write-char #\~ port)]
         [#\a (format-at-index i args display)]
         [#\s (format-at-index i args
                (lambda (e port)
                  (print-expression name e pst port)))]
         [#\v (format-at-index i args
                (lambda (x port)
                  (define e (value->expression name x est))
                  (print-expression name e pst port)))]
         [ch (error name
               "invalid escape character `~a' at ~a in ~a"
               ch
               i
               (reconstruct))])]))

  (define (format-at-index i args show)
    (unless (cons? args)
      (error name "too few arguments at ~a in ~a" i (reconstruct)))
    (show (first args) port)
    (print-from-index (add1 i) (rest args)))

  (define (reconstruct)
    (with-output-to-string
      (printf "(~a" name)
      (for {[arg (in-list args0)]}
        (printf " ~v" arg))
      (printf ")")))

  (print-from-index 0 args0))
