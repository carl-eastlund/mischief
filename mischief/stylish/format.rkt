#lang racket/unit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module Imports

(require
  racket/port
  racket/list
  racket/match
  mischief/boolean
  mischief/stylish/signatures
  no-debug/low-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Imports

(import expression^ print^)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Exports

(export format^)

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
       (match (string-ref fmt i)
         [#\~ (escape-from-index (add1 i) args)]
         [#\newline
          (error name "invalid newline at ~a in ~a" i (reconstruct))]
         [#\space
          (low-level-debug print-separator name port 1 #true)
          (print-from-index (add1 i) args)]
         [ch
          (write-char ch port)
          (print-from-index (add1 i) args)])]))

  (define (escape-from-index i args)
    (cond!
      [(= i n) (error name "unterminated escape at ~a in ~a" i (reconstruct))]
      [(< i n)
       (match (string-ref fmt i)
         [#\~
          (write-char #\~ port)
          (print-from-index (add1 i) args)]
         [(? char-numeric? ch)
          (define n (char->number ch))
          (low-level-debug print-separator name port n #true)
          (print-from-index (add1 i) args)]
         [#\a (format-at-index i args display)]
         [#\s (format-at-index i args
                (lambda (e port)
                  (low-level-debug print-expression name e pst port)))]
         [#\v (format-at-index i args
                (lambda (x port)
                  (define e (low-level-debug value->expression name x est))
                  (low-level-debug print-expression name e pst port)))]
         [#\f (format-at-index i args
                (lambda (fmt+args port)
                  (unless (and
                            (list? fmt+args)
                            (cons? fmt+args)
                            (string? (first fmt+args)))
                    (error name
                      "~a; expected ~a, but found: ~v"
                      "invalid argument to ~f (nested format) escape"
                      "a non-empty list with a string as its first element"
                      fmt+args))
                  (print-formatted name est pst port
                    (first fmt+args)
                    (rest fmt+args))))]
         [ch (error name
               "invalid escape character `~a' at ~a in ~a"
               ch
               i
               (reconstruct))])]))

  (define (format-at-index i args show)
    (unless (cons? args)
      (error name "too few arguments at ~a in ~a" i (reconstruct)))
    (low-level-debug print-to-stylish-port name port
      #false #false #false ;; dummy left/right/cols
      (lambda (port)
        (show (first args) port)))
    (print-from-index (add1 i) (rest args)))

  (define (reconstruct)
    (with-output-to-string
      (lambda ()
        (printf "(~a ~v" name fmt)
        (for {[arg (in-list args0)]}
          (printf " ~v" arg))
        (printf ")"))))

  (define (char->number ch)
    (string->number (string ch)))

  (print-from-index 0 args0))
