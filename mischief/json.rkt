#lang mischief

(provide
  stylish-write-json
  stylish-writeln-json
  json-print-style)

(require json)

(module+ main
  (stylish-writeln-json
    (for/hasheq {[c (in-string "abcdefghijklmnopqrstuvwxyz")]
                 [n (in-naturals 1)]}
      (values (string->symbol (string c))
        (for/list {[i (in-range n)]}
          (cond
            [(zero? i) 'null]
            [(exact-integer? (sqrt i)) (even? i)]
            [(even? i) (- i)]
            [else (exact->inexact i)]))))))

(at-end
  (define json-print-style
    (extend-print-style empty-print-style
      (print-style-extension string? print-json-string)
      (print-style-extension number? print-json-number)
      (print-style-extension hash? print-json-object)
      (print-style-extension list? print-json-array)
      (print-style-extension boolean? print-json-boolean)
      (print-style-extension json-null? print-json-null))))

(define (stylish-write-json x [port (current-output-port)])
  (stylish-write x port json-print-style))

(define (stylish-writeln-json x [port (current-output-port)])
  (stylish-writeln x port json-print-style))

(define (json-null? x)
  (equal? x (json-null)))

(define (print-json-string x port style)
  (write-json x port))

(define (print-json-number x port style)
  (write-json x port))

(define (print-json-object x port style)
  (display "{" port)
  (call-with-stylish-port port
    (lambda {port}
      (define first-time? #true)
      (for {[{k v} (in-hash x)]}
        (cond
          [first-time? (set! first-time? #false)]
          [else (begin (display "," port)
                  (stylish-print-separator port))])
        (call-with-stylish-port port
          (lambda {port}
            (stylish-write (symbol->string k) port style)
            (display ":" port)
            (stylish-write v port style))))))
  (display "}" port))

(define (print-json-array x port style)
  (display "[" port)
  (call-with-stylish-port port
    (lambda {port}
      (define first-time? #true)
      (for {[v (in-list x)]}
        (cond
          [first-time? (set! first-time? #false)]
          [else (begin (display "," port)
                  (stylish-print-separator port))])
        (stylish-write v port style))))
  (display "]" port))

(define (print-json-boolean x port style)
  (display (if x "true" "false") port))

(define (print-json-null x port style)
  (display "null" port))
