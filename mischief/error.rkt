#lang racket/base

(provide
  impossible
  format-application
  format-values
  format-exception)

(require
  (for-syntax
    racket/base
    syntax/parse
    mischief/transform)
  racket/port
  syntax/srcloc
  syntax/location
  mischief/function)

(define-syntax impossible
  (syntax-parser
    [self:id
     #'(report-impossible 'self (quote-srcloc self))]
    [(self:id (~or arg:expr (~seq key:keyword val:expr)) ...)
     (define/syntax-parse {[key/val ...] ...} #'{[key val] ...})
     #'(report-impossible 'self (quote-srcloc self)
         (format-application 'self arg ... key/val ... ...))]))

(define (report-impossible name loc [expr #false])
  (error name
    "internal error~a; should never be called~a"
    (if (source-location-known? loc)
      (format " at ~a" (source-location->string loc))
      "")
    (if expr
      (format ": ~a" expr)
      "")))

(define (make-impossible name loc)
  (define/keywords (proc ks vs . xs)
    (error name
      "~ainternal error; should never be called: ~a"
      (source-location->prefix loc)
      (format "(~s~a~a)"
        name
        (apply string-append
          (for/list {[k (in-list ks)]
                     [v (in-list vs)]}
            (format " ~s ~v" k v)))
        (apply string-append
          (for/list {[x (in-list xs)]}
            (format " ~v" x))))))
  (procedure-rename proc name))

(define/keywords (format-application ks vs proc . xs)
  (with-output-to-string
    (lambda ()
      (printf "(~s" (or (object-name proc) proc))
      (for {[k (in-list ks)] [v (in-list vs)]}
        (printf " ~s ~v" k v))
      (for {[x (in-list xs)]}
        (printf " ~v" x))
      (printf ")"))))

(define format-values
  (case-lambda
    [(x) (format "~v" x)]
    [xs (apply format-application 'values xs)]))

(define (format-exception e)
  (cond
    [(exn? e) (format-application 'error (exn-message e))]
    [else (format-application 'raise e)]))
