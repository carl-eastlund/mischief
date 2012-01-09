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
    mischief/syntax/transform)
  racket/port
  syntax/location
  syntax/srcloc
  mischief/racket/function)

(define-syntax impossible
  (id-transformer
    (syntax-parser
      [self:id
       #'(make-impossible (quote self) (quote-srcloc self))])))

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
