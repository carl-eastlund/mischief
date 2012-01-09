#lang racket/base

(provide
  impossible)

(require
  (for-syntax
    racket/base
    syntax/parse
    mischief/syntax/transform)
  syntax/location
  syntax/srcloc)

(define-syntax impossible
  (id-transformer
    (syntax-parser
      [self:id
       #'(make-impossible (quote self) (quote-srcloc self))])))

(define (make-impossible name loc)
  (make-keyword-procedure
    (lambda (ks vs . xs)
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
              (format " ~v" x))))))))
