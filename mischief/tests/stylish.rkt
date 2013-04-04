#lang mischief

(provide stylish-test-suite)

(require rackunit (for-syntax mischief))

(module+ test
  (require rackunit/text-ui)
  (run-tests stylish-test-suite))

(module+ main
  (require rackunit/gui)
  (test/gui stylish-test-suite #:wait? #true))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ actual:expr expected:expr)
     (quasisyntax/loc stx
       (test-case (format "~s" 'actual)
         (define a #false)
         (define e #false)
         #,(syntax/loc stx (check-not-exn (lambda {} (set! a actual))))
         #,(syntax/loc stx (check-not-exn (lambda {} (set! e expected))))
         #,(syntax/loc stx (check-equal? a e))))]))

(define stylish-test-suite
  (test-suite "mischief/stylish"

    (test-suite "stylish-value->expr"

      (test-suite "self-quoting values"
        (test (stylish-value->expr 0) 0)
        (test (stylish-value->expr "text") "text")
        (test (stylish-value->expr #true) #true)
        (test (stylish-value->expr #\c) #\c))

      (test-suite "quotable values"
        (test (stylish-value->expr 'sym) '(quote sym))
        (test (stylish-value->expr '()) '(quote ()))
        (test
          (stylish-value->expr '(0 "text" #true #\c sym))
          '(quote (0 "text" #true #\c sym)))))))
