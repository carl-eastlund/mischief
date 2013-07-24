#lang mischief

(provide stylish-test-suite)

(require rackunit (for-syntax mischief))

(module+ test
  (require rackunit/text-ui)
  (void (run-tests stylish-test-suite)))

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
        (test (stylish-value->expr '(0 "text" #true #\c sym))
          '(quote (0 "text" #true #\c sym)))
        (test
          (stylish-value->expr
            (vector-immutable '(1 2 3) '(one two three) "abc"))
          '(quote #((1 2 3) (one two three) "abc")))
        (test
          (stylish-value->expr
            (box-immutable (vector-immutable (list 1 'two "three"))))
          '(quote #&#((1 two "three"))))
        (test
          (stylish-value->expr
            (prefab 'type 1 (box-immutable 2) (list 3)))
          '(quote #s(type 1 #&2 (3))))
        (test (stylish-value->expr (hasheqv 1 'one 2 'two 3 'three))
          '(quote #hasheqv((1 . one) (2 . two) (3 . three)))))

      (test-suite "unquotable values"
        (test (stylish-value->expr (void)) '(void))
        (test (stylish-value->expr (letrec {[a a]} a))
          (stylish-unprintable-expr 'undefined))
        (test (stylish-value->expr eof) 'eof)
        (test (stylish-value->expr (srcloc "source" 1 2 3 4))
          '(srcloc "source" 1 2 3 4))
        (test
          (stylish-value->expr
            (list 1 (list 2 3) (srcloc (list 4 5) 6 7 8 9)))
          '(list 1 (quote (2 3)) (srcloc (quote (4 5)) 6 7 8 9)))
        (test
          (stylish-value->expr
            (vector (quote (1 2 3)) (quote (one two three)) "abc"))
          '(vector '(1 2 3) '(one two three) "abc"))
        (test
          (stylish-value->expr
            (box (vector-immutable (list 1 'two "three"))))
          '(box (quote #((1 two "three")))))))))
