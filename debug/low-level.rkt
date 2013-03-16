#lang racket/base

(provide low-level-debug)

(require mischief/function mischief/error)

(define depth 0)

;; Who debugs the debugger:
(define low-level-debug
  (lambda/keywords (ks vs proc . xs)
    (define name (or (object-name proc) proc))
    (dynamic-wind
      (lambda ()
        (set! depth (add1 depth))
        (eprintf "!!>> ~a ~a\n" depth
          (keyword-apply format-application ks vs proc xs)))
      (lambda ()
        (define results
          (call-with-values
            (lambda ()
              (with-handlers
                  {[(lambda (x) #true)
                    (lambda (e)
                      (eprintf "!!-- ~a ~a\n" depth (format-exception e))
                      (raise e))]}
                (keyword-apply proc ks vs xs)))
            list))
        (eprintf "!!-- ~a ~a\n" depth (format-values results))
        (apply values results))
      (lambda ()
        (eprintf "!!<< ~a ~s\n" depth name)
        (set! depth (sub1 depth))))))
