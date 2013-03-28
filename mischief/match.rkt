#lang racket/base

(provide
  match!
  match*!)

(require
  (for-syntax
    racket/base
    syntax/parse
    mischief/transform)
  racket/match
  racket/pretty
  racket/string
  mischief/function
  syntax/srcloc
  syntax/location)

(struct exn:fail:match! exn:fail [inputs srclocs]
  #:property prop:exn:srclocs (eta exn:fail:match!-srclocs))

(define-syntax (match! stx)
  (syntax-parse stx
    [(_ e:expr [pat tem:expr ...+] ...)
     #`(match e
         [pat tem ...] ...
         [x (match!-error 'match!
              (list x)
              (quote-srcloc #,stx))])]))

(define-syntax (match*! stx)
  (syntax-parse stx
    [(_ {e:expr ...} [{pat ...} tem:expr ...+] ...)
     (define/syntax-parse [x ...]
       (for/list {[stx (in-list (attribute e))]}
         (fresh 'input #:source stx)))
     #`(match* {e ...}
         [{pat ...} tem ...] ...
         [{x ...} (match!-error 'match*!
                    (list x ...)
                    (quote-srcloc #,stx))])]))

(define (match!-error sym xs src)
  (raise
    (exn:fail:match!
      (format "~a~a: no matching clause for:\n~a"
        (source-location->prefix src)
        sym
        (string-join (map pretty-format xs) "\n"))
      (current-continuation-marks)
      xs
      (list src))))
