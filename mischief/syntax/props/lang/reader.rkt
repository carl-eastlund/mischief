#lang racket/base

(provide
  (rename-out
    [read/stx-props read]
    [read-syntax/stx-props read-syntax]
    [get-info/stx-props get-info]))

(require
  racket/match
  racket/function
  syntax/module-reader)

(define (parse-language-path bstr)
  (define sym
    (string->symbol
      (bytes->string/latin-1 bstr)))
  (and (module-path? sym) sym))

(define (convert-reader reader)
  (lambda args
    (define rt0 (current-readtable))
    (define rt (make-readtable rt0 #\@ 'dispatch-macro read-stx-props))
    (parameterize {[current-readtable rt]}
      (apply reader args))))

(define (read-stx-props ch port src line col pos)
  (match (read/recursive port)
    [(list (list ks vs) ...)
     (for/fold
         {[stx (read-syntax/recursive src port)]}
         {[k (in-list ks)] [v (in-list vs)]}
       (syntax-property stx k v))]
    [_ (raise
         (exn:fail:read
           (list (srcloc src line col pos 0))
           "expected a list of two-element key/value lists"
           (current-continuation-marks)))]))

(define-values {read/stx-props read-syntax/stx-props get-info/stx-props}
  (make-meta-reader
    'stx-props
    "language path"
    parse-language-path
    convert-reader
    convert-reader
    identity))
