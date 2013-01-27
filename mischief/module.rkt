#lang racket/base

(provide
  resolve-module-path
  module-name?)

(define (resolve-module-path mod-path
          #:namespace [namespace (current-namespace)]
          #:module-name-resolver [resolver (current-module-name-resolver)]
          #:relative-to [relative-to #false]
          #:syntax-context [syntax-context #false]
          #:load? [load? #true])
  (parameterize {[current-namespace namespace]}
    (resolver mod-path relative-to syntax-context load?)))

(define (module-name? x)
  (cond
    [(list? x)
     (and (pair? x) (module-base-name? (car x))
       (and (pair? (cdr x))
         (andmap symbol? (cdr x))))]
    [else (module-base-name? x)]))

(define (module-base-name? x)
  (cond
    [(path? x) (complete-path? x)]
    [else (symbol? x)]))
