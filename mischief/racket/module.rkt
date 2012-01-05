#lang racket/base

(provide
  resolve-module-path)

(define (resolve-module-path mod-path
          #:module-name-resolver [resolver (current-module-name-resolver)]
          #:relative-to [relative-to #false]
          #:syntax-context [syntax-context #false]
          #:load? [load? #true])
  (resolver mod-path relative-to syntax-context load?))
