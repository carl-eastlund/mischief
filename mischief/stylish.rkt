#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require
  racket/unit
  racket/contract
  mischief/stylish/signatures
  mischief/stylish/expression
  mischief/stylish/expr-style
  mischief/stylish/print
  mischief/stylish/print-style
  mischief/stylish/format
  mischief/stylish/stylish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit Invocation

(define-values/invoke-unit/infer
  (link
    expression@
    print@
    format@
    stylish@
    expr-style@
    print-style@))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports

(provide

  with-stylish-port

  gen:stylish-writable
  generic-stylish-write

  gen:stylish-printable
  generic-stylish-value->expr
  generic-stylish-quotable?

  ;; struct-out will not work here currently:
  stylish-comment-expr
  stylish-comment-expr?
  stylish-comment-expr-comment
  stylish-comment-expr-expr

  ;; struct-out will not work here currently:
  stylish-unprintable-expr
  stylish-unprintable-expr?
  stylish-unprintable-expr-name

  (contract-out

    [stylish-print
     (->*
         {any/c}
         {output-port?
          #:expr-style expr-style?
          #:print-style print-style?
          #:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       void?)]
    [stylish-println
     (->*
         {any/c}
         {output-port?
          #:expr-style expr-style?
          #:print-style print-style?
          #:left exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       void?)]
    [stylish-print-handler
     (-> any/c void?)]
    [stylish-print-as-string
     (->*
         {any/c}
         {#:expr-style expr-style?
          #:print-style print-style?
          #:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       string?)]

    [stylish-write
     (->*
         {any/c}
         {output-port?
          print-style?
          #:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       void?)]
    [stylish-writeln
     (->*
         {any/c}
         {output-port?
          print-style?
          #:left exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       void?)]
    [stylish-write-as-string
     (->*
         {any/c}
         {print-style?
          #:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       string?)]

    [stylish-value->expr
     (->*
         {any/c}
         {expr-style?}
       any/c)]
    [stylish-quotable-value?
     (->*
         {any/c}
         {expr-style?}
       boolean?)]

    [stylish-printf
     (->*
         {string?}
         {#:port output-port?
          #:expr-style expr-style?
          #:print-style print-style?
          #:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       #:rest list?
       void?)]
    [stylish-format
     (->*
         {string?}
         {#:expr-style expr-style?
          #:print-style print-style?
          #:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       #:rest list?
       string?)]

    [call-with-stylish-port
     (->*
         {output-port?
          (-> output-port? any)}
         {#:left exact-nonnegative-integer?
          #:right exact-nonnegative-integer?
          #:columns (or/c exact-nonnegative-integer? 'infinity)}
       any)]
    [stylish-print-separator
     (->*
         {output-port?}
         {#:indent exact-nonnegative-integer?
          #:wide? boolean?}
       void?)]

    [current-stylish-print-columns
     (parameter/c (or/c exact-nonnegative-integer? 'infinity))]

    [print-style? predicate/c]
    [empty-print-style print-style?]
    [simple-print-style print-style?]
    [default-print-style print-style?]
    [print-style-extension? predicate/c]
    [print-style-extension
      (->
        predicate/c
        (-> any/c output-port? print-style? any)
        print-style-extension?)]
    [extend-print-style
      (->*
          {print-style?}
          {#:after? boolean?}
        #:rest (listof print-style-extension?)
        print-style?)]
    [set-print-style-default-printer
     (->
       print-style?
       (or/c (-> any/c output-port? void?) #false)
       print-style?)]
    [current-print-style (parameter/c print-style?)]

    [expr-style? predicate/c]
    [empty-expr-style expr-style?]
    [simple-expr-style expr-style?]
    [default-expr-style expr-style?]
    [expr-style-extension? predicate/c]
    [expr-style-extension
      (->*
          {predicate/c
           (-> any/c expr-style? any/c)}
          {(-> any/c expr-style? boolean?)
           boolean?}
        expr-style-extension?)]
    [extend-expr-style
      (->*
          {expr-style?}
          {#:after? boolean?}
        #:rest (listof expr-style-extension?)
        expr-style?)]
    [set-expr-style-default-convert
     (->
       expr-style?
       (or/c (-> any/c any/c) #false)
       expr-style?)]
    [current-expr-style (parameter/c expr-style?)]))
