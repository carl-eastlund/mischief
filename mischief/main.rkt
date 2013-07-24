#lang racket/base
(require mischief/require)

(module+ test
  (require rackunit/docs-complete))

(module+ reflection
  (require scribble/manual)
  (provide phase-1-exports phase-0-exports)
  (define phase-1-exports empty)
  (define phase-0-exports empty))

(define-syntax-rule (require/provide-phase-1 mod-path ...)
  (begin
    (require/provide
      (only-meta-in 1
        (for-syntax mod-path ...)))
    (module+ reflection
      (set! phase-1-exports
        (list* 'mod-path ... phase-1-exports)))))

(define-syntax-rule (require/provide-phase-0 mod-path ...)
  (begin
    (require/provide
      (only-meta-in 0
        mod-path ...))
    (module+ reflection
      (set! phase-0-exports
        (list* 'mod-path ... phase-0-exports)))))

(define-syntax-rule (require/provide/check-docs-phase-0 mod-path ...)
  (begin
    (require/provide-phase-0 mod-path ...)
    (module+ test (check-docs (quote mod-path)) ...)
    (module+ reflection
      (set! phase-0-exports
        (list* 'mod-path ... phase-0-exports)))))

(require/provide-phase-1
  racket)

(require/provide-phase-0
  racket
  racket/block
  racket/generic
  racket/pretty
  racket/syntax
  racket/splicing
  racket/generator
  racket/runtime-path
  racket/date
  data/queue
  syntax/parse
  syntax/parse/define
  syntax/parse/experimental/specialize
  syntax/parse/experimental/template
  syntax/id-table
  syntax/kerncase
  syntax/srcloc
  syntax/location
  syntax/strip-context)

(require/provide/check-docs-phase-0
  mischief/define
  mischief/values
  mischief/error
  mischief/dict
  mischief/for
  mischief/list
  mischief/contract
  mischief/match
  mischief/function
  mischief/boolean
  mischief/maybe
  mischief/symbol
  mischief/keyword
  mischief/quotation
  mischief/phrase
  mischief/stylish
  mischief/string
  mischief/struct
  mischief/visitor
  mischief/memoize
  mischief/require
  mischief/module
  mischief/sort
  mischief/scope
  mischief/parse
  mischief/dye-pack
  mischief/transform
  mischief/shorthand
  mischief/fold
  mischief/id-table
  mischief/stream)
