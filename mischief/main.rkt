#lang racket/base
(require mischief/require)

(module+ test
  (require rackunit/docs-complete))

(define-syntax-rule (require/provide-phase-0 spec ...)
  (require/provide
    (only-meta-in 0
      spec ...)))

(define-syntax-rule (require/provide/check-docs-phase-0 mod-path ...)
  (begin
    (require/provide-phase-0 mod-path ...)
    (module+ test (check-docs (quote mod-path)) ...)))

(require/provide-phase-0
  racket
  racket/block
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
    mischief/srcloc
    mischief/location
    mischief/fold
    mischief/id-table)

(require/provide
  (for-syntax
    (only-meta-in 0
      racket)))
