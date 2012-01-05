#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports and Exports

(require
  mischief/racket/require)

(require/provide
  mischief/racket/stylish/stylish
  mischief/racket/stylish/print-style
  mischief/racket/stylish/expr-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic Configuration

(current-print-style default-print-style)
(current-expr-style default-expr-style)
