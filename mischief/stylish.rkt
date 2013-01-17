#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports and Exports

(require
  mischief/require)

(require/provide
  mischief/stylish/stylish
  mischief/stylish/print-style
  mischief/stylish/expr-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic Configuration

(current-print-style default-print-style)
(current-expr-style default-expr-style)
