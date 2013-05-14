#lang racket
(provide initialize)
(require mischief/stylish)

(define (initialize lang)
  (current-print stylish-print-handler))
