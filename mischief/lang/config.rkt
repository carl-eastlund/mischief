#lang racket
(provide configure)

(define table
  (hash
    'configure-runtime
    (list (vector 'mischief/lang/runtime 'initialize 'mischief))))

(define ((configure lang) sym default)
  (dict-ref table sym (lambda {} default)))
