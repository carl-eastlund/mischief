#lang racket/base

(provide
  procedure-arity->phrase
  list->phrase
  count->phrase)

(require
  racket/list
  mischief/boolean
  mischief/function)

(define procedure-arity->phrase
  (case-lambda
    [(proc-or-arity)
     (cond
       [(procedure? proc-or-arity)
        (define arity (procedure-arity proc-or-arity))
        (define-values {required optional} (procedure-keywords proc-or-arity))
        (arity->phrase arity required optional)]
       [else
        (arity->phrase (normalize-procedure-arity proc-or-arity) '() '())])]
    [(arity required optional)
     (arity->phrase
       (normalize-procedure-arity arity)
       (sort required keyword<?)
       (and optional (sort optional keyword<?)))]))

(define (arity->phrase arity required optional)
  (list->phrase #:separator ";"
    (list/optional
      (positional-arity->phrase (normalize-procedure-arity arity))
      (required-arity->phrase required)
      (optional-arity->phrase optional required))))

(define (positional-arity->phrase arity)
  (cond
    [(list? arity)
     (list->phrase
       #:final "or"
       #:none "no accepted number of arguments (cannot be applied)"
       (map positional-arity-clause->phrase arity))]
    [else (positional-arity-clause->phrase arity)]))

(define (positional-arity-clause->phrase arity)
  (cond
    [(arity-at-least? arity)
     (format "~a or more arguments" (arity-at-least-value arity))]
    [else (count->phrase arity "argument")]))

(define (count->phrase count singular [plural #false])
  (cond
    [(= count 1) (format "~a ~a" count singular)]
    [plural (format "~a ~a" count plural)]
    [else (format "~a ~a~a" count singular 's)]))

(define (required-arity->phrase required)
  (keyword-arity->phrase required))

(define (optional-arity->phrase optional required)
  (cond
    [(not optional)
     (format "any~a keyword arguments"
       (if (null? required) "" " other"))]
    [else (keyword-arity->phrase #:optional? #true
            (optional-only optional required))]))

(define (optional-only optional required)
  (cond
    [(empty? required) optional]
    [(empty? optional) empty]
    [(keyword<? (first optional) (first required))
     (cons (first optional) (optional-only (rest optional) required))]
    [(keyword<? (first required) (first optional))
     (optional-only optional (rest required))]
    [else
     (optional-only (rest optional) (rest required))]))

(define (keyword-arity->phrase keywords #:optional? [optional? #false])
  (cond
    [(empty? keywords) #false]
    [(empty? (rest keywords))
     (format "an ~aargument of keyword ~s"
       (if optional? "optional " "")
       (first keywords))]
    [else (format "~aarguments of keywords ~a"
            (if optional? "optional " "")
            (list->phrase
              (for/list {[kw (in-list keywords)]}
                (format "~s" kw))))]))

(define (list->phrase xs
          #:none [none "none"]
          #:separator [sep ","]
          #:final [final "and"])
  (cond
    [(empty? xs) none]
    [(empty? (rest xs)) (first xs)]
    [(empty? (rest (rest xs)))
     (format "~a and ~a" (first xs) (second xs))]
    [else (apply string-append
            (let loop {[xs xs]}
              (cond
                [(empty? (rest xs))
                 (if final
                   (list final " " (first xs))
                   (list (first xs)))]
                [else (list* (first xs) sep " " (loop (rest xs)))])))]))
