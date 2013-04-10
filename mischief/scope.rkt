#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse)
  racket/list
  racket/function
  racket/match
  racket/syntax
  racket/contract
  syntax/kerncase
  syntax/parse)

(provide
  with-new-scope
  (contract-out
    [scope? (-> any/c boolean?)]
    [scope-definition-context (-> scope? def-ctx?)]
    [scope-expansion-context (-> scope? exp-ctx?)]
    [current-scope (parameter/c scope?)]
    [new-scope (->* {} {#:scope scope?} scope?)]
    [close-scope! (->* {} {#:scope scope?} void?)]
    [call-with-new-scope (->* {(-> scope? any)} {#:scope scope?} any)]
    [expand-in-scope
     (->*
         {syntax?}
         {#:scope scope? #:stop-at (or/c (listof syntax?) #false)}
       syntax?)]
    [expand-expression-in-scope
     (->*
         {syntax?}
         {#:scope scope? #:stop-at (or/c (listof syntax?) #false)}
       syntax?)]
    [expand-in-scope-for-syntax
     (->*
         {syntax?}
         {#:scope scope? #:stop-at (or/c (listof syntax?) #false)}
       syntax?)]
    [in-scope (->* {syntax?} {#:scope scope?} syntax?)]
    [out-of-scope (->* {syntax?} {#:scope scope?} syntax?)]
    [scope-bind-value! (->* {identifier?} {#:scope scope?} void?)]
    [scope-bind-values! (->* {(listof identifier?)} {#:scope scope?} void?)]
    [scope-bind-syntax! (->* {identifier? any/c} {#:scope scope?} void?)]
    [scope-bind-syntaxes!
     (->* {(listof identifier?) list?} {#:scope scope?} void?)]
    [scope-bind-syntax/eval! (->* {identifier? syntax?} {#:scope scope?} void?)]
    [scope-bind-syntaxes/eval!
     (->* {(listof identifier?) syntax?} {#:scope scope?} void?)]
    [scope-static-binding? (->* {identifier?} {#:scope scope?} boolean?)]
    [scope-static-value
     (->*
         {identifier?}
         {#:rename? boolean?
          #:scope scope?
          #:success (-> any/c any)
          #:failure (-> any)}
       any)]
    [scope-delta-introducer
     (->* {identifier?} {#:scope scope?} (-> syntax? syntax?))]
    [eval-in-scope (->* {syntax?} {#:scope scope?} any)]))

(define (def-ctx? x)
  (or (internal-definition-context? x) (eq? x #false)))

(define (exp-ctx? x)
  (case x
    [(expression top-level module module-begin) #true]
    [else (list? x)]))

(struct scope [def-ctx exp-ctx])

(define initial-scope
  (scope #f #f))

(define current-scope
  (make-parameter initial-scope))

(define (scope-definition-context sc)
  (scope-def-ctx sc))

(define (scope-expansion-context sc)
  (or (scope-exp-ctx sc)
    (syntax-local-context)))

(define (new-scope #:scope [sc (current-scope)])
  (match sc
    [(scope def exp)
     (scope
       (syntax-local-make-definition-context def)
       (list* (gensym 'scope) (or exp null)))]))

(define (close-scope! #:scope [sc (current-scope)])
  (internal-definition-context-seal
    (scope-definition-context sc)))

(define (call-with-new-scope f #:scope [sc (current-scope)])
  (define nsc (new-scope #:scope sc))
  (begin0 (f nsc)
    (close-scope! #:scope nsc)))

(define-syntax (with-new-scope stx)
  (syntax-parse stx
    [(_ body ...+)
     #'(call-with-new-scope
         (lambda (sc)
           (parameterize {[current-scope sc]}
             body ...)))]))

(define (expand-in-scope stx
          #:scope [sc (current-scope)]
          #:stop-at [stop
                     (match (scope-expansion-context sc)
                       [(or 'expression 'module-begin) #false]
                       [_ '()])])
  (parameterize {[current-scope initial-scope]}
    (local-expand stx
      (scope-expansion-context sc)
      (if stop (append (kernel-form-identifier-list) stop) null)
      (scope-definition-context sc))))

(define (expand-expression-in-scope stx
          #:scope [sc (current-scope)]
          #:stop-at [stop #false])
  (parameterize {[current-scope initial-scope]}
    (local-expand stx
      'expression
      (if stop (append (kernel-form-identifier-list) stop) null)
      (scope-definition-context sc))))

(define (expand-in-scope-for-syntax stx
          #:scope [sc initial-scope]
          #:stop-at [stop #false])
  (parameterize {[current-scope initial-scope]}
    (local-transformer-expand stx
      (scope-expansion-context sc)
      (if stop (append (kernel-form-identifier-list) stop) null)
      (scope-definition-context sc))))

(define (in-scope stx #:scope [sc (current-scope)])
  (define temp (generate-temporary))
  (syntax-parse (expand-in-scope (datum->syntax #false (list temp stx))
                  #:scope sc
                  #:stop-at (list temp))
    [(_ e) #'e]))

(define (out-of-scope id-stx #:scope [sc (current-scope)])
  (identifier-remove-from-definition-context id-stx
    (scope-definition-context sc)))

(define (scope-bind-syntaxes/eval! id-stxs stx #:scope [sc (current-scope)])
  (parameterize {[current-syntax-context stx]}
    (syntax-local-bind-syntaxes id-stxs stx (scope-definition-context sc))))
(define (scope-bind-syntax/eval! id-stx stx #:scope [sc (current-scope)])
  (scope-bind-syntaxes/eval! (list id-stx) stx #:scope sc))

(define (scope-bind-syntax! id-stx v #:scope [sc (current-scope)])
  (scope-bind-syntaxes/eval! (list id-stx) #`(quote #,v) #:scope sc))
(define (scope-bind-syntaxes! id-stxs vs #:scope [sc (current-scope)])
  (for {[id-stx (in-list id-stxs)] [v (in-list vs)]}
    (scope-bind-syntax! id-stx v #:scope sc)))

(define (scope-bind-values! id-stxs #:scope [sc (current-scope)])
  (scope-bind-syntaxes/eval! id-stxs #f #:scope sc))
(define (scope-bind-value! id-stx #:scope [sc (current-scope)])
  (scope-bind-syntaxes/eval! (list id-stx) #f #:scope sc))

(define (eval-in-scope stx
          #:scope [sc (current-scope)])
  (parameterize {[current-syntax-context stx]}
    (syntax-local-eval stx
      (scope-definition-context sc))))

(define (scope-delta-introducer id-stx
          #:scope [sc (current-scope)])
  (eval-in-scope #:scope sc
    #`((quote #,(lambda () (syntax-local-make-delta-introducer id-stx))))))

(define (scope-static-value id-stx
          #:rename? [rename? #true]
          #:scope [sc (current-scope)]
          #:success [success identity]
          #:failure [failure
                     (lambda ()
                       (wrong-syntax id-stx
                         "expected an identifier with a syntax binding"))])
  (define thunk
    (let/ec return
      (cond
        [rename?
         (define x
           (syntax-local-value id-stx
             (lambda () (return failure))
             (scope-definition-context sc)))
         (lambda () (success x))]
        [else
         (define-values {x id2-stx}
           (syntax-local-value/immediate id-stx
             (lambda () (return failure))
             (scope-definition-context sc)))
         (lambda () (success x))])))
  (thunk))

(define (scope-static-binding? id-stx #:scope [sc (current-scope)])
  (scope-static-value id-stx #:scope sc
    #:success (lambda (x) #t)
    #:failure (lambda () #f)))
